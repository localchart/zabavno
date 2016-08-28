;; -*- mode: scheme; coding: utf-8 -*-
;; PC emulator in Scheme
;; Copyright © 2016 Göran Weinholt <goran@weinholt.se>

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.
#!r6rs

;; Trivial PC BIOS firmware emulation

;; See Ralph Brown's Interrupt List for documentation on what this is
;; trying to be.

(library (zabavno firmware pcbios)
  (export pcbios-setup
          pcbios-load-floppy-image
          pcbios-post-emulator-exit
          pcbios-interrupt)
  (import (rnrs (6))
          (zabavno cpu x86)
          #;(weinholt text hexdump))

  (define-record-type bios
    (fields floppy-drives disk-drives
            ;; DOS
            file-handles (mutable next-fh))
    (protocol
     (lambda (p)
       (lambda ()
         (let ((file-handles (make-vector 16 #f)))
           (vector-set! file-handles 0 (current-input-port))
           (vector-set! file-handles 1 (current-output-port))
           (vector-set! file-handles 2 (current-error-port))
           (p (make-vector 2 #f)
              (make-vector 24 #f)
              file-handles 3))))))

  (define (print . x)
    (for-each (lambda (x) (display x (current-error-port))) x)
    (newline (current-error-port)))

  ;; Converts from CHS format to LBA, for 1.44 MB floppies.
  (define (floppy-2880-chs->lba cylinder head sector)
    (let ((heads/cylinder 2)
          (sectors/track 18))
      (fx+ (fx* (fx+ (fx* cylinder heads/cylinder) head) sectors/track)
           (fx- sector 1))))

  (define (lba->floppy-2880-chs lba)
    (let ((heads/cylinder 2)
          (sectors/track 18))
      (values (fxdiv lba (fx* heads/cylinder sectors/track))
              (fxmod (fxdiv lba sectors/track) heads/cylinder)
              (fx+ (fxmod lba sectors/track) 1))))

  ;; Prepare the current machine for BIOS calls. Returns an object
  ;; that should be passed to other procedures in this library.
  (define (pcbios-setup)
    (do ((seg #xF000)
         (int 0 (+ int 1)))
        ((= int #x100)
         (let ((off int))
           (memory-u8-set! (+ (* seg 16) off) #xCF))) ;IRET
      (let* ((addr (fxarithmetic-shift-left int 2))
             (off int))
        (memory-u16-set! addr off)
        (memory-u16-set! (+ addr 2) seg)
        (memory-u8-set! (real-pointer seg off) #xF1))) ;ICEBP
    ;; BIOS data area (BDA)
    (let ((equipment (fxior (fxarithmetic-shift-left 1 0) ;floppy disk
                            (fxarithmetic-shift-left 0 1) ;no 387
                            (fxarithmetic-shift-left #b10 4)))) ;80x25 color
      (memory-u16-set! (real-pointer #x0040 #x0010) equipment))
    (memory-u16-set! (real-pointer #x0040 #x0013)
                     (div (* 640 1024) 1024)) ;640kB of low memory
    ;; Populate the configuration table
    (let ((configuration-table
           '#vu8(#xF8 #x80 0            ;model, submodel, bios revision
                      #b01100000        ;second 8259, RTC
                      #b00000000
                      #b00000000
                      #b00000000
                      #b00000000)))
      (memory-u16-set! (real-pointer #xF000 #xE6F5)
                       (bytevector-length configuration-table))
      (copy-to-memory (real-pointer #xF000 (+ #xE6F5 2)) configuration-table))
    (make-bios))

  (define (pcbios-load-floppy-image bios-data drive-index image-port)
    ;; Later this image should go to the floppy controller instead.
    (vector-set! (bios-floppy-drives bios-data) drive-index image-port))

  ;; This procedure runs after machine-run has exited and checks if
  ;; the machine is at a BIOS interrupt vector. It's a bit hacky doing
  ;; it this way, but it's easier to get started.
  (define (pcbios-post-emulator-exit bios-data M)
    (cond ((and (eqv? (machine-CS M) #xF000)
                (<= (machine-IP M) #xFF))
           ;; An interrupt vector. Fake BIOS calls.
           (cond ((eqv? (pcbios-interrupt bios-data M (machine-IP M)) 'exit-dos)
                  'exit-emulator)
                 (else
                  (machine-IP-set! M #x100) ;Points at IRET
                  'continue-emulator)))
          (else 'exit-emulator)))

  (define (port-file-size port)
    ;; XXX: This is the only portable way to do this in R6RS Scheme,
    ;; unfortunately. Would need compatibility wrappers for a few
    ;; different schemes.
    (let ((old-position (port-position port)))
      (set-port-position! port 0)
      (let ((bv (make-bytevector 65536)))
        (let lp ((size 0))
          (let ((n (get-bytevector-n! port bv 0 (bytevector-length bv))))
            (cond ((eof-object? n)
                   (set-port-position! port old-position)
                   size)
                  (else
                   (lp (+ size n)))))))))

  ;; Handle a BIOS interrupt.
  (define (pcbios-interrupt bios-data M vec)
    (define (not-implemented)
      ;; XXX: This should be logging instead.
      (print "pcbios: Unhandled BIOS INT #x" (number->string vec 16)
             " AX=#x" (number->string (machine-AX M) 16))
      (set-CF))
    (define (set-CF)
      (let ((addr (+ (* (machine-SS M) 16)
                     (machine-SP M)
                     4)))
        (memory-u16-set! addr (fxior flag-CF (memory-u16-ref addr)))))
    (define (clear-CF)
      (flush-output-port (current-output-port))
      (let ((addr (+ (* (machine-SS M) 16)
                     (machine-SP M)
                     4)))
        (memory-u16-set! addr (fxand (fxnot flag-CF)
                                     (memory-u16-ref addr)))))
    (let ((AH (bitwise-bit-field (machine-AX M) 8 16)))
      (when (machine-debug M)
        (print "pcbios: BIOS INT #x" (number->string vec 16)
               " AX=#x" (number->string (machine-AX M) 16)))
      (case vec
        ((#x06)
         (let ((hex (lambda (x) (number->string x 16)))
               (saved-ip (memory-u16-ref (real-pointer (machine-SS M) (machine-SP M))))
               (saved-cs (memory-u16-ref (real-pointer (machine-SS M) (+ (machine-SP M) 2)))))
           (print "pcbios: Invalid opcode (may be an unimplemented instruction) at "
                  (hex saved-cs) ":" (hex saved-ip) ": "
                  (hex (memory-u8-ref (real-pointer saved-cs saved-ip))) " "
                  (hex (memory-u8-ref (real-pointer saved-cs (fx+ saved-ip 1))))))
         'exit-dos)
        ((#x07)
         (print "pcbios: No x87 emulation has been implemented/installed, exiting")
         'exit-dos)
        ((#x10)
         (case AH
           ((#x0E)
            ;; Write a character. TODO: color.
            (display (integer->char (fxand (machine-AX M) #xff)))
            (clear-CF))
           (else
            (not-implemented))))
        ((#x11)
         ;; Get equipment list.
         (machine-AX-set! M (memory-u16-ref (real-pointer #x0040 #x0010)))
         (clear-CF))
        ((#x12)
         ;; Get memory size.
         (machine-AX-set! M (fxior (fxand (machine-AX M) (fxnot #xffff))
                                   (memory-u16-ref (real-pointer #x0040 #x0013))))
         (clear-CF))
        ((#x13)
         (case AH
           ((#x00)                      ;reset disk system
            (clear-CF))
           ((#x02)                      ;read sectors into memory
            (let ((sectors (fxand (machine-AX M) #xff))
                  (cylinder (fxior (fxbit-field (machine-CX M) 8 16)
                                   (fxarithmetic-shift-left
                                    (fxbit-field (machine-CX M) 6 8) 6)))
                  (sector (fxbit-field (machine-CX M) 0 6))
                  (head (fxbit-field (machine-DX M) 8 16))
                  (drive (fxbit-field (machine-DX M) 0 7))
                  (disk-drive? (fxbit-set? (machine-DX M) 7))
                  (buffer-seg (machine-ES M))
                  (buffer-off (machine-BX M)))
              (when (machine-debug M)
                (print "pcbios: read " sectors " sectors from " (list cylinder head sector)
                       " on drive " drive " to " buffer-seg ":" buffer-off))
              (cond
                ((zero? sectors)
                 (set-CF))
                ((and (not disk-drive?)
                      (fx<? drive (vector-length (bios-floppy-drives bios-data)))
                      (vector-ref (bios-floppy-drives bios-data) drive))
                 => (lambda (image-port)
                      (let ((lba (floppy-2880-chs->lba cylinder head sector)))
                        (set-port-position! image-port (fx* lba 512))
                        (let ((blocks (get-bytevector-n image-port (fx* sectors 512))))
                          (define (set-status status sectors-read)
                            (machine-AX-set! M (fxior (fxand (machine-AX M) (fxnot #xFFFF))
                                                      (fxarithmetic-shift-left status 8)
                                                      (if (bytevector? blocks)
                                                          (fxdiv (bytevector-length blocks) 512)
                                                          0))))
                          (cond ((or (eof-object? blocks) (fx<? (bytevector-length blocks) 512))
                                 (print "pcbios: disk read error from c=" cylinder
                                        ", h=" head ", s=" sector ", lba=" lba)
                                 (set-CF)
                                 (set-status #x04 0)) ;04 = sector not found/read error
                                (else
                                 #;(hexdump (current-error-port) blocks)
                                 (copy-to-memory (real-pointer buffer-seg buffer-off) blocks)
                                 (clear-CF)
                                 (set-status #x00 (fxdiv (bytevector-length blocks) 512))))))))
                (else
                 (set-CF)))))
           ((#x08)                      ;get drive parameters
            (let ((drive (fxbit-field (machine-DX M) 0 7))
                  (disk-drive? (fxbit-set? (machine-DX M) 7)))
              (cond
                ((or disk-drive? (> drive 0))
                 (set-CF))
                (else
                 (let ((drive-type #x04) ;1.44M
                       (maximum-cylinder-number 80)
                       (maximum-sector-number 18)
                       (maximum-head-number 2)
                       (number-of-drives 1))
                   (let ((ax 0)
                         (bx drive-type)
                         (cx (fxior
                              (fxarithmetic-shift-left (fxbit-field maximum-cylinder-number 0 6) 8)
                              (fxarithmetic-shift-left (fxbit-field maximum-cylinder-number 6 8) 5)
                              (fxbit-field maximum-sector-number 0 5)))
                         (dx (fxior
                              (fxarithmetic-shift-left maximum-head-number 8)
                              number-of-drives))
                         ;; TODO: drive parameter table
                         (es #xF000)
                         (di #x0100))
                     (machine-AX-set! M ax)
                     (machine-BX-set! M bx)
                     (machine-CX-set! M cx)
                     (machine-DX-set! M dx)
                     (machine-ES-set! M es)
                     (machine-DI-set! M di)
                     (clear-CF)))))))
           ((#x15)                      ;get disk type
            (let ((drive (fxbit-field (machine-DX M) 0 7))
                  (disk-drive? (fxbit-set? (machine-DX M) 7)))
              (let ((type (if (or disk-drive? (> drive 0))
                              #x00      ; no such drive
                              #x01)))   ; floppy without change-line support
                (machine-AX-set! M (fxior type
                                          (bitwise-and (machine-AX M)
                                                       (fxnot #xFFFF))))
                (clear-CF))))
           (else
            (not-implemented))))
        ((#x14)
         (case AH
           ((#x00)
            ;; Initialize serial port
            (let ((parameters (bitwise-bit-field (machine-AX M) 0 8))
                  (port (bitwise-bit-field (machine-DX M) 0 16)))
              (when (machine-debug M)
                (print "pcbios: initialize serial port " port " with parameters " parameters))
              (machine-AX-set! M (bitwise-ior (bitwise-and (machine-AX M) (fxnot #xffff))
                                              #x0000)) ; line/modem status
              (clear-CF)))
           (else
            (not-implemented))))
        ((#x15)
         (case AH
           ((#xC0)                      ;get configuration
            (machine-ES-set! M #xF000)
            (machine-BX-set! M #xE6F5)
            (machine-AX-set! M (bitwise-and (machine-AX M) (fxnot #xff00)))
            (clear-CF))
           (else
            (not-implemented))))
        ((#x16)
         (case AH
           ((#x00)                      ; get keystroke
            (let* ((c (get-char (current-input-port)))
                   (c (if (eof-object? c) (integer->char 26) c))) ;Ctrl-Z
              (machine-AX-set! M (fxior (fxand (machine-AX M) (fxnot #xffff))
                                        (fx* #x0101 (fxand (char->integer c) #xff))))
              (clear-CF)))
           (else
            (not-implemented))))
        ((#x17)
         (case AH
           ((#x01)
            ;; Initialize parallel port.
            (let ((port (bitwise-bit-field (machine-DX M) 0 16)))
              (when (machine-debug M)
                (print "pcbios: initialize parallel port " port))
              (machine-AX-set! M (bitwise-ior (bitwise-and (machine-AX M) (bitwise-not #xffff00ff))
                                              #x00)))
            (clear-CF))
           (else
            (not-implemented))))
        ((#x19)
         ;; Bootstrap loader. Used to reboot the machine.
         (clear-CF)
         'reboot)
        ((#x1A)                         ;TIME
         ;; TODO: Really get the time.
         (let ((hour 19) (minutes 01) (seconds 23) (dst-flag #x01)
               (century 20) (year 16) (month 8) (day 27))
           (define (bcd n)
             (let lp ((ret 0) (n n) (s 0))
               (if (eqv? n 0)
                   ret
                   (lp (fx+ ret (fxarithmetic-shift-left (fxand n #xf) s))
                       (fxarithmetic-shift-right n 4)
                       (fx+ s 4)))))
           (case AH
             ((#x00)                    ;get system time
              (let* ((ticks-per-day #x1800B0) ; ~18.2 ticks/s
                     (clock-ticks (round (* (+ (* hour 60 60) (* minutes 60) seconds)
                                            (/ (* 24 60 60) ticks-per-day))))
                     (midnight-counter #x00))
                (machine-CX-set! M (bitwise-ior (bitwise-bit-field clock-ticks 16 32)
                                                (bitwise-and (machine-CX M)
                                                             (fxnot #xFFFF))))
                (machine-DX-set! M (bitwise-ior (bitwise-bit-field clock-ticks 0 16)
                                                (bitwise-and (machine-DX M)
                                                             (fxnot #xFFFF))))
                (machine-AX-set! M (bitwise-ior midnight-counter
                                                (bitwise-and (machine-AX M)
                                                             (fxnot #xFF))))
                (clear-CF)))
             ((#x02)                    ;get rtc time
              (machine-CX-set! M (bitwise-ior (bcd (+ (* hour 100) minutes))
                                              (bitwise-and (machine-CX M)
                                                           (fxnot #xFFFF))))
              (machine-DX-set! M (bitwise-ior (bcd (+ (* seconds 100) dst-flag))
                                              (bitwise-and (machine-DX M)
                                                           (fxnot #xFFFF))))
              (clear-CF))
             ((#x04)                    ;get rtc date
              (machine-CX-set! M (bitwise-ior (bcd (+ (* century 100) year))
                                              (bitwise-and (machine-CX M)
                                                           (fxnot #xFFFF))))
              (machine-DX-set! M (bitwise-ior (bcd (+ (* month 100) day))
                                              (bitwise-and (machine-DX M)
                                                           (fxnot #xFFFF))))
              (clear-CF))
             (else
              (not-implemented)))))
        ((#x20)                         ;DOS
         ;; Terminate program. Used to exit the program.
         'exit-dos)
        ((#x21)                         ;DOS
         (case AH
           ((#x09)                      ; Print a $-terminated string.
            (let lp ((i (machine-DX M)))
              (let* ((addr (fx+ (fx* (machine-DS M) 16) i))
                     (char (integer->char (memory-u8-ref addr))))
                (unless (eqv? char #\$)
                  (display char)
                  (unless (fx>? i #xffff)
                    (lp (fx+ i 1))))))
            (machine-AX-set! M (fxior #x0900 (char->integer #\$)))
            (clear-CF))
           ((#x30)                ; DOS version. Reports IBM DOS 5.00.
            (machine-AX-set! M (bitwise-ior #x0005 (bitwise-and (machine-AX M)
                                                                (bitwise-not #xffff))))
            (machine-BX-set! M (bitwise-and (machine-BX M) (bitwise-not #xffff)))
            (machine-CX-set! M (bitwise-and (machine-CX M) (bitwise-not #xffff)))
            (clear-CF))
           ((#x3C)                      ; create or truncate file
            (let-values (((p extract) (open-bytevector-output-port)))
              (do ((addr (real-pointer (machine-DS M) (bitwise-and (machine-DX M) #xffff))
                         (+ addr 1))
                   (limit 128 (- limit 1)))
                  ((or (eqv? (memory-u8-ref addr) 0) (zero? limit)))
                (put-u8 p (memory-u8-ref addr)))
              (let ((filename (utf8->string (extract)))
                    (attrs (bitwise-and (machine-CX M) #xffff))
                    (fh (bios-next-fh bios-data)))
                (when (machine-debug M)
                  (print "pcbios: create " filename " => " fh " with attributes " attrs))
                (guard (exn
                        ((i/o-error? exn)
                         (print "pcbios: failed to create/truncate " filename)
                         (set-CF)))
                  (vector-set! (bios-file-handles bios-data) fh
                               (open-file-output-port filename (file-options no-fail)))
                  (bios-next-fh-set! bios-data (+ fh 1))
                  (machine-AX-set! M (bitwise-ior fh (bitwise-and (machine-AX M)
                                                                  (fxnot #xffff))))
                  (clear-CF)))))
           ((#x3D)                      ; open existing file
            (let-values (((p extract) (open-bytevector-output-port)))
              (do ((addr (real-pointer (machine-DS M) (bitwise-and (machine-DX M) #xffff))
                         (+ addr 1))
                   (limit 128 (- limit 1)))
                  ((or (eqv? (memory-u8-ref addr) 0) (zero? limit)))
                (put-u8 p (memory-u8-ref addr)))
              (let ((filename (utf8->string (extract)))
                    (modes (bitwise-and (machine-AX M) #xff))
                    (fh (bios-next-fh bios-data)))
                (when (machine-debug M)
                  (print "pcbios: open " filename " => " fh " with modes " modes))
                (guard (exn
                        ((i/o-error? exn)
                         (print "pcbios: failed to open " filename)
                         (set-CF)))
                  (cond ((case (fxbit-field modes 0 3)
                           ((#b000) open-file-input-port)
                           ((#b001) open-file-output-port)
                           ((#b010) open-file-input/output-port)
                           (else #f))
                         =>
                         (lambda (opener)
                           (vector-set! (bios-file-handles bios-data) fh
                                        (opener filename (file-options no-create no-truncate)))
                           (bios-next-fh-set! bios-data (+ fh 1))
                           (machine-AX-set! M (bitwise-ior fh (bitwise-and (machine-AX M)
                                                                           (fxnot #xffff))))
                           (clear-CF)))
                        (else
                         (set-CF)))))))
           ((#x3E)                      ; close file
            (let ((file-handles (bios-file-handles bios-data))
                  (file-handle (machine-BX M)))
              (cond
                ((or (not (< file-handle (vector-length file-handles)))
                     (not (vector-ref file-handles file-handle))
                     (not (input-port? (vector-ref file-handles file-handle))))
                 (set-CF))
                (else
                 ;; TODO: handle errors
                 (let ((port (vector-ref file-handles file-handle)))
                   (when (machine-debug M)
                     (print "pcbios: close fh " file-handle))
                   (guard (exn
                           ((i/o-error? exn)
                            (set-CF)))
                     (close-port port)
                     (clear-CF)))))))
           ((#x3F)                      ; read from file or device
            (let ((file-handles (bios-file-handles bios-data))
                  (file-handle (machine-BX M))
                  (count (bitwise-and (machine-CX M) #xFFFF))
                  (buffer (real-pointer (machine-DS M) (bitwise-and (machine-DX M) #xFFFF))))
              (cond
                ((or (not (< file-handle (vector-length file-handles)))
                     (not (vector-ref file-handles file-handle))
                     (not (input-port? (vector-ref file-handles file-handle))))
                 (set-CF))
                (else
                 ;; TODO: handle errors, textual input ports, and CON stops at CR
                 (let* ((port (vector-ref file-handles file-handle))
                        (bv (if (< file-handle 3)
                                (let ((s (get-string-n port count))) ;XXX: can read too much
                                  (if (eof-object? s) s (string->utf8 s)))
                                (get-bytevector-n port count))))
                   (when (machine-debug M)
                     (print "pcbios: read fh " file-handle ", "
                            (or (eof-object? bv) (bytevector-length bv))
                            " bytes into buffer " buffer " of size " count))
                   (cond ((eof-object? bv)
                          (machine-AX-set! M (bitwise-and (machine-AX M)
                                                          (fxnot #xFFFF)))
                          (clear-CF))
                         (else
                          ;; XXX: does not wrap in the segment
                          (copy-to-memory buffer bv 0 (fxmin count (bytevector-length bv)))
                          (machine-AX-set! M (bitwise-ior (bitwise-and (machine-AX M)
                                                                       (fxnot #xFFFF))
                                                          (bytevector-length bv)))
                          (clear-CF))))))))
           ((#x40)                      ; write to file or device
            (let ((file-handles (bios-file-handles bios-data))
                  (file-handle (machine-BX M))
                  (count (fxand (machine-CX M) #xFFFF)))
              (cond ((or (not (< file-handle (vector-length file-handles)))
                         (not (vector-ref file-handles file-handle))
                         (not (output-port? (vector-ref file-handles file-handle))))
                     (set-CF))
                    (else
                     (when (and (machine-debug M) (>= file-handle 3))
                       (print "pcbios: write fh " file-handle ", "
                              count " bytes from buffer "
                              (real-pointer (machine-DS M) (+ (machine-DX M)))))
                     (do ((port (vector-ref file-handles file-handle))
                          (i 0 (+ i 1)))
                         ((= i count)
                          (machine-AX-set! M (bitwise-ior count
                                                          (bitwise-and (machine-AX M)
                                                                       (bitwise-not #xffff))))
                          (flush-output-port port)
                          (clear-CF))
                       (let* ((addr (real-pointer (machine-DS M) (+ (machine-DX M) i)))
                              (byte (memory-u8-ref addr)))
                         (cond ((< file-handle 3)
                                (put-char port (integer->char byte)))
                               (else
                                (put-u8 port byte)))))))))
           ((#x42)                      ; set current file position
            (let ((file-handles (bios-file-handles bios-data))
                  (file-handle (machine-BX M))
                  (whence (bitwise-and (machine-AX M) #xFF))
                  (offset-u32
                   (bitwise-ior
                    (bitwise-arithmetic-shift-left (bitwise-and (machine-CX M) #xFFFF) 16)
                    (bitwise-and (machine-DX M) #xFFFF)))
                  (bv (make-bytevector 4)))
              (bytevector-u32-native-set! bv 0 offset-u32)
              (cond ((or (not (< file-handle (vector-length file-handles)))
                         (not (vector-ref file-handles file-handle))
                         (not (port? (vector-ref file-handles file-handle))))
                     (set-CF))
                    (else
                     (let ((offset (bytevector-s32-native-ref bv 0))
                           (port (vector-ref file-handles file-handle)))
                       (when (machine-debug M)
                         (print "pcbios: seek fh " file-handle " by " offset ", whence " whence))
                       (case whence
                         ((#x00)
                          (set-port-position! port offset)
                          (clear-CF))
                         ((#x01)
                          (set-port-position! port (+ (port-position port) offset))
                          (clear-CF))
                         (else
                          (set-port-position! port (+ (port-file-size port) offset))
                          (clear-CF))))))))
           ((#x4C)                      ;terminate with return code
            (exit (bitwise-bit-field (machine-AX M) 0 8)))
           (else
            (not-implemented))))
        ((#x29)                         ;fast console output
         (put-char (current-output-port)
                   (integer->char (bitwise-bit-field (machine-AX M) 0 8))))
        (else
         (not-implemented))))))
