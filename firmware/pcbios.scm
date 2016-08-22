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

(library (zabavno firmware pcbios)
  (export pcbios-setup
          pcbios-load-floppy-image
          pcbios-post-emulator-exit
          pcbios-interrupt)
  (import (rnrs (6))
          (zabavno cpu x86)
          #;(weinholt text hexdump))

  (define-record-type bios
    (fields floppy-drives disk-drives)
    (protocol
     (lambda (p)
       (lambda ()
         (p (make-vector 2 #f)
            (make-vector 24 #f))))))

  (define (print . x)
    (for-each (lambda (x) (display x (current-error-port))) x)
    (newline))

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
    (memory-u16-set! (real-pointer #x0040 #x0013)
                     (fxdiv (machine-memory-size (current-machine)) 1024))
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

  ;; Handle a BIOS interrupt.
  (define (pcbios-interrupt bios-data M vec)
    (define (not-implemented)
      ;; XXX: This should be logging instead.
      (print "Unhandled BIOS INT #x" (number->string vec 16)
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
        ((#x10)
         (case AH
           ((#x0E)
            ;; Write a character. TODO: color.
            (display (integer->char (fxand (machine-AX M) #xff)))
            (clear-CF))
           (else
            (not-implemented))))
        ((#x12)
         ;; Get memory size.
         (machine-AX-set! M (fxior (fxand (machine-AX M) (fxnot #xffff))
                                   (memory-u16-ref (real-pointer #x0040 #x0013))))
         (clear-CF))
        ((#x13)
         (case AH
           ((#x00)
            ;; Reset disk system.
            (clear-CF))
           ((#x02)
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
                (print "pcbios: read " sectors " sectors from " (list cylinder sector head)
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
           (else
            (not-implemented))))
        ((#x16)
         (case AH
           ((#x00)
            ;; Get keystroke
            (let* ((c (get-char (current-input-port)))
                   (c (if (eof-object? c) (integer->char 26) c))) ;Ctrl-Z
              (machine-AX-set! M (fxior (fxand (machine-AX M) (fxnot #xffff))
                                        (fx* #x0101 (fxand (char->integer c) #xff))))
              (clear-CF)))))
        ((#x19)
         ;; Bootstrap loader. Used to reboot the machine.
         (clear-CF)
         'exit-dos)
        ((#x20)
         ;; Terminate program. Used to exit the program.
         'exit-dos)
        ((#x21)
         (case AH
           ((#x09)
            ;; Print a $-terminated string.
            (let lp ((i (machine-DX M)))
              (let* ((addr (fx+ (fx* (machine-DS M) 16) i))
                     (char (integer->char (memory-u8-ref addr))))
                (unless (eqv? char #\$)
                  (display char)
                  (unless (fx>? i #xffff)
                    (lp (fx+ i 1))))))
            (machine-AX-set! M (fxior #x0900 (char->integer #\$)))
            (clear-CF))
           ((#x30)
            ;; DOS version.
            (machine-AX-set! M #x0000))
           ((#x40)
            ;; XXX: do something about BX.
            (let ((file-handle (machine-BX M))
                  (count (fxand (machine-CX M) #xFFFF)))
              (do ((i 0 (+ i 1)))
                  ((= i count)
                                        ;FIXME: preserve eAX
                   (machine-AX-set! M (machine-CX M)))
                (let* ((addr (+ (* (machine-DS M) 16)
                                (machine-DX M)
                                i))
                       (char (integer->char (memory-u8-ref addr))))
                  (display char))))
            (clear-CF))
           (else
            (not-implemented))))
        (else
         (not-implemented))))))
