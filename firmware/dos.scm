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

;; Trivial DOS emulation

;; See Ralph Brown's Interrupt List for documentation on what this is
;; trying to be.

(library (zabavno firmware dos)
  (export dos-setup)
  (import (rnrs (6))
          (zabavno cpu x86)
          (only (zabavno firmware pcbios) cp437/control))

  (define-record-type dos
    (fields file-handles (mutable next-fh))
    (protocol
     (lambda (p)
       (lambda ()
         (let ((file-handles (make-vector 16 #f)))
           (vector-set! file-handles 0 (current-input-port))
           (vector-set! file-handles 1 (current-output-port))
           (vector-set! file-handles 2 (current-error-port))
           (p file-handles 3))))))

  (define (print . x)
    (for-each (lambda (x) (display x (current-error-port))) x)
    (newline (current-error-port)))

  (define (hex x)
    (number->string x 16))

  ;; Prepare the current machine for DOS calls.
  (define (dos-setup)
    (let ((dos-data (make-dos)))
      ;; Hook interrupt vectors.
      (let ((M (current-machine)))
        (for-each (lambda (vec)
                    (machine-hook-interrupt! M vec
                                             (lambda (M int chain)
                                               (if (eq? (dos-interrupt dos-data M vec)
                                                        'exit-emulator)
                                                   'stop
                                                   #f))))
                  '(#x20 #x21 #x29)))
      dos-data))

;;; Helpers

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

;;; Interrupt handlers

  (define (dos-interrupt dos-data M vec)
    (define (set-CF)
      (let ((addr (real-pointer (machine-SS M) (+ (machine-SP M) 4))))
        (memory-u16-set! addr (fxior flag-CF (memory-u16-ref addr)))))
    (define (clear-CF)
      (flush-output-port (current-output-port))
      (let ((addr (real-pointer (machine-SS M) (+ (machine-SP M) 4))))
        (memory-u16-set! addr (fxand (fxnot flag-CF)
                                     (memory-u16-ref addr)))))
    (define (not-implemented)
      (print "dos: Unhandled DOS INT #x" (number->string vec 16)
             " AX=#x" (number->string (machine-AX M) 16)))
    (let ((AH (bitwise-bit-field (machine-AX M) 8 16)))
      (case vec
        ((#x20)
         ;; Terminate program. Used to exit the program.
         'exit-emulator)
        ((#x21)
         (case AH
           ((#x02)                      ; write character to standard output
            (put-char (current-output-port)
                      (string-ref cp437/control (bitwise-bit-field (machine-DX M) 0 8))))
           ((#x09)                      ; Print a $-terminated string.
            (let lp ((i (machine-DX M)))
              (let* ((addr (fx+ (fx* (machine-DS M) 16) i))
                     (char (memory-u8-ref addr)))
                (unless (eqv? char (char->integer #\$))
                  (put-char (current-output-port)
                            (string-ref cp437/control char))
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
                    (fh (dos-next-fh dos-data)))
                (when (machine-debug M)
                  (print "DOS: create " filename " => " fh " with attributes " attrs))
                (guard (exn
                        ((i/o-error? exn)
                         (print "DOS: failed to create/truncate " filename)
                         (set-CF)))
                  (vector-set! (dos-file-handles dos-data) fh
                               (open-file-output-port filename (file-options no-fail)))
                  (dos-next-fh-set! dos-data (+ fh 1))
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
                    (fh (dos-next-fh dos-data)))
                (when (machine-debug M)
                  (print "DOS: open " filename " => " fh " with modes " modes))
                (guard (exn
                        ((i/o-error? exn)
                         (print "DOS: failed to open " filename)
                         (set-CF)))
                  (cond ((case (fxbit-field modes 0 3)
                           ((#b000) open-file-input-port)
                           ((#b001) open-file-output-port)
                           ((#b010) open-file-input/output-port)
                           (else #f))
                         =>
                         (lambda (opener)
                           (vector-set! (dos-file-handles dos-data) fh
                                        (opener filename (file-options no-create no-truncate)))
                           (dos-next-fh-set! dos-data (+ fh 1))
                           (machine-AX-set! M (bitwise-ior fh (bitwise-and (machine-AX M)
                                                                           (fxnot #xffff))))
                           (clear-CF)))
                        (else
                         (set-CF)))))))
           ((#x3E)                      ; close file
            (let ((file-handles (dos-file-handles dos-data))
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
                     (print "DOS: close fh " file-handle))
                   (guard (exn
                           ((i/o-error? exn)
                            (set-CF)))
                     (close-port port)
                     (clear-CF)))))))
           ((#x3F)                      ; read from file or device
            (let ((file-handles (dos-file-handles dos-data))
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
                     (print "DOS: read fh " file-handle ", "
                            (or (eof-object? bv) (bytevector-length bv))
                            " bytes into buffer " buffer " of size " count))
                   (cond ((eof-object? bv)
                          (machine-AX-set! M (bitwise-and (machine-AX M)
                                                          (fxnot #xFFFF)))
                          (clear-CF))
                         (else
                          ;; XXX: does not check the segment limit
                          (copy-to-memory buffer bv 0 (fxmin count (bytevector-length bv)))
                          (machine-AX-set! M (bitwise-ior (bitwise-and (machine-AX M)
                                                                       (fxnot #xFFFF))
                                                          (bytevector-length bv)))
                          (clear-CF))))))))
           ((#x40)                      ; write to file or device
            (let ((file-handles (dos-file-handles dos-data))
                  (file-handle (machine-BX M))
                  (count (fxand (machine-CX M) #xFFFF)))
              (cond ((or (not (< file-handle (vector-length file-handles)))
                         (not (vector-ref file-handles file-handle))
                         (not (output-port? (vector-ref file-handles file-handle))))
                     (set-CF))
                    (else
                     (when (and (machine-debug M) (>= file-handle 3))
                       (print "DOS: write fh " file-handle ", "
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
                                (put-char port (string-ref cp437/control byte)))
                               (else
                                (put-u8 port byte)))))))))
           ((#x42)                      ; set current file position
            (let ((file-handles (dos-file-handles dos-data))
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
                         (print "DOS: seek fh " file-handle " by " offset ", whence " whence))
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
            (set-CF)
            (not-implemented))))
        ((#x29)                         ;fast console output
         (put-char (current-output-port)
                   (string-ref cp437/control (bitwise-bit-field (machine-AX M) 0 8))))
        (else
         (not-implemented))))))
