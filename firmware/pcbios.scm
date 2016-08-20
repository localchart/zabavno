;; -*- mode: scheme; coding: utf-8 -*-
;; Trivial PC BIOS firmware emulation
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

(library (zabavno firmware pcbios)
  (export pcbios)
  (import (rnrs (6))
          (zabavno cpu x86))

  (define (print . x)
    ;; XXX: This should be logging instead.
    (for-each display x)
    (newline))

  (define (pcbios M vec)
    (define (set-CF)
      (print "Unhandled INT #x" (number->string vec 16)
             " AX=#x" (number->string (machine-AX M) 16))
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
      (case vec
        ((#x10)
         (case AH
           ((#x0E)
            ;; Write a character. TODO: color.
            (display (integer->char (fxand (machine-AX M) #xff)))
            (clear-CF))
           (else
            (set-CF))))
        ((#x13)
         (case AH
           ((#x00)
            ;; Reset disk system.
            (clear-CF))
           (else
            (set-CF))))
        ((#x20)
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
            ;; INT 21 - DOS 2+ - "WRITE" - WRITE TO FILE OR DEVICE
            ;;     AH = 40h
            ;;     BX = file handle
            ;;     CX = number of bytes to write
            ;;     DS:DX -> data to write
            ;; Return: CF clear if successful
            ;;         AX = number of bytes actually written
            ;;     CF set on error
            ;;         AX = error code (05h,06h) (see #01680 at AH=59h/BX=0000h)
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
            (set-CF))))
        (else
         (set-CF)))))

  )