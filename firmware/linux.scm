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

;; Trivial Linux emulation

(library (zabavno firmware linux)
  (export linux-setup)
  (import (rnrs (6))
          (zabavno cpu x86))

  (define EFAULT 14)

  (define (print . x)
    (for-each (lambda (x) (display x (current-error-port))) x)
    (newline (current-error-port)))

  (define-record-type linux)

  ;; Prepare the current machine for Linux syscalls.
  (define (linux-setup)
    (let ((linux-data (make-linux)))
      ;; Hook interrupt vectors.
      (let ((M (current-machine)))
        (machine-A20-gate-control 'enable)
        (machine-hook-interrupt! M #x80
                                 (lambda (M int chain)
                                   ;; TODO: Just fill this in.
                                   (if (eq? (linux-int80 linux-data M)
                                            'exit-emulator)
                                       'stop
                                       #f))))
      linux-data))

  (define (linux-int80 linux-data M)
    (define (not-implemented)
      (print "linux: Unhandled syscall AX=#x" (number->string (machine-AX M) 16))
      (machine-AX-set! M (bitwise-ior #x0005 (bitwise-and (machine-AX M)
                                                          (bitwise-not #xffff)))))

    (let ((number (machine-AX M))
          (arg0 (machine-BX M))
          (arg1 (machine-CX M))
          (arg2 (machine-DX M)))
      (cond ((and (fx<? number (vector-length syscalls)) (vector-ref syscalls number)) =>
             (lambda (proc) (proc M arg0 arg1 arg2)))
            (else
             (machine-AX-set! M (- EFAULT))))))

;;; Syscalls

  (define syscalls (make-vector 319))

  (define-syntax define-syscall
    (syntax-rules ()
      ((_ (name number M) . body)
       (define-syscall (name number M arg0 arg1 arg2) . body))
      ((_ (name number M arg0) . body)
       (define-syscall (name number M arg0 arg1 arg2) . body))
      ((_ (name number M arg0 arg1) . body)
       (define-syscall (name number M arg0 arg1 arg2) . body))
      ((_ (name number M arg0 arg1 arg2) . body)
       (vector-set! syscalls number (lambda (M arg0 arg1 arg2) . body)))))

  (define-syscall (exit 1 M status)
    'exit-emulator)

  (define-syscall (write 4 M fd buf len)
    (let ((port (case fd
                  ((1) (current-output-port))
                  ((2) (current-error-port))
                  (else #f))))
      (cond (port
             ;; FIXME: This should be binary output
             (put-string port (utf8->string (copy-from-memory buf len))))
            (else
             (machine-AX-set! M (- EFAULT)))))))
