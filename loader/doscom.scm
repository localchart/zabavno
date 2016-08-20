;; -*- mode: scheme; coding: utf-8 -*-
;; Loader for PC boot sectors
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

(library (zabavno loader doscom)
  (export run-dos)
  (import (rnrs (6))
          (zabavno cpu x86))

  (define (run-dos filename bios)
    (let ((M (make-machine)))
      (with-machine M
        (lambda ()
          (call-with-port (open-file-input-port filename)
            (lambda (p)
              (let ((boot-sector (get-bytevector-n p 65535)))
                (copy-to-memory (+ (* #x07D2 16) #x0100) boot-sector))))
          (machine-debug-set! M #f)
          (machine-CS-set! M #x07D2)
          (machine-IP-set! M #x0100)
          (machine-DS-set! M #x07D2)
          (machine-ES-set! M #x07D2)
          (machine-SS-set! M #x07D2)
          (machine-SP-set! M #xFFFE)
          (memory-u16-set! (+ (* (machine-SS M) 16)
                              (machine-SP M)) #x0000)
          (do ((seg #xF000)
               (int 0 (+ int 1)))
              ((= int #x100)
               (let ((off int))
                 (memory-u8-set! (+ (* seg 16) off)
                                 #xCF)))     ;IRET
            (let* ((addr (fxarithmetic-shift-left int 2))
                   (off int))
              (memory-u16-set! addr off)
              (memory-u16-set! (+ addr 2) seg)
              (memory-u8-set! (+ (* seg 16) off)
                              #xF1)))     ;ICEBP
          (let lp ()
            (machine-run)
            (cond ((and (eqv? (machine-CS M) #xF000)
                        (<= (machine-IP M) #xFF))
                   ;; An interrupt vector. Fake BIOS calls.
                   (unless (eqv? (bios M (machine-IP M)) 'exit-dos)
                     (machine-IP-set! M #x100) ;Points at IRET
                     (lp))))))))))
