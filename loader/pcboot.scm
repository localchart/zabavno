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

(library (zabavno loader pcboot)
  (export detect-boot-sector run-boot)
  (import (rnrs (6))
          (zabavno cpu x86))

  (define (detect-boot-sector filename)
    "Will be true if the file looks like it contains a PC boot sector."
    (call-with-port (open-file-input-port filename)
      (lambda (p)
        (let ((boot-sector (get-bytevector-n p 512)))
          (and (not (eof-object? boot-sector))
               (= (bytevector-length boot-sector) 512)
               (eqv? (bytevector-u16-ref boot-sector 510 (endianness big)) #x55AA))))))

  ;; Runs a boot sector from a named file and runs it, using the given
  ;; BIOS.
  (define (run-boot filename bios)
    (let ((M (make-machine)))
      (with-machine M
        (lambda ()
          (call-with-port (open-file-input-port filename)
            (lambda (p)
              (let ((boot-sector (get-bytevector-n p 512)))
                (copy-to-memory #x7C00 boot-sector))))
          (machine-debug-set! (current-machine) #f)
          ;; Start running code at 0000:7C00.
          (machine-CS-set! (current-machine) #x0000)
          (machine-IP-set! (current-machine) #x7C00)
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
