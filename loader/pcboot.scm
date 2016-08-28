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
  (export detect-boot-sector load-boot-sector)
  (import (rnrs (6))
          (zabavno cpu x86))

  ;; Check if the port contains a boot sector.
  (define (detect-boot-sector image-port)
    (set-port-position! image-port 0)
    (let ((boot-sector (get-bytevector-n image-port 512)))
      (and (not (eof-object? boot-sector))
           (= (bytevector-length boot-sector) 512)
           (eqv? (bytevector-u16-ref boot-sector 510 (endianness big)) #x55AA))))

  ;; Loads a boot sector (e.g. a floppy boot sector or MBR) into the
  ;; current machine.
  (define (load-boot-sector image-port)
    (let ((M (current-machine))
          (floppy? #t))
      ;; Start running code at 0000:7C00.
      (machine-CS-set! M #x0000)
      (machine-IP-set! M #x7C00)
      (machine-DX-set! M (if floppy? #x2000 #x0280)) ;device supported by INT 13
      (set-port-position! image-port 0)
      (let ((boot-sector (get-bytevector-n image-port 512)))
        (copy-to-memory (real-pointer (machine-CS M) (machine-IP M)) boot-sector)))))
