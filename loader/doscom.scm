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

;; Loader for PC boot sectors

(library (zabavno loader doscom)
  (export load-dos-com-image)
  (import (rnrs (6))
          (zabavno cpu x86))

  ;; Loads a DOS .com image into the current machine.
  (define (load-dos-com-image image-port)
    (let ((M (current-machine)))
      ;; Load the .com image to cs:ip.
      (machine-CS-set! M #x07D2)
      (machine-IP-set! M #x0100)
      (set-port-position! image-port 0)
      (let ((boot-sector (get-bytevector-n image-port 65535)))
        (copy-to-memory (real-pointer (machine-CS M) (machine-IP M)) boot-sector))
      ;; Set registers for starting the image.
      (machine-DS-set! M (machine-CS M))
      (machine-ES-set! M (machine-CS M))
      (machine-SS-set! M (machine-CS M))
      (machine-SP-set! M #xFFFE)          ;stack at the end of the image
      ;; Return to address 0.
      (memory-u16-set! (real-pointer (machine-SS M) (machine-SP M)) #x0000))))
