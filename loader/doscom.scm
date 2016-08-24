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
  (define (load-dos-com-image image-port command-line-arguments)
    (let ((M (current-machine))
          (cs #x07D2))
      ;; Load the .com image to cs:ip.
      (machine-CS-set! M cs)
      (machine-IP-set! M #x0100)
      (set-port-position! image-port 0)
      (let ((boot-sector (get-bytevector-n image-port (- #xffff #x100))))
        (copy-to-memory (real-pointer cs (machine-IP M)) boot-sector))
      ;; Set registers for starting the image.
      (machine-DS-set! M cs)
      (machine-ES-set! M cs)
      (machine-SS-set! M cs)
      ;; Return to address 0.
      (machine-SP-set! M #xFFFE)          ;stack at the end of the image
      (memory-u16-set! (real-pointer (machine-SS M) (machine-SP M)) #x0000)
      ;; Program Segment Prefix (PSP).
      (copy-to-memory (real-pointer cs #x0000) (make-bytevector #x100 0))
      (memory-u8-set! (real-pointer cs #x0000) #xCD)    ;INT
      (memory-u8-set! (real-pointer cs #x0001) #x20)    ;20h
      (memory-u16-set! (real-pointer cs #x0002) #xA000) ;top of memory?
      (memory-u32-set! (real-pointer cs #x0038) #xFFFFFFFF) ;previous PSP
      (memory-u8-set! (real-pointer cs #x0050) #xCD)        ;INT
      (memory-u8-set! (real-pointer cs #x0051) #x21)        ;21h
      (memory-u8-set! (real-pointer cs #x0052) #xCB)        ;RETF
      ;; PSP also contains the command line.
      (let* ((bv (string->utf8 (string-append command-line-arguments "\x0d;")))
             (len (fxmin #x7f (bytevector-length bv))))
        (memory-u8-set! (real-pointer cs #x0080) (fx- len 1))
        (copy-to-memory (real-pointer cs #x0081) bv 0 len)))))
