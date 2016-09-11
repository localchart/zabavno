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

(library (zabavno loader dos)
  (export detect-dos-exe-image
          load-dos-com-image
          load-dos-exe-image)
  (import (rnrs (6))
          (zabavno cpu x86))

  (define EXE-signatures (list (string->utf8 "MZ") (string->utf8 "ZM")))

  (define (detect-dos-exe-image image-port)
    (set-port-position! image-port 0)
    (let* ((signature (get-bytevector-n image-port 2))
           (header (get-bytevector-n image-port (* 2 16))))
      (and (not (eof-object? signature))
           (not (eof-object? header))
           (member signature EXE-signatures)
           (eqv? (bytevector-length header) (* 2 16)))))

  (define (get-u16 image-port)
    (bytevector-u16-ref (get-bytevector-n image-port 2) 0 (endianness little)))

  (define (setup-psp command-line-arguments)
    ;; Program Segment Prefix (PSP).
    (let ((ds (machine-DS (current-machine))))
      (copy-to-memory (real-pointer ds #x0000) (make-bytevector #x100 0))
      (memory-u8-set! (real-pointer ds #x0000) #xCD)    ;INT
      (memory-u8-set! (real-pointer ds #x0001) #x20)    ;20h
      (memory-u16-set! (real-pointer ds #x0002) #xA000) ;top of memory?
      (memory-u32-set! (real-pointer ds #x0038) #xFFFFFFFF) ;previous PSP
      (memory-u8-set! (real-pointer ds #x0050) #xCD)        ;INT
      (memory-u8-set! (real-pointer ds #x0051) #x21)        ;21h
      (memory-u8-set! (real-pointer ds #x0052) #xCB)        ;RETF
      ;; PSP also contains the command line.
      (let* ((bv (string->utf8 (string-append command-line-arguments "\x0d;")))
             (len (fxmin #x7f (bytevector-length bv))))
        (memory-u8-set! (real-pointer ds #x0080) (fx- len 1))
        (copy-to-memory (real-pointer ds #x0081) bv 0 len))))

  ;; Loads a DOS .com image into the current machine.
  (define (load-dos-com-image image-port command-line-arguments)
    (let ((M (current-machine))
          (cs #x07D2))
      ;; Load the .com image to cs:ip.
      (machine-CS-set! M cs)
      (machine-IP-set! M #x0100)
      (set-port-position! image-port 0)
      (let ((image (get-bytevector-n image-port (- #xffff #x100))))
        (when (machine-debug M)
          (display "; Loading DOS COM image of length " (current-error-port))
          (display (bytevector-length image) (current-error-port))
          (newline (current-error-port)))
        (copy-to-memory (real-pointer cs (machine-IP M)) image))
      ;; Set registers for starting the image.
      (machine-DS-set! M cs)
      (machine-ES-set! M cs)
      (machine-SS-set! M cs)
      ;; Return to address 0.
      (machine-SP-set! M #xFFFE)          ;stack at the end of the image
      (memory-u16-set! (real-pointer (machine-SS M) (machine-SP M)) #x0000)
      (setup-psp command-line-arguments)))

  ;; Load a DOS .exe image into the current machine.
  (define (load-dos-exe-image image-port command-line-arguments)
    (let ((M (current-machine))
          (psp-segment #x050)
          (load-segment #x060))
      (set-port-position! image-port 0)
      (let* ((signature (get-u16 image-port))
             (bytes-in-last-block (get-u16 image-port))
             (blocks-in-file (get-u16 image-port))
             (number-of-relocs (get-u16 image-port))
             (header-paragraphs (get-u16 image-port))
             (min-extra-paragraphs (get-u16 image-port))
             (max-extra-paragraphs (get-u16 image-port))
             (ss (get-u16 image-port))
             (sp (get-u16 image-port))
             (checksum (get-u16 image-port))
             (ip (get-u16 image-port))
             (cs (get-u16 image-port))
             (reloc-table-offset (get-u16 image-port))
             (overlay-number (get-u16 image-port)))
        (machine-ES-set! M psp-segment)
        (machine-DS-set! M psp-segment)
        (machine-CS-set! M (fxand (+ load-segment cs) #xffff))
        (machine-IP-set! M ip)
        (machine-SS-set! M (fxand (+ load-segment ss) #xffff))
        (machine-SP-set! M sp)
        ;; Load the image
        (let* ((load-module-size (- (- (* blocks-in-file 512)
                                       (if (zero? bytes-in-last-block)
                                           0
                                           (- 512 bytes-in-last-block)))
                                    (* header-paragraphs 16))))
          (set-port-position! image-port (* header-paragraphs 16))
          (copy-to-memory (real-pointer (machine-CS M) (machine-IP M))
                          (make-bytevector load-module-size 0)) ;XXX: needed?
          (copy-to-memory (real-pointer (machine-SS M) (machine-SP M))
                          (make-bytevector (fx- #x10000 (machine-SP M)) 0))
          (copy-to-memory (real-pointer load-segment #x0000)
                          (get-bytevector-n image-port load-module-size)))
        ;; Process relocations
        (set-port-position! image-port reloc-table-offset)
        (do ((i 0 (fx+ i 1)))
            ((eqv? i number-of-relocs))
          (let* ((reloc-offset (get-u16 image-port))
                 (reloc-segment (get-u16 image-port)))
            (let ((addr (real-pointer (fx+ load-segment reloc-segment)
                                      reloc-offset)))
              (memory-u16-set! addr (fx+ (memory-u16-ref addr) load-segment)))))
        (setup-psp command-line-arguments)))))
