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

;; Loader for ELF images

(library (zabavno loader elf)
  (export detect-elf-image
          load-elf-image)
  (import (rnrs (6))
          (machine-code format elf)
          (zabavno cpu x86))

  ;; Detects the OS, machine type and endianness for the image.
  (define (detect-elf-image filename)
    (define (machine-symbol machine)
      (cond ((= machine EM-386) 'i386)
            (else
             (display "No support for this architecture: " (current-error-port))
             (display (cond ((assv machine elf-machine-names) => cdr)
                            (else machine))
                      (current-error-port))
             (newline (current-error-port))
             #f)))
    (define (os-symbol os-abi abi-version)
      ;; FIXME: Really detect Linux images...
      (cond ((and (= os-abi ELFOSABI-SYSV) (= abi-version 0)) 'linux)
            (else
             (display "No support for this OS: " (current-error-port))
             (display (list os-abi abi-version) (current-error-port))
             (newline (current-error-port))
             #f)))
    (define (endianness-symbol endianness)
      (cond ((= endianness ELFDATA2LSB) 'little)
            ((= endianness ELFDATA2MSB) 'big)
            (else
             (display "No support for this endianness: " (current-error-port))
             (display endianness (current-error-port))
             (newline (current-error-port))
             #f)))
    (cond ((not (is-elf-image? filename))
           (display "Not an ELF image: " (current-error-port))
           (write filename (current-error-port))
           (newline (current-error-port))
           (values #f #f #f))
          (else
           (let ((image (open-elf-image filename)))
             (values (machine-symbol (elf-image-machine image))
                     (os-symbol (elf-image-os-abi image)
                                (elf-image-abi-version image))
                     (endianness-symbol (elf-image-endianness image)))))))

  ;; Load an ELF image (should be statically linked, for now).
  (define (load-elf-image filename)
    (call-with-port (open-file-input-port filename)
      (lambda (p)
        (let ((image (open-elf-image p)))
          (for-each (lambda (segment)
                      (display segment)
                      (newline)
                      (cond ((eqv? (elf-segment-type segment) PT-LOAD)
                             (display "Loading...\n")
                             (set-port-position! p (elf-segment-offset segment))
                             (let ((bv (get-bytevector-n p (elf-segment-filesz segment))))
                               ;; FIXME: This should set up virtual memory mappings.
                               (copy-to-memory (elf-segment-vaddr segment) bv)))))
                    (elf-image-segments image))
          (let ((M (current-machine)))
            (machine-IP-set! M (elf-image-entry image)))
          
          ))))

)
