#!/usr/bin/env scheme-script
;; Copyright © 2017 Göran Weinholt <goran@weinholt.se>

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

;; "Hello, world!" for DOS.

(import (rnrs (6))
        (machine-code assembler x86))

(define (main filename)
  (let ((text `((%origin #x100)
                (%mode 16)
                (mov dx hello-msg)
                (mov ah #x09)
                (int #x21)              ;print $-terminated string
                (int #x20)))            ;exit
        (data `((%label hello-msg)
                (%vu8 ,(string->utf8 "Hello, world!\n$")))))
    ;; Assemble
    (call-with-port (open-file-output-port filename)
      (lambda (p)
        (let-values (((machine-code symbol-table) (assemble (append text data))))
          (put-bytevector p machine-code)
          (close-port p))))))

(main "hello.com")
