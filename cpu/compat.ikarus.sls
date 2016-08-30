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

;; Compatibility for Ikarus 0.0.4-rc1+.

(library (zabavno cpu compat)
  (export bitwise-rotate-bit-field)
  (import (except (rnrs) bitwise-rotate-bit-field))

  (define (bitwise-rotate-bit-field n start end count)
    ;; Straight out of r6rs-lib.
    (define who 'bitwise-rotate-bit-field)
    (assert (<= 0 start end))
    (assert (not (negative? count)))
    (let ((width (- end start)))
      (if (positive? width)
          (let* ((count (mod count width))
                 (field0 (bitwise-bit-field n start end))
                 (field1 (bitwise-arithmetic-shift-left field0 count))
                 (field2 (bitwise-arithmetic-shift-right field0 (- width count)))
                 (field (bitwise-ior field1 field2)))
            (bitwise-copy-bit-field n start end field))
          n))))
