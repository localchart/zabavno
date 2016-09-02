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

;; Compatibility code for GNU Guile.

;; May require this in ~/.guile:

;; (set! %load-extensions (append '(".guile.sls" ".sls") %load-extensions))
;; (read-enable 'r6rs-hex-escapes)
;; (read-enable 'hungry-eol-escapes)

(library (zabavno firmware compat)
  (export input-port-ready?
          get-current-time
          get-current-date)
  (import (rnrs)
          (only (srfi srfi-19)
                current-time current-date
                date-year date-month date-day
                date-hour date-minute date-second date-nanosecond)
          (rename (guile)
                  (char-ready? input-port-ready?)))

  (define (get-current-time)
    (let ((date (current-date))
          (dst? #f))
      (values  (date-hour date)
               (date-minute date)
               (date-second date)
               (date-nanosecond date)
               dst?)))

  (define (get-current-date)
    (let ((date (current-date)))
      (values (date-year date)
              (date-month date)
              (date-day date)))))
