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

;; Default fallbacks when no compatibility library has been loaded.

(library (zabavno firmware compat)
  (export input-port-ready?
          get-current-time
          get-current-date
          port-length)
  (import (rnrs (6)))

  (define input-port-ready?
    (let ((warned? #f))
      (lambda (port)
        (unless warned?
          (display "No (zabavno firmware compat) library has been provided, the keyboard will not work\n"
                   (current-error-port))
          (set! warned? #t))
        #f)))

  (define (get-current-time)
    (values 13 15 00 0 #f))

  (define (get-current-date)
    (values 2016 8 27))

  (define (port-length port)
    ;; There is no native procedure for this in R6RS. It should be
    ;; O(1), and this is O(log n).
    (define (read! port position bv)
      (guard (exn
              (else (eof-object)))
        (set-port-position! port position)
        (get-bytevector-n! port bv 0 (bytevector-length bv))))
    (let ((old-position (port-position port))
          (bv (make-bytevector (expt 2 12))))
      (let lp ((position 0) (i 12))
        ;; Get the upper limit of the next binary search.
        (let ((n (read! port position bv)))
          (cond ((eof-object? n)
                 (if (= position 0)
                     0
                     ;; Now do the binary search.
                     (let lp* ((lower (+ (div position 2) 1))
                               (upper (- position 1)))
                       (let ((middle (+ lower (div (- upper lower) 2))))
                         (let ((n (read! port middle bv)))
                           (cond ((eof-object? n)
                                  (lp* lower (- middle 1)))
                                 ((< n (bytevector-length bv))
                                  ;; Found the size.
                                  (set-port-position! port old-position)
                                  (+ middle n))
                                 (else
                                  (lp* (+ middle 1) upper))))))))
                ((< n (bytevector-length bv))
                 ;; Found the size.
                 (set-port-position! port old-position)
                 (+ position n))
                ((> position (expt 2 64))
                 (error 'port-length "Unable to determine port length" port))
                (else
                 (lp (expt 2 i) (+ i 1)))))))))
