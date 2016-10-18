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

;; Very basic Intel ICH8 emulation.

(library (zabavno hardware ich8)
  (export init)
  (import (rnrs (6))
          (zabavno cpu x86))

  (define (init)
    (define M (current-machine))

    ;; NMI Status and Control Register.
    (define SERR_NMI_STS  #b10000000)
    (define IOCHK_NMI_STS #b01000000)
    (define TMR2_OUT_STS  #b00100000)
    (define REF_TOGGLE    #b00010000)
    (define IOCHK_NMI_EN  #b00001000)
    (define PCI_SERR_EN   #b00000100)
    (define SPKR_DAT_EN   #b00000010)
    (define TIM_CNT2_EN   #b00000001)
    (define nmi-status 0)
    (define NMI_SC
      (case-lambda
        ((_port _size)
         ;; This simulates the refresh cycle toggle, which is used in some programs for timing.
         (set! nmi-status (fxxor nmi-status REF_TOGGLE))
         nmi-status)
        ((_port _size value)
         #t)))

    ;; Fast A20 and Init Register
    (define ALT_A20_GATE #b10)
    (define INIT_NOW     #b01)
    (define PORT92
      (case-lambda
        ((_port _size)                  ;get the A20 gate status
         (if (machine-A20-gate-control)
             ALT_A20_GATE
             0))
        ((_port _size value)
         (machine-A20-gate-control
          (not (fxzero? (fxand value ALT_A20_GATE))))
         (unless (fxzero? (fxand value INIT_NOW))
           (error 'PORT92 "TODO: reboot")))))

    (machine-hook-i/o-port! M #x61 8 NMI_SC)
    (machine-hook-i/o-port! M #x92 8 PORT92)))
