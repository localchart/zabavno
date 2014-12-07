#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright © 2014 Göran Weinholt <goran@weinholt.se>

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

;; This program is written for GNU Guile. Tested with GNU Guile 2.0.11
;; and DOSEMU 1.4.0.1.

(import (zabavno x86)
        (weinholt assembler x86)
        (rnrs))

(define *capture-stdout* #t)

(use-modules (ice-9 popen)
             (ice-9 expect)
             (ice-9 rdelim))

(define (start-dosemu)
  (setenv "TERM" "vt100")
  (open-input-output-pipe "dosemu -3 -dumb"))

(define (start-debug p)
  (let ((expect-port p)
        (expect-char-proc display))
    (expect-strings
     ("^[ABCDEFG]:[^>]>"
      (write-line "debug\r" p)))
    (wait-prompt p)))

(define (wait-prompt p)
  (let ((expect-port p)
        (expect-char-proc display))
    (expect-strings
     ("^-" #f))))

;; Send a program byte-by-byte to the debugger.
(define (send-code p bv)
  (write-line "e 100\r" p)
  (do ((i 0 (+ i 1)))
      ((= i (bytevector-length bv))
       (write-line "\r" p)
       (let ((expect-port p)
             (expect-char-proc display))
         (expect-strings
          ("^-" #t))))
    (let ((expect-port p)
          (expect-char-proc display))
      (expect-strings
       (" [0-9A-Fa-f][0-9A-Fa-f]\\."
        (let ((byte (bytevector-u8-ref bv i)))
          (display (number->string byte 16) p)
          (when (< i (- (bytevector-length bv) 1))
            (display " " p))
          (flush-output-port p)))))))

;; Assemble and send a program to the debugger.
(define (send-program p program)
  (let-values (((code labels)
                (assemble `((%mode 16)
                            ,@program
                            ;; Guard. Will exit dosemu.
                            (icebp)))))
    (send-code p code)))

;; Trace program execution in the debugger. Returns a machine with the
;; registers set as they were in the machine (expect the flags).
(define (trace p address count)
  (write-line (format #f "t =~a ~a\r" (number->string address 16) count) p)
  (let ((m (make-machine)))
    (let ((expect-port p)
          (expect-char-proc display))
      (let lp ()
        (expect-strings
         ;; Get the echo.
         ("t =" (lp))
         ;; Save registers
         ("([ABCD]X|[SB]P|[SD]I|[DESC]S|IP)=([^ ]+)[^0-9A-Fa-f]" =>
          (lambda (_ reg value)
            (let ((reg (string->symbol (string-upcase reg)))
                  (value (string->number value 16)))
              (case reg
                ((AX) (machine-AX-set! m value))
                ((BX) (machine-BX-set! m value))
                ((CX) (machine-CX-set! m value))
                ((DX) (machine-DX-set! m value))
                ((SP) (machine-SP-set! m value))
                ((BP) (machine-BP-set! m value))
                ((SI) (machine-SI-set! m value))
                ((DI) (machine-DI-set! m value))
                ((DS) (machine-DS-set! m value))
                ((ES) (machine-ES-set! m value))
                ((SS) (machine-SS-set! m value))
                ((CS) (machine-CS-set! m value))
                ((IP) (machine-IP-set! m value)))
              (lp))))
         ;; Back at the prompt.
         ("^-"
          m))))))

;; Reads the flags register (instead of trying to decipher debug's
;; syntax).
(define (get-flags p)
  (send-program p '((push ax)
                    (pushfw)
                    (pop ax)))
  (let ((m (trace p #x100 3)))
    (send-program p '((pop ax)))
    (trace p #x100 1)
    (machine-AX m)))

(define (exit-debug p)
  (write-line "q\r" p)
  (let ((expect-port p)
        (expect-char-proc display))
    (expect-strings
     ("^[ABCDEFG]:"
      #f))))

(define (exit-dosemu p)
  (write-line "exitemu\r" p)
  (let ((expect-port p)
        (expect-char-proc display))
    (expect-strings)))

;; Assemble some code, send it to dosemu, run it and return the
;; registers as a machine.
(define (dosemu-emulate-program p program)
  (send-program p program)
  (let ((m (trace p #x100 (length program))))
    (machine-FLAGS-set! m (get-flags p))
    m))

;; Runs a program in the emulator.
(define (zabavno-emulate-program m program)
  (let-values (((code labels)
                (assemble `((%mode 16)
                            ,@program
                            ;; Stop the emulator.
                            (icebp)))))
    (with-machine m
      (lambda ()
        (machine-debug-set! m #t)
        ;; Copy in the code.
        (machine-IP-set! m #x100)
        (let ((addr (+ (* (machine-CS m) 16)
                       (machine-IP m))))
          (copy-to-memory addr code))
        (machine-run)
        m))))

;; Run an assembler program in the emulator and dosemu. Return the two
;; machines and the emulator outputs.
(define (run p program)
  (define (capture thunk)
    (let-values (((port extract) (open-string-output-port)))
      (if *capture-stdout*
          (with-output-to-port port
                               (lambda ()
                                 (let ((ret (thunk)))
                                   (values ret (extract)))))
          (let ((ret (thunk)))
            (values ret "")))))
  ;; Make a copy of debug's original registers.
  (let ((m (capture (lambda () (dosemu-emulate-program p '((nop)))))))
    ;; Turn off TF.
    (machine-FLAGS-set! m (fxand (machine-FLAGS m)
                                 (fxnot flag-TF)))
    ;; Run the program, capturing output.
    (let-values (((mz oz) (capture (lambda () (zabavno-emulate-program m program))))
                 ((md od) (capture (lambda () (dosemu-emulate-program p program)))))
      (values mz oz md od))))

(define flags-mask
  (fxior flag-OF flag-SF flag-ZF flag-AF flag-PF flag-CF))

(define reg-getters
  (list machine-AX machine-BX machine-CX machine-DX machine-SP machine-BP
        machine-SI machine-DI machine-DS machine-ES machine-SS machine-CS
        machine-IP))

(define regs '(AX BX CX DX SP BP
                  SI DI DS ES SS CS IP))

(define (test p program)
  (format #t "~%Testing ~a~%" program)
  (let-values (((mz oz md od) (run p program)))
    (define (show-output prefix str)
      (display (string-join (string-split str #\linefeed)
                            (string-append "\n" prefix) 'prefix))
      (newline))
    (let ((fz (fxand (machine-FLAGS mz) flags-mask))
          (fd (fxand (machine-FLAGS md) flags-mask)))
      (let* ((same-regs
              (for-all (lambda (reg getter)
                         (let ((rz (getter mz))
                               (rd (getter md)))
                           (unless (= rz rd)
                             (format #t "Register ~a differs. #x~x != #x~x.~%" reg rz rd))
                           (eqv? rz rd)))
                       regs reg-getters))
             (same-flags (= fz fd)))
        (unless same-flags
          ;; It can be OK that the flags differ. Some flags are
          ;; undefined after some operations. TODO: keep track of what
          ;; flags are undefined.
          (format #t "FLAGS differs. #b~a != #b~a (#b~a).~%" (number->string fz 2)
                  (number->string fd 2) (number->string (fxxor fd fz) 2)))
        (unless (and same-regs same-flags)
          (show-output "D: " od)
          (show-output "Z: " oz))))))

;;; Run tests

(define (run-tests . program*)
  (define p)
  (dynamic-wind
    (lambda ()
      (set! p (start-dosemu))
      (start-debug p))
    (lambda ()
      (for-each (lambda (program)
                  (test p program))
                program*))
    (lambda ()
      (exit-debug p)
      (exit-dosemu p)
      (close-pipe p)
      (set! p #f))))

;; Some of these tests are waiting for support in the assembler, or
;; for the emulator to save which flags are currently undefined.

(run-tests
 ;; SHR.
 ;; '((mov ax #b0)
 ;;   (shr ax 1))

 ;; '((mov ax #b1)
 ;;   (shr ax 1))

 ;; '((mov ax #b10000000)
 ;;   (shr ax 1))

 ;; LEA
 ;; '((mov si #x1) (mov bp #x1000) (lea ax (mem+ si bp)))
 ;; '((mov si #x1) (mov bp #x1000) (lea si (mem+ si bp)))

 ;; 98, 99. Sign-extension.
 '((mov al #x7f) (cbw))
 '((mov al #x80) (cbw))
 '((mov ax #x7fff) (cwd))
 '((mov ax #x8000) (cwd))

 ;; MOV with segment registers
 '((xor ax ax) (mov ax cs))
 '((mov ax #x700) (mov es ax))

 ;; PUSH, POP
 '((push #x100) (pop ax))
 '((push #x-10) (pop di))

 ;; Group 1.
 '((xor bx bx) (add bl 1))              ;Eb Ib
 '((xor bx bx) (add bx 1))              ;Ev Iz
 '((xor bx bx) (add ebx 1))             ;Ev Iz
 '((xor bx bx) (add bx -1))             ;Ev IbS

 ;; CLC, STC, CMC
 '((clc))
 '((stc))
 '((clc) (cmc))
 '((stc) (cmc))

 )
