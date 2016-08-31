#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright © 2014, 2016 Göran Weinholt <goran@weinholt.se>

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
;; and DOSEMU 1.4.0.8.

(import (zabavno cpu x86)
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
;; registers set as they were in the machine (except the flags).
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
        (machine-trace-set! m #t)
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
          (with-output-to-port
           port (lambda ()
                  (with-error-to-port
                   port (lambda ()
                          (let ((ret (thunk)))
                            (values ret (extract)))))))
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

(define regs '(AX BX CX DX SP BP SI DI DS ES SS CS IP))

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
                             (format #t "Register ~a differs. #x~x != #x~x (zabavno vs dosemu).~%"
                                     reg rz rd))
                           (eqv? rz rd)))
                       regs reg-getters))
             (same-flags (= fz fd)))
        (unless same-flags
          ;; It can be OK that the flags differ. Some flags are
          ;; undefined after some operations. TODO: keep track of what
          ;; flags are undefined.
          (display "Warning: FLAGS differs.")
          (print-flags fz)
          (display " !=")
          (print-flags fd)
          (display " (zabavno vs dosemu).\n"))
        (unless same-regs
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
 ;; SHRD.
 '((mov ax #xabcd) (mov dx #x0123) (shrd ax dx 4))
 '((mov ax #xabcd) (mov dx #x0123) (mov cl 5) (shrd ax dx cl))

 ;; SETcc.
 '((mov ax #xabcd) (stc) (setc al))
 '((mov ax #xabcd) (clc) (setc al))

 ;; Byte registers
 '((mov al 0) (mov ah 1) (mov bl 2) (mov bh 3)
   (mov cl 4) (mov ch 5) (mov dl 6) (mov dh 7))

 ;; IDIV.
 '((mov ax 13) (cwd) (mov bx 4) (idiv bx))
 '((mov ax -13) (cwd) (mov bx 4) (idiv bx))
 '((mov ax 13) (cwd) (mov bx -4) (idiv bx))
 '((mov ax -13) (cwd) (mov bx -4) (idiv bx))

 '((mov ax 13) (cwd) (mov bx 4) (idiv bl))
 '((mov ax -13) (cwd) (mov bx 4) (idiv bl))
 '((mov ax 13) (cwd) (mov bx -4) (idiv bl))
 '((mov ax -13) (cwd) (mov bx -4) (idiv bl))

 ;; CMPS.
 '((mov di #x100)
   (mov si di)
   (%u8 #xa6))
 '((mov di #x100)
   (mov si di)
   (mov cx 1)
   (%u8 #xf3 #xa6))
 '((mov di #x100)
   (mov si di)
   (mov cx 0)
   (%u8 #xf3 #xa6))

 ;; DAA, DAS
 '((mov al #x79) (mov bl #x35) (add al bl) (daa))
 '((mov al #x79) (mov bl #x35) (add al bl) (mov al #x2E) (mov bl #x35) (daa))
 '((mov al #x35) (mov bl #x47) (sub al bl) (das))

 ;; SAR.
 '((mov al #b10000000)
   (sar al 1))
 '((mov al #b01111111)
   (mov cl 2)
   (sar al cl))

 ;; SHR.
 '((mov ax #b0)
   (shr ax 1))
 '((mov ax #b1)
   (shr ax 1))
 '((mov ax #b10000000)
   (shr ax 1))

 ;; RCL.
 '((mov al #b10000000)
   (stc)
   (rcl al 1))
 '((mov al #b10000000)
   (clc)
   (rcl al 1))
 '((mov al #b10001111)
   (stc)
   (rcl al 2))

 ;; RCR.
 '((mov al #b10000000)
   (stc)
   (rcr al 1))
 '((mov al #b10000000)
   (clc)
   (rcr al 1))
 '((mov al #b10001111)
   (stc)
   (rcr al 2))

 ;; ROL.
 '((mov al #b10000000)
   (rol al 1))
 '((mov al #b10100000)
   (rol al 2))

 ;; ROR.
 '((mov al #b10000000)
   (ror al 1))
 '((mov al #b00000001)
   (ror al 1))
 '((mov al #b10000001)
   (ror al 1))
 '((mov ah #b10000001)
   (ror ah 2))

 ;; NEG.
 '((mov al #b00000001)
   (neg al))
 `((mov al ,(fxand -127 #xff))
   (neg al))
 `((mov al 0)
   (neg al))

 ;; NOT.
 '((mov al #b00000000)
   (not al))
 `((mov al #b11111111)
   (not al))

 ;; SHL.
 '((mov ax #b10000000)
   (shl ax 1))
 '((mov ax #b11000000)
   (shl ax 1))
 '((mov ax #b10000000)
   (mov cl 2)
   (shl ax cl))

 ;; TEST.
 '((mov ax #x0101)
   (test ax #x0101))
 '((mov al #x10)
   (test al #x01))

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

 ;; MOVZX
 '((mov al #x7f)
   (movzx ax al))
 '((mov al #x80)
   (movzx bx al))

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

 ;; F6 F7 TEST
 '((mov bx #x1200) (test bh #b10))
 '((mov bx #x1200) (test bh #b01))

 ;; F6 F7 MUL
 '((mov ax 6) (mov dx 7) (mul dx))
 '((mov ax 6) (mov dx 7) (mul dl))
 '((mov ax 3) (mul ax))
 '((mov ah 88) (mov al 2) (mul ah))

 ;; IMUL
 '((mov ax 6) (xor dx dx) (imul dx ax 7))
 '((mov ax -6) (xor dx dx) (imul dx ax -7))
 '((mov ax -32768) (xor dx dx) (imul dx ax -128))
 '((mov dx 6) (mov ax 7) (imul dx ax))
 '((mov dx 6) (mov ax -7) (cwd) (imul ax))
 '((mov al 6) (mov bl -7) (imul bl))

 ;; F6 F7 DIV
 '((mov ax 42) (cwd) (mov cx 6) (div cx))
 '((mov ax 42) (cwd) (mov cx 6) (div cl))
 '((mov ax #x0200)
   (mov bx 16)
   (push bx)
   (mov bp sp)
   (%u8 #xF6 #x76 #x00) #;(div (mem8+ bp)))

 ;; XLATB.
 '((mov di #x4042)
   (push di)
   (mov bx sp)
   (mov al 1)
   (xlatb)
   (pop di))
 )
