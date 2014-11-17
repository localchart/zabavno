;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2014 Göran Weinholt <goran@weinholt.se>

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the \"Software\"),
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

;; Emulates an Intel 80386 in real address mode.

(library (zabavno x86)
  (export machine-run
          machine?
          make-machine
          machine-debug machine-debug-set!
          machine-RAM machine-RAM-set!
          machine-read-hook machine-read-hook-set!
          machine-write-hook machine-write-hook-set!
          machine-in-hook machine-in-hook-set!
          machine-out-hook machine-out-hook-set!
          machine-AX machine-AX-set!
          machine-CX machine-CX-set!
          machine-DX machine-DX-set!
          machine-BX machine-BX-set!
          machine-SP machine-SP-set!
          machine-BP machine-BP-set!
          machine-SI machine-SI-set!
          machine-DI machine-DI-set!
          machine-ES machine-ES-set!
          machine-CS machine-CS-set!
          machine-SS machine-SS-set!
          machine-DS machine-DS-set!
          machine-FS machine-FS-set!
          machine-GS machine-GS-set!
          machine-IP machine-IP-set!
          machine-FLAGS machine-FLAGS-set!
          memory-u8-ref memory-u16-ref memory-u32-ref
          memory-s8-ref memory-s16-ref memory-s32-ref
          memory-u8-set! memory-u16-set! memory-u32-set!

          with-machine
          current-machine
          copy-to-memory copy-from-memory)
  (import (rnrs)
          (rnrs eval))

  ;; Fill memory with this u8 value.
  (define DEFAULT-MEMORY-FILL #xF1)

  (define pretty-print
    (lambda (x) (display x) (newline)))

  (define code-env (environment '(rnrs)))

  ;; For debug printing.
  (define disassemble
    (guard (exn
            (else #f))
      (let ((get-instruction
             (eval 'get-instruction (environment '(weinholt disassembler x86)))))
        (lambda (bv)
          (call-with-port (open-bytevector-input-port bv)
            (lambda (p) (get-instruction p 16 #f)))))))

  ;; Import cp0 for development.
  (define expand/optimize
    (letrec ((try-import (lambda (name lib)
                           (guard (exn (else #f))
                             (eval name (environment lib))))))
      (or (try-import 'expand/optimize '(ikarus))
          (try-import 'expand/optimize '(scheme))
          (try-import 'expand/optimize '(mancer))
          (lambda (x) x))))

  (define-record-type machine
    (nongenerative machine-89ee4b42-da63-4afb-b294-b15350aedbc0)
    (sealed #t)
    (fields (mutable debug)
            ;; Memory and I/O
            (mutable RAM)
            (mutable read-hook)
            (mutable write-hook)
            (mutable in-hook)
            (mutable out-hook)
            ;; GP registers
            (mutable AX)
            (mutable CX)
            (mutable DX)
            (mutable BX)
            (mutable SP)
            (mutable BP)
            (mutable SI)
            (mutable DI)
            ;; Segment registers
            (mutable ES)
            (mutable CS)
            (mutable SS)
            (mutable DS)
            (mutable FS)
            (mutable GS)
            ;; Other stuff
            (mutable IP)
            (mutable FLAGS))
    (protocol
     (lambda (p)
       (lambda ()
         (p #f
            ;; Memory and I/O
            (make-empty-memory)
            (lambda (address size) #f)
            (lambda (address size value) #f)
            (lambda (port size) (- (expt 2 size) 1))
            (lambda (port size value) #f)
            ;; GP registers
            0 0 0 0 0 0 0 0
            ;; Segment registers
            0 0 0 0 0 0
            ;; Other stuff
            0 (fxior flags-always-set flag-IF))))))

  (define *current-machine*)
  (define RAM)
  (define *read-hook*)
  (define *write-hook*)

  (define (with-machine M thunk)
    ;;TODO: dynamic-wind
    (set! *current-machine* M)
    (set! RAM (machine-RAM M))
    (set! *read-hook* (machine-read-hook M))
    (set! *write-hook* (machine-write-hook M))
    (thunk))

  (define (current-machine)
    *current-machine*)

  ;; Copy an instruction from memory.
  (define (copy-inst scaled-cs ip)
    (do ((ret (make-bytevector 15))
         (i 0 (fx+ i 1)))
        ((fx=? i (bytevector-length ret))
         ret)
      (bytevector-u8-set! ret i (memory-u8-ref
                                 (fx+ scaled-cs
                                      (fxand #xffff (fx+ ip i)))))))

  (define (print . x) (for-each display x) (newline))

  (define (print* . x) (for-each display x))

  (define hex
    (case-lambda
      ((x)
       (hex x 1))
      ((x w)
       (let ((s (number->string x 16)))
         (string-append (make-string (max 0 (- w (string-length s)))
                                     #\0)
                        s)))))

  (define (trunc n eos)
    (case eos
      ((16) (fwand n #xffff))
      ((8) (fwand n #xff))
      (else (fwand n #xffffffff))))

  (define (segment-selector n)
    ;; The emulator runs with all segment selectors scaled for easy
    ;; addition.
    (fxarithmetic-shift-right n 4))

  (define (sign-extend n eos)
    ;; Takes an unsigned value and recovers the sign.
    (if (> n (- (expt 2 (- eos 1)) 1))
        (- n (expt 2 eos))
        n))

;;; Bytevector operators for aligned little-endian data

  (define bytevector-u16le-ref
    (if (eq? (native-endianness) (endianness little))
        bytevector-u16-native-ref
        (lambda (bv i)
          (bytevector-u16-ref bv i (endianness little)))))

  (define bytevector-u32le-ref
    (if (eq? (native-endianness) (endianness little))
        bytevector-u32-native-ref
        (lambda (bv i)
          (bytevector-u32-ref bv i (endianness little)))))

  (define bytevector-u16le-set!
    (if (eq? (native-endianness) (endianness little))
        bytevector-u16-native-set!
        (lambda (bv i v)
          (bytevector-u16-set! bv i v (endianness little)))))

  (define bytevector-u32le-set!
    (if (eq? (native-endianness) (endianness little))
        bytevector-u32-native-set!
        (lambda (bv i v)
          (bytevector-u32-set! bv i v (endianness little)))))

;;; Fixnum operations for when the fixnum-width is wider than 32 bits

  (define-syntax define-fx
    (lambda (x)
      (syntax-case x ()
        ((_ name fxname bitwise-name)
         #'(define name
             (if (> (fixnum-width) 32)
                 fxname bitwise-name))))))

  (define-fx fwand fxand bitwise-and)
  (define-fx fwxor fxxor bitwise-xor)
  (define-fx fwior fxior bitwise-ior)
  (define-fx fwnot fxnot bitwise-not)
  (define-fx fw+ fx+ +)
  (define-fx fw- fx- -)
  (define-fx fwbit-set? fxbit-set? bitwise-bit-set?)
  (define-fx fwasr fxarithmetic-shift-right bitwise-arithmetic-shift-right)
  (define-fx fwasl fxarithmetic-shift-left bitwise-arithmetic-shift-left)
  (define (fwzero? x) (eqv? x 0))

  (define fxasr fxarithmetic-shift-right)
  (define fxasl fxarithmetic-shift-left)

;;; Registers

  (define idx-AX 0)
  (define idx-CX 1)
  (define idx-DX 2)
  (define idx-BX 3)
  (define idx-SP 4)
  (define idx-BP 5)
  (define idx-SI 6)
  (define idx-DI 7)

  (define reg-names '#(AX CX DX BX SP BP SI DI))

;;; Flags

  (define (print-flags fl)
    (define flag*
      '(31 30 29 28 27 26 25 24 23 22 ID VIP VIF AC VM RF
           15 NT IOPL1 IOPL0 OF DF IF TF SF ZF 5 AF 3 PF 1 CF))
    (do ((m (expt 2 31) (fxarithmetic-shift-right m 1))
         (flag* flag* (cdr flag*)))
        ((fxzero? m))
      (unless (fxzero? (fxand m fl))
        (display #\space)
        (display (car flag*)))))

  (define flag-OF (expt 2 11))          ;overflow
  (define flag-SF (expt 2 7))           ;sign
  (define flag-ZF (expt 2 6))           ;zero
  (define flag-AF (expt 2 4))           ;adjust
  (define flag-PF (expt 2 2))           ;parity
  (define flag-CF (expt 2 0))           ;carry
  (define flag-DF (expt 2 10))          ;direction
  (define flag-IF (expt 2 9))           ;interrupt enable

  (define flag-TF (expt 2 8))
  (define flag-AC (expt 2 18))

  ;; "Identification of Earlier IA-32 Processors" in IA32 SDM Vol 1.
  ;; Should match an 80386.
  (define flags-never-set (bitwise-ior (expt 2 21) ;no CPUID
                                       (expt 2 18) ;386
                                       (expt 2 15)
                                       ;; IOPL 0 forever
                                       (expt 2 13)
                                       (expt 2 12)))

  (define flags-always-set 2)

;;; Memory

  ;; Debug memory
  (define-syntax mtrace
    (lambda (x)
      (syntax-case x ()
        #;
        ((_ . x*)
         #'(begin
             (for-each display (list . x*))
             (newline)))
        ((_ . x*)
         #'(values)))))

  ;; Instead of using a big bytevector for the RAM, it is split into
  ;; pages that are referenced from a vector.
  (define page-bits 12)
  (define page-size (expt 2 page-bits))

  (define (print-memory addr len)
    (do ((i 0 (fx+ i 1)))
        ((fx=? i len)
         (newline))
      (let ((v (memory-u8-ref (fx+ addr i))))
        (display #\space)
        (when (fx<? v #x10)
          (display #\0))
        (display (hex v)))))

  (define (copy-from-memory addr len)
    (do ((ret (make-bytevector len))
         (i 0 (fx+ i 1)))
        ((fx=? i len) ret)
      (bytevector-u8-set! ret i (memory-u8-ref (fx+ addr i)))))

  (define copy-to-memory
    (case-lambda
      ((addr bv)
       (copy-to-memory addr bv 0 (bytevector-length bv)))
      ((addr bv start count)
       (do ((i 0 (fx+ i 1)))
           ((fx=? i count))
         (memory-u8-set! (fx+ addr i)
                         (bytevector-u8-ref bv (fx+ start i)))))))

  (define (make-empty-memory)
    (make-vector (/ (* 1024 1024) page-size) #f))

  (define (RAM-page addr)
    (fxarithmetic-shift-right addr page-bits))

  (define (RAM-page-offset addr)
    (fxand addr (fx- page-size 1)))

  (define (memory-u8-ref addr)
    (let ((x (cond ((*read-hook* addr 'u8))
                   ((vector-ref RAM (RAM-page addr)) =>
                    (lambda (page)
                      (bytevector-u8-ref page (RAM-page-offset addr))))
                   (else DEFAULT-MEMORY-FILL))))
      (mtrace "memory-u8-ref: " (hex addr) " => " (hex x))
      x))

  (define (memory-u16-ref addr)
    (let ((x (cond ((*read-hook* addr 'u16))
                   ((fxzero? (fxand addr #b1))
                    (cond ((vector-ref RAM (RAM-page addr)) =>
                           (lambda (page)
                             (bytevector-u16le-ref page (RAM-page-offset addr))))
                          (else (* DEFAULT-MEMORY-FILL #x0101))))
                   (else
                    (fxior (memory-u8-ref addr)
                           (fxasl (memory-u8-ref (fx+ addr 1)) 8))))))
      (mtrace "memory-u16-ref: " (hex addr) " => " (hex x))
      x))

  (define (memory-u32-ref addr)
    (let ((x (cond ((*read-hook* addr 'u32))
                   ((fxzero? (fxand addr #b11))
                    (cond ((vector-ref RAM (RAM-page addr)) =>
                           (lambda (page)
                             (bytevector-u32le-ref page (RAM-page-offset addr))))
                          (else (* DEFAULT-MEMORY-FILL #x01010101))))
                   (else
                    (fwior (memory-u8-ref addr)
                           (fxasl (memory-u8-ref (fx+ addr 1)) 8)
                           (fwasl (memory-u8-ref (fx+ addr 2)) 16)
                           (fwasl (memory-u8-ref (fx+ addr 3)) 24))))))
      (mtrace "memory-u32-ref: " (hex addr) " => " (hex x))
      x))

  (define (memory-s8-ref addr)
    (sign-extend (memory-u8-ref addr) 8))

  (define (memory-s16-ref addr)
    (sign-extend (memory-u16-ref addr) 16))

  (define (memory-s32-ref addr)
    (sign-extend (memory-u32-ref addr) 32))

  (define (memory-u8-set! addr value)
    (mtrace "memory-u8-set!: " (hex addr) " " (hex value))
    (cond ((*write-hook* addr value 'u8))
          ((vector-ref RAM (RAM-page addr)) =>
           (lambda (page)
             (bytevector-u8-set! page (RAM-page-offset addr) value)))
          (else
           (let ((page (make-bytevector page-size DEFAULT-MEMORY-FILL)))
             (vector-set! RAM (RAM-page addr) page)
             (bytevector-u8-set! page (RAM-page-offset addr) value)))))

  (define (memory-u16-set! addr value)
    (mtrace "memory-u16-set!: " (hex addr) " " (hex value))
    (cond ((*write-hook* addr value 'u16))
          ((fxzero? (fxand addr #b1))
           (cond ((vector-ref RAM (RAM-page addr)) =>
                  (lambda (page)
                    (bytevector-u16le-set! page (RAM-page-offset addr) value)))
                 (else
                  (let ((page (make-bytevector page-size DEFAULT-MEMORY-FILL)))
                    (vector-set! RAM (RAM-page addr) page)
                    (bytevector-u16le-set! page (RAM-page-offset addr) value)))))
          (else
           (memory-u8-set! addr (fxand value #xff))
           (memory-u8-set! (fx+ addr 1) (fxasr value 8)))))

  (define (memory-u32-set! addr value)
    (mtrace "memory-u32-set!: " (hex addr) " " (hex value))
    (cond ((*write-hook* addr value 'u32))
          ((fxzero? (fxand addr #b11))
           (cond ((vector-ref RAM (RAM-page addr)) =>
                  (lambda (page)
                    (bytevector-u32le-set! page (RAM-page-offset addr) value)))
                 (else
                  (let ((page (make-bytevector page-size DEFAULT-MEMORY-FILL)))
                    (vector-set! RAM (RAM-page addr) page)
                    (bytevector-u32le-set! page (RAM-page-offset addr) value)))))
          (else
           (memory-u8-set! addr (fwand value #xff))
           (memory-u8-set! (fx+ addr 1) (fwand (fwasr value 8) #xff))
           (memory-u8-set! (fx+ addr 2) (fxand (fwasr value 16) #xff))
           (memory-u8-set! (fx+ addr 3) (fwasr value 24)))))

;;; For reading instructions

  (define-syntax with-instruction-u8*
    (lambda (x)
      (syntax-case x (<-)
        ((_ ((byte* <- cs* ip*) ...) body ...)
         #'(with-instruction-immediate* ((byte* <- cs* ip* 8) ...)
             body ...)))))

  (define-syntax with-instruction-s8*
    (lambda (x)
      (syntax-case x (<-)
        ((_ ((byte* <- cs* ip*) ...) body ...)
         #'(with-instruction-immediate-sx* ((byte* <- cs* ip* 8) ...)
             body ...)))))

  (define-syntax with-instruction-u16*
    (lambda (x)
      (syntax-case x (<-)
        ((_ ((byte* <- cs* ip*) ...) body ...)
         #'(with-instruction-immediate* ((byte* <- cs* ip* 16) ...)
             body ...)))))

  (define-syntax with-instruction-s16*
    (lambda (x)
      (syntax-case x (<-)
        ((_ ((byte* <- cs* ip*) ...) body ...)
         #'(with-instruction-immediate-sx* ((byte* <- cs* ip* 16) ...)
             body ...)))))

  ;; Read 8, 16 or 32 bits from the instruction stream, depending on the
  ;; effective operand size.
  (define-syntax with-instruction-immediate*
    (lambda (x)
      (syntax-case x (<-)
        ((_ ((word <- cs ip eos) (word* <- cs* ip* eos*) ...) body ...)
         #'(let ((word
                  (case eos
                    ((16) (memory-u16-ref (fx+ cs ip)))
                    ((8) (memory-u8-ref (fx+ cs ip)))
                    (else (memory-u32-ref (fx+ cs ip)))))
                 (ip (case eos
                       ((16) (fx+ ip 2))
                       ((8) (fx+ ip 1))
                       (else (fx+ ip 4)))))
             (with-instruction-immediate* ((word* <- cs* ip* eos*) ...)
               body ...)))
        ((_ () body ...)
         #'(let () body ...)))))

  (define-syntax with-instruction-immediate-sx*
    (lambda (x)
      (syntax-case x (<-)
        ((_ ((word <- cs ip eos) (word* <- cs* ip* eos*) ...) body ...)
         #'(let ((word
                  (case eos
                    ((16) (memory-s16-ref (fx+ cs ip)))
                    ((8) (memory-s8-ref (fx+ cs ip)))
                    (else (memory-s32-ref (fx+ cs ip)))))
                 (ip (case eos
                       ((16) (fx+ ip 2))
                       ((8) (fx+ ip 1))
                       (else (fx+ ip 4)))))
             (with-instruction-immediate-sx* ((word* <- cs* ip* eos*) ...)
               body ...)))
        ((_ () body ...)
         #'(let () body ...)))))

;;; For decoding instructions

  (define (ModR/M-mod byte) (fxbit-field byte 6 8))

  (define (ModR/M-reg byte) (fxbit-field byte 3 6))

  (define (ModR/M-r/m byte) (fxbit-field byte 0 3))

  (define-syntax with-r/m-operand
    (lambda (x)
      (syntax-case x ()
        ((_ ((ip* store location) (cs ip ModR/M
                                      data-segment stack-segment
                                      effective-address-size))
            body ...)
         #'(let* ((modr/m ModR/M)
                  (mod (ModR/M-mod modr/m))
                  (r/m (ModR/M-r/m modr/m))
                  (eas effective-address-size))
             (let-values (((ip* store location)
                           (cond ((eqv? mod #b11)
                                  ;; Register reference.
                                  (values ip 'reg r/m))
                                 ((eqv? eas 16)
                                  ;; Memory reference.
                                  (%do-addr16 cs ip mod r/m
                                              data-segment stack-segment))
                                 (else
                                  (error 'with-r/m-operand
                                         "TODO: 32-bit addressing" cs ip eas)))))
               body ...)))

        ((_ ((ip* store location ModR/M) (cs ip dseg sseg eas))
            body ...)
         #'(with-instruction-u8* ((ModR/M <- cs ip))
             (with-r/m-operand ((ip store location)
                                (cs ip ModR/M dseg sseg eas))
               body ...))))))

  (define (%do-addr16 cs ip mod r/m dseg sseg)
    (case mod
      ((#b00)
       (cond ((eqv? r/m #b110)
              (with-instruction-s16* ((disp <- cs ip))
                (values ip 'mem (cg+ dseg (cgand #xffff disp)))))
             (else
              (let ((disp (%r/m->address r/m dseg sseg 0)))
                (values ip 'mem disp)))))
      ((#b01)
       (with-instruction-s8* ((disp8 <- cs ip))
         (let ((disp (%r/m->address r/m dseg sseg disp8)))
           (values ip 'mem disp))))
      (else                             ;#b10
       (with-instruction-s16* ((disp16 <- cs ip))
         (let ((disp (%r/m->address r/m dseg sseg disp16)))
           (values ip 'mem disp))))))

  (define (%r/m->address r/m dseg sseg disp)
    ;; r/m comes from ModR/M. dseg and sseg are the current data and
    ;; segment selectors.
    (case r/m
      ((0) (cg+ dseg (cgand #xffff (cg+ disp (cg+ 'BX 'SI)))))
      ((1) (cg+ dseg (cgand #xffff (cg+ disp (cg+ 'BX 'DI)))))
      ((2) (cg+ sseg (cgand #xffff (cg+ disp (cg+ 'BP 'SI)))))
      ((3) (cg+ sseg (cgand #xffff (cg+ disp (cg+ 'BP 'DI)))))
      ((4) (cg+ dseg (cgand #xffff (cg+ disp 'SI))))
      ((5) (cg+ dseg (cgand #xffff (cg+ disp 'DI))))
      ((6) (cg+ sseg (cgand #xffff (cg+ disp 'BP))))
      ((7) (cg+ dseg (cgand #xffff (cg+ disp 'BX))))))

;;; Translation

  (define (cgand op0 op1)
    (if (or (eqv? op0 0) (eqv? op1 0))
        0
        (if (> (fixnum-width) 32)
            `(fxand ,op0 ,op1)
            `(bitwise-and ,op0 ,op1))))

  (define (cgior op0 op1)
    (if (> (fixnum-width) 32)
        `(fxior ,op0 ,op1)
        `(bitwise-ior ,op0 ,op1)))

  (define (cgxor op0 op1)
    (if (equal? op0 op1)
        0
        (if (> (fixnum-width) 32)
            `(fxxor ,op0 ,op1)
            `(bitwise-xor ,op0 ,op1))))

  (define (cgasr op0 op1)
    (if (> (fixnum-width) 32)
        `(fxarithmetic-shift-right ,op0 ,op1)
        `(bitwise-arithmethic-shift-right ,op0 ,op1)))

  (define (cgasl op0 op1)
    (if (> (fixnum-width) 32)
        `(fxarithmetic-shift-left ,op0 ,op1)
        `(bitwise-arithmethic-shift-left ,op0 ,op1)))

  (define (cgbit-set? op0 op1)
    (if (> (fixnum-width) 32)
        `(fxbit-set? ,op0 ,op1)
        `(bitwise-bit-set? ,op0 ,op1)))

  (define (cg+ op0 op1)
    (if (> (fixnum-width) 32)
        `(fx+ ,op0 ,op1)
        `(+ ,op0 ,op1)))

  (define (cg- op0 op1)
    (if (> (fixnum-width) 32)
        `(fx- ,op0 ,op1)
        `(- ,op0 ,op1)))

  (define (cg-reg-ref modr/m eos)
    (cg-register-ref (ModR/M-reg modr/m) eos))

  (define (cgl-reg-set modr/m eos expr)
    (let ((regno (ModR/M-reg modr/m)))
      (cgl-register-update regno eos expr)))

  (define (cg-r/m-ref store location eos)
    (case store
      ((reg)
       (cg-register-ref location eos))
      (else
       (assert (eq? store 'mem))
       `(RAM ,location ,eos))))

  (define (cgl-r/m-set store location eos expr)
    (case store
      ((reg)
       (cgl-register-update location eos expr))
      (else
       (assert (eq? store 'mem))
       `((_ (RAM ,location ,eos ,expr))))))

  ;; Generate lhs and rhs for a let* that updates a register.
  (define (cgl-register-update regno eos expr)
    (case eos
      ((8)
       ;; The byte registers are ordered like this: al cl dl bl ah ch
       ;; dh bh.
       (let ((reg (vector-ref reg-names (fxand regno #b11))))
         (if (> regno #b11)
             `((,reg ,(cgior (cgand reg #xffff00ff)
                             (cgasl (cgand expr #xff) 8))))
             `((,reg ,(cgior (cgand reg #xffffff00)
                             (cgand #xff expr)))))))
      ((16)
       (let ((reg (vector-ref reg-names regno)))
         `((,reg ,(cgior (cgand reg #xffff0000)
                         (cgand #xffff expr))))))
      (else
       (assert (eqv? eos 32))
       (let ((reg (vector-ref reg-names regno)))
         `((,reg ,expr))))))

  (define (cg-register-ref regno eos)
    (case eos
      ((8)
       (let ((reg (vector-ref reg-names (fxand regno #b11))))
         (if (> regno #b11)
             (cgand #xff (cgasr reg 8))
             (cgand #xff reg))))
      ((16)
       (cgand #xffff (vector-ref reg-names regno)))
      (else
       (assert (eqv? eos 32))
       (vector-ref reg-names regno))))

  (define (cg-trunc expr eos)
    (case eos
      ((16) (cgand expr #xffff))
      ((8) (cgand expr #xff))
      (else (cgand expr #xffffffff))))

  (define (cgl-arithmetic result result:u eos operator t0 t1)
    ;; TODO: Check the corner cases.
    (define (cgsub-overflow? a b result result-width)
      ;; If the operation is done with an n bit operand size, then bit
      ;; n-1 will be set if there was an overflow. The result may
      ;; include a borrow.
      (cgbit-set? (cgand (cgxor a b)
                         (cgxor a result))
                  (fx- result-width 1)))
    (define (cgadd-overflow? a b result result-width)
      ;; Same as above. The result may include a carry.
      (cgbit-set? (cgand (cgxor a result)
                         (cgxor b result))
                  (fx- result-width 1)))
    (define (cg-AF a b)
      ;; TODO: this is probably wrong.
      `(if ,(cgbit-set? (cg+ (cgand a #b1111)
                             (cgand b #b1111))
                        4)
           ,flag-AF 0))
    (define (cg-ZF result)
      `(if (eqv? ,result 0) ,flag-ZF 0))
    (define (cg-SF result)
      `(if ,(cgbit-set? result (fx- eos 1)) ,flag-SF 0))
    (define (cg-PF result)
      ;; TODO: LUT.
      `(if (fxeven? (fxbit-count (fxand ,result #xff))) ,flag-PF 0))
    (define (cg-CF)
      `(if ,(cgbit-set? 'tmp eos) ,flag-CF 0))
    (case operator
      ;; XXX: It's the responsibility of the caller to know if the
      ;; result should be used or not.
      ((CMP) (cgl-arithmetic result result:u eos 'SUB t0 t1))
      ((TEST) (cgl-arithmetic result result:u eos 'AND t0 t1))

      ((ADD)
       `((t0 ,t0)
         (t1 ,t1)
         (tmp ,(cg+ 't0 't1))
         (,result ,(cg-trunc 'tmp eos))
         (fl-OF (lambda () (if ,(cgadd-overflow? 't0 't1 result eos) ,flag-OF 0)))
         (fl-SF (lambda () ,(cg-SF result)))
         (fl-ZF (lambda () ,(cg-ZF result)))
         (fl-AF (lambda () ,(cg-AF 't0 't1)))
         (fl-PF (lambda () ,(cg-PF result)))
         (fl-CF (lambda () ,(cg-CF)))))

      ((ADC)
       `((t0 ,t0)
         (t1 ,t1)
         (tmp ,(cg+ (cg+ 't0 't1) '(fl-CF))) ;CF is bit 0
         (,result ,(cg-trunc 'tmp eos))
         (fl-OF (lambda () (if ,(cgadd-overflow? 't0 't1 result eos) ,flag-OF 0)))
         (fl-SF (lambda () ,(cg-SF result)))
         (fl-ZF (lambda () ,(cg-ZF result)))
         (fl-AF (lambda () ,(cg-AF 't0 't1)))
         (fl-PF (lambda () ,(cg-PF result)))
         (fl-CF (lambda () ,(cg-CF)))))

      ((AND)
       `((t0 ,t0)
         (t1 ,t1)
         (tmp ,(cgand 't0 't1))
         (,result ,(cg-trunc 'tmp eos))
         (fl-OF (lambda () 0))
         (fl-SF (lambda () ,(cg-SF result)))
         (fl-ZF (lambda () ,(cg-ZF result)))
         (fl-AF (lambda () 0))          ;undefined
         (fl-PF (lambda () ,(cg-PF result)))
         (fl-CF (lambda () 0))))

      ((INC/DEC)
       `((t0 ,t0)
         (t1 ,t1)
         (tmp ,(cg+ 't0 't1))
         (,result ,(cg-trunc 'tmp eos))
         (fl-OF (lambda () (if ,(cgadd-overflow? 't0 't1 result eos) ,flag-OF 0)))
         (fl-SF (lambda () ,(cg-SF result)))
         (fl-ZF (lambda () ,(cg-ZF result)))
         (fl-AF (lambda () ,(cg-AF 't0 't1)))
         (fl-PF (lambda () ,(cg-PF result)))))

      ((OR)
       `((t0 ,t0)
         (t1 ,t1)
         (tmp ,(cgior 't0 't1))
         (,result ,(cg-trunc 'tmp eos))
         (fl-OF (lambda () 0))
         (fl-SF (lambda () ,(cg-SF result)))
         (fl-ZF (lambda () ,(cg-ZF result)))
         (fl-AF (lambda () 0))          ;undefined
         (fl-PF (lambda () ,(cg-PF result)))
         (fl-CF (lambda () 0))))

      ((SBB)
       `((t0 ,t0)
         (t1 ,t1)
         (tmp ,(cg- (cg- 't0 't1) '(fl-CF))) ;CF is bit 0
         (,result ,(cg-trunc 'tmp eos))
         (fl-OF (lambda () (if ,(cgsub-overflow? 't0 't1 result eos) ,flag-OF 0)))
         (fl-SF (lambda () ,(cg-SF result)))
         (fl-ZF (lambda () ,(cg-ZF result)))
         (fl-AF (lambda () ,(cg-AF 't0 't1)))
         (fl-PF (lambda () ,(cg-PF result)))
         (fl-CF (lambda () ,(cg-CF)))))

      ((SUB)
       `((t0 ,t0)
         (t1 ,t1)
         (tmp ,(cg- 't0 't1))
         (,result ,(cg-trunc 'tmp eos))
         (fl-OF (lambda () (if ,(cgsub-overflow? 't0 't1 result eos) ,flag-OF 0)))
         (fl-SF (lambda () ,(cg-SF result)))
         (fl-ZF (lambda () ,(cg-ZF result)))
         (fl-AF (lambda () ,(cg-AF 't0 't1)))
         (fl-PF (lambda () ,(cg-PF result)))
         (fl-CF (lambda () ,(cg-CF)))))

      ((XOR)
       `((tmp ,(cgxor t0 t1))
         (,result ,(cg-trunc 'tmp eos))
         (fl-OF (lambda () 0))
         (fl-SF (lambda () ,(cg-SF result)))
         (fl-ZF (lambda () ,(cg-ZF result)))
         (fl-AF (lambda () 0))
         (fl-PF (lambda () ,(cg-PF result)))
         (fl-CF (lambda () 0))))

      (else
       (error 'cgl-arithmetic "TODO: Unimplemented operator" operator))))

  (define (cg-pop eos target k)
    (let ((n (/ eos 8)))
      ;; TODO: Giving target like this will not work for 32-bit
      ;; pops and memory destinations.
      `(let* ((,target (RAM ,(cg+ 'ss 'SP) ,eos))
              ,@(cgl-register-update idx-SP eos (cg+ (cg-trunc 'SP eos) n)))
         ,k)))

  (define (cg-push eos expr k)
    (let ((n (/ eos 8)))
      `(begin
         (assert (not (eqv? SP 1)))
         (let* (,@(cgl-register-update idx-SP eos (cg- 'SP n))
                (_ (RAM ,(cg+ 'ss (cg-trunc 'SP eos)) ,eos ,(cg-trunc expr eos))))
           ,k))))

  (define (cg-push* eos x . x*)
    (if (null? x*)
        x
        (cg-push eos x (apply cg-push* eos x*))))

  (define (cg-test-cc cc)
    ;; Appendix B, IA32 SDM Vol 1
    (define (fl= f1 f2)
      `(eqv? ,(fl0 f1) ,(fl0 f2)))
    (define (fl0 fl)
      `(eqv? ,fl 0))
    (if (fxeven? cc)
        `(not ,(cg-test-cc (fx+ cc 1)))
        (case cc
          ((#b0001) (fl0 '(fl-OF)))                            ;NO
          ((#b0011) (fl0 '(fl-CF)))                            ;NB, AE
          ((#b0101) (fl0 '(fl-ZF)))                            ;NE, NZ
          ((#b0111) (fl0 (cgior '(fl-CF) '(fl-ZF))))           ;NBE, A
          ((#b1001) (fl0 '(fl-SF)))                            ;NS
          ((#b1011) (fl0 '(fl-PF)))                            ;NP, PO
          ((#b1101) (fl= '(fl-SF) '(fl-OF)))                   ;NL, GE
          ((#b1111) `(and ,(fl0 '(fl-ZF))                      ;NLE, G
                          ,(fl= '(fl-SF) '(fl-OF))))
          (else
           (error 'cg-test-cc "Invalid condition code" cc)))))

  (define (cgl-merge-fl merge)
    ;; Merge updates of the arithmetic flags into the flags register.
    (if merge
        `((fl (lambda ()
                (fxior (fxand (fl) ,(fxnot (fxior flag-OF flag-SF flag-ZF
                                                  flag-AF flag-PF flag-CF)))
                       (fl-OF) (fl-SF) (fl-ZF) (fl-PF)
                       (fxior (fl-AF) (fl-CF))))))
        '()))

  (define (cg-movs dseg repeat eos eas k-restart k-continue)
    ;; Reads from dseg:rSI and writes to es:rDI, increments or
    ;; decrements rSI and rDI. With repeat=z it repeats until rCX=0.
    ;; For rDI/rSI/rCX eas is used. es can not be overridden.
    (define n (/ eos 8))
    (define (lp-maybe . body)
      (case repeat
        ((z)
         `(let lp ((CX CX) (DI DI) (SI SI) (iterations ,(div 65 n)))
            (cond ((eqv? iterations 0) ,k-restart)
                  (else ,@body))))
        (else `(begin ,@body))))
    `(let* ((n ,(/ eos 8))
            (n (if (fxzero? (fxand (fl) ,flag-DF)) n (fx- n))))
       ,(lp-maybe
         `(let ((src-addr ,(cg+ dseg (cg-register-ref idx-SI eas)))
                (dst-addr ,(cg+ 'es (cg-register-ref idx-DI eas))))
            (RAM dst-addr ,eos (RAM src-addr ,eos)))
         `(let* (,@(cgl-register-update idx-DI eas (cg+ 'DI 'n))
                 ,@(cgl-register-update idx-SI eas (cg+ 'SI 'n)))
            ,(case repeat
               ((z)
                `(let* ((count ,(cg-register-ref idx-CX eas))
                        ,@(cgl-register-update idx-CX eas (cg- 'count 'n)))
                   (if (eqv? ,(cg-register-ref idx-CX eas) 0)
                       ,k-continue
                       (lp CX DI SI (fx- iterations 1)))))
               (else k-continue))))))

  ;; (expand/optimize
  ;;  `(lambda (bus fl AX CX DX BX SP BP SI DI ds ss es fs gs)
  ;;     ,(cg-movs 'ds 'z 16 16 '(values 'restart SI DI CX)
  ;;               '(values 'cont SI DI CX))))

  (define (cg-stos _dseg repeat eos eas k-restart k-continue)
    ;; Copies rAX to es:rDI, increments or decrements rDI.
    ;; With repeat=z it repeats until rCX=0. For rDI/rCX eas
    ;; is used, for rAX it uses eos. es can not be overridden.
    (define n (/ eos 8))
    (define (lp-maybe . body)
      (case repeat
        ((z)
         `(let lp ((CX CX) (DI DI) (SI SI) (iterations ,(div 65 n)))
            (cond ((eqv? iterations 0) ,k-restart)
                  (else ,@body))))
        (else `(begin ,@body))))
    `(let ((n (if (fxzero? (fxand (fl) ,flag-DF)) ,n (fx- ,n))))
       ,(lp-maybe
         `(let ((dst-addr ,(cg+ 'es (cg-register-ref idx-DI eas))))
            (RAM dst-addr ,eos ,(cg-register-ref idx-AX eos)))
         `(let* (,@(cgl-register-update idx-DI eas (cg+ 'DI 'n))
                 ,@(cgl-register-update idx-SI eas (cg+ 'SI 'n)))
            ,(case repeat
               ((z)
                `(let* ((count ,(cg-register-ref idx-CX eas))
                        ,@(cgl-register-update idx-CX eas (cg- 'count 'n)))
                   (if (eqv? ,(cg-register-ref idx-CX eas) 0)
                       ,k-continue
                       (lp CX DI SI (fx- iterations 1)))))
               (else k-continue))))))

  ;; (expand/optimize
  ;;  `(lambda (bus fl AX CX DX BX SP BP SI DI ds ss es fs gs)
  ;;     ,(cg-stos '_ 'z 8 16 '(values 'restart SI DI CX)
  ;;               '(values 'cont SI DI CX))))

  (define (cg-lods dseg repeat eos eas k-restart k-continue)
    ;; Copies ds:rDI to eAX, increments or decrements rDI.
    ;; With repeat=z it repeats until rCX=0. For rDI/rCX eas
    ;; is used, for rAX it uses eos.
    (define n (/ eos 8))
    (define (lp-maybe . body)
      (case repeat
        ((z)
         `(let lp ((CX CX) (DI DI) (SI SI) (iterations ,(div 65 n)))
            (cond ((eqv? iterations 0) ,k-restart)
                  (else ,@body))))
        (else `(begin ,@body))))
    `(let ((n (if (fxzero? (fxand (fl) ,flag-DF)) ,n (fx- ,n))))
       ,(lp-maybe
         `(let ((src-addr ,(cg+ dseg (cg-register-ref idx-SI eas))))
            (let* (,@(cgl-register-update idx-AX eos `(RAM src-addr ,eos))
                   ,@(cgl-register-update idx-DI eas (cg+ 'DI 'n))
                   ,@(cgl-register-update idx-SI eas (cg+ 'SI 'n)))
              ,(case repeat
                 ((z)
                  `(let* ((count ,(cg-register-ref idx-CX eas))
                          ,@(cgl-register-update idx-CX eas (cg- 'count 'n)))
                     (if (eqv? ,(cg-register-ref idx-CX eas) 0)
                         ,k-continue
                         (lp CX DI SI (fx- iterations 1)))))
                 (else k-continue)))))))

  ;; (expand/optimize
  ;;  `(lambda (bus fl AX CX DX BX SP BP SI DI ds ss es fs gs)
  ;;     ,(cg-lods 'ds #f 8 16 '(values 'restart AX SI DI CX)
  ;;               '(values 'cont AX SI DI CX))))


  (define (cg-arithmetic-group op operator ip cs dseg sseg eos eas continue)
    (let ((subop (fwand op #x7)))
      (case (fxasr subop 1)
        ((0)
         ;; op Eb Gv, op Ev Gv
         (let ((eos (if (eqv? subop #x00) 8 eos)))
           (with-r/m-operand ((ip store location modr/m)
                              (cs ip dseg sseg eas))
             `(let* (,@(cgl-arithmetic 'result #f eos operator
                                       (cg-r/m-ref store location eos)
                                       (cg-reg-ref modr/m eos))
                     ,@(case operator
                         ((TEST CMP) '())
                         (else (cgl-r/m-set store location eos 'result))))
                ,(continue #t ip)))))
        ((1)
         ;; op Gv Eb, op Gv Ev
         (let ((eos (if (eqv? subop #x02) 8 eos)))
           (with-r/m-operand ((ip store location modr/m)
                              (cs ip dseg sseg eas))
             `(let* (,@(cgl-arithmetic 'result #f eos operator
                                       (cg-reg-ref modr/m eos)
                                       (cg-r/m-ref store location eos))
                     ,@(case operator
                         ((TEST CMP) '())
                         (else (cgl-reg-set modr/m eos 'result))))
                ,(continue #t ip)))))
        ((2)
         ;; op *AL Ib, op *rAX Iz
         (let ((eos (if (eqv? subop #x04) 8 eos)))
           (with-instruction-immediate* ((imm <- cs ip eos))
             `(let* (,@(cgl-arithmetic 'result #f eos operator
                                       (cg-register-ref idx-AX eos)
                                       imm)
                     ,@(case operator
                         ((TEST CMP) '())
                         (else (cgl-register-update idx-AX eos 'result))))
                ,(continue #t ip)))))
        (else
         (assert #f)))))

  ;; Translate a basic block. This procedure reads instructions at
  ;; cs:ip until it finds a branch of some kind (or something
  ;; complicated), which ends the basic block. It returns a procedure
  ;; that takes the most-used machine registers, does something to
  ;; them and the machine, and returns a new set of registers.
  (define (generate-translation cs ip debug instruction-limit)
    ;; TODO: Memory writes should invalidate the translation cache. So
    ;; translations should stop at page boundaries. Instructions that
    ;; cross page boundaries should not be placed in the translation
    ;; cache, since it's easier to just invalidate whole pages at
    ;; once.
    (define (wrap expr)
      ;; Wrap the flags register and the arithmetic flags.
      `(lambda (bus fl AX CX DX BX SP BP SI DI
                    cs ds ss es fs gs)
         (define RAM
           (case-lambda
             ((addr size) (bus 'read-memory addr size))
             ((addr size value) (bus 'write-memory addr size value))))
         (let ((fl (lambda () fl))
               (fl-OF (lambda () (fxand fl ,flag-OF)))
               (fl-SF (lambda () (fxand fl ,flag-SF)))
               (fl-ZF (lambda () (fxand fl ,flag-ZF)))
               (fl-AF (lambda () (fxand fl ,flag-AF)))
               (fl-PF (lambda () (fxand fl ,flag-PF)))
               (fl-CF (lambda () (fxand fl ,flag-CF))))
           (,expr fl AX CX DX BX SP BP SI DI
                  cs ds ss es fs gs))))
    (define (return merge ip)
      ;; Unwraps the flags register, possibly merging updates from
      ;; arithmetic operations.
      `(let* (,@(cgl-merge-fl merge))
         (values ,ip (fl) AX CX DX BX SP BP SI DI
                 cs ds ss es fs gs)))
    (define (emit expr)
      `(lambda (fl AX CX DX BX SP BP SI DI
                   cs ds ss es fs gs)
         ,expr))
    (wrap
     (let next ((merge #f)
                (ip ip)
                (instruction-count 0))
       (define (continue merge ip)
         ;; XXX: Note that continue (or return) must be called with
         ;; merge set to #t if the arithmetic flags were updated (e.g.
         ;; cgl-arithmetic was used).
         (let ((instruction-count (+ instruction-count 1)))
           (if (fx>=? instruction-count instruction-limit)
               (return merge ip)
               `(,(next merge ip instruction-count)
                 fl AX CX DX BX SP BP SI DI
                 cs ds ss es fs gs))))
       (define first? (eqv? instruction-count 0))
       (define start-ip ip)
       (let prefix ((ip ip)
                    (dseg 'ds)       ;segment for address calculations
                    (sseg 'ss)       ;segment for stack references
                    (eos 16)         ;effective operand size
                    (eas 16)         ;effective address size
                    (lock #f)
                    (repeat #f))
         ;; (print "Reading at CS:IP: " (hex (segment-selector cs)) ":" (hex ip))
         (with-instruction-u8* ((op <- cs ip))
           (case op
             ;; Prefixes. TODO: Limit the number of prefixes read.
             ((#x26) (prefix ip 'es 'es eos eas lock repeat))
             ((#x2E) (prefix ip 'cs 'cs eos eas lock repeat))
             ((#x36) (prefix ip 'ss 'ss eos eas lock repeat))
             ((#x3E) (prefix ip 'ds 'ds eos eas lock repeat))
             ((#x64) (prefix ip 'fs 'fs eos eas lock repeat))
             ((#x65) (prefix ip 'gs 'gs eos eas lock repeat))
             ((#x66) (prefix ip dseg sseg 32 eas lock repeat))
             ((#x67) (prefix ip dseg sseg eos 32 lock repeat))
             ((#xF0) (prefix ip dseg sseg eos eas #t repeat))
             ((#xF2) (prefix ip dseg sseg eos eas lock 'nz))
             ((#xF3) (prefix ip dseg sseg eos eas lock 'z))
             ;; Normal opcodes.
             ((#x00 #x01 #x02 #x03 #x04 #x05)
              (emit (cg-arithmetic-group op 'ADD ip cs dseg sseg eos eas continue)))
             ((#x06)                    ; push *ES
              (emit (cg-push 16 '(fxarithmetic-shift-right es 4) (continue merge ip))))
             ((#x07)                    ; pop *ES
              (emit (cg-pop 16 'tmp `(let ((es ,(cgasl 'tmp 4))) ,(continue merge ip)))))
             ((#x08 #x09 #x0A #x0B #x0C #x0D)
              (emit (cg-arithmetic-group op 'OR ip cs dseg sseg eos eas continue)))
             ((#x0E)                    ; push *CS
              (emit (cg-push 16 '(fxarithmetic-shift-right cs 4) (continue merge ip))))

             ((#x0F)
              ;; Two-byte opcodes.
              (with-instruction-u8* ((op1 <- cs ip))
                (case op1
                  (else
                   (if (not first?)
                       (emit (return merge start-ip))
                       (error 'generate-translation "Can't do it, captain!" cs ip))))))

             ((#x10 #x11 #x12 #x13 #x14 #x15)
              (emit (cg-arithmetic-group op 'ADC ip cs dseg sseg eos eas continue)))
             ((#x16)                    ; push *SS
              (emit (cg-push 16 '(fxarithmetic-shift-right ss 4) (continue merge ip))))
             ((#x17)                    ; pop *SS
              (emit (cg-pop 16 'tmp `(let ((ss ,(cgasl 'tmp 4))) ,(continue merge ip)))))
             ((#x18 #x19 #x1A #x1B #x1C #x1D)
              (emit (cg-arithmetic-group op 'SBB ip cs dseg sseg eos eas continue)))
             ((#x1E)                    ; push *DS
              (emit (cg-push 16 '(fxarithmetic-shift-right ds 4) (continue merge ip))))
             ((#x1F)                    ; pop *DS
              (emit (cg-pop 16 'tmp `(let ((ds ,(cgasl 'tmp 4))) ,(continue merge ip)))))
             ((#x20 #x21 #x22 #x23 #x24 #x25)
              (emit (cg-arithmetic-group op 'AND ip cs dseg sseg eos eas continue)))
             #;((#x27)
                ;; daa
                )
             ((#x28 #x29 #x2A #x2B #x2C #x2D)
              (emit (cg-arithmetic-group op 'SUB ip cs dseg sseg eos eas continue)))
             #;((#x2F)
                ;; das
                )
             ((#x30 #x31 #x32 #x33 #x34 #x35)
              (emit (cg-arithmetic-group op 'XOR ip cs dseg sseg eos eas continue)))
             #;((#x37)
                ;; aaa
                )
             ((#x38 #x39 #x3A #x3B #x3C #x3D)
              (emit (cg-arithmetic-group op 'CMP ip cs dseg sseg eos eas continue)))
             #;((#x3F)
                ;; aas
                )
             ((#x40 #x41 #x42 #x43 #x44 #x45 #x46 #x47 #x48 #x49 #x4A #x4B #x4C #x4D #x4E #x4F)
              ;; inc/dec *rAX/r8 ... *rDI/r15
              (let* ((regno (fxand op #x7))
                     (reg (vector-ref reg-names regno)))
                (emit
                 `(let* (,@(cgl-arithmetic 'result #f eos 'INC/DEC
                                           reg (if (fx<? op #x48) 1 -1))
                         ,@(cgl-register-update regno eos 'result))
                    ,(continue #t ip)))))
             ((#x50 #x51 #x52 #x53 #x54 #x55 #x56 #x57)
              ;; push *rAX/r8 ... *rDI/r15
              (let ((reg (vector-ref reg-names (fxand op #x7))))
                (emit (cg-push eos reg (continue merge ip)))))
             ((#x58 #x59 #x5A #x5B #x5C #x5D #x5E #x5F)
              ;; pop *rAX/r8 ... *rDI/r15
              (let ((reg (vector-ref reg-names (fxand op #x7))))
                (emit (cg-pop eos reg (continue merge ip)))))
             ((#x70 #x71 #x72 #x73 #x74 #x75 #x76 #x77 #x78 #x79 #x7A #x7B #x7C #x7D #x7E #x7F)
              ;; Jcc Jb
              (with-instruction-s8* ((disp <- cs ip))
                (emit
                 `(let* (,@(cgl-merge-fl merge)) ;update fl early
                    (let* ((fl^ (fl))   ;FIXME: clean up. Force one point of fl evaluation.
                           (fl (lambda () fl^)))
                      (if ,(cg-test-cc (fxand op #b1111))
                          ,(return #f (cgand #xffff (cg+ ip disp)))
                          ,(return #f ip)))))))
             ((#x83)
              ;; Group 1. Ev IbS operands.
              (with-r/m-operand ((ip store location modr/m)
                                 (cs ip dseg sseg eas))
                (with-instruction-s8* ((imm <- cs ip))
                  (let ((imm (trunc imm eos))
                        (operator (vector-ref '#(ADD OR ADC SBB AND SUB XOR CMP)
                                              (ModR/M-reg modr/m))))
                    (emit
                     `(let* (,@(cgl-arithmetic 'result #f eos operator
                                               (cg-r/m-ref store location eos)
                                               imm)
                             ,@(case operator
                                 ((TEST CMP) '())
                                 (else (cgl-r/m-set store location eos 'result))))
                        ,(continue #t ip)))))))
             ((#x88 #x89)               ; mov Eb Gb, mov Ev Gv
              (let ((eos (if (eqv? op #x88) 8 eos)))
                (with-r/m-operand ((ip store location modr/m)
                                   (cs ip dseg sseg eas))
                  (emit
                   `(let* (,@(cgl-r/m-set store location eos (cg-reg-ref modr/m eos)))
                      ,(continue merge ip))))))
             ((#x8A #x8B)               ; mov Gb Eb, mov Gv Ev
              (let ((eos (if (eqv? op #x8A) 8 eos)))
                (with-r/m-operand ((ip store location modr/m)
                                   (cs ip dseg sseg eas))
                  (emit
                   `(let* (,@(cgl-reg-set modr/m eos (cg-r/m-ref store location eos)))
                      ,(continue merge ip))))))
             ((#x8E)                    ; mov Sw Ew
              (with-r/m-operand ((ip store location modr/m)
                                 (cs ip dseg sseg eas))
                (let ((reg (or (vector-ref '#(es #f ss ds fs gs #f #f)
                                           (ModR/M-reg modr/m))
                               (error 'generate-translation
                                      "TODO: raise #UD in mov Sw Ew"))))
                  (emit
                   `(let* ((,reg (fx* ,(cg-r/m-ref store location 16) 16)))
                      ,(continue merge ip))))))
             ((#x90)                    ; nop
              (emit (continue merge ip)))
             ((#;#x90 #x91 #x92 #x93 #x94 #x95 #x96 #x97)
              ;; xchg *rCX/r8...*rDI/r15 *rAX
              (let ((regno (fxand op #x7)))
                (emit
                 `(let* ((vAX ,(cg-register-ref idx-AX eos))
                         (vREG ,(cg-register-ref regno eos))
                         ,@(cgl-register-update idx-AX eos 'vREG)
                         ,@(cgl-register-update regno eos 'vAX))
                    ,(continue merge ip)))))
             ((#xA4 #xA5)
              ;; movs Yb Xb, movs Yv Xv
              (let ((eos (if (eqv? op #xA4) 8 eos)))
                (cond ((and repeat (not first?))
                       (emit (return merge start-ip)))
                      (else
                       (emit
                        `(let* (,@(cgl-merge-fl merge)) ;update fl early
                           ,(cg-movs dseg repeat eos eas
                                     (return #f start-ip)
                                     (return #f ip))))))))
             ;; ((#xA6 #xA7) cmps)
             ((#xAA #xAB)
              ;; stos Yb *AL, stos Yv *rAX
              (let ((eos (if (eqv? op #xAA) 8 eos)))
                (cond (repeat
                       (cond ((not first?)
                              (emit (return merge start-ip)))
                             (else
                              (emit
                               `(let* (,@(cgl-merge-fl merge)) ;update fl early
                                  ,(cg-stos dseg repeat eos eas
                                            (return #f start-ip)
                                            (return #f ip)))))))
                      (else
                       (emit
                        (cg-stos dseg #f eos eas #f (continue merge ip)))))))
             ((#xAC #xAD)
              ;; lods *AL Xb, lods *rAX Xv
              (let ((eos (if (eqv? op #xAA) 8 eos)))
                (cond (repeat
                       (cond ((not first?)
                              (emit (return merge start-ip)))
                             (else
                              (emit
                               `(let* (,@(cgl-merge-fl merge)) ;update fl early
                                  ,(cg-lods dseg repeat eos eas
                                            (return #f start-ip)
                                            (return #f ip)))))))
                      (else
                       (emit
                        (cg-lods dseg #f eos eas #f (continue merge ip)))))))
             ;; ((#xAE #xAF) scas)
             ((#xB0 #xB1 #xB2 #xB3 #xB4 #xB5 #xB6 #xB7 #xB8 #xB9 #xBA #xBB #xBC #xBD #xBE #xBF)
              ;; mov *AL/R8L...*BH/R15L Ib
              ;; mov *rAX/r8..*rDI/r15 Iv
              (let ((eos (if (fx<? op #xB8) 8 eos)))
                (with-instruction-immediate* ((imm <- cs ip eos))
                  (emit
                   `(let* (,@(cgl-register-update (fxand op #x7) eos imm))
                      ,(continue merge ip))))))
             ((#xC4 #xC5)               ; les Gz Mp, lds Gz Mp
              (with-r/m-operand ((ip store location modr/m)
                                 (cs ip dseg sseg eas))
                (assert (eq? store 'mem)) ;TODO: #UD
                ;; TODO: is this right? 16:16 or 16:32?
                (let ((off-size (if (eqv? eos 16) 2 4)))
                  (emit
                   `(let* ((seg (fx* (RAM ,(cg+ location off-size) 16) 16))
                           (off (RAM ,location ,eos))
                           ,@(cgl-reg-set modr/m eos 'off)
                           (,(if (eqv? op #xC4) 'es 'ds)
                            seg))
                      ,(continue merge ip))))))
             ((#xC6 #xC7)
              ;; Group 11. Only one mov.
              (let ((eos (if (eqv? op #xC6) 8 eos)))
                (with-r/m-operand ((ip store location modr/m)
                                   (cs ip dseg sseg eas))
                  (with-instruction-immediate* ((imm <- cs ip eos))
                    (case (ModR/M-reg modr/m)
                      ((#x0)            ; mov Eb Ib, mov Ev Iz
                       (emit
                        `(let* (,@(cgl-r/m-set store location eos imm))
                           ,(continue merge ip))))
                      (else
                       (error 'run "TODO: raise #UD in Group 11" (hex op)
                              (hex modr/m))))))))
             ((#xCD)                 ; int Ib
              (let ((idt 0))            ;real mode interrupt vector table
                (with-instruction-u8* ((vec <- cs ip))
                  (emit
                   (cg-push* 16
                             (cg-trunc '(fl) 16)
                             '(fxarithmetic-shift-right cs 4)
                             ip
                             `(let* ((addr ,(cg+ idt (cgasl vec 2)))
                                     (off (RAM addr 16))
                                     (seg (RAM ,(cg+ 'addr 2) 16))
                                     (cs ,(cgasl 'seg 4))
                                     (fl (lambda ()
                                           ,(cgand '(fl) (fxnot (fxior flag-IF
                                                                       flag-TF
                                                                       flag-AC))))))
                                ,(return merge 'off)))))))
             ((#xE8)                    ; call Jz
              (with-instruction-immediate-sx* ((disp <- cs ip eos))
                (emit (cg-push 16 ip (return merge (fwand #xffff (fw+ ip disp)))))))
             ((#xEB)                    ; jmp Jb
              (with-instruction-s8* ((disp <- cs ip))
                (emit (return merge (fwand #xffff (fw+ ip disp))))))
             ((#xF1)
              ;; icebp, In-Circuit Emulator BreakPoint. Exit. Normally
              ;; this would be equivalent to INT 1, except it doesn't
              ;; count as a software interrupt.
              (emit (return merge #f)))
             ((#xF5)                    ; cmc
              (emit
               `(let* ((fl-CF (lambda () (fxxor (fl-CF) ,flag-CF))))
                  ,(continue merge ip))))
             ((#xF8)                    ; clc
              (emit
               `(let* ((fl-CF (lambda () 0)))
                  ,(continue merge ip))))
             ((#xF9)                    ; stc
              (emit
               `(let* ((fl-CF (lambda () ,flag-CF)))
                  ,(continue merge ip))))
             ((#xFA)                    ; cli
              (emit
               `(let* ((fl (lambda () ,(cgand '(fl) (fxnot flag-IF)))))
                  ,(continue merge ip))))
             ((#xFB)                    ; sti
              (emit
               `(let* ((fl (lambda () ,(cgior '(fl) flag-IF))))
                  ,(continue merge ip))))
             ((#xFC)                    ; cld
              (emit
               `(let* ((fl (lambda () ,(cgand '(fl) (fxnot flag-DF)))))
                  ,(continue merge ip))))
             ((#xFD)                    ; std
              (emit
               `(let* ((fl (lambda () ,(cgior '(fl) flag-DF))))
                  ,(continue merge ip))))
             (else
              ;; TODO: #UD
              (if (not first?)
                  (emit (return merge start-ip))
                  (error 'generate-translation "Can't do it, captain!" cs ip)))))))))

  (define (generate-translation! translations cs ip debug instruction-limit)
    (let* ((trans (generate-translation cs ip debug instruction-limit))
           (trans (cond ((procedure? trans)
                         trans)
                        (else
                         (when debug
                           ;; (pretty-print trans)
                           (pretty-print (expand/optimize trans)))
                         (eval trans code-env)))))
      (hashtable-set! translations (fw+ cs ip) trans)
      trans))

;;; Main loop

  (define bus
    (case-lambda
      ((command addr size value)
       ;; TODO: Should actually be able to tell the caller if this
       ;; write should invalidate the translation cache. Code that's
       ;; part of the current translation may have changed (or is that
       ;; illegal use of self-modifying code on the x86?). And
       ;; actually, this can be inlined.
       (assert (fixnum? value))
       (assert (fixnum? addr))
       (case size
         ((8) (memory-u8-set! addr value))
         ((16) (memory-u16-set! addr value))
         ((32) (memory-u32-set! addr value))))
      ((command addr size)
       (case size
         ((8) (memory-u8-ref addr))
         ((16) (memory-u16-ref addr))
         ((32) (memory-u32-ref addr))))))

  (define (machine-run)
    (define M *current-machine*)
    (define debug (machine-debug M))
    (define trace (and debug #f))
    ;; TODO: Very important: invalidation of this translation cache.
    ;; And that might require using something other than a hashtable.
    (define translations (make-eqv-hashtable))

    (let loop ((fl (fxand (fxior (machine-FLAGS M)
                                 flags-always-set)
                          (fxnot flags-never-set)))
               (ip (machine-IP M))
               (cs (fxasl (machine-CS M) 4))
               (ds (fxasl (machine-DS M) 4))
               (ss (fxasl (machine-SS M) 4))
               (es (fxasl (machine-ES M) 4))
               (fs (fxasl (machine-FS M) 4))
               (gs (fxasl (machine-GS M) 4))
               ;; regs
               (AX (machine-AX M))
               (CX (machine-CX M))
               (DX (machine-DX M))
               (BX (machine-BX M))
               (SP (machine-SP M))
               (BP (machine-BP M))
               (SI (machine-SI M))
               (DI (machine-DI M))
               (idt 0))
      (when debug
        (print)
        (print "AX: " (hex AX 4) "  BX: " (hex BX 4)
               "  CX: " (hex CX 4) "  DX: " (hex DX 4)
               "  SP: " (hex SP 4) "  BP: " (hex BP 4)
               "  SI: " (hex SI 4) "  DI: " (hex DI 4))
        (print* "DS: " (hex (segment-selector ds) 4)
                "  ES: " (hex (segment-selector es) 4)
                "  FS: " (hex (segment-selector fs) 4)
                "  GS: " (hex (segment-selector gs) 4)
                "  SS: " (hex (segment-selector ss) 4)
                "  CS: " (hex (segment-selector cs) 4)
                "  IP: " (hex ip 4)
                "  ")
        (print-flags fl)
        (newline)
        (display "SS:SP: ")
        (print-memory (fx+ ss SP) 16)
        ;; (display "@SS:BP: ")
        ;; (display-memory (fx+ ss BP) 16)
        (display "CS:IP: ")
        (print-memory (fx+ cs ip) 16)
        (when disassemble
          (print (hex (segment-selector cs) 4) ":" (hex ip 4) "  "
                 (disassemble (copy-inst cs ip)))))

      (let ((trans (or (hashtable-ref translations (fw+ cs ip) #f)
                       (generate-translation! translations cs ip debug
                                              (if trace 1 32)))))
        (let-values (((ip^ fl^ AX^ CX^ DX^ BX^ SP^ BP^ SI^ DI^
                           cs^ ds^ ss^ es^ fs^ gs^)
                      (trans bus fl AX CX DX BX SP BP SI DI
                             cs ds ss es fs gs)))
          (cond (ip^
                 (loop fl^ ip^ cs^ ds^ ss^ es^ fs^ gs^
                       AX^ CX^ DX^ BX^ SP^ BP^ SI^ DI^
                       idt))
                (else
                 (machine-AX-set! M AX^)
                 (machine-CX-set! M CX^)
                 (machine-DX-set! M DX^)
                 (machine-BX-set! M BX^)
                 (machine-SP-set! M SP^)
                 (machine-BP-set! M BP^)
                 (machine-SI-set! M SI^)
                 (machine-DI-set! M DI^)
                 (machine-ES-set! M (segment-selector es^))
                 (machine-CS-set! M (segment-selector cs^))
                 (machine-SS-set! M (segment-selector ss^))
                 (machine-DS-set! M (segment-selector ds^))
                 (machine-FS-set! M (segment-selector fs^))
                 (machine-GS-set! M (segment-selector gs^))
                 (machine-IP-set! M ip^)
                 (machine-FLAGS-set! M fl^)))))))

  )
