;; -*- mode: scheme; coding: utf-8 -*-
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

;; Emulates an Intel 80386 in real address mode.

;; There is a concept of a current machine (changed using
;; with-machine) and records that keep track of the machine state
;; (made using make-machine). The registers of the machine are set
;; using the record setters and accessors. The memory of the current
;; machine is accessed using the memory-[su]*-{ref,set!} procedures or
;; the copy-{to,from}-memory procedures. The machine-run procedure
;; runs instructions at cs:ip until it encounters an ICEBP
;; instruction.

;; The emulated instructions are translated to Scheme and then
;; compiled by eval so the emulator is somewhat snappy.

;; Debugging is enabled by setting machine-debug to a true value. The
;; debug output contains disassembly if the (weinholt disassembler
;; x86) library from Industria is available.

(library (zabavno cpu x86)
  (export machine-run
          machine?
          make-machine
          machine-debug machine-debug-set!
          machine-trace machine-trace-set!
          machine-RAM
          machine-memory-size
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
          machine-hook-4k-page!
          machine-hook-i/o-port!
          machine-hook-interrupt!
          enable-interrupt-hooks
          call-interrupt-handler

          with-machine
          current-machine
          copy-to-memory copy-from-memory
          real-pointer

          ;; Flag bits.
          flag-OF flag-SF flag-ZF flag-AF flag-PF flag-CF
          flag-DF flag-IF flag-TF flag-AC
          print-flags)
  (import (rnrs (6))
          (rnrs eval (6)))

  ;; Fill memory with this u8 value.
  (define DEFAULT-MEMORY-FILL #xFF)

  (define pretty-print
    (lambda (x) (write x (current-error-port)) (newline (current-error-port))))

  (define code-env (environment '(except (rnrs (6)) bitwise-rotate-bit-field)
                                '(zabavno cpu compat)
                                '(zabavno cpu x86-utils)
                                '(only (rnrs r5rs (6)) quotient remainder)))

  ;; For debug printing.
  (define disassemble
    (guard (exn
            (else #f))
      (let ((get-instruction
             (eval 'get-instruction (environment '(weinholt disassembler x86)))))
        (lambda (bv)
          (call-with-port (open-bytevector-input-port bv)
            (lambda (p)
              (guard
                  (exn
                   (else `(undefined: ,(condition-message exn) ,@(condition-irritants exn))))
                (get-instruction p 16 #f))))))))

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
    (nongenerative machine-920229b9-424c-4b34-8517-9e2c6fb589fe)
    (sealed #t)
    (fields (mutable debug)
            (mutable trace)
            ;; Memory and I/O
            (immutable RAM)
            (immutable I/O)
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
            (mutable FLAGS)
            (immutable int-handlers)
            (immutable translations))
    (protocol
     (lambda (p)
       (lambda ()
         (p #f #f
            ;; Memory and I/O
            (make-empty-memory)
            (make-empty-ports)
            ;; GP registers
            0 0 0 0 0 0 0 0
            ;; Segment registers
            0 0 0 0 0 0
            ;; Other stuff
            0 (fxior flags-always-set flag-IF)
            (make-fallback-int-handlers)
            (make-eqv-hashtable))))))

  (define *current-machine*)
  (define RAM)

  (define (with-machine M thunk)
    (let ((old-machine #f)
          (old-RAM #f))
      (dynamic-wind
        (lambda ()
          (set! old-machine *current-machine*)
          (set! old-RAM RAM)
          (set! *current-machine* M)
          (set! RAM (machine-RAM M)))
        thunk
        (lambda ()
          (set! *current-machine* old-machine)
          (set! RAM old-RAM)
          (set! old-machine #f)
          (set! old-RAM #f)))))

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

  (define (print . x*)
    (for-each (lambda (x) (display x (current-error-port))) x*)
    (newline (current-error-port)))

  (define (print* . x*)
    (for-each (lambda (x) (display x (current-error-port))) x*))

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

  (define (real-pointer segment offset)
    ;; Turn a real-mode pointer into an address.
    (fw+ (fwasl (fwand segment #xffff) 4) (fwand offset #xffff)))

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

  (define idx-AH 4)
  (define idx-CH 5)
  (define idx-DH 6)
  (define idx-BH 7)

;;; Flags

  (define (print-flags fl)
    (define flag*
      '(31 30 29 28 27 26 25 24 23 22 ID VIP VIF AC VM RF
           15 NT IOPL1 IOPL0 OF DF IF TF SF ZF 5 AF 3 PF 1 CF))
    (do ((m (expt 2 31) (fxarithmetic-shift-right m 1))
         (flag* flag* (cdr flag*)))
        ((fxzero? m))
      (unless (fxzero? (fxand m fl))
        (display #\space (current-error-port))
        (display (car flag*) (current-error-port)))))

  (define flag-OF (expt 2 11))          ;overflow
  (define flag-SF (expt 2 7))           ;sign
  (define flag-ZF (expt 2 6))           ;zero
  (define flag-AF (expt 2 4))           ;adjust
  (define flag-PF (expt 2 2))           ;parity
  (define flag-CF (expt 2 0))           ;carry
  (define flag-DF (expt 2 10))          ;direction
  (define flag-IF (expt 2 9))           ;interrupt enable

  (define flag-TF (expt 2 8))           ;trap flag
  (define flag-RF (expt 2 16))          ;resume flag
  (define flag-VM (expt 2 17))          ;virtual-8086 mode
  (define flag-AC (expt 2 18))          ;alignment check

  ;; "Identification of Earlier IA-32 Processors" in IA32 SDM Vol 1.
  ;; Should match an 80386.
  (define flags-never-set (bitwise-ior (expt 2 21) ;no CPUID
                                       (expt 2 18) ;386
                                       (expt 2 15)
                                       ;; IOPL 0 forever
                                       (expt 2 13)
                                       (expt 2 12)
                                       ;; Always zero
                                       (expt 2 3)
                                       (expt 2 5)
                                       (expt 2 15)
                                       (expt 2 22)
                                       (expt 2 23)
                                       (expt 2 24)
                                       (expt 2 25)
                                       (expt 2 26)
                                       (expt 2 27)
                                       (expt 2 28)
                                       (expt 2 29)
                                       (expt 2 30)
                                       (expt 2 31)))

  (define flags-always-set #b10)

;;; Memory

  ;; Debug memory
  (define-syntax mtrace
    (lambda (x)
      (syntax-case x ()
        #;
        ((_ . x*)
         #'(print . x*))
        ((_ . x*)
         #f))))

  ;; Instead of using a big bytevector for the RAM, it is split into
  ;; pages that are referenced from a vector.
  (define page-bits 12)
  (define page-size (expt 2 page-bits))

  ;; The size of the machine's physical memory in bytes.
  (define (machine-memory-size M)
    (* page-size (vector-length (machine-RAM M))))

  (define (print-memory addr len)
    (do ((i 0 (fx+ i 1)))
        ((fx=? i len)
         (newline (current-error-port)))
      (let ((v (memory-u8-ref (fx+ addr i))))
        (display #\space (current-error-port))
        (when (fx<? v #x10)
          (display #\0 (current-error-port)))
        (display (hex v) (current-error-port)))))

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
    (fxarithmetic-shift-right (fxand addr #xFFFFF) page-bits))

  (define (RAM-page-offset addr)
    (fxand addr (fx- page-size 1)))

  ;; All accesses to the page at the given address will be handled by
  ;; the procedure. This allows emulating MMIO for hardware. The
  ;; address must be aligned to a page boundary. For reads the
  ;; procedure is called as (procedure address size), where size is 8,
  ;; 16, 32 or 64. The procedure must return an integer in the range 0
  ;; <= n < (expt 2 size). For writes the procedure is called with as
  ;; (procedure address size value), where value a non-negative
  ;; integer written to memory. Unaligned accesses are currently split
  ;; into byte accesses.
  (define (machine-hook-4k-page! M addr procedure)
    (assert (eqv? (RAM-page-offset addr) 0))
    (assert (procedure? procedure))
    (let ((RAM (machine-RAM M))
          (page (RAM-page addr)))
      (cond ((vector-ref RAM page) =>
             (lambda (previous)
               (print "Warning: machine-hook-4k-page! at address #x" (hex addr)
                      " replaces " (if (bytevector? previous) "RAM" previous)
                      " with " procedure))))
      (vector-set! RAM page procedure)))

  (define (memory-u8-ref addr)
    (let ((x (cond ((vector-ref RAM (RAM-page addr)) =>
                    (lambda (page)
                      (if (bytevector? page)
                          (bytevector-u8-ref page (RAM-page-offset addr))
                          (page addr 8))))
                   (else DEFAULT-MEMORY-FILL))))
      (mtrace "memory-u8-ref: " (hex addr) " => " (hex x))
      x))

  (define (memory-u16-ref addr)
    (let ((x (cond ((fxzero? (fxand addr #b1))
                    (cond ((vector-ref RAM (RAM-page addr)) =>
                           (lambda (page)
                             (if (bytevector? page)
                                 (bytevector-u16le-ref page (RAM-page-offset addr))
                                 (page addr 16))))
                          (else (* DEFAULT-MEMORY-FILL #x0101))))
                   (else
                    (fxior (memory-u8-ref addr)
                           (fxasl (memory-u8-ref (fx+ addr 1)) 8))))))
      (mtrace "memory-u16-ref: " (hex addr) " => " (hex x))
      x))

  (define (memory-u32-ref addr)
    (let ((x (cond ((fxzero? (fxand addr #b11))
                    (cond ((vector-ref RAM (RAM-page addr)) =>
                           (lambda (page)
                             (if (bytevector? page)
                                 (bytevector-u32le-ref page (RAM-page-offset addr))
                                 (page addr 32))))
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
    (cond ((vector-ref RAM (RAM-page addr)) =>
           (lambda (page)
             (invalidate-translation addr)
             (if (bytevector? page)
                 (bytevector-u8-set! page (RAM-page-offset addr) value)
                 (page addr 8 value))))
          (else
           (let ((page (make-bytevector page-size DEFAULT-MEMORY-FILL)))
             (vector-set! RAM (RAM-page addr) page)
             (invalidate-translation addr)
             (bytevector-u8-set! page (RAM-page-offset addr) value)))))

  (define (memory-u16-set! addr value)
    (mtrace "memory-u16-set!: " (hex addr) " " (hex value))
    (cond ((fxzero? (fxand addr #b1))
           (cond ((vector-ref RAM (RAM-page addr)) =>
                  (lambda (page)
                    (invalidate-translation addr)
                    (if (bytevector? page)
                        (bytevector-u16le-set! page (RAM-page-offset addr) value)
                        (page addr 16 value))))
                 (else
                  (let ((page (make-bytevector page-size DEFAULT-MEMORY-FILL)))
                    (vector-set! RAM (RAM-page addr) page)
                    (invalidate-translation addr)
                    (bytevector-u16le-set! page (RAM-page-offset addr) value)))))
          (else
           (memory-u8-set! addr (fxand value #xff))
           (memory-u8-set! (fx+ addr 1) (fxasr value 8)))))

  (define (memory-u32-set! addr value)
    (mtrace "memory-u32-set!: " (hex addr) " " (hex value))
    (cond ((fxzero? (fxand addr #b11))
           (cond ((vector-ref RAM (RAM-page addr)) =>
                  (lambda (page)
                    (invalidate-translation addr)
                    (if (bytevector? page)
                        (bytevector-u32le-set! page (RAM-page-offset addr) value)
                        (page addr 32 value))))
                 (else
                  (let ((page (make-bytevector page-size DEFAULT-MEMORY-FILL)))
                    (vector-set! RAM (RAM-page addr) page)
                    (invalidate-translation addr)
                    (bytevector-u32le-set! page (RAM-page-offset addr) value)))))
          (else
           (memory-u8-set! addr (fwand value #xff))
           (memory-u8-set! (fx+ addr 1) (fwand (fwasr value 8) #xff))
           (memory-u8-set! (fx+ addr 2) (fxand (fwasr value 16) #xff))
           (memory-u8-set! (fx+ addr 3) (fwasr value 24)))))

;;; I/O

  ;; The I/O ports are practically three different address spaces with
  ;; 2^16 unique addresses each.
  (define (make-empty-ports)
    (vector (make-eqv-hashtable)
            (make-eqv-hashtable)
            (make-eqv-hashtable)))

  (define (port-table size)
    (case size
      ((8) (vector-ref (machine-I/O (current-machine)) 0))
      ((16) (vector-ref (machine-I/O (current-machine)) 1))
      ((32) (vector-ref (machine-I/O (current-machine)) 2))))

  (define (port-read port size)
    (let* ((value (cond ((hashtable-ref (port-table size) port #f)
                         => (lambda (port-proc) (port-proc port size)))
                        (else
                         (fw- (fwasl 1 size) 1)))))
      (when (machine-debug (current-machine))
        (print "port-read: " (hex port) " " size " => " value))
      value))

  (define (port-write port size value)
    (when (machine-debug (current-machine))
      (print "port-write: " (hex port) " " size " " (hex value)))
    (cond ((hashtable-ref (port-table size) port #f)
           => (lambda (port-proc)
                (port-proc port size value)))))

  ;; All accesses to the I/O port on the given port and using the
  ;; given size are handled by the procedure. The interface is the
  ;; same as used by the machine-hook-4k-page! procedure, except that
  ;; the size of the access also is specified. The size must be one of
  ;; 8, 16 or 32.
  (define (machine-hook-i/o-port! M port size procedure)
    (assert (fx<? port (expt 2 16)))
    (assert (memv size '(8 16 32)))
    (assert (procedure? procedure))
    (let ((table (port-table size)))
      (cond ((hashtable-ref table port #f) =>
             (lambda (previous)
               (print "Warning: machine-hook-i/o-port! at port #x" (hex port)
                      " and size " size " replaces " previous
                      " with " procedure))))
      (hashtable-set! table port procedure)))

;;; Interrupt vectors

  ;; This defines default procedures for handling interrupts. The
  ;; IVT/IDT in the machine is primarily responsible for keeping track
  ;; of interrupt vectors. This is for things like Scheme-coded BIOS,
  ;; Video BIOS, DOS emulation, etc.

  (define (make-fallback-int-handlers)
    (define (fallback-handler M vec)
      (let ((saved-ip (memory-u16-ref (real-pointer (machine-SS M) (machine-SP M))))
            (saved-cs (memory-u16-ref (real-pointer (machine-SS M) (+ (machine-SP M) 2)))))
        (case vec
          ((#x00)
           (print "Error: divide error at " (hex saved-cs) ":" (hex saved-ip))
           'stop)
          ((#x06)
           (print "Error: invalid opcode at "
                  (hex saved-cs) ":" (hex saved-ip)
                  ": "  (hex (memory-u8-ref (real-pointer saved-cs saved-ip)))
                  " " (hex (memory-u8-ref (real-pointer saved-cs (fx+ saved-ip 1))))
                  " ...: " (disassemble (copy-inst saved-cs saved-ip)))
           'stop)
          ((#x07)
           ;; About "installed": there are x87 emulators for DOS.
           (print "Error: no x87 emulation has been implemented/installed")
           'stop)
          (else
           (print "Warning: unhandled INT #x" (hex vec) " AX=#x" (hex (machine-AX M)))
           'resume))))
    (make-vector 256 fallback-handler))

  ;; Hooks a real-mode interrupt vector. The procedure will be called
  ;; as (procedure M int-vector chain). If the procedure decides to
  ;; chain the interrupt (e.g. because it could not handle it) then
  ;; the chain procedure should be called as (chain M int-vector). If
  ;; the procedure wants the emulator to stop running the machine it
  ;; should return the symbol 'stop. The machine state may be changed
  ;; in any way that implements the interrupt. The return value from
  ;; this procedure is a procedure that may be called without
  ;; arguments to unhook the interrupt handler.
  (define (machine-hook-interrupt! M int-vector handler)
    (assert (fx<=? 0 int-vector 255))
    (assert (procedure? handler))
    (let* ((handlers (machine-int-handlers M))
           (old-handler (vector-ref handlers int-vector))
           (unhook (lambda () (vector-set! handler int-vector old-handler))))
      (vector-set! handlers int-vector
                   (lambda (M int-vector)
                     (handler M int-vector old-handler)))
      unhook))

  ;; Calls an interrupt handler registered with
  ;; machine-hook-interrupt!. Can be used from an interrupt handler.
  (define (call-interrupt-handler int-vector)
    (let* ((M (current-machine))
           (handler (vector-ref (machine-int-handlers M) int-vector)))
      (handler M int-vector)))

  ;; This enables the real-mode interrupt hooks in a very specific
  ;; way: it writes 256 to the IVT at 0:0 to point to interrupt
  ;; handlers, which consist of 256 HLT instructions at BIOS segment
  ;; F000:0 followed by a single IRET. The HLTs are called with IF=0,
  ;; which is usually bad idea, but is handled specially by
  ;; machine-run.
  (define (enable-interrupt-hooks)
    (do ((seg #xF000)
         (int 0 (fx+ int 1)))
        ((fx=? int 256)
         (memory-u8-set! (real-pointer seg int) #xCF)) ;IRET
      (let* ((addr (fxarithmetic-shift-left int 2))
             (off int))
        (memory-u16-set! addr off)
        (memory-u16-set! (fx+ addr 2) seg)
        (memory-u8-set! (real-pointer seg off) #xF4)))) ;HLT

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
    (cond ((or (eqv? op0 0) (eqv? op1 0))
           0)
          ((equal? op0 op1)
           op0)
          (else
           (if (> (fixnum-width) 32)
               `(fxand ,op0 ,op1)
               `(bitwise-and ,op0 ,op1)))))

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

  (define (cgmax op0 op1)
    (if (> (fixnum-width) 32)
        `(fxmax ,op0 ,op1)
        `(max ,op0 ,op1)))

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
      (else
       (assert (eqv? eos 32))
       (cgand expr #xffffffff))))

  (define (cg-recover-sign expr eos)
    ;; Takes an unsigned integer and recovers the sign, in case it's
    ;; larger than the largest signed integer.
    `(let ((x ,expr))
       (if (>= x ,(expt 2 (- eos 1)))
           ,(cg- 'x (expt 2 eos))
           x)))

  (define GROUP-1 '#(ADD OR ADC SBB AND SUB XOR CMP))

  (define GROUP-2 '#(ROL ROR RCL RCR SHL SHR SHL SAR))

  (define GROUP-3 '#(TEST TEST NOT NEG MUL IMUL DIV IDIV))

  (define GROUP-4/5 '#(INC DEC CALL CALLF JMP JMPF PUSH #F))

  ;; (define GROUP-6 '#(SLDT STR LLDT LTR VERR VERW #F #F))

  ;; (define GROUP-7 '#(SGDT SIDT LGDT LIDT SMSW #F LMSW #F))

  (define GROUP-8 '#(#F #F #F #F BT BTS BTR BTC))

  (define (cg-SF result eos)
    `(if ,(cgbit-set? result (fx- eos 1)) ,flag-SF 0))

  (define (cg-ZF result)
    `(if (eqv? ,result 0) ,flag-ZF 0))

  (define (cg-PF result)
    `(if (vector-ref byte-parity-table (fxand ,result #xff)) ,flag-PF 0))

  (define (cgl-arithmetic-shld result eos input0 input1 count)
    `((t0 ,input0)
      (t1 ,input1)
      (count ,(cgand count #b00011111))
      (tmp ,(cgior (cgasl (cgand 't0 (cg- (cgasl 1 (cgmax 0 (cg- eos 'count))) 1))
                          'count)
                   (cgasr 't1 (cgmax 0 (cg- eos 'count)))))
      (,result ,(cg-trunc 'tmp eos))
      (fl-OF (lambda () (fl-OF)))       ;undefined
      (fl-SF (lambda () (if (eqv? count 0) (fl-SF) ,(cg-SF result eos))))
      (fl-ZF (lambda () (if (eqv? count 0) (fl-ZF) ,(cg-ZF result))))
      (fl-AF (lambda () (fl-AF)))       ;undefined
      (fl-PF (lambda () (if (eqv? count 0) (fl-PF) ,(cg-PF result))))
      (fl-CF (lambda () (if (eqv? count 0) (fl-CF)
                            (if ,(cgbit-set? 't0 (cg- eos 'count))
                                ,flag-CF
                                0))))))

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
    (define (cg-AF cg% a b)
      `(if ,(cgbit-set? (cg% (cgand a #b1111)
                             (cgand b #b1111))
                        4)
           ,flag-AF 0))
    (define (cg-CF)
      `(if ,(cgbit-set? 'tmp eos) ,flag-CF 0))
    (case operator
      ;; XXX: It's the responsibility of the caller to know if the
      ;; result should be used or not.
      ((CMP) (cgl-arithmetic result result:u eos 'SUB t0 t1))
      ((TEST) (cgl-arithmetic result result:u eos 'AND t0 t1))

      ((AAD)
       `((t0 ,t0)
         (t1 ,t1)
         (tmp (fxand #xff (fx+ (fxand t0 #xff)
                               (fx* t1 (fxbit-field t0 8 16)))))
         (,result tmp)
         (fl-OF (lambda () 0))          ;undefined
         (fl-SF (lambda () ,(cg-SF result eos)))
         (fl-ZF (lambda () ,(cg-ZF result)))
         (fl-AF (lambda () 0))          ;undefined
         (fl-PF (lambda () ,(cg-PF result)))
         (fl-CF (lambda () 0))))        ;undefined

      ((AAM)
       `((t0 ,t0)
         (t1 ,t1)
         (ah (fxdiv t0 t1))
         (al (fxmod t0 t1))
         (tmp (fxior (fxarithmetic-shift-left ah 8) al))
         (,result tmp)
         ;; Flags reflect AL only, according to Intel documentation.
         (fl-OF (lambda () 0))          ;undefined
         (fl-SF (lambda () ,(cg-SF 'al 8)))
         (fl-ZF (lambda () ,(cg-ZF 'al)))
         (fl-AF (lambda () 0))          ;undefined
         (fl-PF (lambda () ,(cg-PF 'al)))
         (fl-CF (lambda () 0))));undefined

      ((ADD)
       `((t0 ,t0)
         (t1 ,t1)
         (tmp ,(cg+ 't0 't1))
         (,result ,(cg-trunc 'tmp eos))
         (fl-OF (lambda () (if ,(cgadd-overflow? 't0 't1 result eos) ,flag-OF 0)))
         (fl-SF (lambda () ,(cg-SF result eos)))
         (fl-ZF (lambda () ,(cg-ZF result)))
         (fl-AF (lambda () ,(cg-AF cg+ 't0 't1)))
         (fl-PF (lambda () ,(cg-PF result)))
         (fl-CF (lambda () ,(cg-CF)))))

      ((ADC)
       `((t0 ,t0)
         (t1 ,t1)
         (tmp ,(cg+ (cg+ 't0 't1) '(fl-CF))) ;CF is bit 0
         (,result ,(cg-trunc 'tmp eos))
         (fl-OF (lambda () (if ,(cgadd-overflow? 't0 't1 result eos) ,flag-OF 0)))
         (fl-SF (lambda () ,(cg-SF result eos)))
         (fl-ZF (lambda () ,(cg-ZF result)))
         (fl-AF (lambda () ,(cg-AF cg+ 't0 't1)))
         (fl-PF (lambda () ,(cg-PF result)))
         (fl-CF (lambda () ,(cg-CF)))))

      ((AND)
       `((tmp ,(cgand t0 t1))
         (,result tmp)
         (fl-OF (lambda () 0))
         (fl-SF (lambda () ,(cg-SF result eos)))
         (fl-ZF (lambda () ,(cg-ZF result)))
         (fl-AF (lambda () 0))          ;undefined
         (fl-PF (lambda () ,(cg-PF result)))
         (fl-CF (lambda () 0))))

      ((BSF)
       `((t0 ,t0)
         (t1 ,t1)
         (tmp (cond ((eqv? t1 0) t0)
                    (,(fx>? (fixnum-width) eos) (fxfirst-bit-set t1))
                    (else (bitwise-first-bit-set t1))))
         (,result tmp)
         (fl-OF (lambda () 0))          ;undefined
         (fl-SF (lambda () 0))          ;undefined
         (fl-ZF (lambda () (if (eqv? t1 0) ,flag-ZF 0)))
         (fl-AF (lambda () 0))          ;undefined
         (fl-PF (lambda () 0))          ;undefined
         (fl-CF (lambda () 0))))        ;undefined

      ((BSR)
       `((t0 ,t0)
         (t1 ,t1)
         (tmp (cond ((eqv? t1 0) t0)
                    (,(fx>? (fixnum-width) eos) (fx- (fxlength t1) 1))
                    (else (fx- (bitwise-length t1) 1))))
         (,result tmp)
         (fl-OF (lambda () 0))          ;undefined
         (fl-SF (lambda () 0))          ;undefined
         (fl-ZF (lambda () (if (eqv? t1 0) ,flag-ZF 0)))
         (fl-AF (lambda () 0))          ;undefined
         (fl-PF (lambda () 0))          ;undefined
         (fl-CF (lambda () 0))))        ;undefined

      ((BT)
       `((t0 ,t0)
         (t1 ,(cgand t1 (cg- eos 1)))
         (tmp ,t0)
         (,result tmp)
         (fl-OF (lambda () 0))          ;undefined
         (fl-SF (lambda () 0))          ;undefined
         (fl-ZF (lambda () 0))          ;undefined
         (fl-AF (lambda () 0))          ;undefined
         (fl-PF (lambda () 0))          ;undefined
         (fl-CF (lambda () (if ,(cgbit-set? 't0 't1) ,flag-CF 0)))))

      ((BTC)
       `((t0 ,t0)
         (t1 ,(cgand t1 (cg- eos 1)))
         (tmp ,(cgxor 't0 (cgasl 1 't1)))
         (,result tmp)
         (fl-OF (lambda () 0))          ;undefined
         (fl-SF (lambda () 0))          ;undefined
         (fl-ZF (lambda () 0))          ;undefined
         (fl-AF (lambda () 0))          ;undefined
         (fl-PF (lambda () 0))          ;undefined
         (fl-CF (lambda () (if ,(cgbit-set? 't0 't1) ,flag-CF 0)))))

      ((BTR)
       `((t0 ,t0)
         (t1 ,(cgand t1 (cg- eos 1)))
         (tmp ,(cgand 't0 `(,(if (fx>? (fixnum-width) eos) 'fxnot 'bitwise-not) ,(cgasl 1 't1))))
         (,result tmp)
         (fl-OF (lambda () 0))          ;undefined
         (fl-SF (lambda () 0))          ;undefined
         (fl-ZF (lambda () 0))          ;undefined
         (fl-AF (lambda () 0))          ;undefined
         (fl-PF (lambda () 0))          ;undefined
         (fl-CF (lambda () (if ,(cgbit-set? 't0 't1) ,flag-CF 0)))))

      ((BTS)
       `((t0 ,t0)
         (t1 ,(cgand t1 (cg- eos 1)))
         (tmp ,(cgior 't0 (cgasl 1 't1)))
         (,result tmp)
         (fl-OF (lambda () 0))          ;undefined
         (fl-SF (lambda () 0))          ;undefined
         (fl-ZF (lambda () 0))          ;undefined
         (fl-AF (lambda () 0))          ;undefined
         (fl-PF (lambda () 0))          ;undefined
         (fl-CF (lambda () (if ,(cgbit-set? 't0 't1) ,flag-CF 0)))))

      ((DIV)
       ;; This returns an extra variable: div-trap?. If it's true then
       ;; the caller should raise a #DE. Note that R6RS div and mod
       ;; works fine here, since it's unsigned arithmetic.
       `((t0 ,t0)
         (t1 ,t1)
         (div-by-zero (eqv? t1 0))
         (tmp (if div-by-zero 0 (div t0 t1)))
         (,result tmp)
         (div-trap? (or div-by-zero (> tmp ,(- (expt 2 eos) 1))))
         (,result:u (if div-by-zero 0 (mod t0 t1)))
         ;; All the flags are undefined.
         (fl-OF (lambda () 0))          ;undefined
         (fl-SF (lambda () 0))          ;undefined
         (fl-ZF (lambda () 0))          ;undefined
         (fl-AF (lambda () ,flag-AF))   ;undefined
         (fl-PF (lambda () 0))          ;undefined
         (fl-CF (lambda () 0))))        ;undefined

      ((IDIV)
       ;; This returns an extra variable: div-trap?. If it's true then
       ;; the caller should raise a #DE.
       `((t0 ,(cg-recover-sign t0 (* eos 2)))
         (t1 ,(cg-recover-sign t1 eos))
         (div-by-zero (eqv? t1 0))
         (tmp (if div-by-zero 0 (quotient t0 t1)))
         (,result ,(cg-trunc 'tmp eos))
         (div-trap? (or div-by-zero (not (< ,(- (expt 2 (- (* eos 2) 1)))
                                            tmp
                                            ,(- (expt 2 (- (* eos 2) 1)) 1)))))
         (,result:u (if div-by-zero 0 (remainder t0 t1)))
         ;; All the flags are undefined.
         (fl-OF (lambda () 0))          ;undefined
         (fl-SF (lambda () 0))          ;undefined
         (fl-ZF (lambda () 0))          ;undefined
         (fl-AF (lambda () ,flag-AF))   ;undefined
         (fl-PF (lambda () 0))          ;undefined
         (fl-CF (lambda () 0))))        ;undefined

      ((IMUL)
       `((t0 ,(cg-recover-sign t0 eos))
         (t1 ,(cg-recover-sign t1 eos))
         (tmp (* t0 t1))
         (,result ,(cg-trunc 'tmp eos))
         (,result:u ,(or (eqv? result:u '_)
                         (cg-trunc `(bitwise-arithmetic-shift-right tmp ,eos) eos)))
         ;; CF and OF are set if the result did not fit in lower
         ;; destination.
         (div-trap? #f)
         (fl-OF (lambda () (if (<= ,(- (expt 2 (- eos 1))) tmp ,(- (expt 2 (- eos 1)) 1))
                               0 ,flag-OF)))
         (fl-SF (lambda () 0))          ;undefined
         (fl-ZF (lambda () ,flag-ZF))   ;undefined
         (fl-AF (lambda () 0))          ;undefined
         (fl-PF (lambda () ,flag-PF))   ;undefined
         (fl-CF (lambda () (if (<= ,(- (expt 2 (- eos 1))) tmp ,(- (expt 2 (- eos 1)) 1))
                               0 ,flag-CF)))))

      ((INC/DEC)
       `((t0 ,t0)
         (t1 ,t1)
         (tmp ,(cg+ 't0 't1))
         (,result ,(cg-trunc 'tmp eos))
         (fl-OF (lambda () (if ,(cgadd-overflow? 't0 't1 result eos) ,flag-OF 0)))
         (fl-SF (lambda () ,(cg-SF result eos)))
         (fl-ZF (lambda () ,(cg-ZF result)))
         (fl-AF (lambda () ,(cg-AF cg+ 't0 't1)))
         (fl-PF (lambda () ,(cg-PF result)))))

      ((MUL)
       `((t0 ,t0)
         (t1 ,t1)
         (tmp (* t0 t1))
         (,result ,(cg-trunc 'tmp eos))
         (,result:u ,(cg-trunc `(bitwise-arithmetic-shift-right tmp ,eos) eos))
         ;; CF and OF are set if the result did not fit in lower
         ;; destination.
         (div-trap? #f)
         (fl-OF (lambda () (if (>= ,result ,(expt 2 eos)) ,flag-OF 0)))
         (fl-SF (lambda () 0))          ;undefined
         (fl-ZF (lambda () 0))          ;undefined
         (fl-AF (lambda () 0))          ;undefined
         (fl-PF (lambda () 0))          ;undefined
         (fl-CF (lambda () (if (>= ,result ,(expt 2 eos)) ,flag-CF 0)))))

      ((NEG)
       `((t0 ,t0)
         (tmp ,(cg- 0 't0))
         (,result ,(cg-trunc 'tmp eos))
         (fl-OF (lambda () (if ,(cgsub-overflow? 0 't0 result eos) ,flag-OF 0)))
         (fl-SF (lambda () ,(cg-SF result eos)))
         (fl-ZF (lambda () ,(cg-ZF result)))
         (fl-AF (lambda () ,(cg-AF cg- 0 't0)))
         (fl-PF (lambda () ,(cg-PF result)))
         (fl-CF (lambda () (if (eqv? t0 0) 0 ,flag-CF)))))

      ((NOT)
       `((t0 ,t0)
         (tmp ,(cgxor 't0 (- (expt 2 eos) 1)))
         (,result ,(cg-trunc 'tmp eos))))

      ((OR)
       `((t0 ,t0)
         (t1 ,t1)
         (tmp ,(cgior 't0 't1))
         (,result ,(cg-trunc 'tmp eos))
         (fl-OF (lambda () 0))
         (fl-SF (lambda () ,(cg-SF result eos)))
         (fl-ZF (lambda () ,(cg-ZF result)))
         (fl-AF (lambda () 0))          ;undefined
         (fl-PF (lambda () ,(cg-PF result)))
         (fl-CF (lambda () 0))))

      ((RCL)
       `((t0 ,t0)
         (t1 ,(cgand t1 #b00011111))
         (tmp (bitwise-rotate-bit-field ; CF becomes bit 0, all else shifted up
               ,(cgior '(fl-CF) (cgasl 't0 1)) 0 ,(fx+ eos 1) ,(cg- 't1 1)))
         (,result ,(cg-trunc 'tmp eos))
         (fl-OF (lambda () (cond ((eqv? t1 0) (fl-OF))
                                 ;; undefined if t1 > 1
                                 ((not (eqv? ,(cgbit-set? 't0 (cg- eos 't1))
                                             ,(cgbit-set? 'tmp (fx- eos 1))))
                                  ,flag-OF)
                                 (else 0))))
         (fl-CF (lambda () (if (eqv? t1 0) (fl-CF)
                               (if ,(cgbit-set? 't0 (cg- eos 't1))
                                   ,flag-CF
                                   0))))))

      ((RCR)
       `((t0 ,t0)
         (t1 ,(cgand t1 #b00011111))
         (tmp (bitwise-rotate-bit-field ; CF becomes bit 9, 17 or 33 of t0
               ,(cgior 't0 (cgasl '(fl-CF) eos))
               0 ,(fx+ eos 1) ,(cg- (fx+ eos 1) 't1)))
         (,result ,(cg-trunc 'tmp eos))
         (fl-OF (lambda () (cond ((eqv? t1 0) (fl-OF))
                                 ;; undefined if t1 > 1
                                 ((not (eqv? ,(cgbit-set? 'tmp (fx- eos 1))
                                             ,(cgbit-set? 'tmp (fx- eos 2))))
                                  ,flag-OF)
                                 (else 0))))
         (fl-CF (lambda () (if (eqv? t1 0) (fl-CF)
                               (if ,(cgbit-set? 't0 (cg- t1 1))
                                   ,flag-CF
                                   0))))))

      ((ROL)
       `((t0 ,t0)
         (t1 ,(cgand t1 #b00011111))
         (tmp (bitwise-rotate-bit-field t0 0 ,eos t1))
         (,result ,(cg-trunc 'tmp eos))
         (fl-OF (lambda () (cond ((eqv? t1 0) (fl-OF))
                                 ;; undefined if t1 > 1
                                 ((not (eqv? ,(cgbit-set? 't0 (cg- eos 't1))
                                             ,(cgbit-set? 'tmp (fx- eos 1))))
                                  ,flag-OF)
                                 (else 0))))
         (fl-CF (lambda () (if (eqv? t1 0) (fl-CF)
                               (if ,(cgbit-set? 't0 (cg- eos 't1))
                                   ,flag-CF
                                   0))))))

      ((ROR)
       `((t0 ,t0)
         (t1 ,(cgand t1 #b00011111))
         (tmp (bitwise-rotate-bit-field t0 0 ,eos ,(cg- eos 't1)))
         (,result ,(cg-trunc 'tmp eos))
         (fl-OF (lambda () (cond ((eqv? t1 0) (fl-OF))
                                 ;; undefined if t1 > 1
                                 ((not (eqv? ,(cgbit-set? 'tmp (fx- eos 1))
                                             ,(cgbit-set? 'tmp (fx- eos 2))))
                                  ,flag-OF)
                                 (else 0))))
         (fl-CF (lambda () (if (eqv? t1 0) (fl-CF)
                               (if ,(cgbit-set? 't0 (fx- t1 1))
                                   ,flag-CF
                                   0))))))

      ((SAR)
       `((t0 ,(cg-recover-sign t0 eos))
         (t1 ,(cgand t1 #b00011111))
         (tmp ,(cgasr 't0 't1))
         (,result tmp)
         (fl-OF (lambda () (cond ((eqv? t1 0) (fl-OF))
                                 (else 0)))) ;undefined for t1>1
         (fl-SF (lambda () (if (eqv? t1 0) (fl-SF) ,(cg-SF result eos))))
         (fl-ZF (lambda () (if (eqv? t1 0) (fl-ZF) ,(cg-ZF result))))
         (fl-AF (lambda () (if (eqv? t1 0) (fl-AF) ,flag-AF))) ;undefined
         (fl-PF (lambda () (if (eqv? t1 0) (fl-PF) ,(cg-PF result))))
         (fl-CF (lambda () (if (eqv? t1 0) (fl-CF)
                               (if ,(cgbit-set? 't0 (cg- 't1 1))
                                   ,flag-CF
                                   0))))))

      ((SBB)
       `((t0 ,t0)
         (t1 ,t1)
         (tmp ,(cg- (cg- 't0 't1) '(fl-CF))) ;CF is bit 0
         (,result ,(cg-trunc 'tmp eos))
         (fl-OF (lambda () (if ,(cgsub-overflow? 't0 't1 result eos) ,flag-OF 0)))
         (fl-SF (lambda () ,(cg-SF result eos)))
         (fl-ZF (lambda () ,(cg-ZF result)))
         (fl-AF (lambda () ,(cg-AF cg- 't0 't1)))
         (fl-PF (lambda () ,(cg-PF result)))
         (fl-CF (lambda () ,(cg-CF)))))

      ((SHL)                            ;same as SAL
       `((t0 ,t0)
         (t1 ,(cgand t1 #b00011111))
         (tmp ,(cgasl 't0 't1))
         (,result ,(cg-trunc 'tmp eos))
         (fl-OF (lambda () (cond ((eqv? t1 0) (fl-OF))
                                 ;; undefined if t1 > 1
                                 ((not (eqv? ,(cgbit-set? 't0 (cg- eos 't1))
                                             ,(cgbit-set? 'tmp (fx- eos 1))))
                                  ,flag-OF)
                                 (else 0))))
         (fl-SF (lambda () (if (eqv? t1 0) (fl-SF) ,(cg-SF result eos))))
         (fl-ZF (lambda () (if (eqv? t1 0) (fl-ZF) ,(cg-ZF result))))
         (fl-AF (lambda () (if (eqv? t1 0) (fl-AF) ,flag-AF))) ;undefined
         (fl-PF (lambda () (if (eqv? t1 0) (fl-PF) ,(cg-PF result))))
         (fl-CF (lambda () (if (eqv? t1 0) (fl-CF)
                               (if ,(cgbit-set? 't0 (cg- eos 't1))
                                   ,flag-CF
                                   0))))))

      ((SHR)
       `((t0 ,t0)
         (t1 ,(cgand t1 #b00011111))
         (tmp ,(cgasr 't0 't1))
         (,result tmp)
         (fl-OF (lambda () (cond ((eqv? t1 0) (fl-OF))
                                 ((and #;(eqv? t1 1)
                                       ,(cgbit-set? 't0 (fx- eos 1)))
                                  ,flag-OF)
                                 (else 0))))
         (fl-SF (lambda () (if (eqv? t1 0) (fl-SF) ,(cg-SF result eos))))
         (fl-ZF (lambda () (if (eqv? t1 0) (fl-ZF) ,(cg-ZF result))))
         (fl-AF (lambda () (if (eqv? t1 0) (fl-AF) ,flag-AF))) ;undefined
         (fl-PF (lambda () (if (eqv? t1 0) (fl-PF) ,(cg-PF result))))
         (fl-CF (lambda () (if (eqv? t1 0) (fl-CF)
                               (if ,(cgbit-set? 't0 (cg- 't1 1))
                                   ,flag-CF
                                   0))))))

      ((SHRD)
       `((t0 ,t0)
         (t1 ,(cgand t1 #b00011111))
         (tmp ,(cgasr 't0 't1))
         (,result ,(cg-trunc 'tmp eos))
         (fl-OF (lambda () 0))          ;undefined
         (fl-SF (lambda () ,(cg-SF result eos)))
         (fl-ZF (lambda () ,(cg-ZF result)))
         (fl-AF (lambda () 0))          ;undefined
         (fl-PF (lambda () ,(cg-PF result)))
         (fl-CF (lambda () (if ,(cgbit-set? 't0 (cg- 't1 1))
                               ,flag-CF
                               0)))))

      ((SUB)
       `((t0 ,t0)
         (t1 ,t1)
         (tmp ,(cg- 't0 't1))
         (,result ,(cg-trunc 'tmp eos))
         (fl-OF (lambda () (if ,(cgsub-overflow? 't0 't1 result eos) ,flag-OF 0)))
         (fl-SF (lambda () ,(cg-SF result eos)))
         (fl-ZF (lambda () ,(cg-ZF result)))
         (fl-AF (lambda () ,(cg-AF cg- 't0 't1)))
         (fl-PF (lambda () ,(cg-PF result)))
         (fl-CF (lambda () ,(cg-CF)))))

      ((XOR)
       `((tmp ,(cgxor t0 t1))
         (,result ,(cg-trunc 'tmp eos))
         (fl-OF (lambda () 0))
         (fl-SF (lambda () ,(cg-SF result eos)))
         (fl-ZF (lambda () ,(cg-ZF result)))
         (fl-AF (lambda () 0))
         (fl-PF (lambda () ,(cg-PF result)))
         (fl-CF (lambda () 0))))

      (else
       (error 'cgl-arithmetic "TODO: Unimplemented operator" operator))))

  (define (cg-pop eos target k)
    ;; The target is a temporary variable, the register/memory write
    ;; semantics are implemented by the caller.
    ;; FIXME: Check the stack-size.
    `(let* ((,target (RAM ,(cg+ 'ss 'SP) ,eos))
            ,@(cgl-register-update idx-SP eos (cg+ (cg-trunc 'SP eos) (/ eos 8))))
       ,k))

  (define (cg-pop* eos x . x*)
    (if (null? x*)
        x
        (cg-pop eos x (apply cg-pop* eos x*))))

  (define (cg-push eos expr k)
    (let ((n (/ eos 8)))
      ;; On an 8086 the "push sp" instruction pushes SP after
      ;; decrementing it, from 80286 and forward the original value is
      ;; pushed.
      `(begin
         (assert (not (eqv? SP 1))) ; TODO: cg-int-stack (it needs
                                    ; merge, start-ip, return)
         (let* ((value ,(cg-trunc expr eos))
                ,@(cgl-register-update idx-SP eos (cg- 'SP n))
                (_ (RAM ,(cg+ 'ss (cg-trunc 'SP eos)) ,eos value)))
           ,k))))

  (define (cg-push* eos x . x*)
    ;; TODO: A single stack bounds check?
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

  (define (cgl-merge-fl/eval merge)
    ;; Merge updates of the arithmetic flags into the flags register,
    ;; and force evaluation of the flags register. Useful when fl is
    ;; referenced multiple times: avoids allocation of a procedure and
    ;; only evaluates the flags once.
    `(,@(cgl-merge-fl merge)
      (fl^ (fl))
      (fl (lambda () fl^))))

  (define (%cg-rep repeat eos eas k-restart k-continue body)
    ;; TODO: What happens to REPNE on instructions other than CMPS/SCAS?
    `(let ((n (if (fxzero? (fxand (fl) ,flag-DF)) ,(/ eos 8) ,(- (/ eos 8)))))
       ,(if (not repeat)
            `(let ((count 0) (iterations 0) (lp-rep (lambda _ #f))) ,body)
            `(let ((count ,(cg-register-ref idx-CX eas)))
               (if (and ',repeat (eqv? count 0))
                   ,k-continue       ;the instruction became just a nop
                   (let lp-rep ((DI DI) (SI SI) (count count) (iterations 64))
                     (cond
                       ((eqv? iterations 0)
                        ;; Return to machine-run, so that this loop doesn't run forever.
                        (let* (,@(cgl-register-update idx-CX eas 'count))
                          ,k-restart))
                       (else
                        (let* ((iterations (fx- iterations 1))
                               (count (fx- count 1)))
                          ,body)))))))))

  (define (cg-movs dseg repeat eos eas k-restart k-continue)
    ;; Reads from dseg:rSI and writes to es:rDI, increments or
    ;; decrements rSI and rDI. With repeat=z it repeats until rCX=0.
    ;; For rDI/rSI/rCX eas is used. es can not be overridden.
    (%cg-rep repeat eos eas k-restart k-continue
             `(let* ((src-addr ,(cg+ dseg (cg-register-ref idx-SI eas)))
                     (dst-addr ,(cg+ 'es (cg-register-ref idx-DI eas)))
                     ,@(cgl-register-update idx-DI eas (cg+ 'DI 'n))
                     ,@(cgl-register-update idx-SI eas (cg+ 'SI 'n)))
                (RAM dst-addr ,eos (RAM src-addr ,eos))
                (if (not (eqv? count 0))
                    (lp-rep DI SI count iterations)
                    (let* (,@(if repeat (cgl-register-update idx-CX eas 'count) '()))
                      ,k-continue)))))

  (define (cg-stos _dseg repeat eos eas k-restart k-continue)
    ;; Copies rAX to es:rDI, increments or decrements rDI. With
    ;; repeat=z it repeats until rCX=0. For rDI/rCX eas is used, for
    ;; rAX it uses eos. es can not be overridden.
    `(let ((src ,(cg-register-ref idx-AX eos)))
       ,(%cg-rep repeat eos eas k-restart k-continue
                 `(let* ((dst-addr ,(cg+ 'es (cg-register-ref idx-DI eas)))
                         ,@(cgl-register-update idx-DI eas (cg+ 'DI 'n)))
                    (RAM dst-addr ,eos src)
                    (if (not (eqv? count 0))
                        (lp-rep DI SI count iterations)
                        (let* (,@(if repeat (cgl-register-update idx-CX eas 'count) '()))
                          ,k-continue))))))

  (define (cg-lods dseg repeat eos eas k-restart k-continue)
    ;; Copies ds:rSI to eAX, increments or decrements rSI. With
    ;; repeat=z it repeats until rCX=0. For rSI/rCX eas is used, for
    ;; rAX it uses eos.
    (%cg-rep repeat eos eas k-restart k-continue
             `(let* ((src-addr ,(cg+ dseg (cg-register-ref idx-SI eas)))
                     (src (RAM src-addr ,eos))
                     ,@(cgl-register-update idx-SI eas (cg+ 'SI 'n)))
                (if (not (eqv? count 0))
                    (lp-rep DI SI count iterations)
                    (let* (,@(cgl-register-update idx-AX eos 'src)
                           ,@(if repeat (cgl-register-update idx-CX eas 'count) '()))
                      ,k-continue)))))

  (define (cg-cmps dseg repeat eos eas k-restart k-continue)
    ;; Reads from dseg:rSI and es:rDI, increments or decrements rSI
    ;; and rDI. With repeat=z it repeats until rCX=0 or ZF=0, with
    ;; repeat=nz it repeats until rCX=0 or ZF=1. For rDI/rSI/rCX eas
    ;; is used. es can not be overridden.
    (%cg-rep repeat eos eas k-restart k-continue
             `(let* ((src0-addr ,(cg+ dseg (cg-register-ref idx-SI eas)))
                     (src1-addr ,(cg+ 'es (cg-register-ref idx-DI eas)))
                     (src0 (RAM src0-addr ,eos))
                     (src1 (RAM src1-addr ,eos))
                     ,@(cgl-register-update idx-DI eas (cg+ 'DI 'n))
                     ,@(cgl-register-update idx-SI eas (cg+ 'SI 'n)))
                (if ,(case repeat
                       ((z) '(and (not (eqv? count 0)) (eqv? src0 src1)))
                       ((nz) '(and (not (eqv? count 0)) (not (eqv? src0 src1))))
                       (else #f))
                    (lp-rep DI SI count iterations)
                    (let* (,@(if repeat (cgl-register-update idx-CX eas 'count) '())
                           ,@(cgl-arithmetic 'cmp-result #f eos 'CMP 'src0 'src1))
                      ,k-continue)))))

  (define (cg-scas dseg repeat eos eas k-restart k-continue)
    ;; CMPS with eAX as the fixed first operand. Reads from es:rDI(!)
    ;; and compares with eAX, increments or decrements rDI. With
    ;; repeat=z it repeats until rCX=0 or ZF=0, with repeat=nz it
    ;; repeats until rCX=0 or ZF=1. For rDI/rCX eas is used. es can
    ;; not be overridden.
    `(let ((src0 ,(cg-register-ref idx-AX eos)))
       ,(%cg-rep repeat eos eas k-restart k-continue
                 `(let* ((src1-addr ,(cg+ 'es (cg-register-ref idx-DI eas)))
                         (src1 (RAM src1-addr ,eos))
                         ,@(cgl-register-update idx-DI eas (cg+ 'DI 'n)))
                    (if ,(case repeat
                           ((z) '(and (not (eqv? count 0)) (eqv? src0 src1)))
                           ((nz) '(and (not (eqv? count 0)) (not (eqv? src0 src1))))
                           (else #f))
                        (lp-rep DI SI count iterations)
                        (let* (,@(if repeat (cgl-register-update idx-CX eas 'count) '())
                               ,@(cgl-arithmetic 'cmp-result #f eos 'CMP 'src0 'src1))
                          ,k-continue))))))

  (define (cg-outs dseg repeat eos eas k-restart k-continue)
    ;; Reads from dseg:rSI and writes to the port in DX, increments or
    ;; decrements rSI. With repeat=z it repeats until rCX=0. For
    ;; rDI/rCX eas is used. es can not be overridden.
    `(let ((dst-port ,(cg-register-ref idx-DX 16)))
       ,(%cg-rep repeat eos eas k-restart k-continue
                 `(let* ((src-addr ,(cg+ dseg (cg-register-ref idx-SI eas)))
                         ,@(cgl-register-update idx-SI eas (cg+ 'SI 'n)))
                    (I/O dst-port ,eos (RAM src-addr ,eos))
                    (if (not (eqv? count 0))
                        (lp-rep DI SI count iterations)
                        (let* (,@(if repeat (cgl-register-update idx-CX eas 'count) '()))
                          ,k-continue))))))

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

  ;; Generate code for a software interrupt, fault or trap.
  (define (%cg-int vec error-code return merge ip)
    (define idt 0)                   ;real mode interrupt vector table
    (let ((code
           `(let* (,@(cgl-merge-fl/eval merge))
              ,(cg-push* 16 '(fl) '(fxarithmetic-shift-right cs 4) ip
                         `(let* ((addr ,(cg+ idt (cgasl vec 2)))
                                 (off (RAM addr 16))
                                 (seg (RAM ,(cg+ 'addr 2) 16))
                                 (cs ,(cgasl 'seg 4))
                                 (fl (lambda ()
                                       ,(cgand '(fl) (fxnot (fxior flag-IF
                                                                   flag-TF
                                                                   flag-AC))))))
                            ,(return #f 'off))))))
      (if error-code
          (cg-push 16 error-code code)
          code)))

  (define (cg-int-software-interrupt vec return merge ip)
    (%cg-int vec #f return merge ip))

  ;; Various traps/faults that in general are restartable.
  (define (cg-int-divide-by-zero-error return merge start-ip)
    (%cg-int 0 #f return merge start-ip)) ;#DE

  (define (cg-int-breakpoint return merge ip)
    (%cg-int 3 #f return merge ip))     ;#BP

  (define (cg-int-overflow return merge ip)
    (%cg-int 4 #f return merge ip))     ;#OF

  (define (cg-int-invalid-opcode return merge cs start-ip)
    (when (machine-debug (current-machine))
      (print "Warning: translating invalid opcode at " (hex cs) ":" (hex start-ip)
             ": " (number->string (memory-u8-ref (real-pointer cs start-ip)) 16)
             " " (number->string (memory-u8-ref (real-pointer cs (+ start-ip 1))) 16)
             " ...: " (disassemble (copy-inst cs start-ip))))
    (%cg-int 6 #f return merge start-ip)) ;#UD

  (define (cg-int-device-not-available return merge start-ip)
    (%cg-int 7 #f return merge start-ip)) ;#NM

  (define (cg-int-stack return merge start-ip)
    (%cg-int 12 0 return merge start-ip)) ;#SS

  (define (cg-load-far-pointer sreg location modr/m eos continue merge ip)
    `(let* ((addr ,location)
            (off (RAM addr ,eos))
            (seg (RAM ,(cg+ 'addr (/ eos 8)) 16)))
       (let* (,@(cgl-reg-set modr/m eos 'off)
              (,sreg ,(cgasl 'seg 4)))
         ,(continue merge ip))))

  ;; Translate a basic block. This procedure reads instructions at
  ;; cs:ip until it finds a branch of some kind (or something
  ;; complicated), which ends the basic block. It returns a procedure
  ;; that takes the most-used machine registers, does something to
  ;; them and the machine, and returns a new set of registers.
  (define (generate-translation cs ip debug instruction-limit)
    (define (wrap expr)
      ;; Wrap the flags register and the arithmetic flags.
      `(lambda (bus fl AX CX DX BX SP BP SI DI
                    cs ds ss es fs gs)
         (define RAM
           (case-lambda
             ((addr size) (bus 'read-memory addr size))
             ((addr size value) (bus 'write-memory addr size value))))
         (define I/O
           (case-lambda
             ((addr size) (bus 'read-i/o addr size))
             ((addr size value) (bus 'write-i/o addr size value))))
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
       (when (and debug disassemble)
         (print (hex (segment-selector cs) 4) ":" (hex ip 4) "  "
                (disassemble (copy-inst cs ip))))
       (let prefix ((ip ip)
                    (dseg 'ds)       ;segment for address calculations
                    (sseg 'ss)       ;segment for stack references
                    (eos 16)         ;effective operand size
                    (eas 16)         ;effective address size
                    (stack-size 16)  ;stack-size of the stack segment
                    (lock #f)
                    (repeat #f))
         ;; (print "Reading at CS:IP: " (hex (segment-selector cs)) ":" (hex ip))
         (with-instruction-u8* ((op <- cs ip))
           (case op
             ;; Prefixes. TODO: Limit the number of prefixes read, #UD
             ;; for invalid locks.
             ((#x26) (prefix ip 'es 'es eos eas stack-size lock repeat))
             ((#x2E) (prefix ip 'cs 'cs eos eas stack-size lock repeat))
             ((#x36) (prefix ip 'ss 'ss eos eas stack-size lock repeat))
             ((#x3E) (prefix ip 'ds 'ds eos eas stack-size lock repeat))
             ((#x64) (prefix ip 'fs 'fs eos eas stack-size lock repeat))
             ((#x65) (prefix ip 'gs 'gs eos eas stack-size lock repeat))
             ((#x66) (prefix ip dseg sseg 32 eas 32 lock repeat))
             ((#x67) (prefix ip dseg sseg eos 32 stack-size lock repeat))
             ((#xF0) (prefix ip dseg sseg eos eas stack-size #t repeat))
             ((#xF2) (prefix ip dseg sseg eos eas stack-size lock 'nz))
             ((#xF3) (prefix ip dseg sseg eos eas stack-size lock 'z))
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
                  ;; TODO: #x00 GROUP-6
                  ;; TODO: #x01 GROUP-7
                  ;; TODO: #x02 lar
                  ;; TODO: #x03 lsl
                  ;; TODO: #x06 clts
                  ;; TODO: #x20-#x24, #x26 mov for CR*, DR*
                  ((#x80 #x81 #x82 #x83 #x84 #x85 #x86 #x87 #x88 #x89 #x8A #x8B #x8C #x8D #x8E #x8F)
                   ;; Jcc Jz
                   (with-instruction-immediate-sx* ((disp <- cs ip eos))
                     (emit
                      `(let* (,@(cgl-merge-fl merge)) ;update fl early
                         (let ((ip (if ,(cg-test-cc (fxand op1 #b1111))
                                       ,(cgand #xffff (cg+ ip disp))
                                       ,ip)))
                           ,(return #f 'ip))))))
                  ((#x90 #x91 #x92 #x93 #x94 #x95 #x96 #x97 #x98 #x99 #x9A #x9B #x9C #x9D #x9E #x9F)
                   ;; SETcc Eb
                   (with-r/m-operand ((ip store location modr/m) (cs ip dseg sseg eas))
                     (emit
                      `(let (,@(cgl-r/m-set store location 8
                                            `(if ,(cg-test-cc (fxand op1 #b1111)) 1 0)))
                         ,(continue merge ip)))))
                  ((#xA0)               ; push *FS
                   (emit (cg-push 16 '(fxarithmetic-shift-right fs 4) (continue merge ip))))
                  ((#xA1)               ; pop *FS
                   (emit (cg-pop 16 'tmp `(let ((fs ,(cgasl 'tmp 4))) ,(continue merge ip)))))
                  ;; A3 BT is grouped with BTC
                  ((#xA4)               ; shld Ev Gv Ib
                   (with-r/m-operand ((ip store location modr/m) (cs ip dseg sseg eas))
                     (with-instruction-u8* ((imm <- cs ip))
                       (emit
                        `(let* (,@(cgl-arithmetic-shld 'result eos
                                                       (cg-r/m-ref store location eos)
                                                       (cg-reg-ref modr/m eos)
                                                       imm)
                                ,@(cgl-r/m-set store location eos 'result))
                           ,(continue #t ip))))))
                  ((#xA5)               ; shld Ev Gv *CL
                   (with-r/m-operand ((ip store location modr/m) (cs ip dseg sseg eas))
                     (emit
                      `(let* (,@(cgl-arithmetic-shld 'result eos
                                                     (cg-r/m-ref store location eos)
                                                     (cg-reg-ref modr/m eos)
                                                     (cg-register-ref idx-CX 8))
                              ,@(cgl-r/m-set store location eos 'result))
                         ,(continue #t ip)))))
                  ((#xA8)               ; push *GS
                   (emit (cg-push 16 '(fxarithmetic-shift-right gs 4) (continue merge ip))))
                  ((#xA9)               ; pop *GS
                   (emit (cg-pop 16 'tmp `(let ((gs ,(cgasl 'tmp 4))) ,(continue merge ip)))))
                  ;; AB BTS is grouped with BTC
                  ((#xAC)               ; shrd Ev Gv Ib
                   (with-r/m-operand ((ip store location modr/m) (cs ip dseg sseg eas))
                     (with-instruction-u8* ((imm <- cs ip))
                       (emit
                        `(let* ((input (bitwise-ior
                                        (bitwise-arithmetic-shift-left ,(cg-reg-ref modr/m eos) ,eos)
                                        ,(cg-r/m-ref store location eos)))
                                ,@(cgl-arithmetic 'result #f eos 'SHRD 'input imm)
                                ,@(cgl-r/m-set store location eos 'result))
                           ,(continue #t ip))))))
                  ((#xAD)               ; shrd Ev Gv *CL
                   (with-r/m-operand ((ip store location modr/m) (cs ip dseg sseg eas))
                     (emit
                      `(let* ((input (bitwise-ior
                                      (bitwise-arithmetic-shift-left ,(cg-reg-ref modr/m eos) ,eos)
                                      ,(cg-r/m-ref store location eos)))
                              ,@(cgl-arithmetic 'result #f eos 'SHRD
                                                'input (cg-register-ref idx-CX 8))
                              ,@(cgl-r/m-set store location eos 'result))
                         ,(continue #t ip)))))
                  ((#xAF)               ; imul Gv Ev
                   (with-r/m-operand ((ip store location modr/m) (cs ip dseg sseg eas))
                     (emit
                      `(let* (,@(cgl-arithmetic 'result '_ eos 'IMUL
                                                (cg-reg-ref modr/m eos)
                                                (cg-r/m-ref store location eos))
                              ,@(cgl-reg-set modr/m eos 'result))
                         ,(continue #t ip)))))
                  ;; B3 BTR is grouped with BTC
                  ((#xB2 #xB4 #xB5)     ; lss Gv Mp, lfs Gv Mp, lgs Gv Mp
                   (with-r/m-operand ((ip store location modr/m) (cs ip dseg sseg eas))
                     (if (eq? store 'mem)
                         (let ((sreg (case op1
                                       ((#xB2) 'ss)
                                       ((#xB4) 'fs)
                                       ((#xB5) 'gs))))
                           (emit (cg-load-far-pointer sreg location modr/m eos continue merge ip)))
                         (emit (cg-int-invalid-opcode return merge cs start-ip)))))
                  ((#xB6 #xB7)          ; movzx Gv Eb, movzx Gv Ew
                   (let ((os (if (eqv? op1 #xB6) 8 16)))
                     (with-r/m-operand ((ip store location modr/m) (cs ip dseg sseg eas))
                       (emit
                        `(let* (,@(cgl-reg-set modr/m eos (cg-r/m-ref store location os)))
                           ,(continue merge ip))))))
                  ((#xBA)               ; bt Ev Ib, bts Ev Ib, btr Ev Ib, btc Ev Ib
                   (with-r/m-operand ((ip store location modr/m) (cs ip dseg sseg eas))
                     (with-instruction-u8* ((imm <- cs ip))
                       (let ((operator (vector-ref GROUP-8 (ModR/M-reg modr/m))))
                         (if operator
                             (emit
                              `(let* (,@(cgl-arithmetic 'result '_ eos operator
                                                        (cg-r/m-ref store location eos)
                                                        imm)
                                      ,@(cgl-r/m-set store location eos 'result))
                                 ,(continue #t ip)))
                             (emit (cg-int-invalid-opcode return merge cs start-ip)))))))
                  ((#xA3 #xAB #xB3 #xBB) ; bt Ev Gv, bts Ev Gv, btr Ev Gv, btc Ev Gv
                   (let ((operator
                          (case op1
                            ((#xA3) 'BT)
                            ((#xAB) 'BTS)
                            ((#xB3) 'BTR)
                            ((#xBB) 'BTC))))
                     (with-r/m-operand ((ip store location modr/m) (cs ip dseg sseg eas))
                       (emit
                        `(let* (,@(cgl-arithmetic 'result '_ eos operator
                                                  (cg-r/m-ref store location eos)
                                                  (cg-reg-ref modr/m eos))
                                ,@(cgl-r/m-set store location eos 'result))
                           ,(continue #t ip))))))
                  ((#xBC #xBD)          ; bsf Gv Ev, bsr Gv Ev
                   (let ((operator (if (eqv? op1 #xBC) 'BSF 'BSR)))
                     (with-r/m-operand ((ip store location modr/m) (cs ip dseg sseg eas))
                       (emit
                        `(let* (,@(cgl-arithmetic 'result '_ eos operator
                                                  (cg-reg-ref modr/m eos)
                                                  (cg-r/m-ref store location eos))
                                ,@(cgl-reg-set modr/m eos 'result))
                           ,(continue #t ip))))))
                  ((#xBE #xBF)          ; movsx Gv Eb, movsx Gv Ew
                   (let ((os (if (eqv? op1 #xBE) 8 16)))
                     (with-r/m-operand ((ip store location modr/m) (cs ip dseg sseg eas))
                       (emit
                        `(let* ((value ,(cg-recover-sign (cg-r/m-ref store location os) os))
                                ,@(cgl-reg-set modr/m eos 'value))
                           ,(continue merge ip))))))
                  (else
                   (if (not first?)
                       (emit (return merge start-ip))
                       (emit (cg-int-invalid-opcode return merge cs start-ip)))))))

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
             ;; #x27 daa is grouped with das below
             ((#x28 #x29 #x2A #x2B #x2C #x2D)
              (emit (cg-arithmetic-group op 'SUB ip cs dseg sseg eos eas continue)))
             ((#x27 #x2F)               ; daa, das
              (let ((fx^ (if (eqv? op #x27) 'fx+ 'fx-)))
                ;; TODO: This results in terrible code that cp0
                ;; doesn't do anything about, maybe it should just be
                ;; put in a library and be done with.
                (emit
                 `(let*-values (((al AF old-CF)
                                 (values ,(cg-register-ref idx-AX 8) (fl-AF) (fl-CF)))
                                ((al AF CF)
                                 (let ((nibble (fxbit-field al 0 4)))
                                   (if (or (fx>? nibble 9) (not (eqv? AF 0)))
                                       (values (,fx^ al #x06) ,flag-AF
                                               ;; CF is set if al now has a carry/borrow.
                                               (if (fxbit-set? (,fx^ nibble #x06) 4)
                                                   ,flag-CF old-CF))
                                       (values al 0 old-CF))))
                                ((al AF CF)
                                 (let ((nibble (fxbit-field al 4 8)))
                                   (if (or (fx>? nibble 9) (not (eqv? old-CF 0)))
                                       (values (,fx^ al #x60) AF ,flag-CF)
                                       (values al AF 0)))))
                    (let* (,@(cgl-register-update idx-AX 8 'al)
                           (fl-OF (lambda () 0)) ;undefined
                           (fl-SF (lambda () ,(cg-SF 'al 8)))
                           (fl-ZF (lambda () ,(cg-ZF 'al)))
                           (fl-AF (lambda () AF))
                           (fl-PF (lambda () ,(cg-PF 'al)))
                           (fl-CF (lambda () CF)))
                      ,(continue #t ip))))))
             ((#x30 #x31 #x32 #x33 #x34 #x35)
              (emit (cg-arithmetic-group op 'XOR ip cs dseg sseg eos eas continue)))
             ;; 37 aaa is grouped with aas below.
             ((#x38 #x39 #x3A #x3B #x3C #x3D)
              (emit (cg-arithmetic-group op 'CMP ip cs dseg sseg eos eas continue)))
             ((#x37 #x3F)               ; aaa, aas
              (let ((fx^ (if (eqv? op #x37) 'fx+ 'fx-)))
                (emit
                 `(let* ((ax ,(cg-register-ref idx-AX 16))
                         (nibble (fxand ax #xF))
                         (ah (fxarithmetic-shift-right ax 8))
                         (adjust (or (fx>? nibble 9) (not (eqv? (fl-AF) 0)))))
                    (let* ((value (if adjust
                                      (fxior (fxarithmetic-shift-left (,fx^ ah 1) 8)
                                             (fxand (,fx^ nibble 6) #xF))
                                      (fxand ax #xFF0F)))
                           ,@(cgl-register-update idx-AX 16 'value)
                           (fl-OF (lambda () 0)) ;undefined
                           (fl-SF (lambda () 0)) ;undefined
                           (fl-ZF (lambda () 0)) ;undefined
                           (fl-AF (lambda () (if adjust ,flag-AF 0)))
                           (fl-PF (lambda () 0)) ;undefined
                           (fl-CF (lambda () (if adjust ,flag-CF 0))))
                      ,(continue #t ip))))))
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
              (let ((reg (fxand op #x7)))
                (emit (cg-pop eos 'tmp
                              `(let* (,@(cgl-register-update reg eos 'tmp))
                                 ,(continue merge ip))))))
             ((#x60)                    ; pushaw / pushad
              (emit
               `(let ((saved-SP SP))
                  ,(cg-push* eos 'AX 'CX 'DX 'BX 'saved-SP 'BP 'SI 'DI
                             (continue merge ip)))))
             ((#x61)                    ; popaw / popad
              (emit
               (cg-pop* eos 'DI^ 'SI^ 'BP^ 'SP^ 'BX^ 'DX^ 'CX^ 'AX^
                        `(let* (,@(cgl-register-update idx-AX eos 'AX^)
                                ,@(cgl-register-update idx-CX eos 'CX^)
                                ,@(cgl-register-update idx-DX eos 'DX^)
                                ,@(cgl-register-update idx-BX eos 'BX^)
                                ,@(cgl-register-update idx-SP eos 'SP^)
                                ,@(cgl-register-update idx-BP eos 'BP^)
                                ,@(cgl-register-update idx-SI eos 'SI^)
                                ,@(cgl-register-update idx-DI eos 'DI^))
                           ,(continue merge ip)))))
             ;; TODO: 62 bound Gv, Ma
             ;; TODO: 63 arpl Ew Rw
             ((#x68)                    ; push Iz
              (with-instruction-immediate* ((imm <- cs ip eos))
                (emit (cg-push eos imm (continue merge ip)))))
             ((#x69)                    ; imul Gv Ev Iz
              (with-r/m-operand ((ip store location modr/m) (cs ip dseg sseg eas))
                (with-instruction-immediate* ((imm <- cs ip eos))
                  (emit
                   `(let* (,@(cgl-arithmetic 'result '_ eos 'IMUL
                                             (cg-r/m-ref store location eos) imm)
                           ,@(cgl-reg-set modr/m eos 'result))
                      ,(continue #t ip))))))
             ((#x6A)                    ; push IbS
              (with-instruction-s8* ((imm <- cs ip))
                (emit (cg-push eos imm (continue merge ip)))))
             ((#x6B)                    ; imul Gv Ev IbS
              (with-r/m-operand ((ip store location modr/m) (cs ip dseg sseg eas))
                (with-instruction-s8* ((imm <- cs ip))
                  (emit
                   `(let* (,@(cgl-arithmetic 'result '_ eos 'IMUL
                                             (cg-r/m-ref store location eos) imm)
                           ,@(cgl-reg-set modr/m eos 'result))
                      ,(continue #t ip))))))
             ;; TODO: ins Yb *DX, ins Yz *DX
             ((#x6E #x6F)               ; outs *DX Xb, outs *DX Xz
              (let ((eos (if (eqv? op #x6E) 8 eos)))
                (cond ((eqv? repeat 'z)
                       (cond ((not first?)
                              (emit (return merge start-ip)))
                             (else
                              (emit
                               `(let* (,@(cgl-merge-fl merge)) ;update fl early
                                  ,(cg-outs dseg repeat eos eas
                                            (return #f start-ip) (return #f ip)))))))
                      (else
                       (emit (cg-outs dseg repeat eos eas '(error) (continue merge ip)))))))
             ((#x70 #x71 #x72 #x73 #x74 #x75 #x76 #x77 #x78 #x79 #x7A #x7B #x7C #x7D #x7E #x7F)
              ;; Jcc Jb
              (with-instruction-s8* ((disp <- cs ip))
                (emit
                 `(let* (,@(cgl-merge-fl merge)) ;update fl early
                    (let ((ip (if ,(cg-test-cc (fxand op #b1111))
                                  ,(cgand #xffff (cg+ ip disp))
                                  ,ip)))
                      ,(return #f 'ip))))))
             ((#x80 #x81 #x82 #x83)
              ;; Group 1. Eb Ib, Ev Iz, Eb Ib, Ev IbS.
              (let ((eos (if (fxeven? op) 8 eos))
                    (immsize (if (eqv? op #x81) eos 8)))
                (with-r/m-operand ((ip store location modr/m)
                                   (cs ip dseg sseg eas))
                  (with-instruction-immediate* ((imm <- cs ip immsize))
                    (let* ((imm (if (eqv? op #x83) (sign-extend imm 8) imm))
                           (imm (trunc imm eos))
                           (operator (vector-ref GROUP-1 (ModR/M-reg modr/m))))
                      (emit
                       `(let* (,@(cgl-arithmetic 'result #f eos operator
                                                 (cg-r/m-ref store location eos)
                                                 imm)
                               ,@(case operator
                                   ((TEST CMP) '())
                                   (else (cgl-r/m-set store location eos 'result))))
                          ,(continue #t ip))))))))
             ((#x84 #x85)    ; test Eb Gb, test Ev Gv
              (let ((eos (if (eqv? op #x84) 8 eos)))
                (with-r/m-operand ((ip store location modr/m) (cs ip dseg sseg eas))
                  (emit
                   `(let* (,@(cgl-arithmetic 'result #f eos 'TEST
                                             (cg-r/m-ref store location eos)
                                             (cg-reg-ref modr/m eos)))
                      ,(continue #t ip))))))
             ((#x86 #x87)               ; xchg Eb Gb, xchg Ev Gv
              (let ((eos (if (eqv? op #x86) 8 eos)))
                (with-r/m-operand ((ip store location modr/m) (cs ip dseg sseg eas))
                  (emit
                   `(let* ((v0 ,(cg-r/m-ref store location eos))
                           (v1 ,(cg-reg-ref modr/m eos))
                           ,@(cgl-r/m-set store location eos 'v1)
                           ,@(cgl-reg-set modr/m eos 'v0))
                      ,(continue merge ip))))))
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
             ((#x8C)                    ; mov Ev Sw
              (with-r/m-operand ((ip store location modr/m) (cs ip dseg sseg eas))
                (let ((src (vector-ref '#(es cs ss ds fs gs #f #f) (ModR/M-reg modr/m))))
                  (emit
                   (if src
                       `(let* (,@(cgl-r/m-set store location eos (cgasr src 4)))
                          ,(continue merge ip))
                       (cg-int-invalid-opcode return merge cs start-ip))))))
             ((#x8D)                    ; lea Gv M
              (with-r/m-operand ((ip store location modr/m) (cs ip 0 0 eas))
                (emit
                 (if (eq? store 'mem)
                     `(let* (,@(cgl-reg-set modr/m eos location))
                        ,(continue merge ip))
                     (cg-int-invalid-opcode return merge cs start-ip)))))
             ((#x8E)                    ; mov Sw Ew
              (with-r/m-operand ((ip store location modr/m) (cs ip dseg sseg eas))
                (let ((reg (vector-ref '#(es #f ss ds fs gs #f #f) (ModR/M-reg modr/m))))
                  (emit
                   (if reg
                       `(let* ((,reg ,(cgasl (cg-r/m-ref store location 16) 4)))
                          ,(continue merge ip))
                       (cg-int-invalid-opcode return merge cs start-ip))))))
             ((#x8F)                    ; Group 1A - pop Ev
              (with-r/m-operand ((ip store location modr/m) (cs ip dseg sseg eas))
                (emit (if (eqv? (ModR/M-reg modr/m) 0)
                          (cg-pop eos 'tmp
                                  `(let* (,@(cgl-r/m-set store location eos 'tmp))
                                     ,(continue merge ip)))
                          (cg-int-invalid-opcode return merge cs start-ip)))))
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
             ((#x98)                    ; cbw / cwde / cdqe
              ;; Sign-extend al or ax, keeping the register value unsigned.
              (let ((src-width (fxasr eos 1)))
                (emit
                 `(let* ((tmp0 ,(cg-register-ref idx-AX src-width))
                         (tmp1 (if ,(cgbit-set? 'tmp0 (- src-width 1))
                                   ,(cgior 'tmp0 (fwasl (- (fwasl 1 src-width) 1)
                                                        src-width))
                                   ,(cgand 'tmp0 (- (fwasl 1 src-width) 1))))
                         ,@(cgl-register-update idx-AX eos 'tmp1))
                    ,(continue merge ip)))))
             ((#x99)                    ; cwd / cdq / cqo
              ;; Copy sign of eAX into eDX
              (emit
               `(let* ((tmp1 (if ,(cgbit-set? 'AX (- eos 1))
                                 ,(- (fwasl 1 eos) 1)
                                 0))
                       ,@(cgl-register-update idx-DX eos 'tmp1))
                  ,(continue merge ip))))
             ((#x9A)                    ; callf Ap
              (with-instruction-immediate* ((off <- cs ip eos)
                                            (seg <- cs ip 16))
                (emit
                 (cg-push* 16 '(fxarithmetic-shift-right cs 4) ip
                           `(let ((cs ,(fxasl seg 4))
                                  (ip ,off))
                              ,(return merge 'ip))))))
             ((#x9B)                    ; wait/fwait
              ;; TODO: #NM if MP and TS are both set in CR0
              (emit `(if #f
                         ,(cg-int-device-not-available return merge start-ip)
                         ,(continue merge ip))))
             ((#x9C)                    ; pushfw / pushfd
              (emit `(let* (,@(cgl-merge-fl/eval merge)
                            (tmp ,(cgand '(fl) (bitwise-not (bitwise-ior flag-RF flag-VM)))))
                       ,(cg-push eos 'tmp (continue #f ip)))))
             ((#x9D)                    ; popfw / popfd
              (emit
               (cg-pop eos 'tmp
                       `(let ((fl (lambda ()
                                    ;; Update the eos lower bits of
                                    ;; flags. Discard the fl-*
                                    ;; procedures.
                                    ,(cgior flags-always-set
                                            (cgior
                                             ;; Do not change RF/VM
                                             (cgand '(fl) (bitwise-ior flag-RF flag-VM))
                                             (cgand (cgior 'tmp
                                                           ;; popfw leaves upper bits unchanged
                                                           (cgand '(fl) (bitwise-not
                                                                         (- (expt 2 eos) 1))))
                                                    ;; Ignore these flags
                                                    (bitwise-not
                                                     (bitwise-ior flags-never-set flag-RF flag-VM))))))))
                          ;; Need to return so that the fl-*
                          ;; procedures will be reloaded.
                          ,(return #f ip)))))
             ((#x9E)                    ; sahf
              (emit
               `(let* ((AH ,(cg-register-ref idx-AH 8))
                       (fl-SF (lambda () ,(cgand flag-SF 'AH)))
                       (fl-ZF (lambda () ,(cgand flag-ZF 'AH)))
                       (fl-AF (lambda () ,(cgand flag-AF 'AH)))
                       (fl-PF (lambda () ,(cgand flag-PF 'AH)))
                       (fl-CF (lambda () ,(cgand flag-CF 'AH))))
                  ,(continue #t ip))))
             ((#x9F)                    ; lahf
              (emit
               `(let* (,@(cgl-merge-fl/eval merge)
                       (tmp ,(cgior (cgand '(fl) (fxior flag-SF flag-ZF flag-AF flag-PF flag-CF))
                                    flags-always-set))
                       ,@(cgl-register-update idx-AH 8 'tmp))
                  ,(continue #f ip))))
             ((#xA0 #xA1)                    ; mov *AL Ob, mov *rAX Ov
              (let ((eos (if (eqv? op #xA0) 8 eos)))
                (with-instruction-immediate* ((addr <- cs ip eas))
                  (emit
                   `(let* (,@(cgl-register-update idx-AX eos `(RAM ,(cg+ dseg addr) ,eos)))
                      ,(continue merge ip))))))
             ((#xA2 #xA3)               ; mov Ob *AL, mov Ov *rAX
              (let ((eos (if (eqv? op #xA2) 8 eos)))
                (with-instruction-immediate* ((addr <- cs ip eas))
                  (emit
                   `(begin
                      (RAM ,(cg+ dseg addr) ,eos ,(cg-register-ref idx-AX eos))
                      ,(continue merge ip))))))
             ((#xA4 #xA5)               ; movs Yb Xb, movs Yv Xv
              (let ((eos (if (eqv? op #xA4) 8 eos)))
                (cond (repeat
                       (cond ((not first?)
                              (emit (return merge start-ip)))
                             (else
                              (emit
                               `(let* (,@(cgl-merge-fl merge)) ;update fl early
                                  ,(cg-movs dseg repeat eos eas
                                            (return #f start-ip) (return #f ip)))))))
                      (else
                       (emit (cg-movs dseg repeat eos eas '(error) (continue merge ip)))))))
             ((#xA6 #xA7)               ; cmps Xb Yb, cmps Xv Yv
              (let ((eos (if (eqv? op #xA6) 8 eos)))
                (cond (repeat
                       (cond ((not first?) ;finish all previous code
                              (emit (return merge start-ip)))
                             (else
                              (emit (cg-cmps dseg repeat eos eas
                                             (return #f start-ip) (return #t ip))))))
                      (else
                       (emit (cg-cmps dseg repeat eos eas '(error) (continue #t ip)))))))
             ((#xA8 #xA9)               ; test *AL Ib, test *rAX Iz
              (let ((eos (if (eqv? op #xA8) 8 eos)))
                (with-instruction-immediate* ((imm <- cs ip eos))
                  (emit
                   `(let* (,@(cgl-arithmetic 'result #f eos 'TEST
                                             (cg-register-ref idx-AX eos)
                                             imm))
                      ,(continue #t ip))))))
             ((#xAA #xAB)               ; stos Yb *AL, stos Yv *rAX
              (let ((eos (if (eqv? op #xAA) 8 eos)))
                (cond (repeat
                       (cond ((not first?)
                              (emit (return merge start-ip)))
                             (else
                              (emit
                               `(let* (,@(cgl-merge-fl merge)) ;update fl early
                                  ,(cg-stos dseg repeat eos eas
                                            (return #f start-ip) (return #f ip)))))))
                      (else
                       (emit (cg-stos dseg repeat eos eas '(error) (continue merge ip)))))))
             ((#xAC #xAD)               ; lods *AL Xb, lods *rAX Xv
              (let ((eos (if (eqv? op #xAC) 8 eos)))
                (cond (repeat
                       (cond ((not first?)
                              (emit (return merge start-ip)))
                             (else
                              (emit
                               `(let* (,@(cgl-merge-fl merge)) ;update fl early
                                  ,(cg-lods dseg repeat eos eas
                                            (return #f start-ip) (return #f ip)))))))
                      (else
                       (emit (cg-lods dseg repeat eos eas '(error) (continue merge ip)))))))
             ((#xAE #xAF)               ; scas *AL Yb, *rAX Yv
              (let ((eos (if (eqv? op #xAE) 8 eos)))
                (cond (repeat
                       (cond ((not first?) ;finish all previous code
                              (emit (return merge start-ip)))
                             (else
                              (emit (cg-scas dseg repeat eos eas
                                             (return #f start-ip) (return #t ip))))))
                      (else
                       (emit (cg-scas dseg repeat eos eas '(error) (continue #t ip)))))))
             ((#xB0 #xB1 #xB2 #xB3 #xB4 #xB5 #xB6 #xB7 #xB8 #xB9 #xBA #xBB #xBC #xBD #xBE #xBF)
              ;; mov *AL/R8L...*BH/R15L Ib
              ;; mov *rAX/r8..*rDI/r15 Iv
              (let ((eos (if (fx<? op #xB8) 8 eos)))
                (with-instruction-immediate* ((imm <- cs ip eos))
                  (emit
                   `(let* (,@(cgl-register-update (fxand op #x7) eos imm))
                      ,(continue merge ip))))))
             ((#xC0 #xC1)
              ;; Shift Group 2. Eb Ib, Ev Ib.
              (let ((eos (if (eqv? op #xC0) 8 eos)))
                (with-r/m-operand ((ip store location modr/m) (cs ip dseg sseg eas))
                  (with-instruction-u8* ((imm <- cs ip))
                    (let ((operator (vector-ref GROUP-2 (ModR/M-reg modr/m))))
                      (emit
                       `(let* (,@(cgl-arithmetic 'result #f eos operator
                                                 (cg-r/m-ref store location eos)
                                                 imm)
                               ,@(cgl-r/m-set store location eos 'result))
                          ,(continue #t ip))))))))
             ((#xC2 #xC3)               ; ret Iw / ret
              (emit
               (cg-pop eos 'saved-ip
                       `(let ((ip^ ,(if (eqv? eos 32) 'saved-ip (cgand 'saved-ip #xffff))))
                          ,(case op
                             ((#xC2)
                              (with-instruction-u16* ((imm <- cs ip))
                                ;; pop imm bytes off the stack
                                `(let* (,@(cgl-register-update idx-SP eas (cg+ (cg-trunc 'SP eas)
                                                                               imm)))
                                   ,(return merge 'ip^))))
                             (else
                              (return merge 'ip^)))))))
             ((#xC4 #xC5)               ; les Gz Mp, lds Gz Mp
              (with-r/m-operand ((ip store location modr/m) (cs ip dseg sseg eas))
                (if (eq? store 'mem)
                    (let ((sreg (if (eqv? op #xC4) 'es 'ds)))
                      (emit (cg-load-far-pointer sreg location modr/m eos continue merge ip)))
                    (emit (cg-int-invalid-opcode return merge cs start-ip)))))
             ((#xC6 #xC7)               ; Group 11
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
                       (emit (cg-int-invalid-opcode return merge cs start-ip))))))))
             ((#xC8)                    ; enter Iw Ib
              ;; The mother pearl of CISC instructions.
              (with-instruction-immediate* ((storage <- cs ip 16)
                                            (level <- cs ip 8))
                (emit
                 (cg-push stack-size 'BP
                          `(let ((frame-pointer ,(cg-register-ref idx-SP eos))
                                 (old-BP ,(cg-register-ref idx-BP eos)))
                             ,(let lp ((level (fxand level #b11111)))
                                (cond
                                  ((> level 0)
                                   `(let lp-push ((i 1))
                                      (cond
                                        ((fx<? i ,level)
                                         ;; Copy previous frame pointers.
                                         (let* ((tmp (RAM ,(cg+ 'ss `(- BP (* i ,(/ stack-size 8)))) eos)))
                                           ,(cg-push eos 'tmp '(lp-push (fx- level 1)))))
                                       (else
                                        ,(cg-push eos 'frame-pointer (lp 0))))))
                                  (else
                                   `(let* (,@(cgl-register-update idx-SP eos
                                                                  (cg- (cg-register-ref idx-SP eos)
                                                                       storage))
                                           ;; TODO: Check that SP can be written to.
                                           ,@(cgl-register-update idx-BP eos 'frame-pointer))
                                      ,(continue merge ip))))))))))
             ((#xC9)                    ; leave
              (emit
               `(let* (,@(cgl-register-update idx-SP stack-size (cg-register-ref idx-BP stack-size)))
                  ,(cg-pop stack-size 'tmp
                           `(let* (,@(cgl-register-update idx-BP stack-size 'tmp))
                              ,(continue merge ip))))))
             ((#xCA #xCB)               ; retf Iw / retf
              (emit
               (cg-pop* eos 'off 'seg
                        `(let ((cs ,(cgasl (cgand 'seg #xffff) 4))
                               (ip^ ,(if (eqv? eos 32) 'off (cgand 'off #xffff))))
                           ,(case op
                              ((#xCA)
                               (with-instruction-u16* ((imm <- cs ip))
                                 ;; pop imm bytes off the stack
                                 `(let* (,@(cgl-register-update idx-SP eas (cg+ (cg-trunc 'SP eas)
                                                                                imm)))
                                    ,(return merge 'ip^))))
                              (else
                               (return merge 'ip^)))))))
             ((#xCC)                    ; int 3
              (emit (cg-int-breakpoint return merge ip)))
             ((#xCD)                    ; int Ib
              (with-instruction-u8* ((vec <- cs ip))
                (emit (cg-int-software-interrupt vec return merge ip))))
             ((#xCE)                    ; into
              (emit
               `(if ,(eqv? '(fl-OF) 0)
                    ,(return merge ip)
                    ,(cg-int-overflow return merge ip))))
             ((#xCF)                    ; iret
              (emit (cg-pop* eos 'saved-ip 'saved-cs 'saved-flags
                             `(let ((cs ,(cgasl 'saved-cs 4))
                                    (fl (lambda ()
                                          ,(cgand 'saved-flags #xffff))))
                                ;; No need to merge the flags.
                                ,(return #f 'saved-ip)))))
             ((#xD0 #xD1 #xD2 #xD3)
              ;; Shift Group 2. Eb 1, Ev 1, Eb *CL, Ev *CL.
              (let ((eos (if (fxeven? op) 8 eos)))
                (with-r/m-operand ((ip store location modr/m)
                                   (cs ip dseg sseg eas))
                  (let ((imm (if (fx<? op #xD2) 1 (cgand 'CX #xff)))
                        (operator (vector-ref GROUP-2 (ModR/M-reg modr/m))))
                    (emit
                     `(let* (,@(cgl-arithmetic 'result #f eos operator
                                               (cg-r/m-ref store location eos)
                                               imm)
                             ,@(cgl-r/m-set store location eos 'result))
                        ,(continue #t ip)))))))
             ((#xD4 #xD5)               ; aam, aad
              (with-instruction-u8* ((imm <- cs ip))
                (let ((operator (if (eqv? op #xD4) 'AAM 'AAD))
                      (eos (if (eqv? op #xD4) 8 16)))
                  (emit `(let* (,@(cgl-arithmetic 'result #f 16 operator
                                                  (cg-register-ref idx-AX eos)
                                                  imm)
                                ,@(cgl-register-update idx-AX 16 'result))
                           ,(continue #t ip))))))
             ((#xD6)                    ; salc
              (emit
               `(let (,@(cgl-register-update idx-AX 8
                                             `(if (not (eqv? (fl-CF) 0))
                                                  #xff #x00)))
                  ,(continue merge ip))))
             ((#xD7)                    ; xlatb
              (emit
               `(let* ((addr ,(cg+ dseg (cg+ (cg-register-ref idx-BX eas)
                                             (cg-register-ref idx-AX 8))))
                       (result (RAM addr 8))
                       ,@(cgl-register-update idx-AX 8 'result))
                  ,(continue merge ip))))
             ((#xD8 #xD9 #xDA #xDB #xDC #xDD #xDE #xDF) ;x87 instructions
              (emit (cg-int-device-not-available return merge start-ip)))
             ((#xE0 #xE1 #xE2)          ; loopnz Jb, loopz Jb, loop Jb
              (with-instruction-s8* ((disp <- cs ip))
                ;; eas determines if CX or ECX is used.
                (cond ((eqv? (fw+ ip disp) start-ip)
                       (emit            ;pointless timing delay
                        `(let* (,@(cgl-register-update idx-CX eas 0))
                           ,(continue merge ip))))
                      (else
                       (emit
                        `(let* ((count ,(cg-trunc (cg- (cg-register-ref idx-CX eas) 1) eas))
                                ,@(cgl-register-update idx-CX eas 'count))
                           (let ((ip^ (if (and (not (eqv? count 0))
                                               ,(case op
                                                  ((#xE0) '(eqv? (fl-ZF) 0)) ;loopnz
                                                  ((#xE1) '(not (eqv? (fl-ZF) 0))) ;loopz
                                                  (else #t)))
                                          ,(cgand #xffff (cg+ ip disp))
                                          ,ip)))
                             ,(return merge 'ip^))))))))
             ((#xE3)                    ; jcxz Jb, jecxz Jb
              (with-instruction-s8* ((disp <- cs ip))
                (emit
                 `(let ((ip (if (eqv? ,(cg-register-ref idx-CX eas) 0)
                                ,(cgand #xffff (cg+ ip disp))
                                ,ip)))
                    ,(return merge 'ip)))))
             ((#xE4 #xE5)               ; in *AL Ib, in *eAX Ib
              (let ((eos (if (eqv? op #xE4) 8 eos)))
                (with-instruction-u8* ((imm <- cs ip))
                  (emit `(let* ((tmp (I/O ,imm ,eos))
                                ,@(cgl-register-update idx-AX eos 'tmp))
                           ,(continue merge ip))))))
             ((#xE6 #xE7)               ; out Ib *AL, out Ib *eAX
              (let ((eos (if (eqv? op #xE6) 8 eos)))
                (with-instruction-u8* ((imm <- cs ip))
                  (emit `(let ()
                           (I/O ,imm ,eos ,(cg-register-ref idx-AX eos))
                           ,(continue merge ip))))))
             ((#xE8)                    ; call Jz
              (with-instruction-immediate-sx* ((disp <- cs ip eos))
                (emit (cg-push 16 ip (return merge (fwand #xffff (fw+ ip disp)))))))
             ((#xE9)                    ; jmp Jz
              (with-instruction-immediate-sx* ((disp <- cs ip eos))
                (emit (return merge (fwand #xffff (fw+ ip disp))))))
             ((#xEA)                    ; jmpf Ap
              (with-instruction-immediate* ((off <- cs ip eos)
                                            (seg <- cs ip 16))
                (emit
                 `(let ((cs ,(fxasl seg 4))
                        (ip ,off))
                    ,(return merge 'ip)))))
             ((#xEB)                    ; jmp Jb
              (with-instruction-s8* ((disp <- cs ip))
                (emit (return merge (fwand #xffff (fw+ ip disp))))))
             ((#xEC #xED)               ; in *AL *DX, in *eAX *DX
              (let ((eos (if (eqv? op #xEC) 8 eos)))
                (emit `(let* ((port ,(cg-register-ref idx-DX 16))
                              (value (I/O port ,eos))
                              ,@(cgl-register-update idx-AX eos 'value))
                         ,(continue merge ip)))))
             ((#xEE #xEF)               ; out *DX *AL, out *DX *eAX
              (let ((eos (if (eqv? op #xEE) 8 eos)))
                (emit `(let ((port ,(cg-register-ref idx-DX 16))
                             (value ,(cg-register-ref idx-AX eos)))
                         (I/O port ,eos value)
                         ,(continue merge ip)))))
             ((#xF1)                    ; icebp / int1
              (emit (cg-int-software-interrupt 1 return merge ip)))
             ((#xF4)                    ; hlt
              ;; Halt and wait for an interrupt. This is also what
              ;; triggers the BIOS library if IF=0.
              (if (not first?)
                  (emit (return merge start-ip))
                  (emit (return merge #f))))
             ((#xF5)                    ; cmc
              (emit
               `(let* ((fl-CF (lambda () (fxxor (fl-CF) ,flag-CF))))
                  ,(continue #t ip))))
             ((#xF6 #xF7)               ; Unary group 3
              (let ((eos (if (eqv? op #xF6) 8 eos)))
                (with-r/m-operand ((ip store location modr/m) (cs ip dseg sseg eas))
                  (let ((operator (vector-ref GROUP-3 (ModR/M-reg modr/m)))
                        (input (cg-r/m-ref store location eos)))
                    (case operator
                      ((TEST)
                       (with-instruction-immediate* ((imm <- cs ip eos))
                         (emit
                          `(let* (,@(cgl-arithmetic 'result #f eos 'TEST input imm))
                             ,(continue #t ip)))))
                      ((NOT NEG)
                       (emit
                        `(let* (,@(cgl-arithmetic 'result #f eos operator input #f)
                                ,@(cgl-r/m-set store location eos 'result))
                           ,(continue #t ip))))
                      (else
                       ;; mul Eb, imul Eb, div Eb, idiv Eb, mul Ev,
                       ;; imul Ev, div Ev, idiv Ev.
                       (case eos
                         ((8)
                          ;; AX <- AL * input;
                          ;; AL <- AX quotient input, AH <- AX remainder input
                          (emit
                           `(let* ((input0
                                    ,(case operator
                                       ((DIV IDIV) (cg-register-ref idx-AX 16))
                                       ((MUL IMUL) (cg-register-ref idx-AX 8))))
                                   ,@(cgl-arithmetic 'result:l 'result:u eos operator
                                                     'input0 input))
                              (if div-trap?
                                  ,(cg-int-divide-by-zero-error return merge start-ip)
                                  (let* (,@(cgl-register-update idx-AX 16
                                                                (cgior 'result:l
                                                                       (cgasl 'result:u 8))))
                                    ,(continue #t ip))))))
                         (else
                          ;; eDX:eAX <- eAX * input;
                          ;; eAX <- eDX:eAX quotient input, eDX <- eDX:eAX remainder input
                          (emit
                           `(let* ((input0
                                    ,(case operator
                                      ((DIV IDIV) (cgior (cg-register-ref idx-AX eos)
                                                         (cgasl (cg-register-ref idx-DX eos) eos)))
                                      ((MUL IMUL) (cg-register-ref idx-AX eos))))
                                   ,@(cgl-arithmetic 'result:l 'result:u eos operator
                                                     'input0 input))
                              (if div-trap?
                                  ,(cg-int-divide-by-zero-error return merge start-ip)
                                  (let* (,@(cgl-register-update idx-AX eos 'result:l)
                                         ,@(cgl-register-update idx-DX eos 'result:u))
                                    ,(continue #t ip)))))))))))))
             ((#xF8)                    ; clc
              (emit
               `(let* ((fl-CF (lambda () 0)))
                  ,(continue #t ip))))
             ((#xF9)                    ; stc
              (emit
               `(let* ((fl-CF (lambda () ,flag-CF)))
                  ,(continue #t ip))))
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
             ((#xFE #xFF)                    ; Group 4 and Group 5
              (with-r/m-operand ((ip store location modr/m) (cs ip dseg sseg eas))
                (let ((operator (vector-ref GROUP-4/5 (ModR/M-reg modr/m))))
                  (cond
                    ((memq operator '(INC DEC))
                     (let ((eos (if (eqv? op #xFE) 8 eos)))
                       (emit
                        `(let* (,@(cgl-arithmetic 'result #f eos 'INC/DEC
                                                  (cg-r/m-ref store location eos)
                                                  (if (eqv? operator 'INC) 1 -1))
                                ,@(cgl-r/m-set store location eos 'result))
                           ,(continue #t ip)))))
                    ((not (eqv? op #xFF))
                     (emit (cg-int-invalid-opcode return merge cs start-ip)))
                    (else
                     (case operator
                       ((CALL)          ; call Ev
                        (emit
                         (cg-push eas ip
                                  `(let ((ip ,(cg-r/m-ref store location eos)))
                                     ,(return merge (cg-trunc 'ip eas))))))
                       ((CALLF)         ; callf Mp
                        (emit
                         (if (eq? store 'mem)
                             (cg-push* 16 '(fxarithmetic-shift-right cs 4) ip
                                       `(let* ((addr ,location)
                                               (off (RAM addr ,eos))
                                               (seg (RAM ,(cg+ 'addr (/ eos 8)) 16))
                                               (cs ,(cgasl 'seg 4)))
                                          ,(return merge 'off)))
                             (cg-int-invalid-opcode return merge cs start-ip))))
                       ((JMP)           ; jmp Ev
                        (emit
                         `(let* ((ip ,(cg-r/m-ref store location eos)))
                            ,(return merge (cg-trunc 'ip eas)))))
                       ((JMPF)          ; jmpf Mp
                        (emit (if (eq? store 'mem)
                                  `(let* ((addr ,location)
                                          (off (RAM addr ,eos))
                                          (seg (RAM ,(cg+ 'addr (/ eos 8)) 16))
                                          (cs ,(cgasl 'seg 4)))
                                     ,(return merge 'off))
                                  (cg-int-invalid-opcode return merge cs start-ip))))
                       ((PUSH)          ; push Ev
                        (emit (cg-push eos (cg-r/m-ref store location eos)
                                       (continue merge ip))))
                       (else            ; #UD
                        (emit (cg-int-invalid-opcode return merge cs start-ip)))))))))
             (else
              (if (not first?)
                  (emit (return merge start-ip))
                  (emit (cg-int-invalid-opcode return merge cs start-ip))))))))))

;;; Translation cache

  (define (translation-line-number address)
    ;; A cached translation is invalidated if a write is performed in
    ;; the same aligned 128-byte line as an existing translation.
    (fxarithmetic-shift-right address 7))

  (define (generate-translation! cs ip debug instruction-limit)
    (let ((trans (generate-translation cs ip debug instruction-limit)))
      (cond ((procedure? trans)
             trans)
            (else
             (when debug
               ;; (pretty-print trans)
               (pretty-print (expand/optimize trans)))
             (eval trans code-env)))))

  (define (translate cs ip debug instruction-limit)
    ;; TODO: Only keep the least recently used translations.
    (let* ((translations (machine-translations (current-machine)))
           (address (fw+ cs ip))
           (line (translation-line-number address))
           (line-table (hashtable-ref translations line #f)))
      (cond ((and line-table (hashtable-ref line-table address #f)))
            (else
             ;; TODO: need to know how many bytes were translated, or
             ;; need to limit the translation to within a line.
             ;; Self-modifying code might otherwise continue to run
             ;; inside a translation that is trying to invalidate
             ;; itself (although this is to some extent a problem even
             ;; with real hardware, and SMC is unusual).
             (let ((trans (generate-translation! cs ip debug instruction-limit))
                   (line-table (or line-table
                                   (let ((line-table (make-eqv-hashtable)))
                                     (hashtable-set! translations line line-table)
                                     line-table))))
               (hashtable-set! line-table address trans)
               trans)))))

  ;; There was an aligned write to the given address, invalidate any
  ;; cached translations.
  (define (invalidate-translation address)
    (let ((line (translation-line-number address))
          (translations (machine-translations (current-machine))))
      (when (hashtable-contains? translations line)
        (when (machine-debug (current-machine))
          (print "cache: invalidated translation at address " address))
        (hashtable-delete! translations line))))

;;; Main loop

  (define bus
    (case-lambda
      ((command addr size value)
       ;; TODO: Inline this code.
       (case command
         ((write-memory)
          (case size
            ((8) (memory-u8-set! addr value))
            ((16) (memory-u16-set! addr value))
            ((32) (memory-u32-set! addr value))))
         ((write-i/o)
          (port-write addr size value))))
      ((command addr size)
       (case command
         ((read-memory)
          (case size
            ((8) (memory-u8-ref addr))
            ((16) (memory-u16-ref addr))
            ((32) (memory-u32-ref addr))))
         ((read-i/o)
          (port-read addr size))))))

  ;; Run instructions until HLT. If the return value is 'stop then the
  ;; machine has gotten stuck (HLT and IF=0) or an interrupt handler
  ;; has instructed the machine to stop. If the reutrn value is
  ;; 'reboot then the machine should be rebooted.
  (define (machine-run)
    (define M *current-machine*)
    (define debug (machine-debug M))
    (define trace (machine-trace M))
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
               (DI (machine-DI M)))
      (when debug
        (print)
        (print "; AX=" (hex AX 4) "  BX=" (hex BX 4)
               "  CX=" (hex CX 4) "  DX=" (hex DX 4)
               "  SP=" (hex SP 4) "  BP=" (hex BP 4)
               "  SI=" (hex SI 4) "  DI=" (hex DI 4))
        (print* "; DS=" (hex (segment-selector ds) 4)
                "  ES=" (hex (segment-selector es) 4)
                "  FS=" (hex (segment-selector fs) 4)
                "  GS=" (hex (segment-selector gs) 4)
                "  SS=" (hex (segment-selector ss) 4)
                "  CS=" (hex (segment-selector cs) 4)
                "  IP=" (hex ip 4)
                "  ")
        (print-flags fl)
        (newline (current-error-port))
        (print* "SS:SP: ")
        (print-memory (fx+ ss SP) 16)
        ;; (print* "@SS:BP: ")
        ;; (display-memory (fx+ ss BP) 16)
        (print* "CS:IP: ")
        (print-memory (fx+ cs ip) 16))

      (let ((trans (translate cs ip debug (if trace 1 32))))
        (let-values (((ip^ fl^ AX^ CX^ DX^ BX^ SP^ BP^ SI^ DI^
                           cs^ ds^ ss^ es^ fs^ gs^)
                      (trans bus fl AX CX DX BX SP BP SI DI
                             cs ds ss es fs gs)))
          (cond (ip^
                 (loop fl^ ip^ cs^ ds^ ss^ es^ fs^ gs^
                       AX^ CX^ DX^ BX^ SP^ BP^ SI^ DI^))
                (else                   ;hlt was executed
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
                 (machine-IP-set! M ip)
                 (machine-FLAGS-set! M fl^)
                 ;; Default real-mode interrupt handlers.
                 (cond ((eqv? (fxand fl^ flag-IF) 0)
                        ;; TODO: It would be better to not hardcode this.
                        (cond ((and (eqv? cs^ #xF0000) (fx<=? ip #xFF))
                               (let* ((int-vector ip)
                                      (result (call-interrupt-handler int-vector)))
                                 (when debug
                                   (print "INT #x" (hex int-vector) " AX=#x" (hex AX^)))
                                 (cond ((memq result '(stop reboot))
                                        result)
                                       (else
                                        (machine-IP-set! M #x100)
                                        (machine-run)))))
                              (else
                               (print "Warning: the HLT instruction was executed with IF clear, exiting.")
                               'stop)))
                       (else 'hlt)))))))))
