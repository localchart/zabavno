#!r6rs
(import (rnrs)
        (rnrs eval)
        (only (vicare) debug-print)
        (only (vicare compiler) assembler-output))
(assembler-output #t)
(define f
  (eval '(lambda (x)
           (let* ((t0 x)
                  (t1 18)
                  (tmp (fx- t0 t1))
                  (fl-OF (lambda ()
                           (fxand (fxxor t0 t1)
                                  (fxxor t0 tmp)))))
             (fl-OF)))
        (environment '(rnrs))))
(debug-print
 (f 0))
