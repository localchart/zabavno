(library (zabavno firmware compat)
  (export input-port-ready?)
  (import (rnrs (6)))

  (define input-port-ready?
    (let ((warned? #f))
      (unless warned?
        (display "No (zabavno firmware compat) library has been provided, the keyboard will not work\n"
                 (current-error-port))
        (set! warned? #t))
      (lambda (port)
        #f))))
