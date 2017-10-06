;;; SRFI-8: RECEIVE: Binding to multiple values.

(define-syntax receive
  (syntax-rules ()
    ((receive formals expression body ...)
     (call-with-values (lambda () expression)
                       (lambda formals body ...)))))

;; eof
