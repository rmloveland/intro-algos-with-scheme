;;; LET-OPTIONALS: Support for optional arguments.

(define (check-arg pred val caller)
  (if (not (pred val)) (error val caller)))

(define-syntax :optional
  (syntax-rules ()
    ((:optional rest default-exp)
     (let ((maybe-arg rest))
       (cond ((null? maybe-arg) default-exp)
	     ((null? (cdr maybe-arg)) (car maybe-arg))
	     (else (error "too many optional arguments" maybe-arg)))))))

(define-syntax let-optionals*
  (syntax-rules ()
    ((let-optionals* args vars&defaults body1 ...)
     (let ((rest args))
       (really-let-optionals* rest vars&defaults body1 ...)))))

(define-syntax really-let-optionals*
  (syntax-rules ()
    ;; Standard case. Do the first var/default and recurse.
    ((really-let-optionals* args ((var1 default1) etc ...)
       body1 ...)
     (call-with-values (lambda () (if (null? args)
				      (values default1 '())
				      (values (car args) (cdr args))))
		       (lambda (var1 rest)
			 (really-let-optionals* rest (etc ...)
			   body1 ...))))

    ;; Single rest arg -- bind to the remaining rest values.
    ((really-let-optionals* args (rest) body1 ...)
     (let ((rest args)) body1 ...))

    ;; No more vars. Make sure there are no unaccounted-for values, and
    ;; do the body.
    ((really-let-optionals* args () body1 ...)
     (if (null? args) (begin body1 ...)
	 (error "Too many optional arguments." args)))))

;; eof
