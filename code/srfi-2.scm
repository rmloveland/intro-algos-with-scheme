;;; SRFI-2: AND-LET*, an AND with local bindings, a guarded LET* form.

;; Copyright (c) 1993-2001 by Richard Kelsey and Jonathan Rees. See file COPYING.

;; The reference implementation is written in some weird Scheme variant.
;; This is an attempt to produce the same result using SYNTAX-RULES.

;; I found the both the specification and the implementation unhelpful.
;; For example, one would think that (AND-LET* ()) -> #T by analogy with
;; (AND) -> #T.  The specification doesn't say.
;;
;; The following behaves correctly on the test cases at the end of the
;; reference implementation,  except that it doesn't catch the three syntax
;; errors.  There is no way for SYNTAX-RULES to distinguish between a
;; constant and a variable, and no easy way to check if a variable is
;; being used twice in the same AND-LET* (and why is that an error? LET*
;; allows it).

(define-syntax and-let*
  (syntax-rules ()

    ;; No body - behave like AND.
    ((and-let* ())
     #t)
    ((and-let* ((var exp)))
     exp)
    ((and-let* ((exp)))
     exp)
    ((and-let* (var))
     var)

    ;; Have body - behave like LET* but check for #F values.

    ;; No clauses so just use the body.
    ((and-let* () . body)
     (begin . body))

    ;; (VAR VAL) clause - bind the variable and check for #F.
    ((and-let* ((var val) more ...) . body)
     (let ((var val))
       (if var
           (and-let* (more ...) . body)
           #f)))

    ;; Error check to catch illegal (A B ...) clauses.
    ((and-let* ((exp junk . more-junk) more ...) . body)
     (error "syntax error"
            '(and-let* ((exp junk . more-junk) more ...) . body)))

    ;; (EXP) and VAR - just check the value for #F.
    ;; There is no way for us to check that VAR is an identifier and not a
    ;; constant
    ((and-let* ((exp) more ...) . body)
     (if exp
         (and-let* (more ...) . body)
         #f))
    ((and-let* (var more ...) . body)
     (if var
         (and-let* (more ...) . body)
         #f))))

;; eof
