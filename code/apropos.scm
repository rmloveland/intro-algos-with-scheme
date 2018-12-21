;;; APROPOS: List symbols matching a pattern.

(define (get-oblist-procedure)
  ;; -> Proc
  ;; Larceny, Chez: OBLIST
  ;; XXX: Add code for others here? COND-EXPAND?
  oblist)

;; We need to define an OBLIST procedure for all Schemes that don't
;; provide it: Kawa, JScheme, etc.  Unfortunately it's not clear how
;; to accomplish this portably (I haven't been able to get COND-EXPAND
;; to work).
'(cond-expand (oblist)
              (kawa
               (begin
                 (define (oblist)
                   (environment-fold (interaction-environment) cons '())))))

(define (apropos regexp)
  ;; Regexp -> List
  (let* ((list-bindings (get-oblist-procedure))
         (symbols (list-bindings))
         (results '()))
    (for-each (lambda (sym)
                (let ((s (symbol->string sym)))
                  (if (and (pregexp-match regexp s)
                           (not (pregexp-match "%--gensym" s)))
                      (push! sym results))))
              symbols)
    results))

;; eof
