;;; APROPOS: List symbols matching a pattern.

(define (get-oblist-procedure)
  ;; -> Proc
  ;; Larceny, Chez: OBLIST
  ;; XXX: Add code for others here? COND-EXPAND?
  oblist)

;; Kawa
(cond-expand (oblist)
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
