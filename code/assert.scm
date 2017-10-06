;;; ASSERT: Testing facilities.

(define (assert pred expected got msg)
  ;; Proc Value Value String . Value -> IO!
  (format "Testing condition \"~a\"~%" msg)
  (if (pred expected got)
      (format "  ~a~%  IS ~a TO ~%  ~a~%  ~a~%~%" expected pred got #t)
      (format "  ~a~%  IS NOT ~a TO ~%  ~a~%  ~a~%" expected pred got #f)))

;; eof
