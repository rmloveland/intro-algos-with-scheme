;;; test-lib.scm --- A simple testing library.

(define (assert pred expected got msg)
  ;; Proc Value Value String . Value -> IO!
  (format "Testing condition '~a'~%" msg)
  (if (pred expected got)
      (format "~a is ~a ... ~a~%~%" expected got #t)
      (format "~a is not ~a ... ~a~%" expected got #f)))

;; ++ Should this library also have a summarizing procedure that
;; prints the results of the entire test run?

;;; eof
