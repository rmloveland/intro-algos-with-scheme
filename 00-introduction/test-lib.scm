;;; test-lib.scm --- A simple testing library.

(define (assert pred expected got msg . noise)
  ;; Proc Value Value String . Value -> IO!
  (format #t "Testing condition '~A'~% " msg)
  (if (pred expected got)
      (format #t "~A is ~A... ok~%~%" expected got)
      (format #t "~A is not ~A... FAIL~%~%" expected got)))

;; ++ Should this library also have a summarizing procedure that
;; prints the results of the entire test run?

;;; eof
