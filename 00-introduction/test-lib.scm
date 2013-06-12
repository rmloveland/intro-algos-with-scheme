;;; test-lib.scm --- A simple testing library.

(define (assert pred expected got #!optional noise)
  ;; Proc Object Object Any -> IO
  (if (pred expected got)
      (format #t "~A is ~12@A\t... ok~%" expected got)
      (format #t "~A is not ~8@A\t... FAIL~%" expected got)))
