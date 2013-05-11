(define-syntax push!
  (syntax-rules ()
    ((push! item seq)
     (begin (set! seq (cons item seq))
            seq))))

(define-syntax pop!
  (syntax-rules ()
    ((pop! seq)
     (let ((result (car seq)))
       (begin (set! seq (cdr seq))
              result)))))

(define-syntax incf!
  (syntax-rules ()
    ((incf! var)
     (begin (set! var (+ 1 var))
            var))))

(define-syntax decf!
  (syntax-rules ()
    ((decf! var)
     (begin (set! var (- var 1))
            var))))
