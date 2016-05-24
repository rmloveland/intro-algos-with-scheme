(define-syntax dotimes
  (syntax-rules ()
    ((dotimes count body ...)
     (let loop ((counter count))
       (if (> counter 0)
	   (begin
	     body ...
	     (loop (- counter 1))))))))

(define (log10 x)
  (let ((result 
         (/ (log x)
            (log 10))))
    result))

(define (log2 x)
  (let ((result 
         (/ (log x)
            (log 2))))
    result))

(define (puts message)
  (begin (display message)
         (newline)))
