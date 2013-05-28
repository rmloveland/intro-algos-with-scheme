(define (rml/merge pred l r)
  (letrec ((merge-aux
	    (lambda (pred left right result)
	      (cond ((and (null? left)
			 (null? right))
		     (reverse result))
		    ((and (not (null? left))
			  (not (null? right)))
		     (if (pred (car left)
			    (car right))
			 (merge-aux pred
                                    (cdr left)
				    right
				    (cons (car left) result))
			 (merge-aux pred
                                    left
				    (cdr right)
				    (cons (car right) result))))
		    ((not (null? left))
		     (merge-aux pred (cdr left) right (cons (car left) result)))
		    ((not (null? right))
		     (merge-aux pred left (cdr right) (cons (car right) result)))
		    (else #f)))))
    (merge-aux pred l r '())))

(define (rml/merge-sort xs pred)
  (let loop ((xs xs))
	      ; If xs is empty or has 1 element, consider it sorted
	      ; and return it
	      (if (<= (length xs) 1)
		  xs
		  ; Otherwise, split xs into left and right sublists and call yourself recursively
		  (let* ((middle (quotient (length xs) 2))
			 (lower (take xs middle))
			 (upper (drop xs middle))
			 (left (loop lower))
			 (right (loop upper)))
		    (rml/merge pred left right)))))
