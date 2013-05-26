(define (rml/merge l r)
  (letrec ((merge-aux
	    (lambda (left right result)
	      (cond ((and (null? left)
			 (null? right))
		     (reverse result))
		    ((and (not (null? left))
			  (not (null? right)))
		     (if (< (car left)
			    (car right))
			 (merge-aux (cdr left)
				    right
				    (cons (car left) result))
			 (merge-aux left
				    (cdr right)
				    (cons (car right) result))))
		    ((not (null? left))
		     (merge-aux (cdr left) right (cons (car left) result)))
		    ((not (null? right))
		     (merge-aux left (cdr right) (cons (car right) result)))
		    (else #f)))))
    (merge-aux l r '())))

(define (rml/merge-sort xs)
  (letrec ((merge-sort-aux 
	    (lambda (xs)
	      ; If xs is empty or has 1 element, consider it sorted
	      ; and return it
	      (if (or (null? xs)
		      (= (length xs) 1))
		  xs
		  ; Otherwise, split xs into left and right sublists and call yourself recursively
		  (let* ((middle (quotient (length xs) 2))
			 (lower (take xs middle))
			 (upper (drop xs middle))
			 (left (merge-sort-aux lower))
			 (right (merge-sort-aux upper)))
		    (rml/merge left right))))))
    (merge-sort-aux xs)))
