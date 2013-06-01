(load-option 'format)

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

(define (rml/merge-sort xs pred #!optional debug-print)
  (let loop ((xs xs))
	      ; If xs is empty or has 1 element, consider it sorted
	      ; and return it
	      (if (<= (length xs) 1)
		  xs
		  ; Otherwise, split xs into left and right sublists and call yourself recursively
		  (let* ((middle (quotient (length xs) 2))
			 (left (loop (take xs middle)))
			 (right (loop (drop xs middle))))
		    (if (not (default-object? debug-print))
			(format #t "merging:~%~A~%~A~%" left right))
		    (rml/merge pred left right)))))

;; Ported from the MIT Scheme `merge-sort!' implementation, which uses
;; vectors and is way fast. This uses lists and is way slow, slower
;; even than the tree-recursive implementation above (at least until
;; the stack blows up due to the tree recursion).
(define (rml/merge-sort! xs pred)
  (let sort-sublist
      ((xs xs)
       (temp (list-copy xs))
       (low 0)
       (high (length xs)))
    (if (> (- high low) 1)
	(let ((middle (quotient (+ low high) 2)))
	  (sort-sublist temp xs low middle)
	  (sort-sublist temp xs middle high)
	  (let merge ((p low) (p1 low) (p2 middle))
	    (if (< p high)
		(if (and (< p1 middle)
			 (or (= p2 high)
			     (not (pred (list-ref temp p2)
					(list-ref temp p1)))))
		    (begin
		      (list-set! xs p (list-ref temp p1))
		      (merge (+ p 1) (+ p1 1) p2))
		    (begin 
		      (list-set! xs p (list-ref temp p2))
		      (merge (+ p 1) p1 (+ p2 1)))))))))
  xs)

;;; Bottom-up merge sort

;; First, do just what the procedure's name implies: explode the list
;; into lots of little one-element lists.
(define (explode xs)
  (let loop
      ((xs xs)
       (ys '()))
    (if (null? xs)
	ys
	(loop (cdr xs)
	      (cons (list (car xs))
		    ys)))))
