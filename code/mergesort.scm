;;; Sorting procedures.

;; Note that we will be using a number of procedures defined in SRFI-1
;; (http://srfi.schemers.org/srfi-1/srfi-1.html). These procedures are
;; already available in MIT/GNU Scheme's (user) environment. In other
;; words, you don't have to do anything, they're already baked in.

;;; Recursive mergesort

;; First, we need a `merge' procedure.

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

;; Recursive merge sort (note tree recursion)

(define (rml/merge-sort1 xs pred . debug-print)
  (let loop ((xs xs))
    ;; If xs is empty or has 1 element, consider it sorted and return
    ;; it
    (if (<= (length xs) 1)
        xs
        ;; Otherwise, split xs into left and right sublists and call
        ;; yourself recursively
        (let* ((middle (quotient (length xs) 2))
               (left (loop (take xs middle)))
               (right (loop (drop xs middle))))
          (begin
            (if debug-print
                (format #t "merging:~%~A~%~A~%" left right))
            (rml/merge pred left right))))))

;; The above two procedures merged into one

(define (rml/merge-sort2 xs pred . debug-print)
  (let loop ((xs xs))
    ;; If xs is empty or has 1 element, consider it sorted
    ;; and return it
    (if (<= (length xs) 1)
        xs
        ;; Otherwise, split xs into left and right sublists and call yourself recursively
        (let ((middle (quotient (length xs) 2)))
          (begin
            (if debug-print
                (format #t "merging:~%~A~%~A~%" left right))
            (let merge
                ((pred pred)
                 (left (loop (take xs middle)))
                 (right (loop (drop xs middle))) 
                 (result '()))
              (cond ((and (null? left)
                          (null? right))
                     (reverse result))
                    ((and (not (null? left))
                          (not (null? right)))
                     (if (pred (car left)
                               (car right))
                         (merge pred
                                (cdr left)
                                right
                                (cons (car left) result))
                         (merge pred
                                left
                                (cdr right)
                                (cons (car right) result))))
                    ((not (null? left))
                     (merge pred (cdr left) right (cons (car left) result)))
                    ((not (null? right))
                     (merge pred left (cdr right) (cons (car right) result)))
                    (else #f))))))))

;; Ported from the MIT Scheme `merge-sort!' implementation, which uses
;; vectors and is way fast. This uses lists and is way slow, slower
;; even than the tree-recursive implementation above.

'(define (rml/merge-sort! xs pred)
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
;; into lots of little two-element lists. Note that the list you pass
;; to this procedure must be already flattened.

'(define (explode xs)
  (let loop
      ((xs xs)
       (ys '()))
    (if (null? xs)
        ys
        (loop (cdr xs)
              (cons (list (car xs))
                    ys)))))

'(define (explode2 xs)
  (let loop ((xs xs)
             (ys '()))
    (cond ((null? xs)
           ys)
          ((null? (cdr xs))
           (loop '()
                 (cons (list (car xs)) ys)))
          (else
           (loop (cddr xs) ; 3..
                 (cons (list (first xs)
                             (second xs))
                       ys))))))

;; Now the bottom up merge sort!
;; The steps are:
;; 1. Pass through the list, merging sublists of size n.
;; 2. Repeat for subarrays of size 2n.
;;
;; Example:
;((5) (1) (2) (7) (8) (4) (3) (6))
;((1 5) (2) (7) (8) (4) (3) (6))
;((1 5) (2 7) (8) (4) (3) (6))
;((1 5) (2 7) (4 8) (3) (6))
;((1 5) (2 7) (4 8) (3 6))
;((1 2 5 7) (4 8) (3 6))
;((1 2 5 7) (3 4 6 8))
;(1 2 3 4 5 6 7 8)

(define (rml/merge-sort3 xs pred)
  (let ((exploded (explode xs)))
    (let loop ((exploded exploded)
               (result '()))
      (cond ((and (null? exploded)
                  (= 1 (length result)))
             (car result))
            ((null? exploded)
             (loop result
                   exploded))
            ((null? (cdr exploded))
             (loop (cdr exploded)
                   (cons (car exploded) result)))
            (else
             (loop (cddr exploded)
                   (cons (rml/merge <
                                    (first exploded)
                                    (second exploded))
                         result)))))))

;;; Bottom-up, iterative merge sort, rewritten to avoid the O(n)
;;; overhead of `explode'

(define (rml/merge2 pred l r)
  (letrec ((merge-aux
	    (lambda (pred left right result)
	      (cond 
	       ;; If LEFT and RIGHT are both numbers, listify them so
	       ;; MERGE-AUX can work with them.
	       ((and (number? left)
		     (number? right))
		(merge-aux pred (list left) (list right) result))

	       ;; If LEFT is just a number, listify it so MERGE-AUX
	       ;; can work with it.
	       ((number? left)
		(merge-aux pred (list left) right result))
	       
	       ;; Likewise, if RIGHT is just a number, listify it for
	       ;; MERGE-AUX.
	       ((number? right)
		(merge-aux pred left (list right) result))

	       ;; If LEFT and RIGHT are empty, we're done merging.
	       ;; Return the result.
	       ((and (null? left)
		     (null? right))
		(reverse result))

	       ;; If LEFT and RIGHT still have elements to be
	       ;; processed, call PRED and run them through MERGE-AUX
	       ;; again.
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

	       ;; If the cases above haven't matched, and LEFT is not
	       ;; NULL?, I call myself.
	       ((not (null? left))
		(merge-aux pred (cdr left) right (cons (car left) result)))

	       ;; Same as the previous case -- this time with RIGHT.
	       ((not (null? right))
		(merge-aux pred left (cdr right) (cons (car right) result)))

	       ;; We should never get here.
	       (else #f)))))
    (merge-aux pred l r '())))

(define (rml/merge-sort4 xs pred)
  (let loop ((xs xs)
	     (result '()))
    (cond ((and (null? xs)
		(null? (cdr result)))
	   (car result))
	  ((null? xs)
	   (loop result
		 xs))
	  ((null? (cdr xs))
	   (loop (cdr xs)
		 (cons (car xs) result)))
	  (else
	   (loop (cddr xs)
		 (cons (rml/merge2 <
				   (first xs)
				   (second xs))
		       result))))))

;;; Insertion sort (example of ease of implementation vs. performance)

(define (rml/insertion-sort1 xs pred)
  (let loop ((xs xs)
             (result '()))
    (if (null? xs)
        result
        (loop (cdr xs)
              (rml/merge pred
                         (list (first xs))
                         result)))))

(define (rml/insertion-sort2 xs pred)
  (let ((exploded (explode xs)))
    (fold-right (lambda (x y) (rml/merge pred x y)) '() exploded)))

;;; Measuring performance

;; Setting up our lists to sort
;(define unsorted (make-list-of-random-numbers 2000 2000000))
;(define unsorted-huge (make-list-of-random-numbers 20000 2000000))
;(define unsorted-eight '(6 3 4 8 7 2 1 5))

;(with-timing-output (sort unsorted-huge <))
;Run time:	.05
;GC time:	.05
;Actual time:	.103

;(with-timing-output (rml/insertion-sort1 unsorted <))
;Run time:	13.
;GC time:	.33
;Actual time:	13.485

;(with-timing-output (rml/insertion-sort2 unsorted <))
;Run time:	12.95
;GC time:	.34
;Actual time:	13.444

;(with-timing-output (rml/merge-sort1 unsorted-huge <))
;Run time:	1.97
;GC time:	.05
;Actual time:	2.149

;(with-timing-output (rml/merge-sort2 unsorted-huge <))
;Run time:	2.24
;GC time:	.05
;Actual time:	2.35
