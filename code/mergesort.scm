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
                     (if (pred (car left) (car right))
                         (merge-aux pred (cdr left)
                                    right (cons (car left) result))
                         (merge-aux pred left
                                    (cdr right) (cons (car right) result))))
                    ((not (null? left))
                     (merge-aux pred (cdr left) right (cons (car left) result)))
                    ((not (null? right))
                     (merge-aux pred left (cdr right) (cons (car right) result)))
                    (else #f)))))
    (merge-aux pred l r '())))

;; Note tree recursion in this procedure

(define (rml/merge-sort1 xs pred . debug-print)
(define (take xs i)
  (let loop ((xs xs) (ys '()) (i i))
    (cond ((null? xs) (reverse ys))
          ((zero? i) (reverse ys))
          (else (loop (cdr xs) (cons (car xs) ys) (- i 1))))))

;; "drop" the first I elements of XS

(define (drop xs i)
  (let loop ((xs xs) (ys '()) (i i))
    (if (null? xs)
        (reverse ys)
        (loop (cdr xs)
              (if (<= i 0)
                  (cons (car xs) ys)
                  ys)
              (- i 1)))))

(define (first xs) (car xs))
(define (second xs) (cadr xs))
(define (rest xs) (cdr xs))
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
                (format #t "merging ~A and ~A~%" left right))
            (rml/merge pred left right))))))

;; The following procedure is just the above two procedures merged
;; into one.

(define (rml/merge-sort2 xs pred)
  (let loop ((xs xs))
    ;; If xs is empty or has 1 element, consider it sorted
    ;; and return it
    (if (<= (length xs) 1)
        xs
        ;; Otherwise, split xs into left and right sublists and call yourself recursively
        (let ((middle (quotient (length xs) 2)))
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
                  (else #f)))))))



;;; Bottom-up merge sort

;; First, do just what the procedure's name implies: explode the list
;; into lots of little two-element lists. Note that the list you pass
;; to this procedure must be already flattened.

(define (explode xs)
  (let loop ((xs xs) (ys '()))
    (if (null? xs)
        ys
        (loop (cdr xs)
              (cons (list (car xs)) ys)))))

(define (explode2 xs)
  (let loop ((xs ys) (ys '()))
    (cond ((null? xs) ys)
          ((null? (cdr xs))
           (loop '() (cons (list (car xs)) ys)))
          (else
           (loop (cddr xs)
                 (cons (list (first xs) (second xs)) ys))))))

;; Now the bottom up merge sort!
;; The steps are:
;; 1. Pass through the list, merging sublists of size n.
;; 2. Repeat for subarrays of size 2n.
;;
;; Example:
;;
;; ((5) (1) (2) (7) (8) (4) (3) (6))
;; ((1 5) (2) (7) (8) (4) (3) (6))
;; ((1 5) (2 7) (8) (4) (3) (6))
;; ((1 5) (2 7) (4 8) (3) (6))
;; ((1 5) (2 7) (4 8) (3 6))
;; ((1 2 5 7) (4 8) (3 6))
;; ((1 2 5 7) (3 4 6 8))
;; (1 2 3 4 5 6 7 8)

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



;;; Bottom-up, *iterative* merge sort.  This version has been
;;; rewritten to avoid the (unnecessary) overhead of EXPLODE.

(define (merge pred l r)
  (letrec ((merge-aux
            (lambda (pred left right result)
              (cond 
               ;; If LEFT and RIGHT are both numbers, listify them so
               ;; MERGE-AUX can work with them.
               ((and (atom? left)
                     (atom? right))
                (merge-aux pred (list left) (list right) result))

               ;; If LEFT is just a number, listify it so MERGE-AUX
               ;; can work with it.
               ((atom? left)
                (merge-aux pred (list left) right result))
	       
               ;; Likewise, if RIGHT is just a number, listify it for
               ;; MERGE-AUX.
               ((atom? right)
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



;;; Insertion sort: a nice example of ease of implementation
;;; vs. performance.

(define (rml/insertion-sort1 xs pred)
  (let loop ((xs xs)
             (result '()))
    (if (null? xs)
        result
        (loop (cdr xs)
              (rml/merge pred
                         (list (first xs))
                         result)))))

;; Super slow version that uses lists and FOLD-RIGHT.

(define (rml/insertion-sort2 xs pred)
  (let ((exploded (explode xs)))
    (fold-right (lambda (x y) (rml/merge pred x y)) '() exploded)))
