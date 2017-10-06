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
