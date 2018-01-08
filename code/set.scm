;;; -*- Mode: Scheme; -*-

(define (set? x)
  (hash-table? x))

(define (make-set elements)
  (let ((s (make-hash-table)))
    (for-each (lambda (elem)
                (hash-table-set! s elem 1))
              elements)
    s))

(define (set-elements set)
  (let ((rv '()))
    (hash-table-walk set (lambda (k v) (push! k rv)))
    rv))

(define (set-add! item set)
  (hash-table-set! set item 1))

(define (set-remove! item set)
  (hash-table-delete! set item))

(define (set-member? item set)
  (let* ((elems (set-elements set))
         (val (member item elems)))
    (if val #t #f)))

;; eof
