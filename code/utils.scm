;;; -*- Mode: Scheme -*-

;; utils.scm

;; This file contains various helpful things that do not yet have a
;; better home.  For the details of what is exported, see the file
;; `utils.mod`.



;; Generate random numbers using Sedgewick, 2nd ed., p. 513

(define *random-seed* 2718281828)
(define *random-constant* 31415821)

(define (random-integer n)
  (let ((answer #f))
    (begin
      (set! *random-seed*
        (modulo (+ (* *random-seed* *random-constant*) 1) n))
      (set! answer *random-seed*)
      answer)))

;; Generate random strings.

(define (random-char)
  ;; A-Z :: 65-90
  ;; a-z :: 97-122
  (let ((i (modulo (random-integer 10000000) 122)))
    (integer->char i)))

(define (random-string k)
  (let loop ((k k)
             (xs '()))
    (if (= k 0)
        (list->string xs)
        (loop (- k 1) (cons (random-char) xs)))))



;; Generate lists of various types.

(define (make-list* k fn seed)
  (let* ((xs (make-list k seed))
         (ys (map (lambda (elem) (fn elem)) xs)))
    ys))

(define (make-list k seed)
  (let loop ((xs '())
             (k k))
    (if (= k 0)
        xs
        (loop (cons seed xs) (- k 1)))))

(define (seq start end)
  (let loop ((start start)
             (end end)
             (xs '()))
    (if (= start (+ end 1))
        (reverse xs)
        (loop (+ start 1)
              end
              (cons start xs)))))



;; Swap elements of a vector.

(define (vector-swap! V i j)
  (let ((i* (vector-ref V i))
        (j* (vector-ref V j)))
    (begin
      (vector-set! V i j*)
      (vector-set! V j i*))))



;; Not every implementation loads SRFI-1 by default, so these just
;; make life a little easier in foreign lands.

(define (first xs) (car xs))
(define (second xs) (cadr xs))
(define (third xs) (caddr xs))
(define (rest xs) (cdr xs))



;; Various operations on lists.

(define (filter f xs)
  (let ((res '()))
    (for-each (lambda (x) (if (f x) (set! res (cons x res)))) xs)
    (reverse res)))

(define (flatten xs)
  (cond ((null? xs) '())
        ((pair? xs)
         (append (flatten (car xs))
                 (flatten (cdr xs))))
        (else (list xs))))

(define (take xs i)
  (let loop ((xs xs) (ys '()) (i i))
    (cond ((null? xs) (reverse ys))
          ((zero? i) (reverse ys))
          (else (loop (cdr xs) (cons (car xs) ys) (- i 1))))))

(define (drop xs i)
  (let loop ((xs xs) (ys '()) (i i))
    (if (null? xs)
        (reverse ys)
        (loop (cdr xs)
              (if (<= i 0)
                  (cons (car xs) ys)
                  ys)
              (- i 1)))))



;; Handy predicates.

(define (atom? x)
  (not (or (vector? x) (pair? x) (null? x))))

(define (alist? x)
  (and (map (lambda (elem) (list? elem)) x)
       (list? x)))

;; eof
