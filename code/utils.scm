;;; utils.scm

;; Generate random numbers using Sedgewick, 2nd ed., p. 513

(define *name* "Richard M. Loveland")

(define *random-seed* 2718281828)
(define *random-constant* 31415821)

(define (random-integer n)
  (let ((answer #f))
    (begin
      (set! *random-seed*
        (modulo (+ (* *random-seed* *random-constant*) 1) n))
      (set! answer *random-seed*)
      answer)))

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

;; Generating lists with random elements

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

;; '(make-list* 12 random-integer 4096)
;; ;Value 12: (2390 285 1644 3934 1572 890 2020 1443 2197 2533 3955 20)

;; '(make-list* 12 (lambda (x) x) 4096)
;; ;Value 13: (4096 4096 4096 4096 4096 4096 4096 4096 4096 4096 4096 4096)

;; '(make-list* 12 (lambda (x) x) "bar")
;; ;Value 14: ("bar" "bar" "bar" "bar" "bar" "bar" "bar" "bar" "bar" "bar" "bar" "bar")

;; '(make-list* 12 (lambda (x) (string-upcase x)) "bar")
;; ;Value 15: ("BAR" "BAR" "BAR" "BAR" "BAR" "BAR" "BAR" "BAR" "BAR" "BAR" "BAR" "BAR")

;; Testing for atoms.

(define (atom? x)
  (not (or (vector? x) (pair? x) (null? x))))

;; Read in files of S-expressions

(define (read-file f)             ; Only for S-exps due to READ.
  (with-input-from-file f               ; Similar to RUN/SEXPS.
    (lambda () (read))))

;; WITH-INSTRUMENTATION

(define (with-tracing var proc)
  '())

;; eof
