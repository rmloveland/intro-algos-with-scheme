;;;; binary-search.scm

(define %random (make-random 314159))	; ,open random

(define (random n)
  ;; Int -> Int
  (modulo (%random) n))

(define (make-list-of-random-numbers list-length max)
  ;; Int Int -> List
  "Make a list of random integers less than MAX that's LIST-LENGTH long."
  (letrec ((maker
            (lambda (list-length max result)
              (let loop ((n list-length) (result '()))
                (if (= n 0)
                    result
                    (loop (- n 1) (cons (random max) result)))))))
    (maker list-length max '())))

(define (number->integer n)
  ;; Num -> Int
  "Given a number N, return its integer representation.
N can be an integer or flonum (yes, it's quick and dirty)."
  (cond ((floatnum? n)			; ,open floatnums
	 (floor (inexact->exact n)))
	((integer? n) n)
	((exact? n)
	 (floor (exact->inexact n)))
	(else #f)))			; BOOM!!!

(define (split-difference low high)
  ;; Int Int -> Int
  "Given two numbers, return their rough average."
  (if (= (- high low) 1)
      1
      (number->integer (/ (- high low) 2))))

(define (binary-search word xs debug-print?)
  ;; Str List -> Int
  "Do binary search of list XS for WORD. Return the index found, or #f."
  (if (null? xs)
      #f
      (let loop ((low 0)
		 (high (- (length xs) 1)))
	(let* ((try (+ low (split-difference low high)))
	       (word-at-try (list-ref xs try)))
	  (cond
           ((string-ci=? word-at-try word)
	    try)
	   ((< (- high low) 1)
	    #f)
	   ((= (- high try) 1) 
            (if (string-ci=? (list-ref xs low) word)
                low
                #f))
	   ((string-ci<? word-at-try word)
	    (if debug-print?
		(begin (format #f "(string-ci<? ~A ~A) -> #t~%try: ~A high: ~A low: ~A ~%" ; ,open formats
			       word-at-try word try high low)
		       (loop (+ 1 try) high)) ; raise the bottom of the window
		(loop (+ 1 try) high)))
	   ((string-ci>? word-at-try word)
	    (if debug-print?
		(begin (format #f "(string-ci>? ~A ~A) -> #t~%try: ~A high: ~A low: ~A ~%"
			       word-at-try word try high low)
		       (loop low (+ 1 try))) ; lower the top of the window
		(loop low (+ 1 try))))
	   (else 'FAIL))))))

(define (words-file)
  ;; -> Str
  "/home/rml/Code/intro-algos-with-scheme/01-searching/words.scm")

(define *words* (load (words-file)))	; IO!

(define (run-binary-search-tests)
  ;; -> IO!
  "Run our binary search tests using known words from the 'words' file."
  (begin
    (let* ((unsorted (load (words-file)))
	   (sorted (sort-list unsorted string-ci<?))) ; ,open sort
      (format #t "Running binary search tests...~%")
      (assert equal? #f (binary-search "test" '() #f) "element absent: list is empty")
      (assert equal? #f (binary-search "aardvark" sorted #f) "element absent: too small")
      (assert equal? #f (binary-search "zebra" sorted #f) "element absent: too large")
      (assert = 0 (binary-search "accusive" '("accusive") #f) "element present: list of length one")
      (assert = 0 (binary-search "acca" sorted #f) "element present: first item in list")
      (assert = 255 (binary-search "accustomedness" sorted #f) "element present: last item in list")
      (assert = 1 (binary-search "aardvark" '("aardvark" "aardvark" "babylon") #f) "element present: multiple copies of word in list")
      (assert = 1 (binary-search "barbaric" '("accusive" "barbaric") #f) "element present: list of length two")
      (assert = 98
	      (binary-search "acclamator" sorted #f) "element present: general")
      (assert = 1
	      (binary-search "aardvark" '("aardvark" "aardvark" "aardvark" "aardvark") #f) "element present: list is all one value")
      (assert = 143 (binary-search "accomplice" sorted #f) "element present: general")
      (assert = 254 (binary-search "accustomedly" sorted #f) "element present: general"))))

;;; eof
