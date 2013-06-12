;;; A binary search implementation in MIT/GNU Scheme.

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

(define (re-matches? rx line #!optional display-matches)
  ;; Rexp String . Boolean -> Boolean
  "Attempt to match RX against LINE. Print the match if DISPLAY-MATCHES is set."
  (let ((match (re-string-match rx line)))
    (if match
	(if (not (default-object? display-matches))
	    (begin (format #t "|~A|~%" (re-match-extract line match 0))
		   #t)
	    #t)
	#f)))

(define (make-list-of-words-matching re file)
  ;; Rexp String -> List
  "Given a regular expression RE, loop over FILE, gathering matches."
  (call-with-input-file file
    (lambda (port)
      (let loop ((source (read-line port)) (sink '()))
        (if (eof-object? source)
            sink
            (loop (read-line port) (if (re-matches? re source)
                             (cons source sink)
                             sink)))))))

(define (->int n)
  ;; Number -> Int
  "Given a number N, return its integer representation.
N can be an integer or flonum (yes, it's quick and dirty)."
  (flo:floor->exact (exact->inexact n)))

(define (split-difference low high)
  ;; Int Int -> Int
  "Given two numbers, return their rough average."
  (if (= (- high low) 1)
      1
      (->int (/ (- high low) 2))))

(define (binary-search word xs #!optional debug-print)
  ;; String List -> Int
  "Do binary search of list XS for WORD. Return the index found, or #f."
  (if (null? xs)
      #f
      (let loop ((low 0) (high (- (length xs) 1)))
	(let* ((try (+ low (split-difference low high)))
	       (word-at-try (list-ref xs try)))
	  (cond
           ((string=? word-at-try word) try)
	   ((< (- high low) 1) #f)
	   ((= (- high try) 1) 
            (if (string=? (list-ref xs low) word)
                low
                #f))
	   ((string<? word-at-try word)
	    (if (not (default-object? debug-print))
		(begin (format #f "(string<? ~A ~A) -> #t~%try: ~A high: ~A low: ~A ~2%"
			       word-at-try word try high low)
		       (loop (+ 1 try) high)) ; raise the bottom of the window
		(loop (+ 1 try) high)))
	   ((string>? word-at-try word)
	    (if (not (default-object? debug-print))
		(begin (format #f "(string>? ~A ~A) -> #t~%try: ~A high: ~A low: ~A ~2%"
			       word-at-try word try high low)
		       (loop low (+ 1 try))) ; lower the top of the window
		(loop low (+ 1 try))))
	   (else #f))))))

(define (run-binary-search-tests)
  ;; -> IO
  "Run our binary search tests using known words from the 'words' file.
This file should be in the current working directory."
  (with-working-directory-pathname (pwd)
    (lambda ()
      (if (file-exists? "words")
          (begin
            (format #t "file 'words' exists, making a list...~%")
            (let* ((unsorted (make-list-of-words-matching "acc" "words"))
                   (sorted (sort unsorted string<?)))
              (format #t "doing binary searches...~%")
              (assert equal? #f (binary-search "test" '())) ; empty list
              (assert equal? #f (binary-search "aardvark" sorted)) ; element absent and too small
              (assert equal? #f (binary-search "zebra" sorted)) ; element absent and too large
              (assert = 0 (binary-search "accusive" '("accusive"))) ; list of length one
              (assert = 0 (binary-search "acca" sorted)) ; first element of list
              (assert = 1 (binary-search "aardvark" '("aardvark" "aardvark" "babylon"))) ; multiple copies of word in list
              (assert = 1 (binary-search "barbaric" '("accusive" "barbaric"))) ; list of length two
              (assert = 98 (binary-search "acclamator" sorted))
	      (assert = 127 (binary-search "aardvark" (map (lambda (x) "aardvark") sorted))) ; list is all one value
              (assert = 143 (binary-search "accomplice" sorted))
              (assert = 254 (binary-search "accustomedly" sorted))
              (assert = 255 (binary-search "accustomedness" sorted)))) ; last element of list
          (error "File `words' not found in this directory")))))
