;; strings.scm: helpers for string processing            -*- scheme -*-

(define string-split pregexp-split)
(define string-tokenize pregexp-split)

(define (string-trim s . which)
  ;; 'LEFT 'RIGHT, default to both
  (let ((rx-left "^[ \t]+")
        (rx-right "[ \t]+$"))
    (cond ((null? which)
           (let* ((s1 (string-trim-left s))
                  (s2 (string-trim-right s1)))
             s2))
          ((equal? which '(LEFT)) (string-trim-left s))
          ((equal? which '(RIGHT)) (string-trim-right s))
          (else (error "STRING-TRIM - bad arguments: " s which)))))

(define (string-trim-right s)
  (pregexp-replace "[ \t]+$" s ""))

(define (string-trim-left s)
  (pregexp-replace "^[ \t]+" s ""))

(define (mapconcat f xs sep)
  #f)

;; eof
