;;; -*- Mode: Scheme; -*-

;; awkward.scm -- Do AWK-like things from Scheme (awkwardly)

;; AWK built-in variables:

;; - FS: Field separator, defaults to " "

;; - OFS: Output field separator, defaults to " "

;; - RS: Record separator, defaults to "\n"

;; - ORS: Output record separator, defaults to "\n"

;; - NF: Number of fields in the current input record.  Set for each
;;   record (usually a line).

;; - NR: Number of input records that have been processed during
;;   program execution.  Set each time a new record is read.

(define (process-file file)
  (awk file
      ((BEGIN (set! $FS "\t")
              (display "Hello World!")
              (display newline))

       ;; Print lines matching a regex
       ("/Dan/" (display $0))

       ;; Print names of people who didn't work last week
       ((= $3 0) (display $1)))))

;; AWK* takes a filename and an alist of (PATTERN . ACTION) pairs, e.g.,

;; (AWK* "awk-data.txt" '((("/Dan/") . (display $0)) ((= $3 0) (display $1))))

;; Validate pattern-action pairs

(define *pattern-action-pairs* '(("/Dan/" . (display $0)) ((= $3 0) . (display $1))))
(define *pa2* '(("/Dan/" . (display $0))))

(define (get-pattern pair)
  (car pair))

(define (get-action pair)
  (cdr pair))

(define (awk* file pattern-actions)
  (map-lines
   (lambda (line)
     (if (eof-object? line)
         '()
         (let ((field-vars (build-field-vars line "")))
           (for-each (lambda (pa) (apply-pattern pa line)) pattern-actions))))))
                  
;; (awk* "awk-data.txt" *pattern-action-pairs*)
  
(define (get-var var env)
  ;; Symbol Alist -> Object
  (let ((val (assoc var env)))
    (if val (cdr val) #f)))

(define (set-var var val env)
  ;; Symbol Object -> Alist
  (let ((pair (cons var val)))
    (cons pair env)))

(define (build-field-vars line sep)
  ;; String Regexp -> Alist
  ;; (build-field-vars "Hello it's me")
  ;; => '(($0 . "Hello it's me") ($1 . "Hello") ($2 . "It's") ($3 . "Me"))
  (let* ((pieces (string-split sep line))
         ($NF (length pieces))
         (results (list (cons '$NF $NF))))
    (let loop ((pieces pieces)
               (results results)
               (count 0))
      (if (null? pieces)
          (reverse results)
          (cond ((= count 0)
                 (let ((var (make-field-var count)))
                   (loop pieces
                         (cons (cons var line) results)
                         (+ count 1))))
                (else
                 (let ((var (make-field-var count))
                       (this (car pieces))
                       (rest (cdr pieces)))
                   (loop rest
                         (cons (cons var this) results)
                         (+ count 1)))))))))

(define (make-field-var n)
  ;; Integer -> Symbol
  (let* ((s (number->string n))
         (var-string (string-append "$" s)))
    (string->symbol var-string)))
