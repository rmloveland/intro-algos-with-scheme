;;;   -*- Mode: Scheme; -*-

;; io.scm -- Utilities for file input/output

;; The implementation of `READ-LINE` given here may (?) have a bug (or
;; it may just not be R7RS).  Its caller `MAP-LINES` (below) works
;; just fine with Larceny 1.3's default implementation of `READ-LINE`,
;; but inserts an extra "" in between each element if the
;; implementation below is used.  However, everything here works as
;; expected on Chez 9.5 and Scheme48 1.9.

;; For more information about the Larceny behavior and why it may be
;; correct, see the following:
;; 
;; - https://github.com/larcenists/larceny/issues/748
;; 
;; - https://github.com/larcenists/larceny/commit/6f0ce4455c

(define (read-line port)
  ;; Port -> String
  (let ((sep #\newline))
    (let loop ((chars '())
               (this-char (read-char port))
               (next-char (peek-char port)))
      (cond
       ((eof-object? next-char) next-char)
       ((char=? next-char sep) (list->string (reverse (cons this-char chars))))
       (else (loop (cons this-char chars)
                   (read-char port)
                   (peek-char port)))))))

(define (map-lines fn file)
  (call-with-input-file file
    (lambda (port)
      (let loop ((line (read-line port))
                 (lines '()))
        (if (eof-object? line)
            (reverse lines)
            (loop (read-line port)
                  (cons (fn line) lines)))))))
