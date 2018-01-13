;;;   -*- Mode: Scheme; -*-

;; io.scm -- Utilities for file input/output

;; XXX This implementation of `READ-LINE` has a bug.  We know this
;; because `MAP-LINES` (below) works just fine with Larceny's
;; implementation of `READ-LINE`.

(define (read-line port)
  ;; Port -> String
  (let ((sep #\newline))
    (let loop ((chars '())
               (this-char (read-char port))
               (next-char (peek-char port)))
      (cond
       ((eof-object? next-char) next-char)
       ((char=? next-char sep) (list->string (reverse chars)))
       (else (loop (cons this-char chars)
                   (read-char port)
                   (peek-char port)))))))

(define (map-lines file)
  (call-with-input-file file
    (lambda (port)
      (let loop ((line (read-line port))
                 (lines '()))
        (if (eof-object? line)
            (reverse lines)
            (loop (read-line port)
                  (cons line lines)))))))
