;;; -*- Mode: Scheme; -*-

;; (define-enum weekday '(sunday monday tuesday wednesday thursday friday saturday))

(define-record-type enum
  (make-enum name elements)
  enum?
  (elements enum-elements)
  (name enum-name))

(define (enum-member? element enum)
  (let ((val (member element (enum-elements enum))))
    (if val #t #f)))

(define-syntax define-enum
  (syntax-rules ()
    ((define-enum ?name ?elements)
     (make-enum ?name ?elements))))

;; eof
