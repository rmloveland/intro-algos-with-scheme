;;; -*- Mode: Scheme -*-

(load-module 'uuid)
(load-module 'pregexp)
(load-module 'assert)

(define (uuid-test)
  (let* ((id (generate-uuid))
         (retval (pregexp-match "[a-z0-9-]+" (generate-uuid))))
    (and (list? retval)
         (= (length retval) 1))))

(assert equal? (uuid-test) #t "UUID format is correct.")

;; eof
