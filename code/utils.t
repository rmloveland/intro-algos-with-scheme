;;; -*- Mode: Scheme -*-

(load-module 'utils)
(load-module 'assert)

(assert equal? (make-list 3 #f) '(#f #f #f) "Make a list with 3 elements")
(assert equal? (atom? 'FOO) #t "'FOO is an atom")
(assert equal? (atom? '(1 2)) #f "'(1 2) is not an atom")

;; eof
