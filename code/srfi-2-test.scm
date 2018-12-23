;;; -*- Mode: Scheme -*-

(load-module 'srfi-2)
(load-module 'assert)

(assert equal? (and-let* () 1) 1 "1 equals 1")
(assert equal? (and-let* () 1 2) 2 "Print second argument")
(assert equal? (and-let* ()) #t "Return true by default")
(assert equal? (let ((x #f)) (and-let* (x))) #f
        "Pass through outer-scope boolean bindings correctly")
(assert equal? (let ((x 1)) (and-let* (x))) 1
        "Pass through outer-scope numeric bindings correctly")
(assert equal? (and-let* ((x #f))) #f
        "Handle locally scoped boolean bindings correctly")
(assert equal? (and-let* ((x 1)) ) 1
        "Handle locally scoped numeric bindings correctly")
(assert equal? (and-let* ((#f) (x 1)) ) #f "")
(assert equal? (and-let* ((2) (x 1)) ) 1 "")
(assert equal? (and-let* ((x 1) (2)) ) 2 "")
(assert equal? (let ((x #f)) (and-let* (x) x)) #f "")
(assert equal? (let ((x "")) (and-let* (x) x)) "" "")
(assert equal? (let ((x "")) (and-let* (x)  )) "" "")
(assert equal? (let ((x 1)) (and-let* (x) (+ x 1))) 2 "")
(assert equal? (let ((x #f)) (and-let* (x) (+ x 1))) #f "")
(assert equal? (let ((x 1)) (and-let* (((positive? x))) (+ x 1))) 2 "")
(assert equal? (let ((x 1)) (and-let* (((positive? x))) )) #t "")
(assert equal? (let ((x 0)) (and-let* (((positive? x))) (+ x 1))) #f "")
(assert equal? (let ((x 1)) (and-let* (((positive? x)) (x (+ x 1))) (+ x 1)))  3 "")
(assert equal? (let ((x 1)) (and-let* (x ((positive? x))) (+ x 1))) 2 "")
(assert equal? (let ((x 1)) (and-let* (((begin x)) ((positive? x))) (+ x 1))) 2 "")
(assert equal? (let ((x 0)) (and-let* (x ((positive? x))) (+ x 1))) #f "")
(assert equal? (let ((x #f)) (and-let* (x ((positive? x))) (+ x 1))) #f "")
(assert equal? (let ((x #f)) (and-let* (((begin x)) ((positive? x))) (+ x 1))) #f "")
(assert equal? (let ((x 1)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))) #f "")
(assert equal? (let ((x 0)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))) #f "")
(assert equal? (let ((x #f)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))) #f "")
(assert equal? (let ((x 3)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))) 3/2 "")

;; eof
