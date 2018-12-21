;; ******************
;; * LISTING: yasos *
;; ******************
;; FILE		"yasos.scm"
;; IMPLEMENTS	YASOS: Yet Another Scheme Object System
;; AUTHOR	Kenneth Dickey
;; DATE		1992 March 1
;; LAST UPDATED	1992 March 5

;; REQUIRES	R4RS Syntax System

;; NOTES: An object system for Scheme based on the paper by
;; Norman Adams and Jonathan Rees: "Object Oriented Programming in
;; Scheme", Proceedings of the 1988 ACM Conference on LISP and 
;; Functional Programming, July 1988 [ACM #552880].

;;
;; INTERFACE:
;;
;; (DEFINE-OPERATION (opname self arg ...) default-body)
;;
;; (DEFINE-PREDICATE opname)
;;
;; (OBJECT ((name self arg ...) body) ... )
;;
;; (OBJECT-WITH-ANCESTORS ( (ancestor1 init1) ...) operation ...)
;;
;; in an operation {a.k.a. send-to-super}
;;   (OPERATE-AS component operation self arg ...)
;;


;; INSTANCES

; (define-predicate instance?)
; (define (make-instance dispatcher)
;    (object
; 	((instance?  self) #t)
;       ((instance-dispatcher self) dispatcher)
; )  )

(define make-instance 'bogus)  ;; defined below
(define instance?     'bogus)

(define-syntax instance-dispatcher ;; alias so compiler can inline for speed
  (syntax-rules () ((instance-dispatcher inst) (cdr inst))))

(let ((instance-tag "instance") ) ;; Make a unique tag within a local scope.
  ;; No other data object is EQ? to this tag.
  (set! make-instance
        (lambda (dispatcher) (cons instance-tag dispatcher)))
  (set! instance?
        (lambda (obj) (and (pair? obj) (eq? (car obj) instance-tag)))))

;; DEFINE-OPERATION

(define-syntax define-operation
  (syntax-rules ()
    ((define-operation (<name> <inst> <arg> ...) <exp1> <exp2> ...)
     ;;=>
     (define <name>
       (letrec ((self
                 (lambda (<inst> <arg> ...)
                   (cond
                    ((and (instance? <inst>) 
                          ((instance-dispatcher <inst>) self))
                     => (lambda (operation) (operation <inst> <arg> ...)))
                    (else <exp1> <exp2> ...)))))
         self)))
    ((define-operation (<name> <inst> <arg> ...) ) ;; no body
     ;;=>
     (define-operation (<name> <inst> <arg> ...)
       (error "Operation not handled" 
              '<name> 
              (format (if (instance? <inst>) "#<INSTANCE>" "~s") <inst>))))))

;; DEFINE-PREDICATE

(define-syntax define-predicate
  (syntax-rules ()
    ((define-predicate <name>)
     ;;=>
     (define-operation (<name> obj) #f))))

;; OBJECT

(define-syntax object
  (syntax-rules ()
    ((object ((<name> <self> <arg> ...) <exp1> <exp2> ...) ...)
     ;;=>
     (let ((table
            (list (cons <name>
                        (lambda (<self> <arg> ...) <exp1> <exp2> ...))
                  ...)))
       (make-instance
        (lambda (op)
          (cond
           ((assq op table) => cdr)
           (else #f))))))))

;; OBJECT with MULTIPLE INHERITANCE  {First Found Rule}

(define-syntax object-with-ancestors
  (syntax-rules ()
    ((object-with-ancestors ( (<ancestor1> <init1>) ... ) <operation> ...)
     ;;=>
     (let ( (<ancestor1> <init1>) ...  )
       (let ( (child (object <operation> ...)) )
         (make-instance
          (lambda (op) 
            (or ((instance-dispatcher child) op)
                ((instance-dispatcher <ancestor1>) op) ...))))))))

;; OPERATE-AS  {a.k.a. send-to-super}

; used in operations/methods

(define-syntax operate-as
  (syntax-rules ()
    ((operate-as <component> <op> <composit> <arg> ...)
     ;;=>
     (((instance-dispatcher <component>) <op>) <composit> <arg> ...))))

(define-operation (print obj)
   ;; As with CLOS, you must teach new objects how to print
   ;; themselves.
  (format
   ;; if an instance does not have a PRINT operation..
   (if (instance? obj) "#<INSTANCE>" "~s") obj))

(define-operation (size obj)
  ;; Default behavior should be overridden by objects that implement
  ;; this operation.
  (cond   
    ((vector? obj) (vector-length obj))
    ((list?   obj) (length obj))
    ((pair?   obj) 2)
    ((string? obj) (string-length obj))
    ((char?   obj) 1)
    (else 
      (error "Operation not supported: size" obj))))



;;; Examples

;; From the Scheme wiki at http://community.schemewiki.org/?YASOS

;; ------------------------
;; KONS object

;;  interface

(define-predicate pare?)
(define-operation (kar pare))
(define-operation (kdr pare))
(define-operation (set-kar! pare a))
(define-operation (set-kdr! pare d))

;; implementation

(define (kons a d)
  (object 
   ((pare? self) #t) 
   ((kar self) a) 
   ((kdr self) d) 
   ((set-kar! self new-a) 
    (set! a new-a)) 
   ((set-kdr! self new-d) 
    (set! d new-d))
   ((print self)
    (format "#<kons: ~A . ~A>" (kar self) (kdr self)))))

;; ------------------------
;; POINT object

;; interface

(define-predicate point?)               ; #f by default
(define-operation (get-x obj))
(define-operation (get-y obj))
(define-operation (set-x! obj val))
(define-operation (set-y! obj val))
(define-operation (size obj))

;; implementation

(define (make-point x y)
  (object
   ((point? self) #t)
   ((get-x self) x)
   ((get-y self) y)
   ((set-x! self val)
    (set! x val))
   ((set-y! self val)
    (set! y val))
   ((size self) 2)
   ((print self)
    (format "#<POINT: ~A ~A>" (get-x self) (get-y self)))))

;; ------------------------
;; 3-D POINT inherits from POINT

;; interface

(define-operation (get-z obj))
(define-operation (set-z! obj val))

;; implementation

(define (make-3d-point x y z)
  (object-with-ancestors
   ((the-point (make-point x y)))
   ((get-z self) z)
   ((set-z! self val)
    (set! z val) z)
   ((size self) 3)
   ((print self)
    (format "#<3d-point: ~A ~A ~A>" (get-x self) (get-y self) (get-z self)))))
