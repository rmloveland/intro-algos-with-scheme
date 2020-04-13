;; graphs.scm                                         -*- scheme -*-

;; This file contains code for working with unweighted, undirected
;; graphs, adapted from a blog post "Simple network search in
;; Scheme". The implementation is a port to Scheme of the Common Lisp
;; code in Chapter 19 of Winston and Horn's book *Lisp*.

;; In this adjacency list representation, each `(CAR . CDR)` pair
;; represents a `(NODE . NEIGHBORS)` relationship.

;; For more information about the properties of the adjacency list
;; representation of a graph, see Sedgewick, p. 421 (2nd ed.).

;; We can either define the graph statically...

;; (define *graph* '((f e)
;;                   (e b d f)
;;                   (d s a e)
;;                   (c b)
;;                   (b a c e)
;;                   (a s b d)
;;                   (s a d)))

;; ... or build it up stepwise, by consing.

(define (make-graph)
  '())

(define-syntax add-neighbor!
  (syntax-rules ()
    ((_ k v ?graph)
     (let ((new-neighbor (cons k v)))
       (push! new-neighbor ?graph)))))

(define (get-neighbor k graph)
  ;; List -> Any
  (let ((val (assoc k graph)))
    (if val (cdr val) '())))

;; Every time we visit a node, we want to extend our partial paths
;; list with that node.  (A.K.A. our "seen" list.)

(define (extend-path path graph)
  (display (reverse path))
  (newline)
  (map (lambda (new-node)
         (cons new-node path))
       ;; The `FILTER` is necessary here so we don't build circular
       ;; paths that keep us from ever finding the node we want.
       (filter (lambda (neighbor)
                 (not (member neighbor path)))
               (get-neighbor (first path) graph))))

;; If `BREADTH-FIRST?` is true, perform a breadth-first search.
;; Otherwise, use depth-first search.

(define (find-path-between start finish network breadth-first?)
  (let ((queue (list (list start))))
    (let loop ((start start)
               (finish finish)
               (network network)
               (queue queue))
      (cond ((null? queue) '())                    ;Queue empty?
            ((equal? finish (first (first queue))) ;Finish found?
             (reverse (first queue)))              ;Return path.
            (else
             (if breadth-first?
                 (loop start finish network
                       (append
                        (rest queue)
                        (extend-path (first queue) network)))
                 (loop start finish network
                       (append
                        (extend-path (first queue) network)
                        (rest queue)))))))))

(define (run-graph-tests)
  (let ((g (make-graph)))
  (begin
    (add-neighbor! 's '(a d) g))
    (add-neighbor! 'a '(s b d) g)
    (add-neighbor! 'b '(a c e) g)
    (add-neighbor! 'c '(b) g)
    (add-neighbor! 'd '(s a e) g)
    (add-neighbor! 'e '(b d f) g)
    (add-neighbor! 'f '(e) g)
    (assert equal?
            (find-path-between 's 'f g #t)
            '(s d e f)
            "FIND-PATH-BETWEEN: Breadth-first search generates expected output.")
    (assert equal?
            (find-path-between 's 'f g #f)
            '(s a b e f)
            "FIND-PATH-BETWEEN: Depth-first search generates expected output.")))

;; eof
