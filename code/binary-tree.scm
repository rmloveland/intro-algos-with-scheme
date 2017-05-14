;;; binary-tree.scm

;; Constructors

(define (make-bintree-leaf element)
  ;; Any -> List
  "Create a leaf containing ELEMENT."
  (list element))

(define (make-bintree-node element left right)
  ;; Any List List -> List
  "Create a node containing ELEMENT, subtree LEFT and subtree RIGHT."
  (list element left right))

;; Selectors

(define (bintree-root B)
  ;; List -> Any
  (car B))

(define (bintree-leaf-element leaf)
  ;; List -> Any
  "Retrieve the element of LEAF."
  (first leaf))

(define (bintree-node-element node)
  ;; List -> Any
  "Retrieve the element of NODE."
  (first node))

(define (bintree-node-left node)
  ;; List -> Any
  "Retrieve the left subtree of NODE."
  (second node))

(define (bintree-node-right node)
  ;; List -> Any
  "Retrieve the right subtree of NODE."
  (third node))

;; Recognizers

(define (bintree-leaf? tree)
  ;; Any -> Bool
  "Test if TREE is a leaf."
  (and (list? tree)
       (= (length tree) 1)))

(define (bintree-node? tree)
  ;; Any -> Bool
  "Test if TREE is a node."
  (and (list? tree)
       (= (length tree) 3)))

(define (bintree-member? element tree)
  ;; Any List -> Bool
  "Test if ELEMENT is a member of TREE."
  (if (bintree-leaf? tree)
      (equal? element (bintree-leaf-element tree)) 
      (or (equal? element (bintree-node-element tree))
          (bintree-member? element (bintree-node-left tree))
          (bintree-member? element (bintree-node-right tree)))))

;; Exercise: Let size(B) be the number of members of a binary tree
;; B. Give a recursive definition of size(B), then implement a Scheme
;; procedure (bintree-size B) that returns size(B).

;; If B is a leaf, its size is 1. Otherwise, add 1 to the sum of
;; size(left subtree of B) and size(right subtree of B).

(define (bintree-size B)
  "How many elements in binary tree B?"
  (if (bintree-leaf? B)
      1
      (+ 1 (+ (bintree-size (bintree-node-left B))
              (bintree-size (bintree-node-right B))))))

(define (bintree-reverse B)
  "Reverse binary tree B."
  (if (bintree-leaf? B)
      B
      (make-bintree-node
       (bintree-node-element B)
       (bintree-reverse (bintree-node-right B))
       (bintree-reverse (bintree-node-left B)))))

(define (bintree-preorder B)
  "Create a list containing keys of B in preorder."
  (if (bintree-leaf? B)
      (list (bintree-leaf-element B))
      (cons (bintree-node-element B)
            (append
             (bintree-preorder (bintree-node-left B))
             (bintree-preorder (bintree-node-right B))))))

(define (fast-bintree-preorder B)
  ;;
  "A tail-recursive version of bin-tree-preorder."
  (letrec ((preorder-aux
            (lambda (B A)
              (if (bintree-leaf? B)
                  (cons (bintree-leaf-element B) A)
                  (cons (bintree-leaf-element B)
                        (preorder-aux (bintree-node-left B)
                                      (preorder-aux (bintree-node-right B) A)))))))
    (preorder-aux B '())))

;; Exercise: Implement a function that will create a list containing
;; members of a given binary tree in postorder. Implement also a
;; tail-recursive version of the same function.

(define (bintree-postorder-easy B)
  "Create a postorder list of keys in B, defined using
'bintree-preorder.'"
  (if (bintree-leaf? B)
      (list (bintree-leaf-element B))
      (reverse (bintree-preorder B))))

(define (bintree-postorder B)
  "Create a list containing keys of B in postorder."
  (if (bintree-leaf? B)
      (list (bintree-leaf-element B))
      (append (bintree-postorder (bintree-node-right B))
              (append (bintree-postorder (bintree-node-left B))
                      (cons (bintree-leaf-element B) '())))))

(define (fast-bintree-postorder B)
  "A tail-recursive version of 'bintree-postorder'."
  (letrec ((postorder-aux
            (lambda (B A)
              (if (bintree-leaf? B)
                  (cons (bintree-leaf-element B) A)
                  (postorder-aux
                   (bintree-node-right B)
                   (postorder-aux
                    (bintree-node-left B)
                    (cons (bintree-leaf-element B) A)))))))
    (postorder-aux B '())))

;; Exercise: Repeat the last exercise with inorder.

(define (bintree-inorder B)
  "Create a list containing keys of B in 'inorder' (infix) notation."
  (if (bintree-leaf? B)
      (list (bintree-leaf-element B))
      (append
       (append (bintree-inorder (bintree-node-left B))
               (cons
                (bintree-leaf-element B) '()))
       (bintree-inorder (bintree-node-right B)))))

(define (fast-bintree-inorder B)
  "A tail-recursive version of 'bintree-inorder'."
  (letrec ((inorder-aux
            (lambda (B A C)
              (if (bintree-leaf? B)
                  (cons (bintree-leaf-element B) C)
                  (inorder-aux
                   (bintree-node-left B) A
                   (inorder-aux
                    (cons (bintree-leaf-element B) A) C
                    (inorder-aux
                     (bintree-node-right B) A C)))))))
    (inorder-aux B '() '())))

;; Sets

(define (make-empty-set)
  "Creates an empty set."
  '())

(define (set-insert element S)
  "Return a set containing all the members of the set S plus the
element element."
  (lset-adjoin equal? S element))

(define (set-remove element S)
  "Return a set containing all the members of the set S except the
element element."
  (let ((not-element?
         (lambda (elem)
           (if (equal? element elem)
               #f
               #t))))
    (filter not-element? S)))

(define (set-member? element S)
  "Return #t if set S contains element element."
  (if (member element S)
      #t
      #f))

(define (set-empty? S)
  "Return #t if the set S is empty."
  (null? S))


;; Binary search trees

;; (These will be implemented in terms of our binary trees above.)

(define (make-empty-bst)
  "Create an empty binary search tree."
  '())

(define (bst-empty? B)
  "Check if the given binary search tree is empty."
  (null? B))

(define (bst-member? element B)
  "Check if the given element element is a member of binary search tree B."
  (if (bst-empty? B)
      #f
      (bst-nonempty-member? element B)))

(define (bst-nonempty-member? element B)
  "Check if element element is a member of non-empty binary search tree B."
  (if (bintree-leaf? B)
      (= element (bintree-leaf-element B))
      (if (<= element (bintree-node-element B))
          (bst-nonempty-member? element (bintree-node-left B))
          (bst-nonempty-member? element (bintree-node-right B)))))

(define (bst-insert element B)
  "Insert element element into binary search tree B."
  (if (bst-empty? B)
      (make-bintree-leaf element)
      (bst-nonempty-insert element B)))

(define-syntax bst-insert!
  (syntax-rules ()
    ((bst-insert! element tree)
     (set! tree (bst-insert element tree)))))

(define (list->tree xs)
  (let ((tree '()))
    (for-each (lambda (x)
                (set! tree (bst-insert x tree)))
              xs)
    tree))


(define (treesum tree)
  (cond ((null? tree) 0)
        ((atom? (car tree))
         (+ (car tree)
            (treesum (cdr tree))))
        (else (+ (treesum (car tree))
                 (treesum (cdr tree))))))

;; ++ TREESORT has a bug: it's clipping the last element of the list
;; it's sorting, e.g.:

;; > (merge-sort *nums* <)
;; (39 239 318 427 499 548 733 806 826 856 889 944 978 981 1009
;;     1020)
;; > (treesort *nums*)
;; (39 239 318 427 499 548 733 806 826 856 889 944 978 981
;;     1009)

(define (treesort xs)
  (let ((result '())
        (seen '())
        (tree (list->tree xs)))
    (for-each
     (lambda (x)
       (if (member x seen)
           (set! result (cons x result))
           (set! seen (cons x seen))))
     (fast-bintree-preorder tree))
    (reverse result)))

(define (bst-nonempty-insert element B)
  "Insert ELEMENT into the non-empty binary search TREE."
  (if (bintree-leaf? B)
      (bst-leaf-insert element B)
      (let ((this (bintree-node-element B))
            (left (bintree-node-left B))
            (right (bintree-node-right B)))
        (if (<= element (bintree-node-element B))
            (make-bintree-node this
                               (bst-nonempty-insert element (bintree-node-left B))
                               right)
            (make-bintree-node this
                               left
                               (bst-nonempty-insert element (bintree-node-right B)))))))

(define (bst-leaf-insert element L)   ; This needs changing for Harel.
  "Insert element element into a binary search tree with only one leaf."
  (let ((this (bintree-leaf-element L)))
    (if (= element this)
        L                               ; return yourself
        (if (< element this)
            (make-bintree-node element ; Make element a node, and element the lesser leaf
                               (make-bintree-leaf element)
                               (make-bintree-leaf this)) ; Make
                                        ; yourself the right leaf
            (make-bintree-node this     ; else make yourself the node
                               (make-bintree-leaf this) ; ...and the
                                        ; lesser leaf
                               (make-bintree-leaf element)))))) ; make element the
					; greater leaf

;; Removal

(define (bst-remove element B)
  "Remove the element element from the binary search tree B."
  (if (bst-empty? B)
      B
      (if (bintree-leaf? B)
          (bst-leaf-remove element B)
          (bst-node-remove element B))))

(define (bst-leaf-remove element leaf)
  "Remove ELEMENT from binary search tree LEAF."
  (if (= element (bintree-leaf-element L))
      (make-empty-bst)
      L))

(define (bst-node-remove element N)
  "Remove node element from the binary search tree node N."
  (let ((this (bintree-node-element N))
        (left (bintree-node-left N))
        (right (bintree-node-right N)))
    (if (<= element this)
        (if (bintree-leaf? left)
            (if (= element (bintree-leaf-element left))
                right
                N)
            (make-bintree-node this (bst-node-remove element left) right))
        (if (bintree-leaf? right)
            (if (= element (bintree-leaf-element right))
                left
                N)
            (make-bintree-node this left (bst-node-remove element right))))))

;; Tests

(define L '(128 76 106 402 100 46 354 1018 112 28 396 35))
(define test-bintree '(- (+ (128) (12)) (136.2)))
(define test-bst '(2 (1 (1) (2)) (3 (3) (4))))

(define (run-binary-tree-tests)
  (let* ((sorted '(28 35 46 76 100 106 112 128 354 396 402 1018))
         (unsorted '(128 76 106 402 100 46 354 1018 112 28 396 35))
         (treesorted (treesort unsorted))
         (test-bintree '(- (+ (128) (12)) (136.2)))
         (test-bst '(2 (1 (1) (2)) (3 (3) (4)))))
    (assert equal? sorted treesorted "treesort: sort a list")))

;; eof
