;;; -*- mode: scheme -*-

(define-module binary-tree
  (exports

   ;; Binary trees (generic)

   bintree-size
   bintree-reverse
   bintree-preorder
   bintree-postorder
   bintree-inorder
   bintree-root
   bintree-node-left
   bintree-node-right
   bintree-smallest
   bintree-pop!

   ;; Binary search trees (BSTs)

   list->bst
   vector->bst
   bst-empty?
   bst-member?
   bst-insert
   bst-insert!
   bst-remove
   bst-remove!
   bst-smallest

   ;; Misc. BST things
   ;; ++ N.B. These will probably get renamed or even go away

   treesum
   treesort))

;; eof
