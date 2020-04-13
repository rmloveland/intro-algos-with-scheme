;;; -*- Mode: Scheme -*-

(define-module graphs
  (exports
   make-graph
   add-neighbor!
   get-neighbor
   extend-path
   find-path-between
   run-graph-tests)
  (requires destructive-ops utils))

;; eof
