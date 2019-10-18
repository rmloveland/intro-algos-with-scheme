;;; -*- Mode: Scheme -*-

(define-module graphs
  (exports
   make-graph
   add-neighbor!
   get-neighbor
   extend-path
   find-path-between)
  (requires destructive-ops utils))

;; eof
