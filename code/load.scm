;; load.scm: load code for the book                      -*- scheme -*-

(begin
  (load "load-module.scm")
  (for-each (lambda (m) (load-module m))
            '(assert
              binary-search
              binary-tree
              destructive-ops
              format
              graphs
              io
              mergesort
              pregexp
              set
              strings
              utils
              xml
              srfi-16
              srfi-25)))

;; eof
