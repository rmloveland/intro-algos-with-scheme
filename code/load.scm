(begin
  (load "load-module.scm")
  (for-each (lambda (m) (load-module m))
            '(apropos
              assert
              awk
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
              yasos
              let-optionals

              ;; SRFIs
              srfi-0
              srfi-2
              srfi-8
              srfi-9

              ;; The following two modules are broken.  They depend on
              ;; LET-OPTIONALS but apparently macro use does not match
              ;; the SRFI's definition

              ;; srfi-13
              ;; srfi-14

              srfi-16
              ;; srfi-19 ; Broken in JScheme, uses (unsupported) exact number
              srfi-25
              srfi-28
              srfi-43
              srfi-69)))
