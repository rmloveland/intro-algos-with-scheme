;;; main.scm --- Load all code needed to work through the book.

(load-option 'format)
(load-option 'regular-expression)

;++ Rewrite this to use proper pathnames.

(define book-dir 
  (string-append 
   (get-environment-variable "HOME")
   "/Code/mathoms/intro-algos-with-scheme/"))

(define book-files
  (map (lambda (fname)
         (string-append book-dir fname))
       '("/00-introduction/test-lib.scm"
         "/01-searching/binary-search.scm"
         "/02-sorting/mergesort.scm"
         "/03-trees/binary-tree.scm"
         "/utils/timing.scm")))

(for-each (lambda (book-file)
            (load book-file))
          book-files)
