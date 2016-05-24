;; ,open pp random floatnums formats sort srfi-8 srfi-9 srfi-13

(define (load-book-files)
  (let ((the-directory
         (expand-file-name "~/Documents/personal/intro-algos-with-scheme/code/")))
    (with-cwd the-directory
      (load "assert.scm")
      (load "binary-search.scm")
      (load "binary-tree.scm")
      (load "graphs.scm")
      (load "mergesort.scm"))))

(load-book-files)
