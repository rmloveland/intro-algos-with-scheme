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
