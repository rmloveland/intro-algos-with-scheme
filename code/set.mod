;;; -*- Mode: Scheme; -*-

(define-module set
  (exports
   make-set
   set?
   set-elements
   set-add!
   set-remove!
   set-member?)
  (requires srfi-69 destructive-ops))

;; eof
