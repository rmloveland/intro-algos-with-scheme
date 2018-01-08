;; -*- Mode: Scheme; -*-

(define-module xml
  (exports
   make-tag
   display-attr
   display-tag
   svg                                  ;syntax
   circle
   text
   rect
   make-path                            ;syntax
   move-to
   make-points
   line-to
   horizontal-line
   with-attrs                           ;syntax
   vertical-line)
  (requires format utils srfi-9))

;; eof

