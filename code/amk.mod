;;; -*- Mode: Scheme -*-

(define-module amk
  (exports

   ;; Goals.
   fail succeed ==
   all any one neg

   ;; Logic variables.
   fresh var var? circular? _

   ;; Frames (a.k.a. substitutions).
   empty-s ext-s

   ;; Parameterized goals.
   eqo nullo
   memo memqo rmemqo firsto
   conso caro cdro
   appendo pairo filtero reverseo

   ;; Primary interface.
   run*

   (requires utils)))                   ; ATOM?

;; eof
