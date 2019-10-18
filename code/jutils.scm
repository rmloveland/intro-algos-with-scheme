;;; -*- Mode: Scheme -*-

(load "elf/basic.scm")
(load "elf/mbe.scm")

(define (getenv v)
  (System.getenv v))

(define (current-time)
  (/ (System.currentTimeMillis) 1000))
