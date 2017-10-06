;;; UUID: Universally Unique Identifiers.

;;; Copyright (c) 2009, Taylor R. Campbell
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; * Redistributions of source code must retain the above copyright
;;;   notice, this list of conditions and the following disclaimer.
;;;
;;; * Redistributions in binary form must reproduce the above copyright
;;;   notice, this list of conditions and the following disclaimer in
;;;   the documentation and/or other materials provided with the
;;;   distribution.
;;;
;;; * Neither the names of the authors nor the names of contributors
;;;   may be used to endorse or promote products derived from this
;;;   software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Needs RANDOM-INTEGER from SRFI 27.  Uses four bits of randomness at
;;; a time, except for one two-bit piece (the `y' nybble).  Generates
;;; version 4 UUIDs, i.e. random ones.  The other versions are silly --
;;; based on hardware MAC addresses, time stamps, user and group id
;;; numbers, &c.

(define (generate-uuid)

  (define (digit->char digit)
    (integer->char
     (if (< digit #xA)
         (+ (char->integer #\0) digit)
         (+ (char->integer #\a) (- digit #xA)))))

  (define (random-substring! string size position)
    (let ((start position)
          (end (+ position size)))
      (do ((i start (+ i 1)))
          ((>= i end))
        (string-set! string i (digit->char (random-integer #x10))))))

  (let ((uuid
         (make-string (string-length "xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx")
                      #\-)))
    (random-substring! uuid 8 0)
    (random-substring! uuid 4 9)
    (string-set! uuid 14 #\4)
    (random-substring! uuid 3 15)
    (string-set! uuid 19 (digit->char (+ 8 (random-integer 4))))
    (random-substring! uuid 3 20)
    (random-substring! uuid 12 24)
    uuid))

;; eof
