;;; vector-utils.scm
;;;
;;; Copyright (c) 2017 Jeff Spaulding <sarnet@gmail.com>
;;;
;;; This module contains various functions related to scheme vectors
;;; (not mathematical vectors).
;;;
;;; Permission to use, copy, modify, and distribute this software for any
;;; purpose with or without fee is hereby granted, provided that the above
;;; copyright notice and this permission notice appear in all copies.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(define-module (spauldo vector-utils.scm)
  #:use-module (srfi srfi-43)
  #:export (vector-remove-and-splice))

;;; Returns vector v with item i removed
(define (vector-remove-and-splice v i)
  (cond ((< (vector-length v) 2)
         (error
          "vector-remove-splice: attempt to remove last element in vector"
          -1))
        ((eq? i 0)
         (vector-copy v 1))
        ((eq? i (- (vector-length v) 1))
         (vector-copy v 0 i))
        (else
         (vector-append
          (vector-copy v 0 i)
          (vector-copy v (+ i 1) (vector-length v))))))
