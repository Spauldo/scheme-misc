;;; discrete.scm
;;;
;;; Copyright (c) 2017 Jeff Spaulding <sarnet@gmail.com>
;;;
;;; This module contains various functions related to discrete math.
;;;
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

(define-module (spauldo discrete)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:use-module (spauldo vector)
  #:export (get-all-permutations
            even-permutation?))

;;; Returns all permutations of a list
(define (get-all-permutations l)
  (let ((v (list->vector l)))
    (cond ((< (vector-length v) 2)
           (list (list (vector-ref v 0))))
          (else
           (vector-fold (lambda (i state val)
                          (append state (map (lambda (res-list)
                                               (cons val res-list))
                                             (get-all-permutations
                                              (vector->list
                                               (vector-remove-and-splice v i))))))
                        '()
                        v)))))

;;; Is a permutation even or odd?
(define (even-permutation? l)
  (even? (num-inversions l)))

;;; Get number of inversions in a list
(define* (num-inversions l #:optional vec (i 0) (num-swaps 0))
  (let ((v (if vec vec (list->vector l))))
    (if (>= (1+ i) (vector-length v))
        num-swaps
        (if (eq? i (find-most l < v i i))
            (num-inversions l v (1+ i) num-swaps)
            (num-inversions l v (1+ i) (1+ num-swaps))))))

;;; Find position of item in list l that compares the most by comparison comp,
;;; starting with position i
(define* (find-most l comp #:optional vec (i 1) (most 0))
  (let ((v (if vec vec (list->vector l))))
    (if (>= i (vector-length v))
        most
        (find-most l comp v (1+ i) (if (comp (vector-ref v i)
                                             (vector-ref v most))
                                       i
                                       most)))))
