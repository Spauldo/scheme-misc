;;; algebra.scm
;;;
;;; This module contains various basic algebraic equations
;;;
;;; Copyright (c) 2017 Jeff Spaulding <sarnet@gmail.com>
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

(define-module (spauldo algebra)
  :export (natural?
           quadratic
           factorial
           fibonacci))

;; Predicate for natural numbers (nonnegative integers)
(define (natural? n)
  (if (and (integer? n)
           (> n -1))
      #t
      #f))

;; Quadratic Formula
(define (quadratic a b c)
  (define (qh a b c op)
    (/ (op (- b)
           (sqrt (- (* b b) (* 4 a c))))
       (* 2 a)))
  (list
   (qh a b c +)
   (qh a b c -)))

;; Factorial Function
;; This is an O(n) implementation
(define (factorial n)
  (define (fh i n sum)
    (if (> i n)
        sum
        (fh (1+ i) n (* i sum))))
  (cond ((not (natural? n))
         (error "Factorial function only defined for natural numbers."))
        ((< n 2)
         1)
        (else
         (fh 2 n 1))))

;; Fibonacci Function
;; This is an O(n) implementation
(define (fibonacci n)
  (define (fh i j n)
    (if (< n 0)
        j
        (fh j (+ j i) (1- n))))
  (cond ((not (natural? n))
         (error "Fibonacci sequence only defined for natural numbers."))
        ((= n 0)
         0)
        ((< n 3)
         1)
        (else
         (fh 1 1 (- n 3)))))

;; Regarding the two functions below:
;; I did not realize these are part of R5RS already.  I'm removing them because
;; the native implementation is very likely faster than using these.

;; Greatest common divisor, Euclid's algorithm
;; (define (gcd a b)
;;   (cond ((not (and (integer? a)
;; 		   (integer? b)))
;; 	 (error "Greatest common divisor only defined for integers."))
;; 	((= b 0)
;; 	 a)
;; 	(#t
;; 	 (gcd b (euclidean-remainder a b)))))

;; Least common multiple, using gcd
;; (define (lcm a b)
;;   (cond ((not (and (integer? a)
;; 		   (integer? b)))
;; 	 (error "Least common multiple only defined for integers."))
;; 	((and (= a 0) (= b 0))
;; 	 0)
;; 	(#t
;; 	 (* (abs a) (/ (abs b) (gcd a b))))))
