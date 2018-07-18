;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; cycle.scm
;;;
;;; Functions related to cycles and iterated functions
;;;
;;; You're going to want a font and editor that work with unicode for this
;;; one.
;;;
;;; Copyright (C) 2018 Jeff Spaulding <sarnet@gmail.com>
;;;
;;; This is the ISC License.
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

(define-module (spauldo cycle)
  #:export (cycle-detect seq-run))

;; Floyd's tortoise and hare algorithm
;; Takes a function and an initial value.  Returns two values: the point at
;; which the sequence starts to cycle, and the length of the cycle.
;;
;; The function must be one that can be iterated (see the mathematical
;; definition of an iterated function), and must be one that actually
;; cycles.  If it doesn't cycle, you're going to have an infinite loop.
;;
;; See Wikipedia's page on this.  In this code, i is the tortoise, j is the
;; hare,  λ is the length of the cycle, and μ is the xₖ that the cycle begins
;; on.
;;
;; I don't think this will act up for people that (define λ lambda), but I
;; don't do that so I can't be sure without checking.  I'm too lazy to check.
;; Not like anyone else is every going to run this code anyway.
;;
;; This is an example of something that would be a lot clearer using variables
;; instead of each function taking the results of the previous function, but
;; hey, it's Scheme.  Gotta roll with the flow.
(define (cycle-detect f x₀)
  ;; i starts at f(x₀), j starts as f(f(x₀)).  j moves twice as fast as i.
  ;; Returns the vale of j.
  ;; When finished, i and j have found identical values in the sequence.
  (define (first-race i j)
    (display "1 ")
    (if (= i j)
	j
	(first-race (f i) (f (f j)))))

  ;; i reset to x₀, i and j step each, accumulate μ
  ;; Returns four values so that it can set up for third-race:
  ;; 1 (the initial point for λ), μ, i, and f(i).
  ;; By setting the tortoise to x₀ at this point, we guarantee (because math)
  ;; that iterating each forward by one, we will eventually reach a point
  ;; where the tortoise and hare have the same value - and that point will
  ;; be the beginning of the sequence (μ).
  (define (second-race μ i j)
    (display "2 ")
    (if (= i j)
	(values 1 μ i (f i))
	(second-race (1+ μ) (f i) (f j))))

  ;; λ starts at 1, accumulate λ, μ stays the same, i stays the same, j steps.
  ;; Returns μ and λ.
  ;; We've found μ now, and that's where the tortoise (i) is.  So now we move
  ;; the hare (j) forward and count how many steps it takes until i = j.
  ;; That's our λ.
  (define (third-race λ μ i j)
    (display "3 ")
    (if (= i j)
	(values μ λ)
	(third-race (1+ λ) μ i (f j))))

  (call-with-values
      (lambda () (second-race 0 x₀ (first-race (f x₀) (f (f x₀)))))
    third-race))


;; Iterate a sequence (i.e. each iteration applies the function to the previous
;; result).  Returns the values of the sequence as a list.
(define (seq-run f x₀ times)
  (let ((seq '()))
    (define (seq-run-h xᵢ i)
      (set! seq (cons xᵢ seq))
      (when (= (euclidean-remainder i 26) 0)
	(newline))
      (if (< i times)
	  (seq-run-h (f xᵢ) (1+ i))
	  (newline)))
    (seq-run-h x₀ 1)
    (reverse seq)))


;; Not exported or used in the code above, but used as an example of a
;; sequence based on an iterated function.  Use a closure to specify n.
(define (square-plus-1-mod-n x n)
  (euclidean-remainder (+ (* x x) 1) n))
