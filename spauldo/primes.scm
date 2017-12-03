;;; primes.scm
;;;
;;; This module contains various prime-related functions and constants
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

(define-module (spauldo primes))

;; Lookup table for primality.  Each value in the vector is true if the index
;; is prime.
(define prime-vector
  #(#f #f #t #t #f #t #f #t #f #f #f #t #f #t #f #f #f
       #t #f #t #f #f #f #t #f #f #f #f #f #t #f #t #f
       #f #f #f #f #t #f #f #f #t #f #t #f #f #f #t #f
       #f #f #f #f #t #f #f #f #f #f #t #f #t #f #f #f
       #f #f #t #f #f #f #t #f #t #f #f #f #f #f #t #f
       #f #f #t #f #f #f #f #f #t #f #f #f #f #f #f #f
       #t #f #f #f #t #f #t #f #f #f #t #f #t #f #f #f
       #t #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f
       #f #f #t #f #f #f #f #f #t #f #t #f #f #f #f #f
       #f #f #f #f #t #f #t #f #f #f #f #f #t #f #f #f
       #f #f #t #f #f #f #t #f #f #f #f #f #t #f #f #f
       #f #f #t #f #t #f #f #f #f #f #f #f #f #f #t #f
       #t #f #f #f #t #f #t #f #f #f #f #f #f #f #f #f
       #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f
       #f #f #t #f #t #f #f #f #t #f #f #f #f #f #t #f
       #t #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f
       #t #f #f #f #f #f #t #f #f #f #f #f #t #f #t #f
       #f #f #f #f #t #f #f #f #t #f #t #f #f #f #f #f
       #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f
       #f #f #t #f #f #f #t #f #t #f #f #f #t #f #f #f
       #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f
       #t #f #f #f #f #f #f #f #f #f #t #f #t #f #f #f
       #t #f #f #f #f #f #t #f #f #f #f #f #f #f #t #f
       #f #f #f #f #t #f #f #f #f #f #t #f #f #f #t #f
       #f #f #f #f #t #f #f #f #f #f #f #f #t #f #f #f
       #t #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f
       #f #f #t #f #t #f #f #f #f #f #f #f #f #f #t #f
       #t #f #f #f #f #f #t #f #f #f #t #f #f #f #f #f
       #t #f #f #f #f #f #f #f #t #f #f #f #t #f #t #f
       #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f
       #f #f #f #f #f #f #t #f #f #f #t #f #f #f #f #f
       #f #f #t #f #f #f #t #f #f #f #f #f #t #f #f #f
       #f #f #f #f #f #f #f #f #t #f #t #f #f #f #f #f
       #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f
       #f #f #t #f #f #f #f #f #f #f #f #f #t #f #f #f
       #f #f #t #f #f #f #f #f #t #f #t #f #f #f #f #f
       #t #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f
       #t #f #f #f #f #f #t #f #t #f #f #f #f #f #t #f
       #f #f #f #f #t #f #f #f #t #f #t #f #f #f #f #f
       #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f
       #t #f #t #f #f #f #t #f #f #f #f #f #t #f #f #f
       #f #f #t #f #t #f #f #f #f #f #f #f #f #f #f #f
       #t #f #f #f #t #f #f #f #f #f #t #f #f #f #f #f
       #f #f #t #f #f #f #f #f #f #f #f #f #t #f #f #f
       #f #f #f #f #t #f #f #f #f #f #f #f #f #f #t #f
       #f #f #f #f #f #f #t #f #f #f #f #f #t #f #f #f
       #f #f #t #f #f #f #t #f #f #f #f #f #f #f #t #f
       #f #f #f #f #t #f #f #f #t #f #f #f #f #f #f #f
       #t #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f
       #f #f #t #f #f #f #f #f #f #f #f #f #t #f #f #f
       #f #f #f #f #f #f #f #f #t #f #t #f #f #f #f #f
       #f #f #f #f #t #f #t #f #f #f #t #f #t #f #f #f
       #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f
       #f #f #f #f #t #f #f #f #t #f #t #f #f #f #t #f
       #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f
       #t #f #t #f #f #f #t #f #f #f #f #f #f #f #f #f
       #f #f #f #f #f #f #f #f #f #f #t #f #f #f #t #f
       #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f
       #t #f #f #f #f #f #f #f #t #f #f #f #t #f #f #f
       #f #f #t #f #f #f #f #f #t #f #f #f #f #f #f #f
       #f #f #f #f #f #f #t #f #f #f #t #f #f #f #f #f
       #t #f #f #f #f #f #t #f #f #f #f #f #f #f #t #f
       #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f
       #t #f #f #f #t #f #f #f #f #f #t #f #t #f #f))

(define (sieve-of-sundaram n)
  ;; max: the maximum number we use for the first part of the algorithm
  ;; base-nums: a vector of the results from the inner loop
  ;; numbers: a vector of numbers, where the index of each element containing
  ;;   #t is prime
  (let* ((max (floor (/ (- n 2) 2)))
         (base-nums (make-vector (1+ max) #t))
         (numbers (make-vector (1+ n) #f)))

    ;; j loop (from i to max)
    (define (inner-loop i j)
      (let ((index (+ i j (* 2 i j))))
        (when (<= index max)
          (vector-set! base-nums index #f)
          (inner-loop i (1+ j)))))

    ;; i loop (from 1 to max)
    (define (outer-loop i)
      (when (<= i max)
        (inner-loop i i)
        (outer-loop (1+ i))))

    ;; Mark each prime in numbers
    (define (mark-primes index)
      (when (<= index max)
        (when (vector-ref base-nums index)
          (vector-set! numbers (1+ (* 2 index)) #t))
        (mark-primes (1+ index))))

    ;; Main part.  Sundaram does not mark 2 as prime, so we do that manually.
    (when (> n 1)
      (vector-set! numbers 2 #t))
    (outer-loop 1)
    (mark-primes 1)
    numbers))

;; Given a vector where every prime index is true, print each prime and a total
;; of primes found.  For debugging.
(define (print-prime-list primes)
  (let ((l (vector-length primes))
        (num-primes 0))
    (do ((i 0 (1+ i)))
        ((>= i l))
      (when (vector-ref primes i)
        (begin
          (set! num-primes (1+ num-primes))
          (simple-format (current-output-port) "Prime: ~a\n" i))))
    (simple-format
     (current-output-port)
     "Total primes from 0 to ~a: ~a\n" (1- (vector-length primes)) num-primes)))


;; Given a vector where every prime index is true, return a list of primes.
(define (list-primes primes)
  (define (iterate-primes prime-list index max)
    (if (> index max)
        prime-list
        (if (vector-ref primes index)
            (iterate-primes (append prime-list (list index)) (1+ index) max)
            (iterate-primes prime-list (1+ index) max))))
  (iterate-primes '() 0 (1- (vector-length primes))))
