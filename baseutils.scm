;;; baseutils.scm --- Utilities for base conversion
;;;
;;; Contents:
;;; required-digits: Calculate the number of digits in base 'b2' required to
;;;                  represent a number in 'b1' with 'power' digits
;;;
;;; convert-int-between-bases: Convert the string representation of an integer
;;;                            in one base to another base.
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


(use-modules (ice-9 regex)
             (srfi srfi-43))

;;; How many digits in base b2 are required to represent power digits in b1?
;;; Call with ceiling and inexact->exact to get an integer.
(define (required-digits b1 power b2)
  (/ (log10 (expt b1 power))
     (log10 b2)))

(define base-conv-regex
  (make-regexp "^[0-9A-Za-z]+$"))

(define digit-chars
  (vector #\0 #\1 #\2 #\3 #\4 #\5
          #\6 #\7 #\8 #\9 #\A #\B
          #\C #\D #\E #\F #\G #\H
          #\I #\J #\K #\L #\M #\N
          #\O #\P #\Q #\R #\S #\T
          #\U #\V #\W #\X #\Y #\Z))

(define (convert-int-between-bases b1 nstr b2)
  ;; Return digits up to base 37
  (define (digit d)
    (vector-ref digit-chars d))

  ;; Return the base 10 representation of a digit character
  (define (char-value c)
    (vector-index (lambda (ch) (equal? (char-upcase c) ch)) digit-chars))

  ;; Finds the highest power of b that is less than n
  (define (find-highest-power-within-n b n p)
    (if (> (expt b p) n)
        (1- p)
        (find-highest-power-within-n b n (1+ p))))

  ;; Create a plist of exponents in base b and multipliers.
  ;; Pass the highest power of b that is less than n for power, and
  ;; the empty list for explist.
  (define (make-exp-list b n power explist)
    (if (< power 0)
        explist
        (call-with-values (lambda () (euclidean/ n (expt b power)))
          (lambda (q r)
            (make-exp-list b r (1- power) (append explist (list power q)))))))

  ;; Convert a number in base b to base 10
  (define (convert-to-base10 b nstr)
    (if (= b 10)
        (string->number nstr)
        (let ((power (string-length nstr)))
          (string-fold (lambda (c n)
                         (set! power (1- power))
                         (+ n (* (char-value c) (expt b power))))
                       0 nstr))))

  ;; Create a string representing a number in base b.
  ;; Pass a plist in the format of make-exp-list for explist, and the empty list
  ;; for digits.
  (define (explist-to-string b explist digits)
    (if (null? explist)
        (list->string digits)
        (explist-to-string b (cddr explist)
                           (append digits
                                   (list (digit (cadr explist)))))))

  ;; Finally, do something.
  (cond ((not (integer? b1))
         (scm-error 'wrong-type-arg convert-int-between-bases
                    "Wrong type argument in position 1: ~S."
                    '(b1) b1))
        ((not (string? nstr))
         (scm-error 'wrong-type-arg convert-int-between-bases
                    "Wrong type argument in position 2: ~S."
                    '(n) nstr))
        ((not (integer? b2))
         (scm-error 'wrong-type-arg convert-int-between-bases
                    "Wrong type argument in position 3: ~S."
                    '(b2) b2))
        ((or (< b1 2)
             (> b1 37))
         (scm-error 'out-of-range convert-int-between-bases
                    "Value out of range 2 to 37 in position 1: ~S"
                    '(2 b1) b1))
        ((not (regexp-exec base-conv-regex nstr))
         (scm-error 'misc-error convert-int-between-bases
                    "Invalid characters in string in position 2: ~S"
                    '(nstr) #f))
        ((or (< b2 2)
             (> b2 37))
         (scm-error 'out-of-range convert-int-between-bases
                    "Value out of range 2 to 37 in position 3: ~S"
                    '(2 b2) b2))
        (else
         (let ((n10 (convert-to-base10 b1 nstr)))
           (explist-to-string
            b2
            (make-exp-list b2 n10 (find-highest-power-within-n b2 n10 0) '())
            '())))))
