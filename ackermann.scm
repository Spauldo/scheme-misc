;;; ackermann.scm
;;;
;;; An implementation of Ackermann's function in MIT Scheme
;;;
;;; Copyright (c) 2015 Jeff Spaulding <sarnet@gmail.com>
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

(load-option 'format)

(define (ackermann m n p)
  (cond
   ((eq? p 0)
    (+ m n))
   ((and (eq? n 0)
	 (eq? p 1))
    0)
   ((and (eq? n 0)
	 (eq? p 2))
    1)
   ((eq? n 0)
    m)
   (else
    (begin (display (format #f "m: ~A n: ~A p: ~A~%" m n p))
	   (ackermann m (ackermann m (- n 1) p) (- p 1))))))

(define (ackermann2 m n)
  (cond
   ((eq? m 0)
    (+ n 1))
   ((and (> m 0)
	 (eq? n 0))
    (ackermann2 (- m 1) 1))
   ((and (> m 0)
	 (> n 0))
    (begin
      (display (format #f "m: ~A n: ~A~%" m n))
      (ackermann2 (- m 1) (ackermann2 m (- n 1)))))))

(define (A m n)
  (cond
   ((= m 0)
    (+ n 1))
   ((= n 0)
    (A (- m 1) 1))
   (else
    (begin
      (display (format #f "m: ~A n: ~A~%" m n))
      (A (- m 1) (A m (- n 1)))))))
