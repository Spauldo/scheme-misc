;;; vector-math.scm
;;;
;;; Copyright (c) 2017 Jeff Spaulding <sarnet@gmail.com>
;;;
;;; This module contains functions related to vector math
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

(define-module (spauldo vector-math)
  #:use-module (oop goops))


;;; Two-value vectors and associated functions and methods
(define-class <vec2> ()
  (x #:init-value 0 #:init-keyword #:x #:getter get-x)
  (y #:init-value 0 #:init-keyword #:y #:getter get-y)
  (m #:accessor magnitude #:init-keyword #:magnitude
     #:allocation #:virtual
     #:slot-ref (lambda (obj)
                  (let ((x (slot-ref obj 'x))
                        (y (slot-ref obj 'y)))
                    (sqrt (+ (* x x) (* y y)))))
     #:slot-set! (lambda (obj r)
                   (let ((θ (slot-ref obj 'a)))
                     (slot-set! obj 'x (* r (cos θ)))
                     (slot-set! obj 'y (* r (sin θ))))))
  (a #:accessor angle #:init-keyword #:angle
     #:allocation #:virtual
     #:slot-ref (lambda (obj)
                  (let ((x (slot-ref obj 'x))
                        (y (slot-ref obj 'y)))
                    (atan (slot-ref obj 'y) (slot-ref obj 'x))))
     #:slot-set! (lambda (obj θ)
                   (let ((r (slot-ref obj 'm)))
                     (slot-set! obj 'x (* r (cos θ)))
                     (slot-set! obj 'y (* r (sin θ)))))))

;; Normal constructor
(define-syntax vec2
  (syntax-rules ()
    ((vec2 x y)
     (make <vec2> #:x x #:y y))))

;; Polar constructor
(define-syntax vec2-from-polar
  (syntax-rules ()
    ((vec2-from-polar m θ)
     (make <vec2> #:m m #:a θ))))

;; Output
(define-method (write (self <vec2>) port)
  (format port "#<vec2> <~a, ~a> (~a, ~a)"
          (get-x self) (get-y self) (magnitude self) (angle self)))

(define-method (display (self <vec2>) port)
  (format port "<~a, ~a>"
          (get-x self) (get-y self)))

;; Vector addition and subtraction
(define-method (+ (a <vec2>) (b <vec2>))
  (make <vec2>
    #:x (+ (get-x a) (get-x b))
    #:y (+ (get-y a) (get-y b))))

(define-method (- (a <vec2>) (b <vec2>))
  (make <vec2>
    #:x (- (get-x a) (get-x b))
    #:y (- (get-y a) (get-y b))))

;; Scalar multiplication
(define-method (* (a <vec2>) (c <number>))
  (make <vec2>
    #:x (* c (get-x a))
    #:y (* c (get-x b))))

(define-method (* (c <number>) (a <vec2>))
  (* a c))

;; Dot product (in TeX: \cdot)
(define-method (· (a <vec2>) (b <vec2>))
  (+ (* (get-x a) (get-x b))
     (* (get-y a) (get-y b))))

(define-method (dot-product (a <vec2>) (b <vec2>))
  (· a b))



;;; Three-value vectors and associated functions and methods
(define-class <vec3> ()
  (x #:init-value 0 #:init-keyword #:x #:getter get-x)
  (y #:init-value 0 #:init-keyword #:y #:getter get-y)
  (z #:init-value 0 #:init-keyword #:z #:getter get-z)
  (m #:accessor magnitude #:init-keyword #:magnitude
     #:allocation #:virtual
     #:slot-ref (lambda (obj)
                  (let ((x (slot-ref obj 'x))
                        (y (slot-ref obj 'y))
                        (z (slot-ref obj 'z)))
                    (sqrt (+ (* x x) (* y y) (* z z))))))
  (α #:accessor alpha #:init-keyword #:alpha
      #:allocation #:virtual
      #:slot-ref (lambda (obj)
                   (let ((x (slot-ref 'x))
                         (m (slot-ref 'm)))
                     (/ x m))))
  (β #:accessor beta #:init-keyword #:beta
      #:allocation #:virtual
      #:slot-ref (lambda (obj)
                   (let ((y (slot-ref 'y))
                         (m (slot-ref 'm)))
                     (/ y m))))
  (γ #:accessor gamma #:init-keyword #:gamma
      #:allocation #:virtual
      #:slot-ref (lambda (obj)
                   (let ((z (slot-ref 'z))
                         (m (slot-ref 'm)))
                     (/ z m)))))

;; Normal constructor
(define-syntax vec3
  (syntax-rules ()
    ((vec3 x y z)
     (make <vec3> #:x x #:y y #:z z))))

;; Output
(define-method (write (self <vec3>) port)
  (format port "#<vec3> <~a, ~a, ~a> ||v||: ~a α: ~a β: ~a γ: ~a"
          (get-x self) (get-y self) (get-z self) (magnitude self)
          (alpha self) (beta self) (gamma self)))

(define-method (display (self <vec3>) port)
  (format port "<~a, ~a, ~a>"
          (get-x self), (get-y self) (get-z self)))

;; Dot product (in TeX: \cdot)
(define-method (· (a <vec3>) (b <vec3>))
  (+ (* (get-x a) (get-x b))
     (* (get-y a) (get-y b))
     (* (get-z a) (get-z b))))

(define-method (dot-product (a <vec3>) (b <vec3))
  (· a b))

;; Cross product (in TeX: \times)
(define-method (× (a <vec3>) (b <vec3>))
  (make <vec3>
    #:x (- (* (get-z b) (get-y a))
           (* (get-y b) (get-z a)))
    #:y (- (* (get-z b) (get-x a))
           (* (get-x b) (get-z a)))
    #:z (- (* (get-y b) (get-x a))
           (* (get-x b) (get-y a)))))

(define-method (cross-product (a <vec3>) (b <vec3>))
  (× a b))
