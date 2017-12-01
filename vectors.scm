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



(use-modules (oop goops))

(define π (* 4 (atan 1)))

(define-class <angle> ()
  (radians #:init-value 0 #:init-keyword #:radians #:getter get-radians)
  (degrees #:accessor degrees #:init-keyword #:degrees
           #:allocation #:virtual
           #:slot-ref (lambda (obj)
                        (let ((θ (slot-ref obj 'radians)))
                          (/ (* 360 θ) (* 2 π))))
           #:slot-set! (lambda (obj θ)
                         (slot-set! obj 'radians (/ (* 2 π θ) 360)))))

(define-syntax rad
  (syntax-rules ()
    ((rad θ)
     (make <angle> #:radians θ))))

(define-syntax °
  (syntax-rules ()
    ((° θ)
     (make <angle> #:degrees θ))))

(define-method (display (self <angle>) port)
  (format port "~a radians (~a°"
          (get-radians self) (degrees self)))

(define-method (write (self <angle>) port)
  (format port "#<angle> ~a radians (~a°)"
          (get-radians self) (degrees self)))

(define-method (+ (θ1 <angle>) (θ2 <angle>))
  (make <angle> #:radians (+ (get-radians θ1) (get-radians θ2))))

(define-method (- (θ1 <angle>) (θ2 <angle>))
  (make <angle> #:radians (- (get-radians θ1) (get-radians θ2))))

(define-method (* (θ1 <angle>) (θ2 <angle>))
  (make <angle> #:radians (* (get-radians θ1) (get-radians θ2))))

(define-method (/ (θ1 <angle>) (θ2 <angle>))
  (make <angle> #:radians (/ (get-radians θ1) (get-radians θ2))))

(define-method (< (θ1 <angle>) (θ2 <angle>))
  (< (get-radians θ1) (get-radians θ2)))

(define-method (> (θ1 <angle>) (θ2 <angle>))
  (> (get-radians θ1) (get-radians θ2)))

(define-method (= (θ1 <angle>) (θ2 <angle>))
  (= (get-radians θ1) (get-radians θ2)))

(define-method (<= (θ1 <angle>) (θ2 <angle>))
  (<= (get-radians θ1) (get-radians θ2)))

(define-method (>= (θ1 <angle>) (θ2 <angle>))
  (>= (get-radians θ1) (get-radians θ2)))

(define-method (+ (θ1 <number>) (θ2 <angle>))
  (make <angle> #:radians (+ θ1 (get-radians θ2))))

(define-method (- (θ1 <number>) (θ2 <angle>))
  (make <angle> #:radians (- θ1 (get-radians θ2))))

(define-method (* (θ1 <number>) (θ2 <angle>))
  (make <angle> #:radians (* θ1 (get-radians θ2))))

(define-method (/ (θ1 <number>) (θ2 <angle>))
  (make <angle> #:radians (/ θ1 (get-radians θ2))))

(define-method (< (θ1 <number>) (θ2 <angle>))
  (< θ1 (get-radians θ2)))

(define-method (> (θ1 <number>) (θ2 <angle>))
  (> θ1 (get-radians θ2)))

(define-method (= (θ1 <number>) (θ2 <angle>))
  (= θ1 (get-radians θ2)))

(define-method (<= (θ1 <number>) (θ2 <angle>))
  (<= θ1 (get-radians θ2)))

(define-method (>= (θ1 <number>) (θ2 <angle>))
  (>= θ1 (get-radians θ2)))

(define-method (+ (θ1 <angle>) (θ2 <number>))
  (make <angle> #:radians (+ (get-radians θ1) θ2)))

(define-method (- (θ1 <angle>) (θ2 <number>))
  (make <angle> #:radians (- (get-radians θ1) θ2)))

(define-method (* (θ1 <angle>) (θ2 <number>))
  (make <angle> #:radians (* (get-radians θ1) θ2)))

(define-method (/ (θ1 <angle>) (θ2 <number>))
  (make <angle> #:radians (/ (get-radians θ1) θ2)))

(define-method (< (θ1 <angle>) (θ2 <number>))
  (< (get-radians θ1) θ2))

(define-method (> (θ1 <angle>) (θ2 <number>))
  (> (get-radians θ1) θ2))

(define-method (= (θ1 <angle>) (θ2 <number>))
  (= (get-radians θ1) θ2))

(define-method (<= (θ1 <angle>) (θ2 <number>))
  (<= (get-radians θ1) θ2))

(define-method (>= (θ1 <angle>) (θ2 <number>))
  (>= (get-radians θ1) θ2))

(define-method (zero? (θ1 <angle>))
  (make <angle> #:radians (zero? (get-radians θ1))))

(define-method (positive? (θ1 <angle>))
  (make <angle> #:radians (positive? (get-radians θ1))))

(define-method (negative? (θ1 <angle>))
  (make <angle> #:radians (negative? (get-radians θ1))))

(define-method (abs (θ1 <angle>))
  (make <angle> #:radians (abs (get-radians θ1))))

(define-method (sin (θ1 <angle>))
  (make <angle> #:radians (sin (get-radians θ1))))

(define-method (cos (θ1 <angle>))
  (make <angle> #:radians (cos (get-radians θ1))))

(define-method (tan (θ1 <angle>))
  (make <angle> #:radians (tan (get-radians θ1))))

(define-method (sinh (θ1 <angle>))
  (make <angle> #:radians (sinh (get-radians θ1))))

(define-method (cosh (θ1 <angle>))
  (make <angle> #:radians (cosh (get-radians θ1))))

(define-method (tanh (θ1 <angle>))
  (make <angle> #:radians (tanh (get-radians θ1))))

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

(define-class <vec3> ()
  (x #:init-value 0 #:init-keyword #:x #:getter get-x)
  (y #:init-value 0 #:init-keyword #:y #:getter get-y)
  (z #:init-value 0 #:init-keyword #:z #:getter get-z))

(define-syntax vec2
  (syntax-rules ()
    ((vec2 x y)
     (make <vec2> #:x x #:y y))))

(define-syntax vec3
  (syntax-rules ()
    ((vec3 x y z)
     (make <vec3> #:x x #:y y #:z z))))

(define-syntax vec2-from-polar
  (syntax-rules ()
    ((vec2-from-polar m θ)
     (make <vec2> #:m m #:a θ))))

(define-method (write (self <vec2>) port)
  (format port "#<vec2> <~a, ~a> (~a, ~a)"
          (get-x self) (get-y self) (magnitude self) (angle self)))

(define-method (display (self <vec2>) port)
  (format port "<~a, ~a>"
          (get-x self) (get-y self)))

(define-method (write (self <vec3>) port)
  (format port "#<vec3> <~a, ~a, ~a>"
          (get-x self) (get-y self) (get-z self)))

(define-method (display (self <vec3>) port)
  (format port "<~a, ~a, ~a>"
          (get-x self) (get-y self) (get-z self)))

(define-method (+ (a <vec2>) (b <vec2>))
  (make <vec2>
    #:x (+ (get-x a) (get-x b))
    #:y (+ (get-y a) (get-y b))))

(define-method (+ (a <vec3>) (b <vec3>))
  (make <vec3>
    #:x (+ (get-x a) (get-x b))
    #:y (+ (get-y a) (get-y b))
    #:z (+ (get-z a) (get-z b))))

(define-method (- (a <vec3>) (b <vec3>))
  (make <vec3>
    #:x (- (get-x a) (get-x b))
    #:y (- (get-y a) (get-y b))
    #:z (- (get-z a) (get-z b))))

(define-method (* (a <vec2>) (c <number>))
  (make <vec2>
    #:x (* c (get-x a))
    #:y (* c (get-y a))))

(define-method (* (a <vec3>) (c <number>))
  (make <vec3>
    #:x (* c (get-x a))
    #:y (* c (get-y a))
    #:z (* c (get-z a))))

(define-method (· (a <vec2>) (b <vec2>))
  (+ (* (get-x a) (get-x b))
     (* (get-y a) (get-y b))))

(define-method (· (a <vec3>) (b <vec3>))
  (+ (* (get-x a) (get-x b))
     (* (get-y a) (get-y b))
     (* (get-z a) (get-z b))))

(define-method (× (a <vec3>) (b <vec3>))
  (make <vec3>
    #:x (- (* (get-z b) (get-y a))
           (* (get-y b) (get-z a)))
    #:y (- (- (* (get-z b) (get-x a))
              (* (get-x b) (get-z a))))
    #:z (- (* (get-y b) (get-x a))
           (* (get-x b) (get-y a)))))

(define-method (mag (u <vec2>))
  (sqrt (· u u)))

(define-method (mag (u <vec3>))
  (sqrt (· u u)))

(define-method (angle-between-vectors (u <vec2>) (v <vec2>))
  (acos (/ (· u v) (* (mag u) (mag v)))))

(define-method (angle-between-vectors (u <vec3>) (v <vec3>))
  (acos (/ (· u v) (* (mag u) (mag v)))))

(define-method (area-parallelogram (u <vec3>) (v <vec3>))
  (* (mag u) (mag v) (sin (angle-between-vectors u v))))

(define-method (area-parallelopipe (u <vec3>) (v <vec3>) (w <vec3>))
  (abs (· u (× v w))))



