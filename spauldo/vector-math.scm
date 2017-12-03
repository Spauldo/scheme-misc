(define-module (spauldo vector-math)
  #:use-module (oop goops)
  #:use-module (srfi srfi-43)
  #:export (· ×
              handedness use-left-handed-coords! use-right-handed-coords!
              normalize endpoint-distance magnitude projection
              rationalize-vector)
  #:re-export (+ - * /)
  #:duplicates (merge-generics))


;;; Vector Operations

(define-generic ·)

;; Vector Addition and Subtraction
(define-method (+ (v <vector>) (w <vector>))
  (vector-map (lambda (elt1 elt2) (+ elt1 elt2)) v w))

(define-method (- (v <vector>) (w <vector>))
  (vector-map (lambda (elt1 elt2) (- elt1 elt2)) v w))

;; Scalar Multiplication
(define-method (* (v <vector>) (s <number>))
  (vector-map (lambda (i elt) (* elt s)) v))

(define-method (* (s <number>) (v <vector>))
  (vector-map (lambda (i elt) (* elt s)) v))

;; Scalar Division
(define-method (/ (v <vector>) (s <number>))
  (vector-map (lambda (i elt) (/ elt s)) v))

(define-method (/ (s <number>) (v <vector>))
  (vector-map (lambda (i elt) (/ s elt)) v))

;; Dot Product (TeX input \cdot)
(define-method (· (v <vector>) (w <vector>))
  (if (= (vector-length v) (vector-length w))
      (vector-fold (lambda (i sum elt1 elt2)
                     (+ sum (* elt1 elt2)))
                   0 v w)
      (scm-error 'wrong-type-arg '·
                 "Dot product: mismatched vector sizes: ~a ~a"
                 (list v w) #f)))


;;; Vector Misc

(define-generic magnitude)
(define-generic endpoint-distance)
(define-generic normalize)
(define-generic angle-between)
(define-generic projection)
(define-generic rationalize-vector)

;; Distance between the origin and the endpoint of the vector
(define-method (magnitude (v <vector>))
  (sqrt (vector-fold (lambda (i sum elt) (+ sum (* elt elt))) 0 v)))

;; Distance between the endpoints of two vectors
(define-method (endpoint-distance (v <vector>) (w <vector>))
  (sqrt (vector-fold (lambda (i sum elt1 elt2)
                       (+ sum (expt (- elt1 elt2) 2)))
                     0 v w)))

;; Returns a unit vector in the direction of v
(define-method (normalize (v <vector>))
  (* v (/ 1 (magnitude v))))

;; Returns the angle between two vectors in radians
(define-method (angle-between (v <vector>) (w <vector>))
  (acos (/ (· v w) (* (magnitude v) (magnitude w)))))

;; Returns the projection of vector v onto vector w
(define-method (projection (v <vector>) (w <vector>))
  (* w (/ (· v w) (expt (magnitude w) 2))))

;; Converts values in a vector to within ε of the nearest rational number
;; This IGNORES COMPLEX VALUES.  Scheme reader syntax doesn't have rational
;; complex numbers, so I didn't bother trying to force it.
(define-method (rationalize-vector (v <vector>) (ε <number>))
  (vector-map (lambda (i elt)
                (if (real? elt)
                    (rationalize (inexact->exact elt) ε)
                    elt))
              v))

;;; Cross-Product Related
;;; NOTE: Things in this subsection are only for 3D vectors.

;;; I have only included this because it's common, but I don't need it so I
;;; haven't tested it completely.

(define-generic ×)

;; Use Right-Hand Rule by Default
(define handedness 'right)

(define (use-left-handed-coords!)
  (set! handedness 'left))

(define (use-right-handed-coords!)
  (set! handedness 'right))

;; Cross Product for 3D Vectors
(define-method (× (v <vector>) (w <vector>))
  (if (and (= 3 (vector-length v))
           (= 3 (vector-length w)))
      (let ((cp (vector
              (- (* (vector-ref v 1) (vector-ref w 2))
                 (* (vector-ref v 2) (vector-ref w 1)))
              (- (* (vector-ref v 2) (vector-ref w 0))
                 (* (vector-ref v 0) (vector-ref w 2)))
              (- (* (vector-ref v 0) (vector-ref w 1))
                 (* (vector-ref v 1) (vector-ref w 0))))))
        (if (equal? handedness 'right)
            cp
            (vector-map (lambda (i elt) (- elt)) cp)))
      (scm-error 'wrong-type-arg '×
                 "Cross product: both vectors must be dimension 3: ~a ~a"
                 (list v w) #f)))
