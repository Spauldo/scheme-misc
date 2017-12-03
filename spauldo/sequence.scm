(define-module (spauldo sequence)
  #:use-module (ice-9 vlist)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:export (<mseq> make-mseq mseq-ref))

;;; THIS IS NOT THREAD SAFE!
;;; This uses VLists, which are not thread safe.  TODO: wrap vlist-cons in
;;; a mutex or something.

;;; There is memoization functionality in libguile, but it's undocumented. I
;;; don't want to rely on something that might change out from under me.

;;; Memoized sequence class
(define-class <mseq> ()
  ;; Getting the length of a VList is a non-time-constant operation.
  ;; We keep track of the length here to speed things up.  Don't change this
  ;; value in user code!  Imagine it's private, if such a thing existed.
  (mseq-memoized-up-to #:init-value -1
                         #:getter mseq-get-memoized-up-to
                         #:setter mseq-set-memoized-up-to!)
  ;; The function to memoize
  (mseq-func #:init-keyword #:func
                  #:getter mseq-get-func)
  ;; Is the function recursive?
  (mseq-recursive #:init-keyword #:recursive
                  #:getter mseq-recursive?)
  ;; The starting point of the sequence (must be >= 0)
  (mseq-start #:init-value 0 #:init-keyword #:start
                   #:getter mseq-start)
  ;; Our VList
  (mseq-memoized-values #:init-value vlist-null))

;;; Create a memoized sequence from a function.

;;; For non-recursive sequences, the function must take exactly one argument:
;;; the position in the sequence to retrieve.

;;; For recursive sequences, the function must take exactly two arguments:
;;; the position in the sequence to retrieve, and a reference to the mseq that
;;; the function belongs to.  The function can look up previous values by making
;;; calls to mseq-ref.

;;; I will include examples of both.
(define* (make-mseq func #:optional starting-point recursive)
  ;; A function to recursively fill the entries before starting-point with
  ;; NaN values.
  (define (fill-undefined seq n)
          (if (< n 0)
              seq
              (fill-undefined
               (slot-set! seq 'mseq-memoized-values
                          (vlist-cons (nan)
                                      (slot-ref seq 'mseq-memoized-values)))
               (1- n))))
  (when (not (procedure? func))
    (scm-error 'wrong-type-argument 'make-mseq
                 "Not a function: ~a"
                 func #f))
  (when (and starting-point (< starting-point 0))
    (scm-error 'out-of-range 'make-mseq
               "Starting point of sequence cannot be negative: ~a"
               (list starting-point) starting-point))
  (when (not (boolean? recursive))
    (scm-error 'wrong-type-argument 'make-mseq
               "Argument 'recursive' must be a boolean: ~a"
               recursive #f))
  (if starting-point
      ;; We fill the entries of the VList before the starting point with NaN
      (fill-undefined (make <mseq> #:func func
                                   #:start starting-point
                                   #:recursive recursive)
                      (1- starting-point))
      (make <mseq> #:func func #:recursive recursive)))

;;; Access a memoized sequence
;;; Check thread safety
(define-generic mseq-ref)

(define-method (mseq-ref (seq <mseq>) (index <integer>))
  (if (> index (mseq-get-memoized-up-to seq))
      (begin
        (mseq-set-memoized-up-to! seq (1+ (mseq-get-memoized-up-to seq)))
        (if (mseq-recursive? seq)
            (slot-set! seq 'mseq-memoized-values
                       (vlist-cons
                        ((mseq-get-func seq) (mseq-get-memoized-up-to seq) seq)
                        (slot-ref seq 'mseq-memoized-values)))
            (slot-set! seq 'mseq-memoized-values
                       (vlist-cons
                        ((mseq-get-func seq) (mseq-get-memoized-up-to seq))
                        (slot-ref seq 'mseq-memoized-values))))
       (mseq-ref seq index))
      (vlist-ref (slot-ref seq 'mseq-memoized-values)
                 (- (mseq-get-memoized-up-to seq) index))))


;;; --------------------------------------------------------------------------
;;; Some sequence functions to test with

;;; The sequence of integers starting from zero
(define (mseq-integers-function n)
  n)

(define mseq-integers-seq
  (make-mseq mseq-integers-function))

;;; The sequence of integers starting from one
(define mseq-integers-seq-from-1
  (make-mseq mseq-integers-function 1))

;;; BROKEN!  FIX!
;;; Recursive series of factorials
;;; NOTE: mseq-ref is tail-recursive by nature, which means we can do this
;;; without blowing up the stack on recursive calls.
(define (mseq-factorial-function n seq)
  (if (< n 2)
      1
      (* (mseq-ref seq (1- n)) n)))

(define mseq-factorial-seq
  (make-mseq mseq-factorial-function 0 #t))

;;; The Taylor series of Euler's number, which uses another mseq sequence
(define (mseq-eulers-number-function n)
  (/ 1 (mseq-ref mseq-factorial-seq n)))

(define mseq-eulers-number-seq
  (make-mseq mseq-eulers-number-function))
