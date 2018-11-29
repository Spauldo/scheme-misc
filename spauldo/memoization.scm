(define-module (spauldo memoization)
  #:export (make-memoized))

;; Takes a function and returns a memoized version of the function.
;; The function should always use arguments that can be compared with eq?,
;; such as numbers or symbols, and should not return lists.
(define (make-memoized fn)
  (let ((db '())            ; A tree of nested alists, using arguments as keys
	(lookup-success #f) ; Did we find our value?
	(return-val #f))    ; Value to return

    ;; Look up the value in the database.  Create entry if necessary.  Returns
    ;; the database.
    (define (lookup-value alist path args)
      (let ((entry (assq (car path) alist)))
	(cond

	 ;; An entry for this argument exists in the db, and there are
	 ;; more arguments
	 ((and entry
	       (pair? (cdr path)))
	  (let ((new-alist (lookup-value (cdr entry) (cdr path) args)))
	    (when (not lookup-success)
	      (assq-set! alist (car path) new-alist))
	    alist))

	 ;; An entry for this argument exists in the db, and there are no
	 ;; more arguments
	 (entry
	  (set! return-val (cdr entry))
	  (set! lookup-success #t)
	  alist)

	 ;; No entry exists for this argument, and there are more arguments
	 ((pair? (cdr path))
	  (set! alist (assq-set! alist
				 (car path)
				 (lookup-value '() (cdr path) args)))
	  alist)

	 ;; No entry exists for this argument, and there are no more arguments
	 (#t
	  (set! return-val (apply fn args))
	  (set! alist (assq-set! alist (car path) return-val))
	  alist))))

    ;; The function we return.
    ;; Note that we don't do any checking to make sure we have the correct
    ;; number of arguments.
    (lambda (arg1 . arglist)
      (set! lookup-success #f)
      (let* ((args (cons arg1 arglist))
	     (alist (lookup-value db args args)))
	(unless lookup-success
	  (set! db alist))
	return-val))))
