;;; Scheme Underground list-set library				-*- Scheme -*-
;;;
;;; Copyright (c) 1998 by Olin Shivers. You may do as you please with
;;; this code as long as you do not remove this copyright notice or
;;; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.
;;;     -Olin

;;; SRFI DRAFT -- SRFI DRAFT -- SRFI DRAFT -- SRFI DRAFT -- SRFI DRAFT
;;; This is *draft* code for a SRFI proposal. If you see this notice in 
;;; production code, you've got obsolete, bad source -- go find the final 
;;; non-draft code on the Net.
;;; SRFI DRAFT -- SRFI DRAFT -- SRFI DRAFT -- SRFI DRAFT -- SRFI DRAFT

;;; Exported:
;;; lset=  = list1 list2 ... -> boolean
;;; lset<= = list1 list2 ... -> boolean
;;;
;;; lset-adjoin = list elt1 ...
;;; lset-union = list1 ...
;;; lset-intersection = list1 list2 ...
;;; lset-difference = list1 list2 ...
;;; lset-xor = list1 ...
;;; lset-diff+intersection = list1 list2 ...
;;; ... and their side effecting counterparts:
;;;   lset-union! lset-intersection! lset-difference! lset-xor! 
;;;   lset-diff+intersection!
;;;
;;; adjoin {,q,v}: list elt1 ...
;;; union {,q,v} {,!}: list1 ...
;;; intersection {,q,v} {,!}: list1 list2 ...
;;; list-difference {,q,v} {,!}: list1 list2 ...
;;; list-xor {,q,v} {,!}: list1 ...
;;; diff+intersection {,q,v} {,!}: list1 list2 ...

;;; This is carefully tuned code; do not modify casually.
;;; - It is careful to share storage when possible;
;;; - Side-effecting code tries not to perform redundant writes.
;;; - It tries to avoid linear-time scans in special cases where constant-time
;;;   computations can be performed.
;;; - It relies on similar properties from the list-lib code it calls.

;;; If there's a way to mark these procedures as candidates for integration,
;;; they should be so marked. It would also be nice if the n-ary ops could
;;; be unfolded by the compiler into a chain of binary-op applications. But
;;; this kind of compiler technology no longer exists in the Scheme world.

;;; This implementation is intended as a portable reference implementation
;;; of my list-set package. With three exceptions, it is completely R4RS:
;;; - The (RECEIVE (var ...) mv-exp body ...) multi-value-return binding macro
;;; - The procedures defined in my list-lib package, for which there is also
;;;   a portable reference implementation.
;;;   Many calls to a parameter-checking procedure check-arg:
;;;    (define (check-arg pred val caller)
;;;      (let lp ((val val))
;;;        (if (pred val) val (lp (error "Bad argument" val pred caller)))))

;;; Note that this code is, of course, dependent upon standard bindings for
;;; the R5RS procedures -- i.e., it assumes that the variable CAR is bound
;;; to the procedure that takes the car of a list. If your Scheme 
;;; implementation allows user code to alter the bindings of these procedures
;;; in a manner that would be visible to these definitions, then there might
;;; be trouble. You could consider horrible kludgery along the lines of
;;;    (define fact 
;;;      (let ((= =) (- -) (* *))
;;;        (letrec ((real-fact (lambda (n) 
;;;                              (if (= n 0) 1 (* n (real-fact (- n 1)))))))
;;;          real-fact)))
;;; Or you could consider shifting to a reasonable Scheme system that, say,
;;; has a module system protecting code from this kind of lossage.
;;;
;;; This code does a fair amount of run-time argument checking. If your
;;; Scheme system has a sophisticated compiler that can eliminate redundant
;;; error checks, this is no problem. However, if not, these checks incur
;;; some performance overhead -- and, in a safe Scheme implementation, they
;;; are in some sense redundant: if we don't check to see that the PROC 
;;; parameter is a procedure, we'll find out anyway three lines later when
;;; we try to call the value. It's pretty easy to rip all this argument 
;;; checking code out if it's inappropriate for your implementation -- just
;;; nuke every call to CHECK-ARG.
;;;
;;; On the other hand, if you *do* have a sophisticated compiler that will
;;; actually perform soft-typing and eliminate redundant checks (Rice being
;;; the only possible candidate of which I'm aware), leaving these checks 
;;; in can *help*, since their presence can be elided in redundant cases,
;;; and in cases where they are needed, performing the checks early, at
;;; procedure entry, can "lift" a check out of a loop. 
;;;
;;; Finally, I have only checked the properties that can portably be checked
;;; with R5RS Scheme -- and this is not complete. You may wish to alter
;;; the CHECK-ARG parameter checks to perform extra, implementation-specific
;;; checks, such as procedure arity for higher-order values.
;;;

;;; First the procedures that take a comparison function as an argument.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (%lset2<= = lis1 lis2) (every (lambda (x) (mem = x lis2)) lis1))

(define (lset<= = lis1 . lists)
  (check-arg = procedure? lset<=)
  (let lp ((s1 lis1) (rest lists))
    (or (not (pair? rest))
	(let ((s2 (car rest))  (rest (cdr rest)))
	  (and (or (eq? s2 s1)		; Fast path
		   (%lset2<= = s1 s2))	; Real test
	       (lp s2 rest))))))

(define (lset= = lis1 . lists)
  (check-arg = procedure? lset=)
  (let lp ((s1 lis1) (rest lists))
    (or (not (pair? rest))
	(let ((s2 (car rest))
	      (rest (cdr rest)))
	  (and (or (eq? s1 s2)					; Fast path
		   (and (%lset2<= = s1 s2) (%lset2<= = s2 s1)))	; Real test
	       (lp s2 rest))))))

(define (lset-adjoin = lis . elts)
  (check-arg = procedure? lset-adjoin)
  (foldl (lambda (elt ans) (if (mem = elt ans) ans (cons elt ans)))
	 lis elts))


(define (lset-union = . lists)
  (check-arg = procedure? lset-union)
  (if (pair? lists)
      (foldl (lambda (lis ans)	; Compute LIS + ANS.
	       (if (pair? ans)
		   (foldl (lambda (elt ans) (if (mem = elt ans) ans
						(cons elt ans)))
			  ans lis)
		   lis))	; Don't copy LIS if you don't have to.
	     (car lists)
	     (cdr lists))
      '()))

(define (lset-union! = . lists)
  (check-arg = procedure? lset-union!)
  (if (pair? lists)
      (foldl (lambda (lis ans)	; Splice new elts of LIS onto the front of ANS.
	       (if (pair? ans)
		   (pair-foldl (lambda (pair ans)
				 (if (mem = (car pair) ans) ans
				     (begin (set-cdr! pair ans) pair)))
			       ans lis)
		   lis)) ; Don't scan LIS if you don't have to.
	     (car lists)
	     (cdr lists))
      '()))


(define (lset-intersection = lis1 . lists)
  (check-arg = procedure? lset-intersection)
  (if (every pair? lists)
      (foldl (lambda (lis residue) (filter (lambda (x) (mem = x lis)) residue))
	     lis1
	     lists)
      '())) ; Don't do unnecessary scans of LIS1/RESIDUE.

(define (lset-intersection! = lis1 . lists)
  (check-arg = procedure? lset-intersection!)
  (if (every pair? lists)
      (foldl (lambda (lis residue) (filter! (lambda (x) (mem = x lis)) residue))
	     lis1
	     lists)
      '())) ; Don't do unnecessary scans of LIS1/RESIDUE.


(define (lset-difference = lis1 . lists)
  (check-arg = procedure? lset-difference)
  (foldl (lambda (lis residue)
	   (if (pair? lis)
	       (filter (lambda (x) (not (mem = x lis))) residue)
	       residue)) ; Don't do unnecessary scans of RESIDUE.
	 lis1
	 lists))

(define (lset-difference! = lis1 . lists)
  (check-arg = procedure? lset-difference!)
  (foldl (lambda (lis residue)
	   (if (pair? lis)
	       (filter! (lambda (x) (not (mem = x lis))) residue)
	       residue)) ; Don't do unnecessary scans of RESIDUE.
	 lis1
	 lists))


(define (lset-xor = . lists)
  (check-arg = procedure? lset-xor)
  (if (pair? lists)	; We don't really have to do this test, but WTF.
      (foldl (lambda (a b)			; Compute A xor B:
	       (if (pair? a)
		   ;; Compute a-b and a^b, then compute b-(a^b) and
		   ;; cons it onto the front of a-b.
		   (receive (a-b a-int-b) (lset-diff+intersection = a b)
		     (foldl (lambda (xb ans)
			      (if (mem = xb a-int-b) ans (cons xb ans)))
			    a-b
			    b))
		   b)) ; Don't copy B if we don't have to.
	     (car lists)
	     (cdr lists)) 
      '()))

(define (lset-xor! = . lists)
  (check-arg = procedure? lset-xor!)
  (if (pair? lists)	; We don't really have to do this test, but WTF.
      (foldl (lambda (a b)			; Compute A xor B:
	       (if (pair? a)
		   ;; Compute a-b and a^b, then compute b-(a^b) and
		   ;; cons it onto the front of a-b.
		   (receive (a-b a-int-b) (lset-diff+intersection! = a b)
		     (pair-foldl (lambda (b-pair ans)
				   (if (mem = (car b-pair) a-int-b) ans
				       (begin (set-cdr! b-pair ans) b-pair)))
				 a-b
				 b))
		   b)) ; Don't copy B if we don't have to.
	     (car lists)
	     (cdr lists)) 
      '()))


(define (lset-diff+intersection = lis1 . lists)
  (check-arg = procedure? lset-diff+intersection)
  (if (any pair? lists)
      (partition (lambda (elt) (not (any (lambda (lis) (mem = elt lis))
					 lists)))
		 lis1)
      (values lis1 '())))	; Don't scan LIS1 if we don't have to.

(define (lset-diff+intersection! = lis1 . lists)
  (check-arg = procedure? lset-diff+intersection!)
  (if (any pair? lists)
      (partition! (lambda (elt) (not (any (lambda (lis) (mem = elt lis))
					  lists)))
		  lis1)
      (values lis1 '())))	; Don't scan LIS1 if we don't have to.



;;; These are the variants with the built-in comparison functions
;;; (EQUAL?, EQ?, EQV?).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (adjoin  list . elts) (apply lset-adjoin equal? list elts))
(define (adjoinq list . elts) (apply lset-adjoin eq?    list elts))
(define (adjoinv list . elts) (apply lset-adjoin eqv?   list elts))

(define (union  . lists) (apply lset-union equal? lists))
(define (unionq . lists) (apply lset-union eq?    lists))
(define (unionv . lists) (apply lset-union eqv?   lists))

(define (union!  . lists) (apply lset-union! equal? lists))
(define (unionq! . lists) (apply lset-union! eq?    lists))
(define (unionv! . lists) (apply lset-union! eqv?   lists))

(define (intersection  lis1 . lists) (apply lset-intersection equal? lis1 lists))
(define (intersectionq lis1 . lists) (apply lset-intersection eq?    lis1 lists))
(define (intersectionv lis1 . lists) (apply lset-intersection eqv?   lis1 lists))
	 
(define (intersection!  lis1 . lists) (apply lset-intersection! equal? lis1 lists))
(define (intersectionq! lis1 . lists) (apply lset-intersection! eq?    lis1 lists))
(define (intersectionv! lis1 . lists) (apply lset-intersection! eqv?   lis1 lists))

(define (list-difference  lis1 . lists) (apply lset-difference equal? lis1 lists))
(define (list-differenceq lis1 . lists) (apply lset-difference eq?    lis1 lists))
(define (list-differencev lis1 . lists) (apply lset-difference eqv?   lis1 lists))

(define (list-difference!  lis1 . lists) (apply lset-difference! equal? lis1 lists))
(define (list-differenceq! lis1 . lists) (apply lset-difference! eq?    lis1 lists))
(define (list-differencev! lis1 . lists) (apply lset-difference! eqv?   lis1 lists))

(define (list-xor  . lists) (apply lset-xor equal? lists))
(define (list-xorq . lists) (apply lset-xor eq?    lists))
(define (list-xorv . lists) (apply lset-xor eqv?   lists))

(define (list-xor!  . lists) (apply lset-xor! equal? lists))
(define (list-xorq! . lists) (apply lset-xor! eq?    lists))
(define (list-xorv! . lists) (apply lset-xor! eqv?   lists))

(define (diff+intersection  lis1 . lists)
  (apply lset-diff+intersection equal? lis1 lists))
(define (diff+intersectionq lis1 . lists)
  (apply lset-diff+intersection eq?    lis1 lists))
(define (diff+intersectionv lis1 . lists)
  (apply lset-diff+intersection eqv?   lis1 lists))
	 
(define (diff+intersection!  lis1 . lists)
  (apply lset-diff+intersection! equal? lis1 lists))
(define (diff+intersectionq! lis1 . lists)
  (apply lset-diff+intersection! eq?    lis1 lists))
(define (diff+intersectionv! lis1 . lists)
  (apply lset-diff+intersection! eqv?   lis1 lists))
