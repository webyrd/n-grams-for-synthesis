; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Exact complex arithmetic built on rational arithmetic

; from Scheme 48

; By structuring the complex numbers this way---instead of using just
; one representation using tuples of arbitrary reals---we avoid having
; to implement full generic arithmetic below the complex numbers, or
; having to resort to confusing recursion in the generic arithmetic.
; But suit yourself.

; Note that, unlike the COMPNUMS operations, these can return
; ratnums.

(define-record-type :recnum
  (make-recnum real imag)
  recnum?
  (real recnum-real)
  (imag recnum-imag))

(define-record-discloser :recnum
  (lambda (r)
    (list 'recnum
	  (recnum-real r)
	  (recnum-imag r))))

(define (r5rs->recnum n)
  (make-recnum (r5rs->integer (r5rs:real-part n))
	       (r5rs->integer (r5rs:imag-part n))))

(define (rectangulate x y)
  (if (rational= y (r5rs->integer 0))
      x
      (make-recnum x y)))

(define (recnum+ a b)
  (rectangulate (rational+ (recnum-real a) (recnum-real b))
		(rational+ (recnum-imag a) (recnum-imag b))))

(define (recnum- a b)
  (rectangulate (rational- (recnum-real a) (recnum-real b))
		(rational- (recnum-imag a) (recnum-imag b))))

(define (recnum* a b)
  (let ((a1 (recnum-real a))
	(a2 (recnum-imag a))
	(b1 (recnum-real b))
	(b2 (recnum-imag b)))
    (rectangulate (rational- (rational* a1 b1) (rational* a2 b2))
		  (rational+ (rational* a1 b2) (rational* a2 b1)))))

(define (recnum/ a b)
  (let ((a1 (recnum-real a))
	(a2 (recnum-imag a))
	(b1 (recnum-real b))
	(b2 (recnum-imag b)))
    (let ((d (rational+ (rational* b1 b1) (rational* b2 b2))))
      (rectangulate (rational/ (rational+ (rational* a1 b1) (rational* a2 b2)) d)
		    (rational/ (rational- (rational* a2 b1) (rational* a1 b2)) d)))))

(define (recnum= a b)
  (let ((a1 (recnum-real a))
	(a2 (recnum-imag a))
	(b1 (recnum-real b))
	(b2 (recnum-imag b)))
    (and (rational= a1 b1) (rational= a2 b2))))

(define (recnum->string r radix)
  (if (rational-negative? (recnum-imag r))
      (string-append (rational->string (recnum-real r) radix)
		     "-"
		     (rational->string (rational- (recnum-imag r)) radix)
		     "i")
      (string-append (rational->string (recnum-real r) radix)
		     "+"
		     (rational->string (recnum-imag r) radix)
		     "i")))
