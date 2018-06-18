; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Complex arithmetic built on flonum arithmetic

; In theory, a Scheme system might unbox the two flonums inside;
; Larceny actually does this.

; By structuring the complex numbers this way---instead of using just
; one representation using tuples of arbitrary reals---we avoid having
; to implement full generic arithmetic below the complex numbers, or
; having to resort to confusing recursion in the generic arithmetic.
; But suit yourself.

(define-record-type :compnum
  (make-compnum real imag)
  compnum?
  (real compnum-real)
  (imag compnum-imag))

(define-record-discloser :compnum
  (lambda (r)
    (list 'compnum
	  (compnum-real r)
	  (compnum-imag r))))

(define compnum make-compnum)

(define (make-compnum-polar a b)
  (make-compnum-polar (fl* a (flcos b)) (fl* a (flsin b))))

(define (r5rs->compnum n)
  (make-compnum (r5rs->flonum (r5rs:real-part n))
		(r5rs->flonum (r5rs:imag-part n))))

(define (compnum+ a b)
  (make-compnum (fl+ (compnum-real a) (compnum-real b))
		(fl+ (compnum-imag a) (compnum-imag b))))

(define (compnum- a b)
  (make-compnum (fl- (compnum-real a) (compnum-real b))
		(fl- (compnum-imag a) (compnum-imag b))))

(define (compnum* a b)
  (let ((a1 (compnum-real a))
	(a2 (compnum-imag a))
	(b1 (compnum-real b))
	(b2 (compnum-imag b)))
    (make-compnum (fl- (fl* a1 b1) (fl* a2 b2))
		  (fl+ (fl* a1 b2) (fl* a2 b1)))))

(define (compnum/ a b)
  (let ((a1 (compnum-real a))
	(a2 (compnum-imag a))
	(b1 (compnum-real b))
	(b2 (compnum-imag b)))
    (let ((d (fl+ (fl* b1 b1) (fl* b2 b2))))
      (make-compnum (fl/ (fl+ (fl* a1 b1) (fl* a2 b2)) d)
		    (fl/ (fl- (fl* a2 b1) (fl* a1 b2)) d)))))

(define (compnum= a b)
  (let ((a1 (compnum-real a))
	(a2 (compnum-imag a))
	(b1 (compnum-real b))
	(b2 (compnum-imag b)))
    (and (fl= a1 b1) (fl= a2 b2))))

(define (compnum-zero? a)
  (and (flzero? (compnum-real a))
       (flzero? (compnum-imag a))))

(define plus-i (make-compnum (r5rs->flonum 0.0) (r5rs->flonum 1.0)))
(define plus-2i (make-compnum (r5rs->flonum 0.0) (r5rs->flonum 2.0)))
(define minus-i (make-compnum (r5rs->flonum 0.0) (r5rs->flonum -1.0)))
(define compnum-zero (make-compnum (r5rs->flonum 0.0) (r5rs->flonum 0.0)))
(define compnum-one (make-compnum (r5rs->flonum 1.0) (r5rs->flonum 0.0)))
(define compnum-two (make-compnum (r5rs->flonum 2.0) (r5rs->flonum 0.0)))
(define compnum-inf+ (make-compnum flinf+ (r5rs->flonum 0.0)))

(define (compnum-angle z)
  (flatan (compnum-imag z)
	  (compnum-real z)))

(define (compnum-magnitude z)
  (let ((r (compnum-real z))
	(i (compnum-imag z)))
    (flsqrt (fl+ (fl* r r) (fl* i i)))))

(define (compnum-exp c)
  (let ((i (compnum-imag c)))
    (compnum* (make-compnum (flexp (compnum-real c)) (r5rs->flonum 0.0))
	      (make-compnum (flcos i)
			    (flsin i)))))

(define (compnum-log z)
  (compnum+ (fllog (compnum-magnitude z)) 
	    (compnum* plus-i (compnum-angle z))))

(define (compnum-sqrt c)
  (compnum-exp (compnum/ (compnum-log c) compnum-two)))

(define (compnum-sin c)
  (let ((i-c (compnum* c +i)))
    (compnum/ (compnum- (compnum-exp i-c)
			(compnum-exp (compnum- compnum-zero i-c)))
	      plus-2i)))

(define (compnum-cos c)
  (let ((i-c (compnum* c plus-i)))
    (compnum/ (compnum+ (compnum-exp i-c)
			(compnum-exp (compnum- compnum-zero i-c)))
	      (r5rs->flonum 2.0))))

(define (compnum-tan c)
  (compnum/ (compnum-sin c) (compnum-cos c)))

(define (compnum-asin c)
  (compnum* minus-i
	    (compnum-log (compnum+ (compnum* c plus-i)
				   (compnum-sqrt (compnum- compnum-one 
							   (compnum* c c)))))))

(define (compnum-acos c)
  (compnum* minus-i
	    (compnum-log
	     (compnum+ c
		       (compnum* plus-i
				 (compnum-sqrt (compnum- compnum-one
							 (compnum* c c))))))))

(define (compnum-atan1 c)
  (if (or (compnum= c plus-i)
	  (compnum= c minus-i))
      compnum-inf+
      (compnum* plus-i
		(compnum/ (compnum-log (compnum/ (compnum+ plus-i c)
						 (compnum+ plus-i (compnum- compnum-zero c))))
			  compnum-two))))


(define (compnum->string x radix precision)
  (let ((r (compnum-real x))
	(i (compnum-imag x)))
    ;; A little mysterious, to deal with +/-inf.0, +nan.0
    (let ((rr (flonum->string r 10 precision))
	  (ii (flonum->string i 10 precision)))
      (string-append rr
		     (let ((c (string-ref ii 0)))
		       (if (and (not (char=? c #\+))
				(not (char=? c #\-)))
			   "+"
			   ""))
		     ii
		     "i"))))
