; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Numeric coercion

(define (fixnum->ratnum f)
  (make-unreduced-ratnum f (r5rs->integer 1)))

(define (fixnum->recnum f)
  (make-recnum f (r5rs->integer 0)))

(define (fixnum->compnum f)
  (make-compnum (fixnum->flonum f) (r5rs->flonum 0.0)))

(define (bignum->ratnum f)
  (make-unreduced-ratnum f (r5rs->integer 1)))

(define (bignum->recnum f)
  (make-recnum f 0))

(define (bignum->compnum f)
  (make-compnum (integer->flonum f) (r5rs->flonum 0.0)))

(define (ratnum->recnum f)
  (make-recnum f (r5rs->integer 0)))

(define (ratnum->flonum f)
  (rational->flonum f))

(define (ratnum->compnum f)
  (make-compnum (ratnum->flonum f) (r5rs->flonum 0.0)))

(define (recnum->compnum f)
  (make-compnum (rational->flonum (recnum-real f)) 
		(rational->flonum (recnum-imag f))))

(define (flonum->compnum f)
  (make-compnum f (r5rs->flonum 0.0)))

(define (flonum->bignum f)
  (integer->bignum (flonum->integer f)))

(define (compnum->bignum f)
  (flonum->bignum (compnum-real f)))

(define (compnum->integer c)
  (if (flzero? (compnum-imag c))
      (flonum->integer (compnum-real c))
      (error "expected compnum with zero imaginary part" c)))

(define (flonum->recnum f)
  (ratnum->recnum (flonum->rational f)))

(define (compnum->recnum f)
  (make-recnum (flonum->rational (compnum-real f))
	       (flonum->rational (compnum-imag f))))

(define (bignum->flonum f)
  (integer->flonum f))

(define (compnum-float? f)
  (flzero? (compnum-imag f)))

(define (exact-integer? obj)
  (or (fixnum? obj)
      (bignum? obj)))

(define (compnum-integral? f)
  (and (compnum-float? f)
       (flinteger? (compnum-real f))))

(define (recnum-integral? f)
  (and (exact-integer? (recnum-real f))
       (integer-zero? (recnum-imag f))))

(define (id x) x)

; eof
