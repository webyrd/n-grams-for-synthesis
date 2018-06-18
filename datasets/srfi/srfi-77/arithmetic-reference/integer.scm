; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Generic arbitrary-precision integer arithmetic built on top of
; fixnums and bignums.

(define (exact-integer? obj)
  (or (fixnum? obj)
      (bignum? obj)))

(define (integer->r5rs m)
  (if (fixnum? m)
      (fixnum->r5rs m)
      (bignum->r5rs m)))

(define fx-least-r5rs (fixnum->r5rs (least-fixnum)))
(define fx-greatest-r5rs (fixnum->r5rs (greatest-fixnum)))

(define (r5rs->integer m)
  (if (and (r5rs:>= m fx-least-r5rs)
	   (r5rs:<= m fx-greatest-r5rs))
      (r5rs->fixnum m)
      (r5rs->bignum m)))

(define (integer->bignum m)
  (if (bignum? m)
      m
      (fixnum->bignum m)))

(define (make-int*int->val op)
  (lambda (a b)
    (op (integer->bignum a) (integer->bignum b))))

(define integer+ (make-int*int->val bignum+))
(define (integer- m n)
  (integer+ m (integer-negate n)))

; ####assumes symmetric fixnum range
(define (integer-negate m)
  (cond ((bignum? m)
	 (bignum-negate m))
	((fx= m (least-fixnum))
	 least-fixnum-negated)
	(else (fx- m))))

; ####assumes two's complement---oops!
(define least-fixnum-negated (bignum-negate (fixnum->bignum (least-fixnum))))

(define integer* (make-int*int->val bignum*))
(define integer-quotient (make-int*int->val bignum-quotient))
(define integer-remainder (make-int*int->val bignum-remainder))
(define integer-quotient+remainder (make-int*int->val bignum-quotient+remainder))

(define integer= (make-int*int->val bignum=))
(define integer< (make-int*int->val bignum<))

; GCD

(define (integer-zero? x)
  (if (fixnum? x)
      (fxzero? x)
      (bignum-zero? x)))

(define (integer-gcd x y)
  (cond ((integer< x (r5rs->integer 0)) (integer-gcd (integer-negate x) y))
	((integer< y (r5rs->integer 0)) (integer-gcd x (integer-negate y)))
	((integer< x y) (euclid y x))
	(else (euclid x y))))

(define (euclid x y)
  (if (integer-zero? y)
      x
      (euclid y (integer-remainder x y))))

; LCM

(define (integer-lcm x y)
  (let ((g (integer-gcd x y)))
    (if (integer-zero? g)
	g
	(integer* (integer-quotient (integer-abs x) g)
		  (integer-abs y)))))

(define (integer-abs x)
  (if (integer< x (r5rs->integer 0))
      (integer-negate x)
      x))

(define (integer-expt x y)
  (cond ((integer-zero? y)
	 (r5rs->integer 1))
	((integer-odd? y)
	 (integer* x (integer-expt x (integer- y (r5rs->integer 1)))))
	(else 
	 (let ((v (integer-expt x (integer-quotient y (r5rs->integer 2)))))
	   (integer* v v)))))

(define (integer-even? n)
  (integer-zero? (integer-remainder n (r5rs->integer 2))))

(define (integer-odd? n)
  (not (integer-even? n)))

(define (integer> m n)
  (integer< n m))

(define (integer>= m n)
  (not (integer< m n)))

(define (integer<= m n)
  (integer>= n m))

(define (integer-negative? m)
  (integer< m (r5rs->integer 0)))
(define (integer-positive? m)
  (integer> m (r5rs->integer 0)))

(define (integer-min m n)
  (if (integer< m n) m n))
(define (integer-max m n)
  (if (integer> m n) m n))

(define (integer-abs m)
  (if (integer-negative? m)
      (integer-negate m)
      m))

(define (integer->string b r)
  (bignum->string (integer->bignum b) r))

(define (integer-bitwise-not m)
  (if (fixnum? m)
      (fxbitwise-not m)
      (bignum-bitwise-not m)))

(define (make-binary-bitwise-op fix-op big-op)
  (lambda (a b)
    (if (fixnum? a)
	(if (fixnum? b)
	    (fix-op a b)
	    (big-op (fixnum->bignum a) b))
	(if (fixnum? b)
	    (big-op a (fixnum->bignum b))
	    (big-op a b)))))

(define integer-bitwise-ior
  (make-binary-bitwise-op fxbitwise-ior bignum-bitwise-ior))
(define integer-bitwise-xor
  (make-binary-bitwise-op fxbitwise-xor bignum-bitwise-xor))
(define integer-bitwise-and
  (make-binary-bitwise-op fxbitwise-and bignum-bitwise-and))
(define integer-arithmetic-shift-left (make-int*int->val bignum-arithmetic-shift-left))
