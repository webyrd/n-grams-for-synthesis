; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; from Scheme 48:

; Bitwise logical operators on bignums.

(define (bignum-bitwise-not m)
  ;; (integer+ (integer-negate m) -1)
  (bignum- (r5rs->bignum -1) m))

(define (bignum-bitwise-and m n)
  (if (or (bignum-zero? m) (bignum-zero? n))
      (r5rs->fixnum 0)
      (bignum-bitwise-op fxbitwise-and m n)))

(define (bignum-bitwise-ior m n)
  (cond ((bignum-zero? m) n)
	((bignum-zero? n) m)
	(else
	 (bignum-bitwise-op fxbitwise-ior m n))))

(define (bignum-bitwise-xor m n)
  (cond ((bignum-zero? m) n)
	((bignum-zero? n) m)
	(else
	 (bignum-bitwise-op fxbitwise-xor m n))))

(define (bignum-bitwise-op op m n)
  (let ((finish (lambda (sign-bit mag-op)
		  (let ((mag (mag-op op
				     (bignum-magnitude m)
				     (bignum-magnitude n))))
		    (make-integer (if (fxzero? sign-bit) (r5rs->fixnum 1) (r5rs->fixnum -1))
				  (if (fxzero? sign-bit)
				      mag
				      (negate-magnitude mag)))))))
    (if (fx>= (bignum-sign m) (r5rs->fixnum 0))
	(if (fx>= (bignum-sign n) (r5rs->fixnum 0))
	    (finish (op (r5rs->fixnum 0) (r5rs->fixnum 0)) magnitude-bitwise-binop-pos-pos)
	    (finish (op (r5rs->fixnum 0) (r5rs->fixnum 1)) magnitude-bitwise-binop-pos-neg))
	(if (fx>= (bignum-sign n) (r5rs->fixnum 0))
	    (finish (op (r5rs->fixnum 0) (r5rs->fixnum 1)) magnitude-bitwise-binop-neg-pos)
	    (finish (op (r5rs->fixnum 1) (r5rs->fixnum 1)) magnitude-bitwise-binop-neg-neg)))))

(define radix-mask (fx- radix (r5rs->fixnum 1)))

(define (magnitude-bitwise-binop-pos-pos op m n)
  (let recur ((m m) (n n))
    (if (and (zero-magnitude? m) (zero-magnitude? n))
	m
	(adjoin-digit (fxbitwise-and (op (low-digit m) (low-digit n)) radix-mask)
		      (recur (high-digits m) (high-digits n))))))

; Same as the above, except that one magnitude is that of a negative number.

(define (magnitude-bitwise-binop-neg-pos op m n)
  (magnitude-bitwise-binop-pos-neg op n m))

(define (magnitude-bitwise-binop-pos-neg op m n)
  (let recur ((m m) (n n) (carry (r5rs->fixnum 1)))
    (if (and (zero-magnitude? n) (zero-magnitude? m))
	(fixnum->magnitude (op (r5rs->fixnum 0) carry))
	(call-with-values
	 (lambda ()
	   (negate-low-digit n carry))
	 (lambda (n-digit carry)
	   (adjoin-digit (op (low-digit m) n-digit)
			 (recur (high-digits m)
				(high-digits n)
				carry)))))))

; Now both M and N are magnitudes of negative numbers.

(define (magnitude-bitwise-binop-neg-neg op m n)
  (let recur ((m m) (n n) (m-carry (r5rs->fixnum 1)) (n-carry (r5rs->fixnum 1)))
    (if (and (zero-magnitude? n) (zero-magnitude? m))
	(fixnum->magnitude (op m-carry n-carry))
	(call-with-values
	 (lambda ()
	   (negate-low-digit m m-carry))
	 (lambda (m-digit m-carry)
	   (call-with-values
	    (lambda ()
	      (negate-low-digit n n-carry))
	    (lambda (n-digit n-carry)
	      (adjoin-digit (op m-digit n-digit)
			    (recur (high-digits m)
				   (high-digits n)
				   m-carry
				   n-carry)))))))))

(define (negate-low-digit m carry)
  (let ((m (fx+ (fxbitwise-and (fxbitwise-not (low-digit m))
			       radix-mask)
		carry)))
    (if (fx>= m radix)
	(values (fx- m radix) (r5rs->fixnum 1))
	(values m (r5rs->fixnum 0)))))

(define (negate-magnitude m)
  (let recur ((m m) (carry (r5rs->fixnum 1)))
    (if (zero-magnitude? m)
	(fixnum->magnitude carry)
	(call-with-values
	 (lambda ()
	   (negate-low-digit m carry))
	 (lambda (next carry)
	   (adjoin-digit next
			 (recur (high-digits m) carry)))))))

; arithmetic-shift-left

(define (bignum-arithmetic-shift-left m n)
  (make-integer (bignum-sign m)
		(cond ((bignum-positive? n)
		       (shift-left-magnitude (bignum-magnitude m) n))
		      ((fx= (r5rs->fixnum 1) (bignum-sign m))
		       (shift-right-pos-magnitude (bignum-magnitude m) n))
		      (else
		       (shift-right-neg-magnitude (bignum-magnitude m) n)))))

(define big-log-radix (fixnum->bignum log-radix))

(define (shift-left-magnitude mag n)
  (if (or (and (fixnum? n)
	       (fx< n log-radix))
	  (and (bignum? n)
	       (bignum< n big-log-radix)))
      (let* ((n (x->fixnum n))
	     (mask (fx- (fxarithmetic-shift-left (r5rs->fixnum 1) (fx- log-radix n))
			(r5rs->fixnum 1))))
	(let recur ((mag mag)
		    (low (r5rs->fixnum 0)))
	  (if (zero-magnitude? mag)
	      (adjoin-digit low zero-magnitude)
	      ;; Split the low digit into left and right parts, and shift
	      (let ((left (fxarithmetic-shift-left (low-digit mag)
						   (fx- n log-radix))) ;shift right
		    (right (fxarithmetic-shift-left (fxbitwise-and (low-digit mag) mask)
						    n)))
		(adjoin-digit (fxbitwise-ior low right)
			      (recur (high-digits mag)
				     left))))))
      (adjoin-digit (r5rs->fixnum 0)
		    (shift-left-magnitude mag
					  (bignum- (x->bignum n) big-log-radix)))))

; N is nonnegative
(define (shift-right-pos-magnitude mag n)
  (if (or (and (fixnum? n)
	       (fx> n (fx- (r5rs->fixnum 0) log-radix)))
	  (and (bignum? n)
	       (bignum> n (bignum-negate big-log-radix))))
      (let* ((n (x->fixnum n))
	     (mask (fx- (fxarithmetic-shift-left (r5rs->fixnum 1) (fx- (r5rs->fixnum 0) n))
			(r5rs->fixnum 1))))
	(let recur ((mag mag))
	  (let ((low (low-digit mag))
		(high (high-digits mag)))
	    (adjoin-digit
	     (fxbitwise-ior (fxarithmetic-shift-left low n)
			    (fxarithmetic-shift-left (fxbitwise-and mask (low-digit high))
						     (fx+ n log-radix)))
	     (if (zero-magnitude? high)
		 zero-magnitude
		 (recur high))))))
      (shift-right-pos-magnitude (high-digits mag)
				 (bignum+ (x->bignum n) big-log-radix))))

(define (x->bignum n)
  (if (fixnum? n)
      (fixnum->bignum n)
      n))

(define (x->fixnum n)
  (if (bignum? n)
      (bignum->fixnum n)
      n))
 
; N is nonegative
(define (shift-right-neg-magnitude mag n)
  (negate-magnitude
   (let digit-recur ((mag mag) (n n) (carry (r5rs->fixnum 1)))
     (call-with-values
	 (lambda ()
	   (negate-low-digit mag carry))
       (lambda (digits carry)
	 (if (or (and (fixnum? n)
		      (fx<= n (fx- (r5rs->fixnum 0) log-radix)))
		 (and (bignum? n)
		      (bignum<= n (bignum-negate big-log-radix))))
	     (digit-recur (high-digits mag) (bignum+ (x->bignum n) big-log-radix) carry)
	     (let* ((n (x->fixnum n))
		    (mask (fx- (fxarithmetic-shift-left (r5rs->fixnum 1) (fx- (r5rs->fixnum 0) n))
			       (r5rs->fixnum 1))))
	       (let recur ((mag mag) (low digits) (carry carry))
		 (let ((high-digits (high-digits mag)))
		   (call-with-values
		       (lambda ()
			 (negate-low-digit high-digits carry))
		     (lambda (high carry)
		       (adjoin-digit
			(fxbitwise-ior (fxarithmetic-shift-left low n)
				       (fxarithmetic-shift-left (fxbitwise-and mask high)
								(fx+ n log-radix)))
			(if (zero-magnitude? high-digits)
			    (fixnum->magnitude carry)
			    (recur high-digits high carry))))))))))))))

;(define (tst)
;  (let* ((m (random))
;         (n (bitwise-and m 63))
;         (m1 (integer-arithmetic-shift-left
;              (integer-arithmetic-shift-left m n)
;              (- 0 n))))
;    (list n m m1 (= m m1))))
;(define random (make-random 17))
