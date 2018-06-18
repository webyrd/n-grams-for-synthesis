; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Converting numbers to strings

(define (number->string x . more)
  (if (null? more)
      (number2string x (r5rs->integer 10) #f)
      (let ((radix (car more))
	    (precision (if (null? (cdr more))
			   #f
			   (cadr more))))
	(if (and (exact-integer? radix)
		 (integer< (r5rs->integer 1) radix)
		 (integer< radix (r5rs->integer 37)))
	    (number2string x radix precision)
	    (begin
	      (error "Bad radix" radix)
	      #t)))))

(define (number2string x radix precision)
  (cond ((fixnum? x)
	 (integer->string x radix))
	((bignum? x)
	 (bignum->string x radix))
	((flonum? x)
	 (flonum->string x radix precision))
	((compnum? x)
	 (compnum->string x radix precision))
	((ratnum? x)
	 (ratnum->string x radix))
	((recnum? x)
	 (recnum->string x radix))
	(else
	 (error "number->string: not a number: " x)
	 #t)))
