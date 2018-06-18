; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Converting from flonums to rationals.

(define (flonum->integer f)
  (if (flinteger? f)
      (flonum->rational f)
      (error "Can't convert to an integer" f)))

(define (flonum->rational f)
  (if (or (fl= f flinf+)
	  (fl= f flinf-)
	  (flnan? f))
      (error "Can't convert to an exact number" f))
  (let* ((m (flsignificand f))
	 (e (flexponent f))
	 (q  (if (integer>= e (r5rs->integer 0))
		 (integer* m (integer-expt (r5rs->integer 2) e))
		 (integer/ m
			   (integer-expt (r5rs->integer 2) (integer-negate e))))))
    (if (not (integer-zero? (flsign f)))
	(rational- (r5rs->integer 0) q)
	q)))
