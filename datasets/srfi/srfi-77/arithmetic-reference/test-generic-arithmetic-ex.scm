; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Tests for exact generic arithmetic

(define (n-r5rs= a b)
  (exact=? a (r5rs->number b)))

(check (numerical exact-complex? 3+4i) =>  #t)
(check (numerical exact-complex? 3) =>  #t)
(check (numerical exact-rational? 6/10) =>  #t)
(check (numerical exact-rational? 6/3) =>  #t)
(check (exact-integer? (numerical exact-make-rectangular 3 0)) =>  #t)
(check (numerical exact-integer? 3.0) =>  #f)
(check (numerical exact-integer? 8/4) =>  #t)

(check (numerical exact<=? 1 2 3 4) => #t)
(check (numerical exact<? 1 2 7/2 4 9999999999999999999) => #t)
(check (numerical exact>? 1 2 7/2 4 9999999999999999999) => #f)
(check (numerical exact>=? 2 2 3/4 0) => #t)
(check (numerical exact=? 0 -0) => #t)
(check (exact=? (numerical exact-make-rectangular 4 0) (r5rs->number 4)) => #t)
(check (exact=? (numerical exact-make-rectangular 4 0) 
		(numerical exact-make-rectangular 4 2)) => #f)
(check (exact=? (numerical exact-make-rectangular 4 2) 
		(numerical exact-make-rectangular 4 0)) => #f)
(check (exact=? (numerical exact-make-rectangular 4 2) (r5rs->number 4)) => #f)
(check (exact=? (numerical exact-make-rectangular 10000000000000000000000000000000000000000 0) (r5rs->number 10000000000000000000000000000000000000000)) => #t)

(check (numerical exact-zero? 3218943724243) => #f)
(check (numerical exact-zero? 0) => #t)
(check (exact-zero? (numerical exact-make-rectangular 0 0)) => #t)

(check (numerical exact-odd? 5) => #t)
(check (numerical exact-odd? 9999999999989000000000001) => #t)
(check (numerical exact-odd? 9999999999989000000000000) => #f)
(check (numerical exact-even? 5) => #f)
(check (numerical exact-even? 9999999999989000000000001) => #f)
(check (numerical exact-even? 9999999999989000000000000) => #t)

(check (numerical exact-abs 7) ==> 7)
(check (numerical exact-abs -7) ==> 7)
(check (numerical exact-abs 3/4) ==> 3/4)
(check (numerical exact-abs -3/4) ==> 3/4)

(check (numerical exact-max 1 2 4 3 5) ==> 5)
(check (numerical exact-max 1 2 3 5 4) ==> 5)
(check (numerical exact-max 1 5 7/2 2 4) ==> 5)
(check (numerical exact-min 4 1 2 3 5) ==> 1)
(check (numerical exact-min 2 1 3 5 4) ==> 1)
(check (numerical exact-min 1 5 7/2 2 4) ==> 1)

(check (numerical exact+ 3 4) ==> 7)
(check (numerical exact+ 3) ==> 3)
(check (numerical exact+) ==> 0)
(check (numerical exact+ 9999999999999 999999999999) ==> 10999999999998)
(check (numerical exact+ 1000 5) ==> 1005)
(check (numerical exact* 4) ==> 4)
(check (numerical exact*) ==> 1)
(check (numerical exact* 4 3000) ==> 12000)
(check (numerical exact* 9999999999999 999999999999) ==> 9999999999989000000000001)

(check (numerical exact- 3 4) ==> -1)
(check (numerical exact- 3 4 5) ==> -6)
(check (numerical exact- 3) ==> -3)
(check (numerical exact/ 3 4 5) ==> 3/20)
(check (numerical exact/ 3) ==> 1/3)
(check (numerical exact/ 1 2) ==> 1/2)

(check (numerical exact-gcd) ==> 0)
(check (numerical exact-lcm 32 -36) ==> 288)
(check (numerical exact-lcm) ==> 1)

(check (numerical exact-floor #e-4.3) ==> -5)
(check (numerical exact-ceiling #e-4.3) ==> -4)
(check (numerical exact-truncate #e-4.3) ==> -4)
(check (numerical exact-round #e-4.3) ==> -4)

(check (numerical exact-floor #e3.5) ==> 3)
(check (numerical exact-ceiling #e3.5) ==> 4)
(check (numerical exact-truncate #e3.5) ==> 3)

(check (numerical exact-round 7/2) ==> 4)
(check (numerical exact-round 7) ==> 7)

(check (numerical exact-expt 5 3) ==>  125)
(check (numerical exact-expt 3 123) ==> 48519278097689642681155855396759336072749841943521979872827)
(check (numerical exact-expt 5 0) ==> 1)
(check (numerical exact-expt 0 5) ==>  0)
(check (numerical exact-expt 0 0) ==> 1)

(check (exact-numerator (numerical exact/ 3 -4)) ==> -3)
(check (numerical exact-denominator 0) ==> 1)

(for-each (lambda (n)
	    (check
	     (call-with-values
		 (lambda ()
		   (numerical exact-integer-sqrt n))
	       (lambda (s r)
		 (and (exact=? (r5rs->number n)
			       (exact+ (exact* s s) r))
		      
		      (exact<? (r5rs->number n)
			       (exact* (exact+ s (r5rs->number 1))
				       (exact+ s (r5rs->number 1)))))))
	     => #t))
	  '(10
	    100 10000 100000000 10000000000000000 
	    100000000000000000000000000000000
	    10000000000000000000000000000000000000000000000000000000000000000))
				       
		      
