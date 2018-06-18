; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Bignum arithmetic

; from Scheme 48

(define-record-type :bignum
  (make-bignum sign magnitude)
  bignum?
  (sign bignum-sign)
  (magnitude bignum-magnitude))

(define-record-discloser :bignum
  (lambda (r)
    (list 'bignum
	  (bignum->r5rs r))))

(define (fixnum->bignum m)
  (cond ((fx>= m (r5rs->fixnum 0))
	 (make-bignum (r5rs->fixnum 1) (fixnum->magnitude m)))
	((fx= m (least-fixnum))
	 (make-bignum (r5rs->fixnum -1) fx-min-magnitude))
	(else
	 (make-bignum (r5rs->fixnum -1) (fixnum->magnitude (fx- m))))))

(define (r5rs->bignum m)
  (if (bignum? m)
      m
      (cond ((r5rs:>= m 0)
	     (make-bignum (r5rs->fixnum 1) (r5rs->magnitude m)))
	    ((r5rs:= m (fixnum->r5rs (least-fixnum)))
	     (make-bignum (r5rs->fixnum -1) fx-min-magnitude))
	    (else
	     (make-bignum (r5rs->fixnum -1) (r5rs->magnitude (r5rs:- 0 m)))))))

(define (r5rs->magnitude n)
  (let ((radix (fixnum->r5rs radix)))
    (let recur ((n n))
      (if (r5rs:= n 0)
	  zero-magnitude
	  (let ((digit (r5rs->fixnum (r5rs:remainder n radix))))
	    (adjoin-digit digit
			  (recur (r5rs:quotient n radix))))))))

(define (bignum->r5rs n)             ;For debugging
  (r5rs:* (fixnum->r5rs (bignum-sign n))
     (let recur ((digits (bignum-magnitude n)))
       (if (null? digits)
	   0
	   (r5rs:+ (fixnum->r5rs (car digits))
	      (r5rs:* (recur (cdr digits)) (fixnum->r5rs radix)))))))

(define (make-integer sign mag)
  (if (fxpositive? sign)
      (if (smaller-magnitude? fx-max-magnitude mag)
	  (make-bignum sign mag)
	  (magnitude->integer mag))
      (if (smaller-magnitude? fx-min-magnitude mag)
	  (make-bignum sign mag)
	  (if (same-magnitude? mag fx-min-magnitude)
	      (least-fixnum)
	      (fx- (magnitude->integer mag))))))

(define (bignum->integer m)
  (make-integer (bignum-sign m)
		(bignum-magnitude m)))

; #### assumes symmetric fixnums
(define (bignum->fixnum m)
  (let ((sign (bignum-sign m))
	(mag (bignum-magnitude m)))
    (cond
     ((fxpositive? sign)
      (magnitude->integer mag))
     ((same-magnitude? mag fx-min-magnitude)
      (least-fixnum))
     (else (fx- (magnitude->integer mag))))))

; Arithmetic

(define (bignum+ m n)
  (let ((m-sign (bignum-sign m))
	(m-mag (bignum-magnitude m))
	(n-sign (bignum-sign n))
	(n-mag (bignum-magnitude n)))
    (if (fx= m-sign n-sign)
	(make-integer m-sign (add-magnitudes m-mag n-mag))
	(if (smaller-magnitude? m-mag n-mag)
	    (make-integer (fx- m-sign) (subtract-magnitudes n-mag m-mag))
	    (make-integer m-sign (subtract-magnitudes m-mag n-mag))))))

(define (bignum- m n)
  (bignum+ m (bignum-negate n)))

(define (bignum-negate m)
  (make-bignum (fx- (bignum-sign m))
	       (bignum-magnitude m)))

(define (bignum* m n)
  (make-integer (fx* (bignum-sign m) (bignum-sign n))
		(multiply-magnitudes
		 (bignum-magnitude m)
		 (bignum-magnitude n))))

(define (bignum-divide m n cont)
  (divide-magnitudes
   (bignum-magnitude m)
   (bignum-magnitude n)
   (lambda (q r)
     (cont (make-integer (fx* (bignum-sign m) (bignum-sign n)) q)
	   (make-integer (bignum-sign m) r)))))

(define (bignum-quotient m n)
  (bignum-divide m n (lambda (q r) q)))

(define (bignum-remainder m n)
  (bignum-divide m n (lambda (q r) r)))

(define (bignum-quotient+remainder m n)
  (bignum-divide m n values))

(define (bignum= m n)
  (and (fx= (bignum-sign m) (bignum-sign n))
       (same-magnitude? (bignum-magnitude m)
			(bignum-magnitude n))))

(define (bignum< m n)
  (let ((m-sign (bignum-sign m))
	(n-sign (bignum-sign n)))
    (or (fx< m-sign n-sign)
	(and (fx= m-sign n-sign)
	     (if (fxnegative? m-sign)
		 (smaller-magnitude? (bignum-magnitude n)
				     (bignum-magnitude m))
		 (smaller-magnitude? (bignum-magnitude m)
				     (bignum-magnitude n)))))))


(define (bignum<= p q)
  (not (bignum< q p)))

(define (bignum>= p q)
  (not (bignum< p q)))

(define (bignum> p q)
  (bignum< q p))

(define (bignum-zero? m)
  (zero-magnitude? (bignum-magnitude m)))

(define (bignum-positive? m)
  (fxpositive? (bignum-sign m)))
(define (bignum-negative? m)
  (fxnegative? (bignum-sign m)))

(define (bignum-odd? m)
  (fxodd? (low-digit (bignum-magnitude m))))
(define (bignum-even? m)
  (fxeven? (low-digit (bignum-magnitude m))))

(define (bignum-abs m)
  (if (bignum-negative? m)
      (bignum-negate m)
      m))

(define (bignum-min m n)
  (if (bignum<= m n)
      (bignum->integer m)
      (bignum->integer n)))

(define (bignum-max m n)
  (if (bignum>= m n)
      (bignum->integer m)
      (bignum->integer n)))


; Magnitude (unsigned integer) arithmetic

; Fixnum arithmetic without overflow checking sucks
(define log-radix
  (let ((max (fxquotient (greatest-fixnum) (r5rs->fixnum 2)))
	(min (fxquotient (least-fixnum) (r5rs->fixnum 2))))
    (let loop ((l (r5rs->fixnum 1))
	       (r (r5rs->fixnum 1))
	       (rm (r5rs->fixnum -1)))
      (if (or (fx>= r max)
	      (fx<= rm min))
	  (fxquotient l (r5rs->fixnum 2))
	  (loop (fx+ (r5rs->fixnum 1) l)
		(fx* r(r5rs->fixnum 2)) (fx* rm (r5rs->fixnum 2)))))))

(define radix (fxarithmetic-shift-left (r5rs->fixnum 1) log-radix))

(define zero-magnitude '())
(define zero-magnitude? null?)

(define (low-digit m)
  (if (zero-magnitude? m)
      (r5rs->fixnum 0)
      (car m)))

(define (high-digits m)
  (if (zero-magnitude? m)
      m
      (cdr m)))

(define (adjoin-digit d m)
  (if (and (fxzero? d) (zero-magnitude? m))
      m
      (cons d m)))

(define (fixnum->magnitude n)
  (if (fxzero? n)
      zero-magnitude
      (let ((digit (fxremainder n radix)))
	(adjoin-digit digit
		      (fixnum->magnitude (fxquotient n radix))))))

(define (magnitude->integer m)
  (if (zero-magnitude? m)
      (r5rs->fixnum 0)
      (fx+ (low-digit m)
	   (fx* radix (magnitude->integer (high-digits m))))))

(define fx-max-magnitude
  (fixnum->magnitude (greatest-fixnum)))

(define fx-min-magnitude
  (adjoin-digit (fx- (fxremainder (least-fixnum) radix))
		(fixnum->magnitude
		 (fx- (fxquotient (least-fixnum) radix)))))

; Combine two numbers digitwise using op.

(define (combine-magnitudes m n op)
  (let recur ((m m) (n n) (carry (r5rs->fixnum 0)))
    (if (and (zero-magnitude? m) (zero-magnitude? n))
	(fixnum->magnitude carry)
	(let ((result (fx+ carry (op (low-digit m) (low-digit n)))))
	  (call-with-values
	      (lambda () (fxquotient+remainder result radix))
	    (lambda (q r)
	      (if (fxnegative? r)
		  (adjoin-digit (fx+ r radix)
				(recur (high-digits m)
				       (high-digits n)
				       (fx- q (r5rs->fixnum 1))))
		  (adjoin-digit r
				(recur (high-digits m)
				       (high-digits n)
				       q)))))))))

(define (add-magnitudes m n)
  (combine-magnitudes m n fx+))

(define (subtract-magnitudes m n)
  (combine-magnitudes m n fx-))

; Compare

(define (same-magnitude? m n)
  (let loop ((m m) (n n))
    (cond
     ((null? m)
      (null? n))
     ((null? n) #f)
     (else
      (and (fx= (car m) (car n))
	   (loop (cdr m) (cdr n)))))))

(define (smaller-magnitude? m n)
  (let ((m-len (r5rs->fixnum (length m)))
	(n-len (r5rs->fixnum (length n))))
    (cond ((fx< m-len n-len)
	   #t)
	  ((fx< n-len m-len)
	   #f)
	  (else
	   (let loop ((m m) (n n) (a #f))
	     (cond ((zero-magnitude? m)
		    (or (not (zero-magnitude? n)) a))
		   ((zero-magnitude? n) #f)
		   (else
		    (loop (high-digits m)
			  (high-digits n)
			  (or (fx< (low-digit m) (low-digit n))
			      (and (fx= (low-digit m) (low-digit n)) a))))))))))

; Multiply

(define (multiply-magnitudes m n)
  (let recur ((m m) (a zero-magnitude))
    (if (zero-magnitude? m)
	a
	(let ((a (combine-magnitudes a n (lambda (d e)
					   (fx+ d (fx* e (low-digit m)))))))
	  (adjoin-digit (low-digit a)
			(recur (high-digits m) (high-digits a)))))))

; Divide m/n: find q and r such that m = q*n + r, where 0 <= r < n.
; Oh no... time to get out Knuth...
; The main thing we don't do that Knuth does is to normalize the
; divisor (n) by shifting it left.

(define (divide-magnitudes m n cont)
  (if (zero-magnitude? (high-digits n))
      (divide-by-digit m (low-digit n)
		       (lambda (q r)
			 (cont q (adjoin-digit r zero-magnitude))))
      (let recur ((m m) (cont cont))
	(if (smaller-magnitude? m n)
	    (cont zero-magnitude m)
	    (recur
	     (high-digits m)
	     (lambda (q r)
	       ;; 0 <= r < n  and  d < b
	       ;; so  b*r + d < b*n.
	       (divide-step (adjoin-digit (low-digit m) r)
			    n
			    (lambda (q1 r)
			      (cont (adjoin-digit q1 q) r)))))))))
	
; Divide m by n, where  n <= m < b*n, i.e. 1 <= quotient < b.
; E.g.  if  n = 100  then  100 <= m <= 999
;       if  n = 999  then  999 <= m <= 9989

(define (divide-step m n cont)
  (do ((m-high m (high-digits m-high))
       (n-high n (high-digits n-high)))
      ((zero-magnitude? (high-digits (high-digits n-high)))
       ;; Occasionally q^ is one larger than the actual first digit.
       ;; This loop will never iterate more than once.
       (let loop ((q^ (fxmin (guess-quotient-digit m-high n-high)
			     (fx- radix (r5rs->fixnum 1)))))
	 (let ((r (combine-magnitudes m n (lambda (d e)
					    (fx- d (fx* e q^))))))
	   (if (improper-magnitude? r)
	       ;; (begin (write `(addback ,m ,n ,q^ ,r)) (newline) ...)
	       (loop (fx- q^ (r5rs->fixnum 1)))
	       (cont q^ r)))))))

; Compute q such that [m1 m2 m3] = q*[n1 n2] + r with 0 <= r < [n1 n2]
; Can assume b <= [0 n1 n2] <= [m1 m2 m3] <= [n1 n2 b-1]
; Some examples:
;  m / n :  100[1] / 10[02], 099 / 10, 099[1] / 99[0], 999[8] / 99[99]
; Various hacks are possible to improve performance.  In particular, the
; second division can be eliminated if the divisor is normalized.
; See Knuth.
;  [m1 m2] = q0*[n1] + r0
;  [m1 m2 m3] = q0*[n1 n2] + r^
;  r^ = b*r0 + m3 - q0*n2

(define (guess-quotient-digit m n)
  (let ((n1 (low-digit (high-digits n)))
	(n2 (low-digit n))
	(m1 (low-digit (high-digits (high-digits m))))
	(m2 (low-digit (high-digits m)))
	(m3 (low-digit m)))
    (let ((m12 (fx+ (fx* m1 radix) m2)))
      (call-with-values
	  (lambda () (fxquotient+remainder m12 n1))
	(lambda (q0 r0)
	  (let ((r^ (fx- (fx+ (fx* radix r0) m3) (fx* q0 n2)))
		(n12 (fx+ (fx* n1 radix) n2)))
	    (call-with-values
		(lambda () (fxquotient+remainder r^ n12))
	      (lambda (q1 r1)
		(if (fxpositive? q1)
		    (begin (display "This should never happen: q1 = ")
			   (write q1) (newline)))
		(let ((q (fx+ q0 q1)))
		  (if (fxnegative? r1) (fx- q (r5rs->fixnum 1)) q))))))))))

(define (improper-magnitude? m)
  (and (not (zero-magnitude? m))
       (or (fxnegative? (low-digit m))
	   (improper-magnitude? (high-digits m)))))

; Special case of division algorithm for single-digit divisor.

(define (divide-by-digit m d cont)
  (if (fxzero? d)
      (error "integer division by zero" m d)
      (let recur ((m m) (cont cont))
	(if (and (zero-magnitude? (high-digits m))
		 (fx< (low-digit m) d))
	    (cont zero-magnitude (low-digit m))
	    (recur (high-digits m)
		   (lambda (q r)
		     (let ((m1 (fx+ (low-digit m) (fx* radix r))))
		       (call-with-values
			   (lambda () (fxquotient+remainder m1 d))
			 (lambda (q1 r1)
			   (cont (adjoin-digit q1 q) r1))))))))))

;(define (divide-test seed)
;  (let ((random (make-random seed)))
;    (let loop ()
;      (let* ((z1 (integer+ (random) (integer* (random) 10000000)))
;             (z2 (integer+ (random) (integer* (random) 10000000)))
;             (n (max z1 z2))
;             (r (min z1 z2))
;             (q (random))
;             (m (integer+ (integer* n q) r)))
;        (if (not (= n r))
;            (integer-divide m n
;                            (lambda (q1 r1)
;                              (if (and (= q q1) (= r r1))
;                                  (begin (display ".")
;                                         (force-output (current-output-port)))
;                                  (error "division error" m n q q1 r r1)))))
;        (loop)))))


; from Larceny:

; Takes a bignum and a radix and returns the string which is the printable
; representation of the bignum in that radix.
;
; Uses brute force with extreme prejudice.

(define (bignum->string b r)
  (if (bignum-zero? b)
      (string-copy "0")
      (let ((r (fixnum->bignum r))
	    (d "0123456789abcdef")
	    (s (bignum-negative? b)))
	(let loop ((b (bignum-abs b)) (l '()))
	  (if (bignum-zero? b)
	      (if s
		  (list->string (cons #\- l))
		  (list->string l))
	      (bignum-divide->bignums
	       b r
	       (lambda (q r)
		 (loop q
		       (cons (string-ref d (bignum->r5rs r))
			     l))))))))) 

(define (bignum-divide->bignums m n cont)
  (divide-magnitudes
   (bignum-magnitude m)
   (bignum-magnitude n)
   (lambda (q r)
     (cont (make-bignum (fx* (bignum-sign m) (bignum-sign n)) q)
	   (make-bignum (bignum-sign m) r)))))