; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Flonums in terms of R5RS; assumes underlying IEEE-like representation


; SRFI 9
(define-record-type :flonum
  (really-make-flonum inexact)
  flonum?
  (inexact flonum-inexact))

; Scheme 48 extension; comment out if not available
(define-record-discloser :flonum
  (lambda (r)
    (list 'flonum (flonum-inexact r))))

(define (make-flonum n)
  (really-make-flonum (exact->inexact n)))

(define r5rs->flonum make-flonum)

(define flonum->r5rs flonum-inexact)

; for playing around
(define fl make-flonum)

(define (make-fl*fl->fl r5rs-op)
  (lambda (a b)
    (if (or (flnan? a)
	    (flnan? b))
	flnan
	(make-flonum (r5rs-op (flonum-inexact a) (flonum-inexact b))))))

(define fl+/2 (make-fl*fl->fl +))
(define (fl+ . args)
  (reduce (make-flonum 0.0) fl+/2 args))

(define fl-/2 (make-fl*fl->fl -))
(define (fl- arg0 . args)
  (reduce (make-flonum 0.0) fl-/2 (cons arg0 args)))

(define (make-fl->fl r5rs-op)
  (lambda (a)
    (if (flnan? a)
	flnan
	(make-flonum (r5rs-op (flonum-inexact a))))))

(define fl*/2 (make-fl*fl->fl *))
(define (fl* . args)
  (reduce (make-flonum 1.0) fl*/2 args))

(define (/* a b)
  (cond
   ((= b r5rs-inf+)
    (cond
     ((or (= a r5rs-inf+) (= a r5rs-inf-))
      r5rs-nan)
     ((< a 0.0)
      -0.0)
     (else
      0.0)))
   ((= b r5rs-inf-)
    (cond
     ((or (= a r5rs-inf+) (= a r5rs-inf-))
      r5rs-nan)
     ((< a 0.0)
      0.0)
     (else
      -0.0)))
   ((not (= b 0.0)) (/ a b))
   ((= a 0.0) r5rs-nan)
   ((> a 0.0) r5rs-inf+)
   (else r5rs-inf-)))

(define fl//2 (make-fl*fl->fl /*))
(define (fl/ arg0 . args)
  (reduce (make-flonum 1.0) fl//2 (cons arg0 args)))

(define (make-fl*fl->val r5rs-op)
  (lambda (a b)
    (r5rs-op (flonum-inexact a) (flonum-inexact b))))

(define fl= (make-transitive-pred (make-fl*fl->val =)))
(define fl>= (make-transitive-pred (make-fl*fl->val >=)))
(define fl<= (make-transitive-pred (make-fl*fl->val <=)))
(define fl> (make-transitive-pred (make-fl*fl->val >)))
(define fl< (make-transitive-pred (make-fl*fl->val <)))

(define (make-fl->val r5rs-op)
  (lambda (a)
    (r5rs-op (flonum-inexact a))))

(define (flzero? x)
  (fl= x (r5rs->flonum 0.0)))
(define (flpositive? x)
  (fl> x (r5rs->flonum 0.0)))
(define (flnegative? x)
  (fl< x (r5rs->flonum 0.0)))

(define flmin (make-min/max fl<))
(define flmax (make-min/max fl>))

(define (flabs x)
  (if (flnegative? x)
      (fl- x)
      x))

(define flexp (make-fl->fl exp))

(define (log* z)
  (cond
   ((= r5rs-inf+ z)
    r5rs-inf+)
   ((= r5rs-inf- z)
    r5rs-nan)
   ((not (= z z))
    r5rs-nan)
   (else
    (log z))))

(define fllog (make-fl->fl log*))
(define flsin (make-fl->fl sin))
(define flcos (make-fl->fl cos))
(define fltan (make-fl->fl tan))
(define flasin (make-fl->fl asin))
(define flacos (make-fl->fl acos))
(define flatan1 (make-fl->fl atan))
(define flatan2 (make-fl*fl->fl atan))

(define (flatan x . extra)
  (if (null? extra)
      (flatan1 x)
      (flatan2 x (car extra))))

(define (sqrt* z)
  (cond
   ((= r5rs-inf+ z)
    r5rs-inf+)
   ((= r5rs-inf- z)
    r5rs-nan)
   ((not (= z z))
    r5rs-nan)
   (else
    (sqrt z))))

(define flsqrt (make-fl->fl sqrt*))
(define flexpt (make-fl*fl->fl expt))

(define flfloor (make-fl->fl floor))
(define flceiling (make-fl->fl ceiling))
(define fltruncate (make-fl->fl truncate))
(define flround (make-fl->fl round))

(define (fixnum->flonum fx)
  (make-flonum (fixnum->r5rs fx)))

(define (flonum->fixnum f)
  (cond
   ((fl< f (fixnum->flonum (least-fixnum)))
    (least-fixnum))
   ((fl> f (fixnum->flonum (greatest-fixnum)))
    (greatest-fixnum))
   (else
    (r5rs->fixnum (inexact->exact (round (flonum-inexact f)))))))

(define flquotient (make-fl*fl->fl quotient))
(define flremainder (make-fl*fl->fl remainder))
(define (flquotient+remainder a b)
  (values (flquotient a b)
	  (flremainder a b)))
(define flmodulo (make-fl*fl->fl modulo))

(define (fldiv+mod x y)
  (let* ((div
	  (cond
	   ((flpositive? y)
	    (let ((n x)
		  (d y))
	      (if (flnegative? n)
		  (fl- (flquotient (fl- (fl- d n) (r5rs->fixnum 1.0)) d))
		  (flquotient n d))))
	   ((flzero? y)
	    (r5rs->flonum 0.0))
	   ((flnegative? y)
	    (let ((n (fl* (r5rs->fixnum -2) x))
		  (d (fl- y)))
	      (if (fl< n d)
		  (fl- (flquotient (fl- d n) (fl* (r5rs->flonum 2.0) d)))
		  (flquotient (fl+ n (fl+ d (r5rs->flonum -1.0)))
			      (fl* (r5rs->flonum 2.0) d)))))))
	 (mod
	  (fl- x (fl* div y))))
    (values div mod)))

(define (fldiv x y)
  (call-with-values
      (lambda () (fldiv+mod x y))
    (lambda (d m)
      d)))

(define (flmod x y)
  (call-with-values
      (lambda () (fldiv+mod x y))
    (lambda (d m)
      m)))

(define flodd? (make-fl->val odd?))
(define fleven? (make-fl->val even?))

(define flinteger? (make-fl->val integer?))

(define r5rs-inf+ 1e1025)
(define r5rs-inf- -1e1025)
(define r5rs-nan (- r5rs-inf+ r5rs-inf+))

(define flinf+ (make-flonum r5rs-inf+))
(define flinf- (make-flonum r5rs-inf-))

(define flnan (make-flonum r5rs-nan))

(define flnan? (make-fl->val (lambda (x) (not (= x x)))))

(define (infinite?* x)
  (or (= x r5rs-inf+)
      (= x r5rs-inf-)))

(define flinfinite? (make-fl->val infinite?*))

(define (finite?* x)
  (and (= x x)
       (not (infinite?* x))))

(define flfinite? (make-fl->val finite?*))
