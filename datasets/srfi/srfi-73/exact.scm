(require (lib "9.ss" "srfi"))
(require (lib "13.ss" "srfi"))
(require (lib "16.ss" "srfi"))
(require (lib "23.ss" "srfi"))

(define-record-type exact
  (make-exact sign numerator denominator)
  my-exact?
  (sign exact-sign)
  (numerator exact-numerator)
  (denominator exact-denominator))
;sign is either +1 or -1, numerator and denominator
;are both exact, non-negative, and relatively prime integers.

(define |-1/0| (make-exact -1 1 0))
(define |1/0| (make-exact 1 1 0))
(define |-0| (make-exact -1 0 1))

(define (infinite? z)
  (= (exact-denominator z) 0))

(define (exact->value z)
  (/ (exact-numerator z)
     (exact-denominator z)
     (exact-sign z)))
(define (value->exact z)
  (if (negative? z)
      (make-exact -1
                  (numerator (- z))
                  (denominator z))
      (make-exact 1
                  (numerator z)
                  (denominator z))))

(define (string->exact string)
  (if (eqv? (string-ref string 0) #\-)
      (positive-string->exact -1 (string-drop string 1))
      (positive-string->exact 1 string)))
(define (positive-string->exact sign string)
  (let ((position-of/ (string-index string #\/)))
    (if position-of/
        (let ((numerator
               (string->number (string-take string position-of/)))
              (denominator
               (string->number (string-drop string (+ 1 position-of/)))))
          (let ((cd (gcd numerator denominator)))
            (make-exact sign (/ numerator cd) (/ denominator cd))))
        (make-exact sign (string->number string) 1))))
(define (exact->string z)
  (string-append (if (= 1 (exact-sign z))
                     ""
                     "-")
                 (number->string
                  (exact-numerator z))
                 (if (= 1 (exact-denominator z))
                     ""
                     (string-append "/"
                                    (number->string
                                     (exact-denominator z))))))

(define my-rational? my-exact?)
(define (my-integer? obj)
  (and (my-exact? obj)
       (= 1 (exact-denominator obj))))

(define (binary= z1 z2)
  (and (= (exact-sign z1)
          (exact-sign z2))
       (= (exact-numerator z1)
          (exact-numerator z2))
       (= (exact-denominator z1)
          (exact-denominator z2))))
(define (binary< z1 z2)
  (or (and (= -1 (exact-sign z1))
           (= 1 (exact-sign z2)))
      (if (binary= z1 |-1/0|)
          (not (binary= z2 |-1/0|))
          (if (binary= z2 |1/0|)
              (not (binary= z1 |1/0|))
              (and (not (binary= z1 |1/0|))
                   (not (binary= z2 |-1/0|))
                   (< (exact->value z1)
                      (exact->value z2)))))))
(define (binary> z1 z2)
  (binary< z2 z1))
(define (binary<= z1 z2)
  (or (binary< z1 z2)
      (binary= z1 z2)))
(define (binary>= z1 z2)
  (or (binary> z1 z2)
      (binary= z1 z2)))

(define (binary-compare->n-ary compare)
  (letrec ((self
            (lambda (z1 z2 . z3)
              (if (null? z3)
                  (compare z1 z2)
                  (and (compare z1 z2)
                       (apply self z2 z3))))))
    self))
(define my= (binary-compare->n-ary binary=))
(define my< (binary-compare->n-ary binary<))
(define my> (binary-compare->n-ary binary>))
(define my<= (binary-compare->n-ary binary<=))
(define my>= (binary-compare->n-ary binary>=))

(define (my-zero? z)
  (= (exact-numerator z) 0))
(define (my-positive? z)
  (and (= 1 (exact-sign z))
       (not (my-zero? z))))
(define (my-negative? z)
  (and (= -1 (exact-sign z))
       (not (my-zero? z))))

(define (binary-max x1 x2)
  (if (my< x1 x2) x2 x1))
(define (binary-min x1 x2)
  (if (my< x1 x2) x1 x2))
(define (binary-extremum->n-ary extremum)
  (letrec ((self
            (lambda (x1 . x2)
              (if (null? x2)
                  x1
                  (apply self (extremum x1 (car x2)) (cdr x2))))))
    self))
(define my-max (binary-extremum->n-ary binary-max))
(define my-min (binary-extremum->n-ary binary-min))

(define (binary+ z1 z2)
  (if (infinite? z1)
      (if (infinite? z2)
          (if (binary= z1 z2)
              z1
              (error "(- 1/0 1/0)") ;or #i0/0
              )
          z1)
      (if (infinite? z2)
          z2
          (if (= 1 (* (exact-sign z1)
                      (exact-sign z2)))
              (let ((a (exact-numerator z1))
                    (b (exact-denominator z1))
                    (c (exact-numerator z2))
                    (d (exact-denominator z2)))
                (let ((n (+ (* a d) (* b c)))
                      (d (* b d)))
                  (let ((cd (gcd n d)))
                    (make-exact (exact-sign z1)
                                (/ n cd)
                                (/ d cd)))))
              (value->exact (+ (exact->value z1)
                               (exact->value z2)))))))
(define (binary* z1 z2)
  (let ((n (* (exact-numerator z1) (exact-numerator z2)))
        (d (* (exact-denominator z1) (exact-denominator z2))))
    (if (= n d 0)
        (error "(* 0 1/0)") ;or #i0/0
        (let ((cd (gcd n d)))
          (make-exact (* (exact-sign z1)
                         (exact-sign z2))
                      (/ n cd)
                      (/ d cd))))))
(define (binary-op->n-ary op base)
  (letrec ((self
            (case-lambda
              (() base)
              ((z) z)
              ((z1 . z2)
               (apply self (op z1 (car z2)) (cdr z2))))))
    self))
(define my+ (binary-op->n-ary binary+ (make-exact 1 0 1)))
(define my* (binary-op->n-ary binary* (make-exact 1 1 1)))

(define my-
  (case-lambda
    ((z)
     (make-exact (- (exact-sign z))
                 (exact-numerator z)
                 (exact-denominator z)))
    ((z1 . z2)
     (apply my+ z1 (map my- z2)))))
(define my/
  (case-lambda
    ((z)
     (make-exact (exact-sign z)
                 (exact-denominator z)
                 (exact-numerator z)))
    ((z1 . z2)
     (apply my* z1 (map my/ z2)))))

(define (my-abs x)
  (make-exact 1
              (exact-numerator x)
              (exact-denominator x)))

#|
odd?
even?
quotient
remainder
modulo
gcd
lcm
rationalize

are unchanged in this SRFI, so they could be defined as

(define (my-quotient n1 n2)
  (value->exact (quotient (exact->value n1)
                          (exact->value n2))))
...
|#

(define (my-numerator q)
  (* (exact-sign q)
     (exact-numerator q)))
(define my-denominator exact-denominator)

(define (my-floor x)
  (if (my-integer? x)
      x
      (if (my-positive? x)
          (my-truncate x)
          (my- (my-truncate x)
               (make-exact 1 1 1)))))
(define (my-ceiling x)
  (my- (my-floor (my- x))))
(define (my-truncate x)
  (if (infinite? x)
      x
      (make-exact (exact-sign x)
                  (quotient (exact-numerator x)
                            (exact-denominator x))
                  1)))
(define (my-round x)
  (cond ((infinite? x) x)
        ((= 1 (exact-sign x))
         (value->exact (round (exact->value x))))
        (else
         (my- (value->exact (round (exact->value (my- x))))))))