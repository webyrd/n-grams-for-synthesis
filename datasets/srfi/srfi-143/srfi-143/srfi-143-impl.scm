;;;; Procedures not provided by Chicken or by rubber-chicken.

;;; Implementations of arithmetic functions

(define (fx=? i j . ks)
  (if (null? ks)
    (chicken:fx= i j)
    (and (chicken:fx= i j) (apply fx=? j ks))))

(define (fx<? i j . ks)
  (if (null? ks)
    (chicken:fx< i j)
    (and (chicken:fx< i j) (apply fx<? j ks))))

(define (fx>? i j . ks)
  (if (null? ks)
    (chicken:fx> i j)
    (and (chicken:fx> i j) (apply fx>? j ks))))

(define (fx<=? i j . ks)
  (if (null? ks)
    (chicken:fx<= i j)
    (and (chicken:fx<= i j) (apply fx<=? j ks))))

(define (fx>=? i j . ks)
  (if (null? ks)
    (chicken:fx>= i j)
    (and (chicken:fx>= i j) (apply fx>=? j ks))))

(define (fxzero? i) (chicken:fx= i 0))
(define (fxpositive? i) (chicken:fx> i 0))
(define (fxnegative? i) (chicken:fx< i 0))

(define (fxmax i j . ks)
  (if (null? ks)
    (chicken:fxmax i j)
    (chicken:fxmax (chicken:fxmax i j) (apply fxmax j ks))))

(define (fxmin i j . ks)
  (if (null? ks)
    (chicken:fxmin i j)
    (chicken:fxmin (chicken:fxmin i j) (apply fxmin j ks))))

(define (fxabs i)
  (if (fxnegative? i) (fxneg i) i))

(define (fxsquare i) (fx* i i))

(define (fxarithmetic-shift i count)
  (if (negative? count)
    (fxarithmetic-shift-right i (- count))
    (fxarithmetic-shift-left i count)))

;;; Bitwise functions cloned from SRFI 151, fixnum version

;; Helper function
(define (mask start end) (fxnot (fxarithmetic-shift-left -1 (- end start))))

(define (fxif mask n0 n1)
  (fxior (fxand mask n0)
          (fxand (fxnot mask) n1)))

(define (fxbit-set? index n)
  (not (fxzero? (fxand (fxarithmetic-shift-left 1 index) n))))

(define (fxcopy-bit index to bool)
  (if bool
      (fxior to (fxarithmetic-shift-left 1 index))
      (fxand to (fxnot (fxarithmetic-shift-left 1 index)))))

(define (fxfirst-set-bit i) (- (fxbit-count (fxxor i (- i 1))) 1))

(define (fxbit-field n start end)
  (fxand (mask start end) (fxarithmetic-shift n (- start))))

(define (fxbit-field-rotate n count start end)
  (define width (fx- end start))
  (set! count (modulo count width))
  (let ((mask (fxnot (fxarithmetic-shift -1 width))))
    (define zn (fxand mask (fxarithmetic-shift n (- start))))
    (fxior (fxarithmetic-shift
             (fxior (fxand mask (fxarithmetic-shift zn count))
                     (fxarithmetic-shift zn (- count width)))
             start)
            (fxand (fxnot (fxarithmetic-shift mask start)) n))))

(define (fxreverse k n)
  (do ((m (if (negative? n) (fxnot n) n) (fxarithmetic-shift-right m 1))
       (k (fx+ -1 k) (fx+ -1 k))
       (rvs 0 (fxior (fxarithmetic-shift-left rvs 1) (fxand 1 m))))
      ((fxnegative? k) (if (fxnegative? n) (fxnot rvs) rvs))))

(define (fxbit-field-reverse n start end)
  (define width (- end start))
  (let ((mask (fxnot (fxarithmetic-shift-left -1 width))))
    (define zn (fxand mask (fxarithmetic-shift-right n start)))
    (fxior (fxarithmetic-shift-left (fxreverse width zn) start)
            (fxand (fxnot (fxarithmetic-shift-left mask start)) n))))
 
