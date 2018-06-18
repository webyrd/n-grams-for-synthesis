;;;; Chicken module for SRFI 143

(module srfi-143 ()

  (import scheme)
  (import (only chicken include use let*-values))
  (use (rename (except numbers bit-set?)
               (exact-integer-sqrt fxsqrt)))

  (export fx-width fx-greatest fx-least)
  (export fixnum? fx=? fx<? fx>? fx<=? fx>=?
          fxzero? fxpositive? fxnegative?
          fxodd? fxeven? fxmax fxmin)
  (export fx+ fx- fxneg fx* fxquotient fxremainder
          fxabs fxsquare fxsqrt)
  (export fx+/carry fx-/carry fx*/carry)
  (export fxnot fxand fxior fxxor fxarithmetic-shift
          fxarithmetic-shift-left fxarithmetic-shift-right
          fxbit-count fxlength fxif fxbit-set? fxcopy-bit
          fxfirst-set-bit fxbit-field
          fxbit-field-rotate fxbit-field-reverse)

  (import (rename (only chicken
                        fxmax fxmin fx= fx< fx> fx<= fx>= fx/ fxmod
                        fxshl fxshr fixnum-bits
                        most-positive-fixnum most-negative-fixnum)
		  (fxmax chicken:fxmax)
		  (fxmin chicken:fxmin)
		  (fx= chicken:fx=)
		  (fx< chicken:fx<)
		  (fx> chicken:fx>)
		  (fx<= chicken:fx<=)
		  (fx>= chicken:fx>=)
		  (fx/ fxquotient)
		  (fxmod fxremainder)
                  (fxshl fxarithmetic-shift-left)
                  (fxshr fxarithmetic-shift-right)
		  (fixnum-bits fx-width)
		  (most-positive-fixnum fx-greatest)
		  (most-negative-fixnum fx-least)))
  (import (only chicken fx+ fx- fx* fxneg fxand fxior fxxor
                        fxnot fxodd? fxeven? fixnum?))

  ;; Core functions not available in Chicken

  (define (logical:ash-4 x)
    (if (negative? x)
        (+ -1 (quotient (+ 1 x) 16))
        (quotient x 16)))

  (define fxlength
    (letrec ((intlen (lambda (n tot)
                       (case n
                         ((0 -1) (fx+ 0 tot))
                         ((1 -2) (fx+ 1 tot))
                         ((2 3 -3 -4) (fx+ 2 tot))
                         ((4 5 6 7 -5 -6 -7 -8) (fx+ 3 tot))
                         (else (intlen (logical:ash-4 n) (fx+ 4 tot)))))))
      (lambda (n) (intlen n 0))))

  (define fxbit-count
    (letrec ((logcnt (lambda (n tot)
                       (if (fxzero? n)
                           tot
                           (logcnt (fxquotient n 16)
                                   (fx+ (vector-ref
                                       '#(0 1 1 2 1 2 2 3 1 2 2 3 2 3 3 4)
                                       (fxremainder n 16))
                                      tot))))))
      (lambda (n)
        (cond ((fxnegative? n) (logcnt (fxnot n) 0))
              ((fxpositive? n) (logcnt n 0))
              (else 0)))))

  (include "carries.scm")
  (include "srfi-143-impl.scm")
)
