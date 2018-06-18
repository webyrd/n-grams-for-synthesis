;;;; chicken-core, core bitwise operations for Chicken
;;; Copyright (C) 1991, 1993, 2001, 2003, 2005 Aubrey Jaffer
;;; Chicken Scheme provides bitwise-and, bitwise-ior, bitwise-xor, and
;;; arithmetic-shift as built-in operations.  The remaining core bitwise
;;; operations are provided here.
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.


(define (bitwise-not n) (- -1 n))

(define (logical:ash-4 x)
  (if (negative? x)
      (+ -1 (quotient (+ 1 x) 16))
      (quotient x 16)))

(define (logical:reduce op4 ident)
  (lambda args
    (do ((res ident (op4 res (car rgs) 1 0))
         (rgs args (cdr rgs)))
        ((null? rgs) res))))


(define integer-length
  (letrec ((intlen (lambda (n tot)
                     (case n
                       ((0 -1) (+ 0 tot))
                       ((1 -2) (+ 1 tot))
                       ((2 3 -3 -4) (+ 2 tot))
                       ((4 5 6 7 -5 -6 -7 -8) (+ 3 tot))
                       (else (intlen (logical:ash-4 n) (+ 4 tot)))))))
    (lambda (n) (intlen n 0))))

(define bit-count
  (letrec ((logcnt (lambda (n tot)
                     (if (zero? n)
                         tot
                         (logcnt (quotient n 16)
                                 (+ (vector-ref
                                     '#(0 1 1 2 1 2 2 3 1 2 2 3 2 3 3 4)
                                     (modulo n 16))
                                    tot))))))
    (lambda (n)
      (cond ((negative? n) (logcnt (bitwise-not n) 0))
            ((positive? n) (logcnt n 0))
            (else 0)))))
