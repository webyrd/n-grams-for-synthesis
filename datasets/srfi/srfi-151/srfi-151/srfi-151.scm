;;;; chicken implementation of SRFI 151
(module srfi-151 ()
  (import scheme)
  (import (only chicken include use case-lambda when))

  ;; Provides bitwise-not, bitwise-and, butwise-ior, bitwise-xor,
  ;; arithmetic-shift, integer-length.  The remaining
  ;; core function, bit-count, is provided in this file.
  (use numbers)

  (export bitwise-not bitwise-and bitwise-ior bitwise-xor bitwise-eqv
          bitwise-nand bitwise-nor bitwise-andc1 bitwise-andc2
          bitwise-orc1 bitwise-orc2)
  (export arithmetic-shift bit-count integer-length bitwise-if 
          bit-set? copy-bit bit-swap any-bit-set? every-bit-set?  first-set-bit)
  (export bit-field bit-field-any? bit-field-every?  bit-field-clear bit-field-set
          bit-field-replace  bit-field-replace-same
          bit-field-rotate bit-field-reverse)
  (export bits->list list->bits bits->vector vector->bits bits
          bitwise-fold bitwise-for-each bitwise-unfold make-bitwise-generator)

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

  (include "bitwise-33.scm")
  (include "bitwise-60.scm")
  (include "bitwise-other.scm")
)
