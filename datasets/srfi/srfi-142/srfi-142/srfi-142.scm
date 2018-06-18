;;;; chicken implementation of SRFI 142
(module srfi-142 ()
  (import scheme)
  (import (only chicken include use case-lambda when
                        bitwise-and bitwise-ior bitwise-xor
                        arithmetic-shift))
  (use numbers)

  (export bitwise-not bitwise-and bitwise-ior bitwise-xor bitwise-eqv
          bitwise-nand bitwise-nor bitwise-andc1 bitwise-andc2
          bitwise-orc1 bitwise-orc2)
  (export arithmetic-shift bit-count integer-length bitwise-if 
          bit-set? copy-bit bit-swap any-bit-set? every-bit-set?  first-set-bit)
  (export bit-field bit-field-any? bit-field-every?  bit-field-clear bit-field-set
          bit-field-replace  bit-field-replace-same
          bit-field-rotate bit-field-reverse)
  (export integer->list list->integer integer->vector vector->integer bits
          bitwise-fold bitwise-for-each bitwise-unfold make-bitwise-generator)

  (include "chicken-core.scm")
  (include "bitwise-33.scm")
  (include "bitwise-60.scm")
  (include "bitwise-other.scm")
)
