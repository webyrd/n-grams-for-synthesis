; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Converting R5RS numbers to those of the reference implementation

(define (r5rs->number n)
  (cond
   ((exact? n)
    (cond
     ((integer? n)
      (r5rs->integer n))
     ((rational? n)
      (r5rs->ratnum n))
     ((complex? n)
      (r5rs->recnum n))
     (else #f)))
   
   ((real? n)
    (r5rs->flonum n))
   (else
    (r5rs->compnum n))))

      
   
		