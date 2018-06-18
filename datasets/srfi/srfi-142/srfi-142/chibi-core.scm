;;;; chibi-core.scm -- shim interfacing to C code
;; Stripped-down version of lib/srfi/33/bitwise.scm
;; Copyright (c) 2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (bitwise-not i) (- -1 i))

(define (make-nary proc2 default)
  (lambda args
    (if (null? args)
        default
        (let lp ((i (car args)) (ls (cdr args)))
          (if (null? ls)
              i
              (lp (proc2 i (car ls)) (cdr ls)))))))

(define bitwise-and  (make-nary bit-and  -1))
(define bitwise-ior  (make-nary bit-ior   0))
(define bitwise-xor  (make-nary bit-xor   0))
