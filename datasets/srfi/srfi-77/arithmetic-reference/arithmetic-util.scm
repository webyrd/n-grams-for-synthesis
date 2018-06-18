; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Utilities for implementing higher-level arithmetic

(define (make-typo-op/2 proc type)
  (lambda (a b)
    (error "type mismatch" proc type a b)))

(define (never x)
  #f)

(define (always x)
  #t)

(define (make-typo-op/1 proc type)
  (lambda (a)
    (error "type mismatch" proc type a)))

(define (id x) x)

(define (one x)
  (r5rs->integer 1))

(define (one/flo x)
  (r5rs->flonum 1.0))

