#lang racket

;;
;; srfi-48 format test for Racket
;;

(require srfi/38) ; for write-with-shared-structure

;; We need to define 'if' because Racket's 'if' doesn't allow to omit
;; 'else' clause.
(define-syntax if
  (syntax-rules ()
    ((_ test then)
     (when test then))
    ((_ test then else1)
     (cond (test then) (else else1)))))

(include "test-tool.scm")
(include "srfi-48.scm")
(include "test-0001-Racket.scm")

