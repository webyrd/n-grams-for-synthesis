;;
;; srfi-48 format test for Chez Scheme
;;

(import (rnrs))

;; srfi-0
(define-syntax cond-expand
  (syntax-rules (and or not else srfi-0 chezscheme)
    ((cond-expand) (syntax-error "Unfulfilled cond-expand"))
    ((cond-expand (else body ...))
     (begin body ...))
    ((cond-expand ((and) body ...) more-clauses ...)
     (begin body ...))
    ((cond-expand ((and req1 req2 ...) body ...) more-clauses ...)
     (cond-expand
      (req1
       (cond-expand
        ((and req2 ...) body ...)
        more-clauses ...))
      more-clauses ...))
    ((cond-expand ((or) body ...) more-clauses ...)
     (cond-expand more-clauses ...))
    ((cond-expand ((or req1 req2 ...) body ...) more-clauses ...)
     (cond-expand
      (req1
       (begin body ...))
      (else
       (cond-expand
        ((or req2 ...) body ...)
        more-clauses ...))))
    ((cond-expand ((not req) body ...) more-clauses ...)
     (cond-expand
      (req
       (cond-expand more-clauses ...))
      (else body ...)))
    ((cond-expand (srfi-0 body ...) more-clauses ...)
     (begin body ...))
    ((cond-expand (chezscheme body ...) more-clauses ...)
     (begin body ...))
    ((cond-expand (feature-id body ...) more-clauses ...)
     (cond-expand more-clauses ...))))

;; R6RS's 'error' needs a first argument 'who'.
(define error-orig error)
(define (error . args)
  (apply error-orig 'test args))

(define (write-with-shared-structure obj . options)
  (error "write-with-shared-structure is not supported"))

(include "test-tool.scm")
(include "srfi-48.scm")
(include "test-0001.scm")

(exit)

