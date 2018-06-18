;;
;; srfi-48 format test for Chicken
;;

(define (write-with-shared-structure obj . options)
  (error "write-with-shared-structure is not supported"))

(include "test-tool.scm")
(include "srfi-48.scm")
(include "test-0001-Chicken.scm")

(exit)

