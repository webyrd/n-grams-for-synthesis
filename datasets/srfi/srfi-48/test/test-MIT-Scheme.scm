;;
;; srfi-48 format test for MIT/GNU Scheme
;;

(define (write-with-shared-structure obj . options)
  (error "write-with-shared-structure is not supported"))

(load "test-tool.scm")
(load "srfi-48.scm")
(with-output-to-file "test-MIT-Scheme-result.txt"
  (lambda ()
    (load "test-0001-MIT-Scheme.scm")))

(%exit 0) ; force to exit GUI

