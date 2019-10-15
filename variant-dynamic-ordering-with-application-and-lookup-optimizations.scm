(define *output-table-file-name* "tmp/variant-dynamic-ordering-with-application-and-lookup-optimizations-table.scm")

(define allow-incomplete-search? #f)

(define lookup-optimization? #t)

(load "mk-vicare.scm")
(load "mk.scm")
(load "test-check.scm")
(load "interp-app-optimization.scm")
(load "construct-ordering.scm")
(load "interp-simplified-dynamic.scm")
;(load "simplified-interp-tests.scm")
