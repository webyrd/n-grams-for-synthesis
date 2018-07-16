(define *output-table-file-name* "tmp/variant-dynamic-ordering-with-application-optimization-incomplete-table.scm")

(define allow-incomplete-search? #t)

(define lookup-optimization? #f)

(load "mk-vicare.scm")
(load "mk.scm")
(load "test-check.scm")
(load "interp-app-optimization.scm")
(load "construct-ordering.scm")
(load "interp-simplified-dynamic.scm")
(load "simplified-interp-tests.scm")
