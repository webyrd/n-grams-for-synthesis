(define *output-table-file-name* "tmp/variant-expert-ordering-with-application-and-lookup-optimizations-table.scm")

(define allow-incomplete-search? #f)

(define lookup-optimization? #t)

(load "mk-vicare.scm")
(load "mk.scm")
(load "test-check.scm")
(load "interp-app-optimization.scm")
(load "construct-ordering.scm")
(load "interp-expert.scm")
(load "simplified-interp-tests.scm")
