(define *output-table-file-name* "tmp/variant-dynamic-ordering-with-application-and-lookup-optimizations-ml-table.scm")

(define allow-incomplete-search? #f)

(define lookup-optimization? #t)

(load "mk-vicare.scm")
(load "mk.scm")
(load "test-check.scm")
(load "interp-app-optimization-ml.scm")
(load "construct-ordering-ml.scm")
(load "interp-simplified-dynamic-ml.scm")
(load "simplified-interp-tests-ml.scm")
