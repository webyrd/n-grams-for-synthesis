(define *output-table-file-name* "tmp/variant-dynamic-ordering-with-application-and-lookup-optimizations-ml-infer-table.scm")

(define allow-incomplete-search? #f)

(define lookup-optimization? #t)

(load "mk-vicare.scm")
(load "mk.scm")
(load "test-check.scm")
(load "infer-app-optimization-ml.scm")
(load "construct-ordering-ml-infer.scm")
(load "infer-simplified-dynamic-ml.scm")
(load "simplified-infer-tests-ml.scm")
