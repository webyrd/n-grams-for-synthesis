(define *output-table-file-name* "tmp/variant-expert-ordering-ml-interp-and-infer-table.scm")

(define allow-incomplete-search? #f)

(define lookup-optimization? #f)

(load "mk-vicare.scm")
(load "mk.scm")
(load "test-check.scm")

(load "interp-core-ml.scm")
(load "construct-ordering-ml.scm")
(load "interp-expert-ml.scm")

(load "infer-core-ml.scm")
(load "construct-ordering-ml-infer.scm")
(load "infer-expert-ml.scm")

(load "simplified-interp-and-infer-tests-ml.scm")
