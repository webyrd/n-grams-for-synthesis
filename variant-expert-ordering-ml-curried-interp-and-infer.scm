(define *output-table-file-name* "tmp/variant-expert-ordering-ml-curried-interp-and-infer-table.scm")

(define allow-incomplete-search? #f)

(define lookup-optimization? #f)

(load "mk-vicare.scm")
(load "mk.scm")
(load "test-check.scm")

(load "interp-core-ml-curried.scm")
(load "construct-ordering-ml-curried.scm")
(load "interp-expert-ml-curried.scm")

(load "infer-core-ml-curried.scm")
(load "construct-ordering-ml-infer-curried.scm")
(load "infer-expert-ml-curried.scm")

(load "simplified-interp-and-infer-tests-ml-curried.scm")
