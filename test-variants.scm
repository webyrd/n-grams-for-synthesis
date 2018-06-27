;; Run benchmarks for different interpreters

(printf "===== dynamic ordering with application optimization\n")
(system "scheme mk-vicare.scm mk.scm test-check.scm interp-app-optimization.scm construct-ordering.scm interp-simplified-dynamic.scm simplified-interp-tests.scm")

(printf "===== Barliman interpreter\n")
(system "scheme mk-vicare.scm mk.scm test-check.scm interp-barliman.scm simplified-interp-tests.scm")

(printf "===== expert ordering with application optimization\n")
(system "scheme mk-vicare.scm mk.scm test-check.scm interp-app-optimization.scm construct-ordering.scm interp-expert.scm simplified-interp-tests.scm")

(printf "===== dynamic ordering\n")
(system "scheme mk-vicare.scm mk.scm test-check.scm interp-core.scm construct-ordering.scm interp-simplified-dynamic.scm simplified-interp-tests.scm")

(printf "===== expert ordering\n")
(system "scheme mk-vicare.scm mk.scm test-check.scm interp-core.scm construct-ordering.scm interp-expert.scm simplified-interp-tests.scm")

(printf "===== old skool\n")
(system "scheme mk-vicare.scm mk.scm test-check.scm interp-old-style.scm simplified-interp-tests.scm")

(exit)

#|
(define evalo-files '("interp-simplified.scm" "interp-old-style.scm" "interp-old-style-with-list-as-prim.scm"))

(eval '(begin (load "mk-vicare.scm") (load "mk.scm") (load "test-check.scm") (load "interp-simplified.scm") (load "simplified-interp-tests.scm")) (copy-environment (scheme-environment)))

(eval 'evalo (copy-environment (scheme-environment)))

(module m1 ()
  (import scheme)
  (load "mk-vicare.scm")
  (load "mk.scm")
  (load "test-check.scm")
  (load "interp-simplified.scm")
  (load "simplified-interp-tests.scm"))

(system "scheme mk-vicare.scm mk.scm test-check.scm interp-simplified.scm simplified-interp-tests.scm")


;; could modify the test runner to print the output table as a s-expression, and then read from the output port
;; returned by process or open-process-ports
(process "scheme mk-vicare.scm mk.scm test-check.scm interp-simplified.scm simplified-interp-tests.scm")

(open-process-ports "scheme mk-vicare.scm mk.scm test-check.scm interp-simplified.scm simplified-interp-tests.scm")
|#
