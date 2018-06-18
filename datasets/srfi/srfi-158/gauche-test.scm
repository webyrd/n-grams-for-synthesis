;;
;; Just load this file to test on Gauche:
;;
;;  $ gosh ./gauche-test
;;

(use gauche.test)
(use compat.chibi-test)

(add-load-path "." :relative)

(test-start "srfi-158")
(use srfi-158)
(use scheme.base :only (string-for-each bytevector))
(use scheme.list :only (unfold))
(chibi-test (include "./chicken-test.scm"))

(test-end)


