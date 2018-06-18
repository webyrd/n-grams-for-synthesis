(cond-expand
 (gauche
  (use gauche.test)
  (use compat.chibi-test)
  (use srfi-121)
  (test-start "ideque")
  (add-load-path "." :relative)
  (use ideque)
  (test-module 'ideque)
  (chibi-test
   (include "ideque-tests.scm"))
  (test-end))
 (else
  (display "Add implementation-specific test runner.\n")))
