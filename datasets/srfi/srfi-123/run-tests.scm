(import (scheme base)
        (scheme eval)
        (scheme process-context))

(if (eval '(run-tests) (environment '(tests srfi-123)))
    (exit 0)
    (exit 1))
