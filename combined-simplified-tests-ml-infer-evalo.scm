(test-runner
 ;; timeout in seconds
 10

 (test "simple-int-1"
    (run 1 (expr type val)
      (== '5 expr)
      (!-/evalo expr type val))
    '?)

  (test "simple-bool-1"
    (run 1 (expr type val)
      (== '#t expr)
      (!-/evalo expr type val))
    '?)

  (test "simple-bool-2"
    (run 1 (expr type val)
      (== '#f expr)
      (!-/evalo expr type val))
    '?)
 
  (test "simple-lambda-1"
    (run 1 (expr type val)
      (== '(lambda (x) x) expr)
      (!-/evalo expr type val))
    '?)

 )

(exit)
