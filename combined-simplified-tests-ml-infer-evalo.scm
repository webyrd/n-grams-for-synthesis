(test-runner
 ;; timeout in seconds
 10

  (test "simple-bool-1"
    (run 1 (expr type val)
      (== '#t expr)
      (!-/evalo expr type val))
    '(((#t bool #t))))

  (test "simple-bool-2"
    (run 1 (expr type val)
      (== '#f expr)
      (!-/evalo expr type val))
    '(((#f bool #f))))

  (test "simple-num-1"
    (run 1 (expr type val)
      (== '5 expr)
      (!-/evalo expr type val))
    '(((5 int 5))))

  (test "simple-nil-1"
    (run 1 (expr type val)
      (== 'nil expr)
      (!-/evalo expr type val))
    '(((nil (list _.0) nil))))

  (test "simple-cons-1"
    (run 1 (expr type val)
      (== '(cons 3 nil) expr)
      (!-/evalo expr type val))
    '((((cons 3 nil) (list int) (cons 3 nil)))))
  
  (test "simple-null?-1"
    (run 1 (expr type val)
      (== '(null? nil) expr)
      (!-/evalo expr type val))
    '((((null? nil) bool #t))))  

  (test "simple-null?-2"
    (run 1 (expr type val)
      (== '(null? (cons 3 nil)) expr)
      (!-/evalo expr type val))
    '((((null? (cons 3 nil)) bool #f))))
  
  (test "simple-lambda-1"
    (run 1 (expr type val)
      (== '(lambda (x) x) expr)
      (!-/evalo expr type val))
    '?)
  
 )

(exit)
