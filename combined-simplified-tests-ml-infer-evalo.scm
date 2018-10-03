(test-runner
 ;; timeout in seconds
 10

  (test "simple-bool-1"
    (run* (expr type val)
      (== '#t expr)
      (!-/evalo expr type val))
    '(((#t bool #t))))

  (test "simple-bool-2"
    (run* (expr type val)
      (== '#f expr)
      (!-/evalo expr type val))
    '(((#f bool #f))))

  (test "simple-num-1"
    (run* (expr type val)
      (== '5 expr)
      (!-/evalo expr type val))
    '(((5 int 5))))

  (test "simple-nil-1"
    (run* (expr type val)
      (== 'nil expr)
      (!-/evalo expr type val))
    '(((nil (list _.0) nil))))

  (test "simple-cons-1"
    (run* (expr type val)
      (== '(cons 3 nil) expr)
      (!-/evalo expr type val))
    '((((cons 3 nil) (list int) (cons 3 nil)))))
  
  (test "simple-null?-1"
    (run* (expr type val)
      (== '(null? nil) expr)
      (!-/evalo expr type val))
    '((((null? nil) bool #t))))  

  (test "simple-null?-2"
    (run* (expr type val)
      (== '(null? (cons 3 nil)) expr)
      (!-/evalo expr type val))
    '((((null? (cons 3 nil)) bool #f))))

  (test "simple-car-1"
    (run* (expr type val)
      (== '(car (cons 3 (cons 4 nil))) expr)
      (!-/evalo expr type val))
    '((((car (cons 3 (cons 4 nil))) int 3))))

  (test "simple-cdr-1"
    (run* (expr type val)
      (== '(cdr (cons 3 (cons 4 nil))) expr)
      (!-/evalo expr type val))
    '((((cdr (cons 3 (cons 4 nil))) (list int) (cons 4 nil)))))

  (test "simple-pair-1"
    (run* (expr type val)
      (== '(pair 3 #t) expr)
      (!-/evalo expr type val))
    '((((pair 3 #t) (pair int bool) (pair 3 #t)))))

  (test "simple-if-1"
    (run* (expr type val)
      (== '(if (null? (cons 3 nil)) (pair 5 #t) (pair 6 #f)) expr)
      (!-/evalo expr type val))
    '((((if (null? (cons 3 nil)) (pair 5 #t) (pair 6 #f)) (pair int bool) (pair 6 #f)))))
  
  (test "simple-lambda-1"
    (run* (expr type val)
      (== '(lambda (x) x) expr)
      (!-/evalo expr type val))
    '?)

  (test "simple-app-0"
    (run* (expr type val)
      (== '((lambda (x) 6) 7) expr)
      (!-/evalo expr type val))
    '(((((lambda (x) 6) 7) int 6))))
  
  (test "simple-app-1"
    (run* (expr type val)
      (== '((lambda (x) 6) (cons 3 nil)) expr)
      (!-/evalo expr type val))
    '(((((lambda (x) 6) (cons 3 nil)) int 6))))
  
  (test "simple-app-2"
    (run* (expr type val)
      (== '((lambda (x) x) (cons 3 nil)) expr)
      (!-/evalo expr type val))
    '(((((lambda (x) x) (cons 3 nil)) (list int) (cons 3 nil)))))

  (test "simple-app-3"
    (run* (expr type val)
      (== '((lambda (x) (pair x x)) (cons 3 nil)) expr)
      (!-/evalo expr type val))
    '(((((lambda (x) (pair x x)) (cons 3 nil)) (pair (list int) (list int)) (pair (cons 3 nil) (cons 3 nil))))))
  
 )

(exit)
