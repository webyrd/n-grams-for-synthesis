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

  (test "simple-app-4"
    (run* (expr type val)
      (== '((lambda (x y z) (pair (car x) (pair (car y) (car z)))) (cons 3 nil) (cons 4 nil) (cons 5 nil)) expr)
      (!-/evalo expr type val))
    '(((((lambda (x y z)
           (pair (car x) (pair (car y) (car z))))
         (cons 3 nil)
         (cons 4 nil)
         (cons 5 nil))
        (pair int (pair int int))
        (pair 3 (pair 4 5))))))

  (test "simple-letrec-1"
    (run* (expr type val)
      (== '(letrec ((f (lambda (x) x)))
             (f (cons 3 nil)))
          expr)
      (!-/evalo expr type val))
    '((((letrec ((f (lambda (x) x))) (f (cons 3 nil))) (list int) (cons 3 nil)))))

  (test "simple-letrec-2"
    (run* (expr type val)
      (== '(letrec ((append (lambda (l s)
                              (if (null? l)
                                  s
                                  (cons (car l)
                                        (append (cdr l) s))))))
             (list
              (append nil nil)
              (append (cons 1 nil) (cons 2 nil))
              (append (cons 3 (cons 4 nil)) (cons 5 (cons 6 nil)))))
          expr)
      (!-/evalo expr type val))
    '((((letrec ((append (lambda (l s) (if (null? l) s (cons (car l) (append (cdr l) s)))))) (list (append nil nil) (append (cons 1 nil) (cons 2 nil)) (append (cons 3 (cons 4 nil)) (cons 5 (cons 6 nil)))))
        (list (list int))
        (nil
         (cons 1 (cons 2 nil))
         (cons 3 (cons 4 (cons 5 (cons 6 nil)))))))))

  (test "append-eval-only-15"
    (run 1 (expr type val)
      (fresh (q r s t prog)
        (absento 1 prog)
        (absento 2 prog)
        (absento 3 prog)
        (absento 4 prog)
        (absento 5 prog)
        (absento 6 prog)
        (== `(lambda (l s)
               (if ,q
                   ,r
                   (,s (car l) (append ,t s))))
            prog)
        (== `(letrec ((append ,prog))
               (list
                (append nil nil)
                (append (cons 1 nil) (cons 2 nil))
                (append (cons 3 (cons 4 nil)) (cons 5 (cons 6 nil)))))
            expr)
        (== '(nil
              (cons 1 (cons 2 nil))
              (cons 3 (cons 4 (cons 5 (cons 6 nil)))))
            val)
        (!-/evalo expr type val)))
    '((((letrec ((append (lambda (l s) (if (null? l) s (cons (car l) (append (cdr l) s)))))) (list (append nil nil) (append (cons 1 nil) (cons 2 nil)) (append (cons 3 (cons 4 nil)) (cons 5 (cons 6 nil)))))
        (list (list int))
        (nil (cons 1 (cons 2 nil)) (cons 3 (cons 4 (cons 5 (cons 6 nil)))))))))

  (test "append-eval-only-16"
    (run 1 (expr type val)
      (fresh (q r s t u prog)
        (absento 1 prog)
        (absento 2 prog)
        (absento 3 prog)
        (absento 4 prog)
        (absento 5 prog)
        (absento 6 prog)
        (== `(lambda (l s)
               (if ,q
                   ,r
                   (,s (car l) (append ,t ,u))))
            prog)
        (== `(letrec ((append ,prog))
               (list
                (append nil nil)
                (append (cons 1 nil) (cons 2 nil))
                (append (cons 3 (cons 4 nil)) (cons 5 (cons 6 nil)))))
            expr)
        (== '(nil
              (cons 1 (cons 2 nil))
              (cons 3 (cons 4 (cons 5 (cons 6 nil)))))
            val)
        (!-/evalo expr type val)))
    '???)

  (test "append-eval-only-17"
    (run 1 (expr type val)
      (fresh (q r s t prog)
        (absento 1 prog)
        (absento 2 prog)
        (absento 3 prog)
        (absento 4 prog)
        (absento 5 prog)
        (absento 6 prog)
        (== `(lambda (l s)
               (if ,q
                   ,r
                   (,s (car l) ,t)))
            prog)
        (== `(letrec ((append ,prog))
               (list
                (append nil nil)
                (append (cons 1 nil) (cons 2 nil))
                (append (cons 3 (cons 4 nil)) (cons 5 (cons 6 nil)))))
            expr)
        (== '(nil
              (cons 1 (cons 2 nil))
              (cons 3 (cons 4 (cons 5 (cons 6 nil)))))
            val)
        (!-/evalo expr type val)))
    '((((letrec ((append (lambda (l s) (if (null? l) s (cons (car l) (append (cdr l) s)))))) (list (append nil nil) (append (cons 1 nil) (cons 2 nil)) (append (cons 3 (cons 4 nil)) (cons 5 (cons 6 nil)))))
        (list (list int))
        (nil (cons 1 (cons 2 nil)) (cons 3 (cons 4 (cons 5 (cons 6 nil)))))))))

  (test "append-eval-only-18"
     (run 1 (expr type val)
       (fresh (q r s t prog)
         (absento 1 prog)
         (absento 2 prog)
         (absento 3 prog)
         (absento 4 prog)
         (absento 5 prog)
         (absento 6 prog)
         (== `(lambda (l s)
                (if ,q
                    ,r
                    ,s))
             prog)
         (== `(letrec ((append ,prog))
                (list
                 (append nil nil)
                 (append (cons 1 nil) (cons 2 nil))
                 (append (cons 3 (cons 4 nil)) (cons 5 (cons 6 nil)))))
             expr)
         (== '(nil
               (cons 1 (cons 2 nil))
               (cons 3 (cons 4 (cons 5 (cons 6 nil)))))
             val)
         (!-/evalo expr type val)))
     '((((letrec ((append (lambda (l s) (if (null? l) s (cons (car l) (append (cdr l) s)))))) (list (append nil nil) (append (cons 1 nil) (cons 2 nil)) (append (cons 3 (cons 4 nil)) (cons 5 (cons 6 nil)))))
        (list (list int))
        (nil (cons 1 (cons 2 nil)) (cons 3 (cons 4 (cons 5 (cons 6 nil)))))))))

  (test "append-eval-only-18-extra-example"
     (run 1 (expr type val)
       (fresh (q r s t prog)
         (absento 1 prog)
         (absento 2 prog)
         (absento 3 prog)
         (absento 4 prog)
         (absento 5 prog)
         (absento 6 prog)
         (absento 7 prog)
         (absento 8 prog)
         (absento 9 prog)
         (absento 10 prog)
         (absento 11 prog)
         (absento 12 prog)
         (== `(lambda (l s)
                (if ,q
                    ,r
                    ,s))
             prog)
         (== `(letrec ((append ,prog))
                (list
                 (append nil nil)
                 (append (cons 1 nil) (cons 2 nil))
                 (append (cons 3 (cons 4 nil)) (cons 5 (cons 6 nil)))
                 (append (cons 7 (cons 8 (cons 9 nil))) (cons 10 (cons 11 (cons 12 nil))))))
             expr)
         (== '(nil
               (cons 1 (cons 2 nil))
               (cons 3 (cons 4 (cons 5 (cons 6 nil))))
               (cons 7 (cons 8 (cons 9 (cons 10 (cons 11 (cons 12 nil)))))))
             val)
         (!-/evalo expr type val)))
     '((((letrec ((append (lambda (l s) (if (null? l) s (cons (car l) (append (cdr l) s)))))) (list (append nil nil) (append (cons 1 nil) (cons 2 nil)) (append (cons 3 (cons 4 nil)) (cons 5 (cons 6 nil)))))
        (list (list int))
        (nil (cons 1 (cons 2 nil)) (cons 3 (cons 4 (cons 5 (cons 6 nil)))))))))

   (test "append-eval-only-19"
     (run 1 (expr type val)
       (fresh (q r s t prog)
         (absento 1 prog)
         (absento 2 prog)
         (absento 3 prog)
         (absento 4 prog)
         (absento 5 prog)
         (absento 6 prog)
         (== `(lambda (l s)
                ,q)
             prog)
         (== `(letrec ((append ,prog))
                (list
                 (append nil nil)
                 (append (cons 1 nil) (cons 2 nil))
                 (append (cons 3 (cons 4 nil)) (cons 5 (cons 6 nil)))))
             expr)
         (== '(nil
               (cons 1 (cons 2 nil))
               (cons 3 (cons 4 (cons 5 (cons 6 nil)))))
             val)
         (!-/evalo expr type val)))
     '((((letrec ((append (lambda (l s) (if (null? l) s (cons (car l) (append (cdr l) s)))))) (list (append nil nil) (append (cons 1 nil) (cons 2 nil)) (append (cons 3 (cons 4 nil)) (cons 5 (cons 6 nil)))))
        (list (list int))
        (nil (cons 1 (cons 2 nil)) (cons 3 (cons 4 (cons 5 (cons 6 nil)))))))))
  
 )

(exit)
