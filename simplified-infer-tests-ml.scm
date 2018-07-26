(test-runner
 ;; timeout in seconds
 10

(test "23b"
  (run* (q) (type-expo `(lambda (y) (cons 5 y)) q))
  '(((-> ((list int)) (list int)))))

(test "5"
  (run* (q) (type-expo 3 q))
  '((int)))

(test "6"
  (run* (q) (type-expo #f q))
  '((bool)))

(test "8a"
  (run 1 (q) (type-expo `(cons 2 nil) q))
  '(((list int))))

(test "8b"
  (run 10 (q) (type-expo `(cons 2 nil) q))
  '(((list int))))

(test "8c"
  (run* (q) (type-expo `(cons 2 nil) q))
  '(((list int))))

(test "8"
  (run* (q) (type-expo `(cons 2 43) q))
  '())

(test "ext-gamma-mono*o-1"
  (run* (z t gamma)
    (ext-gamma-mono*o `(,z) `(,t) '() gamma))
  '(((_.0 _.1 ((_.0 (mono _.1)))) (sym _.0))))

(test "!-o-lookupo-1"
  (run* (z t gamma type)
    (ext-gamma-mono*o `(,z) `(,t) '() gamma)
    (lookup-!-o z gamma type))
  '(((_.0 _.1 ((_.0 (mono _.1))) _.1) (sym _.0))))

(test "id"
  (run* (q) (type-expo `(lambda (x) x) q))
  '(((-> (_.0) _.0))))

(test "9"
  (run* (q) (type-expo `((lambda (x) x) 3) q))
  '((int)))

 (test "!o-if-1"
  (run* (q)
    (fresh (expr)
      (== `(if (null? (cons 1 nil))
               3
               4)
          expr)
      (type-expo expr q)))
  '((int)))

(test "!o-if-2"
  (run* (q)
    (fresh (expr)
      (== `(if (null? nil)
               3
               4)
          expr)
      (type-expo expr q)))
  '((int)))

(test "10"
  (run* (q) (type-expo `(letrec ((f (lambda (y) #f)))
                          (cons (f 3) (f #f)))
                       q))
  '())

(test "11"
  (run* (q) (type-expo `(letrec ((f (lambda (y) y)))
                          (pair (f 3) (f #f)))
                       q))
  '(((pair int bool))))

(test "12"
  (run* (q) (type-expo `((lambda (f)
                           (pair (f 3) (@ f #f)))
                         (lambda (y) y))
                       q))
  '())

(test "13"
  (run* (q) (type-expo `((lambda (f)
                           (pair (f 3) (f 4)))
                         (lambda (y) y))
                       q))
  '(((pair int int))))

(test "14"
  (run* (q) (type-expo 'nil q))
  '(((list _.0))))

(test "16"
  (run* (q) (type-expo `(car nil) q))
  '((_.0)))

(test "19"
  (run* (q) (type-expo `(pair (cons 3 nil) (cons #f nil)) q))
  '(((pair (list int) (list bool)))))

(test "22"
  (run* (q) (type-expo `(lambda (f) (f f)) q))
  '())

(test "23a"
  (run* (q) (type-expo `(lambda (x y) (cons x y)) q))
  '(((-> (_.0 (list _.0)) (list _.0)))))

(test "23"
  (run* (q) (type-expo `(lambda (y) (cons #f y)) q))
  '(((-> ((list bool)) (list bool)))))

(test "24"
  (run* (q) (type-expo `(letrec ((x (lambda (y) (cons #f y)))) x)
                       q))
  '(((-> ((list bool)) (list bool)))))

(test-p "26"
  (run 1 (q) (type-expo `(lambda (x) ,q) `(-> (bool) bool)))
  (one-of?
    '((#t)
      ;;; hmmm--should ((x)) be considered correct?
      ;;;
      ;;; probably the best way to fix this is to have the formal parameters
      ;;; of lambda associated with types:
      ;;;
      ;;; `(lambda (x : bool) ,q)
      ((x)))))

(test "26b"
  (run 1 (expr)
    (fresh (body)
      (== `(lambda (x) ,body) expr)
      (type-expo expr `(-> (bool) bool))
      (type-expo expr `(-> (int) int))))
  '(((lambda (x) x))))

(test "27"
  (run* (q) (type-expo `(lambda (x) x) q))
  '(((-> (_.0) _.0))))

(test "17"
  (run* (q) (type-expo `(letrec ((append (lambda (l1)
                                           (lambda (l2)
                                             (if (null? l1)
                                                 l2
                                                 (cons (car l1) l2))))))
                          append)
                       q))
  '(((-> ((list _.0)) (-> ((list _.0)) (list _.0))))))

(test "18"
  (run* (q) (type-expo `(letrec ((append (lambda (l1)
                                           (lambda (l2)
                                             (if (null? l1)
                                                 l2
                                                 (cons (cons (car l1) nil)
                                                       (cons (cdr l1) l2)))))))
                          append)
                       q))
  '(((-> ((list _.0)) (-> ((list (list _.0))) (list (list _.0)))))))

(test "20"
  (run* (q) (type-expo `(letrec ((append (lambda (l1)
                                           (lambda (l2)
                                             (if (null? l1)
                                                 l2
                                                 (cons (car l1)
                                                       ((append (cdr l1)) l2)))))))
                          append)
                       q))
  '(((-> ((list _.0)) (-> ((list _.0)) (list _.0))))))

(test "21"
  (run* (q) (type-expo `(letrec ((append (lambda (l1)
                                           (lambda (l2)
                                             (if (null? l1)
                                                 l2
                                                 (cons (car l1)
                                                       ((append (cdr l1)) l2)))))))
                          (letrec ((rev (lambda (l1)
                                          (if (null? l1)
                                              l1
                                              ((append (rev (cdr l1))) (cons (car l1) nil))))))
                            rev))
                       q))
  '(((-> ((list _.0)) (list _.0)))))

(test "30"
  (run* (q) (type-expo `(letrec ((append (lambda (l1 l2)
                                           (if (null? l1)
                                               l2
                                               (cons (car l1)
                                                     (append (cdr l1) l2))))))
                          (letrec ((rev (lambda (l1)
                                          (if (null? l1)
                                              l1
                                              (append (rev (cdr l1)) (cons (car l1) nil))))))
                            rev))
                       q))
  '(((-> ((list _.0)) (list _.0)))))

 )

(exit)
