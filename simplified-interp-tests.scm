;(load "mk-vicare.scm")
;(load "mk.scm")
;(load "test-check.scm")
;(load "interp-simplified.scm")

(test-runner
 ;; timeout in seconds
 5 


(test "quote-and-shadowing-works-1"
  (run* (q)
    (evalo
     `(letrec ((append
                (lambda (l s)
                  (if ((lambda (x) (x #f)) (lambda (y) (quote #t)))
                      s
                      (cons (car l) (append (cdr l) s))))))
        (append '(a b c) '(d e)))
     q))
  '(((d e))))

(test "quote-and-shadowing-works-2"
  (run* (q)
    (evalo
     `(letrec ((append
                (lambda (l s)
                  (if ((lambda (quote) (quote #f)) (lambda (y) (quote #t)))
                      s
                      (cons (car l) (append (cdr l) s))))))
        (append '(a b c) '(d e)))
     q))
  (if *primitives-first-class-and-shadowable?*
      '(((d e)))
      '()))



 ;; match tests
 (test "match-0"
        (run* (q) (evalo '(match '(1 2 3 1 2)
                            [`(,x ,y ,z ,x ,y)
                             (list x y)])
                         q))
        '(((1 2))))

 (test "match-1"
        (run* (q) (evalo '(match '(1 2 3 2 1)
                            [`(,x ,y ,z ,x ,y)
                             (list x y)])
                         q))
        '())

 (test "match-2"
        (run* (q) (evalo '(match '(1)
                            [`(,quote)
                             5])
                         q))
        (if *primitives-first-class-and-shadowable?*
            '((5))
            '()))


 ;; if tests
 (test "if-0"
        (run* (q) (evalo '(if #t 5 6) q))
        '((5)))

 (test "if-1"
        (run* (q) (evalo '(if #f 5 6) q))
        '((6)))

 (test "if-2"
        (run* (q) (evalo '(if 4 5 6) q))
        (if *if-test-requires-boolean?*
            '()
            '((5))))


 ;; and tests
 (test "and-0"
        (run* (q) (evalo '(and) q))
        '((#t)))

 (test "and-1"
        (run* (q) (evalo '(and 5) q))
        '((5)))

 (test "and-2"
        (run* (q) (evalo '(and #f) q))
        '((#f)))

 (test "and-3"
        (run* (q) (evalo '(and 5 6) q))
        '((6)))

(test "and-4"
  (run* (q) (evalo '(and #f 6) q))
  '((#f)))

(test "and-5"
  (run* (q) (evalo '(and (null? '()) 6) q))
  '((6)))

(test "and-6"
  (run* (q) (evalo '(and (null? '(a b c)) 6) q))
  '((#f)))


;; or tests
(test "or-0"
  (run* (q) (evalo '(or) q))
  '((#f)))

(test "or-1"
  (run* (q) (evalo '(or 5) q))
  '((5)))

(test "or-2"
  (run* (q) (evalo '(or #f) q))
  '((#f)))

(test "or-3"
  (run* (q) (evalo '(or 5 6) q))
  '((5)))

(test "or-4"
  (run* (q) (evalo '(or #f 6) q))
  '((6)))

(test "or-5"
  (run* (q) (evalo '(or (null? '()) 6) q))
  '((#t)))

(test "or-6"
  (run* (q) (evalo '(or (null? '(a b c)) 6) q))
  '((6)))


  (test "proof-1"
    (run* (q)
      (evalo
       `(letrec ((member? (lambda (x ls)
                            (if (null? ls)
                                #f
                                (if (equal? (car ls) x)
                                    #t
                                    (member? x (cdr ls)))))))
          (letrec ((proof? (lambda (proof)
                             (match proof
                               [`(assumption ,assms () ,A)
                                (member? A assms)]
                               [`(modus-ponens
                                  ,assms
                                  ((,r1 ,assms ,ants1 (if ,A ,B))
                                   (,r2 ,assms ,ants2 ,A))
                                  ,B)
                                (and (proof? (list r1 assms ants1 (list 'if A B)))
                                     (proof? (list r2 assms ants2 A)))]))))
            (proof? '(modus-ponens
                      (A (if A B) (if B C))
                      ((assumption (A (if A B) (if B C)) () (if B C))
                       (modus-ponens
                        (A (if A B) (if B C))
                        ((assumption (A (if A B) (if B C)) () (if A B))
                         (assumption (A (if A B) (if B C)) () A)) B))
                      C))))
       q))
    '((#t)))


  (test "proof-2b"
    (run* (prf)
      (fresh (rule assms ants)
        (== `(,rule ,assms ,ants C) prf)
        (== `(A (if A B) (if B C)) assms)
        (== '(modus-ponens
              (A (if A B) (if B C))
              ((assumption (A (if A B) (if B C)) () (if B C))
               (modus-ponens
                (A (if A B) (if B C))
                ((assumption (A (if A B) (if B C)) () (if A B))
                 (assumption (A (if A B) (if B C)) () A)) B))
              C)
            prf)
        (evalo
         `(letrec ((member? (lambda (x ls)
                              (if (null? ls)
                                  #f
                                  (if (equal? (car ls) x)
                                      #t
                                      (member? x (cdr ls)))))))
            (letrec ((proof? (lambda (proof)
                               (match proof
                                 [`(assumption ,assms () ,A)
                                  (member? A assms)]
                                 [`(modus-ponens
                                    ,assms
                                    ((,r1 ,assms ,ants1 (if ,A ,B))
                                     (,r2 ,assms ,ants2 ,A))
                                    ,B)
                                  (and (proof? (list r1 assms ants1 (list 'if A B)))
                                       (proof? (list r2 assms ants2 A)))]))))
              (proof? ',prf)))
         #t)))
    '(((modus-ponens (A (if A B) (if B C))
                     ((assumption (A (if A B) (if B C)) () (if B C))
                      (modus-ponens (A (if A B) (if B C))
                                    ((assumption (A (if A B) (if B C)) () (if A B))
                                     (assumption (A (if A B) (if B C)) () A))
                                    B))
                     C))))


  (test "proof-2c"
    (run 1 (prf)
      (fresh (rule assms ants)
        ;; We want to prove that C holds...
        (== `(,rule ,assms ,ants C) prf)
        ;; ...given the assumptions A, A => B, and B => C.
        (== `(A (if A B) (if B C)) assms)
        (evalo
         `(letrec ((member? (lambda (x ls)
                              (if (null? ls)
                                  #f
                                  (if (equal? (car ls) x)
                                      #t
                                      (member? x (cdr ls)))))))
            (letrec ((proof? (lambda (proof)
                               (match proof
                                 [`(assumption ,assms () ,A)
                                  (member? A assms)]
                                 [`(modus-ponens
                                    ,assms
                                    ((,r1 ,assms ,ants1 (if ,A ,B))
                                     (,r2 ,assms ,ants2 ,A))
                                    ,B)
                                  (and (proof? (list r1 assms ants1 (list 'if A B)))
                                       (proof? (list r2 assms ants2 A)))]))))
              (proof? ',prf)))
         #t)))
    '(((modus-ponens (A (if A B) (if B C))
                     ((assumption (A (if A B) (if B C)) () (if B C))
                      (modus-ponens (A (if A B) (if B C))
                                    ((assumption (A (if A B) (if B C)) () (if A B))
                                     (assumption (A (if A B) (if B C)) () A))
                                    B))
                     C))))


  (test "generate-theorems/proofs"
    (length (run 20 (prf)
              (evalo
               `(letrec ((member? (lambda (x ls)
                                    (if (null? ls)
                                        #f
                                        (if (equal? (car ls) x)
                                            #t
                                            (member? x (cdr ls)))))))
                  (letrec ((proof? (lambda (proof)
                                     (match proof
                                       [`(assumption ,assms () ,A)
                                        (member? A assms)]
                                       [`(modus-ponens
                                          ,assms
                                          ((,r1 ,assms ,ants1 (if ,A ,B))
                                           (,r2 ,assms ,ants2 ,A))
                                          ,B)
                                        (and (proof? (list r1 assms ants1 (list 'if A B)))
                                             (proof? (list r2 assms ants2 A)))]))))
                    (proof? ',prf)))
               #t)))
    '20)


  (test "generate-theorems/proofs-using-modus-ponens"
    (length (run 20 (prf)
              (fresh (assms ants conseq)
                (== `(modus-ponens ,assms ,ants ,conseq) prf)
                (evalo
                 `(letrec ((member? (lambda (x ls)
                                      (if (null? ls)
                                          #f
                                          (if (equal? (car ls) x)
                                              #t
                                              (member? x (cdr ls)))))))
                    (letrec ((proof? (lambda (proof)
                                       (match proof
                                         [`(assumption ,assms () ,A)
                                          (member? A assms)]
                                         [`(modus-ponens
                                            ,assms
                                            ((,r1 ,assms ,ants1 (if ,A ,B))
                                             (,r2 ,assms ,ants2 ,A))
                                            ,B)
                                          (and (proof? (list r1 assms ants1 (list 'if A B)))
                                               (proof? (list r2 assms ants2 A)))]))))
                      (proof? ',prf)))
                 #t))))
    '20)


  (test "generate-non-theorems/proofs"
    (length (run 20 (prf)
              (evalo
               `(letrec ((member? (lambda (x ls)
                                    (if (null? ls)
                                        #f
                                        (if (equal? (car ls) x)
                                            #t
                                            (member? x (cdr ls)))))))
                  (letrec ((proof? (lambda (proof)
                                     (match proof
                                       [`(assumption ,assms () ,A)
                                        (member? A assms)]
                                       [`(modus-ponens
                                          ,assms
                                          ((,r1 ,assms ,ants1 (if ,A ,B))
                                           (,r2 ,assms ,ants2 ,A))
                                          ,B)
                                        (and (proof? (list r1 assms ants1 (list 'if A B)))
                                             (proof? (list r2 assms ants2 A)))]))))
                    (proof? ',prf)))
               #f)))
    '20)



;; unit tests

; or
; num
; symbol?
; not

  (test 'stupor-test-1
    (run* (q)
      (evalo '(letrec ((assoc (lambda (x l)
                                 (if (null? l)
                                     0
                                     (if (or (not (symbol? x))
                                             (not (equal? (car (car l)) x)))
                                         (assoc x (cdr l))
                                         (car l))))))
                (assoc 'foo '((bar . 1) (baz . 2) (42 . ignore) (foo . 3) (quux . 4))))
             q))
    '(((foo . 3))))


;; append

  (test 'append-simple-1
    (run* (q)
      (evalo '(letrec ((append (lambda (l s)
                                 (if (null? l)
                                     s
                                     (cons (car l)
                                           (append (cdr l) s))))))
                (append '(1 2 3) '(4 5)))
             q))
    '(((1 2 3 4 5))))


  (test 'append-simple-2
    (run* (q)
      (evalo `(letrec ((append (lambda (l s)
                                 (if (null? l)
                                     s
                                     (cons (car l)
                                           (append (cdr l) s))))))
                (append ',q '(4 5)))
             '(1 2 3 4 5)))
    '(((1 2 3))))


  (test 'append-simple-3
    (run* (x y)
      (evalo `(letrec ((append (lambda (l s)
                                 (if (null? l)
                                     s
                                     (cons (car l)
                                           (append (cdr l) s))))))
                (append ',x ',y))
             '(1 2 3 4 5)))
    '(((() (1 2 3 4 5)))
      (((1) (2 3 4 5)))
      (((1 2) (3 4 5)))
      (((1 2 3) (4 5)))
      (((1 2 3 4) (5)))
      (((1 2 3 4 5) ()))))

;; Tests from 2017 pearl


  (test "proof-backwards-explicit-member?"
    (run 1 (prf)
      (fresh (rule assms ants)
        ;; We want to prove that C holds...
        (== `(,rule ,assms ,ants C) prf)
        ;; ...given the assumptions A, A => B, and B => C.
        (== `(A (if A B) (if B C)) assms)
        (evalo
         `(letrec ((member? (lambda (x ls)
                              (if (null? ls)
                                  #f
                                  (if (equal? (car ls) x)
                                      #t
                                      (member? x (cdr ls)))))))
            (letrec ((proof? (lambda (proof)
                               (match proof
                                 [`(assumption ,assms () ,A)
                                  (member? A assms)]
                                 [`(modus-ponens
                                    ,assms
                                    ((,r1 ,assms ,ants1 (if ,A ,B))
                                     (,r2 ,assms ,ants2 ,A))
                                    ,B)
                                  (and (proof? (list r1 assms ants1 (list 'if A B)))
                                       (proof? (list r2 assms ants2 A)))]))))
              (proof? ',prf)))
         #t)))
    '(((modus-ponens (A (if A B) (if B C))
                     ((assumption (A (if A B) (if B C)) () (if B C))
                      (modus-ponens (A (if A B) (if B C))
                                    ((assumption (A (if A B) (if B C)) () (if A B))
                                     (assumption (A (if A B) (if B C)) () A))
                                    B))
                     C))))


  (test "check quine"
    (run 1 (q)
      (== q '((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x))))
      (evalo
       `(letrec ((eval-quasi (lambda (q eval)
                               (match q
                                 [(? symbol? x) x]
                                 [`() '()]
                                 [`(,`unquote ,exp) (eval exp)]
                                 [`(quasiquote ,datum) ('error)]
                                 [`(,a . ,d)
                                  (cons (eval-quasi a eval) (eval-quasi d eval))]))))
          (letrec ((eval-expr
                    (lambda (expr env)
                      (match expr
                        [`(quote ,datum) datum]
                        [`(lambda (,(? symbol? x)) ,body)
                         (lambda (a)
                           (eval-expr body (lambda (y)
                                             (if (equal? x y)
                                                 a
                                                 (env y)))))]
                        [(? symbol? x) (env x)]
                        [`(quasiquote ,datum)
                         (eval-quasi datum (lambda (exp) (eval-expr exp env)))]
                        [`(,rator ,rand)
                         ((eval-expr rator env) (eval-expr rand env))]
                        ))))
            (eval-expr ',q
                       'initial-env)))
       q))
    (list '(((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x))))))



;; append tests

 (test "append-0"
        (run* (q)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons (car l) (append (cdr l) s))))))
              (append '(a b c) '(d e)))
           '(a b c d e)))
        '((_.0)))

 (test "append-1"
        (run* (q)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons (car l) (append (cdr l) s))))))
              (append '(a b c) '(d e)))
           q))
        '(((a b c d e))))

 (test-p "append-2"
   (run 2 (q)
     (evalo
      `(letrec ((append
                 (lambda (l s)
                   (if (null? l)
                       s
                       (cons (car l) (append (cdr l) s))))))
         (append ,q '(d e)))
      '(a b c d e)))
   (one-of?
     '((('(a b c))
        ((car '((a b c) . _.0)) (absento (closure _.0) (prim _.0))))
       ;;
       (('(a b c))
        (((lambda _.0 '(a b c))) (=/= ((_.0 quote))) (sym _.0)))
       ;;
       (((cdr '(_.0 a b c)) (absento (closure _.0) (prim _.0)))
        ((car '((a b c) . _.0)) (absento (closure _.0) (prim _.0)))))))

 (test "append-3"
        (run* (q)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons (car l) (append (cdr l) s))))))
              (append ',q '(d e)))
           '(a b c d e)))
        '(((a b c))))

 (test "append-4"
        (run* (q r)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons (car l) (append (cdr l) s))))))
              (append ',q ',r))
           '(a b c d e)))
        '(((() (a b c d e)))
          (((a) (b c d e)))
          (((a b) (c d e)))
          (((a b c) (d e)))
          (((a b c d) (e)))
          (((a b c d e) ()))))

 (test "append-5"
        (run 1 (q)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons ,q (append (cdr l) s))))))
              (append '(a b c) '(d e)))
           '(a b c d e)))
        '(((car l))))

 (test "append-6"
        (run 1 (q)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons (car l) (append (cdr ,q) s))))))
              (append '(a b c) '(d e)))
           '(a b c d e)))
        '((l)))

 (test "append-7"
        (run 1 (q)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons (car l) (append (,q l) s))))))
              (append '(a b c) '(d e)))
           '(a b c d e)))
        '((cdr)))

 (test "append-8"
        (run 1 (q r)
          (symbolo q)
          (symbolo r)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons (car l) (append (,q ,r) s))))))
              (append '(a b c) '(d e)))
           '(a b c d e)))
        '(((cdr l))))

 (test "append-9"
   (run 1 (q r)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons (car l) (append (,q ,r) s))))))
              (append '(a b c) '(d e)))
           '(a b c d e)))
        '(((cdr l))))

 (test-p "append-10"
   (run 1 (q)
     (evalo
      `(letrec ((append
                 (lambda (l s)
                   (if ,q
                       s
                       (cons (car l) (append (cdr l) s))))))
         (append '(a b c) '(d e)))
      '(a b c d e)))
   (one-of?
    '((((null? l)))
      ;;
      (((equal? l (quote ())))))))

 (test-p "append-11"
   (run 4 (q)
     (evalo
      `(letrec ((append
                 (lambda (l s)
                   (if ,q
                       s
                       (cons (car l) (append (cdr l) s))))))
         (append '(a b c) '(d e)))
      '(a b c d e)))
   (each-one-of? '(((null? l))
                   (((lambda () (null? l))))
                   ((null? ((lambda () l))))
                   ((equal? '() l))
                   ((and (null? l)))
                   ((and (null? l) (quote _.0)) (=/= ((_.0 #f))) (absento (closure _.0) (prim _.0)))
                   ((and (null? l) (quote #t)))
                   ((if '#t (null? l) _.0))
                   ((if '#f _.0 (null? l)))
                   ((if #t (null? l) _.0)))))

;; slooow--didnt complete in 30 seconds

 (test "append-12"
        (run 1 (q r)
          (absento 'a r)
          (absento 'b r)
          (absento 'c r)
          (absento 'd r)
          (absento 'e r)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if ,q
                            ,r
                            (cons (car l) (append (cdr l) s))))))
              (append '(a b c) '(d e)))
           '(a b c d e)))
        '((((null? l) s))))

 (test "append-13"
        (run 1 (q r)
          (absento 'a r)
          (absento 'b r)
          (absento 'c r)
          (absento 'd r)
          (absento 'e r)
          (absento 'f r)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if ,q
                            ,r
                            (cons (car l) (append (cdr l) s))))))
              (list
               (append '() '())
               (append '(a) '(b))
               (append '(c d) '(e f))))
           '(()
             (a b)
             (c d e f))))
        '((((null? l) s))))

 (test "append-14"
        (run 1 (q r s)
          (absento 'a r)
          (absento 'b r)
          (absento 'c r)
          (absento 'd r)
          (absento 'e r)
          (absento 'f r)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if ,q
                            ,r
                            (,s (car l) (append (cdr l) s))))))
              (list
               (append '() '())
               (append '(a) '(b))
               (append '(c d) '(e f))))
           '(()
             (a b)
             (c d e f))))
        '((((null? l) s cons))))


;; reverse tests

(test 'reverse-1
  (run 1 (q r s)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh (defn)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)
        (== `(lambda (xs)
               (if (null? xs)
                   '()
                   (,q (reverse ,r) ,s)))
            defn)
         
        ;;(== 'append q)
        (== '(cdr xs) r)
        (== '(cons (car xs) '()) s)
        (evalo `(letrec ((append
                          (lambda (l s)
                            (if (null? l) s
                                (cons (car l)
                                      (append (cdr l) s))))))
                  (letrec ((reverse ,defn))
                    (list
                     (reverse '())
                     (reverse '(,g1))
                     (reverse '(,g2 ,g3))
                     (reverse '(,g4 ,g5 ,g6)))))
               (list '() `(,g1) `(,g3 ,g2) `(,g6 ,g5 ,g4))))))    
  '(((append (cdr xs) (cons (car xs) '())))))

(test 'reverse-2
  (run 1 (q r s)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh (defn)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)
        (== `(lambda (xs)
               (if (null? xs)
                   '()
                   (,q (reverse ,r) ,s)))
            defn)
        
        (== 'append q)
        ;;(== '(cdr xs) r)
        (== '(cons (car xs) '()) s)
        (evalo `(letrec ((append
                          (lambda (l s)
                            (if (null? l) s
                                (cons (car l)
                                      (append (cdr l) s))))))
                  (letrec ((reverse ,defn))
                    (list
                     (reverse '())
                     (reverse '(,g1))
                     (reverse '(,g2 ,g3))
                     (reverse '(,g4 ,g5 ,g6)))))
               (list '() `(,g1) `(,g3 ,g2) `(,g6 ,g5 ,g4))))))    
  '(((append (cdr xs) (cons (car xs) '())))))

(test-p 'reverse-3
  (run 1 (q r s)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh (defn)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)
        (== `(lambda (xs)
               (if (null? xs)
                   '()
                   (,q (reverse ,r) ,s)))
            defn)
        
        (== 'append q)
        (== '(cdr xs) r)
        ;;(== '(cons (car xs) '()) s)
        (evalo `(letrec ((append
                          (lambda (l s)
                            (if (null? l) s
                                (cons (car l)
                                      (append (cdr l) s))))))
                  (letrec ((reverse ,defn))
                    (list
                     (reverse '())
                     (reverse '(,g1))
                     (reverse '(,g2 ,g3))
                     (reverse '(,g4 ,g5 ,g6)))))
               (list '() `(,g1) `(,g3 ,g2) `(,g6 ,g5 ,g4))))))
  (one-of? '((((append (cdr xs) (cons (car xs) '()))))
             (((append (cdr xs) (list (car xs))))))))

(test-p 'reverse-4
  (run 1 (q r s)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh (defn)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)
        (== `(lambda (xs)
               (if (null? xs)
                   '()
                   (,q (reverse ,r) ,s)))
            defn)
        
        ;;(== 'append q)
        (== '(cdr xs) r)
        ;;(== '(cons (car xs) '()) s)
        (evalo `(letrec ((append
                          (lambda (l s)
                            (if (null? l) s
                                (cons (car l)
                                      (append (cdr l) s))))))
                  (letrec ((reverse ,defn))
                    (list
                     (reverse '())
                     (reverse '(,g1))
                     (reverse '(,g2 ,g3))
                     (reverse '(,g4 ,g5 ,g6)))))
               (list '() `(,g1) `(,g3 ,g2) `(,g6 ,g5 ,g4))))))
  (one-of? '((((append (cdr xs) (cons (car xs) '()))))
             (((append (cdr xs) (list (car xs))))))))

(test-p 'reverse-5
  (run 1 (q r s)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh (defn)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)
        (== `(lambda (xs)
               (if (null? xs)
                   '()
                   (,q (reverse ,r) ,s)))
            defn)
        
        (== 'append q)
                                        ;(== '(cdr xs) r)
                                        ;(== '(cons (car xs) '()) s)
        (evalo `(letrec ((append
                          (lambda (l s)
                            (if (null? l) s
                                (cons (car l)
                                      (append (cdr l) s))))))
                  (letrec ((reverse ,defn))
                    (list
                     (reverse '())
                     (reverse '(,g1))
                     (reverse '(,g2 ,g3))
                     (reverse '(,g4 ,g5 ,g6)))))
               (list '() `(,g1) `(,g3 ,g2) `(,g6 ,g5 ,g4))))))
    (one-of? '((((append (cdr xs) (cons (car xs) '()))))
               (((append (cdr xs) (list (car xs))))))))

(test-p 'reverse-10
  (run 1 (q r s)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh (defn)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)
        (== `(lambda (xs)
               (if (null? xs)
                   '()
                   (,q (reverse ,r) ,s)))
            defn)
        
        ;;(== 'append q)
        ;;(== '(cdr xs) r)
        ;;(== '(cons (car xs) '()) s)
        (evalo `(letrec ((append
                          (lambda (l s)
                            (if (null? l) s
                                (cons (car l)
                                      (append (cdr l) s))))))
                  (letrec ((reverse ,defn))
                    (list
                     (reverse '())
                     (reverse '(,g1))
                     (reverse '(,g2 ,g3))
                     (reverse '(,g4 ,g5 ,g6)))))
               (list '() `(,g1) `(,g3 ,g2) `(,g6 ,g5 ,g4))))))
  (one-of? '((((append (cdr xs) (cons (car xs) '()))))
             (((append (cdr xs) (list (car xs))))))))

(test 'reverse-20
  (run 1 (q r s t)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh (defn)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)
        (== `(lambda (xs)
               (if (null? xs)
                   '()
                   (,q (,r ,s) ,t)))
            defn)
        
        ;;(== 'append q)
        ;;(== '(cdr xs) r)
        ;;(== '(cons (car xs) '()) s)
        (evalo `(letrec ((append
                          (lambda (l s)
                            (if (null? l) s
                                (cons (car l)
                                      (append (cdr l) s))))))
                  (letrec ((reverse ,defn))
                    (list
                     (reverse '())
                     (reverse '(,g1))
                     (reverse '(,g2 ,g3))
                     (reverse '(,g4 ,g5 ,g6)))))
               (list '() `(,g1) `(,g3 ,g2) `(,g6 ,g5 ,g4))))))
  '(((append (cdr xs) (cons (car xs) '())))))

(test 'reverse-30
  (run 1 (q r s)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh (defn)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)
        (== `(lambda (xs)
               (if (null? xs)
                   '()
                   (,q ,r ,s)))
            defn)
        
        ;;(== 'append q)
        ;;(== '(cdr xs) r)
        ;;(== '(cons (car xs) '()) s)
        (evalo `(letrec ((append
                          (lambda (l s)
                            (if (null? l) s
                                (cons (car l)
                                      (append (cdr l) s))))))
                  (letrec ((reverse ,defn))
                    (list
                     (reverse '())
                     (reverse '(,g1))
                     (reverse '(,g2 ,g3))
                     (reverse '(,g4 ,g5 ,g6)))))
               (list '() `(,g1) `(,g3 ,g2) `(,g6 ,g5 ,g4))))))
  '(((append (cdr xs) (cons (car xs) '())))))

(test 'reverse-40
  (run 1 (q)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh (defn)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)
        (== `(lambda (xs)
               (if (null? xs)
                   '()
                   ,q))
            defn)
        
        ;;(== 'append q)
        ;;(== '(cdr xs) r)
        ;;(== '(cons (car xs) '()) s)
        (evalo `(letrec ((append
                          (lambda (l s)
                            (if (null? l) s
                                (cons (car l)
                                      (append (cdr l) s))))))
                  (letrec ((reverse ,defn))
                    (list
                     (reverse '())
                     (reverse '(,g1))
                     (reverse '(,g2 ,g3))
                     (reverse '(,g4 ,g5 ,g6)))))
               (list '() `(,g1) `(,g3 ,g2) `(,g6 ,g5 ,g4))))))
  '(((append (cdr xs) (cons (car xs) '())))))

 )

(exit)
