;;; Examples taken from 'A Unified Approach to Solving Seven
;;; Programming Problems (Functional Pearl)', ICFP 2017, by William
;;; E. Byrd, Michael Ballantyne, Gregory Rosenblatt, and Matthew Might


(letrec ((member? (lambda (x ls)
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
    (proof? '(modus-ponens (A (if A B) (if B C))
                           ((assumption (A (if A B) (if B C)) () (if B C))
                            (modus-ponens (A (if A B) (if B C))
                                          ((assumption (A (if A B) (if B C)) () (if A B))
                                           (assumption (A (if A B) (if B C)) () A))
                                          B))
                           C))))


(letrec ((eval-quasi (lambda (q eval)
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
    (eval-expr '((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x)))
               'initial-env)))
