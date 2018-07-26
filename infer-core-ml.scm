;; Type inferencer for ML-ish language, based on joint work with Kanae Tsushima.

(define (bool-!-o expr gamma type)
  (conde
    ((== #t expr) (== 'bool type))
    ((== #f expr) (== 'bool type))))

(define (num-!-o expr gamma type)
  (fresh ()
    (numbero expr)
    (== 'int type)))

(define (nil-!-o expr gamma type)
  (fresh ()
    (== 'nil expr)
    (fresh (a)
      (== `(list ,a) type))))

(define (var-!-o expr gamma type)
  (fresh ()
    (symbolo expr)
    (=/= 'nil expr)
    (lookup-!-o expr gamma type)))

(define (null?-!-o expr gamma type)
  (fresh (e a)
    (== `(null? ,e) expr)
    (== 'bool type)
    (!-o e gamma `(list ,a) 'null?)))

(define (car-!-o expr gamma type)
  (fresh (e a)
    (== `(car ,e) expr)
    (== a type)
    (!-o e gamma `(list ,a) 'car)))

(define (cdr-!-o expr gamma type)
  (fresh (e a)
    (== `(cdr ,e) expr)
    (== `(list ,a) type)
    (!-o e gamma `(list ,a) 'cdr)))

(define (cons-!-o expr gamma type)
  (fresh (e1 e2 a)
    (== `(cons ,e1 ,e2) expr)
    (== `(list ,a) type)
    (!-o e2 gamma `(list ,a) 'cons-e2)
    (!-o e1 gamma a 'cons-e1)))

(define (pair-!-o expr gamma type)
  (fresh (e1 e2 t1 t2)
    (== `(pair ,e1 ,e2) expr)
    (== `(pair ,t1 ,t2) type)
    (!-o e1 gamma t1 'pair-e1)
    (!-o e2 gamma t2 'pair-e2)))

(define (if-!-o expr gamma type)
  (fresh (e1 e2 e3)
    (== `(if ,e1 ,e2 ,e3) expr)
    (!-o e1 gamma 'bool 'if-test)
    (!-o e2 gamma type 'if-conseq)
    (!-o e3 gamma type 'if-alt)))

(define (letrec-!-o expr gamma type)
  ;; our letrec is polymorphic
  (fresh (p-name x* body letrec-body t)
    ;; single-function muti-argument letrec version
    (== `(letrec ((,p-name (lambda ,x* ,body)))
           ,letrec-body)
        expr)
    (symbolo p-name)
    (list-of-symbolso x*)

   ;; Make sure the right-hand-side of the polymorphic 'letrec'
   ;; binding has a type, but then forget about the type.
   (fresh (forget-me)     
     (!-o `(lambda ,x* ,body)
          `((,p-name (mono ,forget-me)) . ,gamma)
          forget-me
          'letrec-rhs))
   
   (!-o letrec-body
        `((,p-name (poly ((,p-name (mono ,t)) . ,gamma)
                         (lambda ,x* ,body)))
          . ,gamma)
        type
        'letrec-body)))

(define (lambda-!-o expr gamma type)
  (fresh (x* body t* t^ gamma^)
    (== `(lambda ,x* ,body) expr)
    (== `(-> ,t* ,t^) type)
    (ext-gamma-mono*o x* t* gamma gamma^)
    (!-o body gamma^ t^ 'lambda-body)))

(define (app-!-o expr gamma type)
  (fresh (rator rands t*)
    (== `(,rator . ,rands) expr)
    ;; Multi-argument
    (!-o rator gamma `(-> ,t* ,type) 'app-rator)
    (!-o-randso rands gamma t*)))

(define (equal?-!-o expr gamma type)
  (fresh (e1 e2 t)
    (== `(equal? ,e1 ,e2) expr)
    (== 'bool type)
    (!-o e1 gamma t 'equal?-e1)
    (!-o e2 gamma t 'equal?-e2)))

(define (and-!-o expr gamma type)
  (fresh (e*)
    (== `(and . ,e*) expr)
    (== 'bool type)
    (!-o-ando e* gamma)))

(define (or-!-o expr gamma type)
  (fresh (e*)
    (== `(or . ,e*) expr)
    (== 'bool type)
    (!-o-oro e* gamma)))

(define (list-!-o expr gamma type)
  (fresh (rands a)
    (== `(list . ,rands) expr)
    (== `(list ,a) type)
    (!-o-listo rands gamma a)))

(define (symbol?-!-o expr gamma type)
  (fresh (e t)
    (== `(symbol? ,e) expr)
    (== 'bool type)
    (!-o e gamma t 'symbol?)))

(define (not-!-o expr gamma type)
  (fresh (e)
    (== `(not ,e) expr)
    (== 'bool type)
    (!-o e gamma 'bool 'not)))



(define (lookup-!-o x gamma type)
  (fresh (y t rest)
    (symbolo x)
    (== `((,y ,t) . ,rest) gamma)
    (symbolo y)
    (conde
      ((== x y)
       (conde
         ((== `(mono ,type) t))
         ((fresh (gamma^ x* body)
            (== `(poly ,gamma^ (lambda ,x* ,body)) t)
            (!-o `(lambda ,x* ,body) gamma^ type 'letrec-rhs)))))
      ((=/= x y)
       (lookup-!-o x rest type)))))


(define (!-o-randso expr* gamma type*)
  (conde
    ((== '() expr*)
     (== '() type*))
    ((fresh (a d t-a t-d)
       (== `(,a . ,d) expr*)
       (== `(,t-a . ,t-d) type*)
       (!-o a gamma t-a 'app-rand*)
       (!-o-randso d gamma t-d)))))

(define (!-o-listo expr gamma type)
  (conde
    ((== '() expr)
     ;; 'any' type
     )
    ((fresh (e1 e2)
       (== `(,e1 . ,e2) expr)
       (!-o e1 gamma type 'list)
       (!-o-listo e2 gamma type)))))

;; need to make sure lambdas are well formed.
;; grammar constraints would be useful here!!!
(define (list-of-symbolso los)
  (conde
    ((== '() los))
    ((fresh (a d)
       (== `(,a . ,d) los)
       (symbolo a)
       (list-of-symbolso d)))))

(define (ext-gamma-mono*o x* t* gamma out)
  (conde
    ((== '() x*) (== '() t*) (== gamma out))
    ((fresh (x t dx* dt* gamma2)
       (== `(,x . ,dx*) x*)
       (== `(,t . ,dt*) t*)
       (== `((,x (mono ,t)) . ,gamma) gamma2)
       (symbolo x)
       (ext-gamma-mono*o dx* dt* gamma2 out)))))


(define (!-o-ando e* gamma)
  (conde
    ((== '() e*))
    ((fresh (e e-rest)
       (== `(,e . ,e-rest) e*)
       (!-o e gamma 'bool 'and)
       (!-o-ando e-rest gamma)))))

(define (!-o-oro e* gamma)
  (conde
    ((== '() e*))
    ((fresh (e e-rest)
       (== `(,e . ,e-rest) e*)
       (!-o e gamma 'bool 'or)
       (!-o-oro e-rest gamma)))))




(define empty-gamma '())

(define (type-expo expr type)
  (!-o expr empty-gamma type 'top-level))

(define (alist-ref alist element failure-result)
  (let ((pr (assoc element alist)))
    (if pr (cdr pr) failure-result)))

(define (lookup-!-o-k k)
  (lambda (x gamma type)
    (conde
      ((== '() gamma) k)
      ((fresh (y t rest)
         (symbolo x)
         (== `((,y ,t) . ,rest) gamma)
         (symbolo y)
         (conde
           ((== x y)
            (conde
              ((== `(mono ,type) t))
              ((fresh (gamma^ x* body)
                 (== `(poly ,gamma^ (lambda ,x* ,body)) t)
                 (!-o `(lambda ,x* ,body) gamma^ type 'letrec-rhs)))))
           ((=/= x y)
            ((lookup-!-o-k k) x rest type))))))))

(define build-and-run-conde
  (lambda (expr gamma type list-of-!-o-relations)
    (let ((k (lambdag@ (st)
               (inc (bind (state-depth-deepen (state-with-scope st (new-scope)))
                          (lambdag@ (st)
                            (let loop ((list-of-!-o-relations list-of-!-o-relations))
                              (cond
                                ((null? list-of-!-o-relations) (mzero))
                                (else
                                 (mplus (((car list-of-!-o-relations) expr gamma type) st)
                                        (inc (loop (cdr list-of-!-o-relations)))))))))))))
      (if lookup-optimization?
          ((lookup-!-o-k k) expr gamma type)
          k))))
