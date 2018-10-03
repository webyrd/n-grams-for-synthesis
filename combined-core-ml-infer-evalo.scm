;; ML-ish version of the interpreter, based on joint work with Kanae Tsushima.

(define (bool-!-/evalo expr gamma env type val)
  (conde
    ((== #t expr) (== 'bool type) (== #t val))
    ((== #f expr) (== 'bool type) (== #f val))))

(define (num-!-/evalo expr gamma env type val)
  (fresh ()
    (numbero expr)
    (== 'int type)
    (== expr val)))

(define (nil-!-/evalo expr gamma env type val)
  (fresh ()
    (== 'nil expr)
    (== 'nil val)
    (fresh (alpha)
      (== `(list ,alpha) type))))

(define (var-!-/evalo expr gamma env type val)
  (fresh ()
    (symbolo expr)
    (=/= 'nil expr)
    ;; TODO check order of arguments to lookup
    (lookup-!-/evalo expr gamma env type val)))

(define (null?-!-/evalo expr gamma env type val)
  (fresh (e alpha v)
    (== `(null? ,e) expr)
    (== 'bool type)
    (conde
      ((== 'nil v) (== #t val))
      ((=/= 'nil v) (== #f val)))
    (!-/eval-expo e gamma env `(list ,alpha) v 'null?)))

(define (car-!-/evalo expr gamma env type val)
  (fresh (e alpha d)
    (== `(car ,e) expr)
    (== alpha type)
    (!-/eval-expo e gamma env `(list ,alpha) `(cons ,val ,d) 'car)))

(define (cdr-!-/evalo expr gamma env type val)
  (fresh (e alpha a)
    (== `(cdr ,e) expr)
    (== `(list ,alpha) type)
    (!-/eval-expo e gamma env `(list ,alpha) `(cons ,a ,val) 'cdr)))

(define (cons-!-/evalo expr gamma env type val)
  (fresh (e1 e2 alpha v1 v2)
    (== `(cons ,e1 ,e2) expr)
    (== `(list ,alpha) type)
    (== `(cons ,v1 ,v2) val)
    (!-/eval-expo e1 gamma env `(list ,alpha) v1 'cons-e1)
    (!-/eval-expo e2 gamma env alpha v2 'cons-e2)))

(define (pair-!-/evalo expr gamma env type val)
  (fresh (e1 e2 t1 t2 v1 v2)
    (== `(pair ,e1 ,e2) expr)
    (== `(pair ,t1 ,t2) type)
    (== `(pair ,v1 ,v2) val)
    (!-/eval-expo e1 gamma env t1 v1 'pair-e1)
    (!-/eval-expo e2 gamma env t2 v2 'pair-e2)))

(define (if-!-/evalo expr gamma env type val)
  (fresh (e1 e2 e3 t)
    (== `(if ,e1 ,e2 ,e3) expr)
    (!-/eval-expo e1 gamma env 'bool t 'if-test)
    (conde
      ((== #t t) (!-/eval-expo e2 gamma env type val 'if-conseq))
      ((== #f t) (!-/eval-expo e3 gamma env type val 'if-alt)))))

(define (letrec-!-/evalo expr gamma env type val)
  (fresh (p-name x body letrec-body)
    ;; single-function muti-argument letrec version
    (== `(letrec ((,p-name (lambda ,x ,body)))
           ,letrec-body)
        expr)
    (symbolo p-name)
    (list-of-symbolso x)
    (!-/eval-expo letrec-body
                  `((,p-name . (poly ((,p-name . (mono . ,t)) . ,gamma)
                                     (lambda ,x ,body)))
                    . ,gamma)
                  `((,p-name . (rec . (lambda ,x ,body))) . ,env)
                  type
                  val
                  'letrec-body)))

(define (lambda-!-/evalo expr gamma env type val)
  (fresh (x body t t^)
    (== `(lambda ,x ,body) expr)
    (== `(-> ,t ,t^) type)
    (== `(closure (lambda ,x ,body) ,env) val)
    (list-of-symbolso x)))

(define (app-!-/evalo expr gamma env type val)
  (fresh (rator x* rands body env^ a* res t t*)
    (== `(,rator . ,rands) expr)
    ;; Multi-argument
    (!-/eval-expo rator gamma env `(-> ,t ,type) `(closure (lambda ,x* ,body) ,gamma^ ,env^) 'app-rator)
    (!-/eval-randso rands gamma env t* a*)
    (ext-gamma*/env*o x* t* a* gamma^ gamma^^ env^ env^^)
    (!-/eval-expo body gamma^^ env^^ type val 'lambda)))







(define (equal?-!-/evalo expr gamma env type val)
  (fresh (e1 e2 t v1 v2)
    (== `(equal? ,e1 ,e2) expr)
    (== 'bool type)
    (conde
      ((== v1 v2) (== #t val))
      ((=/= v1 v2) (== #f val)))
    (!-/eval-expo e1 gamma env t v1 'equal?-e1)
    (!-/eval-expo e2 gamma env t v2 'equal?-e2)))

(define (and-!-/evalo expr gamma env type val)
  (fresh (e*)
    (== `(and . ,e*) expr)
    (== 'bool type)
    (ando e* gamma env type val)))

(define (or-!-/evalo expr gamma env type val)
  (fresh (e*)
    (== `(or . ,e*) expr)
    (== 'bool type)
    (oro e* gamma env type val)))

(define (list-!-/evalo expr gamma env type val)
  (fresh (rands alpha a*)
    (== `(list . ,rands) expr)
    (== `(list ,alpha) type)
    (== a* val)
    (eval-listo rands gamma env alpha a*)))

(define (symbol?-!-/evalo expr gamma env type val)
  (fresh (e t v)
    (== `(symbol? ,e) expr)
    (== 'bool type)
    (conde
      ((symbolo v) (== #t val))
      ((not-symbolo v) (== #f val)))
    (!-/eval-expo e gamma env t v 'symbol?)))

(define (not-symbolo t)
  (conde
    ((== #f t))
    ((== #t t))
    ((== '() t))
    ((numbero t))
    ((fresh (a d)
       (== `(,a . ,d) t)))))

(define (not-!-/evalo expr gamma env type val)
  (fresh (e v)
    (== `(not ,e) expr)
    (== 'bool type)
    (conde
      ((== #t v) (== #f val))
      ((== #f v) (== #t val)))
    (!-/eval-expo e gamma env 'bool v 'not)))





(define (lookup-!-/evalo x gamma env type val)
  (fresh (y t v rest-gamma rest-env)
    (symbolo x)
    (== `((,y . ,t) . ,rest-gamma) gamma)
    (== `((,y . ,v) . ,rest-env) env)
    (symbolo y)
    (conde
      ((== x y)
       (conde
         ((== `(val . ,val) v)
          (== `(mono . ,type) t))
         ((fresh (lam-expr)
            (== `(rec . ,lam-expr) v)
            (fresh (gamma^)
              (== `(poly ,gamma^ ,lam-expr) t))
            (== `(closure ,lam-expr ,gamma ,env) val)))))
      ((=/= x y)
       (lookup-!-/evalo x rest-gamma rest-env type val)))))

(define (!-/eval-randso expr* gamma env type* val*)
  (conde
    ((== '() expr*)
     (== '() type*)
     (== '() val*))
    ((fresh (a d t-a t-d v-a v-d)
       (== `(,a . ,d) expr*)
       (== `(,t-a . ,t-d) type*)
       (== `(,v-a . ,v-d) val*)
       (!-/eval-expo a gamma env t-a v-a 'app-rand*)
       (!-/eval-randso d gamma env t-d v-d)))))

(define (eval-listo expr* gamma env type val*)
  (conde
    ((== '() expr*)
     (== '() val*))
    ((fresh (a d v-a v-d)
       (== `(,a . ,d) expr*)
       (== `(,v-a . ,v-d) val*)
       (!-/eval-expo a  gamma env type v-a 'list)
       (eval-listo d gamma env type v-d)))))

;; need to make sure lambdas are well formed.
;; grammar constraints would be useful here!!!
(define (list-of-symbolso los)
  (conde
    ((== '() los))
    ((fresh (a d)
       (== `(,a . ,d) los)
       (symbolo a)
       (list-of-symbolso d)))))

(define (ext-gamma*/env*o x* t* a* gamma gamma^ env env^)
  (conde
    ((== '() x*) (== '() t*) (== '() a*) (== gamma gamma^) (== env env^))
    ((fresh (x t a dx* dt* da* gamma2 env2)
       (== `(,x . ,dx*) x*)
       (== `(,t . ,dt*) t*)
       (== `(,a . ,da*) a*)
       (== `((,x . (mono . ,t)) . ,gamma) gamma2)
       (== `((,x . (val . ,a)) . ,env) env2)
       (symbolo x)
       (ext-gamma*/env*o dx* dt* da* gamma2 gamma^ env2 env^)))))

(define (ando e* gamma env val)
  (conde
    ((== '() e*) (== #t val))
    ((fresh (e)
       (== `(,e) e*)
       (conde
         ((== #f val))
         ((== #t val)))
       (!-/eval-expo e gamma env 'bool val 'and)))
    ((fresh (e1 e2 e-rest v)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (conde
         ((== #f v)
          (== #f val)
          (!-/eval-expo e1 gamma env v 'bool 'and))
         ((== #t v)
          (!-/eval-expo e1 gamma env v 'bool 'and)
          (ando `(,e2 . ,e-rest) gamma env val)))))))

(define (oro e* gamma env val)
  (conde
    ((== '() e*) (== #f val))
    ((fresh (e)
       (== `(,e) e*)
       (conde
         ((== #f val))
         ((== #t val)))
       (!-/eval-expo e gamma env 'bool val 'or)))
    ((fresh (e1 e2 e-rest v)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (conde
         ((== #t v)
          (== v val)
          (!-/eval-expo e1 gamma env 'bool v 'or))
         ((== #f v)
          (!-/eval-expo e1 gamma env 'bool v 'or)
          (oro `(,e2 . ,e-rest) gamma env val)))))))


(define empty-gamma '())
(define empty-env '())

(define (!-/evalo expr type val)
  (!-/eval-expo expr empty-gamma empty-env type val 'top-level))


(define (alist-ref alist element failure-result)
  (let ((pr (assoc element alist)))
    (if pr (cdr pr) failure-result)))


;;; !!! Careful!
;;;
;;; This optimized lookup relies on the ability to recursively pass
;;; around a non-symbol 'x', to reach the base case.
;;;
;;; Don't add a (symbolo x) test, since that will break this code!
;;;
;;; (Is there a better way to write this???)
(define (lookup-!-/evalo-k k)
  (lambda (x gamma env type val)
    (conde
      ((== '() gamma)
       (== '() env)
       ;; run k
       k)
      ((fresh (y t v rest-gamma rest-env)
         (symbolo x)
         (== `((,y . ,t) . ,rest-gamma) gamma)
         (== `((,y . ,v) . ,rest-env) env)
         (symbolo y)
         (conde
           ((== x y)
            (conde
              ((== `(val . ,val) v)
               (== `(mono . ,type) t))
              ((fresh (lam-expr)
                 (== `(rec . ,lam-expr) v)
                 (fresh (gamma^)
                   (== `(poly ,gamma^ ,lam-expr) t))
                 (== `(closure ,lam-expr ,gamma ,env) val)))))
           ((=/= x y)
            ((lookup-!-/evalo-k k) x rest-gamma rest-env type val))))))))

(define build-and-run-conde
  (lambda (expr gamma env type val list-of-eval-relations)
    (let ((k (lambdag@ (st)
               (inc (bind (state-depth-deepen (state-with-scope st (new-scope)))
                          (lambdag@ (st)
                            (let loop ((list-of-eval-relations list-of-eval-relations))
                              (cond
                                ((null? list-of-eval-relations) (mzero))
                                (else
                                 (mplus (((car list-of-eval-relations) expr gamma env type val) st)
                                        (inc (loop (cdr list-of-eval-relations)))))))))))))
      (if lookup-optimization?
          ((lookup-!-/evalo-k k) expr gamma env type val)
          k))))
