;; ML-ish version of the interpreter, based on joint work with Kanae Tsushima.

(define (bool-!-/evalo expr gamma env type val)
  (conde
    ((== #t expr) (== #t val))
    ((== #f expr) (== #f val))))

(define (num-!-/evalo expr gamma env type val)
  (fresh ()
    (numbero expr)
    (== expr val)))

(define (nil-!-/evalo expr gamma env type val)
  (fresh ()
    (== 'nil expr)
    (== 'nil val)))

(define (var-!-/evalo expr gamma env type val)
  (fresh ()
    (symbolo expr)
    (=/= 'nil expr)
    (lookup-!-/evalo expr gamma env type val)))

(define (null?-!-/evalo expr gamma env type val)
  (fresh (e v)
    (== `(null? ,e) expr)
    (conde
      ((== 'nil v) (== #t val))
      ((=/= 'nil v) (== #f val)))
    (eval-expo e env v 'null?)))

(define (car-!-/evalo expr gamma env type val)
  (fresh (e d)
    (== `(car ,e) expr)
    (eval-expo e env `(cons ,val ,d) 'car)))

(define (cdr-!-/evalo expr gamma env type val)
  (fresh (e a)
    (== `(cdr ,e) expr)
    (eval-expo e env `(cons ,a ,val) 'cdr)))

(define (cons-!-/evalo expr gamma env type val)
  (fresh (e1 e2 v1 v2)
    (== `(cons ,e1 ,e2) expr)
    (== `(cons ,v1 ,v2) val)
    (eval-expo e1 env v1 'cons-e1)
    (eval-expo e2 env v2 'cons-e2)))

(define (pair-!-/evalo expr gamma env type val)
  (fresh (e1 e2 v1 v2)
    (== `(pair ,e1 ,e2) expr)
    (== `(pair ,v1 ,v2) val)
    (eval-expo e1 env v1 'pair-e1)
    (eval-expo e2 env v2 'pair-e2)))

(define (if-!-/evalo expr gamma env type val)
  (fresh (e1 e2 e3 t)
    (== `(if ,e1 ,e2 ,e3) expr)
    (eval-expo e1 env t 'if-test)
    (conde
      ((== #t t) (eval-expo e2 env type val 'if-conseq))
      ((== #f t) (eval-expo e3 env type val 'if-alt)))))

(define (letrec-!-/evalo expr gamma env type val)
  (fresh (p-name x body letrec-body)
    ;; single-function muti-argument letrec version
    (== `(letrec ((,p-name (lambda ,x ,body)))
           ,letrec-body)
        expr)
    (symbolo p-name)
    (list-of-symbolso x)
    (eval-expo letrec-body
               `((,p-name . (rec . (lambda ,x ,body))) . ,env)
               val
               'letrec-body)))

(define (lambda-!-/evalo expr gamma env type val)
  (fresh (x body)
    (== `(lambda ,x ,body) expr)
    (== `(closure (lambda ,x ,body) ,env) val)
    (list-of-symbolso x)))

(define (app-!-/evalo expr gamma env type val)
  (fresh (rator x* rands body env^ a* res)
    (== `(,rator . ,rands) expr)
    ;; Multi-argument
    (eval-expo rator env `(closure (lambda ,x* ,body) ,env^) 'app-rator)
    (eval-randso rands env a*)
    (ext-env*o x* a* env^ res)
    (eval-expo body res val 'lambda)))







(define (equal?-!-/evalo expr gamma env type val)
  (fresh (e1 e2 v1 v2)
    (== `(equal? ,e1 ,e2) expr)
    (conde
      ((== v1 v2) (== #t val))
      ((=/= v1 v2) (== #f val)))
    (eval-expo e1 env v1 'equal?-e1)
    (eval-expo e2 env v2 'equal?-e2)))

(define (and-!-/evalo expr gamma env type val)
  (fresh (e*)
    (== `(and . ,e*) expr)
    (ando e* env type val)))

(define (or-!-/evalo expr gamma env type val)
  (fresh (e*)
    (== `(or . ,e*) expr)
    (oro e* env type val)))

(define (list-!-/evalo expr gamma env type val)
  (fresh (rands a*)
    (== `(list . ,rands) expr)
    (== a* val)
    (eval-listo rands env a*)))

(define (symbol?-!-/evalo expr gamma env type val)
  (fresh (e v)
    (== `(symbol? ,e) expr)
    (conde
      ((symbolo v) (== #t val))
      ((not-symbolo v) (== #f val)))
    (eval-expo e env v 'symbol?)))

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
    (conde
      ((== #t v) (== #f val))
      ((== #f v) (== #t val)))
    (eval-expo e env v 'not)))





(define (lookup-!-/evalo x env t)
  (fresh (y b rest)
    (symbolo x)
    (== `((,y . ,b) . ,rest) env)
    (symbolo y)
    (conde
      ((== x y)
       (conde
         ((== `(val . ,t) b))
         ((fresh (lam-expr)
            (== `(rec . ,lam-expr) b)
            (== `(closure ,lam-expr ,env) t)))))
      ((=/= x y)
       (lookup-!-/evalo x rest t)))))

(define (eval-randso expr gamma env type val)
  (conde
    ((== '() expr)
     (== '() val))
    ((fresh (a d v-a v-d)
       (== `(,a . ,d) expr)
       (== `(,v-a . ,v-d) val)
       (eval-expo a env v-a 'app-rand*)
       (eval-randso d env v-d)))))

(define (eval-listo expr gamma env type val)
  (conde
    ((== '() expr)
     (== '() val))
    ((fresh (a d v-a v-d)
       (== `(,a . ,d) expr)
       (== `(,v-a . ,v-d) val)
       (eval-expo a env v-a 'list)
       (eval-listo d env v-d)))))

;; need to make sure lambdas are well formed.
;; grammar constraints would be useful here!!!
(define (list-of-symbolso los)
  (conde
    ((== '() los))
    ((fresh (a d)
       (== `(,a . ,d) los)
       (symbolo a)
       (list-of-symbolso d)))))

(define (ext-env*o x* a* env out)
  (conde
    ((== '() x*) (== '() a*) (== env out))
    ((fresh (x a dx* da* env2)
       (== `(,x . ,dx*) x*)
       (== `(,a . ,da*) a*)
       (== `((,x . (val . ,a)) . ,env) env2)
       (symbolo x)
       (ext-env*o dx* da* env2 out)))))

(define (ando e* env type val)
  (conde
    ((== '() e*) (== #t val))
    ((fresh (e)
       (== `(,e) e*)
       (conde
         ((== #f val))
         ((== #t val)))
       (eval-expo e env type val 'and)))
    ((fresh (e1 e2 e-rest v)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (conde
         ((== #f v)
          (== #f val)
          (eval-expo e1 env v 'and))
         ((== #t v)
          (eval-expo e1 env v 'and)
          (ando `(,e2 . ,e-rest) env type val)))))))

(define (oro e* env type val)
  (conde
    ((== '() e*) (== #f val))
    ((fresh (e)
       (== `(,e) e*)
       (conde
         ((== #f val))
         ((== #t val)))
       (eval-expo e env type val 'or)))
    ((fresh (e1 e2 e-rest v)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (conde
         ((== #t v)
          (== v val)
          (eval-expo e1 env v 'or))
         ((== #f v)
          (eval-expo e1 env v 'or)
          (oro `(,e2 . ,e-rest) env type val)))))))



(define empty-env '())

(define (!-/evalo expr type val)
  (!-/eval-expo expr empty-gamma empty-env type val 'top-level))

;; WEB--is this actually used???
#|
(define (alist-ref alist element failure-result)
  (let ((pr (assoc element alist)))
    (if pr (cdr pr) failure-result)))
|#

;;; !!! Careful!
;;;
;;; This optimized lookup relies on the ability to recursively pass
;;; around a non-symbol 'x', to reach the base case.
;;;
;;; Don't add a (symbolo x) test, since that will break this code!
;;;
;;; (Is there a better way to write this???)
(define (lookup-!-/evalo-k k)
  (lambda (x env t)
    (conde
      ((== '() env) k)
      ((fresh (y b rest)
         (== `((,y . ,b) . ,rest) env)
         (symbolo y)
         (conde
           ((== x y)           
            (conde
              ((== `(val . ,t) b))
              ((fresh (lam-expr)
                 (== `(rec . ,lam-expr) b)
                 (== `(closure ,lam-expr ,env) t)))))
           ((=/= x y)
            ((lookup-!-/evalo-k k) x rest t))))))))

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
