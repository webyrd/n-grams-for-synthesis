;; This version uses expert hand-ordering of clauses (combination of expert knowledge and looking at statistics gathered from n-grams.scm)


;; TODO create a version of this file in which eval-expo is generated by a macro, and then create a version of that file in which ordering is auto generated based on statistics


;; 'interp-simplified' inlines primitives and makes them second-class, removes variadic functions,
;; and removes the non-empty initial environment.  Shadowing of core forms is no longer supported.
;; These simplifications were recommended by Alan Mycroft of U of Cambridge.

;; The definition of 'letrec' is based based on Dan Friedman's code,
;; using the "half-closure" approach from Reynold's definitional
;; interpreters.


(define (quote-evalo expr env val)
  (fresh ()
    (== `(quote ,val) expr)
    (absento 'closure val)
    (absento 'prim val)))

(define (num-evalo expr env val)
  (fresh ()
    (numbero expr)
    (== expr val)))

(define (bool-evalo expr env val)
  (conde
    ((== #t expr) (== #t val))
    ((== #f expr) (== #f val))))

(define (var-evalo expr env val)
  (fresh ()
    (symbolo expr)
    (lookupo expr env val)))

(define (lambda-evalo expr env val)
  (fresh (x body)
    (== `(lambda ,x ,body) expr)
    (== `(closure (lambda ,x ,body) ,env) val)
    (list-of-symbolso x)))

(define (app-evalo expr env val)
  (fresh (rator x* rands body env^ a* res)
    (== `(,rator . ,rands) expr)
    ;; Multi-argument
    (eval-expo rator env `(closure (lambda ,x* ,body) ,env^))
    (eval-listo rands env a*)
    (ext-env*o x* a* env^ res)
    (eval-expo body res val)))

(define (car-evalo expr env val)
  (fresh (e d)
    (== `(car ,e) expr)
    (=/= 'closure val)
    (eval-expo e env `(,val . ,d))))

(define (cdr-evalo expr env val)
  (fresh (e a)
    (== `(cdr ,e) expr)
    (=/= 'closure a)
    (eval-expo e env `(,a . ,val))))

(define (null?-evalo expr env val)
  (fresh (e v)
    (== `(null? ,e) expr)
    (conde
      ((== '() v) (== #t val))
      ((=/= '() v) (== #f val)))
    (eval-expo e env v)))

(define (cons-evalo expr env val)
  (fresh (e1 e2 v1 v2)
    (== `(cons ,e1 ,e2) expr)
    (== `(,v1 . ,v2) val)
    (eval-expo e1 env v1)
    (eval-expo e2 env v2)))

(define (if-evalo expr env val)
  (fresh (e1 e2 e3 t)
    (== `(if ,e1 ,e2 ,e3) expr)
    (eval-expo e1 env t)
    (conde
      ((=/= #f t) (eval-expo e2 env val))
      ((== #f t) (eval-expo e3 env val)))))

(define (equal?-evalo expr env val)
  (fresh (e1 e2 v1 v2)
    (== `(equal? ,e1 ,e2) expr)
    (conde
      ((== v1 v2) (== #t val))
      ((=/= v1 v2) (== #f val)))
    (eval-expo e1 env v1)
    (eval-expo e2 env v2)))

(define (and-evalo expr env val)
  (fresh (e*)
    (== `(and . ,e*) expr)
    (ando e* env val)))

(define (or-evalo expr env val)
  (fresh (e*)
    (== `(or . ,e*) expr)
    (oro e* env val)))

(define (list-evalo expr env val)
  (fresh (rands a*)
    (== `(list . ,rands) expr)
    (== a* val)
    (eval-listo rands env a*)))

(define (symbol?-evalo expr env val)
  (fresh (e v)
    (== `(symbol? ,e) expr)
    (conde
      ((symbolo v) (== #t val))
      ((not-symbolo v) (== #f val)))
    (eval-expo e env v)))

(define (not-evalo expr env val)
  (fresh (e v)
    (== `(not ,e) expr)
    (conde
      ((=/= #f v) (== #f val))
      ((== #f v) (== #t val)))
    (eval-expo e env v)))

(define (letrec-evalo expr env val)
  (fresh (p-name x body letrec-body)
    ;; single-function muti-argument letrec version
    (== `(letrec ((,p-name (lambda ,x ,body)))
           ,letrec-body)
        expr)
    (list-of-symbolso x)
    (eval-expo letrec-body
               `((,p-name . (rec . (lambda ,x ,body))) . ,env)
               val)))



(define (lookupo x env t)
  (fresh (y b rest)
    (== `((,y . ,b) . ,rest) env)
    (conde
      ((== x y)
       (conde
         ((== `(val . ,t) b))
         ((fresh (lam-expr)
            (== `(rec . ,lam-expr) b)
            (== `(closure ,lam-expr ,env) t)))))
      ((=/= x y)
       (lookupo x rest t)))))

(define (eval-listo expr env val)
  (conde
    ((== '() expr)
     (== '() val))
    ((fresh (a d v-a v-d)
       (== `(,a . ,d) expr)
       (== `(,v-a . ,v-d) val)
       (eval-expo a env v-a)
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

(define (ando e* env val)
  (conde
    ((== '() e*) (== #t val))
    ((fresh (e)
       (== `(,e) e*)
       (eval-expo e env val)))
    ((fresh (e1 e2 e-rest v)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (conde
         ((== #f v)
          (== #f val)
          (eval-expo e1 env v))
         ((=/= #f v)
          (eval-expo e1 env v)
          (ando `(,e2 . ,e-rest) env val)))))))

(define (oro e* env val)
  (conde
    ((== '() e*) (== #f val))
    ((fresh (e)
       (== `(,e) e*)
       (eval-expo e env val)))
    ((fresh (e1 e2 e-rest v)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (conde
         ((=/= #f v)
          (== v val)
          (eval-expo e1 env v))
         ((== #f v)
          (eval-expo e1 env v)
          (oro `(,e2 . ,e-rest) env val)))))))





(define match-evalo
  (lambda  (expr env val)
    (fresh (against-expr mval clause clauses)
      (== `(match ,against-expr ,clause . ,clauses) expr)
      (eval-expo against-expr env mval)
      (match-clauses mval `(,clause . ,clauses) env val))))

(define (not-symbolo t)
  (conde
    ((== #f t))
    ((== #t t))
    ((== '() t))
    ((numbero t))
    ((fresh (a d)
       (== `(,a . ,d) t)))))

(define (not-numbero t)
  (conde
    ((== #f t))
    ((== #t t))
    ((== '() t))
    ((symbolo t))
    ((fresh (a d)
       (== `(,a . ,d) t)))))

(define (self-eval-literalo t)
  (conde
    ((numbero t))
    ((booleano t))))

(define (literalo t)
  (conde
    ((numbero t))
    ((symbolo t) (=/= 'closure t))
    ((booleano t))
    ((== '() t))))

(define (booleano t)
  (conde
    ((== #f t))
    ((== #t t))))

(define (regular-env-appendo env1 env2 env-out)
  (conde
    ((== empty-env env1) (== env2 env-out))
    ((fresh (y v rest res)
       (== `((,y . (val . ,v)) . ,rest) env1)
       (== `((,y . (val . ,v)) . ,res) env-out)
       (regular-env-appendo rest env2 res)))))

(define (match-clauses mval clauses env val)
  (fresh (p result-expr d penv)
    (== `((,p ,result-expr) . ,d) clauses)
    (conde
      ((fresh (env^)
         (p-match p mval '() penv)
         (regular-env-appendo penv env env^)
         (eval-expo result-expr env^ val)))
      ((p-no-match p mval '() penv)
       (match-clauses mval d env val)))))

(define (not-in-envo x env)
  (conde
    ((== empty-env env))
    ((fresh (y b rest)
       (== `((,y . ,b) . ,rest) env)
       (=/= y x)
       (not-in-envo x rest)))))

(define (var-p-match var mval penv penv-out)
  (fresh (val)
    (symbolo var)
    (=/= 'closure mval)
    (conde
      ((== mval val)
       (== penv penv-out)
       (lookupo var penv val))
      ((== `((,var . (val . ,mval)) . ,penv) penv-out)
       (not-in-envo var penv)))))

(define (var-p-no-match var mval penv penv-out)
  (fresh (val)
    (symbolo var)
    (=/= mval val)
    (== penv penv-out)
    (lookupo var penv val)))

(define (p-match p mval penv penv-out)
  (conde
    ((self-eval-literalo p)
     (== p mval)
     (== penv penv-out))
    ((var-p-match p mval penv penv-out))
    ((fresh (var pred val)
      (== `(? ,pred ,var) p)
      (conde
        ((== 'symbol? pred)
         (symbolo mval))
        ((== 'number? pred)
         (numbero mval)))
      (var-p-match var mval penv penv-out)))
    ((fresh (quasi-p)
      (== (list 'quasiquote quasi-p) p)
      (quasi-p-match quasi-p mval penv penv-out)))))

(define (p-no-match p mval penv penv-out)
  (conde
    ((self-eval-literalo p)
     (=/= p mval)
     (== penv penv-out))
    ((var-p-no-match p mval penv penv-out))
    ((fresh (var pred val)
       (== `(? ,pred ,var) p)
       (== penv penv-out)
       (symbolo var)
       (conde
         ((== 'symbol? pred)
          (conde
            ((not-symbolo mval))
            ((symbolo mval)
             (var-p-no-match var mval penv penv-out))))
         ((== 'number? pred)
          (conde
            ((not-numbero mval))
            ((numbero mval)
             (var-p-no-match var mval penv penv-out)))))))
    ((fresh (quasi-p)
      (== (list 'quasiquote quasi-p) p)
      (quasi-p-no-match quasi-p mval penv penv-out)))))

(define (quasi-p-match quasi-p mval penv penv-out)
  (conde
    ((== quasi-p mval)
     (== penv penv-out)
     (literalo quasi-p))
    ((fresh (p)
      (== (list 'unquote p) quasi-p)
      (p-match p mval penv penv-out)))
    ((fresh (a d v1 v2 penv^)
       (== `(,a . ,d) quasi-p)
       (== `(,v1 . ,v2) mval)
       (=/= 'unquote a)
       (quasi-p-match a v1 penv penv^)
       (quasi-p-match d v2 penv^ penv-out)))))

(define (quasi-p-no-match quasi-p mval penv penv-out)
  (conde
    ((=/= quasi-p mval)
     (== penv penv-out)
     (literalo quasi-p))
    ((fresh (p)
       (== (list 'unquote p) quasi-p)
       (=/= 'closure mval)
       (p-no-match p mval penv penv-out)))
    ((fresh (a d)
       (== `(,a . ,d) quasi-p)
       (=/= 'unquote a)
       (== penv penv-out)
       (literalo mval)))
    ((fresh (a d v1 v2 penv^)
       (== `(,a . ,d) quasi-p)
       (=/= 'unquote a)
       (== `(,v1 . ,v2) mval)
       (conde
         ((quasi-p-no-match a v1 penv penv^))
         ((quasi-p-match a v1 penv penv^)
          (quasi-p-no-match d v2 penv^ penv-out)))))))


(define empty-env '())

(define (evalo expr val)
  (eval-expo expr empty-env val))

(define (alist-ref alist element failure-result)
  (let ([pr (assoc element alist)])
    (if pr (cdr pr) failure-result)))

(define ngrams-statistics
  (let ((op (open-file-input-port
             "statistics.scm"
             (file-options no-fail)
             (buffer-mode block)
             (make-transcoder (utf-8-codec)))))
    (let ([res (read op)])
      (close-input-port op)
      res)))

(define expert-ordering-alist
  `((quote . ,quote-evalo)
     (num . ,num-evalo)
     (bool . ,bool-evalo)
     (var . ,var-evalo)
     (lambda . ,lambda-evalo)
     (app . ,app-evalo)
     (car . ,car-evalo)
     (cdr . ,cdr-evalo)
     (null? . ,null?-evalo)
     (cons . ,cons-evalo)
     (if . ,if-evalo)
     (equal? . ,equal?-evalo)
     (and . ,and-evalo)
     (or . ,or-evalo)
     (list . ,list-evalo)
     (symbol? . ,symbol?-evalo)
     (not . ,not-evalo)
     (letrec . ,letrec-evalo)
     (match . ,match-evalo)))

(define expert-ordering
  (map cdr expert-ordering-alist))

(define unique
  (lambda (l)
    (if (null? l)
      '()
      (cons (car l) (remove (car l) (unique (cdr l)))))))

(define all-contexts (unique (map caar ngrams-statistics)))

(define orderings-alist
  (let ([ordering-for-context
          (lambda (ctx)
            (let ([ctx-stats (map (lambda (entry) (cons (cadar entry) (cdr entry)))
                                  (filter (lambda (entry) (equal? ctx (caar entry))) ngrams-statistics))])
              (let ([compare
                      (lambda (a b)
                        (> (alist-ref ctx-stats (car a) 0)
                           (alist-ref ctx-stats (car b) 0)))])
                (map cdr (list-sort compare expert-ordering-alist)))))])
    (map (lambda (ctx)
           (cons ctx (ordering-for-context ctx)))
         all-contexts)))

(define order-eval-relations
  (lambda (context)
    (cond
      [(assoc context orderings-alist) => cdr]
      [else
        (error 'eval-expo "bad context")])))

(define (eval-expo expr env val context)
  (build-and-run-conde expr env val
                       ;(order-eval-relations context)
                       expert-ordering
                       ))

(define build-and-run-conde
  (lambda (expr env val list-of-eval-relations)
    (lambda (st)
      (let loop ([list-of-eval-relations list-of-eval-relations])
        (cond
          [(null? list-of-eval-relations) (mzero)]
          [else
            (mplus (((car list-of-eval-relations) expr env val) st)
                   (inc (loop (cdr list-of-eval-relations))))])))))
