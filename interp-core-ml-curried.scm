;; ML-ish version of the interpreter, based on joint work with Kanae Tsushima.

(define (bool-evalo expr env val)
  (conde
    ((== #t expr) (== #t val))
    ((== #f expr) (== #f val))))

(define (num-evalo expr env val)
  (fresh ()
    (numbero expr)
    (== expr val)))

(define (nil-evalo expr env val)
  (fresh ()
    (== 'nil expr)
    (== 'nil val)))

(define (var-evalo expr env val)
  (fresh ()
    (symbolo expr)
    (=/= 'nil expr)
    (lookupo expr env val)))

(define (null?-evalo expr env val)
  (fresh (e v)
    (== `(null? ,e) expr)
    (conde
      ((== 'nil v) (== #t val))
      ((=/= 'nil v) (== #f val)))
    (eval-expo e env v 'null?)))

(define (car-evalo expr env val)
  (fresh (e d)
    (== `(car ,e) expr)
    (eval-expo e env `(cons ,val ,d) 'car)))

(define (cdr-evalo expr env val)
  (fresh (e a)
    (== `(cdr ,e) expr)
    (eval-expo e env `(cons ,a ,val) 'cdr)))

(define (cons-evalo expr env val)
  (fresh (e1 e2 v1 v2)
    (== `(cons ,e1 ,e2) expr)
    (== `(cons ,v1 ,v2) val)
    (eval-expo e1 env v1 'cons-e1)
    (eval-expo e2 env v2 'cons-e2)))

(define (pair-evalo expr env val)
  (fresh (e1 e2 v1 v2)
    (== `(pair ,e1 ,e2) expr)
    (== `(pair ,v1 ,v2) val)
    (eval-expo e1 env v1 'pair-e1)
    (eval-expo e2 env v2 'pair-e2)))

(define (if-evalo expr env val)
  (fresh (e1 e2 e3 t)
    (== `(if ,e1 ,e2 ,e3) expr)
    (eval-expo e1 env t 'if-test)
    (conde
      ((== #t t) (eval-expo e2 env val 'if-conseq))
      ((== #f t) (eval-expo e3 env val 'if-alt)))))

(define (letrec-evalo expr env val)
  (fresh (p-name x body letrec-body)
    ;; single-function curried letrec version
    (== `(letrec ((,p-name (lambda (,x) ,body)))
           ,letrec-body)
        expr)
    (symbolo p-name)
    (symbolo x)
    (eval-expo letrec-body
               `((,p-name (rec (lambda (,x) ,body))) . ,env)
               val
               'letrec-body)))

(define (lambda-evalo expr env val)
  (fresh (x body)
    (== `(lambda (,x) ,body) expr)
    (symbolo x)
    (== `(closure (,x) ,body ,env) val)))

(define (app-evalo expr env val)
  (fresh (rator x rand body arg env^)
    (== `(,rator ,rand) expr)
    ;; Curried
    (eval-expo rator env `(closure (,x) ,body ,env^) 'app-rator)
    ;; should probably be 'app-rand rather than 'app-rand*
    (eval-expo rand env arg 'app-rand*)
    (eval-expo body `((,x (val ,arg)) . ,env^) val 'lambda)))







(define (equal?-evalo expr env val)
  (fresh (e1 e2 v1 v2)
    (== `(equal? ,e1 ,e2) expr)
    (conde
      ((== v1 v2) (== #t val))
      ((=/= v1 v2) (== #f val)))
    (eval-expo e1 env v1 'equal?-e1)
    (eval-expo e2 env v2 'equal?-e2)))

(define (symbol?-evalo expr env val)
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

(define (not-evalo expr env val)
  (fresh (e v)
    (== `(not ,e) expr)
    (conde
      ((== #t v) (== #f val))
      ((== #f v) (== #t val)))
    (eval-expo e env v 'not)))





(define (lookupo x env t)
  (fresh (y b rest z e)
    (symbolo x)
    (== `((,y ,b) . ,rest) env)
    (symbolo y)
    (conde
      ((== x y)
       (conde
         ((== `(val ,t) b))
         ((== `(rec (lambda (,z) ,e)) b)
          (== `(closure (,z) ,e ,env) t))))
      ((=/= x y)
       (lookupo x rest t)))))




(define empty-env '())

(define (evalo expr val)
  (eval-expo expr empty-env val 'top-level))

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
(define (lookupo-k k)
  (lambda (x env t)
    (conde
      ((== '() env) k)
      ((fresh (y b rest)
         (== `((,y ,b) . ,rest) env)
         (symbolo y)
         (conde
           ((== x y)           
            (conde
              ((== `(val ,t) b))
              ((fresh (lam-expr z e)
                 (== `(rec (lambda (,z) ,e)) b)
                 (== `(closure (,z) ,e ,env) t)))))
           ((=/= x y)
            ((lookupo-k k) x rest t))))))))

(define build-and-run-conde
  (lambda (expr env val list-of-eval-relations)
    (let ((k (lambdag@ (st)
               (inc (bind (state-depth-deepen (state-with-scope st (new-scope)))
                          (lambdag@ (st)
                            (let loop ((list-of-eval-relations list-of-eval-relations))
                              (cond
                                ((null? list-of-eval-relations) (mzero))
                                (else
                                 (mplus (((car list-of-eval-relations) expr env val) st)
                                        (inc (loop (cdr list-of-eval-relations)))))))))))))
      (if lookup-optimization?
          ((lookupo-k k) expr env val)
          k))))
