;; Type inferencer for ML-ish language, based on joint work with Kanae Tsushima.

;; Possible changes/improvements:
;;
;; * May want to include the types of arguments
;; as part of the formal parameters to a lambda:
;;
;; (lambda (x : int) x)
;;
;; * May prefer a curried language for localizing holes
;; in ill-typed programs.

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
  (fresh (p-name x body letrec-body t)
    ;; single-function curried letrec version
    (== `(letrec ((,p-name (lambda (,x) ,body)))
           ,letrec-body)
        expr)
    (symbolo p-name)
    (symbolo x)

   ;; Make sure the right-hand-side of the polymorphic 'letrec'
   ;; binding has a type, but then forget about the type.
   (fresh (forget-me)     
     (!-o `(lambda (,x) ,body)
          `((,p-name (mono ,forget-me)) . ,gamma)
          forget-me
          'letrec-rhs))
   
   (!-o letrec-body
        `((,p-name (poly ((,p-name (mono ,t)) . ,gamma)
                         (lambda (,x) ,body)))
          . ,gamma)
        type
        'letrec-body)))

(define (lambda-!-o expr gamma type)
  (fresh (x body t t^ gamma^)
    (== `(lambda (,x) ,body) expr)
    (== `(-> ,t ,t^) type)
    (!-o body `((,x (mono ,t)) . ,gamma) t^ 'lambda-body)))

(define (app-!-o expr gamma type)
  (fresh (rator rand t)
    (== `(,rator ,rand) expr)
    ;; curried
    (!-o rator gamma `(-> ,t ,type) 'app-rator)
    (!-o rand gamma t)))

(define (equal?-!-o expr gamma type)
  (fresh (e1 e2 t)
    (== `(equal? ,e1 ,e2) expr)
    (== 'bool type)
    (!-o e1 gamma t 'equal?-e1)
    (!-o e2 gamma t 'equal?-e2)))

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
  (fresh (y t rest gamma^ x body)
    (symbolo x)
    (== `((,y ,t) . ,rest) gamma)
    (symbolo y)
    (conde
      ((== x y)
       (conde
         ((== `(mono ,type) t))
         ((== `(poly ,gamma^ (lambda (,x) ,body)) t)
          (!-o `(lambda (,x) ,body) gamma^ type 'letrec-rhs))))
      ((=/= x y)
       (lookup-!-o x rest type)))))



(define empty-gamma '())

(define (type-expo expr type)
  (!-o expr empty-gamma type 'top-level))

(define (alist-ref-ml-infer alist element failure-result)
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
(define (lookup-!-o-k k)
  (lambda (x gamma type gamma^ x body)
    (conde
      ((== '() gamma) k)
      ((fresh (y t rest)
         (== `((,y ,t) . ,rest) gamma)
         (symbolo y)
         (conde
           ((== x y)
            (conde
              ((== `(mono ,type) t))
              ((== `(poly ,gamma^ (lambda (,x) ,body)) t)
               (!-o `(lambda (,x) ,body) gamma^ type 'letrec-rhs))))
           ((=/= x y)
            ((lookup-!-o-k k) x rest type))))))))

(define build-and-run-conde-ml-infer
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
