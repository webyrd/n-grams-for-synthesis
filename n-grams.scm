(load "pmatch.scm")
(load "scheme-training-code.scm")

(define bigrams-for-expr
  (lambda (expr)
    (letrec ((bigrams-for-expr
              (lambda (expr parent defn-name args)
                (pmatch expr
                  [(eq? . ,e*)
                   (error 'bigrams-for-expr (format "unconverted eq?"))]
                  [(cond . ,e*)
                   (error 'bigrams-for-expr (format "unconverted cond"))]
                  [(quote ())
                   (list (list 'nil parent))]
                  [(quote ,x) (guard (symbol? x))
                   (list (list 'quoted-symbol parent))]
                  [(quote ,ls) (guard (list? ls))
                   (list (list 'quoted-non-empty-list parent))]
                  [(quote ,_)
                   (error 'bigrams-for-expr (format "unknown quoted form ~s" _))]                  
                  [#t
                   (list (list 'bool parent))]
                  [#f
                   (list (list 'bool parent))]
                  [,n (guard (number? n))
                   (list (list 'num parent))]                  
                  [,x (guard (symbol? x))
                   (list
                     (cond
                       [(eqv? x defn-name) (list 'var-recur parent)]
                       [(memv x args) (list 'var-arg parent)]
                       [else (list 'var parent)]))]
                  [(define ,id ,e) (guard (symbol? id))
                   (cons (list 'define parent) (bigrams-for-expr e 'define id args))]
                  [(lambda ,x ,body)
                   (cons (list 'lambda parent)
                         (bigrams-for-expr body
                                           (if (symbol? x) 'lambda-variadic 'lambda-multi)
                                           defn-name
                                           (if (symbol? x) (list x) x)))]
                  [(if ,test ,conseq ,alt)
                   (cons (list 'if parent)
                         (append (bigrams-for-expr test 'if-test defn-name args)
                                 (bigrams-for-expr conseq 'if-conseq defn-name args)
                                 (bigrams-for-expr alt 'if-alt defn-name args)))]
                  [(symbol? ,e)
                   (cons (list 'symbol? parent)
                         (bigrams-for-expr e 'symbol? defn-name args))]
                  [(not ,e)
                   (cons (list 'not parent)
                         (bigrams-for-expr e 'not defn-name args))]
                  [(and . ,e*)
                   (cons (list 'and parent)
                         (apply append (map (lambda (e) (bigrams-for-expr e 'and defn-name args)) e*)))]
                  [(or . ,e*)
                   (cons (list 'or parent)
                         (apply append (map (lambda (e) (bigrams-for-expr e 'or defn-name args)) e*)))]
                  [(list . ,e*)
                   (cons (list 'list parent)
                         (apply append (map (lambda (e) (bigrams-for-expr e 'list defn-name args)) e*)))]
                  [(null? ,e)
                   (cons (list 'null? parent)
                         (bigrams-for-expr e 'null? defn-name args))]
                  [(pair? ,e)
                   (cons (list 'pair? parent)
                         (bigrams-for-expr e 'pair? defn-name args))]
                  [(car ,e)
                   (cons (list 'car parent)
                         (bigrams-for-expr e 'car defn-name args))]
                  [(cdr ,e)
                   (cons (list 'cdr parent)
                         (bigrams-for-expr e 'cdr defn-name args))]
                  [(cons ,e1 ,e2)
                   (cons (list 'cons parent)
                         (append (bigrams-for-expr e1 'cons-a defn-name args)
                                 (bigrams-for-expr e2 'cons-d defn-name args)))]
                  [(equal? ,e1 ,e2)
                   (cons (list 'equal? parent)
                         (append (bigrams-for-expr e1 'equal? defn-name args)
                                 (bigrams-for-expr e2 'equal? defn-name args)))]
                  [(letrec ((,id (lambda ,x ,body))) ,e)
                   (cons (list 'letrec parent)
                         (append (bigrams-for-expr `(lambda ,x ,body) 'letrec defn-name args)
                                 (bigrams-for-expr e 'letrec id args)))]
                  [(,e . ,e*) ;; application
                   (cons (list 'app parent)
                         (append (bigrams-for-expr e 'app-rator defn-name args)
                                 (apply append (map (lambda (e) (bigrams-for-expr e 'app-rand* defn-name args)) e*))))]
                  [else (error 'bigrams-for-expr (format "unknown expression type ~s" expr))]))))
      (bigrams-for-expr expr 'top-level #f #f))))


(define count-bigrams
  (lambda (bg-ls)
    (letrec ((count-bigrams
              (lambda (bg-ls count-al)
                (cond
                  [(null? bg-ls)
                   (sort
                     (lambda (e1 e2)
                       (or
                        (string<? (symbol->string (caar e1))
                                  (symbol->string (caar e2)))
                        (and
                         (string=? (symbol->string (caar e1))
                                   (symbol->string (caar e2)))
                         (string<? (symbol->string (cadar e1))
                                   (symbol->string (cadar e2))))))
                     count-al)]
                  [else
                   (let ((bg (car bg-ls)))
                     (let ((count-al
                            (cond
                              [(assoc bg count-al) =>
                               (lambda (pr)
                                 (cons (cons bg (add1 (cdr pr)))
                                       (remove pr count-al)))]
                              [else (cons (cons bg 1) count-al)])))
                       (count-bigrams (cdr bg-ls) count-al)))]))))
      (count-bigrams bg-ls '()))))

(define sort-counts-al-by-type/counts
  (lambda (counts-al)
    (sort
     (lambda (e1 e2)
       (or
        (string<? (symbol->string (caar e1))
                  (symbol->string (caar e2)))
        (and
         (string=? (symbol->string (caar e1))
                   (symbol->string (caar e2)))
         (> (cdr e1) (cdr e2)))))
     counts-al)))

(define bigrams (apply append (map bigrams-for-expr exprs)))
(define bigram-counts (count-bigrams (map reverse bigrams)))
(define bigrams-sorted-by-counts (sort (lambda (e1 e2) (> (cdr e1) (cdr e2))) bigram-counts))

;; this is the important one
(define bigrams-sorted-by-type/counts (sort-counts-al-by-type/counts bigram-counts))

#!eof

;; bigrams-sorted-by-type/counts
(((and app) . 22)
 ((and null?) . 8)
 ((and equal?) . 2)
 ((app-rand* var-arg) . 103)
 ((app-rand* cdr) . 65)
 ((app-rand* car) . 40)
 ((app-rand* app) . 24)
 ((app-rand* cons) . 3)
 ((app-rand* nil) . 3)
 ((app-rand* lambda) . 2)
 ((app-rand* if) . 1)
 ((app-rand* list) . 1)
 ((app-rand* null?) . 1)
 ((app-rator var-recur) . 72)
 ((app-rator var) . 61)
 ((app-rator var-arg) . 6)
 ((car var-arg) . 82)
 ((car cdr) . 6)
 ((car car) . 2)
 ((car var) . 2)
 ((cdr var-arg) . 75)
 ((cdr cdr) . 3)
 ((cdr car) . 1)
 ((cons-a car) . 19)
 ((cons-a var-arg) . 10)
 ((cons-a app) . 5)
 ((cons-a cons) . 1)
 ((cons-d app) . 27)
 ((cons-d cons) . 4)
 ((cons-d var-arg) . 2)
 ((cons-d cdr) . 1)
 ((cons-d list) . 1)
 ((define lambda) . 44)
 ((equal? car) . 22)
 ((equal? var-arg) . 17)
 ((equal? quoted-symbol) . 3)
 ((if-alt if) . 40)
 ((if-alt app) . 21)
 ((if-alt cons) . 17)
 ((if-alt and) . 3)
 ((if-alt bool) . 2)
 ((if-alt equal?) . 1)
 ((if-alt or) . 1)
 ((if-conseq bool) . 24)
 ((if-conseq nil) . 15)
 ((if-conseq app) . 10)
 ((if-conseq cons) . 10)
 ((if-conseq var-arg) . 7)
 ((if-conseq num) . 6)
 ((if-conseq and) . 5)
 ((if-conseq car) . 3)
 ((if-conseq cdr) . 3)
 ((if-conseq if) . 1)
 ((if-conseq or) . 1)
 ((if-test null?) . 33)
 ((if-test app) . 20)
 ((if-test equal?) . 17)
 ((if-test and) . 8)
 ((if-test or) . 4)
 ((if-test pair?) . 2)
 ((if-test not) . 1)
 ((lambda-multi if) . 43)
 ((lambda-multi app) . 3)
 ((list car) . 2)
 ((not pair?) . 1)
 ((null? var-arg) . 45)
 ((null? cdr) . 1)
 ((or app) . 7)
 ((or null?) . 4)
 ((or equal?) . 1)
 ((pair? car) . 3)
 ((top-level define) . 44))




;; old

(sort (lambda (e1 e2) (string<? (symbol->string (car e1)) (symbol->string (car e2))))
      '((define top-level)
        (lambda define)
        (if lambda)
        (null? if)
        (var null?)
        (nil if)
        (cons if)
        (app cons)
        (var app)
        (car app)
        (var car)
        (app cons)
        (var app)
        (var app)
        (cdr app)
        (var cdr)))
=>
((app cons)
 (app cons)
 (car app)
 (cdr app)
 (cons if)
 (define top-level)
 (if lambda)
 (lambda define)
 (nil if)
 (null? if)
 (var null?)
 (var app)
 (var car)
 (var app)
 (var app)
 (var cdr))

(sort (lambda (e1 e2) (string<? (symbol->string (car e1)) (symbol->string (car e2))))
      '((define top-level)
        (lambda define)
        (if lambda)
        (null? if-test)
        (var-arg null?)
        (nil if-conseq)
        (cons if-alt)
        (app cons-a)
        (var-arg app-rator)
        (car app-rand*)
        (var-arg car)
        (app cons-d)
        (var-recur app-rator)
        (var-arg app-rand*)
        (cdr app-rand*)
        (var-arg cdr)))
=>
((app cons-a)
 (app cons-d)
 (car app-rand*)
 (cdr app-rand*)
 (cons if-alt)
 (define top-level)
 (if lambda)
 (lambda define)
 (nil if-conseq)
 (null? if-test)
 (var-arg null?)
 (var-arg app-rator)
 (var-arg car)
 (var-arg app-rand*)
 (var-arg cdr)
 (var-recur app-rator))




(count-bigrams
 (map reverse
      '((app cons)
        (app cons)
        (car app)
        (cdr app)
        (cons if)
        (define top-level)
        (if lambda)
        (lambda define)
        (nil if)
        (null? if)
        (var null?)
        (var app)
        (var car)
        (var app)
        (var app)
        (var cdr))))
=>
(((app car) . 1)
 ((app cdr) . 1)
 ((app var) . 3)
 ((car var) . 1)
 ((cdr var) . 1)
 ((cons app) . 2)
 ((define lambda) . 1)
 ((if cons) . 1)
 ((if nil) . 1)
 ((if null?) . 1)
 ((lambda if) . 1)
 ((null? var) . 1)
 ((top-level define) . 1))

(count-bigrams
 (map reverse
      '((app cons-a)
        (app cons-d)
        (car app-rand*)
        (cdr app-rand*)
        (cons if-alt)
        (define top-level)
        (if lambda)
        (lambda define)
        (nil if-conseq)
        (null? if-test)
        (var-arg null?)
        (var-arg app-rator)
        (var-arg car)
        (var-arg app-rand*)
        (var-arg cdr)
        (var-recur app-rator))))
=>
(((app-rand* car) . 1)
 ((app-rand* cdr) . 1)
 ((app-rand* var-arg) . 1)
 ((app-rator var-arg) . 1)
 ((app-rator var-recur) . 1)
 ((car var-arg) . 1)
 ((cdr var-arg) . 1)
 ((cons-a app) . 1)
 ((cons-d app) . 1)
 ((define lambda) . 1)
 ((if-alt cons) . 1)
 ((if-conseq nil) . 1)
 ((if-test null?) . 1)
 ((lambda if) . 1)
 ((null? var-arg) . 1)
 ((top-level define) . 1))




;; old code

(define paths-for-expr
  (lambda (expr)
    (letrec ((paths-for-expr
              (lambda (expr ancestors defn-name args)
                (pmatch expr
                  [(quote ())
                   (list (cons 'nil ancestors))]
                  [#t
                   (list (cons 'bool ancestors))]
                  [#f
                   (list (cons 'bool ancestors))]                  
                  [,x (guard (symbol? x))
                   (list
                     (cond
                       [(eqv? x defn-name) (cons 'var-recur ancestors)]
                       [(memv x args) (cons 'var-arg ancestors)]
                       [else (cons 'var ancestors)]))]
                  [(define ,id ,e) (guard (symbol? id))
                   (paths-for-expr e (cons 'define ancestors) id args)]
                  [(lambda ,args ,body)
                   (paths-for-expr body (cons 'lambda ancestors) defn-name (if (symbol? args) (list args) args))]
                  [(if ,test ,conseq ,alt)
                   (append (paths-for-expr test (cons 'if-test ancestors) defn-name args)
                           (paths-for-expr conseq (cons 'if-conseq ancestors) defn-name args)
                           (paths-for-expr alt (cons 'if-alt ancestors) defn-name args))]
                  [(null? ,e)
                   (paths-for-expr e (cons 'null? ancestors) defn-name args)]
                  [(car ,e)
                   (paths-for-expr e (cons 'car ancestors) defn-name args)]
                  [(cdr ,e)
                   (paths-for-expr e (cons 'cdr ancestors) defn-name args)]
                  [(cons ,e1 ,e2)
                   (append (paths-for-expr e1 (cons 'cons-a ancestors) defn-name args)
                           (paths-for-expr e2 (cons 'cons-d ancestors) defn-name args))]
                  [(equal? ,e1 ,e2)
                   (append (paths-for-expr e1 (cons 'equal?-left ancestors) defn-name args)
                           (paths-for-expr e2 (cons 'equal?-right ancestors) defn-name args))]
                  [(,e . ,e*) ;; application
                   (append (paths-for-expr e (cons 'app-rator ancestors) defn-name args)
                           (apply append (map (lambda (e) (paths-for-expr e (cons 'app-rand* ancestors) defn-name args)) e*)))]
                  [else (error 'paths-for-expr (format "unknown expression type ~s" expr))]))))
      (paths-for-expr expr '(top-level) #f #f))))

(define lowres-bigrams-for-expr
  (lambda (expr)
    (letrec ((lowres-bigrams-for-expr
              (lambda (expr parent)
                (pmatch expr
                  [(quote ())
                   (list (list 'nil parent))]
                  [#t
                   (list (list 'bool parent))]
                  [#f
                   (list (list 'bool parent))]
                  [,x (guard (symbol? x))
                   (list (list 'var parent))]
                  [(define ,id ,e) (guard (symbol? id))
                   (cons (list 'define parent) (lowres-bigrams-for-expr e 'define))]
                  [(lambda ,args ,body)
                   (cons (list 'lambda parent) (lowres-bigrams-for-expr body 'lambda))]
                  [(if ,test ,conseq ,alt)
                   (cons (list 'if parent)
                         (append (lowres-bigrams-for-expr test 'if)
                                 (lowres-bigrams-for-expr conseq 'if)
                                 (lowres-bigrams-for-expr alt 'if)))]
                  [(null? ,e)
                   (cons (list 'null? parent)
                         (lowres-bigrams-for-expr e 'null?))]
                  [(car ,e)
                   (cons (list 'car parent)
                         (lowres-bigrams-for-expr e 'car))]
                  [(cdr ,e)
                   (cons (list 'cdr parent)
                         (lowres-bigrams-for-expr e 'cdr))]
                  [(cons ,e1 ,e2)
                   (cons (list 'cons parent)
                         (append (lowres-bigrams-for-expr e1 'cons)
                                 (lowres-bigrams-for-expr e2 'cons)))]
                  [(equal? ,e1 ,e2)
                   (cons (list 'equal? parent)
                         (append (lowres-bigrams-for-expr e1 'equal?)
                                 (lowres-bigrams-for-expr e2 'equal?)))]
                  [(,e . ,e*) ;; application
                   (cons (list 'app parent)
                         (append (lowres-bigrams-for-expr e 'app)
                                 (apply append (map (lambda (e) (lowres-bigrams-for-expr e 'app)) e*))))]
                  [else (error 'lowres-bigrams-for-expr (format "unknown expression type ~s" expr))]))))
      (lowres-bigrams-for-expr expr 'top-level))))
