(load "pmatch.scm")
(load "corpus.scm")

(define bigrams-for-expr
  (lambda (expr)
    (letrec ((bigrams-for-expr
              (lambda (expr parent defn-name args)
                (pmatch expr
                  [(eq? ,e1 ,e2)
                   (error 'bigrams-for-expr (format "unconverted eq?"))]
                  [(eqv? ,e1 ,e2)
                   (error 'bigrams-for-expr (format "unconverted eqv?"))]
                  [(cond . ,c*)
                   (error 'bigrams-for-expr (format "unconverted cond"))]
                  [(match ,e . ,c*)
                   (cons (list 'match parent)
                         (append (bigrams-for-expr e 'match-against defn-name args)
                                 (apply append (map (lambda (c) (bigrams-for-expr (cadr c) 'match-body defn-name args)) c*))))]
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
                       [(eqv? x defn-name) (list 'var parent)]
                       [(memv x args) (list 'var parent)]
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
                  [(let ,binding* ,e)
                   (cons (list 'let parent)
                         (append (apply append (map (lambda (binding) (bigrams-for-expr (cadr binding) 'let-rhs defn-name args)) binding*))
                                 (bigrams-for-expr e 'let-body defn-name args)))]
                  [(letrec ((,id (lambda ,x ,body))) ,e)
                   (cons (list 'letrec parent)
                         (append (bigrams-for-expr `(lambda ,x ,body) 'letrec-rhs defn-name args)
                                 (bigrams-for-expr e 'letrec-body id args)))]
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
                   (sort-counts-al-by-symbols count-al)]
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

(define sort-counts-al-by-symbols
  (lambda (counts-al)
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
     counts-al)))

(define sort-counts-al-by-counts
  (lambda (counts-al)
    (sort
     (lambda (e1 e2) (> (cdr e1) (cdr e2)))
     counts-al)))

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

(define bigrams (map reverse (apply append (map bigrams-for-expr exprs))))
(define bigram-counts (count-bigrams bigrams))
(define bigrams-sorted-by-counts (sort-counts-al-by-counts bigram-counts))

;; this is the important one
(define bigrams-sorted-by-type/counts (sort-counts-al-by-type/counts bigram-counts))


(define sum-entry-types
  (lambda (alist)
    (let loop ((alist alist)
               (table '()))
      (pmatch alist
        [() (list-sort (lambda (e1 e2) (> (cdr e1) (cdr e2))) table)]
        [(((,parent ,entry) . ,n) . ,rest)
         (cond
           [(assoc entry table) =>
            (lambda (pr)
              (let ((m (cdr pr)))
                (loop rest (cons (cons entry (+ n m)) (remove pr table)))))]
           [else (loop rest (cons (cons entry n) table))])]))))

(define global-frequency-ordering
  (sum-entry-types bigrams-sorted-by-type/counts))

#!eof

;; bigrams-sorted-by-type/counts
(((and app) . 24)
 ((and null?) . 8)
 ((and equal?) . 2)
 ((app-rand* var) . 121)
 ((app-rand* cdr) . 66)
 ((app-rand* car) . 40)
 ((app-rand* app) . 25)
 ((app-rand* lambda) . 4)
 ((app-rand* cons) . 3)
 ((app-rand* list) . 3)
 ((app-rand* nil) . 3)
 ((app-rand* quoted-non-empty-list) . 2)
 ((app-rand* if) . 1)
 ((app-rand* null?) . 1)
 ((app-rand* quoted-symbol) . 1)
 ((app-rator var) . 155)
 ((app-rator app) . 1)
 ((app-rator quoted-symbol) . 1)
 ((car var) . 85)
 ((car cdr) . 6)
 ((car car) . 2)
 ((cdr var) . 76)
 ((cdr cdr) . 3)
 ((cdr car) . 1)
 ((cons-a car) . 19)
 ((cons-a var) . 10)
 ((cons-a app) . 6)
 ((cons-a cons) . 1)
 ((cons-d app) . 28)
 ((cons-d cons) . 4)
 ((cons-d var) . 2)
 ((cons-d cdr) . 1)
 ((cons-d list) . 1)
 ((define lambda) . 44)
 ((equal? car) . 23)
 ((equal? var) . 20)
 ((equal? quoted-symbol) . 3)
 ((if-alt if) . 41)
 ((if-alt app) . 23)
 ((if-alt cons) . 17)
 ((if-alt and) . 3)
 ((if-alt bool) . 2)
 ((if-alt equal?) . 1)
 ((if-alt or) . 1)
 ((if-conseq bool) . 26)
 ((if-conseq nil) . 15)
 ((if-conseq app) . 10)
 ((if-conseq cons) . 10)
 ((if-conseq var) . 8)
 ((if-conseq num) . 6)
 ((if-conseq and) . 5)
 ((if-conseq car) . 3)
 ((if-conseq cdr) . 3)
 ((if-conseq if) . 1)
 ((if-conseq or) . 1)
 ((if-test null?) . 34)
 ((if-test app) . 20)
 ((if-test equal?) . 19)
 ((if-test and) . 8)
 ((if-test or) . 4)
 ((if-test pair?) . 2)
 ((if-test not) . 1)
 ((lambda-multi if) . 45)
 ((lambda-multi app) . 5)
 ((lambda-multi match) . 3)
 ((letrec-body app) . 2)
 ((letrec-body letrec) . 2)
 ((letrec-rhs lambda) . 4)
 ((list var) . 9)
 ((list car) . 2)
 ((list list) . 1)
 ((list quoted-symbol) . 1)
 ((match-against var) . 3)
 ((match-body app) . 6)
 ((match-body var) . 2)
 ((match-body and) . 1)
 ((match-body cons) . 1)
 ((match-body lambda) . 1)
 ((match-body nil) . 1)
 ((not pair?) . 1)
 ((null? var) . 46)
 ((null? cdr) . 1)
 ((or app) . 7)
 ((or null?) . 4)
 ((or equal?) . 1)
 ((pair? car) . 3)
 ((top-level define) . 44)
 ((top-level letrec) . 2))
