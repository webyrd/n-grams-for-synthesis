(load "infer-core-ml.scm")

(set! app-!-o
      (lambda (expr gamma type)
        (fresh (rator rands t*)
          (== `(,rator . ,rands) expr)
          ;; Multi-argument
          (!-o rator gamma `(-> ,t* ,type) 'app-rator)
          (!-o-application rands gamma t*)
          )))


(define-syntax let/vars-ml-infer
  (syntax-rules ()
    ((_ _ () body) body)
    ((_ _ () body ...) (begin body ...))
    ((_ st (qvar ...) body ...)
     (let ((scope (subst-scope (state-S st))))
       (let ((qvar (var scope)) ...)
         body ...)))))

(define (list-split-ground-ml-infer st xs)
  (let loop ((rprefix '()) (xs xs))
    (let ((tm (walk xs (state-S st))))
      (if (pair? tm)
        (loop (cons (walk (car tm) (state-S st)) rprefix) (cdr tm))
        (values rprefix xs)))))

(define (!-o-application rands gamma types)
  (define succeed unit)
  (lambdag@ (st)
    (let-values (((rrands rands-suffix) (list-split-ground-ml-infer st rands)))
      (let-values
        (((ggoals vgoals types-suffix)
          (let loop ((rands (reverse rrands))
                     (ggoals succeed)
                     (vgoals succeed)
                     (types types))
            (if (null? rands) (values ggoals vgoals types)
              (let ((rand (car rands)))
                (let/vars-ml-infer st (types-rest)
                  (let ((goal (fresh (type)
                                (== `(,type . ,types-rest) types)
                                (!-o rand gamma type 'app-rand*))))
                    (if (var? rand)
                        (loop (cdr rands) ggoals (fresh () vgoals goal) types-rest)
                        (loop (cdr rands) (fresh () ggoals goal) vgoals types-rest)))))))))
        ((fresh ()
           ggoals    ; try ground arguments first
           vgoals    ; then fill in unbound arguments
           ; any unbound final segment of arguments
           (!-o-randso rands-suffix gamma types-suffix)) st)))))
