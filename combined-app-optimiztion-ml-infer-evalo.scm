(load "combined-core-ml-infer-evalo.scm")

(set! app-!-/evalo
      (lambda (expr gamma env type val)
        (fresh (rator x* rands body gamma^ gamma^^ env^ env^^ a* t t*)
          (== `(,rator . ,rands) expr)
          ;; Multi-argument
          (!-/eval-expo rator gamma env `(-> ,t ,type) `(closure (lambda ,x* ,body) ,gamma^ ,env^) 'app-rator)
          (ext-gamma*/env*o x* t* a* gamma^ gamma^^ env^ env^^)
          (eval-application rands gamma env t* a* (!-/eval-expo body gamma^^ env^^ type val 'lambda)))))

(define-syntax let/vars
  (syntax-rules ()
    ((_ _ () body) body)
    ((_ _ () body ...) (begin body ...))
    ((_ st (qvar ...) body ...)
     (let ((scope (subst-scope (state-S st))))
       (let ((qvar (var scope)) ...)
         body ...)))))

(define (list-split-ground st xs)
  (let loop ((rprefix '()) (xs xs))
    (let ((tm (walk xs (state-S st))))
      (if (pair? tm)
        (loop (cons (walk (car tm) (state-S st)) rprefix) (cdr tm))
        (values rprefix xs)))))

(define (eval-application rands agamma aenv t* a* body-goal)
  (define succeed unit)
  (lambdag@ (st)
    (let-values (((rrands rands-suffix) (list-split-ground st rands)))
      (let-values
        (((ggoals vgoals types-suffix args-suffix)
          (let loop ((rands (reverse rrands))
                     (ggoals succeed)
                     (vgoals succeed)
                     (types t*)
                     (args a*))
            (if (null? rands) (values ggoals vgoals types args)
              (let ((rand (car rands)))
                (let/vars st (types-rest args-rest)
                  (let ((goal (fresh (type arg)
                                (== `(,type . ,types-rest) types)
                                (== `(,arg . ,args-rest) args)
                                (!-/eval-expo rand agamma aenv type arg 'app-rand*))))
                    (if (var? rand)
                      (loop (cdr rands) ggoals (fresh () vgoals goal) types-rest args-rest)
                      (loop (cdr rands) (fresh () ggoals goal) vgoals types-rest args-rest)))))))))
        ((fresh ()
           ggoals    ; try ground arguments first
           body-goal ; then the body
           vgoals    ; then fill in unbound arguments
           ; any unbound final segment of arguments
           (!-/eval-randso rands-suffix agamma aenv types-suffix args-suffix)) st)))))

