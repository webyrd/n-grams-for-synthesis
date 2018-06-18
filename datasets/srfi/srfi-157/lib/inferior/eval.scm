;; Copyright (C) Marc Nieper-Wißkirchen (2017).  All Rights Reserved. 

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.


(define (eval expression environment)
  (execute (compile-top-level expression) environment))

(define (execute expression environment)
  (define (lookup-variable variable)
    (let ((cell (environment-lookup environment variable)))
      (or cell
	  (begin
	    (let ((cell (box unspecified)))
	      (environment-add-binding! environment variable cell)
	      cell)))))
  (define host-eval-environment
    (host-environment '(scheme base)
		      '(inferior box)
		      '(prefix (inferior primitive) %)))
  ((host-eval `(lambda (lookup-variable)
		 ,expression)
	      host-eval-environment)
  lookup-variable))

(define top-level-environment
  (make-parameter #f))

(define (compile-top-level expression)
  (let ((expressions (flatten-expression expression)))   
    (parameterize ((top-level-environment (make-environment)))
      (let ((expressions (compile* expressions (top-level-environment) #f)))
	`(let ,(environment-map-bindings
		(lambda (identifier cell)
		  `(,(cadr cell) (lookup-variable ',identifier)))
		(top-level-environment))
	   (let ((flag #f)
		 (marks '()))
	     ,@expressions))))))

(define (flatten-expression expression)
  (flatten-expression* (list expression) '()))

(define (flatten-expression* expressions rest)
  (if (null? expressions)
      rest
      (let ((expression (car expressions))
	    (rest (flatten-expression* (cdr expressions) rest)))
	(if (begin? expression)
	    (flatten-expression* (begin-sequence expression) rest)
	    (cons expression rest)))))

(define (compile* expressions environment flag)
  (map (lambda (expression)
	 (let ((expression
		(if (definition? expression)
		    `(set! ,(definition-variable expression)
		       ,(definition-expression expression))
		    expression))) 
	   (compile expression environment #f)))
       expressions))

(define (compile expression environment flag)
  (cond
   ((self-evaluating? expression)
    (compile-self-evaluating expression))
   ((derived? expression)
    (compile-derived expression environment flag))
   ((literal? expression)
    (compile-literal expression))
   ((variable? expression)
    (compile-variable expression environment))
   ((conditional? expression)
    (compile-conditional expression environment flag))
   ((lambda? expression)
    (compile-lambda expression environment))
   ((with-continuation-mark? expression environment)
    (compile-with-continuation-mark expression environment flag))
   ((assignment? expression)
    (compile-assignment expression environment flag))
   ((application? expression)
    (compile-application expression environment flag))
   (else
    (error "unknown expression type" expression))))

(define (self-evaluating? expression)
  (or (unspecified? expression)
      (string? expression)
      (number? expression)
      (vector? expression)
      (bytevector? expression)
      (char? expression)
      (boolean? expression)))

(define (with-continuation-mark? expression environment)
  (and (tagged-list? expression 'with-continuation-mark)
       (not (environment-lookup environment 'with-continuation-mark))))

(define (derived? expression)
  (or (cond? expression)
      (case? expression)
      (and? expression)
      (or? expression)
      (begin? expression)
      (named-let? expression)
      (let? expression)
      (let*? expression)
      (letrec? expression)
      (named-let? expression)
      (do? expression)
      (quasiquote? expression)))

(define (compile-self-evaluating expression)
  `',expression)

(define (compile-derived expression environment flag)
  (compile (transform expression) environment flag))

(define (compile-literal expression)
  `',(literal-datum expression))

(define (compile-variable expression environment)
  (let* ((identifier (variable-identifier expression))
	 (location (environment-lookup environment identifier)))
    (or location
	(let ((cell (make-cell identifier)))
	  (environment-add-binding! (top-level-environment) identifier cell)
	  cell))))

(define (compile-with-continuation-mark expression environment flag)
  (let ((key (cadr expression))
	(value (caddr expression))
	(expression (cadddr expression)))
    `(let* ((key ,(non-tail (compile key environment #f) flag))
	    (value ,(non-tail (compile value environment #f) flag)))
       (let ((flag #t)
	     (marks (%with-continuation-mark flag marks key value)))
	 ,(compile expression environment #t)))))

(define (compile-conditional expression environment flag)
  `(if ,(non-tail (compile (conditional-test expression) environment #f)
		  flag)
       ,(compile (conditional-consequent expression) environment flag)
       ,(if (conditional-alternate? expression)
	    (compile (conditional-alternate expression) environment flag)
	    unspecified)))

(define (compile-lambda expression environment)
  (let* ((formals (lambda-formals expression))
	 (new-formals (map-formals gensym formals)))
    `(lambda (flag marks . ,new-formals)
       ,(compile-body (lambda-body expression)
		      (environment-extend environment
					  (formals->list formals)
					  (formals->list new-formals))))))

(define (compile-assignment expression environment flag)
  (let ((location (compile-variable (assignment-variable expression) environment))
	(expression (non-tail (compile (assignment-expression expression) environment #f)
			      flag)))
    (if (symbol? location)
	`(%set! ,location ,expression)
	`(%set-box! ,(cadr location) ,expression))))

(define (compile-application expression environment flag)
  `(,(non-tail (compile (application-operator expression) environment #f)
	       flag)
    flag
    marks
    . ,(map (lambda (operand)
	      (non-tail (compile operand environment #f)
			flag))
	    (application-operands expression))))

(define (make-cell identifier)
  `(unbox ,(gensym (symbol->string identifier))))

(define (compile-body body environment)
  (if (null? (cdr body))
      (compile (car body) environment #t)
      (let ((body (flatten-body body)))
	(let ((bindings (body-bindings body))
	      (expressions (body-expressions body)))
	  (compile
	   (if (null? bindings)
	       `(begin ,@expressions)
	       `(letrec ,bindings ,@expressions))
	   environment
	   #t)))))

(define (non-tail expression flag)
  (if flag
      `(let ((flag #f)) ,expression)
      expression))

(define (flatten-body body)
  (flatten-expression* body '()))

(define (body-bindings body)
  (let loop ((body body))
    (if (null? body)
	'()
	(let ((expression (car body)))
	  (cond
	   ((definition? expression)
	    (cons `(,(definition-variable expression)
		    ,(definition-expression expression))
		  (loop (cdr body))))
	   (else
	    '()))))))

(define (body-expressions body)
  (let loop ((body body))
    (let ((expression (car body)))
      (cond
       ((definition? expression)
	(loop (cdr body)))
       (else
	body)))))

;; Expression transformers

(define (transform expression)
  (cond
   ((cond? expression)
    (transform-cond expression))
   ((case? expression)
    (transform-case expression))
   ((and? expression)
    (transform-and expression))
   ((or? expression)
    (transform-or expression))
   ((begin? expression)
    (transform-begin expression))
   ((named-let? expression)
    (transform-named-let expression))
   ((let? expression)
    (transform-let expression))
   ((let*? expression)
    (transform-let* expression))
   ((letrec? expression)
    (transform-letrec expression))
   ((do? expression)
    (transform-do expression))
   ((quasiquote? expression)
    (transform-quasiquote expression))))

(define (transform-cond expression)
  (let loop ((clauses (cond-clauses expression))) 
    (if (null? clauses)
	unspecified
	(let ((clause (car clauses)))
	  (cond
	   ((eq? (car clause) 'else)
	    `(begin ,@(cdr clause)))
	   ((null? (cdr clause))
	    `(or ,(car clause)
		 (cond ,@(cdr clauses))))
	   ((eq? (cadr clause) '=>)
	    (let ((result (make-synthetic-identifier 'result)))
	      `(let ((,result ,(car clause)))
		 (if ,result
		     (,(caddr clause) ,result)
		     ,(loop (cdr clauses))))))
	   (else
	    `(if ,(car clause)
		 (begin ,@(cdr clause))
		 ,(loop (cdr clauses)))))))))

(define (transform-case expression)
  (let ((_memv (close-syntax 'memv primitive-environment)))
    (let ((key (make-synthetic-identifier 'key)))
      `(let ((,key ,(case-key expression)))
	 ,(let loop ((clauses (case-clauses expression)))
	    (if (null? clauses)
		unspecified
		(let ((clause (car clauses)))
		  (cond
		   ((eq? (car clause) 'else)
		    `(begin ,@(cdr clause)))
		   (else
		    `(if (_memv ,key (quote ,(car clause)))
			 (begin ,@(cdr clause))
			 ,(loop (cdr clauses))))))))))))

(define (transform-and expression)
  (let loop ((tests (and-tests expression)))
    (cond
     ((null? tests)
      #t)
     ((null? (cdr tests))
      (car tests))
     ((let ((result (make-synthetic-identifier 'result)))
	`(let ((,result ,(car tests)))
	   (if ,result ,(loop (cdr tests)) #f)))))))

(define (transform-or expression)
  (let loop ((tests (or-tests expression)))
    (cond
     ((null? tests)
      '#f)
     ((null? (cdr tests))
      (car tests))
     ((let ((result (make-synthetic-identifier 'result)))
	`(let ((,result ,(car tests)))
	   (if ,result
	       ,result
	       ,(loop (cdr tests)))))))))


(define (transform-begin expression)
  (let loop ((expressions (begin-sequence expression)))
    (if (null? (cdr expressions))
        (car expressions)
        `(let ((,(make-synthetic-identifier 'dummy)
                ,(car expressions)))
           ,(loop (cdr expressions))))))

(define (transform-named-let expression)
  (let ((tag (named-let-tag expression))
        (bindings (named-let-bindings expression)))
    `((letrec ((,tag (lambda ,(map binding-variable bindings)
                       ,@(named-let-body expression))))
        ,tag)
      ,@(map binding-init bindings))))

(define (transform-let expression)
  (let ((bindings (let-bindings expression)))
    `((lambda ,(map binding-variable bindings)
	,@(let-body expression))
      ,@(map binding-init bindings))))

(define (transform-let* expression)
  (let loop ((bindings (let*-bindings expression)))
    (cond
      ((null? bindings)
       `(let () ,@(let*-body expression)))
      ((null? (cdr bindings))
       `(let ,bindings ,@(let*-body expression)))
      (else
       (let ((binding (car bindings)))
         `(let (,binding) ,(loop (cdr bindings))))))))

(define (transform-letrec expression)
  (let*
      ((variables (letrec-variables expression))
       (inits (letrec-inits expression))
       (temps (map make-synthetic-identifier variables))
       (body (letrec-body expression)))
    `(let ,(map (lambda (variable)
		  `(,variable #f))
		variables)
       (let ,(map (lambda (temp init)
		    `(,temp ,init))
		  temps inits)
	 ,@(map (lambda (variable temp)
		  `(set! ,variable ,temp))
		variables temps)
	 ,@body))))

(define (transform-do expression)
  (let ((bindings (do-bindings expression))
        (clause (do-clause expression)))
    (let ((loop (make-synthetic-identifier 'loop)))
      `(let ,loop ,(map (lambda (binding)
                          `(,(car binding) ,(cadr binding)))
                        bindings)
         (if ,(car clause)
             (begin ,@(cdr clause))
             (begin ,@(do-body expression)
                    (,loop ,@(map (lambda (binding)
                                    (if (null? (cddr binding))
                                        (cadr binding)
                                        (caddr binding)))
                                  bindings))))))))

(define (transform-quasiquote expression)
  (let ((_cons (close-syntax 'cons primitive-environment))
	(_list (close-syntax 'list primitive-environment))
	(_append (close-syntax 'append primitive-environment))
	(_apply (close-syntax 'apply primitive-environment))
	(_vector (close-syntax 'vector primitive-environment)))
    (let loop ((template (quasiquote-template expression))
	       (depth 0))
      (cond
       ((and (zero? depth) (tagged-list? template 'unquote))
	(cadr template))
       ((and (zero? depth) (pair? template) (tagged-list? (car template) 'unquote-splicing))
	`(,_append ,(cadar template) ,(loop (cdr template) 0)))
       ((tagged-list? template 'quasiquote)
	`(,_list `quasiquote ,(loop (cadr template) (+ depth 1))))
       ((tagged-list? template 'unquote)
	`(,_list `unquote ,(loop (cadr template) (- depth 1))))
       ((tagged-list? template 'unquote-splicing)
	`(,_list `unquote-splicing ,(loop (caddr template) (- depth 1))))
       ((pair? template)
	`(,_cons ,(loop (car template) depth)
		 ,(loop (cdr template) depth)))
       ((vector? template)
	`(,_apply ,_vector ,(loop (vector->list template) depth)))
       (else
	`(quote ,template))))))

;;; Primitive environment

(define primitives
  '(apply append cons memv list vector))

(define primitive-environment
  (let ((environment (make-environment)))
    (for-each
     (lambda (primitive)
       (environment-add-binding! environment
				 primitive
				 (string->symbol
				  (string-append "%"
						 (symbol->string primitive)))))
     primitives)
    environment))
