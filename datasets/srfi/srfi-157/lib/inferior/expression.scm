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


(define (tagged-list? obj tag)
  (and (pair? obj)
       (eq? tag (car obj))))

;; Variables

(define (variable? obj)
  (identifier? obj))

(define (variable-identifier variable)
  variable)

;; Literals

(define (literal? expression)
  (tagged-list? expression 'quote))

(define (literal-datum literal)
  (cadr literal))

;; Lambdas

(define (lambda? expression)
  (tagged-list? expression 'lambda))

(define (lambda-formals expression)
  (cadr expression))

(define (lambda-body expression)
  (cddr expression))

(define (lambda-expression expression)
  (car (lambda-body expression)))

(define (make-let variables inits expression)
  `((lambda ,variables ,expression) ,@inits))

;; Conditionals

(define (conditional? expression)
  (tagged-list? expression 'if))

(define (conditional-test conditional)
  (cadr conditional))

(define (conditional-consequent conditional)
  (caddr conditional))

(define (conditional-alternate? conditional)
  (not (null? (cdddr conditional))))

(define (conditional-alternate conditional)
  (cadddr conditional))

;; Assignments

(define (assignment? expression)
  (tagged-list? expression 'set!))

(define (assignment-variable assignment)
  (cadr assignment))

(define (assignment-expression assignment)
  (caddr assignment))

;; Begin
(define (begin? expression)
  (tagged-list? expression 'begin))

(define (begin-sequence expression)
  (cdr expression))

;; Applications

(define (application? expression)
  (pair? expression))

(define (application-operator expression)
  (car expression))

(define (application-operands expression)
  (cdr expression))

;; Named let
(define (named-let? expression)
  (and (tagged-list? expression 'let)
       (variable? (cadr expression))))

(define (named-let-tag expression)
  (cadr expression))

(define (named-let-bindings expression)
  (caddr expression))

(define (named-let-body expression)
  (cdddr expression))

;; Let
(define (let? expression)
  (and (tagged-list? expression 'let)
       (not (variable? (cadr expression)))))

(define (let-bindings expression)
  (cadr expression))

(define (let-binding expression)
  (car (let-bindings expression)))

(define (let-body expression)
  (cddr expression))

;; Let*
(define (let*? expression)
  (tagged-list? expression 'let*))

(define (let*-bindings expression)
  (cadr expression))

(define (let*-body expression)
  (cddr expression))

;; Letrec

(define (letrec? expression)
  (tagged-list? expression 'letrec))

(define (letrec-bindings expression)
  (cadr expression))

(define (letrec-body expression)
  (cddr expression))

(define (letrec-variables expression)
  (map binding-variable (letrec-bindings expression)))

(define (letrec-inits expression)
  (map binding-init (letrec-bindings expression)))

(define (letrec-expression expression)
  (caddr expression))

;; Letrec*

(define (letrec*? expression)
  (tagged-list? expression 'letrec*))

(define (letrec*-bindings expression)
  (cadr expression))

(define (letrec*-expression expression)
  (caddr expression))

(define (letrec*-variables expression)
  (map binding-variable (letrec*-bindings expression)))

(define (letrec*-inits expression)
  (map binding-init (letrec*-bindings expression)))

;; Cond
(define (cond? expression)
  (tagged-list? expression 'cond))

(define (cond-clauses expression)
  (cdr expression))

;; Case
(define (case? expression)
  (tagged-list? expression 'case))

(define (case-key expression)
  (cadr expression))

(define (case-clauses expression)
  (cddr expression))

;; And
(define (and? expression)
  (tagged-list? expression 'and))

(define (and-tests expression)
  (cdr expression))

;; Or
(define (or? expression)
  (tagged-list? expression 'or))

(define (or-tests expression)
  (cdr expression))

;; Do
(define (do? expression)
  (tagged-list? expression 'do))

(define (do-bindings expression)
  (cadr expression))

(define (do-clause expression)
  (caddr expression))

(define (do-body expression)
  (cdddr expression))

;; Quasiquotation
(define (quasiquote? expression)
  (tagged-list? expression 'quasiquote))

(define (quasiquote-template expression)
  (cadr expression))

;; Formals

(define (formals->list formals)
  (cond
    ((null? formals)
     '())
    ((pair? formals)
     (cons (car formals)
           (formals->list (cdr formals))))
    (else
     (list formals))))

(define (map-formals proc formals)
  (let loop ((formals formals))
    (cond
     ((null? formals)
      '())
     ((pair? formals)
      (cons (proc (car formals))
	    (loop (cdr formals))))
     (else
      (proc formals)))))

;; Bindings

(define (binding-variable binding)
  (car binding))

(define (binding-init binding)
  (cadr binding))

;; Definitions

(define (definition? form)
  (tagged-list? form 'define))

(define (definition-variable definition)
  (let ((variable (cadr definition)))
    (if (identifier? variable)
	variable
	(car variable))))

(define (definition-expression definition)
  (let ((variable (cadr definition)))
    (if (identifier? variable)
	(caddr definition)
	`(lambda ,(cdr variable) . ,(cddr definition)))))

