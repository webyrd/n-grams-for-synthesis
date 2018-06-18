;; Copyright (C) Marc Nieper-Wißkirchen (2016).  All Rights Reserved. 

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


;; Secret syntactic literals
(define-syntax :call (syntax-rules ()))
(define-syntax :prepare (syntax-rules ()))

(define-syntax ck
  (syntax-rules (quote quasiquote em-cut em-cute)
    ((ck () 'v)
     v)
    ((ck (((op ...) ea ...) . s) 'v)
     (ck s "arg" (op ... 'v) ea ...))
    ((ck s "arg" (quasiquote va))
     (em-quasiquote-aux :call s va '()))
    ((ck s "arg" (op va ...))
     (op :call s va ...))
    ((ck s "arg" (op ...) 'v ea1 ...)
     (ck s "arg" (op ... 'v) ea1 ...))
    ((ck s "arg" (op ...) ea ea1 ...)
     (ck (((op ...) ea1 ...) . s) ea))
    ((ck s (quasiquote ea))
     (em-quasiquote-aux :prepare s ea '()))
    ((ck s ((em-cut a1 ...) a2 ...))
    (em-cut-aux s () (a1 ...) (a2 ...)))
    ((ck s ((em-cute a1 ...) a2 ...))
     (em-cut-aux s () (a1 ...) (a2 ...)))
    ((ck s ((op a ...) ea ...))
     (em-apply :prepare s (op a ...) ea ... '()))
    ((ck s (op ea ...))
     (op :prepare s ea ... ))
    ((ck s v)
     (ck s 'v))))

(define-syntax em-syntax-rules
  (syntax-rules (=>)
    ((em-syntax-rules (literal ...)
       (pattern (element => var) ... template)
       ...)
     (em-syntax-rules-aux1 quote free-identifier=? () (... ...)
			   (literal ...) ((pattern (element => var) ... template) ...)))
    ((em-syntax-rules ellipsis (literal ...)
       (pattern (element => var) ... template)
       ...)
     (em-syntax-rules-aux1 quote bound-identifier=? (ellipsis) ellipsis
			   (literal ...) ((pattern (element => var) ... template) ...)))
    ((em-syntax-rules . _)
     (syntax-error "invalid em-syntax-rules syntax"))))

(define-syntax em-syntax-rules-aux1
  (syntax-rules (=>)
    ((em-syntax-rules-aux1 q c e* e l* ((p t) ...))
     (em-syntax-rules-aux2 q c e* e l* ((p t) ...)))
    
    ((em-syntax-rules-aux1 q c e* e l* r*)
     (em-syntax-rules-aux1 q a c e* e l* r* () ()))

    ((em-syntax-rules-aux1 q a c e* e l* ((p t) . r*) (r1 ...) r2*)
     (em-syntax-rules-aux1 q a c e* e l* r* (r1 ... (p t)) r2*))

    ((em-syntax-rules-aux1 q a c e* e l* (((_ p ...) (i => v) w ... t) . r*) (r1 ...) (r2 ...))
     (em-syntax-rules-aux1 q a c e* e l* r*
			   (r1 ... ((_ p ...) (a i p ...)))
			   (r2 ... ((_ v p ...) w ... t))))

    ((em-syntax-rules-aux1 q a c e* e l* () r1* r2*)
     (begin (define-syntax a
	      (em-syntax-rules-aux1 q c e* e l* r2*))
	    (em-syntax-rules-aux2 q c e* e l* r1*)))))

(define-syntax em-syntax-rules-aux2
  (syntax-rules (quote)
    ((em-syntax-rules-aux2 :quote c e* e (l ...) ((p t) ...))
     (begin (define-syntax o
	      (em-syntax-rules-aux2 :quote o c e* e (l ...) ((p t) ...) ()))
	    o))

    ((em-syntax-rules-aux2 :quote o c (e? ...) e (l ...) () ((p q r t) ...))
     (syntax-rules e? ... (l ... :quote :prepare :call)
		   ((_ :prepare s . p)
		    (ck s "arg" (o) . q))
		   ...
		   ((_ :prepare s . args)
		    (syntax-error "bad arguments to macro call" . args))
		   ((_ :call s . r) (ck s t))
		   ...
		   ((_ . args) (ck () (o . args)))))
    
    ((em-syntax-rules-aux2 :quote o c e* e l* (((op . p) t) . pt*) qu*)
     (em-syntax-rules-aux2 :quote o c e* e l* pt* qu* (p t) () () ()))
    
    ((em-syntax-rules-aux2 :quote o c e* e l* pt* (qu ...) (() t) p q r)
     (em-syntax-rules-aux2 :quote o c e* e l* pt* (qu ... (p q r t))))
    
    ((em-syntax-rules-aux2 :quote o c e* e l* pt* qu* (('x maybe-ellipsis . p) t)
			   (y ...) (z ...) (w ...))
     (c e maybe-ellipsis
	(em-syntax-rules-aux2 :quote o c e* e l* pt* qu* (p t) (y ... %x e) (z ... %x e)
			      (w ... (:quote x) e))
	(em-syntax-rules-aux2 :quote o c e* e l* pt* qu* ((maybe-ellipsis . p) t) (y ... %x) (z ... %x)
			      (w ... (:quote x)))))
    
    ((em-syntax-rules-aux2 :quote o c e* e l* pt* qu* (('x . p) t) (y ...) (z ...) (w ...))
     (em-syntax-rules-aux2 :quote o c e* e l* pt* qu* (p t) (y ... %x) (z ... %x) (w ... (:quote x))))
    
    ((em-syntax-rules-aux2 :quote o c e* e l* pt* qu* ((x maybe-ellipsis . p) t)
			   (y ...) (z ...) (w ...))
     (c e maybe-ellipsis
	(em-syntax-rules-aux2 :quote o c e* e l* pt* qu* (p t) (y ... %x e) (z ... (:quote %x) e)
			      (w ... (:quote x) e))
	(em-syntax-rules-aux2 :quote o c e* e l* pt* qu* ((maybe-ellipsis . p) t)
			      (y ... %x) (z ... (:quote %x)) (w ... (:quote x)))))
    
    ((em-syntax-rules-aux2 :quote o c e* e l* pt* qu* ((x . p) t) (y ...) (z ...) (w ...))
     (em-syntax-rules-aux2 :quote o c e* e l* pt* qu* (p t) (y ... %x)
			   (z ... (:quote %x)) (w ... (:quote x))))))

(define-syntax em
  (syntax-rules (quote :prepare :call)
    ((em :prepare s expression)
     (ck s "arg" (em) 'expression))
    ((em :call s 'expression)
     (let ()
       (em-quasiquote (let () (define-values x ,expression) (apply values x)))))
    ((em expression)
     (ck () (em expression)))))

(define-syntax em-suspend
  (syntax-rules (quote :prepare :call)
    ((em-suspend :prepare s op arg ...)
     (ck s "arg" (em-suspend) op arg ...))
    ((em-suspend :call s 'op 'arg ...)
     (op s arg ...))))

(define-syntax em-resume
  (syntax-rules ()
    ((em-resume t v)
     (ck t v))))

(define-syntax em-cut
  (em-syntax-rules ()
    ((em-cut slot-or-datum ...)
     `(em-cut ,(em-cut-eval slot-or-datum) ...))))

(define-syntax em-cute
  (em-syntax-rules ()
    ((em-cute slot-or-datum ...)
     `(em-cut ,(em-cute-eval slot-or-datum) ...))))

(define-syntax em-cut-eval
  (em-syntax-rules ::: (<> ...)
    ((em-cut-eval <>) <>)
    ((em-cut-eval ...) ...)
    ((em-cut-eval x) 'x)))

(define-syntax em-cut-aux
  (syntax-rules ::: (<> ...)

    ((em-cut-aux s (datum :::) () ())
     (em-apply :prepare s datum ::: '()))

    ((em-cut-aux s (datum :::) (<> ...) (input :::))
     (em-cut-aux s (datum ::: input :::) () ()))

    ((em-cut-aux s (datum :::) (<> slot-or-datum :::) (input1 input2 :::))
     (em-cut-aux s (datum ::: input1) (slot-or-datum :::) (input2 :::)))

    ((em-cut-aux s (datum1 :::) (datum2 slot-or-datum :::) (input :::))
     (em-cut-aux s (datum1 ::: datum2) (slot-or-datum :::) (input :::)))))

(define-syntax em-cute-eval
  (em-syntax-rules ::: (<> ...)
    ((em-cute-eval '<>) <>)
    ((em-cute-eval '...) ...)
    ((em-cute-eval 'x) 'x)))

(define-syntax em-quasiquote
  (em-syntax-rules ()
    ((em-quasiquote form) (em-quasiquote-aux form '()))))

(define-syntax em-quasiquote-aux
  (em-syntax-rules (quasiquote unquote unquote-splicing)
    ((em-quasiquote-aux ,form '())
     form) 
    ((em-quasiquote-aux (,@form . rest) '())
     (em-append form (em-quasiquote-aux rest '())))
    ((em-quasiquote-aux `form 'depth)
     (em-list 'quasiquote (em-quasiquote-aux form '(#f . depth))))
    ((em-quasiquote-aux ,form '(#f . depth))
     (em-list 'unquote (em-quasiquote-aux form 'depth)))
    ((em-quasiquote-aux ,@form '(#f . depth))
     (em-list 'unquote-splicing (em-quasiquote-aux '(form . depth))))
    ((em-quasiquote-aux (car . cdr) 'depth)
     (em-cons (em-quasiquote-aux car 'depth) (em-quasiquote-aux cdr 'depth)))
    ((em-quasiquote-aux #(element ...) 'depth)
     (em-list->vector (em-quasiquote-aux (element ...) 'depth)))
    ((em-quasiquote-aux constant 'depth)
     'constant)))
