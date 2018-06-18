;; Copyright (C) 2003 Taylor Campbell and Scott G. Miller.  See the LICENCE
;; file for details.

;; This file requires Tiny-CLOS and utilities.scm.

(define (subclass? c1 c2)
  (or (eq? c2 <top>)
      (and (memq c2 (class-cpl c1)) #t)))

(define (make-generic-predicate class)
  (let ((g (make-generic)))
    (add-method g
      (make-method (list <object>)
        (lambda (call-next-method obj) #f)))
    (add-method g
      (make-method (list class)
        (lambda (call-next-method obj) #t)))
    g))

(define (make-predicate-method class)
  (make-method (list <class>)
    (lambda (call-next-method obj) #t)))

(define (make-generic/constraint . classes)
  (let ((g (make-generic)))
    (add-method g
      (make-method (list <top>)
        (lambda (call-next-method . args)
          (apply error "Can't apply to anything but with arguments of"
                 classes))))
    g))

(define-syntax define-constrained-generics
  (syntax-rules ()
    ((define-constrained-generics ?class ?generic)
     (define ?generic (make-generic/constraint ?class)))
    ((define-constrained-generics ?class ?generic ?more ...)
     (begin
       (define-constrained-generics ?class ?generic)
       (define-constrained-generics ?class ?more ...)))))

(define-syntax define-supertype-handled
  (syntax-rules ()
    ((define-supertype-handled ?pred?
       ((?f ?arg1 . ?args) ?super)
       ...)
     (begin
       (define (?f ?arg1 . ?args)
         (check-arg ?pred? ?arg1 '?f)
         (generate-call ?super (?arg1 . ?args) ()))
       ...))))

(define-syntax generate-call
  (syntax-rules ()
    ((generate-call ?super () ?args)
     (?super . ?args))
    ((generate-call ?super (?arg . ?more) (?done ...))
     (generate-call ?super ?more (?done ... ?arg)))
    ((generate-call ?super ?last (?arg ...))
     (apply ?super ?arg ... ?last))))

(define (class-predicate class)
  (lambda (x) (subclass? (class-of x) class)))

(define (function->method specs f)
  (make-method specs
    (lambda (call-next-method . args) (apply f args))))

;; Convenience macro for making methods.
(define-syntax method
  (syntax-rules ()
    ((_ ?next-method (?param . ?more-lambda-list) ?e1 ?e2 ...)
     (method-aux () () #f (?param . ?more-lambda-list)
                 ?next-method
                 ?e1 ?e2 ...))))

(define-syntax method-aux
  (syntax-rules ()
    ((_ (?spec ...) (?param ...) #f () ?next-method ?e1 ?e2 ...)
     (make-method (list ?spec ...)
       (lambda (?next-method ?param ...)
         ?e1 ?e2 ...)))
    ((_ (?spec ...) (?param ...) ?rest () ?next-method ?e1 ?e2 ...)
     (make-method (list ?spec ...)
       (lambda (?next-method ?param ... . ?rest)
         ?e1 ?e2 ...)))

    ((_ (?spec ...) (?param ...) #f ((?p ?class) . ?more)
        ?next-method
        ?e1 ?e2 ...)
     (method-aux (?spec ... ?class) (?param ... ?p) #f ?more
                 ?next-method
                 ?e1 ?e2 ...))
    ((_ (?spec ...) (?param ...) #f (?p . ?more)
        ?next-method
        ?e1 ?e2 ...)
     (method-aux (?spec ... <top>) (?param ... ?p) #f ?more
                 ?next-method
                 ?e1 ?e2 ...))

    ((_ (?spec ...) (?param ...) #f ?rest ?next-method ?e1 ?e2 ...)
     (method-aux (?spec ...) (?param ...) ?rest ()
                 ?next-method
                 ?e1 ?e2 ...))))

(define (unzip3 lists)
  (if (null? lists)
      (values '() '() '())
      (receive (z1 z2 z3) (unzip3 (cdr lists))
        (values (cons (caar   lists) z1)
                (cons (cadar  lists) z2)
                (cons (caddar lists) z3)))))

(define (add-methods . generic+function+specs-lists)
  (receive (generics functions specializers-lists)
      (unzip3 generic+function+specs-lists)
    (for-each (lambda (generic function specializers)
;                 (breakpoint
;                  "Adding method ~S to generic ~S with specs ~S"
;                  function generic specializers)
                (add-method generic
                  (function->method specializers function)))
              generics functions specializers-lists)))
