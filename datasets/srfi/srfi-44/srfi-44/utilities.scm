;; Copyright (C) 2003 Taylor Campbell and Scott G. Miller.  See the LICENCE
;; file for details.

;; This file requires SRFI 23 (error).

(define-syntax let*-optionals
  (syntax-rules ()
    ((_ ?proc (?rest ...) ((?var ?default) ...) ?e1 ?e2 ...)
     (let ((rest (?rest ...)))
       (let*-optionals ?proc rest ((?var ?default) ...) ?e1 ?e2 ...)))
    ((_ ?proc ?rest () ?e1 ?e2 ...)
     (if (null? ?rest)
         (begin ?e1 ?e2 ...)
         (error "Too many arguments" ?proc)))
    ((_ ?proc ?rest ((?var1 ?default1) (?var2 ?default2) ...) ?e1 ?e2 ...)
     (list-case ?rest
       (lambda ()
         (let* ((?var1 ?default1) (?var2 ?default2) ...) ?e1 ?e2 ...))
       (lambda (?var1 new-rest)
         (let*-optionals ?proc new-rest ((?var2 ?default2) ...)
           ?e1 ?e2 ...))))))

(define (check-arg pred? arg caller)
  (if (not (pred? arg))
      (error "Bad argument"
             pred?
             arg
             caller)))

(define (always . vals) (lambda _ (apply values vals)))

(define-syntax receive
  (syntax-rules ()
    ((_ ?formals ?producer ?body1 ?body2 ...)
     (call-with-values
       (lambda () ?producer)
       (lambda ?formals ?body1 ?body2 ...)))))

(define-syntax rec
  (syntax-rules ()
    ((rec (?f . ?args) ?body1 ?body2 ...)
     (rec ?f (lambda ?args ?body1 ?body2 ...)))
    ((rec ?x ?y)
     (letrec ((?x ?y)) ?x))))

(define (drop l k)
  (if (zero? k)
      l
      (drop (cdr l) (- k 1))))

(define (take l k)
  (if (zero? k)
      '()
      (cons (car l) (take (cdr l) (- k 1)))))

(define (fold-right kons knil l)
  (if (null? l)
      knil
      (kons (car l) (fold-right kons knil (cdr l)))))

(define (list-case l k-nil k-pair)
  (cond ((null? l)
         (k-nil))
        ((pair? l)
         ;; Use CAR+CDR from SRFI 1, perhaps?
         ;(call-with-values (lambda () (car+cdr l)) k-pair)
         (k-pair (car l) (cdr l)))
        (else
         (error "Not a list" l))))

(define (maybe-cars+cdrs l sk fk)
  (list-case l
    (lambda () (sk '() '()))
    (lambda (list1 list2+)
      (list-case list1
        fk
        (lambda (car1 cdr1)
          (maybe-cars+cdrs list2+
            (lambda (car2+ cdr2+)
              (sk (cons car1 car2+)
                  (cons cdr1 cdr2+)))
            fk))))))
