;		Implementing SRFI-0 as a Read-Time Application
;
; See
;	http://pobox.com/~oleg/ftp/Scheme/read-time-apply.txt
; for more details on reader-constructors, read-time applications, and
; their implementation. Refer to SRFI-0, "Feature-based conditional expansion
; construct," for the definition and discussion of 'cond-expand'
;	http://srfi.schemers.org/srfi-0/srfi-0.html
;
; We assume that "feature identifiers" in effect are contained
; in a list ALL-FEATURES. This list should either be pre-defined in a
; Scheme system, or otherwise defined prior to reading the code in
; question (for example, using various switches or profiles/.rc files, or
; similar initialization code).
;
; The present implementation is directly based on the grammar
; given in SRFI-0, and looks rather similar to the 'cond-expand'
; syntax of the reference implementation.
;
; Note, contrary to what SRFI-0 says, the changes to the reader required to
; implement cond-expand are minor and straightforward. Also note that
; the present implementation relaxes a SRFI-0 constraint: cond-expand
; is no longer restricted to be a top-level expression in a program.
;
; $Id$


(##include "read-apply.scm")

(define-reader-ctor 'cond-expand
  (lambda clauses

    (define (feature-present? id)
      (memq id ALL-FEATURES))

		; Interpret <feature requirement>
		;    --> <feature identifier>
		;      | (and <feature requirement>*)
		;      | (or <feature requirement>*)
		;      | (not <feature requirement>)
    (define (eval-feature-req? feature-req)

      (define (eval-and-clause? req-list)
        (or (null? req-list)
          (and (eval-feature-req? (car req-list))
            (eval-and-clause? (cdr req-list)))))

      (define (eval-or-clause? req-list)
        (and (not (null? req-list))
          (or (eval-feature-req? (car req-list))
            (eval-or-clause? (cdr req-list)))))
 
      (define (eval-not-clause? req)
        (not (eval-feature-req? req)))

      (cond
        ((not (pair? feature-req)) (feature-present? feature-req))
        ((eq? 'and (car feature-req)) (eval-and-clause? (cdr feature-req)))
        ((eq? 'or (car feature-req)) (eval-or-clause? (cdr feature-req)))
        ((eq? 'not (car feature-req)) (apply eval-not-clause? (cdr feature-req)))
        (else (error "Invalid <feature requirement>"))))

    (define (do-cond-expand clauses)
      (cond
        ((null? clauses)  (error "Unfulfilled cond-expand"))
        ((not (pair? (car clauses))) (error "Invalid <cond-expand clause>"))
		    ;((cond-expand (else body ...))
		    ;  (begin body ...))
        ((eq? 'else (caar clauses))
          (or (null? (cdr clauses))
            (error "else clause is not the final one"))
          (cons 'begin (cdar clauses)))
        ((eval-feature-req? (caar clauses))
          (cons 'begin (cdar clauses)))
        (else (do-cond-expand (cdr clauses)))))

    (do-cond-expand clauses)))

