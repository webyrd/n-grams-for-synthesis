(load "variant-dynamic-ordering-with-application-and-lookup-optimizations.scm")

(define-syntax eg
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (printf "Testing ~s\n" title)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (printf "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
                     'tested-expression expected produced)))))))

(eg "procedure? is defined"
 (run 1 (q)
   (evalo '(procedure? (lambda (x) x)) q))
 '((#t)))

(eg
 "for-eacho1"
 (run 2 (q)
   (for-eacho (lambda (x) (== x 1)) (list 1 q)))
 '((1)))

(eg
 "for-eacho2"
 (run 2 (q)
   (for-eacho2 (lambda (x y) (== x y)) (list 1 q) (list q 1)))
 '((1)))

(eg
 "interleave1"
 (run 2 (q)
   (evalo '(interleave (lambda () 1) (lambda () 2)) q))
 '(((1 2))))

(eg
 "interleave2"
 (run 2 (q)
   (evalo '(interleave (lambda () (lambda () 1)) (lambda () 2)) q))
 '(((1 2))))

(eg
 "interleave3"
 (run 2 (q)
   (evalo '(interleave (lambda () (lambda () 1)) (lambda () (interleave (lambda () 2) (lambda () 3)))) q))
 '(((1 (2 3)))))

(eg
 "interleave-rec"
 (run 2 (q)
   (evalo '(letrec ((revi (lambda (l s) (lambda () (if (null? l) s (revi (cdr l) (cons (car l) s)))))))
             (interleave (revi '() '()) (revi '(a b) '(c d))))
          q))
 '(((() (b a c d)))))

(eg
 "interleave-omega"
 (run 2 (q)
   (evalo '(letrec ((omega (lambda () (lambda () (omega)))))
             (interleave (omega) (lambda () 1)))
          (list q 2)))
 '())

(eg
 "interleave-omega-r"
 (run 2 (q)
   (evalo '(letrec ((omega (lambda () (lambda () (omega)))))
             (interleave  (lambda () 1) (omega)))
          (list 2 q)))
 '())

(eg
 "interleave-omega3"
 (run 2 (q)
   (evalo '(letrec ((omega (lambda () (lambda () (omega)))))
             (list 1 (interleave  (lambda () 1) (omega))))
          (list 1 (list 2 q))))
 '())

(eg
 "interleave-omega4"
 (run 2 (q)
   (evalo '(letrec ((omega (lambda () (lambda () (omega)))))
             (interleave (omega) (lambda () 1) (lambda () 3)))
          (list q 2 3)))
 '())

(eg
 "interleave-omega5"
 (run 2 (q)
   (evalo '(letrec ((omega (lambda () (lambda () (omega)))))
             (interleave (lambda () 1) (lambda () 3) (omega)))
          (list 1 4 q)))
 '())

(eg
 "interleave-omega6"
 (run 2 (q)
   (evalo '(letrec ((omega (lambda () (lambda () (omega)))))
             (interleave (lambda () 2) (omega) (lambda () 3)))
          (list 1 q 3)))
 '())
