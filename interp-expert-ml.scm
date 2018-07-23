(define (eval-expo expr env val context)
  ; for debugging build-and-run-code

  #|
  (conde
    ((nil . ,nil-evalo)
    (num . ,num-evalo)
    (bool . ,bool-evalo)
    (var . ,var-evalo)
    (lambda . ,lambda-evalo)
    (app . ,app-evalo)
    (car . ,car-evalo)
    (cdr . ,cdr-evalo)
    (null? . ,null?-evalo)
    (cons . ,cons-evalo)
    (pair . ,pair-evalo)
    (if . ,if-evalo)
    (equal? . ,equal?-evalo)
    (and . ,and-evalo)
    (or . ,or-evalo)
    (list . ,list-evalo)
    (symbol? . ,symbol?-evalo)
    (not . ,not-evalo)
    (letrec . ,letrec-evalo)))
  |#
  
  (build-and-run-conde expr env val
                       expert-ordering-ml
                       ))
