(define (!-o expr gamma type context)
  ; for debugging build-and-run-code

  #|
  (conde
    ((nil . ,nil-!-o)
    (num . ,num-!-o)
    (bool . ,bool-!-o)
    (var . ,var-!-o)
    (lambda . ,lambda-!-o)
    (app . ,app-!-o)
    (car . ,car-!-o)
    (cdr . ,cdr-!-o)
    (null? . ,null?-!-o)
    (cons . ,cons-!-o)
    (pair . ,pair-!-o)
    (if . ,if-!-o)
    (equal? . ,equal?-!-o)
    (and . ,and-!-o)
    (or . ,or-!-o)
    (list . ,list-!-o)
    (symbol? . ,symbol?-!-o)
    (not . ,not-!-o)
    (letrec . ,letrec-!-o)))
  |#
  
  (build-and-run-conde-ml-infer expr gamma type
                                expert-ordering-ml-infer
                                ))
