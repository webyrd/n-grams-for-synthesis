
(define expert-ordering-alist-ml-infer
  `((nil . ,nil-!-o)
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

(define expert-ordering-ml-infer
  (map cdr (if lookup-optimization?
               (remove (assq 'var expert-ordering-alist) expert-ordering-alist-ml-infer)
               expert-ordering-alist-ml-infer)))
