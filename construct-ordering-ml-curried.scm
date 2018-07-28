
(define expert-ordering-alist-ml
  `((nil . ,nil-evalo)
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
    (symbol? . ,symbol?-evalo)
    (not . ,not-evalo)
    (letrec . ,letrec-evalo)))

(define expert-ordering-ml
  (map cdr (if lookup-optimization?
               (remove (assq 'var expert-ordering-alist-ml) expert-ordering-alist-ml)
               expert-ordering-alist-ml)))
