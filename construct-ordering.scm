
(define expert-ordering-alist
  `((quote . ,quote-evalo)
    (num . ,num-evalo)
    (bool . ,bool-evalo)
    (var . ,var-evalo)
    (lambda . ,lambda-evalo)
    (app . ,app-evalo)
    (car . ,car-evalo)
    (cdr . ,cdr-evalo)
    (null? . ,null?-evalo)
    (cons . ,cons-evalo)
    (if . ,if-evalo)
    (equal? . ,equal?-evalo)
    (and . ,and-evalo)
    (or . ,or-evalo)
    (list . ,list-evalo)
    (symbol? . ,symbol?-evalo)
    (not . ,not-evalo)
    (letrec . ,letrec-evalo)
    (match . ,match-evalo)))

(define expert-ordering
  (map cdr (if lookup-optimization?
               (remove (assq 'var expert-ordering-alist) expert-ordering-alist)
               expert-ordering-alist)))
