;;; 2002 Jussi Piitulainen

;;; Error messages must be tested by hand.

(define (r x) (fail-ref 'arguments x))
(define (v x) (fail-ref 'vector x))
(define (a x) (fail-ref 'actor x))

(define (s x) (fail-set 'arguments x))
(define (w x) (fail-set 'vector x))
(define (b x) (fail-set 'actor x))              

(define (fail-ref alt k)
  (define (ref a . ix)
    (case alt
      ((arguments) (apply array-ref a ix))
      ((vector) (array-ref a (apply vector ix)))
      ((actor) (array-ref a (apply array (shape 0 (length ix)) ix)))
      (else (error "bad type"))))
  (case k
    ((0) (ref (shape 0 1)))
    ((1) (ref (shape 0 1) 1))
    ((2) (ref (shape 0 1) 1 2))
    ((3) (ref (shape 0 1) 1 2 3))
    ((4) (ref (array (shape 0 1) *)))
    ((5) (ref (array (shape 0 1) *) 1))
    ((6) (ref (array (shape 0 1) *) 2))
    ((7) (ref (array (shape 0 1) *) 0 1))
    ((8) (ref (array (shape) *) 0))
    (else (error "bad number"))))

(define (fail-set alt k)
  (define (set a . ix.o)
    (define ix (reverse (cdr (reverse ix.o))))
    (define o (car (reverse ix.o)))
    (case alt
      ((arguments) (apply array-set! a ix.o))
      ((vector) (array-set! a (list->vector ix) o))
      ((actor) (array-set! a (apply array (shape 0 (length ix)) ix) o))
      (else (error "bad type"))))
  (case k
    ((0) (set (shape 0 1) *))
    ((1) (set (shape 0 1) 1 *))
    ((2) (set (shape 0 1) 1 2 *))
    ((3) (set (shape 0 1) 1 2 3 *))
    ((4) (set (array (shape 0 1) *) *))
    ((5) (set (array (shape 0 1) *) 1 *))
    ((6) (set (array (shape 0 1) *) 2 *))
    ((7) (set (array (shape 0 1) *) 0 1 *))
    ((8) (set (array (shape) *) 0 *))
    (else (error "bad number"))))

(define (fail-share alt k)
  (define (share a s p)
    (case alt
      ((arguments) (share-array a s p))
      (else (error "bad type"))))
  (case k
    ((0) (share (array (shape 0 1) *)
                (shape 1 2)
                values))
    ((1) (share (array (shape 0 1) *)
                (shape 2 4 2 4)
                (lambda _ 1)))
    ((2) (share (array (shape 0 1) *)
                (shape 2 4 2 4)
                (lambda _ (values 0 0))))
    (else (error "bad number"))))
