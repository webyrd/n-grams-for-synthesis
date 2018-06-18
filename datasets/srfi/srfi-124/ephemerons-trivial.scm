(define-record &lt;ephemeron&gt;
  (%make-ephemeron key datum broken?)
  ephemeron?
  (key ephemeron-key)
  (datum ephemeron-datum)
  (broken? ephemeron-broken?))

(define (make-ephemeron key datum) (%make-ephemeron key datum #f))

(define (reference-barrier key) #t)
