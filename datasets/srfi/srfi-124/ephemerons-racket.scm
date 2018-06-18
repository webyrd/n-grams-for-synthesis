(define %make-ephemeron make-ephemeron)

;; ephemeron? is already defined correctly

(define (make-ephemeron key datum)
  (%make-ephemeron key (cons key datum)))

(define (ephemeron-broken? ephemeron)
  (not (ephemeron-value ephemeron)))

(define (ephemeron-key ephemeron)
  (let ((value (ephemeron-value ephemeron)))
    (if (eq? value #f)
      #f
      (car value))))

(define (ephemeron-datum ephemeron)
  (let ((value (ephemeron-value ephemeron)))
    (if (eq? value #f)
      #f
      (cdr value))))
