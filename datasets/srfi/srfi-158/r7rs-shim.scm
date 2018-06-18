(define *eof-object* (read (open-input-string "")))
(define (eof-object) *eof-object*)
(define (make-bytevector n fill) (make-u8vector n fill))
(define (bytevector . args) (apply u8vector args))
(define (bytevector-u8-set! bv i val) (u8vector-set! bv i val))
(define (bytevector-u8-ref bv i) (u8vector-ref bv i))
(define (bytevector-length bv) (u8vector-length bv))
(define (truncate/ n1 n2) (values (quotient n1 n2) (remainder n1 n2)))


;; trivial version of make-list
(define (make-list n fill)
  (let loop ((n n) (result '()))
    (if (<= n 0)
      result
      (loop (- n 1) (cons fill result)))))


;; trivial version of string-for-each
(define (string-for-each proc string)
  (let ((len (string-length string)))
    (let loop ((i 0))
      (cond
        ((< i len)
         (proc (string-ref string i))
         (loop (+ i 1)))
        (else
         (if #f #f))))))
