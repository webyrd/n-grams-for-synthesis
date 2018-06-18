;;; Necessary R7RS syntax and procedures

(define-syntax when
  (syntax-rules ()
    ((when test . body)
     (if test (begin . body)))))

;;> \procedure{(string-map proc str)}
;;>
;;> Returns a new string composed of applying the procedure \var{proc}
;;> to every character in \var{string}.

(define (string-map proc str . los)
  (let* ((out (open-output-string))
        (void (apply string-for-each
           (lambda args (write-char (apply proc args) out))
           str los))
        (res (get-output-string out)))
    (close-output-port out)
    res))

;;> \procedure{(string-for-each proc str)}
;;>
;;> Apply \var{proc} to every character in \var{str} in order and
;;> discard the result.

(define (string-for-each proc str . los)
  (if (null? los)
      (string-fold (lambda (ch a) (proc ch)) #f str)
      (let ((los (cons str los)))
        (let lp ((is (map string-cursor-start los)))
          (cond
           ((any (lambda (str i)
                   (string-cursor>=? i (string-cursor-end str)))
                 los is))
           (else
            (apply proc (map string-cursor-ref los is))
            (lp (map string-cursor-next los is))))))))

