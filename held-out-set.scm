;; Don't train on these programs!
;;
;; We will use them to benchmark synthesis.

(define held-out-exprs
  '(
    
    (define reverse
      (lambda (ls)
        (if (null? ls)
            '()
            (append (reverse (cdr ls)) (list (car ls))))))

    (define append                     
      (lambda (ls1 ls2)
        (if (null? ls1)
            ls2
            (cons (car ls1) (append (cdr ls1) ls2)))))

    ))
