(define *output-table-file-name* "tmp/variant-expert-ordering-with-application-and-lookup-optimizations-table.scm")

(define allow-incomplete-search? #f)

(define lookup-optimization? #t)

(load "mk-vicare.scm")
(load "mk.scm")
(load "test-check.scm")
(load "interp-app-optimization.scm")
(load "construct-ordering.scm")
(load "interp-expert.scm")

(display "Testing expert ordering with foldr\n")

(time
  (run 1 (defn)  
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh (q)    
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn) 
        (absento g6 defn)
        (absento g7 defn)
    (absento 'match defn)
        (fresh (a)
          (== `(lambda (f acc xs)
                 ,a)
              defn))
        (evalo `(letrec ((foldr ,defn))
                  (list
                    (foldr ',g2 ',g1 '())
                    (foldr (lambda (a d) (cons a d)) ',g3 '(,g4))
                    (foldr (lambda (a d) (cons a d)) ',g4 '(,g5 ,g6))
                    (foldr (lambda (v1 v2) (equal? v1 v2)) ',g7 '(,g7))))
               (list
                g1
                `(,g4 . ,g3)
                `(,g5 ,g6 . ,g4)
                #t
                ))))))

(display "Testing expert ordering with append\n")
(display "Warning this won't terminate\n")


(time
  (run 1 (prog)
       (fresh ()
         (absento 'a prog)
         (absento 'b prog)
         (absento 'c prog)
         (absento 'd prog)
         (absento 'e prog)
         (absento 'f prog)
         (evalo
          `(letrec ((append ,prog))
             (list
              (append '() '())
              (append '(a) '(b))
              (append '(c d) '(e f))))
          '(()
            (a b)
            (c d e f))))))
