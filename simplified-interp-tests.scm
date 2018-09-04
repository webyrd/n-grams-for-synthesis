;(load "mk-vicare.scm")
;(load "mk.scm")
;(load "test-check.scm")
;(load "interp-simplified.scm")

(test-runner
 ;; timeout in seconds

 10


;; append->fold-right
;; TODO build up to full synthesis

(test "append->fold-right-0"
  (run 1 (defn)
    (fresh (body)
      (absento 1 defn)
      (absento 2 defn)
      (absento 3 defn)
      (absento 4 defn)
      (absento 5 defn)
      (absento 6 defn)
      (== defn `(fold-right (lambda (f acc xs)
                              (if (null? xs)
                                  acc
                                  (f (car xs)
                                     (fold-right f acc (cdr xs)))))))
      (evalo
       `(letrec (,defn)
          (letrec ((append
                    (lambda (xs ys)
                      (fold-right (lambda (a d) (cons a d)) ys xs))))
            (list (append '() '())
                  (append '(1) '(2))
                  (append '(3 4) '(5 6)))))
       `(() (1 2) (3 4 5 6)))))
  '(((fold-right (lambda (f acc xs) (if (null? xs) acc (f (car xs) (fold-right f acc (cdr xs)))))))))

;;; TODO -- need more input/output examples to avoid underspecification:
;; variant-dynamic-ordering-with-application-and-lookup-optimizations returns
;;; (((fold-right (lambda (f acc xs) (if (null? xs) acc (cons (car xs) (fold-right xs acc (cdr xs))))))))
(test "append->fold-right-full"
  (run 1 (defn)
    (fresh (body)
      (absento 1 defn)
      (absento 2 defn)
      (absento 3 defn)
      (absento 4 defn)
      (absento 5 defn)
      (absento 6 defn)
      (== defn `(fold-right (lambda (f acc xs) ,body)))
      (evalo
       `(letrec (,defn)
          (letrec ((append
                    (lambda (xs ys)
                      (fold-right (lambda (a d) (cons a d)) ys xs))))
            (list (append '() '())
                  (append '(1) '(2))
                  (append '(3 4) '(5 6)))))
       `(() (1 2) (3 4 5 6)))))
  '(((fold-right (lambda (f acc xs) (if (null? xs) acc (f (car xs) (fold-right f acc (cdr xs)))))))))


;; rember

(test "rember-1"
  (run 1 (q)
    (evalo
     `(letrec ([remove
                (lambda (x ls)
                  (if (null? ls)
                      '()
                      (if (equal? (car ls) x)
                          (remove x (cdr ls))
                          (cons (car ls) (remove x (cdr ls))))))])
        (list (remove 'foo '())
              (remove 'foo '(foo))
              (remove 'foo '(1))
              (remove 'foo '(2 foo 3))
              (remove 'foo '(bar foo baz (foo) foo ((quux foo) foo)))
              (remove 'foo '((4 foo) foo (5 (foo 6 foo)) foo 7 foo (8)))))
     '(() () (1) (2 3) (bar baz (foo) ((quux foo) foo)) ((4 foo) (5 (foo 6 foo)) 7 (8)))))
  '((_.0)))

(test "rember-2"
  (run 1 (A B C)
    (evalo
     `(letrec ([remove
                (lambda (x ls)
                  (if (null? ls)
                      '()
                      (if (equal? (car ls) x)
                          (cons ,A ,B)
                          ,C))) ])
        (list (remove 'foo '())
              (remove 'foo '(foo))
              (remove 'foo '(1))
              (remove 'foo '(2 foo 3))
              (remove 'foo '(bar foo baz (foo) foo ((quux foo) foo)))
              (remove 'foo '((4 foo) foo (5 (foo 6 foo)) foo 7 foo (8)))))
     '(() () (1) (2 3) (bar baz (foo) ((quux foo) foo)) ((4 foo) (5 (foo 6 foo)) 7 (8)))))
  '())


;; interleave
;;; TODO -- build up to full synthesis
 (test "interleave-0"
  (run 1 (defn)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh ()
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)
        (== `(lambda (l1 l2)
               (if (null? l2)
                   l1
                   (cons (car l1) (interleave l2 (cdr l1)))))
            defn)
        (evalo `(letrec ((interleave ,defn))
                  (list
                    (interleave '() '())
                    (interleave '(,g1) '(,g2))
                    (interleave '(,g3 ,g4) '(,g5 ,g6))))
               (list
                '()
                `(,g1 ,g2)
                `(,g3 ,g5 ,g4 ,g6))))))
  '(((lambda (l1 l2)
       (if (null? l2)
           l1
           (cons (car l1) (interleave l2 (cdr l1))))))))

 (test "interleave-1"
   (run 1 (defn)
     (let ((g1 (gensym "g1"))
           (g2 (gensym "g2"))
           (g3 (gensym "g3"))
           (g4 (gensym "g4"))
           (g5 (gensym "g5"))
           (g6 (gensym "g6"))
           (g7 (gensym "g7")))
       (fresh (a)
         (absento g1 defn)
         (absento g2 defn)
         (absento g3 defn)
         (absento g4 defn)
         (absento g5 defn)
         (absento g6 defn)
         (absento g7 defn)
         (== `(lambda (l1 l2)
                (if (null? ,a)
                    l1
                    (cons (car l1) (interleave l2 (cdr l1)))))
             defn)
         (evalo `(letrec ((interleave ,defn))
                   (list
                    (interleave '() '())
                    (interleave '(,g1) '(,g2))
                    (interleave '(,g3 ,g4) '(,g5 ,g6))))
                (list
                 '()
                 `(,g1 ,g2)
                 `(,g3 ,g5 ,g4 ,g6))))))
   '(((lambda (l1 l2)
        (if (null? l2)
            l1
            (cons (car l1) (interleave l2 (cdr l1))))))))

;; foldr

(test "foldr-0"
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
        (== `(lambda (f acc xs)
               (if (null? xs)
                   acc
                   (f (car xs) (foldr f acc (cdr xs)))))
            defn)
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
                )))))
  '(((lambda (f acc xs)
       (if (null? xs)
           acc
           (f (car xs) (foldr f acc (cdr xs))))))))

(test "foldr-1"
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
        (fresh (a)
          (== `(lambda (f acc xs)
                 (if (null? ,a)
                     acc
                     (f (car xs) (foldr f acc (cdr xs)))))
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
                )))))
  '(((lambda (f acc xs)
       (if (null? xs)
           acc
           (f (car xs) (foldr f acc (cdr xs))))))))

(test "foldr-2"
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
        (fresh (a)
          (== `(lambda (f acc xs)
                 (if ,a
                     acc
                     (f (car xs) (foldr f acc (cdr xs)))))
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
                )))))
  '(((lambda (f acc xs)
       (if (null? xs)
           acc
           (f (car xs) (foldr f acc (cdr xs))))))))

(test "foldr-3"
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
        (fresh (a b)
          (== `(lambda (f acc xs)
                 (if ,a
                     ,b
                     (f (car xs) (foldr f acc (cdr xs)))))
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
                )))))
  '(((lambda (f acc xs)
       (if (null? xs)
           acc
           (f (car xs) (foldr f acc (cdr xs))))))))

(test "foldr-4"
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
        (fresh (a b c)
          (== `(lambda (f acc xs)
                 (if ,a
                     ,b
                     (f (car ,c) (foldr f acc (cdr xs)))))
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
                )))))
  '(((lambda (f acc xs)
       (if (null? xs)
           acc
           (f (car xs) (foldr f acc (cdr xs))))))))

(test "foldr-5"
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
        (fresh (a b c)
          (== `(lambda (f acc xs)
                 (if ,a
                     ,b
                     (f ,c (foldr f acc (cdr xs)))))
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
                )))))
  '(((lambda (f acc xs)
       (if (null? xs)
           acc
           (f (car xs) (foldr f acc (cdr xs))))))))

(test "foldr-6"
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
        (fresh (a b c d)
          (== `(lambda (f acc xs)
                 (if ,a
                     ,b
                     (f ,c (foldr f acc (cdr ,d)))))
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
                )))))
  '(((lambda (f acc xs)
       (if (null? xs)
           acc
           (f (car xs) (foldr f acc (cdr xs))))))))

(test "foldr-7"
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
        (fresh (a)
          (== `(lambda (f acc xs)
                 (if (null? xs)
                     acc
                     (f (car xs) (foldr f acc ,a))))
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
                )))))
  '(((lambda (f acc xs)
       (if (null? xs)
           acc
           (f (car xs) (foldr f acc (cdr xs))))))))

(test "foldr-8"
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
        (fresh (a b)
          (== `(lambda (f acc xs)
                 (if (null? xs)
                     acc
                     (f (car xs) (foldr f ,a ,b))))
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
                )))))
  '(((lambda (f acc xs)
       (if (null? xs)
           acc
           (f (car xs) (foldr f acc (cdr xs))))))))

(test "foldr-9"
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
        (fresh (a b c)
          (== `(lambda (f acc xs)
                 (if (null? xs)
                     acc
                     (f (car xs) (foldr ,a ,b ,c))))
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
                )))))
  '(((lambda (f acc xs)
       (if (null? xs)
           acc
           (f (car xs) (foldr f acc (cdr xs))))))))

(test "foldr-10-extra-test"
  (run 1 (defn)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7"))
          (g8 (gensym "g8"))
          (g9 (gensym "g9"))
          (g10 (gensym "g10"))
          (g11 (gensym "g11")))
      (fresh (q)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)
        (absento g8 defn)
        (absento g9 defn)
        (absento g10 defn)
        (absento g11 defn)
        (fresh (a b c d)
          (== `(lambda (f acc xs)
                 (if (null? xs)
                     acc
                     (f (car xs) (,a ,b ,c ,d))))
              defn))
        (evalo `(letrec ((foldr ,defn))
                  (list
                    (foldr ',g2 ',g1 '())
                    (foldr (lambda (a d) (cons a d)) ',g3 '(,g4))
                    (foldr (lambda (a d) (cons a d)) ',g4 '(,g5 ,g6))
                    (foldr (lambda (a d) (cons a d)) ',g7 '(,g8 ,g9 ,g10))
                    (foldr (lambda (v1 v2) (equal? v1 v2)) ',g11 '(,g11))))
               (list
                g1
                `(,g4 . ,g3)
                `(,g5 ,g6 . ,g4)
                `(,g8 ,g9 ,g10 . ,g7)
                #t
                )))))
  '(((lambda (f acc xs)
       (if (null? xs)
           acc
           (f (car xs) (foldr f acc (cdr xs))))))))

(test "foldr-10"
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
        (fresh (a b c d)
          (== `(lambda (f acc xs)
                 (if (null? xs)
                     acc
                     (f (car xs) (,a ,b ,c ,d))))
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
                )))))
  '(((lambda (f acc xs)
       (if (null? xs)
           acc
           (f (car xs) (foldr f acc (cdr xs))))))))

(test "foldr-10-match-not-allowed"
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

        ;; disallow 'match in definition
        (absento 'match defn)
                
        (fresh (a b c d)
          (== `(lambda (f acc xs)
                 (if (null? xs)
                     acc
                     (f (car xs) (,a ,b ,c ,d))))
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
                )))))
  '(((lambda (f acc xs)
       (if (null? xs)
           acc
           (f (car xs) (foldr f acc (cdr xs))))))))
(test "foldr-11"
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
        (fresh (a b c d e)
          (== `(lambda (f acc xs)
                 (if (null? xs)
                     acc
                     (f (car ,e) (,a ,b ,c ,d))))
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
                )))))
  '(((lambda (f acc xs)
       (if (null? xs)
           acc
           (f (car xs) (foldr f acc (cdr xs))))))))

(test "foldr-12"
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
        (fresh (a b c d e f)
          (== `(lambda (f acc xs)
                 (if (null? xs)
                     acc
                     (f (,f ,e) (,a ,b ,c ,d))))
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
                )))))
  '(((lambda (f acc xs)
       (if (null? xs)
           acc
           (f (car xs) (foldr f acc (cdr xs))))))))

(test "foldr-13-ground"
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
        (fresh (a b c d e f)
          (== `(lambda (f acc xs)
                 (if (null? xs)
                     acc
                     (f (car xs) (foldr f acc (cdr xs)))))
              defn)
          (== `(lambda (f acc xs)
                 (if (null? xs)
                     acc
                     (f ,e (,a ,b ,c ,d))))
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
                )))))
  '(((lambda (f acc xs)
       (if (null? xs)
           acc
           (f (car xs) (foldr f acc (cdr xs))))))))

(test "foldr-13"
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
        (fresh (a b c d e f)
          (== `(lambda (f acc xs)
                 (if (null? xs)
                     acc
                     (f ,e (,a ,b ,c ,d))))
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
                )))))
  '(((lambda (f acc xs)
       (if (null? xs)
           acc
           (f (car xs) (foldr f acc (cdr xs))))))))

(test "foldr-14"
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
        (fresh (a b)
          (== `(lambda (f acc xs)
                 (if (null? xs)
                     acc
                     (f ,a ,b)))
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
                )))))
  '(((lambda (f acc xs)
       (if (null? xs)
           acc
           (f (car xs) (foldr f acc (cdr xs))))))))

(test "foldr-15"
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
        (fresh (a b c)
          (== `(lambda (f acc xs)
                 (if (null? xs)
                     acc
                     (,a ,b ,c)))
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
                )))))
  '(((lambda (f acc xs)
       (if (null? xs)
           acc
           (f (car xs) (foldr f acc (cdr xs))))))))

(test "foldr-16"
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
        (fresh (a)
          (== `(lambda (f acc xs)
                 (if (null? xs)
                     acc
                     ,a))
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
                )))))
  '(((lambda (f acc xs)
       (if (null? xs)
           acc
           (f (car xs) (foldr f acc (cdr xs))))))))

(test "foldr-17"
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
        (fresh (a b)
          (== `(lambda (f acc xs)
                 (if (null? xs)
                     ,a
                     ,b))
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
                )))))
  '(((lambda (f acc xs)
       (if (null? xs)
           acc
           (f (car xs) (foldr f acc (cdr xs))))))))

(test "foldr-18"
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
        (fresh (a b c)
          (== `(lambda (f acc xs)
                 (if (null? ,a)
                     ,b
                     ,c))
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
                )))))
  '(((lambda (f acc xs)
       (if (null? xs)
           acc
           (f (car xs) (foldr f acc (cdr xs))))))))

(test-p "foldr-19"
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
        (fresh (a b c)
          (== `(lambda (f acc xs)
                 (if ,a
                     ,b
                     ,c))
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
                )))))
  (one-of?
   '((((lambda (f acc xs)
         (if (null? xs)
             acc
             (f (car xs) (foldr f acc (cdr xs)))))))
     ;;
     (((lambda (f acc xs)
         (if (quote #t)
             (if (null? xs)
                 acc
                 (f (car xs) (foldr f acc (cdr xs))))
             _.0)))))))

(test "foldr-20"
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
                )))))
  '(((lambda (f acc xs)
       (if (null? xs)
           acc
           (f (car xs) (foldr f acc (cdr xs))))))))

(test "foldr-21"
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
                )))))
  '(((lambda (f acc xs)
       (if (null? xs)
           acc
           (f (car xs) (foldr f acc (cdr xs))))))))

 
;; append tests
 
 (test "append-0"
        (run* (q)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons (car l) (append (cdr l) s))))))
              (append '(a b c) '(d e)))
           '(a b c d e)))
        '((_.0)))

 (test "append-1"
        (run* (q)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons (car l) (append (cdr l) s))))))
              (append '(a b c) '(d e)))
           q))
        '(((a b c d e))))

 (test-p "append-2"
   (run 2 (q)
     (evalo
      `(letrec ((append
                 (lambda (l s)
                   (if (null? l)
                       s
                       (cons (car l) (append (cdr l) s))))))
         (append ,q '(d e)))
      '(a b c d e)))
   (one-of?
     '((('(a b c))
        ((car '((a b c) . _.0))
         (absento (closure _.0) (prim _.0))))
       ;;
       (('(a b c))
        (((lambda _.0 '(a b c)))
         (=/= ((_.0 quote)))
         (sym _.0)))
       ;;
       (((cdr '(_.0 a b c))
         (absento (closure _.0) (prim _.0)))
        ((car '((a b c) . _.0))
         (absento (closure _.0) (prim _.0))))
       ;;
       (((cdr '(_.0 a b c))
         (absento (closure _.0) (prim _.0)))
        ((cdr (cdr '(_.0 _.1 a b c)))
         (absento (closure _.0) (closure _.1) (prim _.0) (prim _.1)))))))

 (test "append-3"
        (run* (q)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons (car l) (append (cdr l) s))))))
              (append ',q '(d e)))
           '(a b c d e)))
        '(((a b c))))

 (test "append-4"
        (run* (q r)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons (car l) (append (cdr l) s))))))
              (append ',q ',r))
           '(a b c d e)))
        '(((() (a b c d e)))
          (((a) (b c d e)))
          (((a b) (c d e)))
          (((a b c) (d e)))
          (((a b c d) (e)))
          (((a b c d e) ()))))

 (test "append-5"
        (run 1 (q)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons ,q (append (cdr l) s))))))
              (append '(a b c) '(d e)))
           '(a b c d e)))
        '(((car l))))

 (test "append-6"
        (run 1 (q)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons (car l) (append (cdr ,q) s))))))
              (append '(a b c) '(d e)))
           '(a b c d e)))
        '((l)))

 (test "append-7"
        (run 1 (q)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons (car l) (append (,q l) s))))))
              (append '(a b c) '(d e)))
           '(a b c d e)))
        '((cdr)))

 (test "append-8"
        (run 1 (q r)
          (symbolo q)
          (symbolo r)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons (car l) (append (,q ,r) s))))))
              (append '(a b c) '(d e)))
           '(a b c d e)))
        '(((cdr l))))

 (test "append-9"
   (run 1 (q r)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons (car l) (append (,q ,r) s))))))
              (append '(a b c) '(d e)))
           '(a b c d e)))
        '(((cdr l))))

 (test-p "append-10"
   (run 1 (q)
     (evalo
      `(letrec ((append
                 (lambda (l s)
                   (if ,q
                       s
                       (cons (car l) (append (cdr l) s))))))
         (append '(a b c) '(d e)))
      '(a b c d e)))
   (one-of?
    '((((null? l)))
      ;;
      (((equal? l (quote ())))))))

 (test-p "append-11"
   (run 4 (q)
     (evalo
      `(letrec ((append
                 (lambda (l s)
                   (if ,q
                       s
                       (cons (car l) (append (cdr l) s))))))
         (append '(a b c) '(d e)))
      '(a b c d e)))
   (each-one-of? '(((null? l))
                   (((lambda () (null? l))))
                   ((null? ((lambda () l))))
                   ((equal? '() l))
                   ((and (null? l)))
                   ((and (null? l) (quote _.0)) (=/= ((_.0 #f))) (absento (closure _.0) (prim _.0)))
                   ((and (null? l) (quote #t)))
                   ((if '#t (null? l) _.0))
                   ((if '#f _.0 (null? l)))
                   ((if #t (null? l) _.0))
                   ((null? (cdr (cons s l))))
                   ((equal? l '()))
                   (((lambda (_.0) _.0) (null? l))
                    (=/= ((_.0 and)) ((_.0 car)) ((_.0 cdr)) ((_.0 cons)) ((_.0 equal?)) ((_.0 if)) ((_.0 lambda)) ((_.0 letrec)) ((_.0 list)) ((_.0 match)) ((_.0 not)) ((_.0 null?)) ((_.0 or)) ((_.0 quote)) ((_.0 symbol?)))
                    (sym _.0)))))

 (test "append-12"
   (run 1 (prog)
     (fresh (q r s)
       (absento 'a prog)
       (absento 'b prog)
       (absento 'c prog)
       (absento 'd prog)
       (absento 'e prog)
       (absento 'f prog)
       (== `(lambda (l s)
              (if ,q
                  ,r
                  (cons (car l) (append (cdr l) s))))
           prog)
       (evalo
        `(letrec ((append ,prog))
           (list
            (append '() '())
            (append '(a) '(b))
            (append '(c d) '(e f))))
        '(()
          (a b)
          (c d e f)))))
   '(((lambda (l s)
        (if (null? l)
            s
            (cons (car l) (append (cdr l) s)))))))

 (test "append-13"
   (run 1 (prog)
     (fresh (q r s)
       (absento 'a prog)
       (absento 'b prog)
       (absento 'c prog)
       (absento 'd prog)
       (absento 'e prog)
       (absento 'f prog)
       (== `(lambda (l s)
              (if ,q
                  ,r
                  (cons (car l) (append (cdr l) s))))
           prog)
       (evalo
        `(letrec ((append ,prog))
           (list
            (append '() '())
            (append '(a) '(b))
            (append '(c d) '(e f))))
        '(()
          (a b)
          (c d e f)))))
   '(((lambda (l s)
        (if (null? l)
            s
            (cons (car l) (append (cdr l) s)))))))

 (test "append-14"
   (run 1 (prog)
     (fresh (q r s)
       (absento 'a prog)
       (absento 'b prog)
       (absento 'c prog)
       (absento 'd prog)
       (absento 'e prog)
       (absento 'f prog)
       (== `(lambda (l s)
              (if ,q
                  ,r
                  (,s (car l) (append (cdr l) s))))
           prog)
       (evalo
        `(letrec ((append ,prog))
           (list
            (append '() '())
            (append '(a) '(b))
            (append '(c d) '(e f))))
        '(()
          (a b)
          (c d e f)))))
   '(((lambda (l s)
        (if (null? l)
            s
            (cons (car l) (append (cdr l) s)))))))

 (test "append-15"
   (run 1 (prog)
     (fresh (q r s t)
       (absento 'a prog)
       (absento 'b prog)
       (absento 'c prog)
       (absento 'd prog)
       (absento 'e prog)
       (absento 'f prog)
       (== `(lambda (l s)
              (if ,q
                  ,r
                  (,s (car l) (append ,t s))))
           prog)
       (evalo
        `(letrec ((append ,prog))
           (list
            (append '() '())
            (append '(a) '(b))
            (append '(c d) '(e f))))
        '(()
          (a b)
          (c d e f)))))
   '(((lambda (l s)
        (if (null? l)
            s
            (cons (car l) (append (cdr l) s)))))))

 (test "append-16"
   (run 1 (prog)
     (fresh (q r s t)
       (absento 'a prog)
       (absento 'b prog)
       (absento 'c prog)
       (absento 'd prog)
       (absento 'e prog)
       (absento 'f prog)
       (== `(lambda (l s)
              (if ,q
                  ,r
                  (,s (car l) ,t)))
           prog)
       (evalo
        `(letrec ((append ,prog))
           (list
            (append '() '())
            (append '(a) '(b))
            (append '(c d) '(e f))))
        '(()
          (a b)
          (c d e f)))))
   '(((lambda (l s)
        (if (null? l)
            s
            (cons (car l) (append (cdr l) s)))))))

 (test "append-17"
   (run 1 (prog)
     (fresh (q r s)
       (absento 'a prog)
       (absento 'b prog)
       (absento 'c prog)
       (absento 'd prog)
       (absento 'e prog)
       (absento 'f prog)
       (== `(lambda (l s)
              (if ,q
                  ,r
                  ,s))
           prog)
       (evalo
        `(letrec ((append ,prog))
           (list
            (append '() '())
            (append '(a) '(b))
            (append '(c d) '(e f))))
        '(()
          (a b)
          (c d e f)))))
   '(((lambda (l s)
        (if (null? l)
            s
            (cons (car l) (append (cdr l) s)))))))

 (test "append-18"
   (run 1 (prog)
     (fresh (q)
       (absento 'a prog)
       (absento 'b prog)
       (absento 'c prog)
       (absento 'd prog)
       (absento 'e prog)
       (absento 'f prog)
       (== `(lambda (l s)
              ,q)
           prog)
       (evalo
        `(letrec ((append ,prog))
           (list
            (append '() '())
            (append '(a) '(b))
            (append '(c d) '(e f))))
        '(()
          (a b)
          (c d e f)))))
   '(((lambda (l s)
        (if (null? l)
            s
            (cons (car l) (append (cdr l) s)))))))

 (test-p "append-19"
   (run 1 (prog)
     (fresh (q r)
       (absento 'a prog)
       (absento 'b prog)
       (absento 'c prog)
       (absento 'd prog)
       (absento 'e prog)
       (absento 'f prog)
       (== `(lambda ,q
              ,r)
           prog)
       (evalo
        `(letrec ((append ,prog))
           (list
            (append '() '())
            (append '(a) '(b))
            (append '(c d) '(e f))))
        '(()
          (a b)
          (c d e f)))))
   (one-of?
    '((((lambda (_.0 _.1)
          (if (null? _.0)
              _.1
              (cons (car _.0) (append (cdr _.0) _.1))))
        (=/= ((_.0 _.1)) ((_.0 a)) ((_.0 append)) ((_.0 b)) ((_.0 c)) ((_.0 car)) ((_.0 cdr)) ((_.0 cons)) ((_.0 d)) ((_.0 e)) ((_.0 f)) ((_.0 if)) ((_.0 null?)) ((_.1 a)) ((_.1 append)) ((_.1 b)) ((_.1 c)) ((_.1 car)) ((_.1 cdr)) ((_.1 cons)) ((_.1 d)) ((_.1 e)) ((_.1 f)) ((_.1 if)) ((_.1 null?)))
        (sym _.0 _.1)))
      (((lambda (_.0 _.1) (if (null? _.0) _.1 (cons (car _.0) (append (cdr _.0) _.1)))) (=/= ((_.0 _.1)) ((_.0 a)) ((_.0 and)) ((_.0 append)) ((_.0 b)) ((_.0 c)) ((_.0 car)) ((_.0 cdr)) ((_.0 cons)) ((_.0 d)) ((_.0 e)) ((_.0 equal?)) ((_.0 f)) ((_.0 if)) ((_.0 lambda)) ((_.0 letrec)) ((_.0 list)) ((_.0 match)) ((_.0 not)) ((_.0 null?)) ((_.0 or)) ((_.0 quote)) ((_.0 symbol?)) ((_.1 a)) ((_.1 and)) ((_.1 append)) ((_.1 b)) ((_.1 c)) ((_.1 car)) ((_.1 cdr)) ((_.1 cons)) ((_.1 d)) ((_.1 e)) ((_.1 equal?)) ((_.1 f)) ((_.1 if)) ((_.1 lambda)) ((_.1 letrec)) ((_.1 list)) ((_.1 match)) ((_.1 not)) ((_.1 null?)) ((_.1 or)) ((_.1 quote)) ((_.1 symbol?))) (sym _.0 _.1)))))   
   )

	    
 
;; reverse tests

(test "reverse-1"
  (run 1 (q r s)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh (defn)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)
        (== `(lambda (xs)
               (if (null? xs)
                   '()
                   (,q (reverse ,r) ,s)))
            defn)
         
        ;;(== 'append q)
        (== '(cdr xs) r)
        (== '(cons (car xs) '()) s)
        (evalo `(letrec ((append
                          (lambda (l s)
                            (if (null? l) s
                                (cons (car l)
                                      (append (cdr l) s))))))
                  (letrec ((reverse ,defn))
                    (list
                     (reverse '())
                     (reverse '(,g1))
                     (reverse '(,g2 ,g3))
                     (reverse '(,g4 ,g5 ,g6)))))
               (list '() `(,g1) `(,g3 ,g2) `(,g6 ,g5 ,g4))))))    
  '(((append (cdr xs) (cons (car xs) '())))))

(test "reverse-2"
  (run 1 (q r s)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh (defn)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)
        (== `(lambda (xs)
               (if (null? xs)
                   '()
                   (,q (reverse ,r) ,s)))
            defn)
        
        (== 'append q)
        ;;(== '(cdr xs) r)
        (== '(cons (car xs) '()) s)
        (evalo `(letrec ((append
                          (lambda (l s)
                            (if (null? l) s
                                (cons (car l)
                                      (append (cdr l) s))))))
                  (letrec ((reverse ,defn))
                    (list
                     (reverse '())
                     (reverse '(,g1))
                     (reverse '(,g2 ,g3))
                     (reverse '(,g4 ,g5 ,g6)))))
               (list '() `(,g1) `(,g3 ,g2) `(,g6 ,g5 ,g4))))))    
  '(((append (cdr xs) (cons (car xs) '())))))

(test-p "reverse-3"
  (run 1 (q r s)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh (defn)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)
        (== `(lambda (xs)
               (if (null? xs)
                   '()
                   (,q (reverse ,r) ,s)))
            defn)
        
        (== 'append q)
        (== '(cdr xs) r)
        ;;(== '(cons (car xs) '()) s)
        (evalo `(letrec ((append
                          (lambda (l s)
                            (if (null? l) s
                                (cons (car l)
                                      (append (cdr l) s))))))
                  (letrec ((reverse ,defn))
                    (list
                     (reverse '())
                     (reverse '(,g1))
                     (reverse '(,g2 ,g3))
                     (reverse '(,g4 ,g5 ,g6)))))
               (list '() `(,g1) `(,g3 ,g2) `(,g6 ,g5 ,g4))))))
  (one-of? '((((append (cdr xs) (cons (car xs) '()))))
             (((append (cdr xs) (list (car xs)))))
             (((append (cdr xs) (cons (car xs) (list))))))))

(test-p "reverse-4"
  (run 1 (q r s)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh (defn)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)
        (== `(lambda (xs)
               (if (null? xs)
                   '()
                   (,q (reverse ,r) ,s)))
            defn)
        
        ;;(== 'append q)
        (== '(cdr xs) r)
        ;;(== '(cons (car xs) '()) s)
        (evalo `(letrec ((append
                          (lambda (l s)
                            (if (null? l) s
                                (cons (car l)
                                      (append (cdr l) s))))))
                  (letrec ((reverse ,defn))
                    (list
                     (reverse '())
                     (reverse '(,g1))
                     (reverse '(,g2 ,g3))
                     (reverse '(,g4 ,g5 ,g6)))))
               (list '() `(,g1) `(,g3 ,g2) `(,g6 ,g5 ,g4))))))
  (one-of? '((((append (cdr xs) (cons (car xs) '()))))
             (((append (cdr xs) (list (car xs)))))
             (((append (cdr xs) (cons (car xs) (list))))))))

(test-p "reverse-5"
  (run 1 (q r s)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh (defn)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)
        (== `(lambda (xs)
               (if (null? xs)
                   '()
                   (,q (reverse ,r) ,s)))
            defn)
        
        (== 'append q)
                                        ;(== '(cdr xs) r)
                                        ;(== '(cons (car xs) '()) s)
        (evalo `(letrec ((append
                          (lambda (l s)
                            (if (null? l) s
                                (cons (car l)
                                      (append (cdr l) s))))))
                  (letrec ((reverse ,defn))
                    (list
                     (reverse '())
                     (reverse '(,g1))
                     (reverse '(,g2 ,g3))
                     (reverse '(,g4 ,g5 ,g6)))))
               (list '() `(,g1) `(,g3 ,g2) `(,g6 ,g5 ,g4))))))
    (one-of? '((((append (cdr xs) (cons (car xs) '()))))
               (((append (cdr xs) (list (car xs)))))
               (((append (cdr xs) (cons (car xs) (list))))))))

(test-p "reverse-10"
  (run 1 (q r s)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh (defn)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)
        (== `(lambda (xs)
               (if (null? xs)
                   '()
                   (,q (reverse ,r) ,s)))
            defn)
        
        ;;(== 'append q)
        ;;(== '(cdr xs) r)
        ;;(== '(cons (car xs) '()) s)
        (evalo `(letrec ((append
                          (lambda (l s)
                            (if (null? l) s
                                (cons (car l)
                                      (append (cdr l) s))))))
                  (letrec ((reverse ,defn))
                    (list
                     (reverse '())
                     (reverse '(,g1))
                     (reverse '(,g2 ,g3))
                     (reverse '(,g4 ,g5 ,g6)))))
               (list '() `(,g1) `(,g3 ,g2) `(,g6 ,g5 ,g4))))))
  (one-of? '((((append (cdr xs) (cons (car xs) '()))))
             (((append (cdr xs) (list (car xs)))))
             (((append (cdr xs) (cons (car xs) (list))))))))

(test "reverse-20"
  (run 1 (q r s t)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh (defn)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)
        (== `(lambda (xs)
               (if (null? xs)
                   '()
                   (,q (,r ,s) ,t)))
            defn)
        
        ;;(== 'append q)
        ;;(== '(cdr xs) r)
        ;;(== '(cons (car xs) '()) s)
        (evalo `(letrec ((append
                          (lambda (l s)
                            (if (null? l) s
                                (cons (car l)
                                      (append (cdr l) s))))))
                  (letrec ((reverse ,defn))
                    (list
                     (reverse '())
                     (reverse '(,g1))
                     (reverse '(,g2 ,g3))
                     (reverse '(,g4 ,g5 ,g6)))))
               (list '() `(,g1) `(,g3 ,g2) `(,g6 ,g5 ,g4))))))
  '(((append (cdr xs) (cons (car xs) '())))))

(test "reverse-30"
  (run 1 (q r s)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh (defn)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)
        (== `(lambda (xs)
               (if (null? xs)
                   '()
                   (,q ,r ,s)))
            defn)
        
        ;;(== 'append q)
        ;;(== '(cdr xs) r)
        ;;(== '(cons (car xs) '()) s)
        (evalo `(letrec ((append
                          (lambda (l s)
                            (if (null? l) s
                                (cons (car l)
                                      (append (cdr l) s))))))
                  (letrec ((reverse ,defn))
                    (list
                     (reverse '())
                     (reverse '(,g1))
                     (reverse '(,g2 ,g3))
                     (reverse '(,g4 ,g5 ,g6)))))
               (list '() `(,g1) `(,g3 ,g2) `(,g6 ,g5 ,g4))))))
  '(((append (cdr xs) (cons (car xs) '())))))

(test "reverse-40"
  (run 1 (q)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh (defn)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)
        (== `(lambda (xs)
               (if (null? xs)
                   '()
                   ,q))
            defn)
        
        ;;(== 'append q)
        ;;(== '(cdr xs) r)
        ;;(== '(cons (car xs) '()) s)
        (evalo `(letrec ((append
                          (lambda (l s)
                            (if (null? l) s
                                (cons (car l)
                                      (append (cdr l) s))))))
                  (letrec ((reverse ,defn))
                    (list
                     (reverse '())
                     (reverse '(,g1))
                     (reverse '(,g2 ,g3))
                     (reverse '(,g4 ,g5 ,g6)))))
               (list '() `(,g1) `(,g3 ,g2) `(,g6 ,g5 ,g4))))))
  '(((append (cdr xs) (cons (car xs) '())))))

(test-p "reverse-foldr-6"
  (run 1 (defn)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh (q r s)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)
        (== `(lambda (xs)
               (if ,q ,r ,s))
            defn)
        (evalo `(letrec ((foldl (lambda (f acc xs)
                                  (if (null? xs)
                                      acc
                                      (foldl f (f (car xs) acc) (cdr xs))))))
                  (letrec ((reverse ,defn))
                    (list
                     (reverse '())
                     (reverse '(,g1))
                     (reverse '(,g2 ,g3))
                     (reverse '(,g4 ,g5 ,g6)))))
               (list '() `(,g1) `(,g3 ,g2) `(,g6 ,g5 ,g4))))))
  (one-of?
   '((((lambda (xs)
         (if (null? xs)
             xs
             (foldl cons '() xs)))))
     (((lambda (xs)
         (if (quote #t)
             (if (null? xs)
                 xs
                 (foldl cons (quote ()) xs))
             _.0)))))))


(test "reverse-foldr-7"
  (run 1 (defn)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh (q r s)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)
        (== `(lambda (xs) ,q)
            defn)
        (evalo `(letrec ((foldl (lambda (f acc xs)
                                  (if (null? xs)
                                      acc
                                      (foldl f (f (car xs) acc) (cdr xs))))))
                  (letrec ((reverse ,defn))
                    (list
                      (reverse '())
                      (reverse '(,g1))
                      (reverse '(,g2 ,g3))
                      (reverse '(,g4 ,g5 ,g6)))))
               (list '() `(,g1) `(,g3 ,g2) `(,g6 ,g5 ,g4))))))
  '(((lambda (xs)
       (if (null? xs)
           xs
           (foldl cons '() xs))))))
 

 (test-p "rev-tailcall-1"
   (run 1 (defn)
     (let ((g1 (gensym "g1"))
           (g2 (gensym "g2"))
           (g3 (gensym "g3"))
           (g4 (gensym "g4"))
           (g5 (gensym "g5"))
           (g6 (gensym "g6"))
           (g7 (gensym "g7")))
       (fresh (q r s)
         (absento g1 defn)
         (absento g2 defn)
         (absento g3 defn)
         (absento g4 defn)
         (absento g5 defn)
         (absento g6 defn)
         (absento g7 defn)
         (evalo `(letrec ((rev-tailcall ,defn))                        
                   (list
                    (rev-tailcall '() ',g7)
                    (rev-tailcall '(,g1) ',g7)
                    (rev-tailcall '(,g2 ,g3) ',g7)
                    (rev-tailcall '(,g4 ,g5 ,g6) ',g7)))
                (list g7 `(,g1 . ,g7) `(,g3 ,g2 . ,g7) `(,g6 ,g5 ,g4 . ,g7))))))
   (one-of?
    '((((lambda (_.0 _.1)
          (if (null? _.0)
              _.1
              (rev-tailcall (cdr _.0) (cons (car _.0) _.1))))
        (=/= ((_.0 _.1)) ((_.0 car)) ((_.0 cdr)) ((_.0 cons)) ((_.0 if)) ((_.0 null?)) ((_.0 rev-tailcall)) ((_.1 car)) ((_.1 cdr)) ((_.1 cons)) ((_.1 if)) ((_.1 null?)) ((_.1 rev-tailcall)))
        (sym _.0 _.1)))
      (((lambda (_.0 _.1)
          (if (null? _.0)
              _.1
              (rev-tailcall (cdr _.0) (cons (car _.0) _.1))))
        (=/= ((_.0 _.1)) ((_.0 and)) ((_.0 car)) ((_.0 cdr)) ((_.0 cons)) ((_.0 equal?)) ((_.0 if)) ((_.0 lambda)) ((_.0 letrec)) ((_.0 list)) ((_.0 match)) ((_.0 not)) ((_.0 null?)) ((_.0 or)) ((_.0 quote)) ((_.0 rev-tailcall)) ((_.0 symbol?)) ((_.1 and)) ((_.1 car)) ((_.1 cdr)) ((_.1 cons)) ((_.1 equal?)) ((_.1 if)) ((_.1 lambda)) ((_.1 letrec)) ((_.1 list)) ((_.1 match)) ((_.1 not)) ((_.1 null?)) ((_.1 or)) ((_.1 quote)) ((_.1 rev-tailcall)) ((_.1 symbol?)))
        (sym _.0 _.1)))
      )))

(test-p "foldr-from-append"
  (run 1 (defn-foldr)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh (defn-append A B)
        (absento g1 defn-foldr)
        (absento g2 defn-foldr)
        (absento g3 defn-foldr)
        (absento g4 defn-foldr)
        (absento g5 defn-foldr)
        (absento g6 defn-foldr)
        (absento g7 defn-foldr)
        (== defn-foldr `(lambda (f acc xs)
                          ,B))
        (== defn-append `(lambda (xs ys)
                           (foldr (lambda (a d) (cons a d)) ys xs)))
        (evalo `(letrec ((foldr ,defn-foldr))
                  (letrec ((append ,defn-append))
                    (list
                     (append '() '())
                     (append '(,g1) '(,g2))
                     (append '(,g3 ,g4) '(,g5 ,g6)))))
               (list '() `(,g1 ,g2) `(,g3 ,g4 ,g5 ,g6))))))
  (one-of?
    '((((lambda (f acc xs)
          (if (null? xs)
              acc
              (f (car xs) (foldr f acc (cdr xs)))))))
      ;; this answer is mk cheating, because we are folding using cons!
      (((lambda (f acc xs)
          (if (null? xs)
              acc
              (cons (car xs) (foldr xs acc (cdr xs)))))))
      ;; another cheat
      (((lambda (f acc xs)
          (match xs
            [`() xs]
            [`(,_.0) (cons _.0 acc)]
            [_.1 (f (car _.1) (f (car (cdr _.1)) acc))]
            .
            _.2))
        (=/= ((_.0 acc)) ((_.0 and)) ((_.0 car)) ((_.0 cdr)) ((_.0 cons)) ((_.0 equal?)) ((_.0 if)) ((_.0 lambda)) ((_.0 letrec)) ((_.0 list)) ((_.0 match)) ((_.0 not)) ((_.0 null?)) ((_.0 or)) ((_.0 quote)) ((_.0 symbol?)) ((_.1 acc)) ((_.1 and)) ((_.1 car)) ((_.1 cdr)) ((_.1 cons)) ((_.1 equal?)) ((_.1 f)) ((_.1 if)) ((_.1 lambda)) ((_.1 letrec)) ((_.1 list)) ((_.1 match)) ((_.1 not)) ((_.1 null?)) ((_.1 or)) ((_.1 quote)) ((_.1 symbol?)))
        (sym _.0 _.1))))))

(test-p "append-equal-0"
  (run 1 (defn)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh ()
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)
        (evalo `(letrec ((append ,defn))                  
                  (list
                   (equal? '() (append '() '()))
                   (equal? (list ',g1 ',g2) (append '(,g1) '(,g2)))
                   (equal? (list ',g3 ',g4 ',g5 ',g6) (append '(,g3 ,g4) '(,g5 ,g6)))))
               (list #t #t #t)))))
  (one-of?
    '((((lambda (_.0 _.1) (if (null? _.0) _.1 (cons (car _.0) (append (cdr _.0) _.1))))
       (=/= ((_.0 _.1)) ((_.0 append)) ((_.0 car)) ((_.0 cdr)) ((_.0 cons)) ((_.0 if)) ((_.0 null?)) ((_.1 append)) ((_.1 car)) ((_.1 cdr)) ((_.1 cons)) ((_.1 if)) ((_.1 null?)))
       (sym _.0 _.1)))
      (((lambda (_.0 _.1) (if (null? _.0) _.1 (cons (car _.0) (append (cdr _.0) _.1)))) (=/= ((_.0 _.1)) ((_.0 and)) ((_.0 append)) ((_.0 car)) ((_.0 cdr)) ((_.0 cons)) ((_.0 equal?)) ((_.0 if)) ((_.0 lambda)) ((_.0 letrec)) ((_.0 list)) ((_.0 match)) ((_.0 not)) ((_.0 null?)) ((_.0 or)) ((_.0 quote)) ((_.0 symbol?)) ((_.1 and)) ((_.1 append)) ((_.1 car)) ((_.1 cdr)) ((_.1 cons)) ((_.1 equal?)) ((_.1 if)) ((_.1 lambda)) ((_.1 letrec)) ((_.1 list)) ((_.1 match)) ((_.1 not)) ((_.1 null?)) ((_.1 or)) ((_.1 quote)) ((_.1 symbol?))) (sym _.0 _.1))))))

(test "append-equal-1"
    (run 1 (defn)
      (let ((g1 (gensym "g1"))
            (g2 (gensym "g2"))
            (g3 (gensym "g3"))
            (g4 (gensym "g4"))
            (g5 (gensym "g5"))
            (g6 (gensym "g6"))
            (g7 (gensym "g7")))
        (fresh ()
          (absento g1 defn)
          (absento g2 defn)
          (absento g3 defn)
          (absento g4 defn)
          (absento g5 defn)
          (absento g6 defn)
          (absento g7 defn)
          (evalo `(letrec ((append ,defn))                    
                    (list
                      (equal? (append '() '()) '())
                      (equal? (append '(,g1) '(,g2)) (list ',g1 ',g2))
                      (equal? (append '(,g3 ,g4) '(,g5 ,g6)) (list ',g3 ',g4 ',g5 ',g6))))
                  (list #t #t #t)))))
    '(((lambda (_.0 _.1) (if (null? _.0) _.1 (cons (car _.0) (append (cdr _.0) _.1))))
       (=/= ((_.0 _.1)) ((_.0 append)) ((_.0 car)) ((_.0 cdr)) ((_.0 cons)) ((_.0 if)) ((_.0 null?)) ((_.1 append)) ((_.1 car)) ((_.1 cdr)) ((_.1 cons)) ((_.1 if)) ((_.1 null?)))
       (sym _.0 _.1))))

(test-p "append-fast-1"
    (run 1 (defn)
      (let ((g1 (gensym "g1"))
            (g2 (gensym "g2"))
            (g3 (gensym "g3"))
            (g4 (gensym "g4"))
            (g5 (gensym "g5"))
            (g6 (gensym "g6"))
            (g7 (gensym "g7"))
            (g8 (gensym "g8"))
            (g9 (gensym "g9"))
            (g10 (gensym "g10"))
            (g11 (gensym "g11")))
        (fresh ()
          (absento g1 defn)
          (absento g2 defn)
          (absento g3 defn)
          (absento g4 defn)
          (absento g5 defn)
          (absento g6 defn)
          (absento g7 defn)
          (absento g8 defn)
          (absento g9 defn)
          (absento g10 defn)
          (absento g11 defn)
          (evalo
            `(letrec ((append ,defn)) 
               (list
                (equal? (cons ',g7 (append '() ',g8)) (append (cons ',g7 '()) ',g8))
                (equal? (cons ',g4 (append '(,g5) ',g6)) (append (cons ',g4 '(,g5)) ',g6))
                (equal? (append '(,g9) '(,g10 . ,g11)) (append '(,g9 ,g10) ',g11))))
            '(#t #t #t)))))
    (one-of?
     '((((lambda (_.0 _.1)
           (if (null? _.0)
               _.1
               (cons (car _.0) (append (cdr _.0) _.1))))
         (=/= ((_.0 _.1)) ((_.0 append)) ((_.0 car)) ((_.0 cdr)) ((_.0 cons)) ((_.0 if)) ((_.0 null?)) ((_.1 append)) ((_.1 car)) ((_.1 cdr)) ((_.1 cons)) ((_.1 if)) ((_.1 null?)))
         (sym _.0 _.1)))
       (((lambda (_.0 _.1)
           (if (null? _.0)
               _.1
               (cons (car _.0) (append (cdr _.0) _.1))))
         (=/= ((_.0 _.1)) ((_.0 and)) ((_.0 append)) ((_.0 car)) ((_.0 cdr)) ((_.0 cons)) ((_.0 equal?)) ((_.0 if)) ((_.0 lambda)) ((_.0 letrec)) ((_.0 list)) ((_.0 match)) ((_.0 not)) ((_.0 null?)) ((_.0 or)) ((_.0 quote)) ((_.0 symbol?)) ((_.1 and)) ((_.1 append)) ((_.1 car)) ((_.1 cdr)) ((_.1 cons)) ((_.1 equal?)) ((_.1 if)) ((_.1 lambda)) ((_.1 letrec)) ((_.1 list)) ((_.1 match)) ((_.1 not)) ((_.1 null?)) ((_.1 or)) ((_.1 quote)) ((_.1 symbol?)))
         (sym _.0 _.1)))
       )))


(test "list-nth-element-peano"
  (run 1 (q r)
    (evalo `(letrec ((nth (lambda (n xs)
                            (if (null? n) ,q ,r))))
              (list
               (nth '() '(foo bar))
               (nth '(s) '(foo bar))
               (nth '() '(1 2 3))
               (nth '(s) '(1 2 3))
               (nth '(s s) '(1 2 3))))
           (list 'foo 'bar 1 2 3)))
  '((((car xs) (nth (cdr n) (cdr xs))))))

(test "map-hard-0-gensym"
  (run 1 (defn)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh ()
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)
        (== `(lambda (f xs)
               (if (null? xs)
                   xs
                   (cons (f (car xs))
                         (map f (cdr xs)))))
            defn)
        (evalo `(letrec ((map ,defn))                 
                  (list
                   (map ',g1 '())
                   (map (lambda (p) (car p)) '((,g2 . ,g3)))
                   (map (lambda (p) (cdr p)) '((,g4 . ,g5) (,g6 . ,g7)))))
               (list '() `(,g2) `(,g5 ,g7))))))
  '(((lambda (f xs)
       (if (null? xs)
           xs (cons (f (car xs)) (map f (cdr xs))))))))

(test "map-hard-1-gensym"
  (run 1 (defn)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh (a b c)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)
        (== `(lambda (f xs)
               (if (null? xs)
                   ,a (cons ,b (map f ,c))))
            defn)
        (evalo `(letrec ((map ,defn))             
                  (list
                   (map ',g1 '())
                   (map (lambda (p) (car p)) '((,g2 . ,g3)))
                   (map (lambda (p) (cdr p)) '((,g4 . ,g5) (,g6 . ,g7)))))
               (list '() `(,g2) `(,g5 ,g7))))))
  '(((lambda (f xs)
       (if (null? xs)
           xs (cons (f (car xs)) (map f (cdr xs))))))))

(test "map-hard-2-gensym"
   (run 1 (defn)
     (let ((g1 (gensym "g1"))
           (g2 (gensym "g2"))
           (g3 (gensym "g3"))
           (g4 (gensym "g4"))
           (g5 (gensym "g5"))
           (g6 (gensym "g6"))
           (g7 (gensym "g7")))
       (fresh (a)
         (absento g1 defn)
         (absento g2 defn)
         (absento g3 defn)
         (absento g4 defn)
         (absento g5 defn)
         (absento g6 defn)
         (absento g7 defn)
         (== `(lambda (f xs)
                (if (null? xs)
                    xs (cons (f (car xs)) (map ,a (cdr xs)))))
             defn)
         (evalo `(letrec ((map ,defn))
                   (list
                     (map ',g1 '())
                     (map (lambda (p) (car p)) '((,g2 . ,g3)))
                     (map (lambda (p) (cdr p)) '((,g4 . ,g5) (,g6 . ,g7)))))
                (list '() `(,g2) `(,g5 ,g7))))))
   '(((lambda (f xs)
        (if (null? xs)
            xs (cons (f (car xs)) (map f (cdr xs))))))))

(test "map-hard-3-gensym"
  (run 1 (defn)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh (a)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)
        (== `(lambda (f xs) ,a)
            defn)
        (evalo `(letrec ((map ,defn))                  
                  (list
                   (map ',g1 '())
                   (map car '((,g2 . ,g3)))
                   (map cdr '((,g4 . ,g5) (,g6 . ,g7)))))
               (list '() `(,g2) `(,g5 ,g7))))))
  '(((lambda (f xs)
       (if (null? xs)
           xs (cons (f (car xs)) (map f (cdr xs))))))))

(test "map-hard-4-gensym"
  (run 1 (defn)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh ()
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)
        (evalo `(letrec ((map ,defn))                  
                  (list
                   (map ',g1 '())
                   (map car '((,g2 . ,g3)))
                   (map cdr '((,g4 . ,g5) (,g6 . ,g7)))))
               (list '() `(,g2) `(,g5 ,g7))))))
  '(((lambda (_.0 _.1)
       (if (null? _.1)
           _.1 (cons (_.0 (car _.1)) (map _.0 (cdr _.1)))))
     (sym _.0 _.1))))

 
(test "quote-and-shadowing-works-1"
  (run* (q)
    (evalo
     `(letrec ((append
                (lambda (l s)
                  (if ((lambda (x) (x #f)) (lambda (y) (quote #t)))
                      s
                      (cons (car l) (append (cdr l) s))))))
        (append '(a b c) '(d e)))
     q))
  '(((d e))))

(test "quote-and-shadowing-works-2"
  (run* (q)
    (evalo
     `(letrec ((append
                (lambda (l s)
                  (if ((lambda (quote) (quote #f)) (lambda (y) (quote #t)))
                      s
                      (cons (car l) (append (cdr l) s))))))
        (append '(a b c) '(d e)))
     q))
  (if *primitives-first-class-and-shadowable?*
      '(((d e)))
      '()))



 ;; match tests
 (test "match-0"
        (run* (q) (evalo '(match '(1 2 3 1 2)
                            [`(,x ,y ,z ,x ,y)
                             (list x y)])
                         q))
        '(((1 2))))

 (test "match-1"
        (run* (q) (evalo '(match '(1 2 3 2 1)
                            [`(,x ,y ,z ,x ,y)
                             (list x y)])
                         q))
        '())

 (test "match-2"
        (run* (q) (evalo '(match '(1)
                            [`(,quote)
                             5])
                         q))
        (if *primitives-first-class-and-shadowable?*
            '((5))
            '()))


 ;; if tests
 (test "if-0"
        (run* (q) (evalo '(if #t 5 6) q))
        '((5)))

 (test "if-1"
        (run* (q) (evalo '(if #f 5 6) q))
        '((6)))

 (test "if-2"
        (run* (q) (evalo '(if 4 5 6) q))
        (if *if-test-requires-boolean?*
            '()
            '((5))))


 ;; and tests
 (test "and-0"
        (run* (q) (evalo '(and) q))
        '((#t)))

 (test "and-1"
        (run* (q) (evalo '(and 5) q))
        '((5)))

 (test "and-2"
        (run* (q) (evalo '(and #f) q))
        '((#f)))

 (test "and-3"
        (run* (q) (evalo '(and 5 6) q))
        '((6)))

(test "and-4"
  (run* (q) (evalo '(and #f 6) q))
  '((#f)))

(test "and-5"
  (run* (q) (evalo '(and (null? '()) 6) q))
  '((6)))

(test "and-6"
  (run* (q) (evalo '(and (null? '(a b c)) 6) q))
  '((#f)))


;; or tests
(test "or-0"
  (run* (q) (evalo '(or) q))
  '((#f)))

(test "or-1"
  (run* (q) (evalo '(or 5) q))
  '((5)))

(test "or-2"
  (run* (q) (evalo '(or #f) q))
  '((#f)))

(test "or-3"
  (run* (q) (evalo '(or 5 6) q))
  '((5)))

(test "or-4"
  (run* (q) (evalo '(or #f 6) q))
  '((6)))

(test "or-5"
  (run* (q) (evalo '(or (null? '()) 6) q))
  '((#t)))

(test "or-6"
  (run* (q) (evalo '(or (null? '(a b c)) 6) q))
  '((6)))


  (test "proof-1"
    (run* (q)
      (evalo
       `(letrec ((member? (lambda (x ls)
                            (if (null? ls)
                                #f
                                (if (equal? (car ls) x)
                                    #t
                                    (member? x (cdr ls)))))))
          (letrec ((proof? (lambda (proof)
                             (match proof
                               [`(assumption ,assms () ,A)
                                (member? A assms)]
                               [`(modus-ponens
                                  ,assms
                                  ((,r1 ,assms ,ants1 (if ,A ,B))
                                   (,r2 ,assms ,ants2 ,A))
                                  ,B)
                                (and (proof? (list r1 assms ants1 (list 'if A B)))
                                     (proof? (list r2 assms ants2 A)))]))))
            (proof? '(modus-ponens
                      (A (if A B) (if B C))
                      ((assumption (A (if A B) (if B C)) () (if B C))
                       (modus-ponens
                        (A (if A B) (if B C))
                        ((assumption (A (if A B) (if B C)) () (if A B))
                         (assumption (A (if A B) (if B C)) () A)) B))
                      C))))
       q))
    '((#t)))


  (test "proof-2b"
    (run* (prf)
      (fresh (rule assms ants)
        (== `(,rule ,assms ,ants C) prf)
        (== `(A (if A B) (if B C)) assms)
        (== '(modus-ponens
              (A (if A B) (if B C))
              ((assumption (A (if A B) (if B C)) () (if B C))
               (modus-ponens
                (A (if A B) (if B C))
                ((assumption (A (if A B) (if B C)) () (if A B))
                 (assumption (A (if A B) (if B C)) () A)) B))
              C)
            prf)
        (evalo
         `(letrec ((member? (lambda (x ls)
                              (if (null? ls)
                                  #f
                                  (if (equal? (car ls) x)
                                      #t
                                      (member? x (cdr ls)))))))
            (letrec ((proof? (lambda (proof)
                               (match proof
                                 [`(assumption ,assms () ,A)
                                  (member? A assms)]
                                 [`(modus-ponens
                                    ,assms
                                    ((,r1 ,assms ,ants1 (if ,A ,B))
                                     (,r2 ,assms ,ants2 ,A))
                                    ,B)
                                  (and (proof? (list r1 assms ants1 (list 'if A B)))
                                       (proof? (list r2 assms ants2 A)))]))))
              (proof? ',prf)))
         #t)))
    '(((modus-ponens (A (if A B) (if B C))
                     ((assumption (A (if A B) (if B C)) () (if B C))
                      (modus-ponens (A (if A B) (if B C))
                                    ((assumption (A (if A B) (if B C)) () (if A B))
                                     (assumption (A (if A B) (if B C)) () A))
                                    B))
                     C))))


  (test "proof-2c"
    (run 1 (prf)
      (fresh (rule assms ants)
        ;; We want to prove that C holds...
        (== `(,rule ,assms ,ants C) prf)
        ;; ...given the assumptions A, A => B, and B => C.
        (== `(A (if A B) (if B C)) assms)
        (evalo
         `(letrec ((member? (lambda (x ls)
                              (if (null? ls)
                                  #f
                                  (if (equal? (car ls) x)
                                      #t
                                      (member? x (cdr ls)))))))
            (letrec ((proof? (lambda (proof)
                               (match proof
                                 [`(assumption ,assms () ,A)
                                  (member? A assms)]
                                 [`(modus-ponens
                                    ,assms
                                    ((,r1 ,assms ,ants1 (if ,A ,B))
                                     (,r2 ,assms ,ants2 ,A))
                                    ,B)
                                  (and (proof? (list r1 assms ants1 (list 'if A B)))
                                       (proof? (list r2 assms ants2 A)))]))))
              (proof? ',prf)))
         #t)))
    '(((modus-ponens (A (if A B) (if B C))
                     ((assumption (A (if A B) (if B C)) () (if B C))
                      (modus-ponens (A (if A B) (if B C))
                                    ((assumption (A (if A B) (if B C)) () (if A B))
                                     (assumption (A (if A B) (if B C)) () A))
                                    B))
                     C))))


  (test "generate-theorems/proofs"
    (length (run 20 (prf)
              (evalo
               `(letrec ((member? (lambda (x ls)
                                    (if (null? ls)
                                        #f
                                        (if (equal? (car ls) x)
                                            #t
                                            (member? x (cdr ls)))))))
                  (letrec ((proof? (lambda (proof)
                                     (match proof
                                       [`(assumption ,assms () ,A)
                                        (member? A assms)]
                                       [`(modus-ponens
                                          ,assms
                                          ((,r1 ,assms ,ants1 (if ,A ,B))
                                           (,r2 ,assms ,ants2 ,A))
                                          ,B)
                                        (and (proof? (list r1 assms ants1 (list 'if A B)))
                                             (proof? (list r2 assms ants2 A)))]))))
                    (proof? ',prf)))
               #t)))
    '20)


  (test "generate-theorems/proofs-using-modus-ponens"
    (length (run 20 (prf)
              (fresh (assms ants conseq)
                (== `(modus-ponens ,assms ,ants ,conseq) prf)
                (evalo
                 `(letrec ((member? (lambda (x ls)
                                      (if (null? ls)
                                          #f
                                          (if (equal? (car ls) x)
                                              #t
                                              (member? x (cdr ls)))))))
                    (letrec ((proof? (lambda (proof)
                                       (match proof
                                         [`(assumption ,assms () ,A)
                                          (member? A assms)]
                                         [`(modus-ponens
                                            ,assms
                                            ((,r1 ,assms ,ants1 (if ,A ,B))
                                             (,r2 ,assms ,ants2 ,A))
                                            ,B)
                                          (and (proof? (list r1 assms ants1 (list 'if A B)))
                                               (proof? (list r2 assms ants2 A)))]))))
                      (proof? ',prf)))
                 #t))))
    '20)


  (test "generate-non-theorems/proofs"
    (length (run 20 (prf)
              (evalo
               `(letrec ((member? (lambda (x ls)
                                    (if (null? ls)
                                        #f
                                        (if (equal? (car ls) x)
                                            #t
                                            (member? x (cdr ls)))))))
                  (letrec ((proof? (lambda (proof)
                                     (match proof
                                       [`(assumption ,assms () ,A)
                                        (member? A assms)]
                                       [`(modus-ponens
                                          ,assms
                                          ((,r1 ,assms ,ants1 (if ,A ,B))
                                           (,r2 ,assms ,ants2 ,A))
                                          ,B)
                                        (and (proof? (list r1 assms ants1 (list 'if A B)))
                                             (proof? (list r2 assms ants2 A)))]))))
                    (proof? ',prf)))
               #f)))
    '20)



;; unit tests

; or
; num
; symbol?
; not

  (test "stupor-test-1"
    (run* (q)
      (evalo '(letrec ((assoc (lambda (x l)
                                 (if (null? l)
                                     0
                                     (if (or (not (symbol? x))
                                             (not (equal? (car (car l)) x)))
                                         (assoc x (cdr l))
                                         (car l))))))
                (assoc 'foo '((bar . 1) (baz . 2) (42 . ignore) (foo . 3) (quux . 4))))
             q))
    '(((foo . 3))))


;; append

  (test "append-simple-1"
    (run* (q)
      (evalo '(letrec ((append (lambda (l s)
                                 (if (null? l)
                                     s
                                     (cons (car l)
                                           (append (cdr l) s))))))
                (append '(1 2 3) '(4 5)))
             q))
    '(((1 2 3 4 5))))


  (test "append-simple-2"
    (run* (q)
      (evalo `(letrec ((append (lambda (l s)
                                 (if (null? l)
                                     s
                                     (cons (car l)
                                           (append (cdr l) s))))))
                (append ',q '(4 5)))
             '(1 2 3 4 5)))
    '(((1 2 3))))


  (test "append-simple-3"
    (run* (x y)
      (evalo `(letrec ((append (lambda (l s)
                                 (if (null? l)
                                     s
                                     (cons (car l)
                                           (append (cdr l) s))))))
                (append ',x ',y))
             '(1 2 3 4 5)))
    '(((() (1 2 3 4 5)))
      (((1) (2 3 4 5)))
      (((1 2) (3 4 5)))
      (((1 2 3) (4 5)))
      (((1 2 3 4) (5)))
      (((1 2 3 4 5) ()))))

;; Tests from 2017 pearl


  (test "proof-backwards-explicit-member?"
    (run 1 (prf)
      (fresh (rule assms ants)
        ;; We want to prove that C holds...
        (== `(,rule ,assms ,ants C) prf)
        ;; ...given the assumptions A, A => B, and B => C.
        (== `(A (if A B) (if B C)) assms)
        (evalo
         `(letrec ((member? (lambda (x ls)
                              (if (null? ls)
                                  #f
                                  (if (equal? (car ls) x)
                                      #t
                                      (member? x (cdr ls)))))))
            (letrec ((proof? (lambda (proof)
                               (match proof
                                 [`(assumption ,assms () ,A)
                                  (member? A assms)]
                                 [`(modus-ponens
                                    ,assms
                                    ((,r1 ,assms ,ants1 (if ,A ,B))
                                     (,r2 ,assms ,ants2 ,A))
                                    ,B)
                                  (and (proof? (list r1 assms ants1 (list 'if A B)))
                                       (proof? (list r2 assms ants2 A)))]))))
              (proof? ',prf)))
         #t)))
    '(((modus-ponens (A (if A B) (if B C))
                     ((assumption (A (if A B) (if B C)) () (if B C))
                      (modus-ponens (A (if A B) (if B C))
                                    ((assumption (A (if A B) (if B C)) () (if A B))
                                     (assumption (A (if A B) (if B C)) () A))
                                    B))
                     C))))


(test "eval-expr-1"
  (run 1 (defn)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7"))
          (g8 (gensym "g8"))
          (g9 (gensym "g9"))
          (g10 (gensym "g10"))
          (g11 (gensym "g11")))
      (fresh (A)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)
        (absento g8 defn)
        (absento g9 defn)
        (absento g10 defn)
        (absento g11 defn)
        (== `(lambda (expr env)
               (match expr
                 [`(quote ,datum) datum]
                 [`(lambda (,(? symbol? x)) ,body)
                  (lambda (z)
                    (eval-expr body (lambda (y)
                                      (if (equal? x y)
                                          z
                                          (env y)))))]
                 [(? symbol? x) ,A]
                 [`(cons ,e1 ,e2) (cons (eval-expr e1 env) (eval-expr e2 env))]
                 [`(,rator ,rand) ((eval-expr rator env) (eval-expr rand env))]))
            defn)
        (evalo
         `(letrec ((eval-expr ,defn))
            (list
              (eval-expr '(quote ,g1) 'initial-env)
              (eval-expr '((lambda (x) x) (quote ,g2)) 'initial-env)))
         `(,g1 ,g2)))))
  '(((lambda (expr env)
       (match expr
         [`(quote ,datum) datum]
         [`(lambda (,(? symbol? x)) ,body)
          (lambda (z)
            (eval-expr body (lambda (y)
                              (if (equal? x y)
                                  z
                                  (env y)))))]
         [(? symbol? x) (env x)]
         [`(cons ,e1 ,e2) (cons (eval-expr e1 env) (eval-expr e2 env))]
         [`(,rator ,rand) ((eval-expr rator env) (eval-expr rand env))])))))

  (test "check quine"
    (run 1 (q)
      (== q '((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x))))
      (evalo
       `(letrec ((eval-quasi (lambda (q eval)
                               (match q
                                 [(? symbol? x) x]
                                 [`() '()]
                                 [`(,`unquote ,exp) (eval exp)]
                                 [`(quasiquote ,datum) ('error)]
                                 [`(,a . ,d)
                                  (cons (eval-quasi a eval) (eval-quasi d eval))]))))
          (letrec ((eval-expr
                    (lambda (expr env)
                      (match expr
                        [`(quote ,datum) datum]
                        [`(lambda (,(? symbol? x)) ,body)
                         (lambda (a)
                           (eval-expr body (lambda (y)
                                             (if (equal? x y)
                                                 a
                                                 (env y)))))]
                        [(? symbol? x) (env x)]
                        [`(quasiquote ,datum)
                         (eval-quasi datum (lambda (exp) (eval-expr exp env)))]
                        [`(,rator ,rand)
                         ((eval-expr rator env) (eval-expr rand env))]
                        ))))
            (eval-expr ',q
                       'initial-env)))
       q))
    (list '(((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x))))))



(test "quine-a"
  (run 1 (e)
    (evalo e e))
  '((_.0 (num _.0))))

 (test "quine-b"
  (run 4 (e)
     (evalo e e))
   '((_.0 (num _.0)) (#t) (#f) (((lambda (_.0) (list _.0 (list 'quote _.0))) '(lambda (_.0) (list _.0 (list 'quote _.0))))
      (=/= ((_.0 closure)) ((_.0 list)) ((_.0 prim)) ((_.0 quote)))
      (sym _.0))))

(test "quine-c"
  (run 1 (e)
    (fresh (a d)
      (== '((lambda (x) (list x (list 'quote x))) '(lambda (x) (list x (list 'quote x)))) e)
      (== `(,a . ,d) e))
    (evalo e e))
  '((((lambda (x) (list x (list 'quote x))) '(lambda (x) (list x (list 'quote x)))))))

(test "quine-d"
  (run 1 (e)
    (fresh (a d)
      (== `(,a . ,d) e))
    (evalo e e))
  '((((lambda (_.0) (list _.0 (list 'quote _.0)))
      '(lambda (_.0) (list _.0 (list 'quote _.0))))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 prim)) ((_.0 quote)))
     (sym _.0))))

(test "quine-e"
   (run 1 (e v)
     (fresh (a d)
       (== `(,a . ,d) e))
     (evalo e v)
     (== e v))
   '(((((lambda (_.0) (list _.0 (list 'quote _.0))) '(lambda (_.0) (list _.0 (list 'quote _.0))))
       ((lambda (_.0) (list _.0 (list 'quote _.0))) '(lambda (_.0) (list _.0 (list 'quote _.0)))))
      (=/= ((_.0 closure)) ((_.0 list)) ((_.0 prim)) ((_.0 quote))) (sym _.0))))

(test "twines-a"
   (run 1 (p q)
     (=/= p q)
     (evalo p q)
     (evalo q p))
   '((('((lambda (_.0) (list 'quote (list _.0 (list 'quote _.0)))) '(lambda (_.0) (list 'quote (list _.0 (list 'quote _.0))))) ((lambda (_.0) (list 'quote (list _.0 (list 'quote _.0)))) '(lambda (_.0) (list 'quote (list _.0 (list 'quote _.0)))))) (=/= ((_.0 closure)) ((_.0 list)) ((_.0 prim)) ((_.0 quote))) (sym _.0))))

(test "thrine-a"
   (run 1 (p q r)
     (=/= p q)
     (=/= p r)
     (=/= q r)
     (evalo p q)
     (evalo q r)
     (evalo r p))
   '???)

 )

(exit)
