;; Scheme expressions used in creating n-grams

(define exprs
  '(

    (define reverse
      (lambda (ls)
        (if (null? ls)
            '()
            (append (reverse (cdr ls)) (list (car ls))))))
    
    (define foldr
      (lambda (f base ls)
        (if (null? ls)
            base 
            (f (car ls) 
               (foldr f base (cdr ls))))))

    (define foldl
      (lambda (f base ls)
        (if (null? ls) 
            base 
            (foldl f
                   (f (car ls) base) 
                   (cdr ls))))) 

    (define assoc
      (lambda (x ls)
        (if (null? ls)
            #f
            (if (equal? (car (car ls)) x)
                (car ls)
                (assoc x (cdr ls))))))

    (define lookup
      (lambda (x ls)
        (if (null? ls)
            #f
            (if (equal? (car (car ls)) x)
                (cdr (car ls))
                (lookup x (cdr ls))))))

    (define append
      (lambda (ls1 ls2)
        (if (null? ls1)
            ls2
            (cons (car ls1) (append (cdr ls1) ls2)))))

    (define map
      (lambda (p ls)
        (if (null? ls)
            '()
            (cons (p (car ls)) (map p (cdr ls))))))

    (define zip
      (lambda (ls1 ls2)
        (if (null? ls1)
            '()
            (cons (cons (car ls1) (list (car ls2)))
                  (zip (cdr ls1) (cdr ls2))))))

    (define filter
      (lambda (pred ls)
        (if (null? ls)
            '()
            (if (pred (car ls))
                (cons (car ls) (filter pred (cdr ls)))
                (filter pred (cdr ls))))))

    (define member?
      (lambda (x ls)
        (if (null? ls)
            #f
            (if (equal? (car ls) x)
                #t
                (member? x (cdr ls))))))

    (define insertL
      (lambda (old new ls)
        (if (null? ls)
            '()
            (if (equal? (car ls) old)
                (cons new (cons old (insertL old new (cdr ls))))
                (cons (car ls) (insertL old new (cdr ls)))))))

    (define remove
      (lambda (sym ls)
        (if (null? ls)
            '()
            (if (equal? (car ls) sym)
                (remove (cdr ls))
                (cons (car ls) (remove (cdr ls)))))))

    ;; little schemer definitions
    (define o+
      (lambda (n m)
        (if (zero? m)
            n
            (add1 (o+ n (sub1 m))))))

    (define addtup
      (lambda (tup)
        (if (null? tup)
            0
            (o+ (car tup) (addtup (cdr tup))))))
    
    (define o*
      (lambda (n m)
        (if (zero? m)
            0
            (o+ n (o* n (sub1 m))))))
    
    (define tup+
      (lambda (tup1 tup2)
        (if (null? tup1)
            tup2
            (if (null? tup2)
                tup1
                (cons (o+ (car tup1) (car tup2))
                      (tup+ (cdr tup1) (cdr tup2)))))))

    (define o<
      (lambda (n m)
        (if (zero? m)
            #f
            (if (zero? n)
                #t
                (o< (sub1 n) (sub1 m))))))
    
    (define o=
      (lambda (n m)
        (if (o> n m)
            #f
            (if (o< n m)
                #f
                #t))))
    
    (define o^
      (lambda (n m)
        (if (zero? m)
            1
            (o* n (o^ n (sub1 m))))))
    
    (define o/
      (lambda (n m)
        (if (o< n m)
            0
            (add1 (o/ (o- n m) m)))))

    (define olength
      (lambda (lat)
        (if (null? lat)
            0
            (add1 (olength (cdr lat))))))
    
    (define pick
      (lambda (n lat)
        (if (zero? (sub1 n))
            (car lat)
            (pick (sub1 n) (cdr lat)))))
    
    (define rempick
      (lambda (n lat)
        (if (zero? (sub1 n))
            (cdr lat)
            (cons (car lat) (rempick (sub1 n) (cdr lat))))))
    
    (define all-nums
      (lambda (lat)
        (if (null? lat)
            '()
            (if (number? (car lat))
                (cons (car lat) (all-nums (cdr lat)))
                (all-nums (cdr lat))))))
    
    (define eqan?
      (lambda (a1 a2)
        (if (and (number? a1) (number? a2))
            (= a1 a2)
            (if (or (number? a1) (number? a2))
                #f
                (equal? a1 a2)))))
    
    (define occur
      (lambda (a lat)
        (if (null? lat)
            0
            (if (equal? (car lat) a)
                (add1 (occur a (cdr lat)))
                (occur a (cdr lat))))))
    
    (define rempick-one
      (lambda (n lat)
        (if (one? n)
            (cdr lat)
            (cons (car lat) (rempick-one (sub1 n) (cdr lat))))))
       
    (define rember*
      (lambda (a l)
        (if (null? l)
            '()
            (if (atom? (car l))
                (if (equal? (car l) a)
                    (rember* a (cdr l))
                    (cons (car l) (rember* a (cdr l)))))
            (cons (rember* a (car l)) (rember* a (cdr l))))))

    (define subst*
      (lambda (new old l)
        (if (null? l)
            '()
            (if (not (pair? (car l)))
                (if (equal? (car l) old)
                    (cons new (subst* new old (cdr l)))
                    (cons (car l) (subst* new old (cdr l))))
                (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

    (define insertL*
      (lambda (new old l)
        (if (null? l)
            '()
            (if (pair? (car l))
                (cons (insertL* new old (car l)) (insertL* new old (cdr l)))
                (if (equal? (car l) old)
                    (cons new (cons old (insertL* new old (cdr l))))
                    (cons (car l) (insertL* new old (cdr l))))))))
    
    (define member*
      (lambda (a l)
        (if (null? l)
            #f
            (if (pair? (car l))
                (or (member* a (car l))
                    (member* a (cdr l)))
                (or (equal? (car l) a)
                    (member* a (cdr l)))))))
    
    (define eqlist2?
      (lambda (l1 l2)
        (if (and (null? l1) (null? l2))
            #t
            (if (or (null? l1) (null? l2))
                #f
                (if (and (atom? (car l1)) (atom? (car l2)))
                    (and (equal? (car l1) (car l2))
                         (eqlist2? (cdr l1) (cdr l2)))
                    (if (or (atom? (car l1)) (atom? (car l2)))
                        #f
                        (and (eqlist2? (car l1) (car l2))
                             (eqlist2? (cdr l1) (cdr l2)))))))))

    (define eqlist?
      (lambda (l1 l2)
        (if (and (null? l1) (null? l2))
            #t
            (if (and (null? l1) (atom? (car l2)))
                #f
                (if (null? l1)
                    #f
                    (if (and (atom? (car l1)) (null? l2))
                        #f
                        (if (and (atom? (car l1)) (atom? (car l2)))
                            (and (equal? (car l1) (car l2))
                                 (eqlist? (cdr l1) (cdr l2)))
                            (if (atom? (car l1))
                                #f
                                (if (null? l2)
                                    #f
                                    (if (atom? (car l2))
                                        #f
                                        (and (eqlist? (car l1) (car l2))
                                             (eqlist? (cdr l1) (cdr l2)))))))))))))
    
    (define eqlist3?
      (lambda (l1 l2)
        (if (and (null? l1) (null? l2))
            #t
            (if (or (null? l1) (null? l2))
                #f
                (and (equal2?? (car l1) (car l2))
                     (equal2?? (cdr l1) (cdr l2)))))))

    
    (define numbered?
      (lambda (aexp)
        (if (atom? aexp)
            (number? aexp)
            (if (equal? (car (cdr aexp)) 'o+)
                (and (numbered? (car aexp))
                     (numbered? (car (cdr (cdr aexp)))))
                (if (equal? (car (cdr aexp)) 'ox)
                    (and (numbered? (car aexp))
                         (numbered? (car (cdr (cdr aexp)))))
                    (if (equal? (car (cdr aexp)) 'o^)
                        (and (numbered? (car aexp))
                             (numbered? (car (cdr (cdr aexp)))))
                        #f))))))
    
    (define set?
      (lambda (lat)
        (if (null? lat)
            #t
            (if (member? (car lat) (cdr lat))
                #f
                (set? (cdr lat))))))

    (define subst-f
      (lambda (new old l)
        (if (null? l)
            '()
            (if (equal? (car l) old)
                (cons new (cdr l))
                (cons (car l) (subst new old (cdr l)))))))
    
    (define multirember
      (lambda (a lat)
        (if (null? lat)
            '()
            (if (equal? (car lat) a)
                (multirember a (cdr lat))
                (cons (car lat) (multirember a (cdr lat)))))))
    
    (define multiremberT
      (lambda (test? lat)
        (if (null? lat)
            '()
            (if (test? (car lat))
                (multiremberT test? (cdr lat))
                (cons (car lat)
                      (multiremberT test? (cdr lat)))))))
    
    (define multiremember&co
      (lambda (a lat col)
        (if (null? lat)
            (col '() '())
            (if (equal? (car lat) a)
                (multiremember&co a (cdr lat)
                                  (lambda (newlat seen)
                                    (col newlat (cons (car lat) seen))))
                (multiremember&co a (cdr lat)
                                  (lambda (newlat seen)
                                    (col (cons (car lat) newlat) seen)))))))

    
    (define multiinsertLR
      (lambda (new oldL oldR lat)
        (if (null? lat)
            '()
            (if (equal? (car lat) oldL)
                (cons new
                      (cons oldL
                            (multiinsertLR new oldL oldR (cdr lat))))
                (if (equal? (car lat) oldR)
                    (cons oldR
                          (cons new
                                (multiinsertLR new oldL oldR (cdr lat))))
                    (cons
                     (car lat)
                     (multiinsertLR new oldL oldR (cdr lat))))))))

    ;; seasoned schemer definitions
    (define union
      (lambda (set1 set2)
        (if (null? set1)
            set2
            (if (member? (car set1) set2) 
                (union (cdr set1) set2)
                (cons (car set1) (union (cdr set1) set2))))))

    (define intersect
      (lambda (set1 set2)
        (if (null? set1)
            '()
            (if (member? (car set1) set2)
                (cons (car set1) (intersect (cdr set1) set2))
                (intersect (cdr set1) set2)))))

    (define intersectall
      (lambda (lset)
        (if (null? lset)
            '()
            (if (null? (cdr lset))
                (car lset)
                (intersect (car lset)
                           (intersectall (cdr lset)))))))

    
    ))
