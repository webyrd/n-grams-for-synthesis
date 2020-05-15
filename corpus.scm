;; Scheme expressions used in creating n-grams

;; Basic recursive functions over flat and nested lists, a few
;; recursive functions over numbers, and a few more sophisticated
;; programs using 'letrec' and patter matching.  Some examples adapted
;; from The Little Schemer and The Seasoned Schemer, and others from
;; 'A Unified Approach to Solving Seven Programming Problems
;; (Functional Pearl)' (ICFP 2017).  Hopefully these examples are
;; similar in spirit to recursive functions that a user may wish to
;; synthesize.

;; We need more definitions!!  Perhaps use examples from Essentials of
;; Programming Languages, Scheme and the Art of Programming,
;; miniKanren/microKanren, etc.

;; To better reflect the code handled by relational Scheme
;; interpreters:
;;
;; * 'cond' expressions have been transformed into nested 'if'
;; expressions [some interpreters can handle 'cond', but 'if' seems
;; simpler from the n-grams perspective]
;;
;; * 'eq?' and 'eqv?' have been transformed into 'equal?'
;;
;; * some calls to 'atom?', 'add1', etc., have been left in---any
;; calls to primitives not explicity handled in the 'n-grams.scm'
;; 'bigrams-for-expr' function will be counted as "generic" procedure
;; applications

(define exprs
  '(

    ;;; Adapted from subset of functions from SRFIs

    (define proper-list?
      (lambda (x lag)
        (if (pair? x)
            (let ((x (cdr x)))
              (if (pair? x)
                  (let ((x   (cdr x))
                        (lag (cdr lag)))
                    (and (not (equal? x lag)) (proper-list? x lag)))
                  (null? x)))
            (null? x))))

    (define dotted-list?-aux
      (lambda (x lag)
        (if (pair? x)
            (let ((x (cdr x)))
              (if (pair? x)
                  (let ((x   (cdr x))
                        (lag (cdr lag)))
                    (and (not (equal? x lag)) (dotted-list?-aux x lag)))
                  (not (null? x))))
            (not (null? x)))))

    (define circular-list?-aux
      (lambda (x lag)
        (and (pair? x)
             (let ((x (cdr x)))
               (and (pair? x)
                    (let ((x   (cdr x))
                          (lag (cdr lag)))
                      (or (equal? x lag) (circular-list?-aux x lag))))))))

    (define not-pair?
      (lambda (x)
        (not (pair? x))))

    (define null-list?
      (lambda (l)
        (if (pair? l)
            #f
            (if (null? l)
                #t
                (void)))))

    (define list=-aux
      (lambda (list-a others)
        (or (null? others)
            (let ((list-b (car others))
                  (others (cdr others)))
              (if (equal? list-a list-b) ; EQUAL? => LIST=
                  (list=-aux list-b others)
                  )))))

    (define list=-aux-aux
      (lambda (pair-a pair-b)
        (if (null-list? pair-a)
            (and (null-list? pair-b)
                 (list=-aux list-b others))
            (and (not (null-list? pair-b))
                 (= (car pair-a) (car pair-b))
                 (list=-aux-aux (cdr pair-a) (cdr pair-b))))))

    (define length+-aux
      (lambda (x lag len)               ; Returns #f if X is circular.
        (if (pair? x)
            (let ((x (cdr x))
                  (len (+ len 1)))
              (if (pair? x)
                  (let ((x   (cdr x))
                        (lag (cdr lag))
                        (len (+ len 1)))
                    (and (not (equal? x lag)) (length+-aux x lag len)))
                  len))
            len)))

    (define last-pair-aux
      (lambda (lis)
        (let ((tail (cdr lis)))
          (if (pair? tail) (last-pair-aux tail) lis))))

    (define unzip1
      (lambda (lis)
        (map car lis)))

    (define reduce
      (lambda (f ridentity lis)
        (if (null-list? lis) ridentity
            (fold f (car lis) (cdr lis)))))

    (define alist-cons
      (lambda (key datum alist)
        (cons (cons key datum) alist)))

    (define alist-copy
      (lambda (alist)
        (map (lambda (elt) (cons (car elt) (cdr elt)))
             alist)))

    (define find
      (lambda (pred list)
        (let ((v (find-tail pred list)))
          (if v
              (car v)
              #f))))

    (define break
      (lambda (pred lis)
        (span (lambda (x) (not (pred x))) lis)))

    (define lset-intersection
      (lambda (= lis1 lists)
        (let ((lists (delete lis1 lists equal?))) ; Throw out any LIS1 vals.
          (if (any null-list? lists)
              '()
              (if (null? lists)
                  lis1
                  (filter (lambda (x)
                            (every (lambda (lis) (member x lis =)) lists))
                          lis1))))))

    (define lset-difference
      (lambda (= lis1 lists)
        (let ((lists (filter pair? lists))) ; Throw out empty lists.
          (if (null? lists)
              lis1
              (if (memq lis1 lists)
                  '()
                  (filter (lambda (x)
                            (every (lambda (lis) (not (member x lis =)))
                                   lists))
                          lis1))))))

    (define lset-xor
      (lambda (= lists)
        (reduce (lambda (b a)           ; Compute A xor B:
                  ;; Note that this code relies on the constant-time
                  ;; short-cuts provided by LSET-DIFF+INTERSECTION,
                  ;; LSET-DIFFERENCE & APPEND to provide constant-time short
                  ;; cuts for the cases A = (), B = (), and A equal? B. It takes
                  ;; a careful case analysis to see it, but it's carefully
                  ;; built in.

                  ;; Compute a-b and a^b, then compute b-(a^b) and
                  ;; cons it onto the front of a-b.
                  (receive (a-b a-int-b)   (lset-diff+intersection = a b)
                    (if (null? a-b)
                        (lset-difference = b a)
                        (if (null? a-int-b)
                            (append b a)
                            (fold (lambda (xb ans)
                                    (if (member xb a-int-b =) ans (cons xb ans)))
                                  a-b
                                  b)))))
                '() lists)))

    (define look-up
      (lambda (key alist)
        (and-let* ((x (assq key alist))) (cdr x))))

    (define lset-adjoin
      (lambda (= lis elts)
        (foldl (lambda (elt ans) (if (mem = elt ans) ans (cons elt ans)))
               lis elts)))

    (define lset-union
      (lambda (= lists)
        (if (pair? lists)
            (foldl (lambda (lis ans)	; Compute LIS + ANS.
                     (if (pair? ans)
                         (foldl (lambda (elt ans) (if (mem = elt ans) ans
                                                      (cons elt ans)))
                                ans lis)
                         lis))	; Don't copy LIS if you don't have to.
                   (car lists)
                   (cdr lists))
            '())))

    (define lset-intersection
      (lambda (= lis1 lists)
        (if (every pair? lists)
            (foldl (lambda (lis residue) (filter (lambda (x) (mem = x lis)) residue))
                   lis1
                   lists)
            '())))       ; Don't do unnecessary scans of LIS1/RESIDUE.

    (define lset-difference
      (lambda (= lis1 lists)
        (foldl (lambda (lis residue)
                 (if (pair? lis)
                     (filter (lambda (x) (not (mem = x lis))) residue)
                     residue)) ; Don't do unnecessary scans of RESIDUE.
               lis1
               lists)))

    (define lset-xor
      (lambda (= lists)
        (if (pair? lists) ; We don't really have to do this test, but WTF.
            (foldl (lambda (a b)        ; Compute A xor B:
                     (if (pair? a)
                         ;; Compute a-b and a^b, then compute b-(a^b) and
                         ;; cons it onto the front of a-b.
                         (receive (a-b a-int-b) (lset-diff+intersection = a b)
                           (foldl (lambda (xb ans)
                                    (if (mem = xb a-int-b) ans (cons xb ans)))
                                  a-b
                                  b))
                         b))       ; Don't copy B if we don't have to.
                   (car lists)
                   (cdr lists)) 
            '())))

    (define my-vector?
      (lambda (x)
        (and (ur-vector? x)
             (not (input-string? x))
             (not (output-string? x)))))

    (define input-string?
      (lambda (x)
        (and (ur-vector? x)
             (positive? (ur-vector-length x))
             (equal? input-string-tag (ur-vector-ref x 0)))))

    (define output-string?
      (lambda (x)
        (and (ur-vector? x)
             (positive? (ur-vector-length x))
             (equal? output-string-tag (ur-vector-ref x 0)))))

    (define process-cond-clauses
      (lambda (clauses)
        (if (null? clauses)
            (exit #f)
            (if (or (and (equal? (caar clauses) 'else)
                         (null? (cdr clauses)))
                    (satisfied? (caar clauses)))
                (process-clauses (cdar clauses))
                (process-cond-clauses (cdr clauses))))))

    (define satisfied?
      (lambda (requirement)
        (if (pair? requirement)
            (let ((v (car requirement)))
              (if (equal? 'and v)
                  (all-satisfied? (cdr requirement))
                  (if (equal? 'or v)
                      (any-satisfied? (cdr requirement))
                      (if (equal? 'not v)
                          (not (satisfied? (cadr requirement)))
                          (void)))))
            (memq requirement features))))

    (define all-satisfied?
      (lambda (list)
        (if (null? list)
            #t
            (and (satisfied? (car list))
                 (all-satisfied? (cdr list))))))

    (define any-satisfied?
      (lambda (list)
        (if (null? list)
            #f
            (or (satisfied? (car list))
                (any-satisfied? (cdr list))))))

    (define eval-and-clause?
      (lambda (req-list)
        (or (null? req-list)
            (and (eval-feature-requal? (car req-list))
                 (eval-and-clause? (cdr req-list))))))

    (define eval-or-clause?
      (lambda (req-list)
        (and (not (null? req-list))
             (or (eval-feature-requal? (car req-list))
                 (eval-or-clause? (cdr req-list))))))

    (define eval-not-clause?
      (lambda (req)
        (not (eval-feature-requal? req))))

    (define eval-feature-requal?
      (lambda (feature-req)
        (if (not (pair? feature-req))
            (feature-present? feature-req)
            (if (equal? 'and (car feature-req))
                (eval-and-clause? (cdr feature-req))
                (if (equal? 'or (car feature-req))
                    (eval-or-clause? (cdr feature-req))
                    (if (equal? 'not (car feature-req))
                        (apply eval-not-clause? (cdr feature-req))
                        (void)))))))

    (define time-difference
      (lambda (time1 time2)
        (tm:time-difference time1 time2 (make-time #f #f #f))))

    (define add-duration
      (lambda (time1 duration)
        (tm:add-duration time1 duration (make-time (time-type time1) #f #f))))

    (define subtract-duration
      (lambda (time1 duration)
        (tm:subtract-duration time1 duration (make-time (time-type time1) #f #f))))

    (define tm:encode-julian-day-number
      (lambda (day month year)
        (let* ((a (quotient (- 14 month) 12))
               (y (- (- (+ year 4800) a) (if (negative? year) -1 0)))
               (m (- (+ month (* 12 a)) 3)))
          (+ day
             (quotient (+ (* 153 m) 2) 5)
             (* 365 y)
             (quotient y 4)
             (- (quotient y 100))
             (quotient y 400)
             -32045))))

    (define tm:char-pos
      (lambda (char str index len)
        (if (>= index len)
            #f
            (if (char=? (string-ref str index) char)
                index
                (tm:char-pos char str (+ index 1) len)))))
    
    (define array-length
      (lambda (arr dim)
        (- (array-end arr dim)
           (array-start arr dim))))

    (define shape-for-each
      (lambda (shp proc o)
        (if (null? o)
            (array:arlib:shape-for-each/arguments shp proc)
            (if (vector? (car o))
                (array:arlib:shape-for-each/vector shp proc (car o))
                (array:arlib:shape-for-each/array shp proc (car o))))))

    (define array-for-each-index
      (lambda (arr proc o)
        (if (null? o)
            (array:arlib:array-for-each-index/arguments arr proc)
            (if (vector? (car o))
                (array:arlib:array-for-each-index/vector arr proc (car o))
                (array:arlib:array-for-each-index/array arr proc (car o))))))

    (define sub-dup
      (lambda ()
        (let ((c (read-char)))
          (if (not (eof-object? c))
              ((char->dupper c) c)))))

    (define char->dupper
      (lambda (char)
        (let ((v (assq char *read-alist*)))
          (if v
              (cdr v)
              write-char))))

    (define is-char-terminating?
      (lambda (char)
        (let ((v (assq char *read-terminating?-alist*)))
          (if v
              (cdr v)
              #t))))


    ;;;  Simple recursive Scheme functions (Little Schemer Style)

    ;(define foldr
    ;  (lambda (f base ls)
    ;    (if (null? ls)
    ;        base 
    ;        (f (car ls) 
    ;           (foldr f base (cdr ls))))))

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


    ;; Examples taken from 'A Unified Approach to Solving Seven
    ;; Programming Problems (Functional Pearl)', ICFP 2017, by William
    ;; E. Byrd, Michael Ballantyne, Gregory Rosenblatt, and Matthew Might


    (letrec ((member? (lambda (x ls)
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
        (proof? '(modus-ponens (A (if A B) (if B C))
                               ((assumption (A (if A B) (if B C)) () (if B C))
                                (modus-ponens (A (if A B) (if B C))
                                              ((assumption (A (if A B) (if B C)) () (if A B))
                                               (assumption (A (if A B) (if B C)) () A))
                                              B))
                               C))))


    (letrec ((eval-quasi (lambda (q eval)
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
        (eval-expr '((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x)))
                   'initial-env)))


    ))
