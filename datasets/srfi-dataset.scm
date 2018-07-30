
(define (xcons d a) (cons a d))

(define (list . ans) ans)

(define (proper-list? x)
  (let lp ((x x) (lag x))
    (if (pair? x)
	(let ((x (cdr x)))
	  (if (pair? x)
	      (let ((x   (cdr x))
		    (lag (cdr lag)))
		(and (not (eq? x lag)) (lp x lag)))
	      (null? x)))
	(null? x))))

(define (dotted-list? x)
  (let lp ((x x) (lag x))
    (if (pair? x)
	(let ((x (cdr x)))
	  (if (pair? x)
	      (let ((x   (cdr x))
		    (lag (cdr lag)))
		(and (not (eq? x lag)) (lp x lag)))
	      (not (null? x))))
	(not (null? x)))))

(define (circular-list? x)
  (let lp ((x x) (lag x))
    (and (pair? x)
	 (let ((x (cdr x)))
	   (and (pair? x)
		(let ((x   (cdr x))
		      (lag (cdr lag)))
		  (or (eq? x lag) (lp x lag))))))))

(define (not-pair? x) (not (pair? x)))

(define (null-list? l)
  (cond ((pair? l) #f)
	((null? l) #t)))

(define (list= = . lists)
  (or (null? lists) ; special case

      (let lp1 ((list-a (car lists)) (others (cdr lists)))
	(or (null? others)
	    (let ((list-b (car others))
		  (others (cdr others)))
	      (if (eq? list-a list-b)	; EQ? => LIST=
		  (lp1 list-b others)
		  (let lp2 ((pair-a list-a) (pair-b list-b))
		    (if (null-list? pair-a)
			(and (null-list? pair-b)
			     (lp1 list-b others))
			(and (not (null-list? pair-b))
			     (= (car pair-a) (car pair-b))
			     (lp2 (cdr pair-a) (cdr pair-b)))))))))))

(define (length+ x)			; Returns #f if X is circular.
  (let lp ((x x) (lag x) (len 0))
    (if (pair? x)
	(let ((x (cdr x))
	      (len (+ len 1)))
	  (if (pair? x)
	      (let ((x   (cdr x))
		    (lag (cdr lag))
		    (len (+ len 1)))
		(and (not (eq? x lag)) (lp x lag len)))
	      len))
	len)))

(define (zip list1 . more-lists) (apply map list list1 more-lists))

(define (caar   x) (car (car x)))

(define (cadr   x) (car (cdr x)))

(define (cdar   x) (cdr (car x)))

(define (cddr   x) (cdr (cdr x)))

(define (caaar  x) (caar (car x)))

(define (caadr  x) (caar (cdr x)))

(define (cadar  x) (cadr (car x)))

(define (caddr  x) (cadr (cdr x)))

(define (cdaar  x) (cdar (car x)))

(define (cdadr  x) (cdar (cdr x)))

(define (cddar  x) (cddr (car x)))

(define (cdddr  x) (cddr (cdr x)))

(define (caaaar x) (caaar (car x)))

(define (caaadr x) (caaar (cdr x)))

(define (caadar x) (caadr (car x)))

(define (caaddr x) (caadr (cdr x)))

(define (cadaar x) (cadar (car x)))

(define (cadadr x) (cadar (cdr x)))

(define (caddar x) (caddr (car x)))

(define (cadddr x) (caddr (cdr x)))

(define (cdaaar x) (cdaar (car x)))

(define (cdaadr x) (cdaar (cdr x)))

(define (cdadar x) (cdadr (car x)))

(define (cdaddr x) (cdadr (cdr x)))

(define (cddaar x) (cddar (car x)))

(define (cddadr x) (cddar (cdr x)))

(define (cdddar x) (cdddr (car x)))

(define (cddddr x) (cdddr (cdr x)))

(define first  car)

(define second cadr)

(define third  caddr)

(define fourth cadddr)

(define (fifth   x) (car    (cddddr x)))

(define (sixth   x) (cadr   (cddddr x)))

(define (seventh x) (caddr  (cddddr x)))

(define (eighth  x) (cadddr (cddddr x)))

(define (ninth   x) (car  (cddddr (cddddr x))))

(define (tenth   x) (cadr (cddddr (cddddr x))))

(define (car+cdr pair) (values (car pair) (cdr pair)))

(define (last lis) (car (last-pair lis)))

(define (last-pair lis)
  (let lp ((lis lis))
    (let ((tail (cdr lis)))
      (if (pair? tail) (lp tail) lis))))

(define (unzip1 lis) (map car lis))

(define (reduce f ridentity lis)
  (if (null-list? lis) ridentity
      (fold f (car lis) (cdr lis))))

(define (append-map f lis1 . lists)
  (really-append-map append-map  append  f lis1 lists))

(define (append-map! f lis1 . lists)
  (really-append-map append-map! append! f lis1 lists))

(define (delete x lis . maybe-=)
  (let ((= (:optional maybe-= equal?)))
    (filter (lambda (y) (not (= x y))) lis)))

(define (member x lis . maybe-=)
  (let ((= (:optional maybe-= equal?)))
    (find-tail (lambda (y) (= x y)) lis)))

(define (assoc x lis . maybe-=)
  (let ((= (:optional maybe-= equal?)))
    (find (lambda (entry) (= x (car entry))) lis)))

(define (alist-cons key datum alist) (cons (cons key datum) alist))

(define (alist-copy alist)
  (map (lambda (elt) (cons (car elt) (cdr elt)))
       alist))

(define (alist-delete key alist . maybe-=)
  (let ((= (:optional maybe-= equal?)))
    (filter (lambda (elt) (not (= key (car elt)))) alist)))

(define (find pred list)
  (cond ((find-tail pred list) => car)
	(else #f)))

(define (break  pred lis) (span  (lambda (x) (not (pred x))) lis))

(define (lset-intersection = lis1 . lists)
  (let ((lists (delete lis1 lists eq?))) ; Throw out any LIS1 vals.
    (cond ((any null-list? lists) '())		; Short cut
	  ((null? lists)          lis1)		; Short cut
	  (else (filter (lambda (x)
			  (every (lambda (lis) (member x lis =)) lists))
			lis1)))))

(define (lset-difference = lis1 . lists)
  (let ((lists (filter pair? lists)))	; Throw out empty lists.
    (cond ((null? lists)     lis1)	; Short cut
	  ((memq lis1 lists) '())	; Short cut
	  (else (filter (lambda (x)
			  (every (lambda (lis) (not (member x lis =)))
				 lists))
			lis1)))))

(define (lset-xor = . lists)
  (reduce (lambda (b a)			; Compute A xor B:
	    ;; Note that this code relies on the constant-time
	    ;; short-cuts provided by LSET-DIFF+INTERSECTION,
	    ;; LSET-DIFFERENCE & APPEND to provide constant-time short
	    ;; cuts for the cases A = (), B = (), and A eq? B. It takes
	    ;; a careful case analysis to see it, but it's carefully
	    ;; built in.

	    ;; Compute a-b and a^b, then compute b-(a^b) and
	    ;; cons it onto the front of a-b.
	    (receive (a-b a-int-b)   (lset-diff+intersection = a b)
	      (cond ((null? a-b)     (lset-difference = b a))
		    ((null? a-int-b) (append b a))
		    (else (fold (lambda (xb ans)
				  (if (member xb a-int-b =) ans (cons xb ans)))
				a-b
				b)))))
	  '() lists))

(define (look-up key alist)
  (and-let* ((x (assq key alist))) (cdr x)))

(define (lset-adjoin = lis . elts)
  (foldl (lambda (elt ans) (if (mem = elt ans) ans (cons elt ans)))
	 lis elts))

(define (lset-union = . lists)
  (if (pair? lists)
      (foldl (lambda (lis ans)	; Compute LIS + ANS.
	       (if (pair? ans)
		   (foldl (lambda (elt ans) (if (mem = elt ans) ans
						(cons elt ans)))
			  ans lis)
		   lis))	; Don't copy LIS if you don't have to.
	     (car lists)
	     (cdr lists))
      '()))

(define (lset-intersection = lis1 . lists)
  (if (every pair? lists)
      (foldl (lambda (lis residue) (filter (lambda (x) (mem = x lis)) residue))
	     lis1
	     lists)
      '())) ; Don't do unnecessary scans of LIS1/RESIDUE.

(define (lset-difference = lis1 . lists)
  (foldl (lambda (lis residue)
	   (if (pair? lis)
	       (filter (lambda (x) (not (mem = x lis))) residue)
	       residue)) ; Don't do unnecessary scans of RESIDUE.
	 lis1
	 lists))

(define (lset-xor = . lists)
  (if (pair? lists)	; We don't really have to do this test, but WTF.
      (foldl (lambda (a b)			; Compute A xor B:
	       (if (pair? a)
		   ;; Compute a-b and a^b, then compute b-(a^b) and
		   ;; cons it onto the front of a-b.
		   (receive (a-b a-int-b) (lset-diff+intersection = a b)
		     (foldl (lambda (xb ans)
			      (if (mem = xb a-int-b) ans (cons xb ans)))
			    a-b
			    b))
		   b)) ; Don't copy B if we don't have to.
	     (car lists)
	     (cdr lists)) 
      '()))

(define (adjoin  list . elts) (apply lset-adjoin equal? list elts))

(define (adjoinq list . elts) (apply lset-adjoin eq?    list elts))

(define (adjoinv list . elts) (apply lset-adjoin eqv?   list elts))

(define (union  . lists) (apply lset-union equal? lists))

(define (unionq . lists) (apply lset-union eq?    lists))

(define (unionv . lists) (apply lset-union eqv?   lists))

(define (union!  . lists) (apply lset-union! equal? lists))

(define (unionq! . lists) (apply lset-union! eq?    lists))

(define (unionv! . lists) (apply lset-union! eqv?   lists))

(define (intersection  lis1 . lists) (apply lset-intersection equal? lis1 lists))

(define (intersectionq lis1 . lists) (apply lset-intersection eq?    lis1 lists))

(define (intersectionv lis1 . lists) (apply lset-intersection eqv?   lis1 lists))
	 
(define (intersection!  lis1 . lists) (apply lset-intersection! equal? lis1 lists))

(define (intersectionq! lis1 . lists) (apply lset-intersection! eq?    lis1 lists))

(define (intersectionv! lis1 . lists) (apply lset-intersection! eqv?   lis1 lists))

(define (list-difference  lis1 . lists) (apply lset-difference equal? lis1 lists))

(define (list-differenceq lis1 . lists) (apply lset-difference eq?    lis1 lists))

(define (list-differencev lis1 . lists) (apply lset-difference eqv?   lis1 lists))

(define (list-difference!  lis1 . lists) (apply lset-difference! equal? lis1 lists))

(define (list-differenceq! lis1 . lists) (apply lset-difference! eq?    lis1 lists))

(define (list-differencev! lis1 . lists) (apply lset-difference! eqv?   lis1 lists))

(define (list-xor  . lists) (apply lset-xor equal? lists))

(define (list-xorq . lists) (apply lset-xor eq?    lists))

(define (list-xorv . lists) (apply lset-xor eqv?   lists))

(define (list-xor!  . lists) (apply lset-xor! equal? lists))

(define (list-xorq! . lists) (apply lset-xor! eq?    lists))

(define (list-xorv! . lists) (apply lset-xor! eqv?   lists))

(define (diff+intersection  lis1 . lists)
  (apply lset-diff+intersection equal? lis1 lists))

(define (diff+intersectionq lis1 . lists)
  (apply lset-diff+intersection eq?    lis1 lists))

(define (diff+intersectionv lis1 . lists)
  (apply lset-diff+intersection eqv?   lis1 lists))
	 
(define (diff+intersection!  lis1 . lists)
  (apply lset-diff+intersection! equal? lis1 lists))

(define (diff+intersectionq! lis1 . lists)
  (apply lset-diff+intersection! eq?    lis1 lists))

(define (diff+intersectionv! lis1 . lists)
  (apply lset-diff+intersection! eqv?   lis1 lists))

(define (my-vector? x)
  (and (ur-vector? x)
       (not (input-string? x))
       (not (output-string? x))))

(define (input-string? x)
  (and (ur-vector? x)
       (positive? (ur-vector-length x))
       (eq? input-string-tag (ur-vector-ref x 0))))

(define (output-string? x)
  (and (ur-vector? x)
       (positive? (ur-vector-length x))
       (eq? output-string-tag (ur-vector-ref x 0))))

(define (process-cond-clauses clauses)
  (cond ((null? clauses)
       (exit #f))
      ((or (and (eq? (caar clauses) 'else)
		(null? (cdr clauses)))
	   (satisfied? (caar clauses)))
       (process-clauses (cdar clauses)))
      (else
       (process-cond-clauses (cdr clauses)))))

(define (satisfied? requirement)
  (if (pair? requirement)
    (case (car requirement)
      ((and)
       (all-satisfied? (cdr requirement)))
      ((or)
       (any-satisfied? (cdr requirement)))
      ((not)
       (not (satisfied? (cadr requirement)))))
    (memq requirement features)))


(define (all-satisfied? list)
  (if (null? list)
    #t
    (and (satisfied? (car list))
	 (all-satisfied? (cdr list)))))

(define (any-satisfied? list)
  (if (null? list)
    #f
    (or (satisfied? (car list))
	(any-satisfied? (cdr list)))))

(define (eval-feature-req? feature-req)

  (define (eval-and-clause? req-list)
    (or (null? req-list)
      (and (eval-feature-req? (car req-list))
	(eval-and-clause? (cdr req-list)))))

  (define (eval-or-clause? req-list)
    (and (not (null? req-list))
      (or (eval-feature-req? (car req-list))
	(eval-or-clause? (cdr req-list)))))

  (define (eval-not-clause? req)
    (not (eval-feature-req? req)))

  (cond
    ((not (pair? feature-req)) (feature-present? feature-req))
    ((eq? 'and (car feature-req)) (eval-and-clause? (cdr feature-req)))
    ((eq? 'or (car feature-req)) (eval-or-clause? (cdr feature-req)))
    ((eq? 'not (car feature-req)) (apply eval-not-clause? (cdr feature-req)))))

(define (time-difference time1 time2)
  (tm:time-difference time1 time2 (make-time #f #f #f)))

(define (add-duration time1 duration)
  (tm:add-duration time1 duration (make-time (time-type time1) #f #f)))

(define (subtract-duration time1 duration)
  (tm:subtract-duration time1 duration (make-time (time-type time1) #f #f)))

(define (tm:fractional-part r)
  (if (integer? r) "0"
      (let ((str (number->string (exact->inexact r))))
	(let ((ppos (tm:char-pos #\. str 0 (string-length str))))
	  (substring str  (+ ppos 1) (string-length str))))))

(define (tm:encode-julian-day-number day month year)
  (let* ((a (quotient (- 14 month) 12))
	 (y (- (- (+ year 4800) a) (if (negative? year) -1 0)))
	 (m (- (+ month (* 12 a)) 3)))
    (+ day
       (quotient (+ (* 153 m) 2) 5)
       (* 365 y)
       (quotient y 4)
       (- (quotient y 100))
       (quotient y 400)
       -32045)))

(define (tm:char-pos char str index len)
  (cond
   ((>= index len) #f)
   ((char=? (string-ref str index) char)
    index)
   (else
    (tm:char-pos char str (+ index 1) len))))

(define (array-length arr dim)
  (- (array-end arr dim)
     (array-start arr dim)))

(define (shape-for-each shp proc . o)
  (if (null? o)
      (array:arlib:shape-for-each/arguments shp proc)
      (if (vector? (car o))
          (array:arlib:shape-for-each/vector shp proc (car o))
          (array:arlib:shape-for-each/array shp proc (car o)))))

(define (array-for-each-index arr proc . o)
  (if (null? o)
      (array:arlib:array-for-each-index/arguments arr proc)
      (if (vector? (car o))
          (array:arlib:array-for-each-index/vector arr proc (car o))
          (array:arlib:array-for-each-index/array arr proc (car o)))))

(define (sub-dup)
  (let ((c (read-char)))
    (if (not (eof-object? c))
        ((char->dupper c) c))))

(define *read-alist* '())

(define (char->dupper char)
  (cond ((assq char *read-alist*)
	 => cdr)
	(else
         write-char)))

(define *read-terminating?-alist* '())

(define (is-char-terminating? char)
  (cond ((assq char *read-terminating?-alist*)
	 => cdr)
	(else #t)))

(define whitespace-chars
  (cons #\space 
	(cons #\newline
	      (map (lambda (s) (string-ref s 0))
		   '("	" "" ""))))) ;tab page return

