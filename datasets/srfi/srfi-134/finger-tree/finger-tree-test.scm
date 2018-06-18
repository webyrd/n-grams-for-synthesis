
(import
 (chibi test)
 (finger-tree)
 (generators)
 (scheme base)
 (srfi 1))

(define *exhaustive* #f)

(let* ((add (lambda (left right)
	      right))
       (measure (lambda (element)
		  element))
       (naturals (lambda (n)
		   (list->finger-tree add measure (iota n))))
       
       (t0 (make-finger-tree)) ;; empty case
       (t1 (naturals 1))       ;; single case
       (t2 (naturals 2))       ;; smallest deep case
       (t5 (naturals 5))       ;; deep with one full digit
       (t8 (naturals 8))       ;; deep with both digits full, empty spine
       (t9 (naturals 9))       ;; smallest deep with nonempty spine
       (t33 (naturals 33))     ;; depth 3
       (t999 (naturals 999))   ;; large (ish)

       (seed '(1))
       (proc (lambda (element accum)
	       (cons (remainder (+ (* 7 (car accum))
				   element)
				10)
		     accum))))
       
  ;;; make-finger-tree
  (test #true (finger-tree? t0))
  (test #true (finger-tree-empty? t0))
  (test 0 (finger-tree-length t0))

  ;; finger-tree
  (do ((n 0 (+ 1 n)))
      ((> n 16))
    (test (iota n)
	  (finger-tree->list
	   (apply finger-tree add measure (iota n)))))

  ;;; finger-tree?
  (test #true (finger-tree? t0))
  (test #true (finger-tree? t1))
  (test #true (finger-tree? t2))
  (test #true (finger-tree? t5))
  (test #true (finger-tree? t8))
  (test #true (finger-tree? t9))
  (test #true (finger-tree? t33))
  (test #true (finger-tree? t999))
  (test #false (finger-tree? 42))
  (test #false (finger-tree? (list 42)))

  ;; finger-tree-empty?
  (test #true (finger-tree-empty? t0))
  (test #false (finger-tree-empty? t1))
  (test #false (finger-tree-empty? t2))
  (test #false (finger-tree-empty? t5))
  (test #false (finger-tree-empty? t8))
  (test #false (finger-tree-empty? t9))
  (test #false (finger-tree-empty? t33))
  (test #false (finger-tree-empty? t999))

  ;; finger-tree-length
  (test 0 (finger-tree-length t0))
  (test 1 (finger-tree-length t1))
  (test 2 (finger-tree-length t2))
  (test 5 (finger-tree-length t5))
  (test 8 (finger-tree-length t8))
  (test 9 (finger-tree-length t9))
  (test 33 (finger-tree-length t33))
  (test 999 (finger-tree-length t999))

  ;; finger-tree-left
  (test 0 (finger-tree-left t1))
  (test 0 (finger-tree-left t2))
  (test 0 (finger-tree-left t5))
  (test 0 (finger-tree-left t8))
  (test 0 (finger-tree-left t9))
  (test 0 (finger-tree-left t33))
  (test 0 (finger-tree-left t999))

  ;; finger-tree-right
  (test 0 (finger-tree-right t1))
  (test 1 (finger-tree-right t2))
  (test 4 (finger-tree-right t5))
  (test 7 (finger-tree-right t8))
  (test 8 (finger-tree-right t9))
  (test 32 (finger-tree-right t33))
  (test 998 (finger-tree-right t999))

  ;; finger-tree-add-left
  (define (test-add-left t)
    (test (cons #t (finger-tree->list t))
	  (finger-tree->list (finger-tree-add-left add measure t #t))))
  (test-add-left t0)
  (test-add-left t1)
  (test-add-left t2)
  (test-add-left t5)
  (test-add-left t8)
  (test-add-left t9)
  (test-add-left t33)
  (test-add-left t999)

  ;; finger-tree-test-add-right
  (define (test-add-right t)
    (test (append (finger-tree->list t)
		  (list #t))
	  (finger-tree->list (finger-tree-add-right add measure t #t))))
  (test-add-right t0)
  (test-add-right t1)
  (test-add-right t2)
  (test-add-right t5)
  (test-add-right t8)
  (test-add-right t9)
  (test-add-right t33)
  (test-add-right t999)

  ;; finger-tree-test-remove-left
  (define (test-remove-left t)
    (test (cdr (finger-tree->list t))
	  (finger-tree->list (finger-tree-remove-left t))))
  (test-remove-left t1)
  (test-remove-left t2)
  (test-remove-left t5)
  (test-remove-left t8)
  (test-remove-left t9)
  (test-remove-left t33)
  (test-remove-left t999)

  ;; finger-tree-test-remove-right
  (define (test-remove-right t)
    (test (drop-right (finger-tree->list t) 1)
	  (finger-tree->list (finger-tree-remove-right t))))
  (test-remove-right t1)
  (test-remove-right t2)
  (test-remove-right t5)
  (test-remove-right t8)
  (test-remove-right t9)
  (test-remove-right t33)
  (test-remove-right t999)

  ;; finger-tree-append
  (define (test-append left right)
    (test (append (finger-tree->list left)
		  (finger-tree->list right))
	  (finger-tree->list
	   (finger-tree-append add measure left right))))
  (test-append t0 t0)
  (test-append t0 t1)
  (test-append t0 t2)
  (test-append t0 t5)
  (test-append t0 t8)
  (test-append t0 t9)
  (test-append t0 t33)
  (test-append t0 t999)
  (test-append t1 t0)
  (test-append t1 t1)
  (test-append t1 t2)
  (test-append t1 t5)
  (test-append t1 t8)
  (test-append t1 t9)
  (test-append t1 t33)
  (test-append t1 t999)
  (test-append t2 t0)
  (test-append t2 t1)
  (test-append t2 t2)
  (test-append t2 t5)
  (test-append t2 t8)
  (test-append t2 t9)
  (test-append t2 t33)
  (test-append t2 t999)
  (test-append t5 t0)
  (test-append t5 t1)
  (test-append t5 t2)
  (test-append t5 t5)
  (test-append t5 t8)
  (test-append t5 t9)
  (test-append t5 t33)
  (test-append t5 t999)
  (test-append t8 t0)
  (test-append t8 t1)
  (test-append t8 t2)
  (test-append t8 t5)
  (test-append t8 t8)
  (test-append t8 t9)
  (test-append t8 t33)
  (test-append t8 t999)
  (test-append t9 t0)
  (test-append t9 t1)
  (test-append t9 t2)
  (test-append t9 t5)
  (test-append t9 t8)
  (test-append t9 t9)
  (test-append t9 t33)
  (test-append t9 t999)
  (test-append t33 t0)
  (test-append t33 t1)
  (test-append t33 t2)
  (test-append t33 t5)
  (test-append t33 t8)
  (test-append t33 t9)
  (test-append t33 t33)
  (test-append t33 t999)
  (test-append t999 t0)
  (test-append t999 t1)
  (test-append t999 t2)
  (test-append t999 t5)
  (test-append t999 t8)
  (test-append t999 t9)
  (test-append t999 t33)
  (test-append t999 t999)
  ;; three operands
  (test (append (finger-tree->list t33)
		(finger-tree->list t9)
		(finger-tree->list t999))
	(finger-tree->list
	 (finger-tree-append add measure t33 t9 t999)))
  ;; five operands
  (test (append (finger-tree->list t999)
		(finger-tree->list t33)
		(finger-tree->list t1)
		(finger-tree->list t2)
		(finger-tree->list t999))
	(finger-tree->list
	 (finger-tree-append add measure t999 t33 t1 t2 t999)))

  ;; finger-tree-filter
  (define (test-finger-tree-filter t)
    (test-finger-tree-filter/proc number? t) ; take everything
    (test-finger-tree-filter/proc pair? t)   ; take nothing
    (test-finger-tree-filter/proc even? t)
    (test-finger-tree-filter/proc odd? t))
  (define (test-finger-tree-filter/proc proc t)
    (test (filter proc (finger-tree->list t))
	  (finger-tree->list (finger-tree-filter add measure proc t))))
  (test-finger-tree-filter t0)
  (test-finger-tree-filter t1)
  (test-finger-tree-filter t2)
  (test-finger-tree-filter t5)
  (test-finger-tree-filter t8)
  (test-finger-tree-filter t9)
  (test-finger-tree-filter t33)
  (test-finger-tree-filter t999)

  (define (test-folds t)
    ;; finger-tree-fold-left
    (test (fold proc seed (finger-tree->list t))
	  (finger-tree-fold-left proc seed t))
    ;; finger-tree-fold-right
    (test (fold-right proc seed (finger-tree->list t))
	  (finger-tree-fold-right proc seed t))
    ;; finger-tree-for-each
    (let ((accum seed))
      (finger-tree-for-each (lambda (element)
			      (set! accum (proc element accum)))
			    t)
      (test accum (fold proc seed (finger-tree->list t)))))
  (test-folds t0)
  (test-folds t1)
  (test-folds t2)
  (test-folds t5)
  (test-folds t8)
  (test-folds t9)
  (test-folds t33)
  (test-folds t999)
  ;; multi-argument
  (test (fold + 0 (finger-tree->list t33) (finger-tree->list t33) (finger-tree->list t9))
	(finger-tree-fold-left + 0 t33 t33 t9))
  (test (fold-right + 0 (finger-tree->list t33) (finger-tree->list t33) (finger-tree->list t9))
	(finger-tree-fold-right + 0 t33 t33 t9))

  (define (test-map t)
    (test (map - (finger-tree->list t))
	  (finger-tree->list
	   (finger-tree-map add measure - t))))
  (test-map t0)
  (test-map t1)
  (test-map t2) 
  (test-map t5)
  (test-map t8)
  (test-map t9)
  (test-map t33)
  (test-map t999)
 
  ;; finger-tree-reverse
  (define (test-reverse t)
    ;; reverse once
    (test (reverse (finger-tree->list t))
	  (finger-tree->list (finger-tree-reverse add measure t)))
    ;; reverse twice (identity)
    (test (finger-tree->list t)
	  (finger-tree->list
	   (finger-tree-reverse add measure
				(finger-tree-reverse add measure t)))))	  
  (test-reverse t0)
  (test-reverse t1)
  (test-reverse t2) 
  (test-reverse t5)
  (test-reverse t8)
  (test-reverse t9)
  (test-reverse t33)
  (test-reverse t999)

  ;; generator->finger-tree with specified length
  ;; use entire generator
  (test (iota 100)
	(finger-tree->list
	 (generator->finger-tree add measure
				 (make-iota-generator 100)
				 100)))
  ;; stop early
  (test (iota 95)
	(finger-tree->list
	 (generator->finger-tree add measure
				 (make-iota-generator 100)
				 95)))
  ;; empty
  (test '()
	(finger-tree->list
	 (generator->finger-tree add measure (generator) 0)))

  (do ((n 0 (+ 1 n)))
      ((= n (if *exhaustive*
		1000
		50)))
    (let* ((lst (iota n))
	   (tree (list->finger-tree add measure lst)))
      
      ;; list->finger-tree
      (test lst (finger-tree->list tree))

      ;; generator->finger-tree
      (test lst
	    (finger-tree->list
	     (generator->finger-tree add measure (make-iota-generator n))))
    
      ;; finger-tree->generator
      (test lst
	    (generator->list
	     (finger-tree->generator tree)))

      ;; finger-tree->reverse-generator
      (test (reverse lst)
	    (generator->list
	     (finger-tree->reverse-generator tree)))))

  ;; finger-tree-scan
  (define (test-scan t)
    (let ((scan (lambda (query)
		  (finger-tree-scan add
				    measure
				    (lambda (element)
				      (>= element query))
				    #false
				    t
				    (lambda (element)
				      (= element query))
				    (lambda ()
				      #false)))))
      (finger-tree-for-each (lambda (i)
			      (test #true (scan i))
			      (test #false (scan (+ i 1/2)))
			      (test #false (scan (- i 1/2))))
			    t)))
  (test-scan t0)
  (test-scan t1)
  (test-scan t2)
  (test-scan t5)
  (test-scan t8)
  (test-scan t9)
  (test-scan t33)
  (when *exhaustive* (test-scan t999))

  (define (test-split t)
    (let ((split (lambda (query)
		   (finger-tree-split add
				      measure
				      (lambda (element)
					(>= element query))
				      #false
				      t
				      (lambda (prefix element suffix)
					(values (finger-tree->list prefix)
						element
						(finger-tree->list suffix)))
				      (lambda ()
					#false))))
	  (lst (finger-tree->list t)))
      (finger-tree-for-each (lambda (i)
			      (let*-values (((pre rest) (split-at lst i))
					    ((x suf) (car+cdr rest)))
				;; succeed on matching element
				(test-values (values pre x suf)
					     (split i))
				;; succeed on mismatched element
				(test-values (values pre x suf)
					     (split (- i 1/2)))))
			    t)
      ;; fail
      (test #false (split (length lst)))))

  (test-split t0)
  (test-split t1)
  (test-split t2)
  (test-split t5)
  (test-split t8)
  (test-split t9)
  (test-split t33)
  (when *exhaustive* (test-split t999))

  )
