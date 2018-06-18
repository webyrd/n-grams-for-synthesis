;; SRFI-44 Test Suite
;; 3/5/2004, Scott G. Miller

; Evaluates an expression, returning two values, the first a boolean
; indicating whether an error was caught, the second a return value or 
; if an error occured, an error message or #f if one cannot be obtained.
; Its currently tailored for SISC and must be modified to suit the
; error handling in your implementation.
(define-syntax rationalize-error
  (syntax-rules () 
    ((_ expr)
     (with-failure-continuation
      (lambda (m e)
	(values #f (error-message m)))
      (lambda ()
        (apply values #t
               (call-with-values
                   (lambda () expr)
                 list)))))))

; Performs a test, returning #t if it passed, or another value
; describing why the test failed if possible, or #f.
(define-syntax do-test
  (syntax-rules (=> <> <any> <error>)
    ; The first form just means the test should not complete in error
    ((_ expr)
     (do-test expr => <any>))
    ; This form requires that an error be raised to pass
    ((_ expr => <error>)
     (call-with-values
         (lambda () 
           (rationalize-error expr))
       (lambda (e . v)
         (if (not e)
             (values #t #t)
             (values #f v)))))
    ; This form requires any non-error value
    ((_ expr => <any>)
     (call-with-values
       (lambda () 
	 (rationalize-error expr))
       (lambda (e . v)
         (if e (values #t #t) (values #f v)))))
    ; This form requires that the return value be equal to the rhs
    ((_ expr => val)
     (call-with-values
	 (lambda ()
	   (rationalize-error expr))
       (lambda (e . v)
	 (if (and e (equal? (car v) val))
             (values #t #t)
	     (apply values #f v)))))
    ; This form tests that the return value *not* equal the rhs
    ((_ expr <> val)
          (call-with-values
	 (lambda ()
	   (rationalize-error expr))
       (lambda (e . v)
	 (if (and e (not (equal? (car v) val)))
             (values #t #t)
	     (apply values #f v)))))))



; Display a test
(define test-number 0)

(define-syntax run-test 
  (syntax-rules () 
    ((_ id args ...)
     (begin
       (display id)
       (display ": ")
       (call-with-values
	 (lambda ()
	   (do-test args ...))
	 (lambda (p v)
	   (if p 
	       (display "Passed.")
	       (begin (display "Failed: ")
		      (write v)))))
       (newline)))))

(define-syntax test
  (syntax-rules (:)
    ((_ id (subid : args ...) ...)
     (begin (run-test (format "~a.~a" id subid) args ...) ...))
    ((_ id args ...)
     (test id (0 : args ...)))))

(define-syntax *-
  (syntax-rules ()
    ((_ base-type (class op args ...))
     (test-* 'class 'op 'base-type args ...))
    ((_ (class op args ...))
     (test-* 'class 'op #f args ...))))

(define-syntax test-help
  (syntax-rules ()
    ((_ (class op args ...))
     ((eval (string->symbol (format "~a~a" 'class 'op))
            (interaction-environment)) args ...))))

(define-syntax test-help/-
  (syntax-rules ()
    ((_ (class op args ...))
     ((eval (string->symbol (format "~a-~a" 'class 'op))
            (interaction-environment)) args ...))))

(define-syntax test-*
  (syntax-rules (list alist-map flexible-sequence sequence)
    ((_ id (list op args ...) result ...)
     (test id
           (0 : (test-help/- (list op args ...)) result ...)
           (1 : (test-help/- (flexible-sequence op args ...)) result ...)
           (2 : (test-help/- (sequence op args ...)) result ...)
           (3 : (test-help/- (bag op args ...)) result ...)))
    ((_ id (alist-map op args ...) result ...)
     (test id
           (0 : (test-help/- (alist-map op args ...)) result ...)
           (1 : (test-help/- (map op args ...)) result ...)))
    ((_ id (seq op args ...) result ...)
     (test id (0 : (test-help/- (seq op args ...)) result ...)
              (1 : (test-help/- (sequence op args ...)) result ...)
              (2 : (test-help/- (bag op args ...)) result ...)))
    ((_ id sequence (seq op args ...) result ...)
     (test id (0 : (test-help/- (seq op args ...)) result ...)
              (1 : (test-help/- (sequence op args ...)) result ...)))
    ((_ id flexible-sequence (coll op args ...) result ...)
     (test id
           (0 : (test-help/- (coll op args ...)) result ...)
           (1 : (test-help/- (flexible-sequence op args ...)) result ...)))))

(define-syntax testc-*
  (syntax-rules (list alist-map flexible-sequence sequence)
    ((_ id (list op args ...) result ...)
     (test id
           (0 : (test-help/- (list op args ...)) result ...)
           (1 : (test-help/- (flexible-sequence op args ...)) result ...)
           (2 : (test-help/- (sequence op args ...)) result ...)
           (3 : (test-help/- (bag op args ...)) result ...)
           (4 : (test-help/- (collection op args ...)) result ...)))
    ((_ id (alist-map op args ...) result ...)
     (test id
           (0 : (test-help/- (alist-map op args ...)) result ...)
           (1 : (test-help/- (map op args ...)) result ...)
           (1 : (test-help/- (collection op args ...)) result ...)))
    ((_ id (seq op args ...) result ...)
     (test id (0 : (test-help/- (seq op args ...)) result ...)
              (1 : (test-help/- (sequence op args ...)) result ...)
              (2 : (test-help/- (bag op args ...)) result ...)
              (3 : (test-help/- (collection op args ...)) result ...)))))

(define-syntax testc*
  (syntax-rules (list alist-map flexible-sequence sequence)
    ((_ id (list op args ...) result ...)
     (test id
           (0 : (test-help (list op args ...)) result ...)
           (1 : (test-help (flexible-sequence op args ...)) result ...)
           (2 : (test-help (sequence op args ...)) result ...)
           (3 : (test-help (bag op args ...)) result ...)
           (4 : (test-help (collection op args ...)) result ...)))
    ((_ id (alist-map op args ...) result ...)
     (test id
           (0 : (test-help (alist-map op args ...)) result ...)
           (1 : (test-help (map op args ...)) result ...)
           (1 : (test-help (collection op args ...)) result ...)))
    ((_ id (seq op args ...) result ...)
     (test id (0 : (test-help (seq op args ...)) result ...)
              (1 : (test-help (sequence op args ...)) result ...)
              (2 : (test-help (bag op args ...)) result ...)
              (3 : (test-help (collection op args ...)) result ...)))))


(define-syntax receive-first
  (syntax-rules ()
    ((_ expr ...)
     (call-with-values
         (lambda () expr ...)
       (lambda args
         (car args))))))

(define ls1 #f)
(define ls2 #f)
(define ls3 #f)
(define vec1 #f)
(define vec2 #f)
(define str1 #f)
(define str2 #f)
(define amap1 #f)
(define amap2 #f)
(define amap3 #f)
(define amap4 #f)

(define (list-contents-equal? ls1 ls2)
  (and (= (length ls1) (length ls2))
       (let loop ((x ls1))
         (or (null? x)
             (and (member (car x) ls2)
                  (loop (cdr x)))))))

;Construction
(test 1
      (0 : (set! ls1 (list 1 2 3)))
      (1 : (set! ls2 (make-list)))
      (2 : (set! vec1 (vector 1 2 3)))
      (3 : (set! vec2 (make-vector 3)))
      (4 : (set! str1 (string #\a #\b #\c)))
      (5 : (set! str2 (make-string 3 #\space)))
      (6 : (set! amap1 (alist-map equal? '(a . 1) '(b . 2) '(c . 3))))
      (7 : (set! amap2 (make-alist-map equal?))))

; Enumeration
(define idx1 '((0 . 1) (1 . 2) (2 . 3)))
(define ridx1 '((2 . 3) (1 . 2) (0 . 1)))
(define idx2 '((0 . #\a) (1 . #\b) (2 . #\c)))
(define ridx2 '((2 . #\c) (1 . #\b) (0 . #\a)))

(define (always-proceed f)
  (lambda knils
    (receive new-knils (apply f knils) (apply values
                                              #t new-knils))))
(define apsap (always-proceed
               (lambda (c seed)
                 (string-append (string c) seed))))
(define ap+ (always-proceed +))
(define ap- (always-proceed -))
(define apcons2 (always-proceed (lambda (a b c) (cons (cons a b) c))))

(test 2 (0 : (collection-fold-left ls1 ap+ 0) => 6)
        (1 : (list-fold-left ls1 ap+ 0) => 6)
        (2 : (collection-fold-right ls1 ap- 0) => 2)
        (3 : (list-fold-right ls1 ap- 0) => 2)
        (4 : (collection-fold-keys-left ls1 apcons2 '()) => ridx1)
        (5 : (list-fold-keys-left ls1 apcons2 '()) => ridx1)
        (6 : (collection-fold-keys-right ls1 apcons2 '()) => idx1)
        (7 : (list-fold-keys-right ls1 apcons2 '()) => idx1)
        (8 : (collection-fold-left vec1 ap+ 0) => 6)
        (9 : (vector-fold-left vec1 ap+ 0) => 6)
        (10 : (collection-fold-right vec1 ap- 0) => 2)
        (11 : (vector-fold-right vec1 ap- 0) => 2)
        (12 : (collection-fold-keys-left vec1 apcons2 '()) => ridx1)
        (13 : (vector-fold-keys-left vec1 apcons2 '()) => ridx1)
        (14 : (collection-fold-keys-right vec1 apcons2 '()) => idx1)
        (15 : (vector-fold-keys-right vec1 apcons2 '()) => idx1)
        (16 : (collection-fold-left str1 apsap "d") => "cbad")
        (18 : (collection-fold-right str1 apsap "d") => "abcd")
        (20 : (collection-fold-left amap1 ap+ 0) => 6)
        (21 : (alist-map-fold-left amap1 ap+ 0) => 6)
        (22 : (collection-fold-right amap1 ap+ 0) => 6)
        (23 : (alist-map-fold-right amap1 ap+ 0) => 6)
        (24 : (collection-fold-keys-left str1 apcons2 '()) => ridx2)
        (25 : (string-fold-keys-left str1 apcons2 '()) => ridx2)
        (26 : (collection-fold-keys-right str1 apcons2 '()) => idx2)
        (27 : (string-fold-keys-right str1 apcons2 '()) => idx2)
        )

; Collections
(test 3
      (0 : (collection? ls1) <> #f)
      (1 : (collection? ls2) <> #f)
      (2 : (collection? vec1) <> #f)
      (3 : (collection? vec2) <> #f)
      (4 : (collection? str1) <> #f)
      (5 : (collection? str2) <> #f)
      (6 : (collection? amap1) <> #f)
      (7 : (collection? amap2) <> #f))

(test 4
      (0 : (collection-name ls1) => 'list)
      (1 : (collection-name str1) => 'string)
      (2 : (collection-name vec1) => 'vector)
      (3 : (collection-name amap1) => 'alist-map))

(test 5
      (0 : (list? ls1) <> #f)
      (1 : (string? str1) <> #f)
      (2 : (vector? vec1) <> #f)
      (3 : (alist-map? amap1) <> #f))

(testc-* 6 (list size ls1) => 3)
(testc-* 7 (string size str1) => 3)
(testc-* 8 (vector size vec1) => 3)
(testc-* 9 (alist-map size amap1) => 3)

(testc-* 10 (list count ls1 1) => 1)
(testc-* 11 (string count str1 #\a) => 1)
(testc-* 12 (vector count vec1 1) => 1)
(testc-* 13 (alist-map count amap1 1) => 1)

(testc-* 14 (list count ls1 'a) => 0)
(testc-* 15 (string count str1 #\a) => 0)
(testc-* 16 (vector count vec1 'a) => 0)
(testc-* 17 (alist-map count amap1 'a) => 0)

(testc-* 18 (list get-any ls1))
(testc-* 19 (string get-any str1))
(testc-* 20 (vector get-any vec1))
(testc-* 21 (alist-map get-any amap1))

(testc-* 22 (list empty? ls1) <> #t)
(testc-* 23 (string empty? str1) <> #t)
(testc-* 24 (vector empty? vec1) <> #t)
(testc-* 25 (alist-map empty? amap1) <> #t)

(testc-* 26 (list >list ls1) => ls1)
(testc-* 27 (string >list str1) => '(#\a #\b #\c))
(testc-* 28 (vector >list vec1) => ls1)
(test 29
      (0 : (list-contents-equal? (alist-map->list amap1) ls1) <> #f)
      (1 : (list-contents-equal? (map->list amap1) ls1) <> #f)
      (2 : (list-contents-equal? (collection->list amap1) ls1) <> #f))

(testc-* 30 (list clear ls1) => (make-list))
(testc-* 31 (string clear str1) => (make-string 0))
(testc-* 32 (vector clear vec1) => (make-vector 0))

(test 33
      (0 : (map= equal? (alist-map-clear amap1) (make-alist-map equal?)) <> #f)
      (1 : (map= equal? (map-clear amap1) (make-alist-map equal?)) <> #f)
      (2 : (map= equal? (collection-clear amap1) (make-alist-map equal?)) <> #f))
(testc-* 34 (list clear! ls1) => (make-list))
(testc-* 35 (string clear! str1) => (make-string 0))
(testc-* 36 (vector clear! vec1) => (make-vector 0))
(test 37
      (0 : (map= equal? (alist-map-clear! amap1) (make-alist-map equal?)) <> #f)
      (1 : (set! amap1 (alist-map '(a . 1) '(b . 2) '(c . 3))))
      (2 : (map= equal? (map-clear! amap1) (make-alist-map equal?)) <> #f)
      (3 : (set! amap1 (alist-map '(a . 1) '(b . 2) '(c . 3))))
      (4 : (map= equal? (collection-clear! amap1) (make-alist-map equal?)) <> #f))

(test 38
      (0 : (set! ls1 (list 1 2 3)))
      (1 : (set! vec1 (vector 1 2 3)))
      (2 : (set! str1 (string #\a #\b #\c)))
      (3 : (set! amap1 (alist-map '(a . 1) '(b . 2) '(c . 3)))))

(testc* 39 (list = equal? ls1 ls2) => #f)
(testc* 40 (vector = equal? vec1 vec2) => #f)
(testc* 41 (string = equal? str1 str2) => #f)
(testc* 42 (alist-map = equal? amap1 amap2) => #f)

(test 43
      (0 : (set! ls2 (collection-copy ls1)))
      (1 : (set! str2 (collection-copy str1)))
      (2 : (set! vec2 (collection-copy vec1)))
      (3 : (set! amap2 (collection-copy amap1))))

(testc* 44 (list = equal? ls1 ls2) <> #f)
(testc* 45 (vector = equal? vec1 vec2) <> #f)
(testc* 46 (string = equal? str1 str2) <> #f)
(testc* 47 (alist-map = equal? amap1 amap2) <> #f)

(test 48
      (0 : (bag? ls1) <> #f)
      (1 : (bag? str1) <> #f)
      (2 : (bag? vec1) <> #f))

(test-* 49 (list contains? ls1 2) <> #f)
(test-* 50 (string contains? str1 #\b) <> #f)
(test-* 51 (vector contains? vec1 2) <> #f)

(test 52
      (0 : (list-contents-equal? (list-add ls1 4) (list 4 1 2 3)) <> #f)
      (1 : (list-contents-equal? (flexible-sequence-add ls1 4) (list 4 1 2 3)) <> #f)
      (2 : (list-contents-equal? (sequence-add ls1 4) (list 4 1 2 3)) <> #f)
      (3 : (list-contents-equal? (bag-add ls1 4) (list 4 1 2 3)) <> #f))

(test 53
      (0 : (list-contents-equal? (list-add! ls1 4) (list 4 1 2 3)) <> #f)
      (1 : (set! ls1 (collection-copy ls2)))
      (2 : (list-contents-equal? (flexible-sequence-add! ls1 4)
                                 (list 4 1 2 3)) <> #f)
      (3 : (set! ls1 (collection-copy ls2)))
      (4 : (list-contents-equal? (sequence-add! ls1 4) (list 4 1 2 3)) <> #f)
      (5  : (set! ls1 (collection-copy ls2)))
      (6 : (list-contents-equal? (bag-add! ls1 4) (list 4 1 2 3)) <> #f)
      (7 : (set! ls1 (collection-copy ls2))))

(test-* 55 (list delete ls1 2) => '(1 3))
(test-* 56 (list delete! ls1 2) => '(1 3))
(test 57 (set! ls1 (collection-copy ls2)))

(test-* 58 (list delete-all ls1 2) => '(1 3))
(test-* 59 (list delete-all! ls1 2) => '(1 3))
(test 60 (set! ls1 (collection-copy ls2)))
(test 61 (set! ls3 (list 3 4 5)))

(test 62
      (0 : (list-contents-equal? (list-add-from ls1 ls2) '(1 2 3 3 4 5)) <> #f)
      (1 : (list-contents-equal? (flexible-sequence-add-from ls1 ls2)
                                '(1 2 3 3 4 5)) <> #f)
      (2 : (list-contents-equal? (sequence-add-from ls1 ls2)
                                '(1 2 3 3 4 5)) <> #f)
      (3 : (list-contents-equal? (bag-add-from ls1 ls2)
                                '(1 2 3 3 4 5)) <> #f))
(test 63
      (0 : (list-contents-equal? (list-add-from! ls1 ls2) '(1 2 3 3 4 5)) <> #f)
      (1 : (set! ls1 (collection-copy ls2)))
      (2 : (list-contents-equal? (flexible-sequence-add-from! ls1 ls2)
                                '(1 2 3 3 4 5)) <> #f)
      (3 : (set! ls1 (collection-copy ls2)))
      (4 : (list-contents-equal? (sequence-add-from! ls1 ls2)
                                '(1 2 3 3 4 5)) <> #f)
      (5 : (set! ls1 (collection-copy ls2)))
      (6 : (list-contents-equal? (bag-add-from! ls1 ls2)
                                '(1 2 3 3 4 5)) <> #f))

(test 64
      (0 : (set! ls2 (list 3)))
      (1 : (list-delete-from ls1 ls2) => '(1 2))
      (2 : (flexible-sequence-delete-from ls1 ls2) => '(1 2))
      (3 : (sequence-delete-from ls1 ls2) => '(1 2))
      (4 : (bag-delete-from ls1 ls2) => '(1 2)))

(test 65
      (0 : (list-delete-from! ls1 ls2) => '(1 2))
      (1 : (set! ls2 (list 3)))
      (2 : (flexible-sequence-delete-from! ls1 ls2) => '(1 2))
      (3 : (set! ls2 (list 3)))
      (4 : (sequence-delete-from! ls1 ls2) => '(1 2))
      (5 : (set! ls2 (list 3)))
      (6 : (bag-delete-from! ls1 ls2) => '(1 2)))

(test 66 (set! ls1 (list 1 1 2 2 3 3)))
(test 67 (set! ls3 (list 2 3)))

(test 68
      (0 : (list-delete-all-from ls1 ls3) => '(1 1))
      (1 : (flexible-sequence-delete-all-from ls1 ls3) => '(1 1))
      (2 : (sequence-delete-all-from ls1 ls3) => '(1 1))
      (3 : (bag-delete-all-from ls1 ls3) => '(1 1)))

(test 69
      (0 : (set! ls2 (list 1 2 3)))
      (1 : (set! ls1 (collection-copy ls2))))

(test 70
      (0 : (sequence? ls1) <> #f)
      (1 : (sequence? str1) <> #f)
      (2 : (sequence? vec1) <> #f))

(test-* 71 sequence (list ref ls1 1) => 2)
(test-* 72 sequence (vector ref vec1 1) => 2)
(test-* 73 sequence (string ref str1 1) => #\b)

(test-* 74 sequence (list get-left ls1) => 1)
(test-* 75 sequence (vector get-left vec1) => 1)
(test-* 76 sequence (string get-left str1) => #\a)

(test-* 77 sequence (list get-right ls1) => 3)
(test-* 78 sequence (vector get-right vec1) => 3)
(test-* 79 sequence (string get-right str1) => #\c)

(test-* 80 sequence (list insert-right ls1 4) => '(1 2 3 4))

(test 81
      (0 : (list-insert-right! ls1 4) => '(1 2 3 4))
      (1 : (set! ls1 (collection-copy ls2)))
      (2 : (flexible-sequence-insert-right! ls1 4) => '(1 2 3 4))
      (3 : (set! ls1 (collection-copy ls2)))      
      (4 : (sequence-insert-right! ls1 4) => '(1 2 3 4))
      (5 : (set! ls1 (collection-copy ls2))))

(test-* 82 flexible-sequence (list insert-left ls1 4) => '(4 1 2 3))

(test 83
      (0 : (list-insert-left! ls1 4) => '(4 1 2 3))
      (1 : (set! ls1 (collection-copy ls2)))
      (2 : (flexible-sequence-insert-left! ls1 4) => '(4 1 2 3))
      (3 : (set! ls1 (collection-copy ls2))))

(test-* 84 sequence (list set ls1 1 4) => '(1 4 3))

(test 85
      (0 : (list-set! ls1 1 4) => '(1 4 3))
      (1 : (set! ls1 (collection-copy ls2)))
      (2 : (flexible-sequence-set! ls1 1 4) => '(1 4 3))
      (3 : (set! ls1 (collection-copy ls2)))      
      (4 : (sequence-set! ls1 1 4) => '(1 4 3))
      (5 : (set! ls1 (collection-copy ls2))))

(test-* 86 sequence (list add ls1 4) => '(4 1 2 3))

(test 87
      (0 : (list-add! ls1 4) => '(4 1 2 3))
      (1 : (set! ls1 (collection-copy ls2)))
      (2 : (flexible-sequence-add! ls1 4) => '(4 1 2 3))
      (3 : (set! ls1 (collection-copy ls2)))      
      (4 : (sequence-add! ls1 4) => '(4 1 2 3))
      (5 : (set! ls1 (collection-copy ls2)))
      (6 : (bag-add! ls1 4) => '(4 1 2 3))
      (7 : (set! ls1 (collection-copy ls2))))

(test-* 88 sequence (list copy ls1) => '(1 2 3))
(test-* 89 sequence (list copy ls1 1) => '(2 3))
(test-* 90 sequence (list copy ls1 0 2) => '(1 2))
(test-* 91 sequence (list copy ls1 1 2) => '(2))

(test-* 92 flexible-sequence (list insert ls1 1 4) => '(1 4 2 3))
(test 93
      (0 : (list-insert! ls1 1 4) => '(1 4 2 3))
      (1 : (set! ls1 (collection-copy ls2)))
      (2 : (flexible-sequence-insert! ls1 1 4) => '(1 4 2 3))
      (3 : (set! ls1 (collection-copy ls2))))

(test-* 94 flexible-sequence (list delete-at ls1 1) => '(1 3))
(test 95
      (0 : (list-delete-at! ls1 1) => '(1 3))
      (1 : (set! ls1 (collection-copy ls2)))
      (2 : (flexible-sequence-delete-at! ls1 1) => '(1 3))
      (3 : (set! ls1 (collection-copy ls2))))

(test-* 96 flexible-sequence (list delete-left ls1) => '(2 3))
(test 97
      (0 : (list-delete-left! ls1) => '(2 3))
      (1 : (set! ls1 (collection-copy ls2)))
      (2 : (flexible-sequence-delete-left! ls1) => '(2 3))
      (3 : (set! ls1 (collection-copy ls2))))

(test-* 98 flexible-sequence (list delete-right ls1) => '(1 2))
(test 99
      (0 : (list-delete-right! ls1) => '(1 2))
      (1 : (set! ls1 (collection-copy ls2)))
      (2 : (flexible-sequence-delete-right! ls1) => '(1 2))
      (3 : (set! ls1 (collection-copy ls2))))

(test 100 (map? amap1) <> #f)
(test-* 101 (alist-map comparator amap1) => eqv?)
(test-* 102 (alist-map key-comparator amap1) => eqv?)

(test-* 103 (alist-map contains-key? amap1 'b) <> #f)

(test 104
      (0 : (list-contents-equal? (alist-map-keys->list amap1) '(a b c)) <> #f)
      (1 : (list-contents-equal? (map-keys->list amap1) '(a b c)) <> #f))

(test-* 106 (alist-map get amap1 'b) => 2)
(test-* 107 (alist-map get amap1 'c) => 3)

(test 108
      (0 : (set! amap3 (alist-map '(a . 1) '(b . 2) '(c . 3) '(d . 4))))
      (1 : (set! amap1 (alist-map '(a . 1) '(b . 2) '(c . 3))))
      (2 : (set! amap2 (alist-map '(a . 1) '(b . 2) '(c . 3)))))

(test 109
      (0 : (collection= equal? (receive-first (alist-map-put amap1 'd 4))
                        amap3))
      (1 : (collection= equal? (receive-first (map-put amap1 'd 4))
                        amap3)))

(test 111
      (0 : (map= equal? (receive-first (alist-map-put! amap1 'd 4)) amap3) <> #f)
      (1 : (set! amap1 (collection-copy amap2)))
      (2 : (collection= equal? (receive-first (map-put! amap1 'd 4)) amap3) <> #f)
      (3 : (set! amap1 (collection-copy amap2))))

(test 112 (set! amap3 (alist-map '(a . 1) '(b . 2) '(c . 4))))
(test 113 (collection= equal? (receive-first (alist-map-put amap1 'c 4) amap3))
      <> #f)
(test 114 (collection= equal? (receive-first (map-put amap1 'c 4) amap3)) <> #f)

(test 115
      (0 : (map= equal? (receive-first (alist-map-put! amap1 'c 4)) amap3) <> #f)
      (1 : (set! amap1 (collection-copy amap2)))
      (2 : (collection= equal? (receive-first (map-put! amap1 'c 4)) amap3) <> #f)
      (3 : (set! amap1 (collection-copy amap2))))

(define (add1 x) (+ x 1))

(test 116 (collection= equal? (alist-map-update amap1 'c add1) amap3) <> #f)
(test 117 (collection= equal? (map-update amap1 'c add1) amap3) <> #f)

(test 118
      (0 : (map= equal? (alist-map-update! amap1 'c add1) amap3) <> #f)
      (1 : (set! amap1 (collection-copy amap2)))
      (2 : (collection= equal? (map-update! amap1 'c add1) amap3) <> #f)
      (3 : (set! amap1 (collection-copy amap2))))

(test 119 (set! amap3 (alist-map '(a . 1) '(b . 2))))

(test 120 (collection= equal? (alist-map-delete amap1 'c) amap3) <> #f)
(test 121 (collection= equal? (map-delete amap1 'c) amap3) <> #f)

(test 122
      (0 : (map= equal? (alist-map-delete! amap1 'c) amap3) <> #f)
      (1 : (set! amap1 (collection-copy amap2)))
      (2 : (collection= equal? (map-delete! amap1 'c) amap3) <> #f)
      (3 : (set! amap1 (collection-copy amap2))))

(test 123 (set! amap3 (alist-map '(a . 1))))
(test 124 (collection= equal? (alist-map-delete-from amap1 '(b c)) amap3) <> #f)
(test 125 (collection= equal? (map-delete-from amap1 '(b c)) amap3) <> #f)

(test 126
      (0 : (map= equal? (alist-map-delete-from! amap1 '(b c)) amap3) <> #f)
      (1 : (set! amap1 (collection-copy amap2)))
      (2 : (collection= equal? (map-delete-from! amap1 '(b c)) amap3) <> #f)
      (3 : (set! amap1 (collection-copy amap2))))

(test 127 (set! amap3 (alist-map '(a . 1) '(b . 2) '(c . 3) '(d . 4) '(e . 5))))
(test 128 (set! amap4 (alist-map '(d . 4) '(e . 5))))

(test 129 (collection= equal? (alist-map-add-from amap1 amap4) amap3) <> #f)
(test 130 (collection= equal? (map-add-from amap1 amap4) amap3) <> #f)

(test 131
      (0 : (map= equal? (alist-map-add-from! amap1 amap4) amap3) <> #f)
      (1 : (set! amap1 (collection-copy amap2)))
      (2 : (collection= equal? (map-add-from! amap1 amap4) amap3) <> #f)
      (3 : (set! amap1 (alist-map))))

(testc-* 132 (list get-any '() (lambda () 'false)) => 'false)
(testc-* 133 (vector get-any '#() (lambda () 'false)) => 'false)
(testc-* 134 (string get-any "" (lambda () 'false)) => 'false)
(testc-* 135 (alist-map get-any amap1 (lambda () 'false)) => 'false)

(test-* 136 sequence (list get-left '() (lambda () 'false)) => 'false)
(test-* 137 sequence (vector get-left '#() (lambda () 'false)) => 'false)
(test-* 138 sequence (string get-left "" (lambda () 'false)) => 'false)

(test-* 139 sequence (list get-right '() (lambda () 'false)) => 'false)
(test-* 140 sequence (vector get-right '#() (lambda () 'false)) => 'false)
(test-* 141 sequence (string get-right "" (lambda () 'false)) => 'false)

(test-* 141 sequence (list ref '() 0 (lambda () 'false)) => 'false)
(test-* 142 sequence (vector ref '#() 0 (lambda () 'false)) => 'false)
(test-* 143 sequence (string ref "" 0 (lambda () 'false)) => 'false)

(test-* 144 (alist-map get amap1 'a (lambda () 'false)) => 'false)

(test 145
      (0 : (call-with-values (lambda ()
                               (alist-map-put amap1 'a 4 (lambda () 'false)))
             (lambda (map val) val)) => 'false)
      (1 : (call-with-values (lambda ()
                               (map-put amap1 'a 4 (lambda () 'false)))
             (lambda (map val) val)) => 'false))

(test 146
      (0 : (call-with-values (lambda ()
                              (alist-map-put! amap1 'a 4 (lambda () 'false)))
            (lambda (map val) val)) => 'false)
      (1 : (set! amap1 (alist-map)))
      (2 : (call-with-values (lambda ()
                              (map-put! amap1 'a 4 (lambda () 'false)))
            (lambda (map val) val)) => 'false)
      (3 : (set! amap1 (alist-map))))

(test 147
      (0 : (alist-map-get
            (alist-map-update amap1 'a add1 (lambda () 1)) 'a) => 2)
      (1 : (alist-map-get
            (map-update amap1 'a add1 (lambda () 1)) 'a) => 2))


(test 148
      (0 : (alist-map-get
            (alist-map-update! amap1 'a add1 (lambda () 1)) 'a) => 2)
      (1 : (set! amap1 (alist-map)))
      (2 : (alist-map-get
            (map-update! amap1 'a add1 (lambda () 1)) 'a) => 2)
      (3 : (set! amap1 (alist-map))))
