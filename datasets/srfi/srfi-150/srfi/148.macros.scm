;; Copyright (C) Marc Nieper-Wißkirchen (2016).  All Rights Reserved. 

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.


;; General

(define-syntax em-constant
  (em-syntax-rules ::: ()
    ((em-constant 'const)
     (em-cut 'em-constant-aux 'const <> ...))))

(define-syntax em-constant-aux
  (em-syntax-rules ()
    ((em-constant-aux 'const 'arg ...)
     'const)))

(define-syntax em-append
  (em-syntax-rules ()
    ((em-append) ''())
    ((em-append 'l) 'l)
    ((em-append 'm ... '(a ...) 'l) (em-append 'm ... '(a ... . l)))))

(define-syntax em-list
  (em-syntax-rules ()
    ((em-list 'a ...) '(a ...))))

(define-syntax em-cons
  (em-syntax-rules ()
    ((em-cons 'h 't) '(h . t))))

(define-syntax em-cons*
  (em-syntax-rules ()
    ((em-cons* 'e ... 't) '(e ... . t))))

(define-syntax em-car
  (em-syntax-rules ()
    ((em-car '(h . t)) 'h)))

(define-syntax em-cdr
  (em-syntax-rules ()
    ((em-cdr '(h . t)) 't)))

(define-syntax em-apply
  (em-syntax-rules ()
    ((em-apply 'keyword 'datum1 ... '(datum2 ...))
     (keyword 'datum1 ... 'datum2 ...))))

(define-syntax em-call
  (em-syntax-rules ()
    ((em-apply 'keyword 'datum ...)
     (keyword 'datum ...))))

(define-syntax em-eval
  (em-syntax-rules ()
    ((em-eval '(keyword datum ...))
     (keyword datum ...))))

(define-syntax em-error
  (em-syntax-rules ()
    ((em-error 'message 'arg ...)
     (em-suspend 'em-error-aux 'message 'arg ...))))

(define-syntax em-error-aux
  (syntax-rules ()
    ((em-error-aux s message arg ...)
     (syntax-error message arg ...))))

(define-syntax em-gensym
  (em-syntax-rules ()
    ((em-gensym) 'g)))

(define-syntax em-generate-temporaries
  (em-syntax-rules ()
    ((em-generate-temporaries '()) '())
    ((em-generate-temporaries '(h . t))
     (em-cons (em-gensym) (em-generate-temporaries 't)))))

(define-syntax em-quote
  (em-syntax-rules ()
    ((em-quote 'x) ''x)))

;; Boolean logic

(define-syntax em-if
  (em-syntax-rules ()
    ((em-if '#f consequent alternate)
     alternate)
    ((em-if 'test consequent alternate)
     consequent)))

(define-syntax em-not
  (em-syntax-rules ()
    ((em-not '#f)
     '#t)
    ((em-not '_)
     '#f)))

(define-syntax em-or
  (em-syntax-rules ()
    ((em-or)
     '#f)
    ((em-or 'x y ...)
     (em-if 'x 'x (em-or y ...)))))

(define-syntax em-and
  (em-syntax-rules ()
    ((em-and 'x)
     'x)
    ((em-and 'x y ...)
     (em-if 'x (em-and y ...) '#f))))

(define-syntax em-null?
  (em-syntax-rules ()
    ((em-null? '())
     '#t)
    ((em-null? '_)
     '#f)))

(define-syntax em-pair?
  (em-syntax-rules ()
    ((em-null? '(_ . _))
     '#t)
    ((em-null? '_)
     '#f)))

(define-syntax em-list?
  (em-syntax-rules ()
    ((em-list? '())
     '#t)
    ((em-list? '(_ . x))
     (em-list? 'x))
    ((em-list? '_)
     '#f)))

(define-syntax em-boolean?
  (em-syntax-rules ()
    ((em-boolean? '#f)
     '#t)
    ((em-boolean? '#t)
     '#t)
    ((em-boolean? '_)
     '#f)))

(define-syntax em-vector?
  (em-syntax-rules ()
    ((em-vector? '#(x ...))
     '#t)
    ((em-vector? '_)
     '#f)))

(define-syntax em-symbol?
  (em-syntax-rules ()
    ((em-symbol? '(x . y)) '#f)
    ((em-symbol? '#(x ...)) '#f)
    ((em-symbol? 'x)
     (em-suspend 'em-symbol?-aux 'x))))

(define-syntax em-symbol?-aux
  (syntax-rules ()
    ((em-symbol?-aux s x)
     (begin
       (define-syntax test
	 (syntax-rules ::: ()
		       ((test x %s) (em-resume %s '#t))
		       ((test y %s) (em-resume %s '#f))))
       (test symbol s)))))

(define-syntax em-bound-identifier=?
  (em-syntax-rules ()
    ((em-bound-identifier=? 'id 'b)
     (em-suspend em-bound-identifier=?-aux 'id 'b))))

(define-syntax em-bound-identifier=?-aux
  (syntax-rules ()
    ((em-bound-identifier=?-aux s id b)
     (bound-identifier=? id b (em-resume s '#t) (em-resume s '#f)))))

(define-syntax em-free-identifier=?
  (em-syntax-rules ()
    ((em-free-identifier=? 'id1 'id2)
     (em-suspend em-free-identifier=?-aux 'id1 'id2))))

(define-syntax em-free-identifier=?-aux
  (syntax-rules ()
    ((em-free-identifier=?-aux s id1 id2)
     (free-identifier=? id1 id2 (em-resume s '#t) (em-resume s '#f)))))

(define-syntax em-constant=?
  (em-syntax-rules ()
    ((ck= 'x 'y)
     (em-suspend em-constant=?-aux 'x 'y))))

(define-syntax em-constant=?-aux
  (syntax-rules ()
    ((em-constant=?-aux s x y)
     (begin
       (define-syntax test
	 (syntax-rules ::: ()
		       ((test x %s) (em-resume %s '#t))
		       ((test z %s) (em-resume %s '#f))))
       (test y s)))))

(define-syntax em-equal?
  (em-syntax-rules ()
    ((em-equal? '(x . y) '(u . v))
     (em-and (em-equal? 'x 'u) (em-equal? 'y 'v)))
    ((em-equal '#(x ...) '#(u ...))
     (em-and (em-equal? 'x 'u) ...))
    ((em-equal '() '())
     '#t)
    ((em-equal 'x 'u)
     (em-if (em-symbol? 'x)
	    (em-bound-identifier=? 'x 'u)
	    (em-constant=? 'x 'u)))
    ((em-equal '_ '_)
     '#f)))

;; List processing

(define-syntax em-caar
  (em-syntax-rules ()
    ((em-caar '((a . b) . c))
     'a)))

(define-syntax em-cadr
  (em-syntax-rules ()
    ((em-cadr '(a . (b . c)))
     'b)))

(define-syntax em-cdar
  (em-syntax-rules ()
    ((em-cdar '((a . b) . c))
     'b)))

(define-syntax em-cddr
  (em-syntax-rules ()
    ((em-cddr '(a . (b . c)))
     'c)))

(define-syntax em-first
  (em-syntax-rules ()
    ((em-first '(a . z))
     'a)))

(define-syntax em-second
  (em-syntax-rules ()
    ((em-second '(a b . z))
     'b)))

(define-syntax em-third
  (em-syntax-rules ()
    ((em-third '(a b c . z))
     'c)))

(define-syntax em-fourth
  (em-syntax-rules ()
    ((em-forth '(a b c d . z))
     'd)))

(define-syntax em-fifth
  (em-syntax-rules ()
    ((em-fifth '(a b c d e . z))
     'e)))

(define-syntax em-sixth
  (em-syntax-rules ()
    ((em-sixth '(a b c d e f . z))
     'f)))

(define-syntax em-seventh
  (em-syntax-rules ()
    ((em-seventh '(a b c d e f g . z))
     'g)))

(define-syntax em-eighth
  (em-syntax-rules ()
    ((em-eighth '(a b c d e f g h . z))
     'h)))

(define-syntax em-ninth
  (em-syntax-rules ()
    ((em-ninth '(a b c d e f g h i . z))
     'i)))

(define-syntax em-tenth
  (em-syntax-rules ()
    ((em-tenth '(a b c d e f g h i j . z))
     'j)))

(define-syntax em-make-list
  (em-syntax-rules ()
    ((em-make-list '() 'fill)
     '())
    ((em-make-list '(h . t) 'fill)
     (em-cons 'fill (em-make-list 't 'fill)))))

(define-syntax em-reverse
  (em-syntax-rules ()
    ((em-reverse '())
     '())
    ((em-reverse '(h ... t))
     (em-cons 't (em-reverse '(h ...))))))

(define-syntax em-list-tail
  (em-syntax-rules ()
    ((em-list-tail 'list '())
     'list)
    ((em-list-tail '(h . t) '(u . v))
     (em-list-tail 't 'v))))

(define-syntax em-drop
  (em-syntax-rules ()
    ((em-drop 'arg ...)
     (em-list-ref 'arg ...))))

(define-syntax em-list-ref
  (em-syntax-rules ()
    ((em-list-ref '(h . t) '())
     'h)
    ((em-list-ref '(h . t) '(u . v))
     (em-list-ref 't 'v))))

(define-syntax em-take
  (em-syntax-rules ()
    ((em-take '_ '())
     '())
    ((em-take '(h . t) '(u . v))
     (em-cons 'h (em-take 't 'v)))))

(define-syntax em-take-right
  (em-syntax-rules ()
    ((em-take-right '(a ... . t) '())
     't)
    ((em-take-right '(a ... b . t) '(u . v))     
     `(,@(em-take-right '(a ...) 'v) b . t)
     )))

(define-syntax em-drop-right
  (em-syntax-rules ()
    ((em-drop-right '(a ... . t) '())
     '(a ...))
    ((em-drop-right '(a ... b . t) '(u . v))
     (em-drop-right '(a ...) 'v))))

(define-syntax em-last
  (em-syntax-rules ()
    ((em-last '(a ... b . t))
     'b)))

(define-syntax em-last-pair
  (em-syntax-rules ()
    ((em-last '(a ... b . t))
     '(b . t))))

;; Folding, unfolding and mapping

(define-syntax em-fold
  (em-syntax-rules ()
    ((em-fold 'kons 'knil '(h . t) ...)
     (em-fold 'kons (kons 'h ... 'knil) 't ...))
    ((em-fold 'kons 'knil '_ ...)
     'knil)))

(define-syntax em-fold-right
  (em-syntax-rules ()
    ((em-fold-right 'kons 'knil '(h . t) ...)
     (kons 'h ... (em-fold-right 'kons 'knil 't ...)))
    ((em-fold-right 'kons 'knil '_ ...)
     'knil)))

(define-syntax em-unfold
  (em-syntax-rules ()
    ((em-unfold 'stop? 'mapper 'successor 'seed 'tail-mapper)
     (em-if (stop? 'seed)
	    (tail-mapper 'seed)
	    (em-cons (mapper 'seed)
		     (em-unfold 'stop? 'mapper 'successor (successor 'seed) 'tail-mapper))))
    ((em-unfold 'stop? 'mapper 'successor 'seed)
     (em-unfold 'stop? 'mapper 'successor 'seed (em-constant '())))))

(define-syntax em-unfold-right
  (em-syntax-rules ()
    ((em-unfold-right 'stop? 'mapper 'successor 'seed 'tail)
     (em-if (stop? 'seed)
	    'tail
	    (em-unfold-right 'stop?
			     'mapper
			     'successor
			     (successor 'seed)
			     (em-cons (mapper 'seed) 'tail))))
    ((em-unfold-right 'stop? 'mapper 'successor 'seed)
     (em-unfold-right 'stop? 'mapper 'successor 'seed '()))))

(define-syntax em-map
  (em-syntax-rules ()
    ((em-map 'proc '(h . t) ...)
     (em-cons (proc 'h ...) (em-map 'proc 't ...)))
    ((em-map 'proc '_ ...)
     '())))

(define-syntax em-append-map
  (em-syntax-rules ()
    ((em-append-map 'proc '(h . t) ...)
     (em-append (proc 'h ...) (em-append-map 'proc 't ...)))
    ((em-append-map map 'proc '_)
     '())))

;; Filtering

(define-syntax em-filter
  (em-syntax-rules ()
    ((em-filter 'pred '())
     '())
    ((em-filter 'pred '(h . t))
     (em-if (pred 'h)
	    (em-cons 'h (em-filter 'pred 't))
	    (em-filter 'pred 't)))))

(define-syntax em-remove
  (em-syntax-rules ()
    ((em-remove 'pred '())
     '())
    ((em-remove 'pred '(h . t))
     (em-if (pred 'h)
	    (em-remove 'pred 't)
	    (em-cons 'h (em-remove 'pred 't))))))

;; Searching

(define-syntax em-find
  (em-syntax-rules ()
    ((em-find 'pred '())
     '())
    ((em-find 'pred '(h . t))
     (em-if (pred 'h)
	    'h
	    (em-find 'pred 't)))))

(define-syntax em-find-tail
  (em-syntax-rules ()
    ((em-find-tail 'pred '())
     '#f)
    ((em-find-tail 'pred '(h . t))
     (em-if (pred 'h)
	    '(h . t)
	    (em-find-tail 'pred 't)))))

(define-syntax em-take-while
  (em-syntax-rules ()
    ((em-take-while 'pred '())
     '())
    ((em-take-while 'pred '(h . t))
     (em-if (pred 'h)
	    (em-cons 'h (em-take-while 'pred 't))
	    '()))))

(define-syntax em-drop-while
  (em-syntax-rules ()
    ((em-drop-while 'pred '())
     '())
    ((em-drop-while 'pred '(h . t))
     (em-if (pred 'h)
	    (em-drop-while 'pred 't)
	    '(h . t)))))

(define-syntax em-any
  (em-syntax-rules ()
    ((em-any 'pred '(h . t) ...)
     (em-or (pred 'h ...) (em-any 'pred 't ...)))
    ((em-any 'pred '_ ...)
     '#f)))

(define-syntax em-every
  (em-syntax-rules ()
    ((em-every 'pred '() ...)
     '#t)
    ((em-every 'pred '(a b . x) ...)
     (em-and (pred 'a ...) (em-every 'pred '(b . x) ...)))
    ((em-every 'pred '(h . t) ...)
     (pred 'h ...))))

(define-syntax em-member
  (em-syntax-rules ()
    ((em-member 'obj 'list 'compare)
     (em-find-tail (em-cut 'compare 'obj <>) 'list))
    ((em-member 'obj 'list)
     (em-member 'obj 'list 'em-equal?))))

;; Association lists

(define-syntax em-assoc
  (em-syntax-rules ()
    ((em-assoc 'key '() 'compare)
     '#f)
    ((em-assoc 'key '((k . v) . t) 'compare)
     (em-if (compare 'key 'k)
	    '(k . v)
	    (em-assoc 'key 't 'compare)))
    ((em-assoc 'key 'alist)
     (em-assoc 'key 'alist 'em-equal?))))

(define-syntax em-alist-delete
  (em-syntax-rules ()
    ((em-alist-delete 'key '() 'compare)
     '())
    ((em-alist-delete 'key '((k . v) . t) 'compare)
     (em-if (compare 'key 'k)
	    (em-alist-delete 'key 't 'compare)
	    (em-cons '(k . v) (em-alist-delete 'key 't 'compare))))
    ((em-alist-delete 'key 'alist)
     (em-alist-delete 'key 'alist 'em-equal?))))

;; Set operations

(define-syntax em-set<=
  (em-syntax-rules ()
    ((em-set<= 'compare '())
     '#t)
    ((em-set<= 'compare 'list)
     '#t)
    ((em-set<= 'compare '() 'list)
     '#t)
    ((em-set<= 'compare '(h . t) 'list)
     (em-and (em-member 'h 'list 'compare)
	     (em-set<= 'compare 't 'list)))
    ((em-set<= 'compare 'list1 'list2 'list ...)
     (em-and (em-set<= 'compare 'list1 'list2)
	     (em-set<= 'compare 'list2 'list ...)))))

(define-syntax em-set=
  (em-syntax-rules ()
    ((em-set= 'compare 'list)
     '#t)
    ((em-set= 'compare 'list1 list2)
     (em-and (em-set<= 'compare 'list1 'list2)
	     (em-set<= 'compare 'list2 'list1)))
    ((em-set= 'compare 'list1 'list2 'list ...)
     (em-and (em-set= 'list1 'list2)
	     (em-set= 'list1 'list ...)))))

(define-syntax em-set-adjoin
  (em-syntax-rules ()
    ((em-set-adjoin 'compare 'list)
     'list)
    ((em-set-adjoin 'compare 'list 'element1 'element2 ...)
     (em-if (em-member 'element1 'list 'compare)
	    (em-set-adjoin 'compare 'list 'element2 ...)
	    (em-set-adjoin 'compare (em-cons 'element1 'list) 'element2 ...)))))

(define-syntax em-set-union
  (em-syntax-rules ()
    ((em-set-union 'compare 'list ...)
     (em-apply 'em-set-adjoin 'compare '() (em-append 'list ...)))))

(define-syntax em-set-intersection
  (em-syntax-rules ()
    ((em-set-intersection 'compare 'list)
     'list)
    ((em-set-intersection 'compare 'list1 'list2)
     (em-filter (em-cut 'em-member <> 'list2 'compare) 'list1))
    ((em-set-intersection 'compare 'list1 'list2 'list ...)
     (em-set-intersection 'compare (em-set-intersection 'list1 'list2) 'list ...))))

(define-syntax em-set-difference
  (em-syntax-rules ()
    ((em-set-difference 'compare 'list)
     'list)
    ((em-set-difference 'compare 'list1 'list2)
     (em-remove (em-cut 'em-member <> 'list2 'compare) 'list1))
    ((em-set-difference 'compare 'list1 'list2 'list ...)
     (em-set-difference 'compare (em-set-difference 'list1 'list2) 'list ...))))

(define-syntax em-set-xor
  (em-syntax-rules ()
    ((em-set-xor 'compare 'list1 'list2)
     (em-set-union 'compare
		   (em-set-difference 'compare 'list1 'list2)
		   (em-set-difference 'compare 'list2 'list1)))))

;; Vector processing

(define-syntax em-vector
  (em-syntax-rules ()
    ((em-vector 'element ...)
     '#(element ...))))

(define-syntax em-list->vector
  (em-syntax-rules ()
    ((em-list->vector '(element ...))
     '#(element ...))))

(define-syntax em-vector->list
  (em-syntax-rules ()
    ((em-list->vector '#(x ...))
     '(x ...))))

(define-syntax em-vector-map
  (em-syntax-rules ()
    ((em-vector-map 'proc 'vector ...)
     (em-list->vector (em-map 'proc (em-vector->list 'vector) ...)))))

(define-syntax em-vector-ref
  (em-syntax-rules ()
    ((em-vector-ref '#(element1 element2 ...) '())
     'element1)
    ((em-vector-ref '#(element1 element2 ...) '(h . t))
     (em-vector-ref '#(element2 ...) 't))))

;; Combinatorics

(define-syntax em-0
  (em-syntax-rules ()
    ((em-0)
     '())))

(define-syntax em-1
  (em-syntax-rules ()
    ((em-1)
     '(0))))

(define-syntax em-2
  (em-syntax-rules ()
    ((em-2)
     '(0 1))))

(define-syntax em-3
  (em-syntax-rules ()
    ((em-3)
     '(0 1 2))))

(define-syntax em-4
  (em-syntax-rules ()
    ((em-4)
     '(0 1 2 3))))

(define-syntax em-5
  (em-syntax-rules ()
    ((em-5)
     '(0 1 2 3 4))))

(define-syntax em-6
  (em-syntax-rules ()
    ((em-6)
     '(0 1 2 3 4 5))))

(define-syntax em-7
  (em-syntax-rules ()
    ((em-7)
     '(0 1 2 3 4 5 6))))

(define-syntax em-8
  (em-syntax-rules ()
    ((em-8)
     '(0 1 2 3 4 5 6 7))))

(define-syntax em-9
  (em-syntax-rules ()
    ((em-9)
     '(0 1 2 3 4 5 6 7 8))))

(define-syntax em-10
  (em-syntax-rules ()
    ((em-10)
     '(0 1 2 3 4 5 6 7 8 9))))

(define-syntax em=
  (em-syntax-rules ()
    ((em= '_)
     '#t)
    ((em= '() '())
     '#t)
    ((em= '(h . t) '())
     '#f)
    ((em= '() '(h . t))
     '#f)
    ((em= '(h . t) '(u . v))
     (em= 't 'v))
    ((em= 'list1 'list2 'list ...)
     (em-and (em= 'list1 'list2)
	     (em= 'list1 'list ...)))))

(define-syntax em<
  (em-syntax-rules ()
    ((em<)
     '#t)
    ((em< 'list)
     '#t)
    ((em< '_ '())
     '#f)
    ((em< '() '_)
     '#t)
    ((em< '(t . h) '(u . v))
     (em< 'h 'v))
    ((em< 'list1 'list2 'list ...)
     (em-and (em< 'list1 'list2)
	     (em< 'list2 'list ...)))))

(define-syntax em<=
  (em-syntax-rules ()
    ((em<=)
     '#t)
    ((em<= 'list)
     '#t)
    ((em<= '() '_)
     '#t)
    ((em<= '_ '())
     '#f)
    ((em<= '(t . h) '(u . v))
     (em<= 'h 'v))
    ((em<= 'list1 'list2 'list ...)
     (em-and (em<= 'list1 'list2)
	     (em<= 'list2 'list ...)))))

(define-syntax em>
  (em-syntax-rules ()
    ((em> 'list ...)
     (em-apply 'em< (em-reverse '(list ...))))))

(define-syntax em>=
  (em-syntax-rules ()
    ((em>= 'list ...)
     (em-apply 'em<= (em-reverse '(list ...))))))

(define-syntax em-zero? em-null?)

(define-syntax em-even?
  (em-syntax-rules ()
    ((em-even? '())
     '#t)
    ((em-even? '(a b . c))
     (em-even? 'c))
    ((em-even? '_)
     '#f)))

(define-syntax em-odd?
  (em-syntax-rules ()
    ((em-odd? 'list)
     (em-not (em-even? 'list)))))

(define-syntax em+ em-append)

(define-syntax em-
  (em-syntax-rules ()
    ((em- 'list)
     'list)
    ((em- 'list '())
     'list)
    ((em- '(a ... b) '(u . v))
     (em- '(a ...) 'v))
    ((em- 'list1 'list2 'list ...)
     (em- (em- 'list1 'list2) 'list ...))))

(define-syntax em*
  (em-syntax-rules ()
    ((em*)
     '(()))
    ((em* 'list1 'list2 ...)
     (em*-aux 'list1 (em* 'list2 ...)))))

(define-syntax em*-aux
  (em-syntax-rules ()
    ((em*-aux '(x ...) 'list)
     (em-append (em-map (em-cut 'em-cons 'x <>) 'list) ...))))

(define-syntax em-quotient
  (em-syntax-rules ()
    ((em-quotient 'list 'k)
     (em-if (em>= 'list 'k)
	    (em-cons (em-car 'list)
		     (em-quotient (em-list-tail 'list 'k) 'k))
	    '()))))

(define-syntax em-remainder
  (em-syntax-rules ()
    ((em-quotient 'list 'k)
     (em-if (em>= 'list 'k)
	    (em-remainder (em-list-tail 'list 'k) 'k)
	    'list))))

(define-syntax em-binom
  (em-syntax-rules ()
    ((em-binom 'list '())
     '(()))
    ((em-binom '() '(h . t))
     '())
    ((em-binom '(u . v) '(h . t))
     (em-append (em-map (em-cut 'em-cons u <>) (em-binom 'v 't))
		(em-binom 'v '(h . t))))))

(define-syntax em-fact
  (em-syntax-rules ()
    ((em-fact '())
     '(()))
    ((em-fact 'list)
     (em-append-map 'em-fact-cons*
		    'list (em-map 'em-fact (em-fact-del 'list))))))

(define-syntax em-fact-del
  (em-syntax-rules ()
    ((em-fact-del '())
     '())
    ((em-fact-del '(h . t))
     `(t ,@(em-map (em-cut 'em-cons 'h <>) (em-fact-del 't))))))

(define-syntax em-fact-cons*
  (em-syntax-rules ()
    ((em-fact-cons* 'a '((l ...) ...))
     '((a l ...) ...))))
 
