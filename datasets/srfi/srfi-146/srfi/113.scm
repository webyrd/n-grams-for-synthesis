;; Copyright (C) Marc Nieper-Wißkirchen (2017).  All Rights
;; Reserved.

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

(define-record-type <set>
  (make-set mapping)
  set?
  (mapping set-mapping))

(define-record-type <bag>
  (make-bag mapping)
  bag?
  (mapping bag-mapping))

(define (make-empty-set comparator)
  (make-set (mapping comparator)))

(define (make-empty-bag comparator)
  (make-bag (mapping comparator)))

;; Constructors

(define (set comparator . elements)
  (assume (comparator? comparator))
  (set-unfold null? car cdr elements comparator))

(define (bag comparator . elements)
  (assume (comparator? comparator))
  (bag-unfold null? car cdr elements comparator))

(define (set-unfold stop? mapper successor seed comparator)
  (assume (procedure? stop?))
  (assume (procedure? mapper))
  (assume (procedure? successor))
  (assume (comparator? comparator))
  (make-set (mapping-unfold stop?
			    (lambda (seed)
			      (values (mapper seed) 1))
			    successor
			    seed
			    comparator)))

(define (bag-unfold stop? mapper successor seed comparator)
  (assume (procedure? stop?))
  (assume (procedure? mapper))
  (assume (procedure? successor))
  (assume (comparator? comparator))
  (let loop ((bag (make-empty-bag comparator))
	     (seed seed))
    (if (stop? seed)
	bag
	(loop (bag-adjoin bag (mapper seed))
	      (successor seed)))))

;; Predicates

(define (set-contains? set element)
  (assume (set? set))
  (mapping-contains? (set-mapping set) element))

(define (bag-contains? bag element)
  (assume (bag? bag))
  (mapping-contains? (bag-mapping bag) element))

(define (set-empty? set)
  (assume (set? set))
  (mapping-empty? (set-mapping set)))

(define (bag-empty? bag)
  (assume (bag? bag))
  (mapping-empty? (bag-mapping bag)))

(define (set-disjoint? set1 set2)
  (assume (set? set1))
  (assume (set? set2))
  (mapping-disjoint? (set-mapping set1) (set-mapping set2)))

(define (bag-disjoint? bag1 bag2)
  (assume (bag? bag1))
  (assume (bag? bag2))
  (mapping-disjoint? (bag-mapping bag1) (bag-mapping bag2)))

;; Accessors

(define (set-member set element default)
  (assume (set? set))
  (call/cc
   (lambda (return)
     (mapping-search (set-mapping set)
		     element
		     (lambda (insert ignore)
		       (return default))
		     (lambda (old-element old-count update remove)
		       (return old-element))))))

(define (bag-member bag element default)
  (assume (bag? bag))
  (call/cc
   (lambda (return)
     (mapping-search (bag-mapping bag)
		     element
		     (lambda (insert ignore)
		       (return default))
		     (lambda (old-element old-count update remove)
		       (return old-element))))))

(define (set-element-comparator set)
  (assume (set? set))
  (mapping-key-comparator (set-mapping set)))

(define (bag-element-comparator bag)
  (assume (bag? bag))
  (mapping-key-comparator (bag-mapping bag)))

;; Updaters

(define (set-adjoin set . elements)
  (assume (set? set))
  (make-set (fold (lambda (element mapping)
		    (receive (mapping value)
			(mapping-intern mapping element (lambda () 1))
		      mapping))
		  (set-mapping set) elements)))

(define (bag-adjoin bag . elements)
  (assume (bag? bag))
  (fold (lambda (element bag)
	  (bag-increment bag element 1))
	bag elements))

(define set-adjoin! set-adjoin)
(define bag-adjoin! bag-adjoin)

(define (set-replace set element)
  (assume (set? set))
  (make-set (mapping-replace (set-mapping set) element 1)))

(define (bag-replace bag element)
  (assume (bag? bag))
  (receive (mapping obj)
      (mapping-search (bag-mapping bag)
		      element
		      (lambda (insert ignore)
			(ignore #f))
		      (lambda (old-element count update remove)
			(update element count #f)))
    (make-bag mapping)))

(define set-replace! set-replace)
(define bag-replace! bag-replace)

(define (set-delete set . elements)
  (assume (set? set))
  (set-delete-all set elements))

(define (bag-delete bag . elements)
  (assume (bag? bag))
  (bag-delete-all bag elements))

(define set-delete! set-delete)
(define bag-delete! bag-delete)

(define (set-delete-all set elements)
  (assume (set? set))
  (make-set (mapping-delete-all (set-mapping set) elements)))

(define (bag-delete-all bag elements)
  (assume (bag? bag))
  (make-bag (mapping-delete-all (bag-mapping bag) elements)))

(define set-delete-all! set-delete-all)
(define bag-delete-all! bag-delete-all)

(define (set-search! set element failure success)
  (assume (set? set))
  (assume (procedure? failure))
  (assume (procedure? success))
  (values set #f)
  (call/cc
   (lambda (return)
     (receive (mapping obj)
	 (mapping-search (set-mapping set)
			 element
			 (lambda (insert ignore)
			   (failure (lambda (obj)
				      (insert 1 obj))
				    ignore))
			 (lambda (old-element count update remove)
			   (remove #f)
			   (success old-element
				    (lambda (new-element obj)
				      (if (=? (set-element-comparator set)
					      old-element
					      new-element)
					  (update new-element count obj)
					  (return (set-adjoin (set-delete set old-element)
							      new-element)
						  obj)))
				    remove)))
       (values (make-set mapping) obj)))))

(define (bag-search! bag element failure success)
  (assume (bag? bag))
  (assume (procedure? failure))
  (assume (procedure? success))
  (call/cc
   (lambda (return)
     (receive (mapping obj)
	 (mapping-search (bag-mapping bag)
			 element
			 (lambda (insert ignore)
			   (failure (lambda (obj)
				      (insert 1 obj))
				    ignore))
			 (lambda (old-element old-count update remove)
			   (success old-element
				    (lambda (new-element obj)
				      (if (=? (bag-element-comparator bag)
					      old-element
					      new-element)
					  (update new-element old-count obj)
					  (return (bag-adjoin (bag-delete bag old-element)
							      new-element)
						  obj)))
				    (lambda (obj)
				      (let ((new-count (- old-count 1)))
					(if (zero? new-count)
					    (remove obj)
					    (update old-element new-count obj)))))))
       (values (make-bag mapping) obj)))))

;; The whole set

(define (set-size set)
  (assume (set? set))
  (mapping-size (set-mapping set)))

(define (bag-size bag)
  (assume (bag? bag))
  (mapping-fold (lambda (element count acc)
		  (+ count acc))
		0 (bag-mapping bag)))

(define (set-find predicate set failure)
  (assume (procedure? predicate))
  (assume (set? set))
  (assume (procedure? failure))
  ((call/cc
    (lambda (return-thunk) 
      (receive (element count)
	  (mapping-find (lambda (element count)
			  (predicate element))
			(set-mapping set)
			(lambda () (return-thunk failure)))
	(lambda () element))))))

(define (bag-find predicate bag failure)
  (assume (procedure? predicate))
  (assume (bag? bag))
  (assume (procedure? failure))
  ((call/cc
    (lambda (return-thunk) 
      (receive (element count)
	  (mapping-find (lambda (element count)
			  (predicate element))
			(bag-mapping bag)
			(lambda () (return-thunk failure)))
	(lambda () element))))))

(define (set-count predicate set)
  (assume (procedure? predicate))
  (assume (set? set))
  (mapping-count (lambda (element count)
		   (predicate element))
		 (set-mapping set)))

(define (bag-count predicate bag)
  (assume (procedure? predicate))
  (assume (bag? bag))
  (mapping-fold (lambda (element count acc)
		  (if (predicate element)
		      (+ count acc)
		      acc))
		0 (bag-mapping bag)))

(define (set-any? predicate set)
  (assume (procedure? predicate))
  (assume (set? set))
  (mapping-any? (lambda (element count)
		  (predicate element))
		(set-mapping set)))

(define (bag-any? predicate bag)
  (assume (procedure? predicate))
  (assume (bag? bag))
  (mapping-any? (lambda (element count)
		  (predicate element))
		(bag-mapping bag)))

(define (set-every? predicate set)
  (assume (procedure? predicate))
  (assume (set? set))
  (mapping-every? (lambda (element count)
		    (predicate element))
		  (set-mapping set)))

(define (bag-every? predicate bag)
  (assume (procedure? predicate))
  (assume (bag? bag))
  (mapping-every? (lambda (element count)
		    (predicate element))
		  (bag-mapping bag)))

;; Mapping and folding

(define (set-map proc comparator set)
  (assume (procedure? proc))
  (assume (comparator? comparator))
  (assume (set? set))
  (make-set (mapping-map (lambda (element count)
			   (values (proc element)
				   count))
			 (set-element-comparator set)
			 (set-mapping set))))

(define (bag-map proc comparator bag)
  (assume (procedure? proc))
  (assume (comparator? comparator))
  (assume (bag? bag))
  (mapping-fold (lambda (element count bag)
		  (let loop ((count count) (bag bag))
		    (if (zero? count)
			bag
			(loop (- count 1) (bag-adjoin bag (proc element))))))
		(make-empty-bag comparator) (bag-mapping bag)))

(define (set-for-each proc set)
  (assume (procedure? proc))
  (assume (set? set))
  (mapping-for-each (lambda (element count)
		      (proc element))
		    (set-mapping set)))

(define (bag-for-each proc bag)
  (assume (procedure? proc))
  (assume (bag? bag))
  (mapping-for-each (lambda (element count)
		      (do ((count count (- count 1)))
			  ((zero? count))
			(proc element)))
		    (bag-mapping bag)))

(define (set-fold proc nil set)
  (assume (procedure? proc))
  (assume (set? set))
  (mapping-fold (lambda (element count nil)
		  (proc element nil))
		nil (set-mapping set)))

(define (bag-fold proc nil bag)
  (assume (procedure? proc))
  (assume (bag? bag))
  (mapping-fold (lambda (element count acc)
		  (let loop ((count count) (acc acc))
		    (if (zero? count)
			acc
			(loop (- count 1) (proc element acc)))))
		nil (bag-mapping bag)))

(define (set-filter predicate set)
  (assume (procedure? predicate))
  (assume (set? set))
  (make-set (mapping-filter (lambda (element count)
			      (predicate element))
			    (set-mapping set))))

(define (bag-filter predicate bag)
  (assume (procedure? predicate))
  (assume (bag? bag))
  (make-bag (mapping-filter (lambda (element count)
			      (predicate element))
			    (bag-mapping bag))))

(define set-filter! set-filter)
(define bag-filter! bag-filter)

(define (set-remove predicate set)
  (assume (procedure? predicate))
  (assume (set? set))
  (make-set (mapping-remove (lambda (element count)
			      (predicate element))
			    (set-mapping set))))

(define (bag-remove predicate bag)
  (assume (procedure? predicate))
  (assume (bag? bag))
  (make-bag (mapping-remove (lambda (element count)
			      (predicate element))
			    (bag-mapping bag))))

(define set-remove! set-remove)
(define bag-remove! bag-remove)

(define (set-partition predicate set)
  (assume (procedure? predicate))
  (assume (set? set))
  (receive (mapping1 mapping2)
      (mapping-partition (lambda (element count)
			(predicate element))
			 (set-mapping set))
    (values (make-set mapping1) (make-set mapping2))))

(define (bag-partition predicate bag)
  (assume (procedure? predicate))
  (assume (bag? bag))
  (receive (mapping1 mapping2)
      (mapping-partition (lambda (element count)
			(predicate element))
			 (bag-mapping bag))
    (values (make-bag mapping1) (make-bag mapping2))))

(define set-partition! set-partition)
(define bag-partition! bag-partition)

;; Copying and conversion

(define (set-copy set)
  (assume (set? set))
  set)

(define (bag-copy bag)
  (assume (bag? bag))
  bag)

(define (set->list set)
  (assume (set? set))
  (mapping-fold/reverse (lambda (element count lst)
			  (cons element lst))
			'() (set-mapping set)))

(define (bag->list bag)
  (assume (bag? bag))
  (mapping-fold/reverse (lambda (element count lst)
			  (let loop ((count count) (lst lst))
			    (if (zero? count)
				lst
				(loop (- count 1)
				      (cons element lst)))))
			'() (bag-mapping bag)))

(define (list->set comparator lst)
  (assume (comparator? comparator))
  (assume (list? lst))
  (apply set comparator lst))

(define (list->bag comparator lst)
  (assume (comparator? comparator))
  (assume (list? lst))
  (apply bag comparator lst))

(define (list->set! set lst)
  (assume (set? set))
  (assume (list? lst))
  (apply set-adjoin set lst))

(define (list->bag! bag lst)
  (assume (bag? bag))
  (assume (list? lst))
  (apply bag-adjoin bag lst))

;; Subsets

(define (set=? set . sets)
  (assume (set? set))
  (apply mapping=? (make-eqv-comparator) (set-mapping set) (map set-mapping sets)))

(define (bag=? bag . bags)
  (assume (bag? bag))
  (apply mapping=? (make-eqv-comparator) (bag-mapping bag) (map bag-mapping bags)))

(define (set<? set . sets)
  (assume (set? set))
  (apply mapping<? (make-eqv-comparator) (set-mapping set) (map set-mapping sets)))

(define (set>? set . sets)
  (assume (set? set))
  (apply mapping>? (make-eqv-comparator) (set-mapping set) (map set-mapping sets)))

(define (set<=? set . sets)
  (assume (set? set))
  (apply mapping<=? (make-eqv-comparator) (set-mapping set) (map set-mapping sets)))

(define (set>=? set . sets)
  (assume (set? set))
  (apply mapping>=? (make-eqv-comparator) (set-mapping set) (map set-mapping sets)))

(define bag<=?
  (case-lambda
    ((bag)
     (assume (bag? bag))
     #t)
    ((bag1 bag2)
     (assume (bag? bag1))
     (assume (bag? bag2))
     (%bag<=? bag1 bag2))
    ((bag1 bag2 . bags)
     (assume (bag? bag1))
     (assume (bag? bag2))
     (and (%bag<=? bag1 bag2)
          (apply bag<=? bag2 bags)))))

(define (%bag<=? bag1 bag2)
  (assume (bag? bag1))
  (assume (bag? bag2))
  (let ((less? (comparator-ordering-predicate (bag-element-comparator bag1)))
	(gen1 (bag-generator bag1))
	(gen2 (bag-generator bag2)))
    (let loop ((item1 (gen1))
	       (item2 (gen2)))
      (cond
       ((eof-object? item1)
	#t)
       ((eof-object? item2)
	#f)
       (else
	(let ((key1 (car item1)) (value1 (cadr item1))
	      (key2 (car item2)) (value2 (cadr item2)))
	  (cond
	   ((less? key1 key2)
	    #f)
	   ((less? key2 key1)
	    (loop item1 (gen2)))
	   ((<= value1 value2)
	    (loop (gen1) (gen2)))
	   (else
	    #f))))))))

(define bag>?
  (case-lambda
    ((bag)
     (assume (bag? bag))
     #t)
    ((bag1 bag2)
     (assume (bag? bag1))
     (assume (bag? bag2))
     (%bag>? bag1 bag2))
    ((bag1 bag2 . bags)
     (assume (bag? bag1))
     (assume (bag? bag2))
     (and (%bag>? bag1 bag2)
          (apply bag>? bag2 bags)))))

(define (%bag>? bag1 bag2)
  (assume (bag? bag1))
  (assume (bag? bag2))
  (not (%bag<=? bag1 bag2)))

(define bag<?
  (case-lambda
    ((bag)
     (assume (bag? bag))
     #t)
    ((bag1 bag2)
     (assume (bag? bag1))
     (assume (bag? bag2))
     (%bag<? bag1 bag2))
    ((bag1 bag2 . bags)
     (assume (bag? bag1))
     (assume (bag? bag2))
     (and (%bag<? bag1 bag2)
          (apply bag<? bag2 bags)))))

(define (%bag<? bag1 bag2)
  (assume (bag? bag1))
  (assume (bag? bag2))
  (%bag>? bag2 bag1))

(define bag>=?
  (case-lambda
    ((bag)
     (assume (bag? bag))
     #t)
    ((bag1 bag2)
     (assume (bag? bag1))
     (assume (bag? bag2))
     (%bag>=? bag1 bag2))
    ((bag1 bag2 . bags)
     (assume (bag? bag1))
     (assume (bag? bag2))
     (and (%bag>=? bag1 bag2)
          (apply bag>=? bag2 bags)))))

(define (%bag>=? bag1 bag2)
  (assume (bag? bag1))
  (assume (bag? bag2))
  (not (%bag<? bag1 bag2)))

(define (bag-generator bag)
  (make-coroutine-generator
   (lambda (yield)
     (mapping-for-each (lambda item (yield item)) (bag-mapping bag)))))

;; Set theory operations

(define (set-union set . sets)
  (assume (set? set))
  (make-set (apply mapping-union (set-mapping set) (map set-mapping sets))))

(define (set-intersection set . sets)
  (assume (set? set))
  (make-set (apply mapping-intersection (set-mapping set) (map set-mapping sets))))

(define (set-difference set . sets)
  (assume (set? set))
  (make-set (apply mapping-difference (set-mapping set) (map set-mapping sets))))

(define (set-xor set1 set2)
  (assume (set? set1))
  (assume (set? set2))  
  (make-set (mapping-xor (set-mapping set1) (set-mapping set2))))

(define set-union! set-union)
(define set-intersection! set-intersection)
(define set-difference! set-difference)
(define set-xor! set-xor)

(define (bag-union bag . bags)
  (assume (bag? bag))
  (fold (lambda (bag2 bag1)
	  (mapping-fold (lambda (element count bag)
			  (bag-update bag element (lambda (old-count) (max old-count count))))
			bag1 (bag-mapping bag2)))
	bag bags))

(define (bag-intersection bag . bags)
  (assume (bag? bag))
  (fold (lambda (bag2 bag1)
	  (%bag-intersection bag1 bag2))
	bag bags))

(define (%bag-intersection bag1 bag2)
  (assume (bag? bag1))
  (assume (bag? bag2))
  (let ((less? (comparator-ordering-predicate (bag-element-comparator bag1)))
	(gen1 (bag-generator bag1))
	(gen2 (bag-generator bag2)))
    (let loop ((item1 (gen1))
	       (item2 (gen2))
	       (bag (make-empty-bag (bag-element-comparator bag1))))
      (cond
       ((eof-object? item1)
	bag)
       ((eof-object? item2)
	bag)
       (else
	(let ((key1 (car item1)) (count1 (cadr item1))
	      (key2 (car item2)) (count2 (cadr item2)))
	  (cond
	   ((less? key1 key2)
	    (loop (gen1) item2 bag))
	   ((less? key2 key1)
	    (loop item1 (gen2) bag))
	   (else
	    (loop (gen1) (gen2) (bag-increment bag key1 (min count1 count2)))))))))))

(define (bag-difference bag . bags)
  (assume (bag? bag))
  (fold (lambda (bag2 bag1)
	  (mapping-fold (lambda (element count bag)
			  (bag-update bag element (lambda (old-count) (max 0 (- old-count count)))))
			bag1 (bag-mapping bag2)))
	bag bags))

(define (bag-xor bag1 bag2)
  (assume (bag? bag1))
  (assume (bag? bag2))
  (mapping-fold (lambda (element count bag)
		  (bag-update bag element (lambda (old-count) (abs (- old-count count)))))
		bag1 (bag-mapping bag2)))

(define bag-union! bag-union)
(define bag-intersection! bag-intersection)
(define bag-difference! bag-difference)
(define bag-xor! bag-xor)

(define (bag-update bag element updater)
  (receive (mapping obj)
      (mapping-search (bag-mapping bag)
		      element
		      (lambda (insert ignore)
			(let ((new-count (updater 0)))
			  (if (zero? new-count)
			      (ignore #f)
			      (insert new-count #f))))
		      (lambda (old-element old-count update remove)
			(let ((new-count (updater old-count)))
			  (if (zero? new-count)
			      (remove #f)
			      (update element new-count #f)))))
    (make-bag mapping)))

;; Additional bag procedures

(define (bag-sum bag . bags)
  (assume (bag? bag))
  (if (null? bags)
      bag
      (mapping-fold (lambda (element count bag)
		      (bag-update bag element (lambda (old-count) (+ old-count count))))
		    bag (bag-mapping (car bags)))))

(define bag-sum! bag-sum)

(define (bag-product n bag)
  (assume (not (negative? n)))
  (assume (bag? bag))
  (make-bag (mapping-map (lambda (element count)
			   (values element (* n count)))
			 (bag-element-comparator bag)
			 (bag-mapping bag))))

(define bag-product! bag-product)

(define (bag-unique-size bag)
  (assume (bag? bag))
  (mapping-size (bag-mapping bag)))

(define (bag-element-count bag element)
  (assume (bag? bag))
  (mapping-ref/default (bag-mapping bag) element 0))

(define (bag-for-each-unique proc bag)
  (assume (bag? bag))
  (mapping-for-each proc (bag-mapping bag)))

(define (bag-fold-unique proc nil bag)
  (assume (procedure? proc))
  (assume (bag? bag))
  (mapping-fold proc nil (bag-mapping bag)))

(define (bag-increment bag element count)
  (assume (exact-integer? count))
  (assume (bag? bag))
  (bag-update bag element (lambda (old-count)
			    (max 0 (+ count old-count)))))

(define bag-increment! bag-increment)

(define (bag-decrement! bag element count)
  (assume (exact-integer? count))
  (assume (bag? bag))
  (bag-update bag element (lambda (old-count)
			    (max 0 (- old-count count)))))

(define (bag->set bag)
  (assume (bag? bag))
  (make-set (mapping-map (lambda (element count)
			   (values element 1))
			 (bag-element-comparator bag)
			 (bag-mapping bag))))

(define (set->bag set)
  (assume (set? set))
  (make-bag (set-mapping set)))

(define (set->bag! bag set)
  (set-fold (lambda (element bag)
	      (bag-adjoin! bag element))
	    bag set))

(define (bag->alist bag)
  (assume (bag? bag))
  (mapping->alist (bag-mapping bag)))

(define (alist->bag comparator alist)
  (assume (comparator? comparator))
  (assume (list? alist))
  (make-bag (alist->mapping comparator alist)))

;; Comparators

(define mapping-ordering
  (comparator-ordering-predicate (make-mapping-comparator (make-default-comparator))))

(define (set-ordering set1 set2)
  (mapping-ordering (set-mapping set1) (set-mapping set2)))

(define (bag-ordering bag1 bag2)
  (mapping-ordering (bag-mapping bag1) (bag-mapping bag2)))

(define set-comparator
  (make-comparator set? set=? set-ordering #f))

(define bag-comparator
  (make-comparator bag? bag=? bag-ordering #f))

(comparator-register-default! set-comparator)
(comparator-register-default! bag-comparator)
