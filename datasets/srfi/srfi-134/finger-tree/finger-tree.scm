
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Non-exported definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A finger tree conforms to one of three shapes:

;; 1) An *empty* tree contains nothing.
(define-record-type <empty>
  (make-empty)
  empty?)

;; 2) A *single* tree contains exactly one element.
(define-record-type <single>
  (make-single x)
  single?
  (x single-x))

;; 3) A *deep* tree contains:
;;
;;    a) a *left digit*, a list of 1-4 elements in left-to-right
;;       (forward) order;
;;
;;    b) a promise for a *spine*, a nested finger tree containing
;;       *nodes* (described below); and
;;
;;    c) a *right digit*, a list of 1-4 elements in right-to-left
;;       (backward) order.
;;
;; We use the convention that the identifier sp represents a Spine
;; Promise, while s represents the Spine object itself (i.e. s is
;; (force sp)).
;;
;; Observe:
;;
;;  - Each digit must be non-empty.
;;
;;  - A deep tree contains O(1) elements, not counting the spine.
;;
;;  - The leftmost element is at the front of the left digit, and the
;;    rightmost element is at the front of the right digit.
(define-record-type <deep>
  (make-deep l sp r)
  deep?
  (l deep-l)
  (sp deep-sp)
  (r deep-r))

;; A *node* conforms to one of two shapes:
;;
;; 1) A *node2* (binary node) containing two elements x and y, and a
;; cached measurement m of x and y; or
;;
;; 2) a *node3* (trinary node) containing three elements x, y, an z,
;; and a cached measurement m of x, y, and z.
(define-record-type <node2>
  (make-node2 m x y)
  node2?
  (m node2-m)
  (x node2-x)
  (y node2-y))

(define-record-type <node3>
  (make-node3 m x y z)
  node3?
  (m node3-m)
  (x node3-x)
  (y node3-y)
  (z node3-z))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WHY THIS IS EFFICIENT
;;
;; A deep node contains 2-8 elements on the left and right sides of
;; the structure; and a spine, which is a nested finger tree, where
;; *each element of the inner tree is a node containing 2-3 elements
;; of the outer tree.*
;;
;; Consider a deep tree which is large enough to be nested several
;; levels deep. We'll call each of its client-visible elements a
;; "native element."
;;
;; The outermost deep tree contains 2-8 native elements in the digits;
;; and a spine, where each element in the spine tree is a node of 2-3
;; native elements.
;;
;; The inner tree contains 2-8 nodes in the digits, corresponding to
;; 4-24 native elements; and a spine, where each element in the
;; inner-spine tree is a node-of-nodes containing 4-9 native elements.
;;
;; The inner-inner tree contains 2-8 nodes-of-nodes in the digits,
;; corresponding to 8-72 native elements; and a spine, where each
;; element in the inner-inner-spine tree is a node-of-nodes-of-nodes
;; containing 8-27 native elements.
;;
;; This pattern continues indefinitely until the innermost spine is
;; empty or single. Therefore, for sufficiently large n, the number of
;; levels of nesting is bounded between the base-2 and base-3
;; logarithm of n, so is O(log n).
;;
;; Since each shape of tree (empty, single, deep) contains O(1)
;; elements, and the depth of the tree is O(log n), any recursive
;; algorithm which spends O(1) time examining a tree and recurses at
;; most once into the spine takes O(log n) worst case time. This is
;; why the search tree-like operations take O(log n) time.
;;
;; Likewise, any recursive algorithm which spends O(1) time on a tree,
;; and recurses into the spine only every 1/k operations for k>1,
;; takes O(1) amortized time, and rarely at most O(log n) worst case
;; time. This is why the deque operations take O(1) amortized time.
;;
;; The cached measurements are essential to the search-tree
;; operations. No matter how deeply nested a node is, the sum of the
;; measurements of all its elements is stored in a record field with a
;; fast O(1)-time getter.

;; Syntax to match and unpack a tree.
(define-syntax match-tree
  (syntax-rules (empty single deep)
    ((match-tree TREE
       ((empty) EMPTY-BODY ...)
       ((single X) SINGLE-BODY ...)
       ((deep L SP R) DEEP-BODY ...))
     (if (empty? TREE)
	 (begin EMPTY-BODY ...)
	 (match-nonempty-tree TREE
	   ((single X) SINGLE-BODY ...)
	   ((deep L SP R) DEEP-BODY ...))))))

;; Syntax to match and unpack a non-empty tree.
(define-syntax match-nonempty-tree
  (syntax-rules (single deep)
    ((match-nonempty-tree TREE
       ((single X) SINGLE-BODY ...)
       ((deep L SP R) DEEP-BODY ...))
     (let ((t TREE))
       (if (single? t)
	   (let ((X (single-x t)))
	     SINGLE-BODY ...)
	   (let ((L (deep-l t))
		 (SP (deep-sp t))
		 (R (deep-r t)))
	     DEEP-BODY ...))))))

;; Syntax to match and unpack a node.
(define-syntax match-node
  (syntax-rules (binary trinary)
    ((match-node NODE
       ((binary M2 X2 Y2) NODE2-BODY ...)
       ((trinary M3 X3 Y3 Z3) NODE3-BODY ...))
     (let ((n NODE))
       (if (node2? n)
	   (let ((M2 (node2-m n))
		 (X2 (node2-x n))
		 (Y2 (node2-y n)))
	     NODE2-BODY ...)
	   (let ((M3 (node3-m n))
		 (X3 (node3-x n))
		 (Y3 (node3-y n))
		 (Z3 (node3-z n)))
	     NODE3-BODY ...))))))

;; Singleton empty tree, and promise for an empty tree. Due to purity,
;; these may be shared among all uses of empty trees.
(define *empty-promise* (delay (make-empty)))
(define *empty* (force *empty-promise*))

;; Annotate when a value is undefined or unused.
(define *undefined* #false)

;; Create and return a binary node.
(define (node2 add measure x y)
  (make-node2 (add (measure x) ; cached measurement
		   (measure y))
	      x
	      y))

;; Create and return a trinary node.
(define (node3 add measure x y z)
  (make-node3 (add (add (measure x) ; cached measurement
			(measure y))
		   (measure z))
	      x
	      y
	      z))

;; Return a node's cached measurement.
(define (node-measure node)
  (if (node2? node)
      (node2-m node)
      (node3-m node)))

;; Return a freshly-allocated list containing the elements of the
;; given node.
(define (node->list node)
  (match-node node
    ((binary  m x y)
     (list x y))	       
    ((trinary m x y z)
     (list x y z))))

;; Repair and return a deep node which, as a result of some other
;; operation, now has an empty left digit. s is the spine and r is the
;; right digit.
(define (drop-left-digit s r)
  ;; A proper deep node must have at least one element in its left
  ;; digit, so we have to either find some elements to move there, or
  ;; stop being a deep node.
  (cond
   ((not (empty? s))
    ;; The spine has at least one node, so we pull one out and break
    ;; it up into 2 or 3 elements.
    (make-deep (node->list (finger-tree-left s))
	       (delay (finger-tree-remove-left s))
	       r))
   ((pair? (cdr r))
    ;; The spine is empty, but the right digit has at least two
    ;; elements, so we can move one of them over to the left. Since
    ;; the right digit is in reverse order, we migrate the last
    ;; element, so we don't need to reverse a list.
    (let-values (((r l) (split-at r (- (length r) 1))))
      (make-deep l *empty-promise* r)))
   (else
    ;; If we get here, the spine is empty and the right digit has
    ;; exactly 1 element. We have no choice but to downgrade to a
    ;; single-shaped tree.
    (make-single (first r)))))

;; Symmetric to drop-left-digit.
(define (drop-right-digit l s)
  (cond
   ((not (empty? s))
    (make-deep l
	       (delay (finger-tree-remove-right s))
	       (reverse! (node->list (finger-tree-right s)))))
   ((pair? (cdr l))
    (let-values (((l r) (split-at l (- (length l) 1))))
      (make-deep l *empty-promise* r)))
   (else
    (make-single (car l)))))

;; Like for-each, but visits elements in reverse order.
(define (for-each-reverse proc lst)
  (let loop ((lst lst))
    (unless (null? lst)
      (loop (cdr lst))
      (proc (car lst)))))    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Whole-tree operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-finger-tree)
  *empty*)

(define (finger-tree add measure . elements)
  (list->finger-tree add measure elements))

(define (finger-tree? obj)
  (or (empty? obj)
      (single? obj)
      (deep? obj)))

(define finger-tree-empty? empty?)

(define (finger-tree-force tree)
  (match-tree tree
    ((empty)
     tree)
    ((single x)
     tree)
    ((deep l sp r)
     (finger-tree-force (force sp))
     tree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sequence operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define finger-tree-append
  (case-lambda
   ((add measure tree1 tree2)
    ;; Exactly two operands.
    (let recurse ((measure measure) (tree1 tree1) (tree2 tree2))
      (cond
       ;; Trivial cases when at least one tree is empty, so we simply
       ;; return the non-empty tree.
       ((empty? tree2)
	tree1)
       ((empty? tree1)
	tree2)
       ;; Trivial cases when one of the trees is single, so appending
       ;; reduces to adding one element to the left/right end.
       ((single? tree2)
	(finger-tree-add-right add measure tree1 (single-x tree2)))
       ((single? tree1)
	(finger-tree-add-left add measure tree2 (single-x tree1)))
       (else
	;; Non-trivial case when both trees are deep.
	;;
	;; Altogether we have two deep nodes:
	;;
	;;     Left deep node:           Right deep node:
	;;     left-l  left-s  left-r    right-l  right-s  right-r
	;;
	;; We will form one deep node, re-using the outer digits, and
	;; merging together the spines and inner digits to eventually
	;; form one new spine.
	;;
	;;               New spine:
	;;     left-l    (left-s left-r right-l right-s)    right-r
	;;
	;; To merge the stuff in the middle, we combine left-r (1-4
	;; elements) and right-l (1-4 elements) into one list of
	;; "loose" elements (2-8). Then we break up the loose elements
	;; into nodes and add them to the right of left-s.
	;;
	;; At that point the new spine is just (left-s right-s), i.e.
	;; two finger trees, which we append recursively.
	(make-deep (deep-l tree1) ; reuse outer left digit
		   (delay
		     ;; Combine the inner digits into one list of
		     ;; elements in left-to-right order.
		     (let ((loose (append! (reverse (deep-r tree1))
					   (deep-l tree2))))
		       ;; Greedy algorithm: while loose elements
		       ;; remain, if there are 2 or 4 loose elements
		       ;; left, move the first two into a 2-node;
		       ;; otherwise move the first 3 into a
		       ;; 3-node. Observe that, for any number of
		       ;; loose elements 2-8, this algorithm
		       ;; partitions the elements into the minimal
		       ;; number of nodes.
		       (let loop ((loose loose)
				  (k (length loose))
				  (s (force (deep-sp tree1))))
		       (case k
			 ((0)
			  ;; All loose nodes have been elminated;
			  ;; recursively append the two spines, and
			  ;; we're done.
			  (recurse node-measure s (force (deep-sp tree2))))
			 ((2 4)
			  (loop (cddr loose)
				(- k 2)
				(finger-tree-add-right add node-measure s
						       (node2 add measure
							      (first loose)
							      (second loose)))))
			 (else
			  (loop (cdddr loose)
				(- k 3)
				(finger-tree-add-right add node-measure s
						       (node3 add measure
							      (first loose)
							      (second loose)
							      (third loose)))))))))
		   (deep-r tree2)))))) ; reuse outer right digit
   ((add measure tree1 . rest)
    ;; More than two operands. Append each pair with the two-operand
    ;; case above.
    (fold (lambda (tree accum)
	    (finger-tree-append add measure accum tree))
	  tree1
	  rest))))

(define (finger-tree-filter add measure proc tree)
  (generator->finger-tree add
			  measure
			  (gfilter proc
				   (finger-tree->generator tree))))

(define (finger-tree-fold-left proc seed . trees)
  (apply generator-fold proc seed (map finger-tree->generator trees)))

(define (finger-tree-fold-right proc seed . trees)
  (apply generator-fold proc seed (map finger-tree->reverse-generator trees)))

(define (finger-tree-for-each proc . trees)
  (apply generator-for-each proc (map finger-tree->generator trees)))

(define (finger-tree-length tree)
  (finger-tree-fold-left (lambda (element accum)
			   (+ 1 accum))
			 0
			 tree))

(define (finger-tree-map add measure proc . trees)
  (apply finger-tree-fold-left
	 (lambda args
	   (let-values (((elts accums) (split-at! args (- (length args) 1))))
	     (finger-tree-add-right add
				    measure
				    (car accums)
				    (apply proc elts))))
	 *empty*
	 trees))

(define (finger-tree-reverse add measure tree)
  (let recurse ((measure measure)
		(rev (lambda (element)
		       element))
		(tree tree))
    (match-tree tree
      ((empty)
       *empty*)
      ((single x)
       (make-single (rev x)))
      ((deep l sp r)
       (make-deep (map rev r)
		  (delay
		    (recurse node-measure
			     (lambda (node)
			       (match-node node
			         ((binary m x y)
				  (node2 add measure (rev y) (rev x)))
				 ((trinary m x y z)
				  (node3 add measure (rev z) (rev y) (rev x)))))
			     (force sp)))
		  (map rev l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deque operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (finger-tree-left tree)
  (match-nonempty-tree tree
    ((single x)
     x)
    ((deep l sp r)
     (first l))))

(define (finger-tree-right tree)
  (match-nonempty-tree tree
    ((single x)
     x)
    ((deep l sp r)
     (first r))))

(define (finger-tree-add-left add measure tree element)
  (let recurse ((measure measure) (tree tree) (element element))
    (match-tree tree
     ((empty)
      ;; Empty becomes single.
      (make-single element))
     ((single x)
      ;; Single becomes a deep node with 1 element in each digit.
      (make-deep (list element) *empty-promise* (list x)))
     ((deep l sp r)
      (if (< (length l) 4)
	  ;; Left digit has room for one more element, done.
	  (make-deep (cons element l) sp r)

	  ;; Left digit is already full of exactly 4 elements. Turn
	  ;; the last three digit elements into a trinary node and
	  ;; lazily push it into the spine. The remaining 1 digit
	  ;; element, and the newly-added element, become a new left
	  ;; finger of 2 elements. Observe that, when adding elements
	  ;; this way, the recursive case only happens in 1/3 add
	  ;; operations, and that each nested level recurses less
	  ;; frequently than the last by a factor of 3, which works
	  ;; out to O(1) amortized time per add.
	  (make-deep (list element (first l))
		     (delay
		       (recurse node-measure
				(force sp)
				(node3 add
				       measure
				       (second l)
				       (third l)
				       (fourth l))))
		     r))))))

(define (finger-tree-add-right add measure tree element)
  ;; Symmetrical to finger-tree-add-left.
  (let recurse ((measure measure) (tree tree) (element element))
    (match-tree tree
     ((empty)
      (make-single element))
     ((single x)
      (make-deep (list x) *empty-promise* (list element)))
     ((deep l sp r)
      (if (< (length r) 4)
	  (make-deep l sp (cons element r))
	  (make-deep l
		     (delay
		       (recurse node-measure
				(force sp)
				(node3 add
				       measure
				       (fourth r) ; reverse order
				       (third r)
				       (second r))))
		     (list element (first r))))))))

(define (finger-tree-remove-left tree)
  (match-nonempty-tree tree
    ((single x)
     ;; Single becomes empty.
     *empty*)
    ((deep l sp r)
     (if (pair? (cdr l))
	 ;; Left finger has multiple elements, so we simply drop one
	 ;; and we're done.
	 (make-deep (cdr l) sp r)

	 ;; Otherwise the left digit is now effectively empty, which
	 ;; is an invariant violation that we need to repair. Observe
	 ;; that, as with the add operations, this case only happens
	 ;; 1/3 the time.
	 (drop-left-digit (force sp) r)))))

(define (finger-tree-remove-right tree)
  ;; Symmetrical to finger-tree-remove-left.
  (match-nonempty-tree tree
    ((single x)
     *empty*)
    ((deep l sp r)
     (if (pair? (cdr r))
	 (make-deep l sp (cdr r))
	 (drop-right-digit l (force sp))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scan and split
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (finger-tree-scan add measure pred zero tree match absent)
  ;; This is a long procedure, but not too complex if we take it one
  ;; step at a time. We approach this recursively. Within the
  ;; recursion, match and absent have an extra argument giving the
  ;; measurement that has been computed so far. So we call
  ;; (match accum-before element), where element is a matching
  ;; element, and accum-before is the accumulated measurement prior to
  ;; element. Or, we call (absent accum-after), where accum-after is
  ;; the accumulated measurement after all of tree. These extra
  ;; arguments are necessary because, if we recursively search in the
  ;; spine and find a match, the match is a *node* that contains some
  ;; matching element, and we still need to work out which of of the
  ;; node elements is the true match.
  (let recurse ((measure measure)
		(zero zero)
		(tree tree)

		;; The outermost match and absent procedures drop the
		;; extra measurement arguments before passing control
		;; back to the client's continuations.
		(match (lambda (accum-before element)
			 (match element)))
		(absent (lambda (accum-after)
			  (absent))))
    (match-tree tree
      ((empty)
       ;; An empty tree can't possibly have a match.
       (absent zero))
      ((single x)
       ;; Single tree; measure its only element and decide whether
       ;; it's a match or not.
       (let ((accum/x (add zero (measure x))))
	 (if (pred accum/x)
	     (match zero x)
	     (absent accum/x))))
      ((deep l sp r)
       ;; (scan-digit fold-proc zero lst match absent) is a helper
       ;; procedure for scanning a left or right digit. fold-proc is
       ;; expected to be fold or fold-right, indicating which order to
       ;; scan. lst is the digit to scan. match and absent are
       ;; continuations for match and absent conditions, as usual.
       (let* ((scan-digit (lambda (fold-proc zero lst match absent)
			    (call/cc
			     (lambda (return)
			       (absent
				(fold-proc (lambda (element accum)
					     (let ((next-accum (add accum (measure element))))
					       (if (pred next-accum)
						   (return (match accum element))
						   next-accum)))
					   zero
					   lst)))))))
	 ;; First, scan the left digit in left-to-right order.
	 (scan-digit fold zero l match
		     (lambda (accum/left)
		       ;; No match in the left digit, so try
		       ;; recursively scanning the spine.
		       (recurse node-measure
				accum/left
				(force sp)
				(lambda (accum-before matching-node)
				  ;; We found a matching node in the
				  ;; spine. But which of the node's 2
				  ;; or 3 elements do we pass back to
				  ;; our client?
				  (match-node matching-node
				    ((binary m x y)
				     (let ((accum/x (add accum-before (measure x))))
				       (if (pred accum/x)
					   (match accum-before x) ; x matches
					   (match accum/x y))))   ; else y must match
				    ((trinary m x y z)
				     (let ((accum/x (add accum-before (measure x))))
				       (if (pred accum/x)
					   (match accum-before x) ; x matches
					   (let ((accum/y (add accum/x (measure y))))
					     (if (pred accum/y)
						 (match accum/x y) ; y matches
						 (match accum/y z)))))))) ; else z must match
				(lambda (accum/left+spine)
				  ;; No match in the left digit nor
				  ;; the spine. Finally, scan the
				  ;; right digit in right-to-left
				  ;; order.
				  (scan-digit fold-right accum/left+spine r match
					      ;; No match anywhere in
					      ;; the tree.
					      absent))))))))))

(define (finger-tree-split add measure pred zero tree match absent)
  ;; This is essentially the same algorithm as finger-tree-scan, but
  ;; is a little more complicated since we also need to build the
  ;; prefix and suffix trees when we find a match. Just like the scan
  ;; procedure, we solve this recursively with continuation procedures
  ;; that take an extra argument for the accumulated measurement.
  (let recurse ((measure measure)
		(zero zero)
		(tree tree)
		(match (lambda (accum prefix element suffix)
			 (match prefix element suffix)))
		(absent (lambda (accum)
			  (absent))))
    (match-tree tree
      ((empty)
       (absent zero))
      ((single x)
       (let ((accum/x (add zero (measure x))))
	 (if (pred accum/x)
	     (match zero *empty* x *empty*) ; prefix and suffix both empty
	     (absent accum/x))))
      ((deep l sp r)
       ;; Helper procedure to check the left digit, or call absent if
       ;; no match can be found.
       (let ((split-left (lambda (absent)
			  (let loop ((accum zero)
				     (i 0)
				     (rest l))
			    (if (null? rest)
				(absent accum)
				(let* ((x (car rest))
				       (next-rest (cdr rest))
				       (accum/x (add accum (measure x))))
				  (if (pred accum/x)
				      (match accum
					     (list->finger-tree add measure (take l i))
					     x
					     (if (null? next-rest)
						 (drop-left-digit (force sp) r)
						 (make-deep next-rest sp r)))
				      (loop accum/x
					    (+ i 1)
					    next-rest))))))))
	 (split-left
	  (lambda (accum/left)
	    (recurse node-measure
		     accum/left
		     (force sp)
		     (lambda (accum-before spine-prefix node spine-suffix)
		       (match-node node
		         ((binary m x y)
			  (let ((accum/x (add accum-before (measure x))))
			    (if (pred accum/x)
				(match accum-before
				       (drop-right-digit l spine-prefix)
				       x
				       (make-deep (list y) (delay spine-suffix) r))
				(match accum/x
				       (make-deep l (delay spine-prefix) (list x))
				       y
				       (drop-left-digit spine-suffix r)))))
			 ((trinary m x y z)
			  (let ((accum/x (add accum-before (measure x))))
			    (if (pred accum/x)
				(match accum-before
				       (drop-right-digit l spine-prefix)
				       x
				       (make-deep (list y z) (delay spine-suffix) r))
				(let ((accum/y (add accum/x (measure y))))
				  (if (pred accum/y)
				      (match accum/x
					     (make-deep l (delay spine-prefix) (list x))
					     y
					     (make-deep (list z) (delay spine-suffix) r))
				      (match accum/y
					     (make-deep l (delay spine-prefix) (list y x))
					     z
					     (drop-left-digit spine-suffix r)))))))))
		     (lambda (accum/left+spine)
		       (let loop ((accum accum/left+spine)
				  (i 0)
				  (rest (reverse r)))
			 (if (null? rest)
			     (absent accum)
			     (let* ((x (car rest))
				    (next-rest (cdr rest))
				    (accum/x (add accum (measure x))))
			       (if (pred accum/x)
				   (match accum
					  (if (zero? i)
					      (drop-right-digit l (force sp))
					      (make-deep l sp (take-right r i)))
					  x
					  (list->finger-tree add measure next-rest))
				   (loop accum/x
					 (+ i 1)
					 next-rest))))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;> Conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define generator->finger-tree
  ;; Several other conversion procedures reduce to this one, so we're
  ;; careful to keep this code tight.
  (case-lambda
   ;; Unknown length; simply add to the right until done.
   ((add measure gen)
    (generator-fold (lambda (element accum)
		      (finger-tree-add-right add measure accum element))
		    *empty*
		    gen))

   ;; Fast path for when the length is known a priori.
   ((add measure gen k)
    (let recurse ((measure measure) (gen gen) (k k))
      (cond
       ((zero? k)
	;; Empty.
	*empty*)
       ((= k 1)
	;; Single.
	(make-single (gen)))
       ((<= k 5)
	;; Deep node with an empty spine and only one element in the
	;; right digit. We use a let* form to ensure that the left digit
	;; is generated before the right digit, since order of
	;; evaluation matters when dealing with generators.
	(let* ((l (generator->list gen (- k 1)))
	       (r (generator->reverse-list gen 1)))
	  (make-deep l *empty-promise* r)))
       ((<= k 8)
	;; Deep node with 4 elements in the left digit, remaining
	;; elements in the right digit, and an empty spine. Remember to
	;; reverse the right digit.
	(let* ((l (generator->list gen 4))
	       (r (generator->reverse-list gen (- k 4))))
	  (make-deep l *empty-promise* r)))
       (else
	;; Deep node with a non-empty spine. We set aside 4 elements for
	;; the left digit, at least one for the right digit, and build a
	;; spine from as many full 3-nodes as possible from the
	;; remaining elements. Mathematically, the number of 3-nodes is
	;; floor((k-5)/3). Observe that, since k >= 9, there is at least
	;; one node. Further observe that, for any k >= 9, this leaves
	;; 1-3 leftover elements for the right digit.
	(let* ((node-count (floor-quotient (- k 5) 3))
	       (right-k (- k 4 (* 3 node-count)))
	       (l (generator->list gen 4))
	       (s (recurse node-measure
			   (lambda ()
			     ;; Generator that yields 3-nodes in proper
			     ;; order. If our calculations are correct,
			     ;; there is no need to worry about gen
			     ;; going empty.
			     (let* ((x (gen))
				    (y (gen))
				    (z (gen)))
			       (node3 add measure x y z)))
			   node-count))
	       (r (generator->reverse-list gen right-k)))
	  (make-deep l (delay s) r))))))))

(define (list->finger-tree add measure lst)
  (generator->finger-tree add
			  measure
			  (list->generator lst)
			  (length lst)))

(define (finger-tree->generator tree)
  (make-coroutine-generator
   (lambda (yield)
     (let recurse ((yield yield) (tree tree))
       (match-tree tree
         ((empty)
	  *undefined*)
	 ((single x)
	  (yield x))
	 ((deep l sp r)
	  (for-each yield l)
	  (recurse (lambda (node)
		    (match-node node
		      ((binary m x y)
		       (yield x)
		       (yield y))
		      ((trinary m x y z)
		       (yield x)
		       (yield y)
		       (yield z))))
		   (force sp))
	  (for-each-reverse yield r)))))))		     
	  
(define (finger-tree->reverse-generator tree)
  (make-coroutine-generator
   (lambda (yield)
     (let recurse ((yield yield) (tree tree))
       (match-tree tree
         ((empty)
	  *undefined*)
	 ((single x)
	  (yield x))
	 ((deep l sp r)
	  (for-each yield r)
	  (recurse (lambda (node)
		    (match-node node
		      ((binary m x y)
		       (yield y)
		       (yield x))
		      ((trinary m x y z)
		       (yield z)
		       (yield y)
		       (yield x))))
		   (force sp))
	  (for-each-reverse yield l)))))))		     

(define (finger-tree->list tree)
  (generator->list (finger-tree->generator tree)))
