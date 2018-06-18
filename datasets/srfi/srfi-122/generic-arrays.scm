;;; declarations to reduce the size of the .o file

(declare (standard-bindings)(extended-bindings)(block)(fixnum)(not safe))

;;; The following macro is used to determine whether certain keyword arguments
;;; were omitted.  It is specific to Gambit-C's compiler.
;;; Redefine it for other schemes.


(##define-macro (macro-absent-obj)  `',(##type-cast -6 2))

;;; This is Gambit-specific to make it a bit faster

(declare (inline))

(define (##exact-integer? x)
  (or (##fixnum? x)
      (##bignum? x)))

(declare (not inline))

;;; We need a multi-argument every, but not something as fancy as in Olin Shiver's
;;; list library.  (Shiver's version works fine, though, for our purposes.)

(define (##every pred list . lists)
  (if (pair? lists)
      (let loop ((lists (cons list lists)))
	(or (null? (car lists))
	    (and (apply pred (map car lists))
		 (loop (map cdr lists)))))
      (let loop ((list list))
	(or (null? list)
	    (and (pred (car list))
		 (loop (cdr list)))))))

(define (##vector-every pred vec #!optional (vec2 (macro-absent-obj)) #!rest vecs)
  
  (define (every1 vec i)
    (or (< i 0)
	(and (pred (vector-ref vec i))
	     (every1 vec (- i 1)))))
  
  (define (every2 vec1 vec2 i)
    (or (< i 0)
	(and (pred (vector-ref vec1 i) (vector-ref vec2 i))
	     (every2 vec1 vec2 (- i 1)))))

  (define (every-general vecs i)
    (or (< i 0)
	(and (apply pred (map (lambda (vec) (vector-ref vec i)) vecs))
	     (every-general vecs (- i 1)))))

  (cond ((eq? vec2 (macro-absent-obj))
	 (every1 vec (- (vector-length vec) 1)))
	((null? vecs)
	 (every2 vec vec2 (- (vector-length vec) 1)))
	(else
	 (every-general (cons vec (cons vec2 vecs)) (- (vector-length vec) 1)))))

(define (##vector-map f vec #!optional (vec2 (macro-absent-obj)) #!rest vecs)

  (define (map1 f vec)
    (let* ((n (vector-length vec))
	   (result (make-vector n)))
      (do ((i 0 (fx+ i 1)))
	  ((fx= i n) result)
	(vector-set! result i (f (vector-ref vec i))))))

  (define (map2 f vec1 vec2)
    (let* ((n (vector-length vec1))
	   (result (make-vector n)))
      (do ((i 0 (fx+ i 1)))
	  ((fx= i n) result)
	(vector-set! result i (f (vector-ref vec1 i)
				 (vector-ref vec2 i))))))

  (cond ((eq? vec2 (macro-absent-obj))
	 (map1 f vec))
	((null? vecs)
	 (map2 f vec vec2))
	(else ; punt
	 (list->vector (apply map f (map vector->list (cons vec (cons vec2 vecs))))))))

(declare (inline))

;;; An interval is a cross product of multi-indices

;;; [l_0,u_0) x [l_1,u_1) x ... x [l_n-1,u_n-1)

;;; where l_i < u_i for 0 <= i < n, and n > 0 is the dimension of the interval

(define-structure ##interval
  lower-bounds            ;; a vector of exact integers l_0,...,l_n-1
  upper-bounds)           ;; a vector of exact integers u_0,...,u_n-1

(define (interval? x)
  (##interval? x))

(declare (not inline))

(define (make-interval lower-bounds upper-bounds)
  (cond ((not (and (vector? lower-bounds)
                   (< 0 (vector-length lower-bounds))
                   (##vector-every ##exact-integer? lower-bounds)))
	 (error "make-interval: The first argument is not a nonempty vector of exact integers: " lower-bounds upper-bounds))
        ((not (and (vector? upper-bounds)
                   (< 0 (vector-length upper-bounds))
                   (##vector-every ##exact-integer? upper-bounds)))
	 (error "make-interval: The second argument is not a nonempty vector of exact integers: " lower-bounds upper-bounds))
	((not (= (vector-length lower-bounds) (vector-length upper-bounds)))
	 (error "make-interval: The first and second arguments are not the same length: " lower-bounds upper-bounds))
	((not (##vector-every (lambda (x y) (< x y)) lower-bounds upper-bounds))
	 (error "make-interval: Each lower-bound must be less than the associated upper-bound: " lower-bounds upper-bounds))
	(else
	 (make-##interval (vector-copy lower-bounds) (vector-copy upper-bounds)))))


(declare (inline))

(define (##interval-dimension interval)
  (vector-length (##interval-lower-bounds interval)))

(define (##interval-lower-bound interval i)
  (vector-ref (##interval-lower-bounds interval) i))

(define (##interval-upper-bound interval i)
  (vector-ref (##interval-upper-bounds interval) i))

(define (##interval-lower-bounds->vector interval)
  (##vector-copy (##interval-lower-bounds interval)))

(define (##interval-upper-bounds->vector interval)
  (##vector-copy (##interval-upper-bounds interval)))

(define (##interval-lower-bounds->list interval)
  (vector->list (##interval-lower-bounds interval)))

(define (##interval-upper-bounds->list interval)
  (vector->list (##interval-upper-bounds interval)))

(declare (not inline))

(define (interval-dimension interval)
  (cond ((not (interval? interval))
	 (error "interval-dimension: argument is not an interval: " interval))
	(else
	 (##interval-dimension interval))))

(define (interval-lower-bound interval i)
  (cond ((not (interval? interval))
	 (error "interval-lower-bound: argument is not an interval: " interval i))
	((not (##exact-integer? i))
	 (error "interval-lower-bound: argument is not an exact integer: " interval i))
	((not (< -1 i (##interval-dimension interval)))
	 (error "interval-lower-bound: index is not between 0 (inclusive) and (interval-dimension interval) (exclusive): " interval i))
	(else
	 (##interval-lower-bound interval i))))

(define (interval-upper-bound interval i)
  (cond ((not (interval? interval))
	 (error "interval-upper-bound: argument is not an interval: " interval i))
	((not (##exact-integer? i))
	 (error "interval-upper-bound: argument is not an exact integer: " interval i))
	((not (< -1 i (##interval-dimension interval)))
	 (error "interval-upper-bound: index is not between 0 (inclusive) and (interval-dimension interval) (exclusive): " interval i))
	(else
	 (##interval-upper-bound interval i))))

(define (interval-lower-bounds->vector interval)
  (cond ((not (interval? interval))
	 (error "interval-lower-bounds->vector: argument is not an interval: " interval))
	(else
	 (##interval-lower-bounds->vector interval))))

(define (interval-upper-bounds->vector interval)
  (cond ((not (interval? interval))
	 (error "interval-upper-bounds->vector: argument is not an interval: " interval))
	(else
	 (##interval-upper-bounds->vector interval))))

(define (interval-lower-bounds->list interval)
  (cond ((not (interval? interval))
	 (error "interval-lower-bounds->list: argument is not an interval: " interval))
	(else
	 (##interval-lower-bounds->list interval))))

(define (interval-upper-bounds->list interval)
  (cond ((not (interval? interval))
	 (error "interval-upper-bounds->list: argument is not an interval: " interval))
	(else
	 (##interval-upper-bounds->list interval))))

(define (interval-projections interval right-dimension)
  (cond ((not (interval? interval))
	 (error "interval-projections: The first argument is not an interval: " interval right-dimension))
	((not (< 1 (##interval-dimension interval)))  ;; redundant check, but useful error message
	 (error "interval-projections: The dimension of the first argument is not greater than 1: " interval right-dimension))
	((not (##exact-integer? right-dimension))
	 (error "interval-projections: The second argument is not an exact integer: " interval right-dimension))
	((not (< 0 right-dimension (##interval-dimension interval)))
	 (error "interval-projections: The second argument is not between 0 and the dimension of the first argument (exclusive): " interval right-dimension))
	(else
	 (##interval-projections interval right-dimension))))

(define (##interval-projections interval right-dimension)
  (let* ((n (##interval-dimension interval))
	 (left-dimension (fx- n right-dimension))
	 (lower-bounds (##interval-lower-bounds interval))
	 (upper-bounds (##interval-upper-bounds interval))
	 (left-lower-bounds (make-vector left-dimension))
	 (left-upper-bounds (make-vector left-dimension))
	 (right-lower-bounds (make-vector (- n left-dimension)))
	 (right-upper-bounds (make-vector (- n left-dimension))))
    (do ((i 0 (+ i 1)))
	((= i left-dimension)
	 (do ((i i (+ i 1)))
	     ((= i n)
	      (values (make-##interval left-lower-bounds
				       left-upper-bounds)
		      (make-##interval right-lower-bounds
				       right-upper-bounds)))
	   (vector-set! right-lower-bounds (- i left-dimension) (vector-ref lower-bounds i))
	   (vector-set! right-upper-bounds (- i left-dimension) (vector-ref upper-bounds i))))
      (vector-set! left-lower-bounds i (vector-ref lower-bounds i))
      (vector-set! left-upper-bounds i (vector-ref upper-bounds i)))))

(define (permutation? permutation)
  (and (vector? permutation)
       (let* ((n (vector-length permutation))
	      (permutation-range (make-vector n #f)))
	 ;; we'll write things into permutation-range
	 ;; each box should be written only once
	 (let loop ((i 0))
	   (or (= i n)
	       (let ((p_i (vector-ref permutation i)))
		 (and (fixnum? p_i) ;; a permutation index can't be a bignum
		      (< -1 p_i n)
		      (not (vector-ref permutation-range p_i))
		      (let ()
			(vector-set! permutation-range p_i #t)
			(loop (+ i 1))))))))))



(define (##vector-permute vector permutation)
  (let* ((n (vector-length vector))
	 (result (make-vector n)))
    (do ((i 0 (+ i 1)))
	((= i n) result)
      (vector-set! result i (vector-ref vector (vector-ref permutation i))))))

(define (##vector-permute->list vector permutation)
  (do ((i (- (vector-length vector) 1) (- i 1))
       (result '() (cons (vector-ref vector (vector-ref permutation i))
			 result)))
      ((< i 0) result)))

(define (##permutation-invert permutation)
  (let* ((n (vector-length permutation))
	 (result (make-vector n)))
    (do ((i 0 (+ i 1)))
	((= i n) result)
      (vector-set! result (vector-ref permutation i) i))))



(define (##interval-permute interval permutation)
  (make-##interval (##vector-permute (##interval-lower-bounds interval) permutation)
		   (##vector-permute (##interval-upper-bounds interval) permutation)))

(define (interval-permute interval permutation)
  (cond ((not (interval? interval))
	 (error "interval-permute: The first argument is not an interval: " interval permutation))
	((not (permutation? permutation))
	 (error "interval-permute: The second argument is not a permutation: " interval permutation))
	((not (= (interval-dimension interval) (vector-length permutation)))
	 (error "interval-permute: The dimension of the first argument (an interval) does not equal the length of the second (a permutation): " interval permutation))
	(else
	 (##interval-permute interval permutation))))

(define (translation? translation)
  (and (vector? translation)
       (##vector-every ##exact-integer? translation)))

(define (interval-translate interval translation)
  (cond ((not (interval? interval))
	 (error "interval-translate: The first argument is not an interval: " interval translation))
	((not (translation? translation))
	 (error "interval-translate: The second argument is not a vector of exact integers: " interval translation))
	((not (= (##interval-dimension interval)
		 (vector-length translation)))
	 (error "interval-translate: The dimension of the first argument (an interval) does not equal the length of the second (a vector): " interval translation))
	(else
	 (##interval-translate interval translation))))

(define (##interval-translate Interval translation)
  (make-##interval (##vector-map + (interval-lower-bounds->vector Interval) translation)
		   (##vector-map + (interval-upper-bounds->vector Interval) translation)))

(define (##interval-scale interval scales)
  (let* ((uppers (##interval-upper-bounds->vector interval))
         (lowers (##interval-lower-bounds->vector interval))
         (new-uppers (##vector-map (lambda (u s)
                                    (quotient (+ u s -1) s))
                                   uppers scales)))
    (make-##interval lowers new-uppers)))

(define (interval-scale interval scales)
  (cond ((not (and (interval? interval)
                   (##vector-every zero? (interval-lower-bounds->vector interval))))
         (error "interval-scale: The first argument is not an interval with all lower bounds zero: " interval scales))
        ((not (and (vector? scales)
                   (##vector-every ##exact-integer? scales)
                   (##vector-every positive? scales)))
         (error "interval-scale: The second argument is not a vector of positive, exact, integers: " interval scales))
        ((not (= (vector-length scales) (interval-dimension interval)))
         (error "interval-scale: The dimension of the first argument (an interval) is not equal to the length of the second (a vector): "
                interval scales))
        (else
         (##interval-scale interval scales))))

(define (interval-dilate interval lower-diffs upper-diffs)
  (cond ((not (interval? interval))
	 (error "interval-dilate: first argument is not an interval: " interval lower-diffs upper-diffs))
	((not (vector? lower-diffs))
	 (error "interval-dilate: second argument must be a vector: " interval lower-diffs upper-diffs))
	((not (vector? upper-diffs))
	 (error "interval-dilate: third argument must be a vector: " interval lower-diffs upper-diffs))
	((not (= (vector-length lower-diffs)
		 (vector-length upper-diffs)
		 (##interval-dimension interval)))
	 (error "interval-dilate: The second and third arguments must have the same length as the dimension of the first argument: " interval lower-diffs upper-diffs))
	((not (and (##vector-every ##exact-integer? lower-diffs)
		   (##vector-every ##exact-integer? upper-diffs)))
	 (error "interval-dilate: The second and third arguments must contain only exact integers: " interval lower-diffs upper-diffs))
	(else
	 (let ((new-lower-bounds (##vector-map + (##interval-lower-bounds interval) lower-diffs))
	       (new-upper-bounds (##vector-map + (##interval-upper-bounds interval) upper-diffs)))
	   (if (##vector-every < new-lower-bounds new-upper-bounds)
	       (make-##interval new-lower-bounds new-upper-bounds)
	       (error "interval-dilate: the resulting interval is empty: " interval lower-diffs upper-diffs))))))

(define (##interval-volume interval)
  (do ((i (- (##interval-dimension interval) 1) (- i 1))
       (result 1 (let ()
		   (* result (- (##interval-upper-bound interval i)
				(##interval-lower-bound interval i))))))
      ((< i 0) result)))

(define (interval-volume interval)
  (cond ((not (interval? interval))
	 (error "interval-volume: argument is not an interval: " interval))
	(else
	 (##interval-volume interval))))

(define (##interval= interval1 interval2)
  (and (equal? (##interval-upper-bounds interval1)
	       (##interval-upper-bounds interval2))
       (equal? (##interval-lower-bounds interval1)
	       (##interval-lower-bounds interval2))))

(define (interval= interval1 interval2)
  (cond ((not (and (interval? interval1)
		   (interval? interval2)))
	 (error "interval=: Not all arguments are intervals: " interval1 interval2))
	(else
	 (##interval= interval1 interval2))))

(define (##interval-subset? interval1 interval2)
  (and (= (##interval-dimension interval1) (##interval-dimension interval2))
       (##vector-every >= (##interval-lower-bounds interval1) (##interval-lower-bounds interval2))
       (##vector-every <= (##interval-upper-bounds interval1) (##interval-upper-bounds interval2))))

(define (interval-subset? interval1 interval2)
  (cond ((not (and (interval? interval1)
		   (interval? interval2)))
	 (error "interval-subset?: Not all arguments are intervals: " interval1 interval2))
	(else
	 (##interval-subset? interval1 interval2))))

(define (##interval-intersect intervals)
  (let ((lower-bounds (apply ##vector-map max (map ##interval-lower-bounds intervals)))
	(upper-bounds (apply ##vector-map min (map ##interval-upper-bounds intervals))))
    (and (##vector-every < lower-bounds upper-bounds)
	 (make-##interval lower-bounds upper-bounds))))

(define (interval-intersect interval1 #!optional (interval2 (macro-absent-obj)) #!rest intervals)
  (cond ((eq? interval2 (macro-absent-obj))
	 (cond ((not (interval? interval1))
		(error "interval-intersect: The argument is not an interval: " interval1))
	       (else
		interval1)))
	(else
	 (let ((intervals (cons interval1 (cons interval2 intervals))))
	   (cond ((not (##every interval? intervals))
		  (apply error "interval-intersect: Not all arguments are intervals: " intervals))
		 ((not (apply = (map ##interval-dimension intervals)))
		  (apply error "interval-intersect: Not all arguments have the same dimension: " intervals))
		 (else
		  (##interval-intersect intervals)))))))

(declare (inline))

(define (##interval-contains-multi-index?-1 interval i)
  (and (<= (##interval-lower-bound interval 0) i) (< i (##interval-upper-bound interval 0))))

(define (##interval-contains-multi-index?-2 interval i j)
  (and (<= (##interval-lower-bound interval 0) i) (< i (##interval-upper-bound interval 0))
       (<= (##interval-lower-bound interval 1) j) (< j (##interval-upper-bound interval 1))))

(define (##interval-contains-multi-index?-3 interval i j k)
  (and (<= (##interval-lower-bound interval 0) i) (< i (##interval-upper-bound interval 0))
       (<= (##interval-lower-bound interval 1) j) (< j (##interval-upper-bound interval 1))
       (<= (##interval-lower-bound interval 2) k) (< k (##interval-upper-bound interval 2))))

(define (##interval-contains-multi-index?-4 interval i j k l)
  (and (<= (##interval-lower-bound interval 0) i) (< i (##interval-upper-bound interval 0))
       (<= (##interval-lower-bound interval 1) j) (< j (##interval-upper-bound interval 1))
       (<= (##interval-lower-bound interval 2) k) (< k (##interval-upper-bound interval 2))
       (<= (##interval-lower-bound interval 3) l) (< l (##interval-upper-bound interval 3))))

(declare (not inline))

(define (##interval-contains-multi-index?-general interval multi-index)
  (let loop ((i 0)
	     (multi-index multi-index))
    (or (null? multi-index)
	(let ((component (car multi-index)))
	  (and (<= (##interval-lower-bound interval i) component)
	       (< component (##interval-upper-bound interval i))
	       (loop (+ i 1)
		     (cdr multi-index)))))))

(define (interval-contains-multi-index? interval i #!rest multi-index-tail)

  ;; this is relatively slow, but (a) I haven't seen a need to use it yet, and (b) this formulation
  ;; significantly simplifies testing the error checking

  (cond ((not (interval? interval))
	 (error "interval-contains-multi-index?: argument is not an interval: " interval))
	(else
	 (let ((multi-index (cons i multi-index-tail)))
	   (cond ((not (= (##interval-dimension interval)
			  (length multi-index)))
		  (apply error "interval-contains-multi-index?: dimension of interval does not match number of arguments: " interval multi-index))
		 ((not (##every ##exact-integer? multi-index))
		  (apply error "interval-contains-multi-index?: at least one multi-index component is not an exact integer: " interval multi-index))
		 (else
		  (##interval-contains-multi-index?-general interval multi-index)))))))

;;; Applies f to every element of the domain; assumes that f is thread-safe,
;;; the order of application is not specified

(define (interval-for-each f interval)
  (cond ((not (interval? interval))
	 (error "interval-for-each: Argument is not a interval: " interval))
	((not (procedure? f))
	 (error "interval-for-each: Argument is not a procedure: " f))
	(else
	 (##interval-for-each f interval))))

(define (##interval-for-each f interval)
  (case (##interval-dimension interval)
    ((1) (let ((lower-i (##interval-lower-bound interval 0))
	       (upper-i (##interval-upper-bound interval 0)))
	   (let i-loop ((i lower-i))
	     (if (< i upper-i)
		 (begin
		   (f i)
		   (i-loop (+ i 1)))))))
    ((2) (let ((lower-i (##interval-lower-bound interval 0))
	       (lower-j (##interval-lower-bound interval 1))
	       (upper-i (##interval-upper-bound interval 0))
	       (upper-j (##interval-upper-bound interval 1)))
	   (let i-loop ((i lower-i))
	     (if (< i upper-i)
		 (let j-loop ((j lower-j))
		   (if (< j upper-j)
		       (begin
			 (f i j)
			 (j-loop (+ j 1)))
		       (i-loop (+ i 1))))))))
    ((3) (let ((lower-i (##interval-lower-bound interval 0))
	       (lower-j (##interval-lower-bound interval 1))
	       (lower-k (##interval-lower-bound interval 2))
	       (upper-i (##interval-upper-bound interval 0))
	       (upper-j (##interval-upper-bound interval 1))
	       (upper-k (##interval-upper-bound interval 2)))
	   (let i-loop ((i lower-i))
	     (if (< i upper-i)
		 (let j-loop ((j lower-j))
		   (if (< j upper-j)
		       (let k-loop ((k lower-k))
			 (if (< k upper-k)
			     (begin
			       (f i j k)
			       (k-loop (+ k 1)))
			     (j-loop (+ j 1))))
		       (i-loop (+ i 1))))))))
    ((4) (let ((lower-i (##interval-lower-bound interval 0))
	       (lower-j (##interval-lower-bound interval 1))
	       (lower-k (##interval-lower-bound interval 2))
	       (lower-l (##interval-lower-bound interval 3))
	       (upper-i (##interval-upper-bound interval 0))
	       (upper-j (##interval-upper-bound interval 1))
	       (upper-k (##interval-upper-bound interval 2))
	       (upper-l (##interval-upper-bound interval 3)))
	   (let i-loop ((i lower-i))
	     (if (< i upper-i)
		 (let j-loop ((j lower-j))
		   (if (< j upper-j)
		       (let k-loop ((k lower-k))
			 (if (< k upper-k)
			     (let l-loop ((l lower-l))
			       (if (< l upper-l)
				   (begin
				     (f i j k l)
				     (l-loop (+ l 1)))
				   (k-loop (+ k 1))))
			     (j-loop (+ j 1))))
		       (i-loop (+ i 1))))))))
    (else

     (let* ((lower-bounds (##interval-lower-bounds->list interval))
	    (upper-bounds (##interval-upper-bounds->list interval))
	    (arg          (map values lower-bounds)))                ; copy lower-bounds
       
       ;; I'm not particularly happy with set! here because f might capture the continuation
       ;; and then funny things might pursue ...
       ;; But it seems that the only way to have this work efficiently without the set
       ;; is to have arrays with fortran-style numbering.
       ;; blah

       (define (iterate lower-bounds-tail
			upper-bounds-tail
			arg-tail)
	 (let ((lower-bound (car lower-bounds-tail))
	       (upper-bound (car upper-bounds-tail)))
	   (if (null? (cdr arg-tail))
	       (let loop ((i lower-bound))
		 (if (< i upper-bound)
		     (begin
		       (set-car! arg-tail i)
		       (apply f arg)
		       (loop (+ i 1)))))
	       (let loop ((i lower-bound))
		 (if (< i upper-bound)
		     (begin
		       (set-car! arg-tail i)
		       (iterate (cdr lower-bounds-tail)
				(cdr upper-bounds-tail)
				(cdr arg-tail))
		       (loop (+ i 1))))))))

       (iterate lower-bounds
		upper-bounds
		arg)))))

;;; Calculates
;;;
;;; (...(operator (operator (operator identity (f multi-index_1)) (f multi-index_2)) (f multi-index_3)) ...)
;;;
;;; where multi-index_1, multi-index_2, ... are the elements of interval in lexicographical order
;;; This version assumes, and may use, that f is thread-safe and that operator is associative.
;;; The order of application of f and operator is not specified.


(define (##interval-fold f operator identity interval)
  (case (##interval-dimension interval)
    ((1) (let ((lower-i (##interval-lower-bound interval 0))
	       (upper-i (##interval-upper-bound interval 0)))
	   (let i-loop ((i lower-i) (result identity))
	     (if (= i upper-i)
		 result
		 (i-loop (+ i 1) (operator (f i) result))))))
    ((2) (let ((lower-i (##interval-lower-bound interval 0))
	       (lower-j (##interval-lower-bound interval 1))
	       (upper-i (##interval-upper-bound interval 0))
	       (upper-j (##interval-upper-bound interval 1)))
	   (let i-loop ((i lower-i) (result identity))
	     (if (= i upper-i)
		 result
		 (let j-loop ((j lower-j) (result result))
		   (if (= j upper-j)
		       (i-loop (+ i 1) result)
		       (j-loop (+ j 1) (operator (f i j) result))))))))
    ((3) (let ((lower-i (##interval-lower-bound interval 0))
	       (lower-j (##interval-lower-bound interval 1))
	       (lower-k (##interval-lower-bound interval 2))
	       (upper-i (##interval-upper-bound interval 0))
	       (upper-j (##interval-upper-bound interval 1))
	       (upper-k (##interval-upper-bound interval 2)))
	   (let i-loop ((i lower-i) (result identity))
	     (if (= i upper-i)
		 result
		 (let j-loop ((j lower-j) (result result))
		   (if (= j upper-j)
		       (i-loop (+ i 1) result)
		       (let k-loop ((k lower-k) (result result))
			 (if (= k upper-k)
			     (j-loop (+ j 1) result)
			     (k-loop (+ k 1) (operator (f i j k) result))))))))))
    ((4) (let ((lower-i (##interval-lower-bound interval 0))
	       (lower-j (##interval-lower-bound interval 1))
	       (lower-k (##interval-lower-bound interval 2))
	       (lower-l (##interval-lower-bound interval 3))
	       (upper-i (##interval-upper-bound interval 0))
	       (upper-j (##interval-upper-bound interval 1))
	       (upper-k (##interval-upper-bound interval 2))
	       (upper-l (##interval-upper-bound interval 3)))
	   (let i-loop ((i lower-i) (result identity))
	     (if (= i upper-i)
		 result
		 (let j-loop ((j lower-j) (result result))
		   (if (= j upper-j)
		       (i-loop (+ i 1) result)
		       (let k-loop ((k lower-k) (result result))
			 (if (= k upper-k)
			     (j-loop (+ j 1) result)
			     (let l-loop ((l lower-l) (result result))
			       (if (= l upper-l)
				   (k-loop (+ k 1) result)
				   (l-loop (+ l 1) (operator (f i j k l) result))))))))))))
    (else
     (let* ((lower-bounds (##interval-lower-bounds->list interval))
	    (upper-bounds (##interval-upper-bounds->list interval))
	    (arg          (map values lower-bounds)))                ; copy lower-bounds
       
       ;; I'm not particularly happy with set! here because f or operator might capture
       ;; the continuation and then funny things might pursue ...
       ;; But it seems that the only way to have this work efficiently without the set~
       ;; is to have arrays with fortran-style numbering.
       ;; blah
       
       (define (iterate lower-bounds-tail
			upper-bounds-tail
			arg-tail
			result)
	 (let ((lower-bound (car lower-bounds-tail))
	       (upper-bound (car upper-bounds-tail)))
	   (if (null? (cdr arg-tail))
	       (let loop ((i lower-bound)
			  (result result))
		 (if (= i upper-bound)
		     result
		     (begin
		       (set-car! arg-tail i)
		       (loop (+ i 1)
			     (operator (apply f arg) result)))))
	       (let loop ((i lower-bound)
			  (result result))
		 (if (= i upper-bound)
		     result
		     (begin
		       (set-car! arg-tail i)
		       (loop (+ i 1)
			     (iterate (cdr lower-bounds-tail)
				      (cdr upper-bounds-tail)
				      (cdr arg-tail)
				      result))))))))

       (iterate lower-bounds
		upper-bounds
		arg
		identity)))))

;; We'll use the same basic container for all types of arrays.

(declare (inline))

(define-structure ##array-base
  ;; Part of all arrays
  domain                  ;; an interval
  getter                  ;; (lambda (i_0 ... i_n-1) ...) returns a value for (i_0,...,i_n-1) in (array-domain a)
  ;; Part of mutable arrays
  setter                  ;; (lambda (v i_0 ... i_n-1) ...) sets a value for (i_0,...,i_n-1) in (array-domain a)
  ;; Part of specialized arrays
  storage-class           ;; a storage-class
  body                    ;; the backing store for this array
  indexer                 ;; see below
  safe?                   ;; do we check whether bounds (in getters and setters) and values (in setters) are valid
  )

(define specialized-array-default-safe?
  (let ((##specialized-array-default-safe? #f))
    (lambda (#!optional (bool (macro-absent-obj)))
      (cond ((eq? bool (macro-absent-obj))
	     ##specialized-array-default-safe?)
	    ((not (boolean? bool))
	     (error "specialized-array-default-safe?: The argument is not a boolean: " bool))
	    (else
	     (set! ##specialized-array-default-safe? bool))))))


(declare (not inline))

;; An array has a domain (which is an interval) and an getter that maps that domain into some type of
;; Scheme objects

(define (make-array domain getter #!optional (setter (macro-absent-obj)))
  (let ((setter (cond ((eq? setter (macro-absent-obj))
		       #f)
		      ((procedure? setter)
		       setter)
		      (else
		       (error "make-array: The third argument is not a procedure: " domain getter setter)))))
    (cond ((not (interval? domain))
	   (error "make-array: The first argument is not an interval: " domain getter setter))
	  ((not (procedure? getter))
	   (error "make-array: The second argument is not a procedure: " domain getter setter))
	  (else
	   (make-##array-base domain
			      getter
			      setter
			      #f        ; storage-class
			      #f        ; body
			      #f        ; indexer
			      #f        ; safe?
			      )))))

(define (array? x)
  (##array-base? x))

(define (array-domain obj)
  (cond ((not (array? obj))
	 (error "array-domain: object is not an array: " obj))
	(else
	 (##array-base-domain obj))))

(define (array-getter obj)
  (cond ((not (array? obj))
	 (error "array-getter: object is not an array: " obj))
	(else
	 (##array-base-getter obj))))

(define (array-dimension array)
  (cond ((not (array? array))
	 (error "array-dimension: argument is not an array: " array))
	(else
	 (##interval-dimension (array-domain array)))))


;;;
;;; A mutable array has, in addition a setter, that satisfies, roughly
;;;
;;; If (i_1, ..., i_n)\neq (j_1, ..., j_n) \in (array-domain a)
;;;
;;; and
;;;
;;; ((array-getter a) j_1 ... j_n) => x
;;;
;;; then "after"
;;;
;;; ((array-setter a) v i_1 ... i_n)
;;;
;;; we have
;;;
;;; ((array-getter a) j_1 ... j_n) => x
;;;
;;; and
;;;
;;; ((array-getter a) i_1 ... i_n) => v
;;;

(define (mutable-array? obj)
  (and (array? obj)
       (not (eq? (##array-base-setter obj) #f))))

(define (array-setter obj)
  (cond ((not (mutable-array? obj))
	 (error "array-setter: object is not an mutable array: " obj))
	(else
	 (##array-base-setter obj))))

;;;
;;; A storage-class contains functions and objects to manipulate the
;;; backing store of a specialized-array.
;;;
;;; getter:   (lambda (body i) ...)   returns the value of body at index i
;;; setter:   (lambda (body i v) ...) sets the value of body at index i to v
;;; checker:  (lambda (val) ...)      checks that val is an appropriate value for storing in (maker n)
;;; maker:    (lambda (n val) ...)    makes a body of length n with value val
;;; length:   (lambda (body) ...)     returns the number of objects in body
;;; default:  object                  is the default value with which to fill body
;;;

(define-structure storage-class getter setter checker maker length default)

;;; We define specialized storage-classes for:
;;;
;;; 32- and 64-bit floating-point numbers,
;;; complex numbers with real and imaginary parts of 32- and 64-bit floating-point numbers respectively
;;; 8-, 16-, 32-, and 64-bit signed integers,
;;; 8-, 16-, 32-, and 64-bit unsigned integers, and
;;; 1-bit unsigned integers
;;;
;;; as well as generic objects.

(define-macro (make-standard-storage-classes)
  
  (define (symbol-concatenate . symbols)
    (string->symbol (apply string-append (map (lambda (s)
						(if (string? s)
						    s
						    (symbol->string s)))
					      symbols))))
  
  `(begin
     ,@(map (lambda (name prefix default checker)
	      `(define ,(symbol-concatenate name '-storage-class)
		 (make-storage-class
		  ;; getter:
		  (lambda (v i)
		    (,(symbol-concatenate prefix 'vector-ref) v i))
		  ;; setter:
		  (lambda (v i val)
		    (,(symbol-concatenate prefix 'vector-set!) v i val))
		  ;; checker
		  ,checker
		  ;; maker:
		  ,(symbol-concatenate 'make- prefix 'vector)
		  ;; length:
		  ,(symbol-concatenate prefix 'vector-length)
		  ;; default:
		  ,default)))
	    '(generic s8 u8 s16 u16 s32 u32 s64 u64 f32 f64)
	    '(""      s8 u8 s16 u16 s32 u32 s64 u64 f32 f64)
	    '(#f       0  0   0   0   0   0   0   0 0.0 0.0)
	    `((lambda (x) #t)                             ; generic
	      (lambda (x)                                 ; s8
		(and (##fixnum? x)
		     (fx<= ,(- (expt 2 7))
			   x
			   ,(- (expt 2 7) 1))))
	      (lambda (x)                                ; u8
		(and (##fixnum? x)
		     (fx<= 0
			   x
			   ,(- (expt 2 8) 1))))
	      (lambda (x)                               ; s16
		(and (##fixnum? x)
		     (fx<= ,(- (expt 2 15))
			   x
			   ,(- (expt 2 15) 1))))
	      (lambda (x)                               ; u16
		(and (##fixnum? x)
		     (fx<= 0
			   x
			   ,(- (expt 2 16) 1))))
	      (lambda (x)                               ; s32
		(declare (generic))
		(and (##exact-integer? x)
		     (<= ,(- (expt 2 31))
			 x
			 ,(- (expt 2 31) 1))))
	      (lambda (x)                               ; u32
		(declare (generic))
		(and (##exact-integer? x)
		     (<= 0
			 x
			 ,(- (expt 2 32) 1))))
	      (lambda (x)                              ; s64
		(declare (generic))
		(and (##exact-integer? x)
		     (<= ,(- (expt 2 63))
			 x
			 ,(- (expt 2 63) 1))))
	      (lambda (x)                              ; u64
		(declare (generic))
		(and (##exact-integer? x)
		     (<= 0
			 x
			 ,(- (expt 2 64) 1))))
	      (lambda (x) (##flonum? x))               ; f32
	      (lambda (x) (##flonum? x))               ; f64
	      ))))

(make-standard-storage-classes)

;;; for bit-arrays, body is a vector, the first element of which is the actual number of elements,
;;; the second element of which is a u16vector that contains the bit string

(define u1-storage-class
  (make-storage-class
   ;; getter:
   (lambda (v i)
     (let ((index (fxarithmetic-shift-right i 4))
	   (shift (fxand i 15))
	   (bodyv (vector-ref v  1)))
       (fxand
	(fxarithmetic-shift-right
	 (u16vector-ref bodyv index)
	 shift)
	1)))
   ;; setter: 
   (lambda (v i val)
     (let ((index (fxarithmetic-shift-right i 4))
	   (shift (fxand i 15))
	   (bodyv (vector-ref v  1)))
       (u16vector-set! bodyv index (fxior (fxarithmetic-shift-left val shift)
					  (fxand (u16vector-ref bodyv index)
						 (fxnot  (fxarithmetic-shift-left 1 shift)))))))
   ;; checker
   (lambda (val)
     (and (##fixnum? val)
	  (eq? 0 (fxand -2 val))))
   ;; maker:
   (lambda (size initializer)
     (let ((u16-size (fxarithmetic-shift-right (+ size 15) 4)))
       (vector size (make-u16vector u16-size (if (zero? initializer) 0 65535)))))
   ;; length:
   (lambda (v)
     (vector-ref v 0))
   ;; default:
   0))

(define-macro (make-complex-storage-classes)
  (define (symbol-concatenate . symbols)
    (string->symbol (apply string-append (map (lambda (s)
						(if (string? s)
						    s
						    (symbol->string s)))
					      symbols))))
  (define construct
    (lambda (size)
      (let ((prefix (string-append "c" (number->string (fx* 2 size))))
	    (floating-point-prefix (string-append "f" (number->string size))))
	`(define ,(symbol-concatenate prefix '-storage-class)
	   (make-storage-class
	    ;; getter
	    (lambda (body i)
	      (make-rectangular (,(symbol-concatenate floating-point-prefix 'vector-ref) body (fx* 2 i))
				(,(symbol-concatenate floating-point-prefix 'vector-ref) body (fx+ (fx* 2 i) 1))))
	    ;; setter
	    (lambda (body i obj)
	      (,(symbol-concatenate floating-point-prefix 'vector-set!) body (fx* 2 i)         (real-part obj))
	      (,(symbol-concatenate floating-point-prefix 'vector-set!) body (fx+ (fx* 2 i) 1) (imag-part obj)))
	    ;; checker
	    (lambda (obj)
	      (and (complex? obj)
		   (inexact? (real-part obj))
		   (inexact? (imag-part obj))))
	    ;; maker
	    (lambda (n val)
	      (let ((l (* 2 n))
		    (re (real-part val))
		    (im (imag-part val)))
		(let ((result (,(symbol-concatenate 'make-
						    floating-point-prefix
						    'vector)
			       l)))
		  (do ((i 0 (+ i 2)))
		      ((= i l) result)
		    (,(symbol-concatenate floating-point-prefix 'vector-set!) result i re)
		    (,(symbol-concatenate floating-point-prefix 'vector-set!) result (fx+ i 1) im)))))
	    ;; length
	    (lambda (body)
	      (fxquotient (,(symbol-concatenate floating-point-prefix 'vector-length) body) 2))
	    ;; default
	    0.+0.i)))))
  (let ((result
	 `(begin
	    ,@(map construct
		   '(32 64)))))
    result))

(make-complex-storage-classes)

;;; 
;;; Conceptually, an indexer is itself a 1-1 array that maps one interval to another; thus, it is
;;; an example of an array that can return multiple values.
;;; 
;;; Rather than trying to formalize this idea, and trying to get it to work with array-map,
;;; array-fold, ..., we'll just manipulate the getter functions of these conceptual arrays.
;;; 
;;; Indexers are 1-1 affine maps from one interval to another.
;;; 
;;; The indexer field of a specialized-array obj is a 1-1 mapping from
;;; 
;;; (array-domain obj)
;;; 
;;; to [0, top), where top is 
;;; 
;;; ((storage-class-length (array-storage-class obj)) (array-body obj))
;;; 

;; unfortunately, the next two functions were written by hand, so beware of bugs.

(define (##indexer-1 base
		     low-0
		     increment-0)
  (if (zero? base)
      (if (zero? low-0)
	  (cond ((= 1 increment-0)    (lambda (i) i))
		;;((= -1 increment-0)   (lambda (i) (- i)))               ;; an impossible case
		(else                 (lambda (i) (* i increment-0))))
	  (cond ((= 1 increment-0)    (lambda (i) (- i low-0)))
		;;((= -1 increment-0)   (lambda (i) (- low-0 i)))         ;; an impossible case
		(else                 (lambda (i) (* increment-0 (- i low-0))))))
      (if (zero? low-0)
	  (cond ((= 1 increment-0)    (lambda (i) (+ base i)))
		((= -1 increment-0)   (lambda (i) (- base i)))
		(else                 (lambda (i) (+ base (* increment-0 i)))))
	  (cond ((= 1 increment-0)    (lambda (i) (+ base (- i low-0))))
		((= -1 increment-0)   (lambda (i) (+ base (- low-0 i))))
		(else                 (lambda (i) (+ base (* increment-0 (- i low-0)))))))))

(define (##indexer-2 base
		     low-0       low-1
		     increment-0 increment-1)
  (if (zero? base)
      (if (zero? low-0)
	  (cond ((= 1 increment-0)
		 (if (zero? low-1)
		     (cond ((= 1 increment-1)    (lambda (i j) (+ i j)))
			   ((= -1 increment-1)   (lambda (i j) (+ i (- j))))
			   (else                 (lambda (i j) (+ i (* increment-1 j)))))
		     (cond ((= 1 increment-1)    (lambda (i j) (+ i (- j low-1))))
			   ((= -1 increment-1)   (lambda (i j) (+ i (- low-1 j))))
			   (else                 (lambda (i j) (+ i (* increment-1 (- j low-1))))))))
		#;((= -1 increment-0)         ;; an impossible case
		 (if (zero? low-1)
		     (cond ((= 1 increment-1)   (lambda (i j) (- j                           i)))
			   ((= -1 increment-1)  (lambda (i j) (- (- j)                       i)))
			   (else                (lambda (i j) (- (* increment-1 j)           i))))
		     (cond ((= 1 increment-1)   (lambda (i j) (- (- j low-1)                 i)))
			   ((= -1 increment-1)  (lambda (i j) (- (- low-1 j)                 i)))
			   (else                (lambda (i j) (- (* increment-1 (- j low-1)) i))))))
		(else
		 (if (zero? low-1)
		     (cond ((= 1 increment-1)   (lambda (i j) (+ (* increment-0 i) j)))
			   ((= -1 increment-1)  (lambda (i j) (+ (* increment-0 i) (- j))))
			   (else                (lambda (i j) (+ (* increment-0 i) (* increment-1 j)))))
		     (cond ((= 1 increment-1)   (lambda (i j) (+ (* increment-0 i) (- j low-1))))
			   ((= -1 increment-1)  (lambda (i j) (+ (* increment-0 i) (- low-1 j))))
			   (else                (lambda (i j) (+ (* increment-0 i) (* increment-1 (- j low-1)))))))))
	  (cond ((= 1 increment-0)
		 (if (zero? low-1)
		     (cond ((= 1 increment-1)    (lambda (i j) (+ (- i low-0) j)))
			   ((= -1 increment-1)   (lambda (i j) (+ (- i low-0) (- j))))
			   (else                 (lambda (i j) (+ (- i low-0) (* increment-1 j)))))
		     (cond ((= 1 increment-1)    (lambda (i j) (+ (- i low-0) (- j low-1))))
			   ((= -1 increment-1)   (lambda (i j) (+ (- i low-0) (- low-1 j))))
			   (else                 (lambda (i j) (+ (- i low-0) (* increment-1 (- j low-1))))))))
		#;((= -1 increment-0)         ;; an impossible case
		 (if (zero? low-1)
		     (cond ((= 1 increment-1)   (lambda (i j) (- j                           (- i low-0))))
			   ((= -1 increment-1)  (lambda (i j) (- (- j)                       (- i low-0))))
			   (else                (lambda (i j) (- (* increment-1 j)           (- i low-0)))))
		     (cond ((= 1 increment-1)   (lambda (i j) (- (- j low-1)                 (- i low-0))))
			   ((= -1 increment-1)  (lambda (i j) (- (- low-1 j)                 (- i low-0))))
			   (else                (lambda (i j) (- (* increment-1 (- j low-1)) (- i low-0)))))))
		(else
		 (if (zero? low-1)
		     (cond ((= 1 increment-1)   (lambda (i j) (+ (* increment-0 (- i low-0)) j)))
			   ((= -1 increment-1)  (lambda (i j) (+ (* increment-0 (- i low-0)) (- j))))
			   (else                (lambda (i j) (+ (* increment-0 (- i low-0)) (* increment-1 j)))))
		     (cond ((= 1 increment-1)   (lambda (i j) (+ (* increment-0 (- i low-0)) (- j low-1))))
			   ((= -1 increment-1)  (lambda (i j) (+ (* increment-0 (- i low-0)) (- low-1 j))))
			   (else                (lambda (i j) (+ (* increment-0 (- i low-0)) (* increment-1 (- j low-1))))))))))
      (if (zero? low-0)
	  (cond ((= 1 increment-0)
		 (if (zero? low-1)
		     (cond ((= 1 increment-1)    (lambda (i j) (+ base i j)))
			   ((= -1 increment-1)   (lambda (i j) (+ base i (- j))))
			   (else                 (lambda (i j) (+ base i (* increment-1 j)))))
		     (cond ((= 1 increment-1)    (lambda (i j) (+ base i (- j low-1))))
			   ((= -1 increment-1)   (lambda (i j) (+ base i (- low-1 j))))
			   (else                 (lambda (i j) (+ base i (* increment-1 (- j low-1))))))))
		((= -1 increment-0)
		 (if (zero? low-1)
		     (cond ((= 1 increment-1)   (lambda (i j) (- (+ base j)                           i)))
			   ((= -1 increment-1)  (lambda (i j) (- (- base j)                           i)))
			   (else                (lambda (i j) (- (+ base (* increment-1 j))           i))))
		     (cond ((= 1 increment-1)   (lambda (i j) (- (+ base (- j low-1))                 i)))
			   ((= -1 increment-1)  (lambda (i j) (- (+ base (- low-1 j))                 i)))
			   (else                (lambda (i j) (- (+ base (* increment-1 (- j low-1))) i))))))
		(else
		 (if (zero? low-1)
		     (cond ((= 1 increment-1)   (lambda (i j) (+ base (* increment-0 i) j)))
			   ((= -1 increment-1)  (lambda (i j) (+ base (* increment-0 i) (- j))))
			   (else                (lambda (i j) (+ base (* increment-0 i) (* increment-1 j)))))
		     (cond ((= 1 increment-1)   (lambda (i j) (+ base (* increment-0 i) (- j low-1))))
			   ((= -1 increment-1)  (lambda (i j) (+ base (* increment-0 i) (- low-1 j))))
			   (else                (lambda (i j) (+ base (* increment-0 i) (* increment-1 (- j low-1)))))))))
	  (cond ((= 1 increment-0)
		 (if (zero? low-1)
		     (cond ((= 1 increment-1)    (lambda (i j) (+ base (- i low-0) j)))
			   ((= -1 increment-1)   (lambda (i j) (+ base (- i low-0) (- j))))
			   (else                 (lambda (i j) (+ base (- i low-0) (* increment-1 j)))))
		     (cond ((= 1 increment-1)    (lambda (i j) (+ base (- i low-0) (- j low-1))))
			   ((= -1 increment-1)   (lambda (i j) (+ base (- i low-0) (- low-1 j))))
			   (else                 (lambda (i j) (+ base (- i low-0) (* increment-1 (- j low-1))))))))
		((= -1 increment-0)
		 (if (zero? low-1)
		     (cond ((= 1 increment-1)   (lambda (i j) (- (+ base j)                           (- i low-0))))
			   ((= -1 increment-1)  (lambda (i j) (- (- base j)                           (- i low-0))))
			   (else                (lambda (i j) (- (+ base (* increment-1 j))           (- i low-0)))))
		     (cond ((= 1 increment-1)   (lambda (i j) (- (+ base (- j low-1))                 (- i low-0))))
			   ((= -1 increment-1)  (lambda (i j) (- (+ base (- low-1 j))                 (- i low-0))))
			   (else                (lambda (i j) (- (+ base (* increment-1 (- j low-1))) (- i low-0)))))))
		(else
		 (if (zero? low-1)
		     (cond ((= 1 increment-1)   (lambda (i j) (+ base (* increment-0 (- i low-0)) j)))
			   ((= -1 increment-1)  (lambda (i j) (+ base (* increment-0 (- i low-0)) (- j))))
			   (else                (lambda (i j) (+ base (* increment-0 (- i low-0)) (* increment-1 j)))))
		     (cond ((= 1 increment-1)   (lambda (i j) (+ base (* increment-0 (- i low-0)) (- j low-1))))
			   ((= -1 increment-1)  (lambda (i j) (+ base (* increment-0 (- i low-0)) (- low-1 j))))
			   (else                (lambda (i j) (+ base (* increment-0 (- i low-0)) (* increment-1 (- j low-1))))))))))))

;;; after this we basically punt

(define (##indexer-3 base
		     low-0       low-1       low-2
		     increment-0 increment-1 increment-2)
  (if (= 0 low-0 low-1 low-2)
      (if (= base 0)
	  (if (= increment-2 1)
	      (lambda (i j k)
		(+ (* increment-0 i)
		   (* increment-1 j)
		   k))
	      (lambda (i j k)
		(+ (* increment-0 i)
		   (* increment-1 j)
		   (* increment-2 k))))
	  (if (= increment-2 1)
	      (lambda (i j k)
		(+ base
		   (* increment-0 i)
		   (* increment-1 j)
		   k))
	      (lambda (i j k)
		(+ base
		   (* increment-0 i)
		   (* increment-1 j)
		   (* increment-2 k)))))
      (if (= base 0)
	  (if (= increment-2 1)
	      (lambda (i j k)
		(+ (* increment-0 (- i low-0))
		   (* increment-1 (- j low-1))
		   (- k low-2)))
	      (lambda (i j k)
		(+ (* increment-0 (- i low-0))
		   (* increment-1 (- j low-1))
		   (* increment-2 (- k low-2)))))
	  (if (= increment-2 1)
	      (lambda (i j k)
		(+ base
		   (* increment-0 (- i low-0))
		   (* increment-1 (- j low-1))
		   (- k low-2)))
	      (lambda (i j k)
		(+ base
		   (* increment-0 (- i low-0))
		   (* increment-1 (- j low-1))
		   (* increment-2 (- k low-2))))))))

(define (##indexer-4 base
		     low-0       low-1       low-2       low-3
		     increment-0 increment-1 increment-2 increment-3)
  (if (= 0 low-0 low-1 low-2 low-3)
      (if (= base 0)
	  (if (= increment-3 1)
	      (lambda (i j k l)
		(+ (* increment-0 i)
		   (* increment-1 j)
		   (* increment-2 k)
		   l))
	      (lambda (i j k l)
		(+ (* increment-0 i)
		   (* increment-1 j)
		   (* increment-2 k)
		   (* increment-3 l))))
	  (if (= increment-3 1)
	      (lambda (i j k l)
		(+ base
		   (* increment-0 i)
		   (* increment-1 j)
		   (* increment-2 k)
		   l))
	      (lambda (i j k l)
		(+ base
		   (* increment-0 i)
		   (* increment-1 j)
		   (* increment-2 k)
		   (* increment-3 l)))))
      (if (= base 0)
	  (if (= increment-3 1)
	      (lambda (i j k l)
		(+ (* increment-0 (- i low-0))
		   (* increment-1 (- j low-1))
		   (* increment-2 (- k low-2))
		   (- l low-3)))
	      (lambda (i j k l)
		(+ (* increment-0 (- i low-0))
		   (* increment-1 (- j low-1))
		   (* increment-2 (- k low-2))
		   (* increment-3 (- l low-3)))))
	  (if (= increment-3 1)
	      (lambda (i j k l)
		(+ base
		   (* increment-0 (- i low-0))
		   (* increment-1 (- j low-1))
		   (* increment-2 (- k low-2))
		   (- l low-3)))
	      (lambda (i j k l)
		(+ base
		   (* increment-0 (- i low-0))
		   (* increment-1 (- j low-1))
		   (* increment-2 (- k low-2))
		   (* increment-3 (- l low-3))))))))

(define (##indexer-generic base lower-bounds increments)
  (let ((result
	 (lambda multi-index
	   (do ((multi-index  multi-index  (cdr multi-index))
		(lower-bounds lower-bounds (cdr lower-bounds))
		(increments   increments   (cdr increments))
		(result       base         (+ result (* (car increments)
							(- (car multi-index)
							   (car lower-bounds))))))
	       ((null? multi-index) result)))))
    result))


;;; 
;;; The default getter and the setter of a specialized-array a are given by
;;; 
;;; (lambda (i_0 ... i_n-1)
;;;   ((storage-class-getter (array-storage-class a))
;;;    (array-body a)
;;;    ((array-indexer a) i_0 ... i_n-1)))
;;; 
;;; (lambda (v i_0 ... i_n-1)
;;;   ((storage-class-setter (array-storage-class a))
;;;    (array-body a)
;;;    ((array-indexer a) i_0 ... i_n-1)
;;;    v))
;;; 
;;; The default initializer-value is 
;;; 
;;; (storage-class-default (array-storage-class a))
;;; 
;;; The default body is
;;; 
;;; ((storage-class-maker (array-storage-class a))
;;;  (interval-volume domain)
;;;  initializer-value)
;;; 
;;; The default indexer is the mapping of
;;; the domain to the natural numbers in lexicographical order.
;;; 

(define (specialized-array? obj)
  (and (mutable-array? obj)
       (not (eq? (##array-base-body obj) #f))))

(define (array-body obj)
  (cond ((not (specialized-array? obj))
	 (error "array-body: argument is not a specialized array: " obj))
	(else
	 (##array-base-body obj))))

(define (array-indexer obj)
  (cond ((not (specialized-array? obj))
	 (error "array-indexer: argument is not a specialized array: " obj))
	(else
	 (##array-base-indexer obj))))

(define (array-storage-class obj)
  (cond ((not (specialized-array? obj))
	 (error "array-storage-class: argument is not a specialized array: " obj))
	(else
	 (##array-base-storage-class obj))))

(define (array-safe? obj)
  (cond ((not (specialized-array? obj))
	 (error "array-safe?: argument is not a specialized array: " obj))
	(else
	 (##array-base-safe? obj))))

(define (##finish-specialized-array domain storage-class body indexer safe?)
  (let ((storage-class-getter (storage-class-getter storage-class))
	(storage-class-setter (storage-class-setter storage-class))
	(checker (storage-class-checker storage-class))
	(indexer indexer)
	(body body))

    ;;; we write the following three macros to specialize the setters and getters in the
    ;;; non-safe case to reduce one more function call.

    (define-macro (expand-storage-class original-suffix replacement-suffix expr)
      
      (define (symbol-append . args)
	(string->symbol (apply string-append (map (lambda (x) (if (symbol? x) (symbol->string x) x)) args))))
      
      (define (replace old-symbol new-symbol expr)
	(let loop ((expr expr))
	  (cond ((pair? expr)           ;; we don't use map because of dotted argument list in general setter
		 (cons (loop (car expr))
		       (loop (cdr expr))))
		((eq? expr old-symbol)
		 new-symbol)
		(else
		 expr))))
      
      `(cond ,@(map (lambda (name prefix)
		      `((eq? storage-class ,(symbol-append name '-storage-class))
			,(replace (symbol-append 'storage-class original-suffix)
				  (symbol-append prefix 'vector replacement-suffix)
				  expr)))
		    '(generic s8 u8 s16 u16 s32 u32 s64 u64 f32 f64)
		    '(""      s8 u8 s16 u16 s32 u32 s64 u64 f32 f64))
	     (else
	      ,expr)))
    
    (define-macro (expand-getters expr)
      `(expand-storage-class -getter -ref ,expr))
    
    (define-macro (expand-setters expr)
      `(expand-storage-class -setter -set! ,expr))
    
    (let ((getter (if safe?
		      (case (##interval-dimension domain)
			((1)  (lambda (i)
				(cond ((not (##exact-integer? i))
				       (error "array-getter: multi-index component is not an exact integer: " i))
				      ((not (##interval-contains-multi-index?-1 domain i))
				       (error "array-getter: domain does not contain multi-index: "    domain i))
				      (else
				       (storage-class-getter body (indexer i))))))
			((2)  (lambda (i j)
				(cond ((not (and (##exact-integer? i)
						 (##exact-integer? j)))
				       (error "array-getter: multi-index component is not an exact integer: " i j))
				      ((not (##interval-contains-multi-index?-2 domain i j))
				       (error "array-getter: domain does not contain multi-index: "    domain i j))
				      (else
				       (storage-class-getter body (indexer i j))))))
			((3)  (lambda (i j k)
				(cond ((not (and (##exact-integer? i)
						 (##exact-integer? j)
						 (##exact-integer? k)))
				       (error "array-getter: multi-index component is not an exact integer: " i j k))
				      ((not (##interval-contains-multi-index?-3 domain i j k))
				       (error "array-getter: domain does not contain multi-index: "    domain i j k))
				      (else
				       (storage-class-getter body (indexer i j k))))))
			((4)  (lambda (i j k l)
				(cond ((not (and (##exact-integer? i)
						 (##exact-integer? j)
						 (##exact-integer? k)
						 (##exact-integer? l)))
				       (error "array-getter: multi-index component is not an exact integer: " i j k l))
				      ((not (##interval-contains-multi-index?-4 domain i j k l))
				       (error "array-getter: domain does not contain multi-index: "    domain i j k l))
				      (else
				       (storage-class-getter body (indexer i j k l))))))
			(else (lambda multi-index
				(cond ((not (##every ##exact-integer? multi-index))
				       (apply error "array-getter: multi-index component is not an exact integer: " multi-index))
				      ((not (= (##interval-dimension domain) (length multi-index)))
				       (apply error "array-getter: multi-index is not the correct dimension: " domain multi-index))
				      ((not (##interval-contains-multi-index?-general domain multi-index))
				       (apply error "array-getter: domain does not contain multi-index: "    domain multi-index))
				      (else
				       (storage-class-getter body (apply indexer multi-index)))))))
		      (case (##interval-dimension domain)
			((1)  (expand-getters (lambda (i)         (storage-class-getter body (indexer i)))))
			((2)  (expand-getters (lambda (i j)       (storage-class-getter body (indexer i j)))))
			((3)  (expand-getters (lambda (i j k)     (storage-class-getter body (indexer i j k)))))
			((4)  (expand-getters (lambda (i j k l)   (storage-class-getter body (indexer i j k l)))))
			(else (expand-getters (lambda multi-index (storage-class-getter body (apply indexer multi-index))))))))
	  (setter (if safe?
		      (case (##interval-dimension domain)
			((1)  (lambda (value i)
				(cond ((not (##exact-integer? i))
				       (error "array-setter: multi-index component is not an exact integer: " i))
				      ((not (##interval-contains-multi-index?-1 domain i))
				       (error "array-setter: domain does not contain multi-index: "    domain i))
				      ((not (checker value))
				       (error "array-setter: value cannot be stored in body: " value))
				      (else
				       (storage-class-setter body (indexer i) value)))))
			((2)  (lambda (value i j)
				(cond ((not (and (##exact-integer? i)
						 (##exact-integer? j)))
				       (error "array-setter: multi-index component is not an exact integer: " i j))
				      ((not (##interval-contains-multi-index?-2 domain i j))
				       (error "array-setter: domain does not contain multi-index: "    domain i j))
				      ((not (checker value))
				       (error "array-setter: value cannot be stored in body: " value))
				      (else
				       (storage-class-setter body (indexer i j) value)))))
			((3)  (lambda (value i j k)
				(cond ((not (and (##exact-integer? i)
						 (##exact-integer? j)
						 (##exact-integer? k)))
				       (error "array-setter: multi-index component is not an exact integer: " i j k))
				      ((not (##interval-contains-multi-index?-3 domain i j k))
				       (error "array-setter: domain does not contain multi-index: "    domain i j k))
				      ((not (checker value))
				       (error "array-setter: value cannot be stored in body: " value))
				      (else
				       (storage-class-setter body (indexer i j k) value)))))
			((4)  (lambda (value i j k l)
				(cond ((not (and (##exact-integer? i)
						 (##exact-integer? j)
						 (##exact-integer? k)
						 (##exact-integer? l)))
				       (error "array-setter: multi-index component is not an exact integer: " i j k l))
				      ((not (##interval-contains-multi-index?-4 domain i j k l))
				       (error "array-setter: domain does not contain multi-index: "    domain i j k l))
				      ((not (checker value))
				       (error "array-setter: value cannot be stored in body: " value))
				      (else
				       (storage-class-setter body (indexer i j k l) value)))))
			(else (lambda (value . multi-index)
				(cond ((not (##every ##exact-integer? multi-index))
				       (apply error "array-setter: multi-index component is not an exact integer: " multi-index))
				      ((not (= (##interval-dimension domain) (length multi-index)))
				       (apply error "array-setter: multi-index is not the correct dimension: " domain multi-index))
				      ((not (##interval-contains-multi-index?-general domain multi-index))
				       (apply error "array-setter: domain does not contain multi-index: "    domain multi-index))
				      ((not (checker value))
				       (error "array-setter: value cannot be stored in body: " value))
				      (else
				       (storage-class-setter body (apply indexer multi-index) value))))))
		      (case (##interval-dimension domain)
			((1)  (expand-setters (lambda (value i)             (storage-class-setter body (indexer i)                 value))))
			((2)  (expand-setters (lambda (value i j)           (storage-class-setter body (indexer i j)               value))))
			((3)  (expand-setters (lambda (value i j k)         (storage-class-setter body (indexer i j k)             value))))
			((4)  (expand-setters (lambda (value i j k l)       (storage-class-setter body (indexer i j k l)           value))))
			(else (expand-setters (lambda (value . multi-index) (storage-class-setter body (apply indexer multi-index) value))))))))
      (make-##array-base domain
			 getter
			 setter
			 storage-class
			 body
			 indexer
			 safe?))))

(define (##interval->basic-indexer interval)
  (case (interval-dimension interval)
    ((1) (let ((low-0 (interval-lower-bound interval 0))
               (increment-0 1))
           (##indexer-1 0 low-0 increment-0)))
    ((2) (let* ((low-0 (##interval-lower-bound interval 0))
                (low-1 (##interval-lower-bound interval 1))
                (increment-1 1)
                (increment-0 (* increment-1
                                (- (##interval-upper-bound interval 1)
                                   (##interval-lower-bound interval 1)))))
           (##indexer-2 0
                        low-0 low-1
                        increment-0 increment-1)))
    ((3) (let* ((low-0 (##interval-lower-bound interval 0))
                (low-1 (##interval-lower-bound interval 1))
                (low-2 (##interval-lower-bound interval 2))
                (increment-2 1)
                (increment-1 (* increment-2
                                (- (##interval-upper-bound interval 2)
                                   (##interval-lower-bound interval 2))))
                (increment-0 (* increment-1
                                (- (##interval-upper-bound interval 1)
                                   (##interval-lower-bound interval 1)))))
           (##indexer-3 0
                        low-0 low-1 low-2
                        increment-0 increment-1 increment-2)))
    ((4) (let* ((low-0 (##interval-lower-bound interval 0))
                (low-1 (##interval-lower-bound interval 1))
                (low-2 (##interval-lower-bound interval 2))
                (low-3 (##interval-lower-bound interval 3))
                (increment-3 1)
                (increment-2 (* increment-3
                                (- (##interval-upper-bound interval 3)
                                   (##interval-lower-bound interval 3))))
                (increment-1 (* increment-2
                                (- (##interval-upper-bound interval 2)
                                   (##interval-lower-bound interval 2))))
                (increment-0 (* increment-1
                                (- (##interval-upper-bound interval 1)
                                   (##interval-lower-bound interval 1)))))
           (##indexer-4 0
                        low-0 low-1 low-2 low-3
                        increment-0 increment-1 increment-2 increment-3)))
    (else
     (let ((lower-bounds (##interval-lower-bounds->list interval))
           (upper-bounds (##interval-upper-bounds->list interval)))
       (let ((ranges (map (lambda (u l) (- u l)) upper-bounds lower-bounds)))
         (do ((ranges (reverse ranges) (cdr ranges))
              (increments (list 1) (cons (* (car increments) (car ranges))
                                         increments)))
             ((null? (cdr ranges)) (##indexer-generic 0 lower-bounds increments))))))))

(define (make-specialized-array interval
                                #!optional
                                (storage-class generic-storage-class)
                                (safe? (specialized-array-default-safe?)))
  (cond ((not (interval? interval))
	 (error "make-specialized-array: The first argument is not an interval: " interval))
	((not (storage-class? storage-class))
	 (error "make-specialized-array: The second argument is not a storage-class: " interval storage-class))
	((not (boolean? safe?))
	 (error "make-specialized-array: The third argument is not a boolean: " interval storage-class safe?))
	(else
	 (let* ((body        ((storage-class-maker storage-class)
                              (##interval-volume interval)
                              (storage-class-default storage-class)))
		(indexer     (##interval->basic-indexer interval)))
	   (##finish-specialized-array interval
				       storage-class
				       body
				       indexer
				       safe?)))))

;;; 
;;; The domain of the result is the same as the domain of the argument.
;;; 
;;; Builds a new specialized-array and populates the body of the result with
;;; (array-getter array) applied to the elements of (array-domain array)


(define (array->specialized-array array
                                  #!optional
                                  (result-storage-class generic-storage-class)
                                  (safe? (specialized-array-default-safe?)))
  (cond ((not (array? array))
	 (error "array->specialized-array: Argument is not an array: " array))
	((not (storage-class? result-storage-class))
	 (error "array->specialized-array: result-storage-class is not a storage-class: " result-storage-class))
	((not (boolean? safe?))
	 (error "array->specialized-array: safe? is not a boolean: " safe?))
	(else
	 (let* ((domain               (array-domain array))
		(result               (make-specialized-array domain
							      result-storage-class
							      safe?))
		(getter               (array-getter array)))
           (if (eq? result-storage-class generic-storage-class)   ;; checker always returns #t
               (let ((body      (array-body result))
                     ;; The result's indexer steps from 0 to (vector-length body) so we
                     ;; use that fact here instead of calling (array-indexer result).
                     (index     0))
                 (##interval-for-each
                  (case (##interval-dimension domain)
                    ((1)  (lambda (i)
                            (vector-set! body index (getter i))
                            (set! index (fx+ index 1))))
                    ((2)  (lambda (i j)
                            (vector-set! body index (getter i j))
                            (set! index (fx+ index 1))))
                    ((3)  (lambda (i j k)
                            (vector-set! body index (getter i j k))
                            (set! index (fx+ index 1))))
                    ((4)  (lambda (i j k l)
                            (vector-set! body index (getter i j k l))
                            (set! index (fx+ index 1))))
                    (else (lambda multi-index
                            (vector-set! body index (apply getter multi-index))
                            (set! index (fx+ index 1)))))
                  domain))
               (let ((checker              (storage-class-checker result-storage-class))
                     (body                 (array-body result))
                     (storage-class-setter (storage-class-setter result-storage-class))
                     ;; The result's indexer steps from 0 to (vector-length body) so we
                     ;; use that fact here instead of calling (array-indexer result).
                     (index                0))
                 (##interval-for-each
                  (case (##interval-dimension domain)
                    ((1)  (lambda (i)
                            (let ((item (getter i)))
                              (if (checker item)
                                  (begin (storage-class-setter body index item) (set! index (fx+ index 1)))
                                  (error "array->specialized-array: not all elements of the array can be manipulated by the storage class: "
                                         array result-storage-class safe?)))))
                    ((2)  (lambda (i j)
                            (let ((item (getter i j)))
                              (if (checker item)
                                  (begin (storage-class-setter body index item) (set! index (fx+ index 1)))
                                  (error "array->specialized-array: not all elements of the array can be manipulated by the storage class: "
                                         array result-storage-class safe?)))))
                    ((3)  (lambda (i j k)
                            (let ((item (getter i j k)))
                              (if (checker item)
                                  (begin (storage-class-setter body index item) (set! index (fx+ index 1)))
                                  (error "array->specialized-array: not all elements of the array can be manipulated by the storage class: "
                                         array result-storage-class safe?)))))
                    ((4)  (lambda (i j k l)
                            (let ((item (getter i j k l)))
                              (if (checker item)
                                  (begin (storage-class-setter body index item) (set! index (fx+ index 1)))
                                  (error "array->specialized-array: not all elements of the array can be manipulated by the storage class: "
                                         array result-storage-class safe?)))))
                    (else (lambda multi-index
                            (let ((item (apply getter multi-index)))
                              (if (checker item)
                                  (begin (storage-class-setter body index item) (set! index (fx+ index 1)))
                                  (error "array->specialized-array: not all elements of the array can be manipulated by the storage class: "
                                         array result-storage-class safe?))))))
                  domain)))
	   result))))

;;; 
;;; In the next function, old-indexer is an affine 1-1 mapping from an interval to [0,N), for some N.
;;; 
;;; new-domain->old-domain is an affine 1-1 mapping from new-domain to the domain of old-indexer.
;;; 

(define (##compose-indexers old-indexer new-domain new-domain->old-domain)
  (case (##interval-dimension new-domain)
    ((1) (let* ((lower-0 (##interval-lower-bound new-domain 0))
		(upper-0 (##interval-upper-bound new-domain 0))
		(base (call-with-values
			  (lambda () (new-domain->old-domain lower-0))
			old-indexer))
		(increment-0 (if (< (+ lower-0 1) upper-0)
				 (- (call-with-values
					(lambda () (new-domain->old-domain (+ lower-0 1)))
				      old-indexer)
				    base)
				 0)))
	   (##indexer-1 base lower-0 increment-0)))
    
    ((2) (let* ((lower-0 (##interval-lower-bound new-domain 0))
		(lower-1 (##interval-lower-bound new-domain 1))
		(upper-0 (##interval-upper-bound new-domain 0))
		(upper-1 (##interval-upper-bound new-domain 1))
		(base (call-with-values
			  (lambda () (new-domain->old-domain lower-0 lower-1))
			old-indexer))
		(increment-0 (if (< (+ lower-0 1) upper-0)
				 (- (call-with-values
					(lambda () (new-domain->old-domain (+ lower-0 1) lower-1))
				      old-indexer)
				    base)
				 0))
		(increment-1 (if (< (+ lower-1 1) upper-1)
				 (- (call-with-values
					(lambda () (new-domain->old-domain lower-0 (+ lower-1 1)))
				      old-indexer)
				    base)
				 0)))
	   (##indexer-2 base lower-0 lower-1 increment-0 increment-1)))
    ((3) (let* ((lower-0 (##interval-lower-bound new-domain 0))
		(lower-1 (##interval-lower-bound new-domain 1))
		(lower-2 (##interval-lower-bound new-domain 2))
		(upper-0 (##interval-upper-bound new-domain 0))
		(upper-1 (##interval-upper-bound new-domain 1))
		(upper-2 (##interval-upper-bound new-domain 2))
		(base (call-with-values
			  (lambda () (new-domain->old-domain lower-0 lower-1 lower-2))
			old-indexer))
		(increment-0 (if (< (+ lower-0 1) upper-0)
				 (- (call-with-values
					(lambda () (new-domain->old-domain (+ lower-0 1) lower-1 lower-2))
				      old-indexer)
				    base)
				 0))
		(increment-1 (if (< (+ lower-1 1) upper-1)
				 (- (call-with-values
					(lambda () (new-domain->old-domain lower-0 (+ lower-1 1) lower-2))
				      old-indexer)
				    base)
				 0))
		(increment-2 (if (< (+ lower-2 1) upper-2)
				 (- (call-with-values
					(lambda () (new-domain->old-domain lower-0 lower-1 (+ lower-2 1)))
				      old-indexer)
				    base)
				 0)))
	   (##indexer-3 base lower-0 lower-1 lower-2 increment-0 increment-1 increment-2)))
    ((4) (let* ((lower-0 (##interval-lower-bound new-domain 0))
		(lower-1 (##interval-lower-bound new-domain 1))
		(lower-2 (##interval-lower-bound new-domain 2))
		(lower-3 (##interval-lower-bound new-domain 3))
		(upper-0 (##interval-upper-bound new-domain 0))
		(upper-1 (##interval-upper-bound new-domain 1))
		(upper-2 (##interval-upper-bound new-domain 2))
		(upper-3 (##interval-upper-bound new-domain 3))
		(base (call-with-values
			  (lambda () (new-domain->old-domain lower-0 lower-1 lower-2 lower-3))
			old-indexer))
		(increment-0 (if (< (+ lower-0 1) upper-0)
				 (- (call-with-values
					(lambda () (new-domain->old-domain (+ lower-0 1) lower-1 lower-2 lower-3))
				      old-indexer)
				    base)
				 0))
		(increment-1 (if (< (+ lower-1 1) upper-1)
				 (- (call-with-values
					(lambda () (new-domain->old-domain lower-0 (+ lower-1 1) lower-2 lower-3))
				      old-indexer)
				    base)
				 0))
		(increment-2 (if (< (+ lower-2 1) upper-2)
				 (- (call-with-values
					(lambda () (new-domain->old-domain lower-0 lower-1 (+ lower-2 1) lower-3))
				      old-indexer)
				    base)
				 0))
		(increment-3 (if (< (+ lower-3 1) upper-3)
				 (- (call-with-values
					(lambda () (new-domain->old-domain lower-0 lower-1 lower-2 (+ lower-3 1)))
				      old-indexer)
				    base)
				 0)))
	   (##indexer-4 base lower-0 lower-1 lower-2 lower-3 increment-0 increment-1 increment-2 increment-3)))
    (else
     (let* ((lower-bounds (##interval-lower-bounds->list new-domain))
	    (upper-bounds (##interval-upper-bounds->list new-domain))
	    (base (call-with-values
		      (lambda () (apply new-domain->old-domain lower-bounds))
		    old-indexer))
	    (increments (let ((increments   (map (lambda (x) 0) lower-bounds))
			      (lower-bounds (map (lambda (x) x) lower-bounds)))
			  (let loop ((l lower-bounds)
				     (u upper-bounds)
				     (i increments)
				     (base base))
			    (if (null? l)
				increments
				(let ((new-base
				       (if (< (+ (car l) 1)
					      (car u))
					   (begin
					     (set-car! l (+ (car l) 1))
					     (let ((new-base (call-with-values
								 (lambda () (apply new-domain->old-domain lower-bounds))
							       old-indexer)))
					       (set-car! i (- new-base base))
					       new-base))
					   base)))
				  (loop (cdr l)
					(cdr u)
					(cdr i)
					new-base)))))))
       (##indexer-generic base lower-bounds increments)))))

;;; 
;;; You want to share the backing store of array.
;;; 
;;; So you specify a new domain and an affine 1-1 mapping from the new-domain to the old-domain.
;;; 

(define (specialized-array-share array
				 new-domain
				 new-domain->old-domain
				 #!optional (safe? (specialized-array-default-safe?)))
  (cond ((not (specialized-array? array))
	 (error "specialized-array-share: array is not a specialized-array: " array))
	((not (interval? new-domain))
	 (error "specialized-array-share: new-domain is not an interval: " new-domain))
	((not (procedure? new-domain->old-domain))
	 (error "specialized-array-share: new-domain->old-domain is not a procedure: " new-domain->old-domain))
	((not (boolean? safe?))
	 (error "specialized-array-share: safe? is not a boolean: " safe?))
	(else
	 (let ((old-domain        (array-domain       array))
	       (old-indexer       (array-indexer      array))
	       (body              (array-body         array))
	       (storage-class     (array-storage-class array)))
	   (##finish-specialized-array new-domain
				       storage-class
				       body
				       (##compose-indexers old-indexer new-domain new-domain->old-domain)
				       safe?)))))

(define (##immutable-array-extract array new-domain)
  (make-array new-domain
	      (array-getter array)))

(define (##mutable-array-extract array new-domain)
  (make-array new-domain
	      (array-getter array)
	      (array-setter array)))

(define (##specialized-array-extract array new-domain)
  ;; call ##finish-specialized-array instead of filling the entries of #array-base
  ;; by hand because specialized-array-default-safe? may not be the same as
  ;; (array-safe? array)
  (##finish-specialized-array new-domain
			      (array-storage-class array)
			      (array-body array)
			      (array-indexer array)
			      (specialized-array-default-safe?)))

(define (array-extract array new-domain)
  (cond ((not (array? array))
	 (error "array-extract: The first argument is not an array: " array new-domain))
	((not (interval? new-domain))
	 (error "array-extract: The second argument is not an interval: " array new-domain))
	((not (##interval-subset? new-domain (array-domain array)))
	 (error "array-extract: The second argument (an interval) is not a subset of the domain of the first argument (an array): " array new-domain))
	((specialized-array? array)
	 (##specialized-array-extract array new-domain))
	((mutable-array? array)
	 (##mutable-array-extract array new-domain))
	(else
	 (##immutable-array-extract array new-domain))))

(define (##getter-translate getter translation)
  (case (vector-length translation)
    ((1) (lambda (i)
	   (getter (- i (vector-ref translation 0)))))
    ((2) (lambda (i j)
	   (getter (- i (vector-ref translation 0))
		   (- j (vector-ref translation 1)))))
    ((3) (lambda (i j k)
	   (getter (- i (vector-ref translation 0))
		   (- j (vector-ref translation 1))
		   (- k (vector-ref translation 2)))))
    ((4) (lambda (i j k l)
	   (getter (- i (vector-ref translation 0))
		   (- j (vector-ref translation 1))
		   (- k (vector-ref translation 2))
		   (- l (vector-ref translation 3)))))
    (else
     (let ((n (vector-length translation))
	   (translation-list (vector->list translation)))
       (lambda indices
	 (cond ((not (= (length indices) n))
		(error "The number of indices does not equal the array dimension: " indices))
	       (else
		(apply getter (map - indices translation-list)))))))))

(define (##setter-translate setter translation)
  (case (vector-length translation)
    ((1) (lambda (v i)
	   (setter v
		   (- i (vector-ref translation 0)))))
    ((2) (lambda (v i j)
	   (setter v
		   (- i (vector-ref translation 0))
		   (- j (vector-ref translation 1)))))
    ((3) (lambda (v i j k)
	   (setter v
		   (- i (vector-ref translation 0))
		   (- j (vector-ref translation 1))
		   (- k (vector-ref translation 2)))))
    ((4) (lambda (v i j k l)
	   (setter v
		   (- i (vector-ref translation 0))
		   (- j (vector-ref translation 1))
		   (- k (vector-ref translation 2))
		   (- l (vector-ref translation 3)))))
    (else
     (let ((n (vector-length translation))
	   (translation-list (vector->list translation)))
       (lambda (v . indices)
	 (cond ((not (= (length indices) n))
		(error "The number of indices does not equal the array dimension: " v indices))
	       (else
		(apply setter v (map - indices translation-list)))))))))

(define (##immutable-array-translate array translation)
  (make-array (##interval-translate (array-domain array) translation)
	      (##getter-translate (array-getter array) translation)))

(define (##mutable-array-translate array translation)
  (make-array (##interval-translate (array-domain array) translation)
	      (##getter-translate (array-getter array) translation)
	      (##setter-translate (array-setter array) translation)))

(define (##specialized-array-translate array translation)
  (specialized-array-share array
			   (##interval-translate (array-domain array) translation)
			   (##getter-translate values translation)))

(define (array-translate array translation)
  (cond ((not (array? array))
	 (error "array-translate: the first argument is not an array: " array translation))
	((not (translation? translation))
	 (error "array-translate: the second argument is not a vector of exact integers: " array translation))
	((not (fx= (array-dimension array)
		   (vector-length translation)))
	 (error "array-translate: the dimension of the first argument (an array) does not equal the dimension of the second argument (a vector): " array translation))
	((specialized-array? array)
	 (##specialized-array-translate array translation))
	((mutable-array? array)
	 (##mutable-array-translate array translation))
	(else
	 (##immutable-array-translate array translation))))

(define-macro (setup-permuted-getters-and-setters)

  (define (iota n)
    ;; generates list of (- n 1) ... 0
    (if (zero? n)
	'()
	(cons (- n 1) (iota (- n 1)))))

  (define (list-remove l i)
    ;; new list that removes (list-ref l i) from l
    (if (zero? i)
	(cdr l)
	(cons (car l)
	      (list-remove (cdr l) (- i 1)))))

  (define (list-take l i)
    ;; makes new list of first i items of l
    (if (zero? i)
	'()
	(cons (car l)
	      (list-take (cdr l) (- i 1)))))


  (define (permutations l)
    ;; generates list of all permutations of l
    (if (null? (cdr l))
	(list l)
	(apply append (map (lambda (i)
			     (let ((x    (list-ref l i))
				   (rest (list-remove l i)))
			       (map (lambda (tail)
				      (cons x tail))
				    (permutations rest))))
			   (reverse (iota (length l)))))))

  (define (concat . args)
    (string->symbol (apply string-append (map (lambda (s) (if (string? s) s (symbol->string s ))) args))))

  (define (permuter name transform-arguments)
    `(define (,(concat name '-permute) ,name permutation)
       (case (vector-length permutation)
	 ,@(map (lambda (i)
		  `((,i) (cond ,@(let ((args (list-take '(i j k l) i)))
				   (map (lambda (perm permuted-args)
					  `((equal? permutation ',(list->vector perm))
					    (lambda ,(transform-arguments permuted-args)
					      ,`(,name ,@(transform-arguments args)))))
					(permutations (list-take '(0 1 2 3) i))
					(permutations args))))))
		'(1 2 3 4))
	 (else
	  (let ((n (vector-length permutation))
		(permutation-inverse (##permutation-invert permutation)))
	    (lambda ,(transform-arguments 'indices)
	      (if (not (= (length indices) n))
		  (error "number of indices does not equal permutation dimension: " indices permutation)
		  (apply ,name ,@(transform-arguments '((##vector-permute->list (list->vector indices) permutation-inverse)))))))))))

  (let ((result
	 `(begin
	    ,(permuter '##getter values)
	    ,(permuter '##setter (lambda (args) (cons 'v args))))))
    result))

(setup-permuted-getters-and-setters)

(define (##immutable-array-permute array permutation)
  (make-array (##interval-permute (array-domain array) permutation)
	      (##getter-permute (array-getter array) permutation)))

(define (##mutable-array-permute array permutation)
  (make-array (##interval-permute (array-domain array) permutation)
	      (##getter-permute (array-getter array) permutation)
	      (##setter-permute (array-setter array) permutation)))

(define (##specialized-array-permute array permutation)
  (specialized-array-share array
			   (##interval-permute (array-domain array) permutation)
			   (##getter-permute values permutation)))

(define (array-permute array permutation)
  (cond ((not (array? array))
	 (error "array-permute: the first argument is not an array: " array permutation))
	((not (permutation? permutation))
	 (error "array-permute: the second argument is not a permutation: " array permutation))
	((not (fx= (array-dimension array)
		   (vector-length permutation)))
	 (error "array-permute: the dimension of the first argument (an array) does not equal the dimension of the second argument (a permutation): " array permutation))
	((specialized-array? array)
	 (##specialized-array-permute array permutation))
	((mutable-array? array)
	 (##mutable-array-permute array permutation))
	(else
	 (##immutable-array-permute array permutation))))

(define-macro (setup-reversed-getters-and-setters)

  (define (make-symbol . args)
    (string->symbol
     (apply string-append
	    (map (lambda (x)
		   (cond ((string? x) x)
			 ((symbol? x) (symbol->string x))
			 ((number? x) (number->string x))))
		 args))))

  (define (truth-table n)   ;; generate all combinations of n #t and #f
    (if (zero? n)
	'(())
	(let ((subtable (truth-table (- n 1))))
	  (apply append (map (lambda (value)
			       (map (lambda (t)
				      (cons value t))
				    subtable))
			     '(#t #f))))))

  (define (iota n)
    ;; generates list of (- n 1) ... 0
    (if (zero? n)
	'()
	(cons (- n 1) (iota (- n 1)))))
  
  
  (define (generate-code-for-fixed-n name transformer n)
    (let ((zero-to-n-1
	   (reverse (iota n)))
	  (table
	   (truth-table n)))
      `((,n) (let (,@(map (lambda (k)
			    `(,(make-symbol 'adjust_ k) (+ (##interval-upper-bound interval ,k)
							   (##interval-lower-bound interval ,k)
							   -1)))
			  zero-to-n-1))
	       (cond ,@(map (lambda (table-entry)
			      `((equal? flip? ',(list->vector table-entry))
				(lambda ,(transformer (map (lambda (k)
							     (make-symbol 'i_ k))
							   zero-to-n-1))
				  (,name ,@(transformer (map (lambda (flip? k)
							       (if flip?
								   `(- ,(make-symbol 'adjust_ k)
								       ,(make-symbol 'i_ k))
								   `,(make-symbol 'i_ k)))
							     table-entry zero-to-n-1))))))
			    table))))))

  (define (reverser name transform-arguments)
    `(define (,(make-symbol name '-reverse) ,name flip? interval)
       (case (vector-length flip?)
	 ,@(map (lambda (n)
		  (generate-code-for-fixed-n name transform-arguments n))
		'(1 2 3 4))
	 (else
	  (let ((n
		 (vector-length flip?))
		(flip?
		 (vector->list flip?))
		(adjust
		 (map (lambda (u_k l_k)
			(+ u_k l_k -1))
		      (vector->list (##interval-upper-bounds interval))
		      (vector->list (##interval-lower-bounds interval)))))
	    (lambda ,(transform-arguments 'indices)
	      (if (not (= (length indices) n))
		  (error "number of indices does not equal array dimension: " indices)
		  (apply ,name ,@(transform-arguments '((map (lambda (i adjust flip?)
							       (if flip?
								   (- adjust i)
								   i))
							     indices adjust flip?)))))))))))
  (let ((result
	 `(begin
	    ,(reverser '##getter values)
	    ,(reverser '##setter (lambda (args) (cons 'v args))))))
    #;(pp result)
    result))

(setup-reversed-getters-and-setters)

(define (##immutable-array-reverse array flip?)
  (make-array (array-domain array)
	      (##getter-reverse (array-getter array) flip? (array-domain array))))

(define (##mutable-array-reverse array flip?)
  (make-array (array-domain array)
	      (##getter-reverse (array-getter array) flip? (array-domain array))
	      (##setter-reverse (array-setter array) flip? (array-domain array))))

(define (##specialized-array-reverse array flip?)
  (specialized-array-share array
			   (array-domain array)
			   (##getter-reverse values flip? (array-domain array))))

(define (array-reverse array flip?)
  (cond ((not (array? array))
	 (error "array-reverse: the first argument is not an array: " array flip?))
	((not (and (vector? flip?)
		   (##vector-every boolean? flip?)))
	 (error "array-reverse: the second argument is not a vector of booleans: " array flip?))
	((not (fx= (array-dimension array)
		   (vector-length flip?)))
	 (error "array-reverse: the dimension of the first argument (an array) does not equal the dimension of the second argument (a vector of booleans): " array flip?))
	((specialized-array? array)
	 (##specialized-array-reverse array flip?))
	((mutable-array? array)
	 (##mutable-array-reverse array flip?))
	(else
	 (##immutable-array-reverse array flip?))))



(define-macro (macro-generate-sample)
  
  (define (make-symbol . args)
    (string->symbol
     (apply string-append
	    (map (lambda (x)
		   (cond ((string? x) x)
			 ((symbol? x) (symbol->string x))
			 ((number? x) (number->string x))))
		 args))))
  
  (define (take l n)
    (if (zero? n)
        '()
        (cons (car l) (take (cdr l) (- n 1)))))
  
  (define (remove l n)
    (if (zero? n)
        l
        (remove (cdr l) (- n 1))))
  
  (define (first-half l)
    (take l (quotient (length l) 2)))
  
  (define (second-half l)
    (remove l (quotient (length l) 2)))
  
  (define (iota n)
    ;; generates list of (- n 1) ... 0
    (if (zero? n)
	'()
	(cons (- n 1) (iota (- n 1)))))
  
  (define (arg-lists ks)
    (if (null? ks)
        '(())
        (let* ((k (car ks))
               (i_k (make-symbol 'i_ k))
               (s_k (make-symbol 's_ k))
               (sublists
                (arg-lists (cdr ks)))
               (plains
                (map (lambda (l)
                       (cons i_k l))
                     sublists))
               (scales
                (map (lambda (l)
                       (cons `(* ,i_k ,s_k) l))
                     sublists)))
          (append plains
                scales))))

  (define (transformer args) args)
  (define name 'getter)

  (define (code-for-one-n name transformer n)
    (let* ((zero-to-n-1
            (reverse (iota n)))
           (arg-list
            (map (lambda (k)
                   (make-symbol 'i_ k))
                 zero-to-n-1))
           (args
            (arg-lists zero-to-n-1)))
      (define (build-code args ks)
        (if (null? (cdr args))
            `(lambda ,(transformer arg-list)
               (,name ,@(transformer (car args))))
            (let* ((k (car ks))
                   (s_k (make-symbol 's_ k))
                   (plains (first-half args))
                   (scales (second-half args)))
              `(if (= 1 ,s_k)
                   ,(build-code plains (cdr ks))
                   ,(build-code scales (cdr ks))))))
      `((,n)
        (let (,@(map (lambda (k)
                       `(,(make-symbol 's_ k) (vector-ref scales ,k)))
                     zero-to-n-1))
          ,(build-code args zero-to-n-1)))))
  
  (define (sampler name transformer)
    `(define (,(make-symbol name '-sample) ,name scales interval)
       (case (vector-length scales)
         ,@(map (lambda (n)
                  (code-for-one-n name transformer n))
                '(1 2 3 4))
         (else
          (let ((n
                 (vector-length scales))
                (scales
                 (vector->list scales)))
            (lambda ,(transformer 'indices)
              (if (not (= (length indices) n))
                  (error "number of indices does not equal array dimension: " indices)
                  (apply ,name ,@(transformer '((map (lambda (i s)
                                                       (* s i))
                                                     indices scales)))))))))))
                


  (let ((result
         `(begin
            ,(sampler '##getter values)
            ,(sampler '##setter (lambda (args) (cons 'v args))))))
    #;(pp result)
    result))

(macro-generate-sample)


(define (##immutable-array-sample array scales)
  (make-array (##interval-scale (array-domain array) scales)
              (##getter-sample (array-getter array) scales (array-domain array))))

(define (##mutable-array-sample array scales)
  (make-array (##interval-scale (array-domain array) scales)
	      (##getter-sample (array-getter array) scales (array-domain array))
	      (##setter-sample (array-setter array) scales (array-domain array))))

(define (##specialized-array-sample array scales)
  (specialized-array-share array
			   (##interval-scale (array-domain array) scales)
			   (##getter-sample values scales (array-domain array))))

(define (array-sample array scales)
  (cond ((not (and (array? array)
                   (##vector-every zero? (interval-lower-bounds->vector (array-domain array)))))
         (error "array-sample: The first argument is an array whose domain has nonzero lower bounds: " array scales))
        ((not (and (vector? scales)
                   (##vector-every ##exact-integer? scales)
                   (##vector-every positive? scales)))
         (error "array-sample: The second argument is not a vector of positive, exact, integers: " array scales))
        ((not (= (vector-length scales) (array-dimension array)))
         (error "array-sample: The dimension of the first argument (an array) is not equal to the length of the second (a vector): "
                array scales))
        ((specialized-array? array)
         (##specialized-array-sample array scales))
        ((mutable-array? array)
         (##mutable-array-sample array scales))
        (else
         (##immutable-array-sample array scales))))

(define (##immutable-array-curry array right-dimension)
  (call-with-values
      (lambda () (interval-projections (array-domain array) right-dimension))
    (lambda (left-interval right-interval)
      (let ((getter (array-getter array)))
	(make-array left-interval
		    (case (##interval-dimension left-interval)
		      ((1)  (case (##interval-dimension right-interval)
			      ((1)  (lambda (i)      (make-array right-interval (lambda (j)         (getter i j)))))
			      ((2)  (lambda (i)      (make-array right-interval (lambda (j k)       (getter i j k)))))
			      ((3)  (lambda (i)      (make-array right-interval (lambda (j k l)     (getter i j k l)))))
			      (else (lambda (i)      (make-array right-interval (lambda multi-index (apply getter i multi-index)))))))
		      ((2)  (case (##interval-dimension right-interval)
			      ((1)  (lambda (i j)    (make-array right-interval (lambda   (k)       (getter i j k)))))
			      ((2)  (lambda (i j)    (make-array right-interval (lambda   (k l)     (getter i j k l)))))
			      (else (lambda (i j)    (make-array right-interval (lambda multi-index (apply getter i j multi-index)))))))
		      ((3)  (case (##interval-dimension right-interval)
			      ((1)  (lambda (i j k)  (make-array right-interval (lambda     (l)     (getter i j k l)))))
			      (else (lambda (i j k)  (make-array right-interval (lambda multi-index (apply getter i j k multi-index)))))))
		      (else (lambda left-multi-index
			      (make-array right-interval
					  (lambda right-multi-index
					    (apply getter (append left-multi-index right-multi-index))))))))))))

(define (##mutable-array-curry array right-dimension)
  (call-with-values
      (lambda () (interval-projections (array-domain array) right-dimension))
    (lambda (left-interval right-interval)
      (let ((getter (array-getter array))
	    (setter   (array-setter   array)))
	(make-array left-interval
		    (case (##interval-dimension left-interval)
		      ((1)  (case (##interval-dimension right-interval)
			      ((1)  (lambda (i)     (make-array right-interval
								(lambda (  j)     (getter   i j))
								(lambda (v j)     (setter v i j)))))
			      ((2)  (lambda (i)     (make-array right-interval
								(lambda (  j k)   (getter   i j k))
								(lambda (v j k)   (setter v i j k)))))
			      ((3)  (lambda (i)     (make-array right-interval
								(lambda (  j k l) (getter   i j k l))
								(lambda (v j k l) (setter v i j k l)))))
			      (else (lambda (i)     (make-array right-interval
								(lambda      multi-index  (apply getter   i     multi-index))
								(lambda (v . multi-index) (apply setter v i     multi-index)))))))
		      ((2)  (case (##interval-dimension right-interval)
			      ((1)  (lambda (i j)   (make-array right-interval
								(lambda (    k)   (getter   i j k))
								(lambda (v   k)   (setter v i j k)))))
			      ((2)  (lambda (i j)   (make-array right-interval
								(lambda (    k l) (getter   i j k l))
								(lambda (v   k l) (setter v i j k l)))))
			      (else (lambda (i j)   (make-array right-interval
								(lambda      multi-index  (apply getter   i j   multi-index))
								(lambda (v . multi-index) (apply setter v i j   multi-index)))))))
		      ((3)  (case (##interval-dimension right-interval)
			      ((1)  (lambda (i j k) (make-array right-interval
								(lambda (      l) (getter   i j k l))
								(lambda (v     l) (setter v i j k l)))))
			      (else (lambda (i j k) (make-array right-interval
								(lambda      multi-index  (apply getter   i j k multi-index))
								(lambda (v . multi-index) (apply setter v i j k multi-index)))))))
		      (else (lambda left-multi-index
			      (make-array right-interval
					  (lambda      right-multi-index  (apply getter   (append left-multi-index right-multi-index)))
					  (lambda (v . right-multi-index) (apply setter v (append left-multi-index right-multi-index))))))))))))

(define (##specialized-array-curry array right-dimension)
  (call-with-values
      (lambda () (interval-projections (array-domain array) right-dimension))
    (lambda (left-interval right-interval)
      (make-array left-interval
		  (case (##interval-dimension left-interval)
		    ((1)  (case (##interval-dimension right-interval)
			    ((1)  (lambda (i)     (specialized-array-share array right-interval (lambda (j)                         (values i j    )))))
			    ((2)  (lambda (i)     (specialized-array-share array right-interval (lambda (j k)                       (values i j k  )))))
			    ((3)  (lambda (i)     (specialized-array-share array right-interval (lambda (j k l)                     (values i j k l)))))
			    (else (lambda (i)     (specialized-array-share array right-interval (lambda multi-index (apply values i     multi-index)))))))
		    ((2)  (case (##interval-dimension right-interval)
			    ((1)  (lambda (i j)   (specialized-array-share array right-interval (lambda (  k)                       (values i j k  )))))
			    ((2)  (lambda (i j)   (specialized-array-share array right-interval (lambda (  k l)                     (values i j k l)))))
			    (else (lambda (i j)   (specialized-array-share array right-interval (lambda multi-index (apply values i j   multi-index)))))))
		    ((3)  (case (##interval-dimension right-interval)
			    ((1)  (lambda (i j k) (specialized-array-share array right-interval (lambda (    l)                    (values i j k l)))))
			    (else (lambda (i j k) (specialized-array-share array right-interval (lambda multi-index (apply values i j k multi-index)))))))
		    (else (lambda left-multi-index 
			    (specialized-array-share array right-interval (lambda right-multi-index (apply values (append left-multi-index right-multi-index)))))))))))

(define (array-curry array right-dimension)
  (cond ((not (array? array))
	 (error "array-curry: The first argument is not an array: " array right-dimension))
	((not (##exact-integer? right-dimension))
	 (error "array-curry: The second argument is not an exact integer: " array right-dimension))
	((not (< 0 right-dimension (##interval-dimension (array-domain array))))
	 (error "array-curry: The second argument is not between 0 and (interval-dimension (array-domain array)) (exclusive): " array right-dimension))
	((specialized-array? array)
	 (##specialized-array-curry array right-dimension))
	((mutable-array? array)
	 (##mutable-array-curry array right-dimension))
	(else ; immutable array
	 (##immutable-array-curry array right-dimension))))

;;; 
;;; array-map returns an array whose domain is the same as the common domain of (cons array arrays)
;;; and whose getter is
;;; 
;;; (lambda multi-index
;;;   (apply f (map (lambda (g) (apply g multi-index)) (map array-getter (cons array arrays)))))
;;; 
;;; This function is also used in array-for-each, so we try to specialize the this
;;; function to speed things up a bit.
;;; 

(define (##specialize-function-applied-to-array-getters f array arrays)
  (let ((domain (array-domain array))
	(getter-0 (array-getter array)))
    (case (length arrays)
      ((0) (case (##interval-dimension domain)
	     ((1)  (lambda (i)         (f (getter-0 i))))
	     ((2)  (lambda (i j)       (f (getter-0 i j))))
	     ((3)  (lambda (i j k)     (f (getter-0 i j k))))
	     ((4)  (lambda (i j k l)   (f (getter-0 i j k l))))
	     (else (lambda multi-index (f (apply getter-0 multi-index))))))
      
      ((1) (let ((getter-1 (array-getter (car arrays))))
	     (case (##interval-dimension domain)
	       ((1)  (lambda (i)         (f (getter-0 i)
					    (getter-1 i))))
	       ((2)  (lambda (i j)       (f (getter-0 i j)
					    (getter-1 i j))))
	       ((3)  (lambda (i j k)     (f (getter-0 i j k)
					    (getter-1 i j k))))
	       ((4)  (lambda (i j k l)   (f (getter-0 i j k l)
					    (getter-1 i j k l))))
	       (else (lambda multi-index (f (apply getter-0 multi-index)
					    (apply getter-1 multi-index)))))))
      ((2) (let ((getter-1 (array-getter (car arrays)))
		 (getter-2 (array-getter (cadr arrays))))
	     (case (##interval-dimension domain)
	       ((1)  (lambda (i)         (f (getter-0 i)
					    (getter-1 i)
					    (getter-2 i))))
	       ((2)  (lambda (i j)       (f (getter-0 i j)
					    (getter-1 i j)
					    (getter-2 i j))))
	       ((3)  (lambda (i j k)     (f (getter-0 i j k)
					    (getter-1 i j k)
					    (getter-2 i j k))))
	       ((4)  (lambda (i j k l)   (f (getter-0 i j k l)
					    (getter-1 i j k l)
					    (getter-2 i j k l))))
	       (else (lambda multi-index (f (apply getter-0 multi-index)
					    (apply getter-1 multi-index)
					    (apply getter-2 multi-index)))))))
      ((3) (let ((getter-1 (array-getter (car arrays)))
		 (getter-2 (array-getter (cadr arrays)))
                 (getter-3 (array-getter (caddr arrays))))
	     (case (##interval-dimension domain)
	       ((1)  (lambda (i)         (f (getter-0 i)
					    (getter-1 i)
					    (getter-2 i)
                                            (getter-3 i))))
	       ((2)  (lambda (i j)       (f (getter-0 i j)
					    (getter-1 i j)
					    (getter-2 i j)
                                            (getter-3 i j))))
	       ((3)  (lambda (i j k)     (f (getter-0 i j k)
					    (getter-1 i j k)
					    (getter-2 i j k)
                                            (getter-3 i j k))))
	       ((4)  (lambda (i j k l)   (f (getter-0 i j k l)
					    (getter-1 i j k l)
					    (getter-2 i j k l)
                                            (getter-3 i j k l))))
	       (else (lambda multi-index (f (apply getter-0 multi-index)
					    (apply getter-1 multi-index)
					    (apply getter-2 multi-index)
                                            (apply getter-3 multi-index)))))))
      (else
       (let ((getters (cons getter-0 (map array-getter arrays))))
	 (case (##interval-dimension domain)
	   ((1)  (lambda (i)         (apply f (map (lambda (g) (g i))                 getters))))
	   ((2)  (lambda (i j)       (apply f (map (lambda (g) (g i j))               getters))))
	   ((3)  (lambda (i j k)     (apply f (map (lambda (g) (g i j k))             getters))))
	   ((4)  (lambda (i j k l)   (apply f (map (lambda (g) (g i j k l))           getters))))
	   (else (lambda multi-index (apply f (map (lambda (g) (apply g multi-index)) getters))))))))))

(define (array-map f array #!rest arrays)
  (cond ((not (procedure? f))
	 (apply error "array-map: The first argument is not a procedure: " f array arrays))
	((not (##every array? (cons array arrays)))
	 (apply error "array-map: Not all arguments after the first are arrays: " f array arrays))
	((not (##every (lambda (d) (##interval= d (array-domain array))) (map array-domain arrays)))
	 (apply error "array-map: Not all arguments after the first have the same domain: " f array arrays))
	(else
         (make-array (array-domain array)
                     (##specialize-function-applied-to-array-getters f array arrays)))))

;;; applies f to the elements of the arrays in lexicographical order.

(define (array-for-each f array #!rest arrays)
  (cond ((not (procedure? f))
	 (apply error "array-for-each: The first argument is not a procedure: " f array arrays))
	((not (##every array? (cons array arrays)))
	 (apply error "array-for-each: Not all arguments after the first are arrays: " f array arrays))
	((not (##every (lambda (d) (##interval= d (array-domain array))) (map array-domain arrays)))
	 (apply error "array-for-each: Not all arguments after the first have the same domain: " f array arrays))
	(else
	 (##interval-for-each (##specialize-function-applied-to-array-getters f array arrays)
                              (array-domain array)))))

(define-macro (macro-make-predicates)
  
  (define (concat . args)
    (string->symbol (apply string-append (map (lambda (s) (if (string? s) s (symbol->string s ))) args))))
  
  (define (make-predicate name connector)
    `(define (,(concat '##interval- name) f interval)
       (case (##interval-dimension interval)
         ((1) (let ((lower-i (##interval-lower-bound interval 0))
                    (upper-i (##interval-upper-bound interval 0))
                    (index   0)
                    (n       (##interval-volume interval)))
                (let i-loop ((i lower-i)
                             (index (- n 1)))
                  (cond ((zero? index)
                         (f i))
                        (else
                         (,connector (f i)
                                     (i-loop (+ i 1)
                                             (- index 1))))))))
         ((2) (let ((lower-i (##interval-lower-bound interval 0))
                    (lower-j (##interval-lower-bound interval 1))
                    (upper-i (##interval-upper-bound interval 0))
                    (upper-j (##interval-upper-bound interval 1))
                    (n       (##interval-volume interval)))
                (let i-loop ((i lower-i)
                             (index (- n 1)))
                  ;; (< i upper-i) is always true because index is >= 0
                  (let j-loop ((j lower-j)
                               (index index))
                    (cond ((zero? index)
                           (f i j))
                          ((< j upper-j)
                           (,connector (f i j)
                                       (j-loop (+ j 1)
                                               (- index 1))))
                          (else
                           (i-loop (+ i 1)
                                   index)))))))
         ((3) (let ((lower-i (##interval-lower-bound interval 0))
                    (lower-j (##interval-lower-bound interval 1))
                    (lower-k (##interval-lower-bound interval 2))
                    (upper-i (##interval-upper-bound interval 0))
                    (upper-j (##interval-upper-bound interval 1))
                    (upper-k (##interval-upper-bound interval 2))
                    (n       (##interval-volume interval)))
                (let i-loop ((i lower-i)
                             (index (- n 1)))
                  ;; (< i upper-i) is always true because index is >= 0
                  (let j-loop ((j lower-j)
                               (index index))
                    (if (< j upper-j)
                        (let k-loop ((k lower-k)
                                     (index index))
                          (cond ((zero? index)
                                 (f i j k))
                                ((< k upper-k)
                                 (,connector (f i j k)
                                             (k-loop (+ k 1)
                                                     (- index 1))))
                                (else
                                 (j-loop (+ j 1)
                                         index))))
                        (i-loop (+ i 1)
                                index))))))
         ((4) (let ((lower-i (##interval-lower-bound interval 0))
                    (lower-j (##interval-lower-bound interval 1))
                    (lower-k (##interval-lower-bound interval 2))
                    (lower-l (##interval-lower-bound interval 3))
                    (upper-i (##interval-upper-bound interval 0))
                    (upper-j (##interval-upper-bound interval 1))
                    (upper-k (##interval-upper-bound interval 2))
                    (upper-l (##interval-upper-bound interval 3))
                    (n       (##interval-volume interval)))
                (let i-loop ((i lower-i)
                             (index (- n 1)))
                  (let j-loop ((j lower-j)
                               (index index))
                    (if (< j upper-j)
                        (let k-loop ((k lower-k)
                                     (index index))
                          (if (< k upper-k)
                              (let l-loop ((l lower-l)
                                           (index index))
                                (cond ((zero? index)
                                       (f i j k l))
                                      ((< l upper-l)
                                       (,connector (f i j k l)
                                                   (l-loop (+ l 1)
                                                           (- index 1))))
                                      (else
                                       (k-loop (+ k 1)
                                               index))))
                              (j-loop (+ j 1)
                                      index)))
                        (i-loop (+ i 1)
                                index))))))
         (else

          (let* ((lowers     (##interval-lower-bounds->vector interval))
                 (uppers     (##interval-upper-bounds->vector interval))
                 (dimensions (vector-length lowers))
                 (arg        (vector->list lowers))                    ;; the argument to which f is applied
                 (tails      (let ((result (make-vector dimensions)))  ;; the tails of the argument
                               (do ((i 0 (fx+ i 1))
                                    (arg arg (cdr arg)))
                                   ((fx= i dimensions) result)
                                 (vector-set! result i arg)))))
            (let loop ((dimension 0)
                       (total-index (- (interval-volume interval) 1)))
              (cond ((= (car (vector-ref tails dimension))
                        (vector-ref uppers dimension))
                     ;; We're done iterating in this dimension, set the arg index
                     ;; at this dimension back to the lower bound, increment the
                     ;; arg index at the previous dimension, and go back to the
                     ;; previous dimension
                     (let ((previous-tail (vector-ref tails (fx- dimension 1))))
                       (set-car! (vector-ref tails  dimension)
                                 (vector-ref lowers dimension))
                       (set-car! previous-tail
                                 (+ 1 (car previous-tail)))
                       (loop (fx- dimension 1)
                             total-index)))
                    ((fx< dimension (fx- dimensions 1))
                     (loop (fx+ dimension 1)
                           total-index))
                    ;; Now we're at the final dimension 
                    ((zero? total-index)
                     (apply f arg))
                    (else
                     (,connector (apply f arg)
                                 (let ((current-tail (vector-ref tails dimension)))
                                   (set-car! current-tail
                                             (+ (car current-tail) 1))
                                   (loop dimension
                                         (- total-index 1))))))))))))

  (let ((result
         `(begin
            ,@(map make-predicate
                   '(any every)
                   '(or and)))))
    result))

(macro-make-predicates)

(define (array-every f array #!rest arrays)
  (cond ((not (procedure? f))
	 (apply error "array-every: The first argument is not a procedure: " f array arrays))
	((not (##every array? (cons array arrays)))
	 (apply error "array-every: Not all arguments after the first are arrays: " f array arrays))
	((not (##every (lambda (d) (##interval= d (array-domain array))) (map array-domain arrays)))
	 (apply error "array-every: Not all arguments after the first have the same domain: " f array arrays))
	(else
         (##interval-every (##specialize-function-applied-to-array-getters f array arrays)
                           (array-domain array)))))

(define (array-any f array #!rest arrays)
  (cond ((not (procedure? f))
	 (apply error "array-any: The first argument is not a procedure: " f array arrays))
	((not (##every array? (cons array arrays)))
	 (apply error "array-any: Not all arguments after the first are arrays: " f array arrays))
	((not (##every (lambda (d) (##interval= d (array-domain array))) (map array-domain arrays)))
	 (apply error "array-any: Not all arguments after the first have the same domain: " f array arrays))
	(else
         (##interval-any (##specialize-function-applied-to-array-getters f array arrays)
                         (array-domain array)))))


(define (##array-fold op id a)
  (##interval-fold (array-getter a) op id (array-domain a)))

(define (array-fold op id a)
  (cond ((not (procedure? op))
	 (error "array-fold: The first argument is not a procedure: " op id a))
	((not (array? a))
	 (error "array-fold: The third argument is not an array: " op id a))
	(else
	 (##array-fold op id a))))

(define (array-fold-right op id a)
  (cond ((not (procedure? op))
	 (error "array-fold-right: The first argument is not a procedure: " op id a))
	((not (array? a))
	 (error "array-fold-right: The third argument is not an array: " op id a))
	(else
	 (##array-fold op id (array-reverse a (make-vector (array-dimension a) #t))))))

(define (array->list array)
  (cond ((not (array? array))
	 (error "array->list: object is not an array: " array))
 	(else
	 (array-fold-right cons '() array))))

(define (list->specialized-array l interval #!optional (result-storage-class generic-storage-class) (safe? (specialized-array-default-safe?)))
  (cond ((not (list? l))
	 (error "list->specialized-array: First argument is not a list: " l interval))
	((not (interval? interval))
	 (error "list->specialized-array: Second argument is not an interval: " l interval))
	((not (storage-class? result-storage-class))
	 (error "list->specialized-array: Third argument is not a storage-class: " l interval result-storage-class))
	((not (boolean? safe?))
	 (error "list->specialized-array: Fourth argument is not a boolean: " l interval result-storage-class safe?))
	(else
	 (let* ((checker
		 (storage-class-checker  result-storage-class))
		(setter
		 (storage-class-setter   result-storage-class))
		(result
		 (make-specialized-array interval
				         result-storage-class
				         safe?))
		(body
		 (array-body result))
		(n
		 (interval-volume interval)))
	   (let loop ((i 0)
		      (local l))
	     (if (or (= i n) (null? local))
		 (if (and (= i n) (null? local))
		     result
		     (error "list->specialized-array: The length of the first argument does not equal the volume of the second: " l interval))
		 (let ((item (car local)))
		   (if (checker item)
		       (begin
			 (setter body i item)
			 (loop (+ i 1)
			       (cdr local)))
		       (error "list->specialized-array: Not every element of the list can be stored in the body of the array: " l interval)))))))))


(declare (inline))
