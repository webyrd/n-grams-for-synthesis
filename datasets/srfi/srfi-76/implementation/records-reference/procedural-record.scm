; Implementation of procedural layer for Records SRFI

; Copyright (C) Michael Sperber (2005). All Rights Reserved. 
; 
; Permission is hereby granted, free of charge, to any person
; obtaining a copy of this software and associated documentation files
; (the "Software"), to deal in the Software without restriction,
; including without limitation the rights to use, copy, modify, merge,
; publish, distribute, sublicense, and/or sell copies of the Software,
; and to permit persons to whom the Software is furnished to do so,
; subject to the following conditions:
; 
; The above copyright notice and this permission notice shall be
; included in all copies or substantial portions of the Software.
; 
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

(define make-field-spec cons)

(define field-spec-mutable? car)
(define field-spec-name cdr)

(define (field-spec=? spec-1 spec-2)
  (and (eq? (field-spec-mutable? spec-1)
	    (field-spec-mutable? spec-2))
       (eq? (field-spec-name spec-1)
	    (field-spec-name spec-2))))

(define :record-type-data (make-vector-type 'vector-type-descriptor '() #f))

(define (really-make-record-type-data uid sealed? opaque? field-specs immutable?)
  (typed-vector :record-type-data
		uid sealed? opaque? field-specs immutable?))

(define (make-record-type-data uid sealed? opaque? field-specs)
  (really-make-record-type-data 
   uid sealed? opaque? field-specs
   (and (not (any field-spec-mutable? field-specs)) #t)))

(define (record-type-data? thing)
  (has-vector-type? :record-type-data thing))

; this is #f in the generative case
(define (record-type-uid rtd)
  (typed-vector-ref :record-type-data (vector-type-data rtd) 0))
(define (record-type-sealed? rtd)
  (typed-vector-ref :record-type-data (vector-type-data rtd) 1))
(define (record-type-opaque? rtd)
  (typed-vector-ref :record-type-data (vector-type-data rtd) 2))
(define (record-type-field-specs rtd)
  (typed-vector-ref :record-type-data (vector-type-data rtd) 3))
(define (record-type-immutable? rtd)
  (typed-vector-ref :record-type-data (vector-type-data rtd) 4))

(define (record-type-descriptor=? rtd-1 rtd-2)
  (and (eq? (record-type-parent rtd-1) (record-type-parent rtd-2))
       (eq? (record-type-uid rtd-1) (record-type-uid rtd-2))
       (every field-spec=?
	      (record-type-field-specs rtd-1)
	      (record-type-field-specs rtd-2))))

(define (uid->record-type-descriptor uid)
  (find (lambda (rtd)
	  (eq? (record-type-uid rtd) uid))
	*nongenerative-record-types*))

(define (record-type-generative? rtd)
  (not (record-type-uid rtd)))

(define *nongenerative-record-types* '())

(define (make-record-type-descriptor name parent uid sealed? opaque? field-specs)
  (if (and parent
	   (record-type-sealed? parent))
      (error "can't extend a sealed parent class" parent))
  (if (and parent
	   (not (record-type-uid parent)) ; parent generative
	   uid)			  ; ... but this one is non-generative
      (error "a generative type can only be extended to give a generative type" parent))
  (let ((opaque? (if parent
		     (or (record-type-opaque? parent)
			 opaque?)
		     opaque?))
	(field-specs (map parse-field-spec field-specs)))
    (let ((rtd 
	   (make-vector-type name
			     (if parent (list parent) '())
			     (make-record-type-data uid sealed? opaque? field-specs))))
      (if uid
	  (cond
	   ((uid->record-type-descriptor uid)
	    => (lambda (old-rtd)
		 (if (record-type-descriptor=? rtd old-rtd)
		     old-rtd
		     (error "mismatched nongenerative record types with identical uids"
			    old-rtd rtd))))
	   (else
	    (set! *nongenerative-record-types* 
		  (cons rtd *nongenerative-record-types*))
	    rtd))
	  rtd))))

(define (record-type-descriptor? thing)
  (and (vector-type? thing)
       (record-type-data? (vector-type-data thing))))

(define (ensure-rtd thing)
  (if (not (record-type-descriptor? thing))
      (error "not a record-type descriptor" thing)))

(define (record-type-name rtd)
  (ensure-rtd rtd)
  (vector-type-name rtd))

(define (record-type-parent rtd)
  (ensure-rtd rtd)
  (let ((supertypes (vector-type-supertypes rtd)))
    (if (null? supertypes)
	#f
	(car supertypes))))

(define (parse-field-spec spec)
  (apply (lambda (mutability name)
	   (make-field-spec
	    (case mutability
	      ((mutable) #t)
	      ((immutable) #f)
	      (else (error "field spec with invalid mutability specification" spec)))
	    name))
	 spec))

(define (record-type-field-names rtd)
  (map field-spec-name (record-type-field-specs rtd)))

(define (field-count rtd)
  (let loop ((rtd rtd)
	     (count 0))
    (if (not rtd)
	count
	(loop (record-type-parent rtd)
	      (+ count (length (record-type-field-specs rtd)))))))
	 
(define (record? thing)
  (and (typed-vector? thing)
       (record-type-descriptor? (typed-vector-type thing))))

(define (make-record rtd size)
  (make-typed-vector rtd size))

(define (record rtd . components)
  (apply typed-vector rtd components))

(define (record-rtd rec)
  (typed-vector-type rec))

(define (record-ref rtd rec index)
  (typed-vector-ref rtd rec index))

(define (record-set! rtd rec index val)
  (typed-vector-set! rtd rec index val))

(define (make-record-immutable! r)
  (make-typed-vector-immutable! r))

; Constructing constructors

(define :record-constructor-descriptor (make-vector-type 'record-constructor-descriptor '() #f))

(define (make-record-constructor-descriptor rtd previous protocol)
  (let ((parent (record-type-parent rtd)))
    (if (and previous (not parent))
	(error "mismatch between rtd and constructor descriptor" rtd previous))

    (if (and previous
	     (not protocol)
	     (record-constructor-descriptor-custom-protocol? previous))
	(error "default protocol requested when parent constructor descriptor has custom one"
	       protocol previous)) 
  
    (let ((custom-protocol? (and protocol #t))
	  (protocol (or protocol (default-protocol rtd)))
	  (previous
	   (if (or previous
		   (not parent))
	       previous
	       (make-record-constructor-descriptor parent #f #f))))
 
      (typed-vector :record-constructor-descriptor
		    rtd protocol custom-protocol? previous))))

(define (default-protocol rtd)
  (let ((parent (record-type-parent rtd)))
    (if (not parent)
	(lambda (p)
	  (lambda field-values
	    (apply p field-values)))
	(let ((parent-field-count (field-count parent)))
	  (lambda (p)
	    (lambda all-field-values
	      (call-with-values
		  (lambda () (split-at all-field-values parent-field-count))
		(lambda (parent-field-values this-field-values)
		  (apply (apply p parent-field-values) this-field-values)))))))))

(define (record-constructor-descriptor-rtd desc)
  (typed-vector-ref :record-constructor-descriptor desc 0))
(define (record-constructor-descriptor-protocol desc)
  (typed-vector-ref :record-constructor-descriptor desc 1))
; this field is for error checking
(define (record-constructor-descriptor-custom-protocol? desc)
  (typed-vector-ref :record-constructor-descriptor desc 2))
(define (record-constructor-descriptor-previous desc)
  (typed-vector-ref :record-constructor-descriptor desc 3))

; A "seeder" is the procedure passed to the cons conser, used to seed
; the initial field values.

(define (make-make-seeder real-rtd wrap for-desc)
  (let recur ((for-desc for-desc))
    (let* ((for-rtd (record-constructor-descriptor-rtd for-desc))
	   (for-rtd-field-count (length (record-type-field-specs for-rtd))))
      (cond
       ((record-constructor-descriptor-previous for-desc)
	=> (lambda (parent-desc)
	     (let ((parent-protocol (record-constructor-descriptor-protocol parent-desc))
		   (parent-make-seeder (recur parent-desc)))
	       (lambda extension-field-values
		 (lambda parent-protocol-args
		   (lambda for-rtd-field-values
		     (if (not (= (length for-rtd-field-values) for-rtd-field-count))
			 (error "wrong number of arguments to record constructor"
				for-rtd for-rtd-field-values))
		     (apply (parent-protocol
			     (apply parent-make-seeder
				    (append for-rtd-field-values extension-field-values)))
			    parent-protocol-args)))))))
       (else
	(lambda extension-field-values
	  (lambda for-rtd-field-values
	    (if (not (= (length for-rtd-field-values) for-rtd-field-count))
		(error "wrong number of arguments to record constructor"
		       for-rtd for-rtd-field-values))
	    (wrap
	     (apply record real-rtd
		    (append for-rtd-field-values extension-field-values))))))))))

; does RTD-1 represent an ancestor of RTD-2?

; This suggests the corresponding procedure in VECTOR-TYPES should be
; abstracted out.

(define (rtd-ancestor? rtd-1 rtd-2)
  (let loop ((rtd-2 rtd-2))
    (or (eq? rtd-1 rtd-2)
	(and rtd-2
	     (loop (record-type-parent rtd-2))))))

(define (record-constructor desc)
  (let* ((rtd (record-constructor-descriptor-rtd desc))
	 (process
	  (if (record-type-immutable? rtd)
	      (lambda (r)
		(make-record-immutable! r)
		r)
	      (lambda (r) r)))
	 (wrap (if (record-type-opaque? rtd)
		   (lambda (r)
		     (make-opaque-cell (lambda (access-key)
					 (and (record-type-descriptor? access-key)
					      (rtd-ancestor? access-key rtd)))
				       (process r)))
		   process)))
    ((record-constructor-descriptor-protocol desc)
     ((make-make-seeder rtd wrap desc)))))

(define (record-with-rtd? obj rtd)
  (has-vector-type? rtd obj))

(define (record-predicate rtd)
  (lambda (thing)
    (let ((thing (if (opaque-cell? thing)
		     (opaque-cell-ref rtd thing)
		     thing)))
      (record-with-rtd? thing rtd))))

(define (record-accessor rtd field-id)
  (let ((index (field-id-index rtd field-id)))
    (lambda (thing)
      (let ((thing (if (opaque-cell? thing)
		       (opaque-cell-ref rtd thing)
		       thing)))
	(record-ref rtd thing index)))))

(define (record-mutator rtd field-id)
  (if (not (record-field-mutable? rtd field-id))
      (error "record-mutator called on immutable field" rtd field-id))
  (let ((index (field-id-index rtd field-id)))
    (lambda (thing val)
      (let ((thing (if (opaque-cell? thing)
		     (opaque-cell-ref rtd thing)
		     thing)))
	(record-set! rtd thing index val)))))

; A FIELD-ID is an index, which refers to a field in RTD itself.
(define (field-id-index rtd field-id)
  (+ (field-count (record-type-parent rtd))
     field-id))

(define (record-field-mutable? rtd field-id)
  (field-spec-mutable? (list-ref (record-type-field-specs rtd) field-id)))
