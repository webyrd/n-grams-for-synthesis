(define *correct-count* 0)
(define *failed-count* 0)

(define-syntax check
  (syntax-rules (=>)
    ((check ec => desired-result)
     (check ec => (equal?) desired-result))
    ((check ec => (equal?) desired-result)
     (begin
       (newline)
       (write (quote ec))
       (newline)
       (let ((actual-result ec))
         (display "  => ")
         (write actual-result)
         (if (equal? actual-result desired-result)
             (begin
               (display " ; correct")
               (set! *correct-count* (+ *correct-count* 1)) )
             (begin
               (display " ; *** failed ***, desired result:")
               (newline)
               (display "  => ")
               (write desired-result)
               (set! *failed-count* (+ *failed-count* 1)) ))
         (newline) )))))

; procedural layer

(define :point
  (make-record-type-descriptor
   'point #f
   #f #f #f 
   '((mutable x) (mutable y))))

(define make-point
  (record-constructor (make-record-constructor-descriptor :point #f #f)))

(define point? (record-predicate :point))
(define point-x (record-accessor :point 0))
(define point-y (record-accessor :point 1))
(define point-x-set! (record-mutator :point 0))
(define point-y-set! (record-mutator :point 1))

(define p1 (make-point 1 2))
(check (point? p1) => #t)
(check (point-x p1) => 1)
(check (point-y p1) => 2)
(point-x-set! p1 5)
(check (point-x p1) => 5)

(define :point2
  (make-record-type-descriptor
   'point2 :point 
   #f #f #f '((mutable x) (mutable y))))

(define make-point2
  (record-constructor (make-record-constructor-descriptor :point2 #f #f)))
(define point2? (record-predicate :point2))
(define point2-xx (record-accessor :point2 0))
(define point2-yy (record-accessor :point2 1))

(define p2 (make-point2 1 2 3 4))
(check (point? p2) => #t)
(check (point-x p2) => 1)
(check (point-y p2) => 2)
(check (point2-xx p2) => 3)
(check (point2-yy p2) => 4)

; explicit naming

(define-record-type (point3 make-point3 point3?)
  (fields (immutable x point3-x)
          (mutable y point3-y set-point3-y!))
  (nongenerative point3-4893d957-e00b-11d9-817f-00111175eb9e))

(define-record-type (cpoint make-cpoint cpoint?)
  (parent point3)
  (protocol
   (lambda (p)
     (lambda (x y c) 
       ((p x y) (color->rgb c)))))
  (fields (mutable rgb cpoint-rgb cpoint-rgb-set!)))

(define (color->rgb c)
  (cons 'rgb c))

(define p3-1 (make-point3 1 2))
(define p3-2 (make-cpoint 3 4 'red))

(check (point3? p3-1) => #t)
(check (point3? p3-2) => #t)
(check (point3? (vector)) => #f)
(check (point3? (cons 'a 'b)) => #f)
(check (cpoint? p3-1) => #f)
(check (cpoint? p3-2) => #t)
(check (point3-x p3-1) => 1)
(check (point3-y p3-1) => 2)
(check (point3-x p3-2) => 3)
(check (point3-y p3-2) => 4)
(check (cpoint-rgb p3-2) => '(rgb . red))

(set-point3-y! p3-1 17)
(check (point3-y p3-1) => 17)

(check (record-rtd p3-1) => (record-type-descriptor point3))

(define-record-type (ex1 make-ex1 ex1?)
  (protocol (lambda (p) (lambda a (p a))))
  (fields
   (immutable f ex1-f)))

(define ex1-i1 (make-ex1 1 2 3))
(check (ex1-f ex1-i1) => '(1 2 3))

(define-record-type (ex2 make-ex2 ex2?)
  (protocol (lambda (p) (lambda (a . b) (p a b))))
  (fields
   (immutable a ex2-a)
   (immutable b ex2-b)))

(define ex2-i1 (make-ex2 1 2 3))
(check (ex2-a ex2-i1) => 1)
(check (ex2-b ex2-i1) => '(2 3))

; implicit naming

(define *ex3-instance* #f)

(define-record-type ex3
  (parent cpoint)
  (protocol
   (lambda (p)
     (lambda (x y t)
       (let ((r ((p x y 'red) t)))
	 (set! *ex3-instance* r)
	 r))))
  (fields 
   (mutable thickness))
  (sealed #t) (opaque #t))

(define ex3-i1 (make-ex3 1 2 17))
(check (ex3? ex3-i1) => #t)
(check (cpoint-rgb ex3-i1) => '(rgb . red))
(check (ex3-thickness ex3-i1) => 17)
(ex3-thickness-set! ex3-i1 18)
(check (ex3-thickness ex3-i1) => 18)
(check *ex3-instance* => (eq?) ex3-i1)

(check (record? ex3-i1) => #f)

; fancy constructor

(define-record-type (unit-vector make-unit-vector unit-vector?)
  (protocol
   (lambda (p)
     (lambda (x y z)
       (let ((length (+ (* x x) (* y y) (* z z))))
	 (p  (/ x length)
	     (/ y length)
	     (/ z length))))))
  (fields (immutable x unit-vector-x)
	  (immutable y unit-vector-y)
	  (immutable z unit-vector-z)))

(newline)
(display "correct tests: ")
(display *correct-count*)
(newline)
(display "failed tests: ")
(display *failed-count*)
(newline)
