(define (tag x) `(#(,x)))

(define (check-arg pred? val caller)
  (if (pred? val)
      #t
      (check-arg pred?
                 (error "Bad argument"
                        val
                        pred?
                        caller)
                 caller)))

(define-syntax let-optionals*
  (syntax-rules ()
    ((_ ?rest () ?e1 ?e2 ...)
     (begin ?e1 ?e2 ...))
    ((_ ?rest ((?var ?default) . ?more) ?e1 ?e2 ...)
     (let* ((rest ?rest)
            (var (if (null? rest) ?default (car rest)))
            (next-rest (if (null? rest) '() (cdr rest))))
       (let-optionals* ?rest ((?var ?default) . ?more) ?e1 ?e2 ...)))))

;;;;;; - Iterator Operations -

(define-predicate iterator?)

(define-predicate iterator-at-start?)
(define-predicate iterator-at-end?)

(define-operation (iterator-value iterator))
(define-operation (iterator-next iterator))
(define-operation (iterator-size iterator))
(define-operation (iterator-set-value! iterator))
(define-operation (iterator-remove-value! iterator))

(define-operation (collection? x)
  (or (bag? x)
      (set? x)
      (dictionary? x)
      (ordered-collection? x)))

(define-predicate ordered-collection?)
(define-predicate bag?)
(define-predicate set?)
(define-operation (sequence? x)
  (flexible-sequence? x))
(define-predicate flexible-sequence?)
(define-predicate dict?)

;;;;;; - Scheme Collection API -

;;; - Lists -

(define (make-list . maybe-size+fill)
  ;; (if #f #f) yields the 'unspecific' value.
  (let-optionals* maybe-size+fill ((size 0) (fill (if #f #f)))
    (check-arg (lambda (x)
                 (and (integer? x)
                      (not (negative? x))))
               size 'make-list)
    (let loop ((result '()) (i 0))
      (if (= i size)
          result
          (loop (cons fill result) (+ i 1))))))

;; LIST is predefined.
;; (define (list . elements) elements)

;; LIST? is predefined.
;; (define (list? x)
;;   (or (null? x) (and (pair? x) (list? (cdr x)))))

(define (list-iterator l)
  (check-arg pair? l 'list-iterator)
  (let ((start-tag (tag "at start")))
    (let next-iter ((l (cons start-tag l)) (prev-iter #f))
      (object unspecific
        ((iterator? self) #t)
        ((iterator-at-start? self)
         (and (pair? l) (eq? (car l) start-tag)))
        ((iterator-at-end? self)
         ;; Should dotted lists work here?  If so, then this is an ok
         ;; definition; otherwise, it should do something like this:
         ;; (cond
         ;;  ((null? self) #t)
         ;;  ((pair? self) #f)
         ;;  (else <error message>))
         (not (pair? self)))
        ((iterator-value self)
         (cond
          ((iterator-at-start? self)
           (error "iterator-value (list): at start; can't get value"
                  self))
          ((iterator-at-end? self)
           (error "iterator-value (list): at end; can't get value"
                  self))
          (else (car l))))
        ((iterator-next self)
         (if (iterator-at-end? self)
             (error "iterator-next (list): at end; can't advance"
                    self)
             (next-iterator (cdr l) self)))
        ((iterator-previous self)
         (if (iterator-at-start? self)
             (error "iterator-previous (list): at start; can't go back"
                    self)
             prev-iter))
        ((iterator-size self)
         ;; This should possibly be made more efficient.
         ;; -1 if we're at the start, so we get rid of the START-TAG.
         (+ (if (iterator-at-start? self) -1 0) (length l)))
        ((iterator-set-value! self val)
         (if (iterator-at-start? self)
             (error
              "iterator-set-value! (list): at start; can't set value"
              self val)
             (set-car! l val)))
        ((iterator-remove-value! self)
         (cond
          ((iterator-at-start? self)
           (error (string-append "iterator-remove-value! (list):"
                                 " at start, can't remove value")
                  self))
          ((iterator-at-end? self)
           (error (string-append "iterator-remove-value! (list):"
                                 " at end; can't remove value")
                  self))
          (else (set-car! l (cadr l))
                (set-cdr! l (cddr l)))))
        ((iterator-insert-value! self val)
         (cond
          ((iterator-at-start? self)
           (error "iterator-insert-value!: at start; can't insert"
                  self val))
          ((iterator-at-end? self)
           
           (if #f #f))
          (else
           ;; Grab the CAR and the CDR of the current cell now, and
           ;; copy it so we can safely mutate it later.
           (let ((new-tail (cons (car l) (cdr l))))
             (set-car! l val)
             (set-cdr! l new-tail)))))))))

(define (list-contains? l val)
  (check-arg (lambda (x) (or (pair? x) (null? x))) l 'list-contains?)
  (and (memq val l) #t))

(define (list-add! l val)
  (check-arg pair? l 'list-add!)
  (let ((new-tail (cons (car l) (cdr l))))
    (set-car! l val)
    (set-cdr! l new-tail)))

(define (list-remove! l val)
  (check-arg pair? l 'list-remove!)
  (let ((cell (memq val l)))
    (if cell
        (begin
          (set-car! cell (cadr cell))
          (set-cdr! cell (cddr cell))))))

(define (list-remove-any! l val)
  (check-arg pair? l 'list-remove-any!)
  (let loop ((l (memq val l)))
    (if (not (null? l))
        (begin
          (set-car! l (cadr l))
          (set-cdr! l (cddr l))
          (loop (memq val l))))))

(define (list-add-all! l bag)
  (check-arg pair? l 'list-add-all!)
  (check-arg bag? bag 'list-add-all!)
  (if (not (bag-empty? bag))
      (let loop ((i (iterator-next (bag-iterator bag))))
        (cond
         ((iterator-at-end? i) (if #f #f))
         (else (list-add! l (iterator-value i))
               (loop (iterator-next i)))))))

(define (list-remove-all! l bag)
  (check-arg pair? l 'list-remove-all!)
  (check-arg bag? bag 'list-remove-all!)
  (if (not (bag-empty? bag))
      (let loop ((i (iterator-next (bag-iterator bag))))
        (if (iterator-at-end? i)
            (if #f #f)
            (begin
              (list-remove! l (iterator-value i))
              (loop (iterator-next i)))))))

(define (list-remove-any-of! l bag)
  (check-arg pair? l 'list-remove-any-of!)
  (check-arg bag? bag 'list-remove-any-of!)
  (if (not (bag-empty? bag))
      (let loop ((l l))
        (cond
         ((null? l) (if #f #f))
         ((null? (cddr l))
         ((bag-contains? bag (car l))
          (if (null? (cdr l))
              (set-car! l (lambda () "bogosity"))
              (begin
                (set-car! l (cadr l))
                (set-cdr! l (cddr l))
                (loop (cdr l)))))
         (else (loop (cdr l)))))))

;;; - Alists -

(define *alist-tag* (tag "alist"))

(define (make-alist key=?)
  (check-arg procedure? key=? 'make-alist)
  (list *alist-tag* key=?))

(define (alist . rest)
  (let ((key=? (cond ((null? rest) eqv?)
                     ((procedure? (car rest)) (car rest))
                     (else eqv?))))
    (check-arg procedure? key=? 'alist)
    (for-each (lambda (key+value)
                (check-arg pair? key+value 'alist))
              key+value-pairs)
    (cons *alist-tag* (cons key=? key+value-pairs))))

(define (alist-first alist)
  (check-arg (lambda (x)
               (and (quick-alist? x)
                    (not (alist-empty? x))))
             alist 'alist-first)
  (cadar alist))

(define (alist-first-key alist)
  (car (alist-first alist)))
(define (alist-first-value alist)
  (cdr (alist-first alist)))

(define (alist-rest alist)
  (check-arg (lambda (x)
               (and (quick-alist? x)
                    (not (alist-empty? x))))
             alist 'alist-rest)
  (cons *alist-tag*
        (cons (cadr alist) (cdddr alist))))

(define (quick-alist? x)
  (and (pair? x)
       (eq? (car x) *alist-tag*)
       (pair? (cdr x))
       (procedure? (cadr x))
       (or (pair? (cddr x))
           (null? (cddr x)))))

(define (alist? x)
  (and (quick-alist? x)
       (let loop ((l (cddr x)))
         (cond
          ((null? l) #t)
          ((and (pair? l)
                (pair? (car l)))
           (loop (cdr l)))
          (else #f)))))

(define (make-alist-iterator-maker get put!)
  (lambda (alist)
    (check-arg quick-alist? alist 'alist-iterator)
    (let ((start-tag (tag "start tag")))
      (let next-iter ((l (cons start-tag (cddr alist))) (prev-iter #f))
        (object unspecific
          ((iterator? self) #t)
          ((iterator-at-start? self)
           (and (pair? l) (eq? (car l) start-tag)))
          ((iterator-at-end? self) (null? l))
          ((iterator-value self)
           (cond
            ((iterator-at-start? self)
             (error "iterator-value (alist): at start; can't get value"
                    self))
            ((iterator-at-end? self)
             (error "iterator-value (alist): at end; can't get value"))
            (else (get (car l)))))
          ((iterator-next self)
           (next-iter (cdr l) self))
          ((iterator-size self)
           (+ (if (iterator-at-start? self) -1 0) (length l)))
          ((iterator-previous self) prev-iter)
          ((iterator-set-value! self val)
           (cond
            ((iterator-at-start? self)
             (error
              "iterator-set-value! (alist): at start; can't set value"
              self val))
            ((iterator-at-end? self)
             (error
              "iterator-set-value! (alist): at end; can't set value"
              self val))
            (else (put! (alist-first alist) val))))
          ((iterator-remove-value! self)
           (cond
            ((iterator-at-start? self)
             (error (string-append "iterator-remove-value! (alist):"
                                   " at start; can't remove value")
                    self))
            ((iterator-at-end? self)
             (error (string-append "iterator-remove-value! (alist):"
                                   " at end; can't remove value")
                    self))
            (else (set-cdr! (cdr alist) (cdddr alist))))))))))

(define alist-key-iterator
  (make-alist-iterator-maker car set-car!))
(define alist-iterator
  (make-alist-iterator-maker cdr set-cdr!))

(define (alist-keys alist)
  (check-arg quick-alist? alist 'alist-keys)
  (map car (cddr alist)))
(define (alist-values alist)
  (check-arg quick-alist? alist 'alist-values)
  (map cdr (cddr alist)))

(define (alist-cell-lookup alist key caller)
  (check-arg quick-alist? alist caller)
  (let ((elt=? (cadr alist)))
    (let loop ((l (cddr alist)))
      (cond
       ((null? l) #f)
       ;; Type check as we go along, not in two iterations.
       ((not (pair? l))
        (error (string-append (symbol->string caller)
                              ": not an alist")
               alist l))
       (else
        (let ((cell (car l)))
          (if (elt=? (car cell) key)
              cell
              (loop (cdr l)))))))))

(define (alist-get alist key . maybe-fk)
  (let-optionals* maybe-fk ((fk (lambda () #f)))
    (check-arg procedure? fk 'alist-get)
    (cond
     ((alist-cell-lookup alist key 'alist-get) => cdr)
     (else (fk)))))

(define (alist-put! alist key val . maybe-retval)
  (let-optionals* maybe-retval ((retval #f))
    (cond
     ((alist-cell-lookup alist key 'alist-put!) =>
      (lambda (cell)
        (let ((old-val (cdr cell)))
          (set-cdr! cell val)
          old-val)))
     (else retval))))

(define (make-alist-remove! name sk)
  (lambda (alist key)
    (check-arg quick-alist? alist ame)
    (let ((elt=? (cadr alist)))
      (let loop ((l (cddr alist)))
        (cond
         ((null? l) (if #f #f))
         ((not (pair? l))
          (error (string-append (symbol->string name)
                                ": not an alist")
                 alist l))
         ((elt=? (caar l) key)
          (if (null? (cdr l))
              ;; Make sure you can never get this association without
              ;; much hackage.
              (set-car! l (cons (lambda () "bogosity")
                                (lambda () "bogosity")))
              (begin
                (set-car! l (cadr l))
                (set-cdr! l (cddr l))
                (sk loop l))))
         (else (loop (cdr l))))))))

(define alist-remove!
  (make-alist-remove! 'alist-remove! (lambda (loop l) (if #f #f))))
(define alist-remove-any!
  (make-alist-remove! 'alist-remove-any! (lambda (loop l) (loop l))))

(define (make-alist-bulk-remove! name sk)
  (lambda (alist bag)
    (check-arg quick-alist? alist name)
    (check-arg bag? bag name)
    (let ((elt=? (cadr alist)))
      (let loop ((l (cddr alist)))
        (cond
         ((null? l) (if #f #f))
         ((not (pair? l))
          (error (string-append (symbol->string name)
                                ": not an alist")
                 alist l))
         ((bag-contains? (caar alist) bag)
          (if (null? (cdr l))
              (set-car! l (cons (lambda () "bogosity")
                                (lambda () "bogosity")))
              (begin
                (set-car! l (cadr l))
                (set-cdr! l (cddr l))
                (sk loop l))))
         (else (loop (cdr l))))))))

(define (alist-remove-all! alist bag)
  (check-arg quick-alist? alist 'alist-remove-all!)
  (check-arg bag? bag 'alist-remove-all!)
  (if (not (bag-empty? bag))
      (let loop ((i (iterator-next (bag-iterator bag))))
        (if (not (iterator-at-end? bag))
            (begin
              (alist-remove! alist (iterator-value i))
              (loop (iterator-next i)))))))

(define (alist-remove-any-of! alist bag)
  (check-arg quick-alist? alist 'alist-remove-any-of!)
  (check-arg bag? bag 'alist-remove-any-of!)
  (cond
   ((null? alist) (if #f #f))
   ((null? (cdr alist))
    (if (bag-contains? bag (caar alist))
        (begin
          (set-car! (car alist) (lambda () "bogosity"))
          (set-cdr! (car alist) (lambda () "bogosity")))))
   (else
    (let loop ((alist alist))
      (cond
       ((null? (cddr alist))
        (cond
         ((bag-contains? bag (caar alist))
          (set-car! alist (cadr alist))
          (set-cdr! alist (cddr alist)))
         ((bag-contains? bag (caadr alist))
          (set-cdr! alist '()))))
       ((bag-contains? bag (caar alist))

(define (dict-for-each proc dict)
  (if (not (dict-empty? dict))
      (let loop ((key-i (iterator-next (dict-key-iterator dict)))
                 (val-i (iterator-next (dict-iterator dict))))
        (if (not (iterator-at-end? key-i))
            (begin
              (proc (iterator-value key-i)
                    (iterator-value val-i))
              (loop (iterator-next key-i)
                    (iterator-next val-i)))))))

(define (alist-add-all! alist dict)
  (check-arg quick-alist? alist 'alist-add-all!)
  (check-arg dict? dict 'alist-add-all!)
  (dict-for-each (lambda (key val) (alist-put! alist key val))
                 dict)))

(define (make-vector/string-iterator-maker name x-ref x-set! x-len x?
                                           at-start? at-end? start
                                           next prev)
  (lambda (vec/str)
    (check-arg x? vec/str (string->symbol
                           (string-append (symbol->string name)
                                          "-iterator")))
    (let ((len (x-len vec/str))
          (err (lambda (iter-op msg . objs)
                 (apply error
                        (string-append iter-op
                                       " "
                                       (symbol->string name)
                                       ": "
                                       msg)
                        objs))))
      (let next-iter ((i start))
        (object unspecific
          ((iterator? self) #t)
          ((iterator-at-start? self)
           (at-start? i len))
          ((iterator-at-end? self)
           (at-end? i len))
          ((iterator-value self)
           (cond
            ((iterator-at-start? self)
             (err "iterator-value"
                  "at start; can't get value"
                  self))
            ((iterator-at-end? self)
             (err "iterator-value"
                  "at end; can't get value"))
            (else (x-ref vec/str i))))
          ((iterator-next self)
           (next err i len next-iter))
          ((iterator-size self)
           (if (negative? i)
               len
               (- len i)))
          ((iterator-previous self)
           (prev err i len next-iter))
          ((iterator-set-value! self val)
           (cond
            ((iterator-at-start? self)
             (err "iterator-set-value!"
                  "at start; can't set value"
                  self val))
            ((iterator-at-end? self)
             (err "iterator-set-value!"
                  "at end; can't set value"
                  self val))
            (else (x-set! vec/str i val)))))))))

;;; - Vectors -

;; MAKE-VECTOR is predefined.
;; VECTOR is predefined.
;; VECTOR? is predefined.

(define vector-iterator
  (make-vector/string-iterator-maker 'vector
                                     vector-ref vector-set! vector?
                                     (lambda (i len) (negative? i))
                                     (lambda (i len) (>= i len))
                                     (lambda (err i len next-iter)
                                       (if (>= i len)
                                           (err
                                            "iterator-next"
                                            "at end; can't advance")
                                           (next-iter (+ i 1))))
                                     (lambda (err i len next-iter)
                                       (if (negative? i)
                                           (err
                                            "iterator-previous"
                                            "at start; can't retreat")
                                           (next-iter (- i 1))))))

(define vector-reverse-iterator
  (make-vector/string-iterator-maker 'vector-reverse
                                     vector-ref vector-set! vector?
                                     (lambda (i len) (>= i len))
                                     (lambda (i len) (negative? i))
                                     (lambda (err i len next-iter)
                                       (if (negative? i)
                                           (err
                                            "iterator-next"
                                            "at end; can't advance")
                                           (next-iter (- i 1))))
                                     (lambda (err i len next-iter)
                                       (if (>= i len)
                                           (err
                                            "iterator-previous"
                                            "at start; can't retreat")
                                           (next-iter (+ i 1))))))

(define (vector-contains? vec val)
  (let loop ((i 0))
    (cond
     ((>= i (vector-length vec)) #f)
     ((eqv? val (vector-ref vec i)) #t)
     (else (loop (+ i 1))))))

;;; - Strings -

;; MAKE-STRING is predefined.
;; STRING is predefined.
;; STRING? is predefined.

(define string-iterator
  (make-vector/string-iterator-maker 'string
                                     string-ref string-set! string?
                                     (lambda (i len) (negative? i))
                                     (lambda (i len) (>= i len))
                                     (lambda (err i len next-iter)
                                       (if (>= i len)
                                           (err
                                            "iterator-next"
                                            "at end; can't advance")
                                           (next-iter (+ i 1))))
                                     (lambda (err i len next-iter)
                                       (if (negative? i)
                                           (err
                                            "iterator-previous"
                                            "at start; can't retreat")
                                           (next-iter (- i 1))))))

(define string-reverse-iterator
  (make-vector/string-iterator-maker 'string-reverse
                                     string-ref string-set! string?
                                     (lambda (i len) (>= i len))
                                     (lambda (i len) (negative? i))
                                     (lambda (err i len next-iter)
                                       (if (negative? i)
                                           (err
                                            "iterator-next"
                                            "at end; can't advance")
                                           (next-iter (- i 1))))
                                     (lambda (err i len next-iter)
                                       (if (>= i len)
                                           (err
                                            "iterator-previous"
                                            "at start; can't retreat")
                                           (next-iter (+ i 1))))))

(define (string-contains? str char)
  (let loop ((i 0))
    (cond
     ((>= i (string-length str)) #f)
     ((char=? (string-ref str 0) char) #t)
     (else (loop (+ i 1))))))
