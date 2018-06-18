;;; Guile implementation.

(define-module (srfi srfi-126))

(use-modules
 (srfi srfi-1)
 (srfi srfi-9)
 (srfi srfi-9 gnu)
 (srfi srfi-11)
 (ice-9 hash-table)
 (ice-9 control)
 ((rnrs hashtables) #:select
  (equal-hash string-hash string-ci-hash symbol-hash)))

(export
 make-eq-hashtable make-eqv-hashtable make-hashtable
 alist->eq-hashtable alist->eqv-hashtable alist->hashtable
 weakness
 hashtable? hashtable-size
 hashtable-ref hashtable-set! hashtable-delete! hashtable-contains?
 hashtable-lookup hashtable-update! hashtable-intern!
 hashtable-copy hashtable-clear! hashtable-empty-copy
 hashtable-keys hashtable-values hashtable-entries
 hashtable-key-list hashtable-value-list hashtable-entry-lists
 hashtable-walk hashtable-update-all! hashtable-prune! hashtable-merge!
 hashtable-sum hashtable-map->lset hashtable-find
 hashtable-empty? hashtable-pop! hashtable-inc! hashtable-dec!
 hashtable-equivalence-function hashtable-hash-function
 hashtable-weakness hashtable-mutable?
 hash-salt
 )

(re-export equal-hash string-hash string-ci-hash symbol-hash)

(define-record-type <hashtable>
  (%make-hashtable %table %hash %assoc hash equiv weakness mutable)
  hashtable?
  (%table %hashtable-table)
  (%hash %hashtable-hash)
  (%assoc %hashtable-assoc)
  (hash hashtable-hash-function)
  (equiv hashtable-equivalence-function)
  (weakness hashtable-weakness)
  (mutable hashtable-mutable? %hashtable-set-mutable!))

(define nil (cons #f #f))
(define (nil? obj) (eq? obj nil))

(define (make-table capacity weakness)
  (let ((capacity (or capacity 32)))
    (case weakness
      ((#f) (make-hash-table capacity))
      ((weak-key) (make-weak-key-hash-table capacity))
      ((weak-value) (make-weak-value-hash-table capacity))
      ((weak-key-and-value) (make-doubly-weak-hash-table capacity))
      (else (error "Hashtable weakness not supported." weakness)))))

(define* (make-eq-hashtable #:optional capacity weakness)
  (let ((table (make-table capacity weakness)))
    (%make-hashtable table hashq assq #f eq? weakness #t)))

(define* (make-eqv-hashtable #:optional capacity weakness)
  (let ((table (make-table capacity weakness)))
    (%make-hashtable table hashv assv #f eqv? weakness #t)))

(define* (make-hashtable hash equiv #:optional capacity weakness)
  (cond
   ((and (not hash) (eq? equiv eq?))
    (make-eq-hashtable capacity weakness))
   ((and (not hash) (eq? equiv eqv?))
    (make-eqv-hashtable capacity weakness))
   (else
    (let* ((table (make-table capacity weakness))
           (hash (if (pair? hash)
                     (car hash)
                     hash))
           (%hash (lambda (obj bound)
                           (modulo (hash obj) bound)))
           (assoc (lambda (key alist)
                    (find (lambda (entry)
                            (equiv (car entry) key))
                          alist))))
      (%make-hashtable table %hash assoc hash equiv weakness #t)))))

(define (alist->eq-hashtable . args)
  (apply alist->hashtable #f eq? args))

(define (alist->eqv-hashtable . args)
  (apply alist->hashtable #f eqv? args))

(define alist->hashtable
  (case-lambda
    ((hash equiv alist)
     (alist->hashtable hash equiv #f #f alist))
    ((hash equiv capacity alist)
     (alist->hashtable hash equiv capacity #f alist))
    ((hash equiv capacity weakness alist)
     (let ((ht (make-hashtable hash equiv capacity weakness)))
       (for-each (lambda (entry)
                   (hashtable-set! ht (car entry) (cdr entry)))
                 (reverse alist))
       ht))))

(define-syntax weakness
  (lambda (stx)
    (syntax-case stx ()
      ((_ <sym>)
       (let ((sym (syntax->datum #'<sym>)))
         (case sym
           ((weak-key weak-value weak-key-and-value ephemeral-key
                      ephemeral-value ephemeral-key-and-value)
            #''sym)
           (else
            (error "Bad weakness symbol." sym))))))))

(define (hashtable-size ht)
  (hash-count (const #t) (%hashtable-table ht)))

(define* (%hashtable-ref ht key default)
  (hashx-ref (%hashtable-hash ht) (%hashtable-assoc ht)
             (%hashtable-table ht) key default))

(define* (hashtable-ref ht key #:optional (default nil))
  (let ((val (%hashtable-ref ht key default)))
    (if (nil? val)
        (error "No association for key in hashtable." key ht)
        val)))

(define (assert-mutable ht)
  (when (not (hashtable-mutable? ht))
    (error "Hashtable is immutable." ht)))

(define (hashtable-set! ht key value)
  (assert-mutable ht)
  (hashx-set! (%hashtable-hash ht) (%hashtable-assoc ht)
              (%hashtable-table ht) key value)
  *unspecified*)

(define (hashtable-delete! ht key)
  (assert-mutable ht)
  (hashx-remove! (%hashtable-hash ht) (%hashtable-assoc ht)
                 (%hashtable-table ht) key)
  *unspecified*)

(define (hashtable-contains? ht key)
  (not (nil? (%hashtable-ref ht key nil))))

(define (hashtable-lookup ht key)
  (let ((val (%hashtable-ref ht key nil)))
    (if (nil? val)
        (values #f #f)
        (values val #t))))

(define* (hashtable-update! ht key updater #:optional (default nil))
  (assert-mutable ht)
  (let ((handle (hashx-create-handle!
                 (%hashtable-hash ht) (%hashtable-assoc ht)
                 (%hashtable-table ht) key nil)))
    (if (eq? nil (cdr handle))
        (if (nil? default)
            (error "No association for key in hashtable." key ht)
            (set-cdr! handle (updater default)))
        (set-cdr! handle (updater (cdr handle))))
    (cdr handle)))

(define (hashtable-intern! ht key default-proc)
  (assert-mutable ht)
  (let ((handle (hashx-create-handle!
                 (%hashtable-hash ht) (%hashtable-assoc ht)
                 (%hashtable-table ht) key nil)))
    (when (nil? (cdr handle))
      (set-cdr! handle (default-proc)))
    (cdr handle)))

(define* (hashtable-copy ht #:optional mutable weakness)
  (let ((copy (hashtable-empty-copy ht (hashtable-size ht) weakness)))
    (hashtable-walk ht
      (lambda (k v)
        (hashtable-set! copy k v)))
    (%hashtable-set-mutable! copy mutable)
    copy))

(define* (hashtable-clear! ht #:optional _capacity)
  (assert-mutable ht)
  (hash-clear! (%hashtable-table ht))
  *unspecified*)

(define* (hashtable-empty-copy ht #:optional capacity weakness)
  (make-hashtable (hashtable-hash-function ht)
                  (hashtable-equivalence-function ht)
                  (case capacity
                    ((#f) #f)
                    ((#t) (hashtable-size ht))
                    (else capacity))
                  (or weakness (hashtable-weakness ht))))

(define (hashtable-keys ht)
  (let ((keys (make-vector (hashtable-size ht))))
    (hashtable-sum ht 0
      (lambda (k v i)
        (vector-set! keys i k)
        (+ i 1)))
    keys))

(define (hashtable-values ht)
  (let ((vals (make-vector (hashtable-size ht))))
    (hashtable-sum ht 0
      (lambda (k v i)
        (vector-set! vals i v)
        (+ i 1)))
    vals))

(define (hashtable-entries ht)
  (let ((keys (make-vector (hashtable-size ht)))
        (vals (make-vector (hashtable-size ht))))
    (hashtable-sum ht 0
      (lambda (k v i)
        (vector-set! keys i k)
        (vector-set! vals i v)
        (+ i 1)))
    (values keys vals)))

(define (hashtable-key-list ht)
  (hashtable-map->lset ht (lambda (k v) k)))

(define (hashtable-value-list ht)
  (hashtable-map->lset ht (lambda (k v) v)))

(define (hashtable-entry-lists ht)
  (let ((keys&vals (cons '() '())))
    (hashtable-walk ht
      (lambda (k v)
        (set-car! keys&vals (cons k (car keys&vals)))
        (set-cdr! keys&vals (cons v (cdr keys&vals)))))
    (car+cdr keys&vals)))

(define (hashtable-walk ht proc)
  (hash-for-each proc (%hashtable-table ht)))

(define (hashtable-update-all! ht proc)
  (assert-mutable ht)
  (hash-for-each-handle
   (lambda (handle)
     (set-cdr! handle (proc (car handle) (cdr handle))))
   (%hashtable-table ht)))

(define (hashtable-prune! ht pred)
  (assert-mutable ht)
  (let ((keys (hashtable-sum ht '()
                (lambda (k v keys-to-delete)
                  (if (pred k v)
                      (cons k keys-to-delete)
                      keys-to-delete)))))
    (for-each (lambda (k)
                (hashtable-delete! ht k))
              keys)))

(define (hashtable-merge! ht-dest ht-src)
  (assert-mutable ht-dest)
  (hashtable-walk ht-src
    (lambda (k v)
      (hashtable-set! ht-dest k v)))
  ht-dest)

(define (hashtable-sum ht init proc)
  (hash-fold proc init (%hashtable-table ht)))

(define (hashtable-map->lset ht proc)
  (hash-map->list proc (%hashtable-table ht)))

(define (hashtable-find ht pred)
  (let/ec return
    (hashtable-walk ht
      (lambda (k v)
        (when (pred k v)
          (return k v #t))))
    (return #f #f #f)))

(define (hashtable-empty? ht)
  (zero? (hashtable-size ht)))

(define (hashtable-pop! ht)
  (assert-mutable ht)
  (when (hashtable-empty? ht)
    (error "Cannot pop from empty hashtable." ht))
  (let-values (((k v found?) (hashtable-find ht (const #t))))
    (hashtable-delete! ht k)
    (values k v)))

(define* (hashtable-inc! ht k #:optional (x 1))
  (assert-mutable ht)
  (hashtable-update! ht k (lambda (v) (+ v x)) 0))

(define* (hashtable-dec! ht k #:optional (x 1))
  (assert-mutable ht)
  (hashtable-update! ht k (lambda (v) (- v x)) 0))

(define (hash-salt) 0)

(set-record-type-printer!
 <hashtable>
 (lambda (ht port)
   (with-output-to-port port
     (lambda ()
       (let ((equal-hash (@ (rnrs hashtables) equal-hash))
             (string-hash (@ (rnrs hashtables) string-hash))
             (string-ci-hash (@ (rnrs hashtables) string-ci-hash))
             (symbol-hash (@ (rnrs hashtables) symbol-hash))
             (hash (hashtable-hash-function ht))
             (equiv (hashtable-equivalence-function ht))
             (alist (hashtable-map->lset ht cons)))
         (cond
          ((and (not hash) (eq? equiv eq?))
           (display "#hasheq")
           (display alist))
          ((and (not hash) (eq? equiv eqv?))
           (display "#hasheqv")
           (display alist))
          (else
           (display "#hash")
           (cond
            ((and (eq? hash (@ (rnrs hashtables) equal-hash)) (eq? equiv equal?))
             (display alist))
            ((and (eq? hash (@ (rnrs hashtables) string-hash)) (eq? equiv string=?))
             (display (cons 'string alist)))
            ((and (eq? hash string-ci-hash) (eq? equiv string-ci=?))
             (display (cons 'string-ci alist)))
            ((and (eq? hash symbol-hash) (eq? equiv eq?))
             (display (cons 'symbol alist)))
            (else
             (display (cons 'custom alist)))))))))))

(read-hash-extend
 #\h
 (lambda (char port)
   (with-input-from-port port
     (lambda ()
       (let ((equal-hash (@ (rnrs hashtables) equal-hash))
             (string-hash (@ (rnrs hashtables) string-hash))
             (string-ci-hash (@ (rnrs hashtables) string-ci-hash))
             (symbol-hash (@ (rnrs hashtables) symbol-hash))
             (type (string-append "h" (symbol->string (read))))
             (alist (read)))
         (cond
          ((string=? type "hasheq")
           (alist->eq-hashtable alist))
          ((string=? type "hasheqv")
           (alist->eqv-hashtable alist))
          (else
           (when (not (string=? type "hash"))
             (error "Unrecognized hash type." type))
           (let* ((has-tag? (symbol? (car alist)))
                  (subtype (if has-tag?
                               (car alist)
                               "equal"))
                  (alist (if has-tag?
                             (cdr alist)
                             alist)))
             (cond
              ((string=? subtype "equal")
               (alist->hashtable equal-hash equal? alist))
              ((string=? subtype "string")
               (alist->hashtable string-hash string=? alist))
              ((string=? subtype "string-ci")
               (alist->hashtable string-ci-hash string-ci=? alist))
              ((string=? subtype "symbol")
               (alist->hashtable symbol-hash eq? alist))
              (else
               (error "Unrecognized hash subtype." subtype)))))))))))

;; Local Variables:
;; eval: (put 'hashtable-walk 'scheme-indent-function 1)
;; eval: (put 'hashtable-update-all! 'scheme-indent-function 1)
;; eval: (put 'hashtable-prune! 'scheme-indent-function 1)
;; eval: (put 'hashtable-sum 'scheme-indent-function 2)
;; eval: (put 'hashtable-map->lset 'scheme-indent-function 1)
;; eval: (put 'hashtable-find 'scheme-indent-function 1)
;; End:
