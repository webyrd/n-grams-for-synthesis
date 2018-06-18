;;;; Ephemerons
;;;; Implementation sketch, with expected constant extra GC time

;;; Copyright (C) 2010--2015 Taylor R Campbell.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                         ;;;
;;; WARNING: This is not working code.  This is a sketch I wrote            ;;;
;;; before implementing working code in MIT Scheme for ephemerons.          ;;;
;;;                                                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Background:  What are ephemerons?
;;;
;;;   An ephemeron is an object with two subobjects called its key and
;;;   its datum, somewhat, but not exactly, like this:
;;;
;;;      (define (make-ephemeron key datum)
;;;        (cons (make-weak-cell key) datum))
;;;
;;;      (define (ephemeron.key ephemeron)
;;;        (weak-cell-value (car ephemeron)))
;;;
;;;      (define (ephemeron.datum ephemeron)
;;;        (cdr ephemeron))
;;;
;;;   In real ephemerons, however, not only is the reference to the
;;;   key is weak, but the reference to the datum is weak too.  The
;;;   key and datum are both considered live iff the key is considered
;;;   live by paths other than the ephemeron.
;;;
;;;   Thus, the key is proven dead iff the only transitive strong
;;;   references to the key from the roots of garbage collection pass
;;;   through the ephemeron -- even if the datum has a strong
;;;   reference to the key; and if the key is proven dead, then the
;;;   reference to the datum is dropped, and its storage may be
;;;   reclaimed.
;;;
;;;   In other words, the references to the key and datum are preserved
;;;   iff somebody other than the ephemeron cares about the key.
;;;
;;;   So, for example, the following program may yield (#F #F):
;;;
;;;     (let ((ephemeron
;;;            (let ((datum (cons 0 (cons 1 2))))
;;;              (make-ephemeron (cdr datum) datum))))
;;;       (collect-garbage)
;;;       (list (ephemeron.key ephemeron)
;;;             (ephemeron.datum ephemeron)))
;;;
;;;    However, if ephemerons were implemented with pairs and weak
;;;    cells as above, then it would always return ((1 . 2) (0 1 . 2)),
;;;    because the cdr of the pair would have a strong reference to (0
;;;    1 . 2), the datum, which has a strong reference to (1 . 2), the
;;;    key.

;;; Given:
;;;
;;;   (OBJECT-HASH <object> <modulus>) -> non-negative integer below <modulus>
;;;
;;;     Returns the least non-negative residue modulo <modulus> of an
;;;     integer unique to <object>.
;;;
;;;   (MARKED? <oldobject>) -> boolean
;;;   (TRACE <oldobject>) -> newobject
;;;   (FOLLOW <oldobject>) -> newobject
;;;
;;;     In a copying collector, MARKED? tells whether <oldobject> has
;;;     a forwarding pointer.  Otherwise, it tells whether <oldobject>
;;;     has been marked.  Immediate objects may be considered always
;;;     marked.
;;;
;;;     TRACE guarantees that <oldobject> and all objects transitively
;;;     strongly referenced by it are marked.  In a copying collector,
;;;     TRACE returns the object in the new space to which <oldobject>
;;;     has been forwarded.  Otherwise, it returns <oldobject>.
;;;
;;;     In a copying collector, if <oldobject> is an object in the old
;;;     space with a forwarding pointer, FOLLOW returns the object in
;;;     the new space to which it has been forwarded; it is an error
;;;     if <oldobject> is not an object in the old space with a
;;;     forwarding pointer.  In a non-copying collector, FOLLOW
;;;     returns <oldobject>; it is an error if <oldobject> is not
;;;     marked.
;;;
;;;   (RECYCLE-VECTOR! <vector> <length> <fill>) -> vector
;;;
;;;     <Vector> must be an unmarked vector of length at least
;;;     <length>.  In a copying collector, returns a vector in the new
;;;     space of length <length> all of whose elements are initialized
;;;     to <fill>.  Otherwise, shrinks <vector> in place to <length>,
;;;     releasing any storage formerly but no longer occupied by the
;;;     vector, and replaces all its elements by <fill>.
;;;
;;;   (NEXT-HASH-TABLE-SIZE <size>) -> non-negative integer
;;;
;;;     Returns a next good size for a hash table's array after <size>.
;;;
;;; Assumptions:
;;;
;;;   1. The garbage collector runs in one swell foop -- that is, we
;;;      are not using an incremental or generational or regional
;;;      garbage collector, or anything of the sort.
;;;
;;;   2. Tracing an unmarked object marks it.  Tracing a marked object
;;;      has no effect.  Thus, every object is marked at most once.
;;;
;;;   3. After marking an object, the garbage collector applies
;;;      POST-MARK-HOOK to the object's copy in the new space, in a
;;;      copying collector; or to the object, in a non-copying
;;;      collector.
;;;
;;;   4. UPDATE-EPHEMERONS is not called until after the bulk of the
;;;      garbage collector's work is done -- that is, all strong paths
;;;      from the roots have been traversed.  The garbage collector may
;;;      update ordinary weak references only after UPDATE-EPHEMERONS.
;;;
;;; Constraints:
;;;
;;;   1. We are in a garbage collector -- we are out of memory and
;;;      cannot allocate more than a constant amount of auxiliary
;;;      storage.

;;; Claims:
;;;
;;;   1. The storage for the key of an ephemeron is reclaimed (or, the
;;;      key of an ephemeron is not traced) iff the only references to
;;;      the key are through the datum.
;;;
;;;   2. If the storage for the key of an ephemeron is reclaimed, then
;;;      the ephemeron is destroyed -- its key and datum are both
;;;      replaced by #F.
;;;
;;;   3. POST-MARK-HOOK runs in expected O(1) time, and
;;;      UPDATE-EPHEMERONS runs in expected O(n + m) time, where n is
;;;      the number of live ephemerons and m is the number (and size)
;;;      of objects reachable only through ephemerons.
;;;
;;;   4. The size of the new space never increases beyond the size of
;;;      the old space as a consequence of collecting ephemerons.
;;;      (This requires RECYCLE-VECTOR! to behave as requested.)
;;;
;;; Questions, in descending order of importance:
;;;
;;;   1. Can this be done with smaller constant factors in space?
;;;      That is, can this be done with less space reserved for each
;;;      ephemeron?
;;;
;;;   2. Can this be done with smaller constant factors in time?  For
;;;      example, can it be done with less pointer-chasing?

;;;; Ephemeron Layout and Creation

;;; The number of words used by n ephemerons is
;;;
;;;   (+ vector-overhead (* n (+ record-overhead 4))),
;;;
;;; because each ephemeron has four words for record fields, and one
;;; word reserved in the ephemeron hash table.

(define-record-type <ephemeron>
    (%make-ephemeron key datum next-listed next-queued)
    ephemeron?
  (key ephemeron.key set-ephemeron.key!)
  (datum ephemeron.datum set-ephemeron.datum!)

  ;; While tracing, the garbage collector links up a list of the live
  ;; ephemerons through this field; *EPHEMERON-LIST* heads the list.
  (next-listed ephemeron.next-listed set-ephemeron.next-listed!)

  ;; The NEXT-QUEUED field is multiplexed for buckets in the ephemeron
  ;; hash table and for the ephemeron queue, used to process ephemerons
  ;; whose keys have just been marked.
  (next-queued ephemeron.next-queued set-ephemeron.next-queued!))

;;; Invariant: (<= *ephemeron-count* (vector-length *ephemeron-hash-table*))

(define (make-ephemeron key datum)
  (set! *ephemeron-count* (+ 1 *ephemeron-count*))
  ;; XXX Insert appropriate bookkeeping to guarantee the GC will free
  ;; up enough space for an ephemeron and possibly a reallocated
  ;; bucket array.
  (if (< (vector-length *ephemeron-hash-table*) *ephemeron-count*)
      (set! *ephemeron-hash-table*
            (make-vector (next-hash-table-size *ephemeron-count*))))
  (%make-ephemeron key datum #f #f #f))

;;;; GC State

;;; *EPHEMERON-COUNT*
;;;
;;;     Number of live ephemerons since the start of the last GC.

(define *ephemeron-count* 0)

;;; *EPHEMERON-HASH-TABLE*
;;;
;;;     Map from keys to ephemerons with that key.  The key is not yet
;;;     marked.  (Array of hash table buckets.)

(define *ephemeron-hash-table* (make-vector 0))

;;; *EPHEMERON-LIST*
;;;
;;;     List of all live ephemerons.  Only the ephemeron itself is
;;;     guaranteed marked; its key, and consequently datum, may not
;;;     be.

(define *ephemeron-list* #f)

;;; *EPHEMERON-QUEUE*
;;;
;;;     Queue of ephemerons whose keys have been marked but whose data
;;;     have yet to be traced.

(define *ephemeron-queue* #f)

;;; *UPDATING-EPHEMERONS?*
;;;
;;;     True if we're in the middle of UPDATE-EPHEMERONS.

(define *updating-ephemerons?* #f)

(define (for-each-ephemeron procedure)
  (let loop ((ephemeron *ephemeron-list*))
    (if (ephemeron? ephemeron)
        (let ((next (ephemeron.next-listed ephemeron)))
          (procedure ephemeron)
          (loop next)))))

(define (walk-ephemeron-queue procedure)
  (let loop ()
    (let ((ephemeron *ephemeron-queue*))
      (if (ephemeron? ephemeron)
          (begin (set! *ephemeron-queue* (ephemeron.next-queued ephemeron))
                 (procedure ephemeron)
                 (loop))))))

;;;; GC Post-Mark Hook

(define (post-mark-hook object)
  ;; OBJECT has been marked.  Do everything related to ephemerons
  ;; needed by it.
  (if (ephemeron? object) (post-mark-ephemeron object))
  (if *updating-ephemerons?* (queue-ephemerons-for-key object)))

(define (queue-ephemerons-for-key object)
  ;; OBJECT has been marked.  Queue all ephemerons dependent on
  ;; it as their key to be marked.
  (let* ((hash-table *ephemeron-hash-table*)
         (index (object-hash object (vector-length hash-table)))
         (bucket (vector-ref hash-table index)))
    ;; Insert all ephemerons with this key into the queue of
    ;; ephemerons with marked keys.  Some entries in this bucket may
    ;; have different keys because of hash collisions -- skip them.
    (let loop ((ephemeron bucket) (previous #f))
      (if (ephemeron? ephemeron)
          (let ((next (ephemeron.next-queued ephemeron)))
            (if (eq? object (ephemeron.key ephemeron))
                (begin
                  (if previous
                      (set-ephemeron.next-queued! previous next)
                      (vector-set! hash-table index next))
                  (set-ephemeron.next-queued! ephemeron *ephemeron-queue*)
                  (set! *ephemeron-queue* ephemeron)))
            (loop next ephemeron))))))

(define (post-mark-ephemeron ephemeron)
  ;; EPHEMERON has been marked.  Insert it into the list of live
  ;; ephemerons, and map its key back to it.
  (set-ephemeron.next-listed! ephemeron *ephemeron-list*)
  (set! *ephemeron-list* ephemeron)
  (let ((key (ephemeron.key ephemeron)))
    (if (not (marked? key))
        (let* ((hash-table *ephemeron-hash-table*)
               (index (object-hash key (vector-length hash-table))))
          (set-ephemeron.next-queued! ephemeron (vector-ref hash-table index))
          (vector-set! hash-table index ephemeron)))))

;;;; GC Hooks

;;; The GC must call INITIALIZE-EPHEMERONS before marking any
;;; ephemerons, and UPDATE-EPHEMERONS after tracing all strong
;;; references in the heap.

(define (initialize-ephemerons)
  (set! *ephemeron-count* 0))

(define (update-ephemerons)
  ;; All paths from roots not via ephemeron data have been traced.
  ;; Now trace paths from the data of ephemerons whose keys have been
  ;; marked.
  (set! *updating-ephemerons?* #t)
  ;; Ephemeron keys and data point at oldspace.
  (for-each-ephemeron pre-process-ephemeron)
  (walk-ephemeron-queue process-ephemeron)
  ;; Ephemeron keys point at oldspace, data point at newspace.
  (for-each-ephemeron post-process-ephemeron)
  ;; Ephemeron keys and data point at newspace.
  ;;
  ;; Invariant: post-GC ephemeron count <= pre-GC ephemeron count.
  ;; Thus, if there was space pre-GC for the ephemeron hash table,
  ;; there is space post-GC for it.
  (set! *ephemeron-hash-table*
        (recycle-vector! *ephemeron-hash-table* *ephemeron-count* #f))
  (set! *updating-ephemerons?* #f))

(define (pre-process-ephemeron ephemeron)
  ;; Ephemeron has been marked.  If its key has been marked without
  ;; taking any ephemeron data into consideration, its datum must now
  ;; be marked.
  (let ((key (ephemeron.key ephemeron)))
    (if (marked? key)
        (queue-ephemerons-for-key key))))

(define (process-ephemeron ephemeron)
  ;; Ephemeron has been marked, and so has its key.  So trace its
  ;; datum.
  (set-ephemeron.datum! ephemeron (trace (ephemeron.datum ephemeron))))

(define (post-process-ephemeron ephemeron)
  ;; Ephemeron has been marked.  Unhook all the GC structures, count
  ;; it for bookkeeping, and if the key was marked, follow any
  ;; forwarding pointer; otherwise break the ephemeron.
  (set! *ephemeron-count* (+ 1 *ephemeron-count*))
  (set-ephemeron.next-listed! ephemeron #f)
  (set-ephemeron.next-queued! ephemeron #f)
  (let ((key (ephemeron.key ephemeron)))
    (if (marked? key)
        (set-ephemeron.key! ephemeron (follow key))
        (begin (set-ephemeron.key! ephemeron #f)
               (set-ephemeron.datum! ephemeron #f)))))
