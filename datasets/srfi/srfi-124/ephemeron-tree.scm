;;;; Ephemerons
;;;; Implementation sketch, with guaranteed worst-case extra GC time

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
;;;   (MAKE-EPHEMERON-TREE) -> ephemeron-tree
;;;   (EPHEMERON-TREE/EMPTY! <ephemeron-tree>)
;;;   (EPHEMERON-TREE/FOR-EACH <ephemeron-tree> <procedure>)
;;;   (EPHEMERON-TREE/INSERT! <ephemeron-tree> <ephemeron>) -> ephemeron
;;;   (EPHEMERON-TREE/DELETE-KEY! <ephemeron-tree> <object>) -> ephemeron or #f
;;;
;;;     Ephemeron trees use the left, right, and parent fields of
;;;     ephemerons, and can store, say, a colour, red or black, in the
;;;     pointers to children.
;;;
;;;     EPHEMERON-TREE/INSERT! does not replace.  It returns whatever
;;;     ephemeron has the key after the operation, after O(log n) time
;;;     using O(1) space.
;;;
;;;     EPHEMERON-TREE/DELETE-KEY! returns the ephemeron in the tree
;;;     whose key is <object>, or #F if there is none, after O(log n)
;;;     time using O(1) space.
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
;;;   3. POST-MARK-HOOK runs in worst-case O(log n) time, and
;;;      UPDATE-EPHEMERONS runs in worst-case O((n + m) log n) time,
;;;      where n is the number of live ephemerons and m is the number
;;;      (and size) of objects reachable only through ephemerons.
;;;
;;; Questions, in descending order of importance:
;;;
;;;   1. Can this be done without the logarithmic factors?
;;;
;;;   2. Can this be done with smaller constant factors in space?
;;;      That is, can this be done with less space reserved for each
;;;      ephemeron?
;;;
;;;   3. Can this be done with smaller constant factors in time?
;;;
;;;   4. Can this be done avoiding the complication of balanced tree
;;;      deletion in ephemeron trees, without using more space per
;;;      ephemeron?

;;;; Ephemeron Layout and Construction; GC State

(define-record-type <ephemeron>
    (%make-ephemeron key datum left right parent next-listed next-queued)
    ephemeron?
  (key ephemeron.key set-ephemeron.key!)
  (datum ephemeron.datum set-ephemeron.datum!)
  (left ephemeron.left set-ephemeron.left!)
  (right ephemeron.right set-ephemeron.right!)
  (parent ephemeron.parent set-ephemeron.parent!)

  ;; While tracing, the garbage collector links up a list of the live
  ;; ephemerons through this field; *EPHEMERON-LIST* heads the list.
  (next-listed ephemeron.next-listed set-ephemeron.next-listed!)

  ;; When an ephemeron has been marked and its key not yet marked, it
  ;; is stored in *EPHEMERON-TREE* linked up via NEXT-QUEUED with all
  ;; other ephemerons with the same key.  When its key has been
  ;; marked, it and all ephemerons with the same key are linked up via
  ;; NEXT-QUEUED in *EPHEMERON-QUEUE* for their data to be traced.
  (next-queued ephemeron.next-queued set-ephemeron.next-queued!))

(define (make-ephemeron key datum)
  (%make-ephemeron key datum #f #f #f #f #f))

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

;;; *EPHEMERON-TREE*
;;;
;;;     Map from keys to ephemerons with that key.  The key is not yet
;;;     marked.  (Root of balanced tree.)

(define *ephemeron-tree* (make-ephemeron-tree)) ;Map from key -> ephemeron

;;; *UPDATING-EPHEMERONS?*
;;;
;;;     True if we're in the middle of UPDATE-EPHEMERONS.

(define *updating-ephemerons?* #f)

(define (for-each-ephemeron procedure)
  (let loop ((ephemeron *ephemeron-list*))
    (if (ephemeron? ephemeron)
        (begin
          (procedure ephemeron)
          (loop (ephemeron.next-listed ephemeron))))))

(define (walk-ephemeron-queue procedure)
  (do () ((not (ephemeron? *ephemeron-queue*)))
    (let ((ephemeron *ephemeron-queue*))
      (set! *ephemeron-queue* (ephemeron.next-queued ephemeron))
      (procedure ephemeron))))

;;;; GC Post-Mark Hook

(define (post-mark-hook object)
  ;; OBJECT has been marked.  Do everything related to ephemerons
  ;; needed by it.
  (if (ephemeron? object) (post-mark-ephemeron object))
  (if *updating-ephemerons?* (queue-ephemerons-for-key object)))

(define (queue-ephemerons-for-key object)
  ;; OBJECT has been marked.  Queue all ephemerons dependent on it as
  ;; their key to be traced.
  (let ((ephemeron (ephemeron-tree/delete-key! *ephemeron-tree* object)))
    (if (ephemeron? ephemeron)
        (let ((first ephemeron))
          ;; Insert all ephemerons with this key into the queue of
          ;; ephemerons with marked keys.
          (let loop ((ephemeron ephemeron))
            (let ((next (ephemeron.next-queued ephemeron)))
              (if (ephemeron? next)
                  (loop next)
                  (let ((last ephemeron))
                    (set-ephemeron.next-queued! last *ephemeron-queue*)
                    (set! *ephemeron-queue* first)))))))))

(define (post-mark-ephemeron ephemeron)
  ;; EPHEMERON has been marked.  Insert it into the list of live
  ;; ephemerons, and map its key back to it.
  (set-ephemeron.next-listed! ephemeron *ephemeron-list*)
  (set! *ephemeron-list* ephemeron)
  (let ((key (ephemeron.key ephemeron)))
    (if (not (marked? key))
        (let ((ephemeron* (ephemeron-tree/insert! *ephemeron-tree* ephemeron)))
          (if (eq? ephemeron* ephemeron)
              (set-ephemeron.next-queued! ephemeron #f)
              (let ((ephemeron** (ephemeron.next-queued ephemeron*)))
                ;; Insert this ephemeron the queue of ephemerons with
                ;; the same key.
                (set-ephemeron.next-queued! ephemeron* ephemeron)
                (set-ephemeron.next-queued! ephemeron ephemeron**)))))))

;;;; GC Hooks

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
  (ephemeron-tree/empty! *ephemeron-tree*)
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
  ;; Ephemeron has been marked.  Unhook all the GC structures, and if
  ;; the key was marked, follow any forwarding pointer; otherwise
  ;; break the ephemeron.
  (set-ephemeron.left! ephemeron #f)
  (set-ephemeron.right! ephemeron #f)
  (set-ephemeron.parent! ephemeron #f)
  (set-ephemeron.next-listed! ephemeron #f)
  (set-ephemeron.next-queued! ephemeron #f)
  (let ((key (ephemeron.key ephemeron)))
    (if (marked? key)
        (set-ephemeron.key! ephemeron (follow key))
        (begin (set-ephemeron.key! ephemeron #f)
               (set-ephemeron.datum! ephemeron #f)))))
