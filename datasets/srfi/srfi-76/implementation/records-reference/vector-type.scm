; Vector types for Records SRFI

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

(define-record-type :vector-type
  (make-vector-type name supertypes data)
  vector-type?
  (name vector-type-name)
  (supertypes vector-type-supertypes)
  (data vector-type-data))

; does TYPE-1 represent an ancestor of TYPE-2?
(define (type-ancestor? type-1 type-2)
  (let recur ((type-2 type-2))
    (or (eq? type-1 type-2)
	(let loop ((supers (vector-type-supertypes type-2)))
	  (if (null? supers)
	      #f
	      (or (recur (car supers))
		  (loop (cdr supers))))))))

; Typed vectors

(define-record-type :typed-vector
  (really-make-typed-vector type immutable? components)
  typed-vector?
  (type typed-vector-type)
  ;; the following are begging to be unboxed
  (immutable? typed-vector-immutable? set-typed-vector-immutable?!)
  (components typed-vector-components))

(define (has-vector-type? type thing)
  (and (typed-vector? thing)
       (type-ancestor? type
		      (typed-vector-type thing))))

(define (make-typed-vector type size)
  (really-make-typed-vector type #f (make-vector size)))

(define (typed-vector type . vals)
  (really-make-typed-vector type #f (list->vector vals)))

(define (ensure-has-vector-type type typed-vector)
  (if (not (has-vector-type? type typed-vector))
      (error "invalid argument: not of type" type typed-vector)))

(define (typed-vector-length type typed-vector)
  (ensure-has-vector-type type typed-vector)
  (vector-length (typed-vector-components typed-vector)))

(define (typed-vector-ref type typed-vector index)
  (ensure-has-vector-type type typed-vector)
  (vector-ref (typed-vector-components typed-vector) index))

(define (typed-vector-set! type typed-vector index val)
  (ensure-has-vector-type type typed-vector)
  (if (typed-vector-immutable? typed-vector)
      (error "typed vector immutable" typed-vector))
  (vector-set! (typed-vector-components typed-vector)
	       index
	       val))

(define (make-typed-vector-immutable! typed-vector)
  (set-typed-vector-immutable?! typed-vector #t))
