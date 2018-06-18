; Scheme 48 package definitions for Records SRFI

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

(define-interface vector-types-interface
  (export make-vector-type
	  vector-type?
	  vector-type-name vector-type-supertypes vector-type-data
	  has-vector-type?
	  make-typed-vector typed-vector
	  typed-vector?
	  typed-vector-length
	  typed-vector-ref typed-vector-set!
	  typed-vector-type
	  make-typed-vector-immutable!))

(define-structure vector-types vector-types-interface
  (open scheme
	srfi-9 ; DEFINE-RECORD-TYPE
	srfi-23 ; ERROR
	)
  (files vector-type))

(define-interface opaque-cells-interface
  (export make-opaque-cell
	  opaque-cell?
	  opaque-cell-ref opaque-cell-set!
	  opaque-cell-key-fits?))

(define-structure opaque-cells opaque-cells-interface
  (open scheme
	srfi-9 ; DEFINE-RECORD-TYPE
	srfi-23 ; ERROR
	)
  (files opaque-cell))

(define-interface procedural-record-types-interface
  (export make-record-type-descriptor
	  record-type-descriptor?
	  make-record-constructor-descriptor
	  record-constructor record-predicate
	  record-accessor record-mutator))

(define-interface record-reflection-interface
  (export record-type-name
	  record-type-parent
	  record-type-sealed?
	  record-type-uid
	  record-type-generative?
	  record-type-field-names
	  record-type-opaque?
	  record-field-mutable?

	  record? record-rtd))

(define-structures ((procedural-record-types procedural-record-types-interface)
		    (record-reflection record-reflection-interface))
  (open scheme
	(subset srfi-1 (find every any delete-duplicates split-at))
	srfi-23 ; ERROR
	srfi-26 ; CUT
	opaque-cells
	vector-types
	)
  (files procedural-record))

(define-interface syntactic-record-types/explicit-interface
  (export (define-record-type :syntax)
	  (record-type-descriptor :syntax)
	  (record-constructor-descriptor :syntax)))

(define-structure syntactic-record-types/explicit syntactic-record-types/explicit-interface
  (open scheme
	srfi-23 ; ERROR
	procedural-record-types)
  (files syntactic-record-explicit))

(define-interface syntactic-record-types/implicit-interface
  (export (define-record-type :syntax)
	  (record-type-descriptor :syntax)
	  (record-constructor-descriptor :syntax)))

(define-structure syntactic-record-types/implicit syntactic-record-types/implicit-interface
  (open scheme
	(modify syntactic-record-types/explicit
		(rename (define-record-type define-record-type/explicit))))
  (files syntactic-record-implicit-r5rs
	 syntactic-record-implicit-s48))
