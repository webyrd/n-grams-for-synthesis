;;; SRFI-14 interface for Scheme48				-*- Scheme -*-
;;; 
;;; Complete interface spec for the SRFI-14 char-set-lib library in the
;;; Scheme48 interface and module language. The interface is fully typed, in
;;; the Scheme48 type notation. The structure definitions also provide a
;;; formal description of the external dependencies of the source code.

(define-interface char-set-interface
  (export (char-set? (proc (:value) :boolean))
	  ((char-set= char-set<=) (proc (&rest :value) :boolean))

	  (char-set-hash (proc (:value &opt :exact-integer) :exact-integer))

	  ;; Cursors are exact integers in the reference implementation.
	  ;; These typings would be different with a different cursor
	  ;; implementation.
	  ;; Too bad Scheme doesn't have abstract data types.
	  (char-set-cursor      (proc (:value) :exact-integer))
	  (char-set-ref         (proc (:value :exact-integer) :char))
	  (char-set-cursor-next (proc (:value :exact-integer) :exact-integer))
	  (end-of-char-set?     (proc (:value) :boolean))

	  (char-set-fold (proc ((proc (:char :value) :value) :value :value)
			       :value))
	  (char-set-unfold (proc ((proc (:value) :boolean)
				  (proc (:value) :value)
				  (proc (:value) :value)
				  :value
				  &opt :value)
				 :value))

	  (char-set-unfold! (proc ((proc (:value) :boolean)
				   (proc (:value) :value)
				   (proc (:value) :value)
				   :value :value)
				  :value))

	  (char-set-for-each (proc ((proc (:char) :values) :value) :unspecific))
	  (char-set-map (proc ((proc (:char) :char) :value) :value))

	  (char-set-copy (proc (:value) :value))

	  (char-set (proc (&rest :char) :value))

	  (list->char-set (proc (:value &opt :value) :value))
	  (list->char-set! (proc (:value :value) :value))

	  (string->char-set  (proc (:value &opt :value) :value))
	  (string->char-set! (proc (:value :value) :value))

	  (ucs-range->char-set (proc (:exact-integer :exact-integer &opt
				      :boolean :value)
				     :value))
	  (ucs-range->char-set! (proc (:exact-integer :exact-integer
				      :boolean :value)
				     :value))

	  (char-set-filter  (proc ((proc (:char) :boolean) :value &opt :value) :value))
	  (char-set-filter! (proc ((proc (:char) :boolean) :value :value) :value))

	  (->char-set (proc (:value) :value))

	  (char-set-size (proc (:value) :exact-integer))
	  (char-set-count (proc ((proc (:char) :boolean) :value) :exact-integer))
	  (char-set-contains? (proc (:char :value) :boolean))

	  (char-set-every (proc ((proc (:char) :boolean) :value) :boolean))
	  (char-set-any (proc ((proc (:char) :boolean) :value) :value))

	  ((char-set-adjoin  char-set-delete
	    char-set-adjoin! char-set-delete!)
	   (proc (:value &rest :char) :value))

	  (char-set->list   (proc (:value) :value))
	  (char-set->string (proc (:value) :string))

	  (char-set-complement (proc (:value) :value))
	  ((char-set-union char-set-intersection char-set-xor)
	   (proc (&opt :value) :value))
	  
	  (char-set-difference (proc (:value &opt :value) :value))

	  (char-set-diff+intersection (proc (:value &rest :value)
					    (some-values :value :value)))

	  (char-set-complement! (proc (:value) :value))

	  ((char-set-union! char-set-intersection!
	    char-set-xor! char-set-difference!)
	   (proc (:value &opt :value) :value))

	  (char-set-diff+intersection! (proc (:value :value &rest :value)
					     (some-values :value :value)))

	  char-set:lower-case
	  char-set:upper-case
	  char-set:letter
	  char-set:digit
	  char-set:letter+digit
	  char-set:graphic
	  char-set:printing
	  char-set:whitespace
	  char-set:blank
	  char-set:iso-control
	  char-set:punctuation
	  char-set:symbol
	  char-set:hex-digit
	  char-set:ascii
	  char-set:empty
	  char-set:full
	  ))

(define-structure char-set-lib char-set-interface
  (open error-package	; ERROR procedure
	nlet-opt	; LET-OPTIONALS* and :OPTIONAL
	ascii		; CHAR->ASCII ASCII->CHAR
	bitwise		; BITWISE-AND
	jar-d-r-t-package ; DEFINE-RECORD-TYPE/JAR macro.
	scheme)

  (begin (define (check-arg pred val caller)
	   (let lp ((val val))
	     (if (pred val) val (lp (error "Bad argument" val pred caller)))))

	 (define %latin1->char ascii->char)	; Works for S48
	 (define %char->latin1 char->ascii)	; Works for S48

	 ;; Here's a SRFI-19 d-r-t defined in terms of jar's almost-identical
	 ;; d-r-t.
	 (define-syntax define-record-type
	   (syntax-rules ()
             ((define-record-type ?name ?stuff ...)
	      (define-record-type/jar ?name ?name ?stuff ...)))))

  (files srfi-14)
  (optimize auto-integrate))

;;; Import jar's DEFINE-RECORD-TYPE macro, and export it under the
;;; name DEFINE-RECORD-TYPE/JAR.
(define-structure jar-d-r-t-package (export (define-record-type/jar :syntax))
  (open define-record-types ; JAR's record macro
	scheme)
  (begin (define-syntax define-record-type/jar
	   (syntax-rules ()
             ((define-record-type/jar ?stuff ...)
	      (define-record-type ?stuff ...))))))
