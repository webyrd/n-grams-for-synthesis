;;; Complete interface spec for the SRFI-13 string-lib and 	-*- Scheme -*-
;;; string-lib-internals libraries in the Scheme48 interface
;;; and module language. The interfaces are fully typed, in
;;; the Scheme48 type notation. The structure definitions also
;;; provide a formal description of the external dependencies
;;; of the source code.

;;; string-lib
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; string-map string-map!
;;; string-fold       string-unfold
;;; string-fold-right string-unfold-right 
;;; string-tabulate string-for-each string-for-each-index
;;; string-every string-any
;;; string-hash string-hash-ci
;;; string-compare string-compare-ci
;;; string=    string<    string>    string<=    string>=    string<>
;;; string-ci= string-ci< string-ci> string-ci<= string-ci>= string-ci<> 
;;; string-downcase  string-upcase  string-titlecase  
;;; string-downcase! string-upcase! string-titlecase! 
;;; string-take string-take-right
;;; string-drop string-drop-right
;;; string-pad string-pad-right
;;; string-trim string-trim-right string-trim-both
;;; string-filter string-delete
;;; string-index string-index-right 
;;; string-skip  string-skip-right
;;; string-count
;;; string-prefix-length string-prefix-length-ci
;;; string-suffix-length string-suffix-length-ci
;;; string-prefix? string-prefix-ci?
;;; string-suffix? string-suffix-ci?
;;; string-contains string-contains-ci
;;; string-fill! string-copy! 
;;; string-copy substring/shared
;;; string-reverse string-reverse! reverse-list->string
;;; string->list
;;; string-concatenate string-concatenate/shared
;;; string-concatenate-reverse string-concatenate-reverse/shared
;;; string-append/shared
;;; xsubstring string-xcopy!
;;; string-null?
;;; string-join
;;; string-tokenize
;;; string-replace
;;; 
;;; string? make-string string string-length string-ref string-set! 
;;; string-append list->string
;;;
;;; make-kmp-restart-vector string-kmp-partial-search kmp-step
;;; string-parse-start+end
;;; string-parse-final-start+end
;;; let-string-start+end
;;; check-substring-spec
;;; substring-spec-ok?

(define-interface xstring-lib-interface
  (export
   ;; string-map proc s [start end] -> s
   (string-map (proc ((proc (:char) :char)
		      :string
		      &opt :exact-integer :exact-integer)
		     :string))

   ;; string-map! proc s [start end] -> unspecific
   (string-map! (proc ((proc (:char) :values)
		       :string
		       &opt :exact-integer :exact-integer)
		      :unspecific))

   ;; string-fold       kons knil s [start end] -> value
   ;; string-fold-right kons knil s [start end] -> value
   ((string-fold string-fold-right)
    (proc ((proc (:char :value) :value)
	   :value :string
	   &opt :exact-integer :exact-integer)
	  :value))

   ;; string-unfold       p f g seed [base make-final] -> string
   ;; string-unfold-right p f g seed [base make-final] -> string
   ((string-unfold string-unfold)
    (proc ((proc (:value) :boolean)
	   (proc (:value) :char)
	   (proc (:value) :value)
	   :value
	   &opt :string (proc (:value) :string))
	  :string))

;   Enough is enough.
;   ;; string-unfoldn p f g seed ... -> string
;   (string-unfoldn (proc ((procedure :values :boolean)
;                          (procedure :values :char)
;                          (procedure :values :values)
;                          &rest :value)
;                         :string))

   ;; string-tabulate proc len -> string
   (string-tabulate (proc ((proc (:exact-integer) :char) :exact-integer)
			  :string))

   ;; string-for-each       proc s [start end] -> unspecific
   ;; string-for-each-index proc s [start end] -> unspecific
   ((string-for-each string-for-each-index)
    (proc ((proc (:char) :values) :string &opt :exact-integer :exact-integer)
	  :unspecific))

   ;; string-every pred s [start end]
   ;; string-any   pred s [start end]
   (string-every
    (proc ((proc (:char) :boolean) :string &opt :exact-integer :exact-integer)
	  :boolean))
   (string-any
    (proc ((proc (:char) :boolean) :string &opt :exact-integer :exact-integer)
	  :value))

   ;; string-hash    s [bound start end]
   ;; string-hash-ci s [bound start end]
   ((string-hash string-hash-ci)
    (proc (:string &opt :exact-integer :exact-integer :exact-integer)
	  :exact-integer))

   ;; string-compare    string1 string2 lt-proc eq-proc gt-proc [start end]
   ;; string-compare-ci string1 string2 lt-proc eq-proc gt-proc [start end]
   ((string-compare string-compare-ci)
    (proc (:string :string (proc (:exact-integer) :values)
		           (proc (:exact-integer) :values)
			   (proc (:exact-integer) :values)
			   &opt :exact-integer :exact-integer)
	  :values))

   ;; string< string1 string2 [start1 end1 start2 end2]
   ((string= string< string> string<= string>= string<>
     string-ci= string-ci< string-ci> string-ci<= string-ci>= string-ci<>)
    (proc (:string :string &opt :exact-integer :exact-integer
		                :exact-integer :exact-integer)
	  :boolean))

   ;; string-titlecase  string [start end]
   ;; string-upcase     string [start end]
   ;; string-downcase   string [start end]
   ;; string-titlecase! string [start end]
   ;; string-upcase!    string [start end]
   ;; string-downcase!  string [start end]
   ((string-titlecase  string-upcase  string-downcase)
    (proc (:string &opt :exact-integer :exact-integer) :string))
   ((string-titlecase! string-upcase! string-downcase!)
    (proc (:string &opt :exact-integer :exact-integer) :unspecific))

   ;; string-take       string nchars
   ;; string-drop       string nchars
   ;; string-take-right string nchars
   ;; string-drop-right string nchars
   ((string-take string-drop string-take-right string-drop-right)
    (proc (:string :exact-integer) :string))

   ;; string-pad       string k [char start end] 
   ;; string-pad-right string k [char start end] 
   ((string-pad string-pad-right)
    (proc (:string :exact-integer &opt :char :exact-integer :exact-integer)
	  :string))

   ;; string-trim       string [char/char-set/pred start end] 
   ;; string-trim-right string [char/char-set/pred start end] 
   ;; string-trim-both  string [char/char-set/pred start end] 
   ((string-trim string-trim-right string-trim-both)
    (proc (:string &opt :value :exact-integer :exact-integer)
	  :string))

   ;; string-filter char/char-set/pred string [start end]
   ;; string-delete char/char-set/pred string [start end]
   ((string-filter string-delete)
    (proc (:value :string &opt :exact-integer :exact-integer) :string))

   ;; string-index       string char/char-set/pred [start end]
   ;; string-index-right string char/char-set/pred [end start]
   ;; string-skip        string char/char-set/pred [start end]
   ;; string-skip-right  string char/char-set/pred [end start]
   ((string-index string-index-right string-skip string-skip-right)
    (proc (:string :value &opt :exact-integer :exact-integer)
	  :value))

   ;; string-count string char/char-set/pred [start end]
   (string-count (proc (:string :value &opt :exact-integer :exact-integer)
		       :exact-integer))

   ;; string-prefix-length    string1 string2 [start1 end1 start2 end2]
   ;; string-suffix-length    string1 string2 [start1 end1 start2 end2]
   ;; string-prefix-length-ci string1 string2 [start1 end1 start2 end2]
   ;; string-suffix-length-ci string1 string2 [start1 end1 start2 end2]
   ((string-prefix-length string-prefix-length-ci
     string-suffix-length string-suffix-length-ci)
    (proc (:string :string &opt
		   :exact-integer :exact-integer :exact-integer :exact-integer)
	  :exact-integer))

   ;; string-prefix?    string1 string2 [start1 end1 start2 end2]
   ;; string-suffix?    string1 string2 [start1 end1 start2 end2]
   ;; string-prefix-ci? string1 string2 [start1 end1 start2 end2]
   ;; string-suffix-ci? string1 string2 [start1 end1 start2 end2]
   ((string-prefix? string-prefix-ci?
     string-suffix? string-suffix-ci?)
    (proc (:string :string &opt
		   :exact-integer :exact-integer :exact-integer :exact-integer)
	  :boolean))

   ;; string-contains    string pattern [s-start s-end p-start p-end]
   ;; string-contains-ci string pattern [s-start s-end p-start p-end]
   ((string-contains string-contains-ci)
    (proc (:string :string &opt :exact-integer :exact-integer
		                :exact-integer :exact-integer)
	  :value))

   ;; string-fill! string char [start end]
   (string-fill! (proc (:string :char &opt :exact-integer :exact-integer)
		       :unspecific))

   ;; string-copy! to tstart from [fstart fend]
   (string-copy! (proc (:string :exact-integer :string
				&opt :exact-integer :exact-integer)
		       :unspecific))

   ;; string-copy        s [start end] -> string
   ;; substring/shared   s start [end] -> string
   (string-copy      (proc (:string &opt :exact-integer :exact-integer) :string))
   (substring/shared (proc (:string :exact-integer &opt :exact-integer) :string))

   ;; string-reverse  s [start end]
   ;; string-reverse! s [start end]
   (string-reverse  (proc (:string &opt :exact-integer :exact-integer) :string))
   (string-reverse! (proc (:string &opt :exact-integer :exact-integer) :unspecific))

   ;; reverse-list->string char-list
   ;; string->list s [start end]
   ;; string-concatenate        string-list
   ;; string-concatenate/shared string-list
   ;; string-append/shared s ...
   (reverse-list->string (proc (:value) :string))
   (string->list (proc (:string &opt :exact-integer :exact-integer) :value))
   ((string-concatenate string-concatenate/shared) (proc (:value) :string))
   (string-append/shared (proc (&rest :string) :string))

   ;; string-concatenate-reverse        string-list [final-string end]
   ;; string-concatenate-reverse/shared string-list [final-string end]
   ((string-concatenate-reverse string-concatenate-reverse/shared)
    (proc (:value &opt :string :exact-integer) :string))

   ;; xsubstring s from [to start end]
   ;; string-xcopy! target tstart s from [to start end]
   (xsubstring (proc (:string :exact-integer &opt
			      :exact-integer :exact-integer :exact-integer)
		     :string))
   (string-xcopy! (proc (:string :exact-integer :string :exact-integer &opt
				 :exact-integer :exact-integer :exact-integer)
			:unspecific))

   ;; string-null? s
   (string-null? (proc (:string) :boolean))

   ;; string-join string-list [delim grammar]
   (string-join (proc (:value &opt :string :symbol) :string))

   ;; string-tokenize string [token-chars start end]
   (string-tokenize (proc (:string &opt :value :exact-integer :exact-integer)
			  :value))

   ;; string-replace s1 s2 start1 end1 [start2 end2]
   (string-replace (proc (:string :string :exact-integer :exact-integer
				  &opt :exact-integer :exact-integer)
			 :string))

   ;; Here are the R4RS/R5RS procs
   (string? (proc (:value) :boolean))
   (make-string (proc (:exact-integer &opt :char) :string))
   (string (proc (&rest :char) :string))
   (string-length (proc (:string) :exact-integer))
   (string-ref (proc (:string :exact-integer) :char))
   (string-set! (proc (:string :exact-integer :char) :unspecific))
   (string-append (proc (&rest :string) :string))
   (list->string (proc (:value) :string))

   ;; These are the R4RS types for STRING-COPY, STRING-FILL!, and
   ;; STRING->LIST. The string-lib types are different -- extended.
   ;(string-copy (proc (:string) :string))
   ;(string-fill! (proc (:string :char) :unspecific))
   ;(string->list (proc (:string) :value))

   ))


;;; make-kmp-restart-vector
;;; string-kmp-partial-search
;;; kmp-step
;;; string-parse-start+end
;;; string-parse-final-start+end
;;; let-string-start+end
;;; check-substring-spec
;;; substring-spec-ok?

(define-interface string-lib-internals-interface
  (export
   (let-string-start+end :syntax)
   (string-parse-start+end (proc ((procedure :values :values) :string :value)
				 (some-values :exact-integer :exact-integer :value)))
   (string-parse-final-start+end (proc ((procedure :values :values) :string :value)
				       (some-values :exact-integer :exact-integer)))
   (check-substring-spec (proc ((procedure :values :values) :string :exact-integer :exact-integer)
			       :unspecific))
   (substring-spec-ok? (proc ((procedure :values :values) :string :exact-integer :exact-integer)
			     :boolean))

   ;; string-kmp-partial-search pat rv s i [c= p-start s-start s-end] -> integer
   (string-kmp-partial-search (proc (:string :vector :string :exact-integer
				     &opt (proc (:char :char) :boolean)
				          :exact-integer :exact-integer :exact-integer)
				    :exact-integer))

   ;; make-kmp-restart-vector s [c= start end] -> vector
   (make-kmp-restart-vector (proc (:string &opt (proc (:char :char) :boolean)
					        :exact-integer :exact-integer)
				  :vector))

   ;; kmp-step pat rv c i c= p-start -> integer
   (kmp-step (proc (:string :vector :char :exact-integer
			    (proc (:char :char) :boolean)
			    :exact-integer)
		   :exact-integer))
   ))


(define-structures ((string-lib string-lib-interface)
		    (string-lib-internals string-lib-internals-interface))
  (access scheme)	; Get at R5RS SUBSTRING
  (open receiving	; RECEIVE
	char-set-lib	; Various
	bitwise		; BITWISE-AND for hashing
	error-package	; ERROR
	nlet-opt	; LET-OPTIONALS* :OPTIONAL
	scheme)

  ;; A few cheesy S48/scsh definitions for string-lib dependencies:
  (begin (define (check-arg pred val caller)
	   (let lp ((val val))
	     (if (pred val) val (lp (error "Bad argument" val pred caller)))))

	 ;; These two internal procedures are correctly defined for ASCII or
	 ;; Latin-1. They are *not* correct for Unicode.
	 (define (char-cased? c) (char-set-contains? char-set:letter c))
	 (define (char-titlecase c) (char-upcase c)))

  (files srfi-13))
