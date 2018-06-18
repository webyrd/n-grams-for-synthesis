; Helper code for managing options for Comprehensive I/O SRFI

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

; This gives you an alternative form for constructing records with
; keywords.  Say you have:

; (define-record-type :foo-options
;   (make-foo-options bar baz)
;   foo-options?
;   (bar foo-options-bar)
;   (baz foo-options-baz))

; You can then get a FOO-OPTIONS macro defined for you via:

; (define-options-type foo-options update-foo-options
;   (make-foo-options (bar foo-options-bar)
; 		      (baz foo-options-baz)))

; This allows you to do:

; (foo-options (bar 1) (baz 2))
; or:
; (update-foo-options o (baz 7))

; The latter form expects O to be a record of the same type---it gives
; you a record identical to O except for the BAR field which is 7.

(define unassigned (list 'unassigned))

(define (options-value-unassigned? x)
  (eq? x unassigned))

(define-syntax define-options-type
  (syntax-rules ()
    ((define-options-type ?create ?update (?cons ?field ...))
     (define-options-type-helper (?field ...) () ?create ?update ?cons))))

(define-syntax define-options-type-helper
  (syntax-rules ()
    ((define-options-type-helper ((?field0 ?accessor0) ?field-data ...) (?helper ...)
       ?create ?update ?cons)
     (define-options-type-helper (?field-data ...) (?helper ... (?field0 ?accessor0 helper))
       ?create ?update ?cons))
    ((define-options-type-helper () ((?field ?accessor ?helper) ...)
       ?create ?update ?cons)
     (begin
       (define-syntax ?helper
	 (syntax-rules (?field)
	   ((?helper ?default (?field ?field-exp) . ?clauses)
	    ?field-exp)
	   ((?helper ?default (?other-field ?field-exp) . ?clauses)
	    (?helper ?default . ?clauses))
	   ((?helper ?default)
	    ?default)))
       ...

       (define-syntax ?create
	 (syntax-rules ()
	   ((?create . ?clauses)
	    (?cons (?helper unassigned . ?clauses)
		   ...))))

       (define-syntax ?update
	 (syntax-rules ()
	   ((?create ?from . ?clauses)
	    (let ((from ?from))
	      (?cons (?helper (?accessor from) . ?clauses)
		     ...)))))))))
