; 		Validating Read-time Applications (reader-constructors)
;
; Instructions:
;	gsi vread-apply.scm
; or (load "vread-apply.scm") or (include "vread-apply.scm")
; within a Gambit interpreter or application.
; In either case, the current directory must contain
;	myenv.scm, catch-error.scm and read-apply.scm
; which are all available from
;	http://pobox.com/~oleg/ftp/Scheme/
; (the same site you must've gotten this code from).
; See read-apply.scm for more details.
;
; $Id$

(##include "myenv.scm")
(##include "catch-error.scm")
(##include "read-apply.scm")

(display "\nValidating Reader-Constructors and Read-Time applications...\n\n")

		; make sure that reading of a string STR gave the expected
		; result
(define (expect STR expected-result)
  (display "Reading ")
  (write STR)
  (display " => ")
  (let ((real-result
        (with-input-from-string STR read)))
    (write real-result)
    (if (equal? real-result expected-result)
      (display " as expected\n")
      (error " which is different from the expected result "
        expected-result))))


	; Check out that reading of STR  has failed indeed
	; (as it was supposed to)
(define-macro (must-have-failed STR)
  `(begin
    (display "Trying ") (write ,STR) (display "...\n")
    (assert (failed?
      (##catch-all
        (lambda (sig args) (error "catching " sig args))
        (lambda () (with-input-from-string ,STR read)))))))



(cerr "\nTest a few error conditions (before any reader-ctors are defined)\n")
(must-have-failed "#,(+ 1 2)")


(cerr "\nDefining a reader-ctor for '+\n")
(define-reader-ctor '+ +) 
(expect "#,(+ 1 2)" 3)

(cerr "\nTesting for a few more error conditions\n")
(must-have-failed "#,1 ")
(must-have-failed "#,(- 1 2)")

(cerr "\nDefining a reader-ctor for a standard datatype - list\n")
(define-reader-ctor 'list list) 
(expect "#,(list 1 2 #f \"4 5\")" '(1 2 #f "4 5"))

(cerr "\nNested applications\n")
(must-have-failed "#,(+ 1 (+ 2 3))")
(expect "#,(+ 1 #,(+ 2 3))" 6)

(cerr "\nDeclaring my-vector 'structure'\n")
(define-reader-ctor 'my-vector
  (lambda x (apply vector (cons 'my-vector-tag x))))
(expect "#,(my-vector (my-vector 1 2))" '#(my-vector-tag (my-vector 1 2)))
;        a vector whose second element is a list of a symbol my-vector,
;        number 1, and number 2.
(expect "#,(my-vector #,(my-vector 1 2))" '#(my-vector-tag #(my-vector-tag 1 2)))
(expect "#,(my-vector #,(my-vector #,(+ 9 -4)))"
	'#(my-vector-tag #(my-vector-tag 5)))
;        a vector whose second element is a my-vector constructed from
;        number 5

(cerr "\nUniform vectors (SRFI-4), which incidentally inspired this proposal\n")
(define-reader-ctor 'f32 f32vector)
(expect "#,(f32 1.0 2.0 3.0)" '#f32(1.0 2. 3.))

(cerr "\nLoading a file containing read-time applications\n")
(let ((temp-load-file "/tmp/t.scm")
      (src-code "(define (temp-proc) (let ((v '#,(f32 1.0 2.0 3.0))) (f32vector-ref v 1)))")
      )
    (cerr "\twriting the following string into a temp file " temp-load-file
      "\n\t" src-code nl)
    (with-output-to-file temp-load-file
      (lambda () (display src-code)))
    (load temp-load-file)
    (cerr "\tLoading of this file must've defined a new procedure temp-proc:\n\t")
    (pp temp-proc)
    (cerr "\n\tMaking sure that evaluating (temp-proc) yields the expected result")
    (assert (equal? (temp-proc) 2.0))
    (cerr "\n\tDone\n")
)

(cerr "\nIn a #,(tag arg...) form, the tag itself may be a read-time application\n")
(define-reader-ctor 'plus-or-list
  (let ((flag #t))
    (lambda () (begin0 (if flag '+ 'list) (set! flag (not flag))))))
(expect "#,(#,(plus-or-list) 1 2)" 3)
(expect "#,(#,(plus-or-list) 1 2)" '(1 2))
(expect "#,(#,(plus-or-list) 1 2)" 3)

(cerr "\nTesting reading of a 'file' object\n")
(let ((file-name "/tmp/a") (pattern "123 -!# 456"))
  (cerr "\nWriting pattern '" pattern "' into a file '" file-name "'\n")
  (with-output-to-file file-name
    (lambda () (display pattern)))
  (define-reader-ctor 'file open-input-file)
  (let* ((external-file-representation
        (with-output-to-string
          (lambda ()
            (display "#,")
            (write `(file ,file-name)))))
      (form `(with-input-from-string ,external-file-representation
          (lambda () (read-char (read)))))
      )
    (newline) (write form) (display " => ")
    (let ((c (eval form)) (c-expected (string-ref pattern 0)))
      (write c)
      (if (equal? c c-expected)
        (display " as expected\n")
        (error " which is different from the expected result "
          c-expected)))))
