; Validate implementation of cond-expand as a read-time application
; See cond-expand.scm for all the details.
;
; $Id$

(##include "myenv.scm")

(cerr
  "\nValidating implementation of cond-expand as a read-time application...\n\n")


(define SRFI0-ex1-code
  (apply string-append
    (list-intersperse
      (list
        "#,(cond-expand"
        "((and srfi-1 srfi-10)"
        "(write 1))"
        "((or srfi-1 srfi-10)"
        "(write 2))"
        "(else))")
      "\n")))

		; Interpret file file-name given feature-list
(define (with-features feature-list file-name)
  (cerr "\n\twhen features " feature-list " are defined: ")
  (OS:system "gsi -e '(##include \"cond-expand.scm\")' "
    "-e \"(define ALL-FEATURES '" 
    (with-output-to-string (lambda () (write feature-list)))
    ")\" " file-name))

(cerr "cond-expanding code in SRFI-0, example 1...\n" SRFI0-ex1-code nl)
(let ((file-name "/tmp/a"))

  (with-output-to-file file-name
    (lambda () (display SRFI0-ex1-code)))

  (with-features '(srfi-1) file-name)
  (with-features '(srfi-10) file-name)
  (with-features '(srfi-1 srfi-10) file-name)
  (with-features '(srfi-2) file-name)
  (cerr "\n\tDone\n")
)

(cerr "\nprinting cond-expanded code in SRFI-0, example 1...\n" SRFI0-ex1-code nl)
(let ((file-name "/tmp/a"))

  (with-output-to-file file-name
    (lambda () (display "(display '") (display SRFI0-ex1-code) (display ")")))

  (with-features '(srfi-1) file-name)
  (with-features '(srfi-10) file-name)
  (with-features '(srfi-1 srfi-10) file-name)
  (with-features '(srfi-2) file-name)
  (cerr "\n\tDone\n")
)


(define SRFI0-ex2-code
  (apply string-append
    (list-intersperse
      (list
        "#,(cond-expand"
        "(command-line"
	"(define (program-name) (car (argv)))))"
	"(pp program-name)")
      "\n")))

(cerr "\ncond-expanding code in SRFI-0, example 2...\n" SRFI0-ex2-code nl)
(let ((file-name "/tmp/a"))

  (with-output-to-file file-name
    (lambda () (display SRFI0-ex2-code)))

  (with-features '(command-line) file-name)
  (with-features '(srfi-10 command-line) file-name)
  (with-features '(srfi-10) file-name)
  (cerr "\n\tDone\n")
)


; Emulating #+ and #- of CL
; 
; http://www.harlequin.com/education/books/HyperSpec/Body/sec_24-1-2-1-1.html
; <BLOCKQUOTE>
; 24.1.2.1.1 Examples of Feature Expressions
; For example, suppose that in implementation A, the features spice and
; perq are present, but the feature lispm is not present; in
; implementation B, the feature lispm is present, but the features spice
; and perq are not present; and in implementation C, none of the features
; spice, lispm, or perq are present. The next figure shows some sample
; expressions, and how they would be read in these implementations.
; </BLOCKQUOTE>


(cerr "\n\ncond-expanding code similar to CL's"
  " 24.1.2.1.1 Examples of Feature Expressions\n")

(define (interpret-string-in-ABC . strs)
  (let ((file-name "/tmp/a"))

    (with-output-to-file file-name
      (lambda () (display "(write ") (for-each display strs) (display ")")))
    (cerr "\ninterpreting string: ") (for-each display strs) (newline)

  (with-features '(spice perq) file-name)
  (with-features '(lispm) file-name)
  (with-features '(srfi-10) file-name)
  (cerr "\n\tDone\n")
))

(interpret-string-in-ABC
  "(cons #,(cond-expand (spice \"Spice\") ((not spice) \"Lispm\")) 'X)")

(interpret-string-in-ABC
  "(cons #,(cond-expand (spice \"Spice\") (lispm \"Lispm\")) 'X)")

(interpret-string-in-ABC
  "(cons #,(cond-expand (spice \"Spice\") (perq \"Lispm\")) 'X)")
(interpret-string-in-ABC
  "(cons #,(cond-expand (perq \"Perq\") (spice \"Spice\")) 'X)")

(interpret-string-in-ABC
  "'#,(cond-expand ((or spice lispm) (let ((a 3) (b 3)) (foo a))) "
  " (else (let ((a 3)) (foo a))))")

(interpret-string-in-ABC
  "(cons #,(cond-expand (lispm \"Spice\") (spice \"foo\") "
  "((not (or lispm spice)) 7)) 'x)")

