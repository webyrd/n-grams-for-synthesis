;; Copyright (C) Marc Nieper-Wißkirchen (2017).  All Rights Reserved. 

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Repl

(define (repl)
  (parameterize ((interaction-environment (setup-interaction-environment)))    
    (write-string "Inferior Scheme 0.1\n")
    (write-string "Copyright (C) Marc Nieper-Wißkirchen (2017).  All Rights Reserved.\n")
    (newline)
    (let loop ()
      (write-string "inferior> ")
      (let ((expression (read)))
	(unless (eof-object? expression)
	  (let-values ((result (eval expression (interaction-environment))))
	    (for-each
	     (lambda (value)
	       (unless (unspecified? value)
		 (write value)
		 (newline)))
	     result))
	  (loop))))))

(define initial-procedures
  '(current-continuation-marks
    continuation-marks?
    continuation-mark-set->list
    continuation-mark-set->list
    continuation-mark-set-first
    call-with-immediate-continuation-mark
    load
    =
    <
    >
    <=
    >=
    +
    *
    -
    /
    abs
    apply
    append
    angle
    assoc
    assq
    assv
    acos
    asin
    atan
    boolean?
    call-with-current-continuation
    call-with-input-file
    call-with-output-file
    car
    cdr
    caar
    cadr
    cdar
    cddr
    caaar
    caadr
    cadar
    caddr
    cdaar
    cdadr
    cddar
    cdddr
    caaaar
    caaadr
    caadar
    caaddr
    cadaar
    cadadr
    caddar
    cadddr
    cdaaar
    cdaadr
    cdadar
    cdaddr
    cddaar
    cddadr
    cdddar
    cddddr
    ceiling
    char?
    char=?
    char<?
    char>?
    char<=?
    char>=?
    char-alphabetic?
    char-downcase
    char-ci=?
    char-ci<?
    char-ci>?
    char-ci<=?
    char-ci>=?
    char->integer
    char-lower-case?
    char-numeric?
    char-upcase
    char-upper-case?
    char-whitespace?
    close-input-port
    close-output-port
    complex?
    cons
    cos
    current-input-port
    current-output-port
    denominator
    display
    eof-object?
    eq?
    equal?
    eqv?
    even?
    exact?
    exact->inexact
    exp
    expt
    floor
    for-each
    gcd
    imag-part
    inexact?
    inexact->exact
    input-port?
    integer?
    integer->char
    length
    lcm
    list
    list?
    list-ref
    log
    magnitude
    make-rectangular
    make-polar
    make-string
    make-vector
    map
    max
    min
    member
    memq
    memv
    modulo
    negative?
    newline
    not
    number?
    number->string
    numerator
    null?
    odd?
    open-input-file
    open-output-file
    output-port?
    pair?
    positive?
    procedure?
    quotient
    rational?
    rationalize
    real?
    real-part
    read
    read-char
    remainder
    reverse
    round
    peek-char
    set-car!
    set-cdr!
    sin
    sqrt
    string
    string?
    string=?
    string<?
    string>?
    string<=?
    string>=?
    string-append
    string-ci=?
    string-ci<?
    string-ci>?
    string-ci<=?
    string-ci>=?
    string-length
    string-ref
    string-set!
    string->number
    string->symbol
    substring
    symbol?
    symbol->string
    tan
    truncate
    write
    vector
    vector?
    vector-length
    vector-ref
    vector-set!
    zero?))

(define (setup-interaction-environment)
  (define primitive-environment (host-environment '(inferior primitive)))
  (let ((environment (make-environment)))
    (for-each
     (lambda (proc)
       (environment-add-binding! environment
				 proc
				 (box (host-eval proc primitive-environment))))
     initial-procedures)
    environment))

(define interaction-environment (make-parameter #f))
