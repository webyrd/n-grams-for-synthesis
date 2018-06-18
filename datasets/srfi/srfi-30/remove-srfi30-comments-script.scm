#! /usr/bin/env scheme-r5rs

(define (main arguments)
  (dup-all-but-srfi-30-comments)
  0)

; Don't detabify this file!

(define whitespace-chars
  (cons #\space 
	(cons #\newline
	      (map (lambda (s) (string-ref s 0))
		   '("	" "" ""))))) ;tab page return

(define (dup-all-but-srfi-30-comments)
  (let loop ()
    (if (not (eof-object? (peek-char)))
	(begin 
	  (sub-dup)
	  (loop)))))

; Main dispatch

(define (sub-dup)
  (let ((c (read-char)))
    (if (not (eof-object? c))
        ((char->dupper c) c))))

(define *read-alist* '())

(define (char->dupper char)
  (cond ((assq char *read-alist*)
	 => cdr)
	(else
         write-char)))

(define *read-terminating?-alist* '())

(define (is-char-terminating? char)
  (cond ((assq char *read-terminating?-alist*)
	 => cdr)
	(else #t)))

(define (set-standard-syntax! char terminating? dupper)
  (set! *read-alist* (cons (cons char dupper) *read-alist*))
  (set! *read-terminating?-alist* (cons (cons char terminating?) 
					*read-terminating?-alist*)))

(let ((sub-dup-whitespace
       (lambda (c)
         (write-char c)
         (sub-dup))))
  (for-each (lambda (c)
	      (set! *read-alist* (cons (cons c sub-dup-whitespace) *read-alist*)))
	    whitespace-chars))

(let ((sub-dup-constituent
       (lambda (c)
	 (sub-dup-token c))))
  (for-each (lambda (c)
              (set-standard-syntax! c #f sub-dup-constituent))
            (string->list
             (string-append "!$%&*+-./0123456789:<=>?@^_~ABCDEFGHIJKLM"
                            "NOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))))

; Usual read macros

(define (set-standard-dup-macro! c terminating? proc)
  (set-standard-syntax! c terminating? proc))

(for-each (lambda (c)
            (set-standard-dup-macro! c #t
              (lambda (c)
                (write-char c))))
          (string->list "()'`,"))

(set-standard-dup-macro! #\" #t
  (lambda (c)
    (write-char c)
    (let loop ()
      (let ((c (read-char)))
        (cond ((eof-object? c))
              ((char=? c #\\)
	       (write-char c)
               (let ((c (read-char)))
                 (cond ((not (eof-object? c))
			(write-char c)
			(loop)))))
              ((char=? c #\")
	       (write-char c))
              (else
	       (write-char c)
	       (loop)))))))

(set-standard-dup-macro! #\; #t
  (lambda (c)
    (write-char c)
    (dup-line)
    (sub-dup)))

(define (dup-line)
  (let loop ()
    (let ((c (read-char)))
      (cond ((eof-object? c))
	    ((char=? c #\newline)
	     (write-char c))
	    (else 
	     (write-char c)
	     (loop))))))

(define *sharp-macros* '())

(define (define-sharp-macro c proc)
  (set! *sharp-macros* (cons (cons c proc) *sharp-macros*)))

(define (define-dupping-sharp-macro c proc)
  (define-sharp-macro c 
    (lambda (c)
      (write-char #\#)
      (proc c))))

(set-standard-dup-macro! #\# #f
  (lambda (c)
    (let* ((c (peek-char))
	   (c (if (eof-object? c)
		  (error "end of file after #")
		  (char-downcase c)))
	   (probe (assq c *sharp-macros*)))
      (if probe
	  ((cdr probe) c)
	  (error "unknown # syntax")))))

(define-dupping-sharp-macro #\f
  (lambda (c) (write-char (read-char))))

(define-dupping-sharp-macro #\t
  (lambda (c) (write-char (read-char))))

(define-dupping-sharp-macro #\\
  (lambda (c)
    (write-char (read-char))
    (let ((c (peek-char)))
      (cond ((eof-object? c)
	     (error "end of file after #\\"))
	    ((char-alphabetic? c)
	     (sub-dup))
	    (else
	     (write-char (read-char)))))))

(define-dupping-sharp-macro #\(
  (lambda (c)
    (write-char (read-char))
    (sub-dup)))

(define-sharp-macro #\|
  (lambda (c)
    (skip-comment! 'read-sharp)))

; SRFI 22's script prelude
(define-sharp-macro #\!
  (lambda (c)
    (write-char #\#)
    (write-char (read-char))
    (dup-line)
    (sub-dup)))

(let ((number-sharp-macro
       (lambda (c)
	 (write-char c)
	 (sub-dup-token #\#))))
  (for-each (lambda (c)
	      (define-dupping-sharp-macro c number-sharp-macro))
	    '(#\b #\o #\d #\x #\i #\e)))

; Tokens

(define (sub-dup-token c)
  (write-char c)
  (let loop ()
    (let ((c (peek-char)))
      (cond ((or (eof-object? c)
		 (is-char-terminating? c)))
            (else
	     (write-char (read-char))
             (loop))))))

;;; reference implementation for skipping a single nested multi-line comment

(define (skip-comment! . maybe-start-state)
  (let lp ((state (if (null? maybe-start-state) 'start (car maybe-start-state))) 
	   (nested-level 0))
    (define (next-char)
      (let ((c (read-char)))
	(if (eof-object? c)
	    (error "EOF inside block comment -- #| missing a closing |#")
	    c)))

    (case state
      ((start) (case (next-char)
		 ((#\|) (lp 'read-bar nested-level))
		 ((#\#) (lp 'read-sharp nested-level))
		 (else (lp 'start nested-level))))
      ((read-bar) (case (next-char)
		    ((#\#) (if (> nested-level 1)
			       (lp 'start (- nested-level 1))))
		    (else (lp 'start nested-level))))
      ((read-sharp) (case (next-char)
		      ((#\|) (lp 'start (+ nested-level 1)))
		      (else (lp 'start nested-level)))))))