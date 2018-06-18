(define code-names
  (with-input-from-file "generic-arrays.scm"
    (lambda ()
      (let loop ((result '())
		 (obj (read)))
;;	(pp obj)   ;;; make sure all functions are defined with (define (a b) ...)
	(if (##eof-object? obj)
	    result
	    (if (and (list? obj)
		     (not (null? obj))
		     (eq? (car obj) 'define)
		     (not (null? (cdr obj)))
		     (list? (cadr obj))
		     (not (null? (cadr obj)))
		     (symbol? (caadr obj))
		     (< 0 (string-length (symbol->string (caadr obj))))
		     (not (eq? (string-ref (symbol->string (caadr obj)) 0)
			       #\#)))
		(loop (cons (caadr obj) result)
		      (read))
		(loop result (read))))))))

(define other-names
  (with-input-from-file "generic-arrays.scm"
    (lambda ()
      (let loop ((result '())
		 (obj (read)))
;;	(pp obj)   ;;; make sure all functions are defined with (define (a b) ...)
	(if (##eof-object? obj)
	    result
	    (if (and (list? obj)
		     (not (null? obj))
		     (eq? (car obj) 'define)
		     (not (null? (cdr obj)))
		     (symbol? (cadr obj)))
		(loop (cons (cadr obj) result)
		      (read))
		(loop result (read))))))))

(define srfi-names
  (with-input-from-file "srfi-122.scm"
    (lambda ()
      (let loop ((obj (read)))
	(if (not (and (list? obj)
		      (not (null? obj))
		      (eq? (car obj)
			   'with-output-to-file)))
	    (loop (read))
	    (let ((result '()))
	      (define (process obj)
		(if (list? obj)
		    (if (and (not (null? obj))
			     (eq? (car obj)
				  'format-lambda-list))
			(set! result (cons (car (cadadr obj))
					   result))
			(for-each process obj))))
	      (process obj)
	      result))))))

(define (in-a-not-in-b a b)
  (do  ((a a (cdr a))
	(result '() (if (memq (car a) b)
			result
			(cons (car a) result))))
      ((null? a) result)))

(newline)(pp "SRFI names not in code: ")
(pp (in-a-not-in-b srfi-names code-names))

(newline)(pp "Code names not in SRFI: ")
(pp (in-a-not-in-b code-names srfi-names))
