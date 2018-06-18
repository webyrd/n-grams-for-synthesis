; Implementation of Primitive I/O for Comprehensive I/O SRFI

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

; Buffers

(define (make-i/o-buffer size)
  (make-blob size))

; Readers

(define-record-type :reader
  (make-reader id
	       descriptor chunk-size
	       read! available
	       get-position set-position! end-position
	       close)
  reader?
  ;; a string describing the reader
  (id reader-id)
  ;; low-level object
  (descriptor reader-descriptor)
  ;; recommendation
  (chunk-size reader-chunk-size)
  ;; blob start count -> count
  (read! reader-read!-proc)
  ;; () -> (union count #f)
  ;; hint only!
  (available reader-available-proc)
  ;; optional: () -> exact-integer
  (get-position reader-get-position-proc)
  ;; optional: exact-integer -> ()
  (set-position! reader-set-position!-proc)
  ;; optional: exact-integer -> ()
  (end-position reader-end-position-proc)
  ;; () -> whatever
  (close reader-close-proc))

(define (make-simple-reader id
			    descriptor chunk-size
			    read-blob! available
			    get-position set-position! end-position
			    close)
  (make-reader id
	       descriptor chunk-size
	       read-blob! available
	       get-position set-position! end-position
	       close))

(define (reader-read! reader blob start count)
  ((reader-read!-proc reader) blob start count))

(define (reader-available reader)
  ((reader-available-proc reader)))

(define (reader-has-get-position? reader)
  (and (reader-get-position-proc reader) #t))

(define (reader-get-position reader)
  (cond
   ((reader-get-position-proc reader)
    => (lambda (proc) (proc)))
   (else
    (raise
     (condition
      (&message
       (message "reader-get-position operation not available"))
      (&i/o-operation-not-available-error
       (operation reader-get-position))
      (&i/o-reader/writer-error
       (reader/writer reader)))))))

(define (reader-has-set-position!? reader)
  (and (reader-set-position!-proc reader) #t))

(define (reader-set-position! reader pos)
  (cond
   ((reader-set-position!-proc reader)
    => (lambda (proc)
	 (proc pos)))
   (else
    (raise
     (condition
      (&message
       (message "reader-set-position! operation not available"))
      (&i/o-operation-not-available-error
       (operation reader-set-position!))
      (&i/o-reader/writer-error
       (reader/writer reader)))))))

(define (reader-has-end-position? reader)
  (and (reader-end-position-proc reader) #t))

(define (reader-end-position reader)
  (cond
   ((reader-end-position-proc reader)
    => (lambda (proc) (proc)))
   (else
    (raise
     (condition
      (&message
       (message "reader-end-position operation not available"))
      (&i/o-operation-not-available-error
       (operation reader-end-position))
      (&i/o-reader/writer-error
       (reader/writer reader)))))))

(define (reader-close reader)
  ((reader-close-proc reader)))

(define (open-blob-reader b)
  (let ((size (blob-length b))
	(b (blob-copy b))
	(pos 0))

    (define (ensure-open)
      (if (not b)
	  (raise (condition
		  (&message
		   (message "blob reader closed"))
		  (&i/o-closed-error)
		  (&i/o-reader/writer-error
		   (reader/writer reader))))))
    
    (define reader
      (make-reader "<u8 vector>"
		   b
		   5			; for debugging
		   (lambda (blob start count)
		     (ensure-open)
			
		     (let ((real-count (min count (- size pos))))
		       (blob-copy! b pos
				       blob start real-count)
		       (set! pos (+ pos real-count))
		       real-count))
		   (lambda ()
		     (ensure-open)
		     (- size pos))
		   (lambda ()
		     (ensure-open)
		     pos)
		   (lambda (new-pos)
		     (ensure-open)
		     (if (<= new-pos size)
			 (set! pos new-pos)
			 (raise
			  (condition
			   (&message
			    (message "invalid position"))
			   (&i/o-invalid-position-error
			    (position new-pos))))))
		   (lambda ()
		     (ensure-open)
		     size)
		   (lambda ()
		     (set! b #f))))	; for GC
    reader))

(define (port-read-blob! port blob start count)
  (guard
   (c
    ;; convert SRFI-36 I/O errors to this SRFI's I/O error
    ((srfi-36:i/o-error? c)
     (raise
      (condition
       (&message
	(message "I/O error in READ-BLOB!"))
       (&i/o-read-error)))))
   (let loop ((index 0))
     (if (>= index count)
	 index
	 (let ((thing (read-byte port)))
	   (if (eof-object? thing)
	       index
	       (begin
		 (blob-u8-set! blob (+ start index) thing)
		 (loop (+ 1 index)))))))))

(define (make-port-reader filename port)
  (make-reader filename
	       port
	       10			; for testing
	       (cut port-read-blob! port <> <> <>)
	       (lambda () #f)	  ; no way to implement this currently
	       #f #f #f	; we currently have no way to implement this in Scheme 48
	       (cut close-input-port port)))

(define open-file-reader
  (opt-lambda (filename (options (file-options)))
    (guard
     (c
      ;; convert SRFI-36 I/O errors to this SRFI's I/O error
      ((srfi-36:i/o-error? c)
       (raise
	(condition
	 (&message
	  (message "I/O error in OPEN-FILE-READER"))
	 (&i/o-read-error)))))
     (make-port-reader filename (open-input-file filename)))))

(define the-standard-input-reader
  (let ((port (current-input-port)))
    (make-reader "<stdin>"
		 port
		 4096
		 (cut port-read-blob! port <> <> <>)
		 (lambda () 7)		; whatever
		 #f #f #f ; doesn't usually make sense
		 unspecific)))

(define (standard-input-reader)
  the-standard-input-reader)

; Writers

(define-record-type :writer
  (make-writer id
	       descriptor chunk-size
	       write!
	       get-position set-position! end-position
	       close)
  writer?
  ;; a string describing the writer
  (id writer-id)
  ;; low-level object
  (descriptor writer-descriptor)
  ;; recommendation
  (chunk-size writer-chunk-size)
  ;; octet vector start count -> count
  (write! writer-write!-proc)
  ;; optional: () -> exact-integer
  (get-position writer-get-position-proc)
  ;; optional: exact-integer -> ()
  (set-position! writer-set-position!-proc)
  ;; optional: exact-integer -> ()
  (end-position writer-end-position-proc)
  ;; () -> whatever
  (close writer-close-proc))

(define (make-simple-writer id
			    descriptor chunk-size
			    write!
			    get-position set-position! end-position
			    close)
  (make-writer id
	       descriptor chunk-size
	       write!
	       get-position set-position! end-position
	       close))

(define (writer-write! writer blob start count)
  ((writer-write!-proc writer) blob start count))

(define (writer-has-get-position? writer)
  (and (writer-get-position-proc writer) #t))

(define (writer-get-position writer)
  (cond
   ((writer-get-position-proc writer)
    => (lambda (proc) (proc)))
   (else
    (raise
     (condition
      (&message
       (message "writer-get-position operation not available"))
      (&i/o-operation-not-available-error
       (operation writer-get-position))
      (&i/o-reader/writer-error
       (reader/writer writer)))))))

(define (writer-has-set-position!? writer)
  (and (writer-set-position!-proc writer) #t))

(define (writer-set-position! writer pos)
  (cond
   ((writer-set-position!-proc writer)
    => (lambda (proc)
	 (proc pos)))
   (else
    (raise
     (condition
      (&message
       (message "writer-set-position! operation not available"))
      (&i/o-operation-not-available-error
       (operation writer-set-position!))
      (&i/o-reader/writer-error
       (reader/writer writer)))))))

(define (writer-has-end-position? writer)
  (and (writer-end-position-proc writer) #t))

(define (writer-end-position writer)
  (cond
   ((writer-end-position-proc writer)
    => (lambda (proc) (proc)))
   (else
    (raise
     (condition
      (&message
       (message "writer-end-position operation not available"))
      (&i/o-operation-not-available-error
       (operation writer-end-position))
      (&i/o-reader/writer-error
       (reader/writer writer)))))))

(define (writer-close writer)
  ((writer-close-proc writer)))

(define (open-blob-writer)
  (let ((buffer-cell
	 (cons (make-blob 512) ; buffer
	       0))			; size of data in buffer
	(pos 0))

    (define (ensure-open)
      (if (not buffer-cell)
	  (raise (condition
		  (&message
		   (message "blob writer closed"))
		  (&i/o-closed-error)
		  (&i/o-reader/writer-error
		   (reader/writer writer))))))

    (define writer
      (make-writer "<octet vector>"
		   buffer-cell
		   3
		   (lambda (blob start count)
		     (ensure-open)
		     ;; resize buffer if necessary
		     (let loop ((length (blob-length (car buffer-cell))))
		       (cond
			((> (+ pos count) length)
			 (loop (* 2 length)))
			((> length (blob-length (car buffer-cell)))
			 (let ((new-buffer (make-blob length)))
			   (blob-copy! (car buffer-cell) 0
					   new-buffer 0
					   (cdr buffer-cell))
			   (set-car! buffer-cell new-buffer)))))

		     (blob-copy! blob start
				     (car buffer-cell) pos
				     count)
		     (set-cdr! buffer-cell (max (cdr buffer-cell) (+ pos count)))
		     (set! pos (+ pos count))
		     count)
		   (lambda ()
		     (ensure-open)
		     pos)
		   (lambda (new-pos)
		     (ensure-open)
		     (if (<= new-pos (cdr buffer-cell))
			 (set! pos new-pos)
			 (raise
			  (condition
			   (&message
			    (message "invalid position"))
			   (&i/o-invalid-position-error
			    (position new-pos))))))
		   (lambda ()
		     (ensure-open)
		     (cdr buffer-cell))
		   (lambda ()
		     (set-car! buffer-cell #f))))
    writer))
		   
(define (writer-blob writer)
  (let* ((buffer-cell (writer-descriptor writer))
	 (target (make-blob (cdr buffer-cell))))
    (blob-copy! (car buffer-cell) 0
		    target 0
		    (cdr buffer-cell))
    target))

(define (port-write! port blob start count)
  (guard
   (c
    ;; convert SRFI-36 I/O errors to this SRFI's I/O error
    ((srfi-36:i/o-error? c)
     (raise
      (condition
       (&message
	(message "I/O error in WRITER-WRITE!"))
       (&i/o-read-error)))))

   (let ((count (+ 1 (random-integer count)))) ; be nasty for testing purposes
     (let loop ((index 0))
       (if (< index count)
	   (begin
	     (write-byte (blob-u8-ref blob (+ start index)) port)
	     (loop (+ 1 index)))))
     (force-output port)
     count)))

(define (make-port-writer id port)
  (make-writer id
	       port
	       7			; benasty for testing purposes
	       (cut port-write! port <> <> <>)
	       #f #f #f ; we currently have no way to implement this in Scheme 48
	       (cut close-output-port port)))

; this is awkward, because the Scheme 48 POSIX interface is too old
; Oleg Kiselyov to the rescue!

(define-syntax convert-options-helper
  (syntax-rules ()
    ((convert-options-helper ?var ?additionals (?clause ...))
     (cond ?clause ...))
    ((convert-options-helper ?var (?additional ...) (?clause ...)
			     (?option ...) ?options ...)
     (convert-options-helper ?var (?additional ...)
			     (?clause ... ((file-options-include?
					    ?var (file-options ?option ...))
					   (posix:file-options ?additional ... ?option ...)))
			     ?options ...))))

(define-syntax convert-options
  (syntax-rules ()
    ((convert-options (?additional ...) ?exp)
     (let ((options ?exp))
       (convert-options-helper
	options (?additional ...) ()
	(create exclusive truncate append)
	
	(create truncate append)
	(create exclusive append)
	(create exclusive truncate)
	(exclusive truncate append)

	(create exclusive)
	(create truncate)
	(create append)
	(exclusive truncate)
	(exclusive append)
	(truncate append)
   
	(create)
	(exclusive)
	(truncate)
	(append)
	
	())))))

(define (file-options->posix-file-options/write options)
  (convert-options (write-only) options))

(define (file-options->posix-file-options/read-write options)
  (convert-options (read-write) options))

(define open-file-writer
  (opt-lambda (filename (options (file-options)))
    (guard
     (c
      ;; convert SRFI-36 I/O errors to this SRFI's I/O error
      ((srfi-36:i/o-error? c)
       (raise
	(condition
	 (&message
	  (message "I/O error in OPEN-FILE-WRITER"))
	 (&i/o-read-error)))))
     (make-port-writer
      filename
      (posix:open-file filename
		       (file-options->posix-file-options/write options))))))

(define the-standard-output-writer
  (let ((port (current-output-port)))
    (make-writer "<stdout>"
		 port
		 4096
		 (cut port-write! port <> <> <>)
		 #f #f #f ; doesn't usually make sense
		 unspecific)))

(define (standard-output-writer)
  the-standard-output-writer)

(define the-standard-error-writer
  (let ((port (current-error-port)))
    (make-writer "<stderr>"
		 port
		 1
		 (cut port-write! port <> <> <>)
		 #f #f #f ; doesn't usually make sense
		 unspecific)))

(define (standard-error-writer)
  the-standard-error-writer)

; Both

(define (open-file-reader+writer filename options)
  (guard
   (c
    ;; convert SRFI-36 I/O errors to this SRFI's I/O error
    ((srfi-36:i/o-error? c)
     (raise
      (condition
       (&message
	(message "I/O error in OPEN-FILE-WRITER"))
      (&i/o-read-error)))))
   (let* ((output-port
	   (posix:open-file filename
			    (file-options->posix-file-options/read-write options)))
	  (input-port
	   (posix:dup-switching-mode output-port)))
     (values (make-port-reader filename input-port)
	     (make-port-writer filename output-port)))))
