; Implementation of output streams for Comprehensive I/O SRFI

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

(define-enumerated-type output-stream-status :output-stream-status
  output-stream-status?
  output-stream-statuses
  output-stream-status-name
  output-stream-status-index
  (open terminated closed))

(define-record-type :translated-output-stream
  (really-make-translated-output-stream base status proc state)
  translated-output-stream?
  (base output-stream-base)
  (status translated-output-stream-status
	  set-translated-output-stream-status!)
  ;; output-stream state (union octet blob) -> state
  (proc output-stream-translator-proc)
  (state output-stream-translator-state
	 set-output-stream-translator-state!))

(define (make-translated-output-stream base proc state)
  (really-make-translated-output-stream base 
					(output-stream-status open)
					proc state))

;; output streams

(define-record-type :writer-output-stream
  (really-make-writer-output-stream writer
				    buffer-mode buffer
				    buffer-position buffer-start buffer-count
				    state)
  writer-output-stream?
  (writer output-stream-writer)
  (buffer-mode writer-output-stream-buffer-mode
	       really-set-writer-output-stream-buffer-mode!)
  ;; octet vector
  (buffer output-stream-buffer
	  set-output-stream-buffer!)
  ;; may be #f if the underlying writer doesn't have GET-POSITION-PROC
  (buffer-position output-stream-buffer-position
		   set-output-stream-buffer-position!)
  (buffer-start output-stream-buffer-start
		set-output-stream-buffer-start!)
  (buffer-count output-stream-buffer-count
		set-output-stream-buffer-count!)
  (state writer-output-stream-current-status
	 set-writer-output-stream-current-status!))

(define (open-writer-output-stream writer buffer-mode)
  (really-make-writer-output-stream writer
				    buffer-mode
				    (make-i/o-buffer (writer-chunk-size writer))
				    (if (writer-has-get-position? writer)
					(writer-get-position writer)
					#f)
				    0 0
				    (output-stream-status open)))

(define (output-stream? thing)
  (or (translated-output-stream? thing)
      (writer-output-stream? thing)))

(define (make-output-stream-proc proc)
  (lambda (output-stream . rest)
    (let loop ((output-stream output-stream))
      (cond ((translated-output-stream? output-stream)
	     (loop (output-stream-base output-stream)))
	    ((writer-output-stream? output-stream)
	     (apply proc output-stream rest))))))

(define (output-stream-current-status output-stream)
  (cond
   ((writer-output-stream? output-stream)
    (writer-output-stream-current-status output-stream))
   ((translated-output-stream? output-stream)
    (translated-output-stream-status output-stream))))

(define output-stream-buffer-mode
  (make-output-stream-proc writer-output-stream-buffer-mode))
(define really-set-output-stream-buffer-mode!
  (make-output-stream-proc really-set-writer-output-stream-buffer-mode!))

(define (empty-buffer! output-stream)
  (cond
   ((writer? (output-stream-writer output-stream))
    (let* ((writer (output-stream-writer output-stream))
	   (count (output-stream-buffer-count output-stream))
	   (start (output-stream-buffer-start output-stream))
	   (sent
	    (writer-write! writer
			   (output-stream-buffer output-stream)
			   start count)))
      (if (= count sent)
	  (begin
	    (set-output-stream-buffer-start! output-stream 0)
	    (set-output-stream-buffer-count! output-stream 0))
	  (begin
	    (set-output-stream-buffer-start! output-stream (+ start sent))
	    (set-output-stream-buffer-count! output-stream (- count sent))))))))
 
(define (output-stream-buffer-blob-left output-stream)
  (- (blob-length (output-stream-buffer output-stream))
     (+ (output-stream-buffer-start output-stream)
	(output-stream-buffer-count output-stream))))

(define (ensure-output-stream-open output-stream)
  (if  (not (eq? (output-stream-status open)
		 (output-stream-current-status output-stream)))
       (raise
	(condition
	 (&message
	  (message "output stream closed"))
	 (&i/o-stream-error
	  (stream output-stream))
	 (&i/o-closed-error)))))

(define output-blob
  (opt-lambda (output-stream
	       blob
	       (start 0) (count (- (blob-length blob) start)))
    (ensure-output-stream-open output-stream)
    (cond
     ((translated-output-stream? output-stream)
      (set-output-stream-translator-state!
       output-stream
       ((output-stream-translator-proc output-stream)
	(output-stream-base output-stream)
	(output-stream-translator-state output-stream)
	blob start count)))
     ((writer-output-stream? output-stream)
      (let* ((buffer (output-stream-buffer output-stream))
	     (buffer-size (blob-length buffer)))

	(let loop ((sent 0))
	  (if (< sent count)
	      (let ((left (output-stream-buffer-blob-left output-stream)))
		(if (positive? left)
		    (let ((to-copy (min left (- count sent)))
			  (buffer-count (output-stream-buffer-count output-stream)))
		      (blob-copy! blob (+ start sent)
				  buffer (+ (output-stream-buffer-start output-stream)
					    buffer-count)
				  to-copy)
		      (set-output-stream-buffer-count! output-stream
						       (+ buffer-count to-copy))
		      (loop (+ sent to-copy)))
		    (begin
		      (empty-buffer! output-stream)
		      (loop sent)))))))

      (let ((b (output-stream-buffer-mode output-stream)))
	(if (or (eq? (buffer-mode none) b)
		(and (eq? (buffer-mode line) b)
		     (blob-index blob 10 start (+ start count))))
	    (flush-output-stream output-stream)))))))

(define (output-u8 output-stream octet)
  (ensure-output-stream-open output-stream)
  (cond
   ((translated-output-stream? output-stream)
    (set-output-stream-translator-state!
     output-stream
     ((output-stream-translator-proc output-stream)
      (output-stream-base output-stream)
      (output-stream-translator-state output-stream)
      octet #f #f)))
   ((writer-output-stream? output-stream)
    (let* ((buffer (output-stream-buffer output-stream))
	   (buffer-size (blob-length buffer)))

      (let loop ()
	(if (positive? (output-stream-buffer-blob-left output-stream))
	    (let ((buffer-count (output-stream-buffer-count output-stream)))
	      (blob-u8-set! buffer
			    (+ (output-stream-buffer-start output-stream) buffer-count)
			    octet)
	      (set-output-stream-buffer-count! output-stream (+ 1 buffer-count)))
	    (begin
	      (empty-buffer! output-stream)
	      (loop)))))

    (let ((b (output-stream-buffer-mode output-stream)))
      (if (or (eq? (buffer-mode none) b)
	      (and (eq? (buffer-mode line) b)
		  (= octet 10)))
	  (flush-output-stream output-stream))))))

(define (output-char output-stream char)
  (let* ((blob (make-blob 4))
	 (octet-count (encode-char/utf-8 char blob 0)))
    (output-blob output-stream blob 0 octet-count)))
	      
(define (set-output-stream-buffer-mode! output-stream mode)
  (if (eq? (buffer-mode none) mode)
      (flush-output-stream output-stream))
  (really-set-output-stream-buffer-mode! output-stream mode))

(define (flush-writer-output-stream output-stream)
  (if (eq? (output-stream-status open)
	   (output-stream-current-status output-stream))
      (let ((new-position
	     (cond
	      ((output-stream-buffer-position output-stream)
	       => (lambda (pos)
		    (+ pos (output-stream-buffer-count output-stream))))
	      (else #f))))
			     
	(let loop ()
	  (if (positive? (output-stream-buffer-count output-stream))
	      (begin
		(empty-buffer! output-stream)
		(loop))))
	(set-output-stream-buffer-position! output-stream
					    new-position))))
  
(define flush-output-stream
  (let ((really-flush
	 (make-output-stream-proc flush-writer-output-stream)))
    (lambda (output-stream)
      (if (and (translated-output-stream? output-stream)
	       (eq? (output-stream-status open)
		    (output-stream-current-status output-stream)))
	  ((output-stream-translator-proc output-stream)
	   (output-stream-base output-stream)
	   (output-stream-translator-state output-stream)
	   #f #f #f))
      (really-flush output-stream))))

(define output-string
  (opt-lambda (output-stream
	       string 
	       (start 0) (count (- (string-length string) start)))
    (output-blob output-stream (string->utf-8-n string start count))))

(define (output-stream-position output-stream)
  (ensure-output-stream-open output-stream)
  (cond
   ((and (writer-output-stream? output-stream)
	 (output-stream-buffer-position output-stream))
    => (lambda (pos)
	 (+ pos (output-stream-buffer-count output-stream))))
   (else
    (raise
     (condition
      (&message
       (message
	"output-stream-position operation not available"))
      (&i/o-stream-error
       (stream output-stream))
      (&i/o-operation-not-available-error
       (operation output-stream-position)))))))

(define (set-output-stream-position! output-stream pos)
  (ensure-output-stream-open output-stream)
  (if (and (writer-output-stream? output-stream)
	   (writer-has-set-position!? (output-stream-writer output-stream)))
      (begin
	(flush-output-stream output-stream)
	(writer-set-position! (output-stream-writer output-stream) pos)
	(set-output-stream-buffer-position! output-stream pos))
      (raise
       (condition
	(&message
	 (message
	  "set-output-stream-position! operation not available"))
	(&i/o-stream-error
	 (stream output-stream))
	(&i/o-operation-not-available-error
	 (operation set-output-stream-position!))))))

(define (output-stream-underliers output-stream)
  (ensure-output-stream-open output-stream)
  (flush-output-stream output-stream)  
  (cond
   ((writer-output-stream? output-stream)
    (values (output-stream-writer output-stream)
	    #f #f))
   ((translated-output-stream? output-stream)
    (values (output-stream-base output-stream)
	    (output-stream-translator-proc output-stream)
	    (output-stream-translator-state output-stream)))))

(define (output-stream-writer+constructor output-stream)
  (ensure-output-stream-open output-stream)
  (flush-output-stream output-stream)  
  (cond
   ((writer-output-stream? output-stream)
    (set-writer-output-stream-current-status! output-stream (output-stream-status terminated))
    (values (output-stream-writer output-stream)
	    open-writer-output-stream))
   ((translated-output-stream? output-stream)
    (let ((translator-proc (output-stream-translator-proc output-stream))
	  (translator-state (output-stream-translator-state output-stream)))
      (call-with-values
	  (lambda () 
	    (output-stream-writer+constructor
	     (output-stream-base output-stream)))
	(lambda (writer base-construct)
	  (set-translated-output-stream-status! output-stream (output-stream-status terminated))
	  (values writer
		  (lambda (writer)
		    (make-translated-output-stream (base-construct writer)
						   translator-proc
						   translator-state)))))))))

(define (close-output-stream output-stream)
  (flush-output-stream output-stream)
  (let loop ((output-stream output-stream))
    (cond
     ((translated-output-stream? output-stream)
      (loop (output-stream-base output-stream))
      (set-translated-output-stream-status! output-stream
					    (output-stream-status closed)))
     ((writer-output-stream? output-stream)
      (if (not (eq? (output-stream-status closed)
		    (writer-output-stream-current-status output-stream)))
	  (begin
	    (writer-close (output-stream-writer output-stream))
	    (set-writer-output-stream-current-status! output-stream
						      (output-stream-status closed))))))))

(define open-file-output-stream
  (opt-lambda (filename (options (file-options)))
    (open-writer-output-stream (open-file-writer filename options)
			       (buffer-mode block))))

(define (call-with-blob-output-stream proc)
  (let* ((writer (open-blob-writer))
	 (stream
	  (open-writer-output-stream writer (buffer-mode none))))
    (proc stream)
    (writer-blob writer)))

(define (call-with-string-output-stream proc)
  (utf-8->string (call-with-blob-output-stream proc)
		 #\?))

(define (call-with-output-stream stream proc)
  (let ((results (call-with-values (lambda () (proc stream))
		   list)))
    (close-output-stream stream)
    (apply values results)))

(define (standard-output-stream)
  (open-writer-output-stream (standard-output-writer)
			     (buffer-mode line)))

(define (standard-error-stream)
  (open-writer-output-stream (standard-error-writer)
			     (buffer-mode none)))
