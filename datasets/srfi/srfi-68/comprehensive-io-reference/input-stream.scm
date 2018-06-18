; Implementation of input streams for Comprehensive I/O SRFI

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

; for invalid character encodings
(define *error-char* #\?)

(define the-empty-blob (make-blob 0))

; buffer objects
; Note that a buffer is part of a chain that is shared by all the input
; streams start were generated from the same starting stream.
(define-record-type :buffer
  (really-make-buffer position blob count
		      next)
  buffer?
  ;; may be #f if the stream is translated or the underlying reader
  ;; doesn't have GET-POSITION-PROC
  (position buffer-position)
  ;; may be #f
  (blob buffer-blob
	set-buffer-blob!)
  ;; may be #f
  (count buffer-count
	 set-buffer-count!)
  (next really-buffer-next set-buffer-next!))

(define (make-buffer position blob count)
  (really-make-buffer position blob count #f))

; make a buffer that has yet to be filled
(define (make-virgin-buffer position)
  (make-buffer position #f #f))

; check if a buffer is virgin
(define (buffer-virgin? buffer)
  (not (buffer-blob buffer)))

; get the next buffer; create it if necessary
(define (buffer-next buffer)
  (or (really-buffer-next buffer)
      (let* ((next-position
	      (cond
	       ((buffer-position buffer)
		=> (lambda (pos)
		     (+ pos (buffer-count buffer))))
	       (else #f)))
	     (next (make-virgin-buffer next-position)))
	(set-buffer-next! buffer next)
	next)))

; translator objects
(define-record-type :input-stream-translator
  (make-input-stream-translator base proc)
  input-stream-translator?
  (base input-stream-translator-base
	set-input-stream-translator-base!)

  ;; input-stream wish -> (blob input-stream)
  ;; where wish is either
  ;; - #f (arbitary chunk) (should call INPUT-BLOB-SOME)
  ;; - #t as much as possible (should call INPUT-ALL)
  ;; - an integer hint as to how much the client wants (should call INPUT-N)
  (proc input-stream-translator-proc))

(define-enumerated-type input-stream-status :input-stream-status
  input-stream-status?
  input-stream-statuses
  input-stream-status-name
  input-stream-status-index
  (open truncated closed))

; input streams
(define-record-type :input-stream
  (really-make-input-stream status-cell underlying buffer buffer-start)
  input-stream?
  (status-cell input-stream-status-cell)
  ;; either an :INPUT-STREAM-STATE or an :INPUT-STREAM-TRANSLATOR
  (underlying input-stream-underlying)
  (buffer input-stream-buffer
	  set-input-stream-buffer!)
  (buffer-start input-stream-buffer-start
		set-input-stream-buffer-start!))

(define (make-input-stream status-cell underlying buffer buffer-start)
  (really-make-input-stream status-cell
			    underlying buffer buffer-start))


(define (input-stream-current-status input-stream)
  (cell-ref (input-stream-status-cell input-stream)))

(define (set-input-stream-current-status! input-stream status)
  (cell-set! (input-stream-status-cell input-stream)
	     status))

; return the size of the buffer data associated with a stream
(define (input-stream-buffer-size input-stream)
  (- (or (buffer-count (input-stream-buffer input-stream)) 0)
     (input-stream-buffer-start input-stream)))

; copy the buffer data associated with a stream to a fresh octet vector
(define (input-stream-copy-buffer input-stream)
  (let ((buffer (input-stream-buffer input-stream))
	(output-blob (make-blob (input-stream-buffer-size input-stream)))
	(start (input-stream-buffer-start input-stream)))
    (blob-copy! (buffer-blob buffer)
		start
		output-blob 0
		(- (buffer-count (input-stream-buffer input-stream)) start))
    output-blob))

; if the buffer associated with a stream is virgin, fill it
(define (maybe-fill-input-stream! input-stream)
  (let ((buffer (input-stream-buffer input-stream)))
    (cond
     ((not (buffer-virgin? buffer))
      (unspecific))
     ((eq? (input-stream-status truncated)
	   (input-stream-current-status input-stream))
      ;; normalize
      (set-buffer-blob! buffer the-empty-blob)
      (set-input-stream-buffer-start! input-stream 0)
      (set-buffer-count! buffer 0))
     (else
      (fill-buffer-from-underlying! buffer
				    (input-stream-underlying input-stream)
				    #f)
      (set-input-stream-buffer-start! input-stream 0)))))
						 
; fill buffer with data from the underlying
(define (fill-buffer-from-underlying! buffer underlying wish)
  (call-with-values
      (lambda () (milk-underlying underlying wish))
    (lambda (blob count)
      (if (zero? count)
	  (make-buffer-eof! buffer)
	  (begin
	    (set-buffer-blob! buffer blob)
	    (set-buffer-count! buffer count))))))

; mark a buffer as denoting EOF
(define (make-buffer-eof! buffer)
  (set-buffer-blob! buffer the-empty-blob)
  (set-buffer-count! buffer 0))

(define (buffer-eof? buffer)
  (zero? (blob-length (buffer-blob buffer))))

; get data from the underlying of the stream
; returns buffer and octet count in buffer

; If we could truncate the octet vector after the fact, we could
; probably save some space.
; WISH may be
; - a number
; - #f if we don't care
; - #t as much as possible

(define (milk-underlying underlying wish)
  (cond
   ((reader? underlying)
    (milk-reader underlying wish))
   ((input-stream-translator? underlying)
    (milk-translator underlying wish))))

(define (milk-reader reader wish)
   (let* ((chunk-size (cond
		       ((and (number? wish) (reader-available reader))
			=> (lambda (available)
			     (min wish available)))
		       ((and wish (reader-available reader)))
		       (else
			(reader-chunk-size reader))))
	  (blob (make-i/o-buffer chunk-size))
	  (count (reader-read! reader blob 0 chunk-size)))
     (values blob count)))

(define (milk-translator translator wish)
  (let ((base (input-stream-translator-base translator)))
    (call-with-values
	(lambda ()
	  ((input-stream-translator-proc translator) base wish))
      (lambda (stuff new-base)
	(set-input-stream-translator-base! translator new-base)
	(if (not stuff)
	    (values stuff 0)
	    (values stuff (blob-length stuff)))))))

; raise a condition if a the stream is not open
(define (ensure-input-stream-open input-stream)
  (if (not (eq? (input-stream-status open)
		(input-stream-current-status input-stream)))
      (raise
       (condition
	(&message
	 (message
	  "input stream closed"))
	(&i/o-stream-error
	 (stream input-stream))
	(&i/o-closed-error)))))

; copy stream data into a fresh octet vector of size RESULT-SIZE
; assumes that RESULT-SIZE blob are actually available
; ####note it has significant overlap with INPUT-STREAM-SKIP
(define (input-from-buffers input-stream result result-start result-size)

  (let loop ((buffer (input-stream-buffer input-stream))
	     (start (input-stream-buffer-start input-stream))
	     (index 0))

    (if (>= index result-size)
	(values result
		(make-input-stream (input-stream-status-cell input-stream)
				   (input-stream-underlying input-stream)
				   buffer start))
	(let* ((available-count (- (buffer-count buffer) start))
	       (wanted-count (- result-size index))
	       (chunk-size (min wanted-count available-count)))
	    
	  (blob-copy! (buffer-blob buffer) start
		      result (+ result-start index)
		      chunk-size)

	  (if (> available-count wanted-count)
	      (values result
		      (make-input-stream (input-stream-status-cell input-stream)
					 (input-stream-underlying input-stream)
					 buffer (+ start wanted-count)))
	      (loop (buffer-next buffer)
		    0 (+ index chunk-size)))))))

; try to fill buffers according to WISH
; WISH may be either a number or #t if we want to exhaust the reader
; returns count of blob actually retrieved
(define (try-fill-buffers! input-stream wish)
  (let ((underlying (input-stream-underlying input-stream))
	(truncated? (eq? (input-stream-status truncated)
			 (input-stream-current-status input-stream))))
    (let loop ((buffer (input-stream-buffer input-stream))
	       (total-size
		(- (input-stream-buffer-start input-stream)))) ; not including BUFFER
      (if (and (number? wish)
	       (>= total-size wish))
	  total-size
	  (begin
	    (if (buffer-virgin? buffer)
		(if truncated?
		    (make-buffer-eof! buffer)
		    (fill-buffer-from-underlying! buffer underlying
						  (if (number? wish)
						      (- wish total-size)
						      #t))))
	    (let* ((count (buffer-count buffer))
		   (total-size (+ total-size count)))
		      
	      (if (positive? count)
		  (loop (buffer-next buffer) total-size)
		  total-size)))))))

; return a stream n blob from INPUT-STREAM 
; this assumes that there are actually n blob in the buffers
; ####note it has significant overlap with INPUT-FROM-BUFFERS
(define (input-stream-skip input-stream n)
  (let loop ((buffer (input-stream-buffer input-stream))
	     (start (input-stream-buffer-start input-stream))
	     (index 0))
    
    (if (>= index n)
	(make-input-stream (input-stream-status-cell input-stream)
			   (input-stream-underlying input-stream)
			   buffer start)
	(let* ((available-count (- (buffer-count buffer) start))
	       (wanted-count (- n index))
	       (chunk-size (min wanted-count available-count)))
	  
	  (if (> available-count wanted-count)
	      (make-input-stream (input-stream-status-cell input-stream)
				 (input-stream-underlying input-stream)
				 buffer (+ start wanted-count))
	      (loop (buffer-next buffer)
		    0 (+ index chunk-size)))))))

; skip past the buffer INPUT-STREAM points at
(define (input-stream-next input-stream)
  (make-input-stream (input-stream-status-cell input-stream)
		     (input-stream-underlying input-stream)
		     (buffer-next (input-stream-buffer input-stream)) 0))

(define (input-blob-some input-stream)
  (ensure-input-stream-open input-stream)
  (maybe-fill-input-stream! input-stream)
  (values (if (buffer-eof? (input-stream-buffer input-stream))
	      #f
	      (input-stream-copy-buffer input-stream))
	  (input-stream-next input-stream)))

(define (input-string input-stream)
  (call-with-values
      (lambda () (input-blob-some input-stream))
    (lambda (blob new-input-stream)
      (if (not blob)
	  (values #f new-input-stream)
	  (let loop ((repeat? #f)
		     (blob blob))
	    (call-with-values
		(lambda ()
		  (bytes-string-size/utf-8 blob 0 (blob-length blob) #f))
	      (lambda (status consumed char-count)
		(cond
		 ((not (zero? char-count))
		  (let ((target (make-string char-count)))
		    (decode-string/utf-8 blob
					 0 (blob-length blob)
					 target 0 char-count
					 *error-char*)
		    (values target
			    (input-stream-skip input-stream consumed))))
		 (repeat?
		  ;; incomplete encoding
		  (values (string *error-char*) (input-stream-skip input-stream 1)))
		 (else
		  (call-with-values 
		      (lambda () (input-blob-n input-stream 4))
		    (lambda (blob new-input-stream)
		      (loop #t blob))))))))))))

(define (input-blob-n input-stream n)
  (ensure-input-stream-open input-stream)
  (if (zero? n)
      (values the-empty-blob input-stream)
      (let ((available-count (try-fill-buffers! input-stream n)))
	(if (zero? available-count)
	    (values #f
		    (input-stream-next input-stream))
	    (let ((result-size (min available-count n)))
	      (input-from-buffers input-stream
				  (make-blob result-size)
				  0 result-size))))))

(define (input-blob-n! input-stream blob start n)
  (ensure-input-stream-open input-stream)
  (if (zero? n)
      (values 0 input-stream)
      (let ((available-count (try-fill-buffers! input-stream n)))
	(if (zero? available-count)
	    (values #f
		    (input-stream-next input-stream))
	    (let ((result-size (min available-count n)))
	      (call-with-values
		  (lambda ()
		    (input-from-buffers input-stream blob start result-size))
		(lambda (_ stream)
		  (values result-size stream))))))))

(define (input-blob-all input-stream)
  (ensure-input-stream-open input-stream)
  (let ((available-count (try-fill-buffers! input-stream #t)))
    (if (zero? available-count)
	(values #f
		(input-stream-next input-stream))
	(input-from-buffers input-stream
			    (make-blob available-count)
			    0 available-count))))

(define (input-string-all input-stream)
  (call-with-values
      (lambda () (input-blob-all input-stream))
    (lambda (blob new-input-stream)
      (if (not blob)
	  (values #f new-input-stream)
	  (call-with-values
	      (lambda () (bytes-string-size/utf-8 blob 0 (blob-length blob) #f))
	    (lambda (status consumed char-count)

	      (cond
	       ((eq? status (decoding-status complete))
		(let ((target (make-string char-count)))
		  (decode-string/utf-8 blob
				       0 (blob-length blob)
				       target 0 char-count
				       *error-char*)
		  (values target new-input-stream)))
	       ((eq? status (decoding-status incomplete))
		(let* ((error-char-count (- (blob-length blob) consumed))
		       (target (make-string (+ char-count error-char-count))))
		  (decode-string/utf-8 blob
				       0 (blob-length blob)
				       target 0 char-count
				       *error-char*)
		  (do ((i 0 (+ 1 i)))
		      ((>= i error-char-count))
		    (string-set! target (+ char-count i) *error-char*))
		  (values target new-input-stream))))))))))

(define (input-string-n input-stream n)
  (input-string-n/generalized "" string-concatenate-reverse
			      input-stream n))

(define (input-string-n! input-stream blob start n)
  (input-string-n/generalized 0
			      (lambda (rev-strings)
				(strings-copy! (reverse rev-strings) blob start))

			      input-stream n))

(define (input-string-n/generalized zero make-result
				    input-stream n)
  (if (zero? n)
      (values zero input-stream)
      (let loop ((input-stream input-stream) (rev-strings '()) (count 0))
	(if (= count n)
	    (values (make-result rev-strings)
		    input-stream)
	    (call-with-values
		(lambda () (input-string input-stream))
	      (lambda (portion new-input-stream)
		(cond
		 ((not portion)		; EOF
		  (if (null? rev-strings)
		      (values #f new-input-stream)
		      (values (make-result rev-strings)
			      input-stream)))
		 ((> (+ count (string-length portion)) n)
		  (let* ((wanted-count (- n count))
			 (wanted (substring portion 0 wanted-count)))
		    (values (make-result (cons wanted rev-strings))
			    (input-stream-skip input-stream
					       (string-encoding-length/utf-8 wanted
									     0 wanted-count)))))
		 (else
		  (loop new-input-stream
			(cons portion rev-strings)
			(+ count (string-length portion)))))))))))

; returns number of characters copied
(define (strings-copy! strings target start)
  (let loop ((index start) (strings strings))
    (if (null? strings)
	(- index start)
	(begin
	  (string-copy! target index
			(car strings))
	  (loop (+ index (string-length (car strings)))
		(cdr strings))))))
	
;; note that, unlike in the SML basis, this *can* be used to read past EOF
(define (input-u8 input-stream)
  (ensure-input-stream-open input-stream)
  (maybe-fill-input-stream! input-stream)
  (if (zero? (input-stream-buffer-size input-stream))
      (values #f
	      (input-stream-next input-stream))
      (values (blob-u8-ref (buffer-blob (input-stream-buffer input-stream))
			   (input-stream-buffer-start input-stream))
	      (input-stream-skip input-stream 1))))

(define (input-char input-stream)
  (ensure-input-stream-open input-stream)
  (maybe-fill-input-stream! input-stream)
  (if (buffer-eof? (input-stream-buffer input-stream))
      (values #f
	      (input-stream-next input-stream))
      (let ((buffer (input-stream-buffer input-stream))
	    (start (input-stream-buffer-start input-stream)))
	(let ((blob (buffer-blob buffer))
	      (total-count (buffer-count buffer)))
	  (call-with-values
	      (lambda () (decode-char/utf-8 blob start (- total-count start)))
	    (lambda (status c consumed)
	      (cond
	       ((eq? status (decoding-status complete))
		(values c
			(input-stream-skip input-stream consumed)))
	       ((eq? status (decoding-status invalid))
		(values *error-char*
			(input-stream-skip input-stream-skip 1)))
	       ((eq? status (decoding-status incomplete))
		(call-with-values
		    (lambda () (input-blob-n input-stream 4))
		  (lambda (blob new-input-stream)
		    (call-with-values
			(lambda () (decode-char/utf-8 blob 0 (blob-length blob)))
		      (lambda (status c consumed)
			(cond
			 ((eq? status (decoding-status complete))
			  (values c
				  (input-stream-skip input-stream consumed)))
			 ((eq? status (decoding-status invalid))
			  (values *error-char*
				  (input-stream-skip input-stream-skip 1)))
			 ((eq? status (decoding-status incomplete)) ; EOF before encoding ends
			  (values *error-char*
				  (input-stream-skip input-stream-skip 1))))))))))))))))

(define (input-line input-stream)

  (define (get-line count rest-stream)
    (call-with-values
	(lambda () (input-blob-n input-stream count))
      (lambda (blob new-stream)
	(values (if blob
		    (utf-8->string blob *error-char*)
		    #f)
		rest-stream))))

  (let loop ((input-stream input-stream) (count 0))
    (call-with-values
	(lambda () (input-blob-some input-stream))
      (lambda (chunk new-stream)
	(cond
	 (chunk
	  (let ((size (blob-length chunk)))
	    (cond
	     ((blob-index chunk 10 0 size) ; 10's for newline
	      => (lambda (index)
		   (get-line (+ count index)
			     (input-stream-skip input-stream (+ 1 index)))))
	     (else
	      (loop new-stream (+ count size))))))
	 ((zero? count)
	  (values #f new-stream))
	 (else
	  (get-line count input-stream)))))))

(define (blob-index octet-vector octet start end)
  (let loop ((index start))
    (cond
     ((>= index end)
      #f)
     ((= octet (blob-u8-ref octet-vector index))
      index)
     (else
      (loop (+ 1 index))))))

(define (stream-eof? input-stream)
  (call-with-values
      (lambda () (input-u8 input-stream))
    (lambda (octet input-stream)
      (not octet))))

(define (input-stream-position input-stream)
  (ensure-input-stream-open input-stream)
  (let ((buffer (input-stream-buffer input-stream)))
    (cond
     ((buffer-position buffer)
      => (lambda (pos)
	   (+ pos (input-stream-buffer-start input-stream))))
     (else
      (raise
       (condition
	(&message
	 (message "input-stream-position operation not available"))
	(&i/o-stream-error 
	 (stream input-stream))
	(&i/o-operation-not-available-error
	 (operation input-stream-position))))))))

(define (input-stream-underliers input-stream)
  (ensure-input-stream-open input-stream)
  (set-input-stream-current-status! input-stream (input-stream-status truncated))
  (let ((underlying (input-stream-underlying input-stream)))
    (cond
     ((reader? underlying)
      (values underlying
	      #f))
     ((input-stream-translator? underlying)
      (values (input-stream-translator-base underlying)
	      (input-stream-translator-proc underlying))))))

(define (input-stream-reader+constructor input-stream)
  (ensure-input-stream-open input-stream)
  (let ((underlying (input-stream-underlying input-stream)))
    (set-input-stream-current-status! input-stream (input-stream-status truncated))
    (cond
     ((reader? underlying)
      (values underlying open-reader-input-stream))
     ((input-stream-translator? underlying)
      (let ((translate-proc (input-stream-translator-proc underlying)))
	(call-with-values
	    (lambda ()
	      (input-stream-reader+constructor (input-stream-translator-base underlying)))
	  (lambda (reader base-construct)
	    (values reader
		    (lambda (reader)
		      (make-translated-input-stream (base-construct reader)
						    translate-proc))))))))))

(define (close-input-stream input-stream)
  (if (not (eq? (input-stream-status closed)
		(input-stream-current-status input-stream)))
      (let ((underlying (input-stream-underlying input-stream)))
	(cond
	 ((reader? underlying)
	  (reader-close underlying))
	 ((input-stream-translator? underlying)
	  (close-input-stream (input-stream-translator-base underlying))))
	(set-input-stream-current-status! input-stream (input-stream-status closed)))))

(define open-file-input-stream
  (opt-lambda (filename (options (file-options)))
    (open-reader-input-stream (open-file-reader filename options))))

(define (open-blob-input-stream blob)
  (open-reader-input-stream (open-blob-reader blob)))

(define (open-string-input-stream string)
  (open-reader-input-stream (open-blob-reader (string->utf-8 string))))

(define open-reader-input-stream
  (opt-lambda (reader (buffer-contents #f))
    (let ((pos
	   (if (reader-has-get-position? reader)
	       (reader-get-position reader)
	       #f)))
      (make-input-stream (make-cell (input-stream-status open))
			 reader
			 (if buffer-contents
			     (make-buffer pos
					  buffer-contents (blob-length buffer-contents))
			     (make-virgin-buffer pos))
			 0))))

(define make-translated-input-stream
  (opt-lambda (input-stream translate-proc (buffer-contents #f))
    (make-input-stream (make-cell (input-stream-status open))
		       (make-input-stream-translator input-stream translate-proc)
		       (if buffer-contents
			   (make-buffer #f
					buffer-contents (blob-length buffer-contents))
			   (make-virgin-buffer #f))
		       0)))

(define (call-with-input-stream stream proc)
  (let ((results (call-with-values (lambda () (proc stream))
		   list)))
    (close-input-stream stream)
    (apply values results)))

(define (standard-input-stream)
  (open-reader-input-stream (standard-input-reader)))
