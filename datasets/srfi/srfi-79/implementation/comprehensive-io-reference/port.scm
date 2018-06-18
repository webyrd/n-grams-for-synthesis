; Implementation of ports for Comprehensive I/O SRFI

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

(define-condition-type &i/o-port-error &i/o-error
  i/o-port-error?
  (port i/o-error-port))

;; Input ports

(define-record-type :stream-input-port
  (make-stream-input-port stream)
  stream-input-port?
  (stream input-port-stream set-input-port-stream!))

(define input-port? stream-input-port?)

(define (standard-input-port)
  (standard-input-stream))

(define (input-stream->input-port input-stream maybe-transcoder)
  (make-stream-input-port
   (if maybe-transcoder
       (transcode-input-stream input-stream maybe-transcoder)
       input-stream)))

(define (transcode-input-port! port transcoder)
  (set-input-port-stream! port
			  (transcode-input-stream (input-port-stream port)
						  transcoder)))

(define-syntax read-proc
  (syntax-rules ()
    ((read-proc input-proc arg1 ...)
     (lambda (input-port arg1 ...)
       (call-with-values
	   (lambda () (input-proc (input-port-stream input-port) arg1 ...))
	 (lambda (stuff new-stream)
	   (set-input-port-stream! input-port new-stream)
	   stuff))))))

(define read-blob-some (read-proc input-blob-some))
(define read-u8 (read-proc input-u8))
(define read-blob-all (read-proc input-blob-all))
(define read-blob-n (read-proc input-blob-n n))
(define read-blob-n! (read-proc input-blob-n blob start count))
(define read-char (read-proc input-char))
(define read-string (read-proc input-string))
(define read-string-all (read-proc input-string-all))
(define read-string-n (read-proc input-string-n n))
(define read-string-n! (read-proc input-string-n! blob start n))
(define read-line (read-proc input-line))

(define (make-peek-proc input-proc)
  (lambda (input-port)
    (call-with-values
	(lambda () (input-proc (input-port-stream input-port)))
      (lambda (stuff new-stream)
	stuff))))

(define peek-u8 (make-peek-proc input-u8))
(define peek-char (make-peek-proc input-char))

(define (port-eof? input-port)
  (stream-eof? (input-port-stream input-port)))

(define (close-input-port input-port)
  (close-input-stream (input-port-stream input-port)))

(define (input-port-position input-port)
  (input-stream-position (input-port-stream input-port)))

(define (set-input-port-position! input-port pos)
  (call-with-values
      (lambda ()
	(input-stream-reader+constructor (input-port-stream input-port)))
    (lambda (reader make-stream)
      (if (not (reader-has-set-position!? reader))
	      (raise
	       (condition
		(&message
		 (message "set-input-port-position! operation not available"))
		(&i/o-operation-not-available-error
		 (operation set-input-port-position!))
		(&i/o-reader/writer-error
		 (reader/writer reader))
		(&i/o-port-error
		 (port input-port)))))
      (reader-set-position! reader pos)
      (set-input-port-stream! input-port (make-stream reader)))))

(define open-file-input-port
  (opt-lambda (filename (options (file-options)) (transcoder #f))
    (input-stream->input-port (open-file-input-stream filename)
			      transcoder)))

(define open-blob-input-port
  (opt-lambda (blob (transcoder #f))
    (input-stream->input-port (open-blob-input-stream blob)
			      transcoder)))

(define open-string-input-port
  (opt-lambda (string (transcoder #f))
    (input-stream->input-port (open-string-input-stream string)
			      transcoder)))

(define open-reader-input-port
  (opt-lambda (reader (transcoder #f))
    (input-stream->input-port (open-reader-input-stream reader)
			      transcoder)))

(define (call-with-input-port port proc)
  (let ((results (call-with-values (lambda () (proc port))
		   list)))
    (close-input-port port)
    (apply values results)))

;; Output ports

(define-record-type :stream-output-port
  (make-stream-output-port stream)
  stream-output-port?
  (stream output-port-stream
	  set-output-port-stream!))

(define output-port? stream-output-port?)

(define *standard-output-port* 
  (make-stream-output-port (standard-output-stream)))

(define (standard-output-port)
  *standard-output-port*)

(define (output-stream->output-port output-stream maybe-transcoder)
  (make-stream-output-port
   (if maybe-transcoder
       (transcode-output-stream output-stream maybe-transcoder)
       output-stream)))

(define (transcode-output-port! port transcoder)
  (set-output-port-stream! port
			  (transcode-output-stream (output-port-stream port)
						   transcoder)))

(define *standard-error-port*
  (make-stream-output-port (standard-error-stream)))

(define (standard-error-port)
  *standard-error-port*)

(define-syntax write-proc
  (syntax-rules ()
    ((write-proc output-proc arg1 ...)
     (lambda (output-port arg1 ...)
       (output-proc (output-port-stream output-port) arg1 ... )))))

(define-syntax write-proc-with-optionals
  (syntax-rules ()
    ((write-proc-with-optionals output-proc arg1 ...)
     (lambda (output-port arg1 ... . rest)
       (apply output-proc (output-port-stream output-port) arg1 ... rest)))))

; renamed from R5RS WRITE-<stuff>
(define write-blob (write-proc-with-optionals output-blob blob ))
(define write-u8 (write-proc output-u8 octet))
(define write-string (write-proc-with-optionals output-string string))
(define write-char (write-proc output-char char))

(define (flush-output-port output-port)
  (flush-output-stream (output-port-stream output-port)))

(define (output-port-buffer-mode output-stream)
  (output-stream-buffer-mode (output-port-stream output-stream)))

(define (set-output-port-buffer-mode! output-stream mode)
  (set-output-stream-buffer-mode! (output-port-stream output-stream)
				  mode))

(define (close-output-port output-port)
  (close-output-stream (output-port-stream output-port)))

(define (output-port-position output-port)
  (output-stream-position (output-port-stream output-port)))

(define (set-output-port-position! output-port pos)
  (set-output-stream-position! (output-port-stream output-port) pos))

(define open-file-output-port
  (opt-lambda (filename (options (file-options)) (transcoder #f))
    (output-stream->output-port (open-file-output-stream filename options)
				transcoder)))

(define open-writer-output-port
  (opt-lambda (writer buffer-mode (transcoder #f))
    (output-stream->output-port (open-writer-output-stream writer buffer-mode)
				transcoder)))

(define call-with-blob-output-port
  (opt-lambda (proc (transcoder #f))
    (call-with-blob-output-stream
     (lambda (output-stream)
       (proc 
	(output-stream->output-port output-stream transcoder))))))

(define call-with-string-output-port
  (opt-lambda (proc (transcoder #f))
    (call-with-string-output-stream
     (lambda (output-stream)
       (proc
	(output-stream->output-port output-stream transcoder))))))

(define (call-with-output-port port proc)
  (let ((results (call-with-values (lambda () (proc port))
		   list)))
    (close-output-port port)
    (apply values results)))

(define (newline output-port)
  (write-char output-port #\newline))

(define open-file-input+output-ports
  (opt-lambda (filename (options (file-options)) (transcoder #f))
    (call-with-values
	(lambda () (open-file-input+output-streams filename options))
      (lambda (input-stream output-stream)
	(values (input-stream->input-port input-stream transcoder)
		(output-stream->output-port output-stream transcoder))))))

