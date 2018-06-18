; Some examples for Comprehensive I/O SRFI

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

; The CHECK macro is essentially stolen from Sebastian Egner's SRFIs.

(define *correct-count* 0)
(define *failed-count* 0)

(define-syntax check
  (syntax-rules (=>)
    ((check ec => desired-result)
     (check ec => (equal?) desired-result))
    ((check ec => (equal?) desired-result)
     (begin
       (r5rs:newline)
       (r5rs:write (quote ec))
       (r5rs:newline)
       (let ((actual-result ec))
         (r5rs:display "  => ")
         (r5rs:write actual-result)
         (if (equal? actual-result desired-result)
             (begin
               (r5rs:display " ; correct")
               (set! *correct-count* (+ *correct-count* 1)) )
             (begin
               (r5rs:display " ; *** failed ***, desired result:")
               (r5rs:newline)
               (r5rs:display "  => ")
               (r5rs:write desired-result)
               (set! *failed-count* (+ *failed-count* 1)) ))
         (r5rs:newline) )))))

; defining your own customized reader:

; This one reads from a list of octet vectors.  A null octet vector
; yields EOF.
(define (open-blobs-reader bs)
  (let* ((pos 0))
		   
    (make-simple-reader
     "<octet vectors>"
     bs
     5					; for debugging
     (lambda (blob start count)
       (cond
	((null? bs)
	 0)
	(else
	 (let* ((b (car bs))
		(size (blob-length b))
		(real-count (min count (- size pos))))
	   (blob-copy! b pos
			   blob start
			   real-count)
	   (set! pos (+ pos real-count))
	   (if (= pos size)
	       (begin
		 (set! bs (cdr bs))
		 (set! pos 0)))
	   real-count))))
     ;; pretty rough ...
     (lambda ()
       (if (null? bs)
	   0
	   (- (blob-length (car bs)) pos)))
     #f #f #f				; semantics would be unclear
     (lambda ()
       (set! bs #f)))))			; for GC

(define (open-strings-reader strings)
  (open-blobs-reader (map string->utf-8 strings)))

(define (open-blobs-input-stream blobs)
  (open-reader-input-stream (open-blobs-reader blobs)))

(define (open-strings-input-stream strings)
  (open-reader-input-stream (open-blobs-reader (map string->utf-8 strings))))
 
(define three-lines-string
  (call-with-string-output-port
   (lambda (port)
     (write-string port "foo") (newline port)
     (write-string port "bar") (newline port)
     (write-string port "baz") (newline port))))

; Stream I/O

; from Reppy:
; Note the threading of the stream.
(define (input-two-lines s)
  (let*-values (((line-1 s-2) (input-line s))
		((line-2 _)   (input-line s-2)))
    (values line-1 line-2)))

(check (call-with-input-stream (open-string-input-stream three-lines-string)
         (lambda (s)
	   (call-with-values (lambda () (input-two-lines s)) cons)))
       => (cons "foo" "bar"))

; from Reppy:

; There may be life after end of file; hence, the following is not
; guaranteed to return true
(define (at-end?/broken s)
  (let ((z (stream-eof? s)))
    (let-values (((a s-2) (input-blob-some s)))
      (let ((x (stream-eof? s-2)))
	(equal? z x)))))

(check (call-with-input-stream
	(open-reader-input-stream
	 (open-strings-reader '("foo" "" "baz")))
	(lambda (s)
	  (let-values (((text s-2) (input-string-n s 3)))
	    (at-end?/broken s-2))))
       => #f)

; ... but this is
(define (at-end? s)
  (let ((z (stream-eof? s)))
    (let-values (((a s-2) (input-blob-some s)))
      (let ((x (stream-eof? s)))
	(equal? z x)))))
  
(check (call-with-input-stream
	(open-reader-input-stream
	 (open-strings-reader '("foo" "" "baz")))
	(lambda (s)
	  (let-values (((text s-2) (input-string-n s 3)))
	    (at-end? s-2))))
       => #t)

; from Reppy:
(define (open-it filename)
  (guard
   (condition
    ((i/o-error? condition)
     (if (message-condition? condition)
	 (begin
	   (write-string (standard-error-port)
			 (condition-message condition))
	   (newline (standard-error-port))))
     #f))
   (open-file-input-stream filename)))

(check (open-it "/foo/bar/bla/baz")
       => #f)

(call-with-output-port (open-file-output-port "test-file" (file-options truncate create))
  (lambda (port)
    (write-string port "foo")
    (newline port)))

; Imperative I/O

; from Reppy:

; read all directly
(define (get-contents filename)
  (call-with-input-port (open-file-input-port filename)
    read-blob-all))

(check (blob->u8-list (get-contents "test-file"))
       => '(102 111 111 10))

; read all octet-by-octet
(define (get-contents-2 filename)
  (call-with-input-port (open-file-input-port filename)
    (lambda (port)
      (let loop ((accum '()))
	(let ((thing (read-u8 port)))
	  (if (not thing)
	      (list->blob (reverse accum))
	      (loop (cons thing accum))))))))

(define (list->blob l)
  (let ((blob (make-blob (length l))))
    (let loop ((i 0) (l l))
      (if (null? l)
	  blob
	  (begin
	    (blob-u8-set! blob i (car l))
	    (loop (+ 1 i) (cdr l)))))))

(check (blob->u8-list (get-contents-2 "test-file"))
       => '(102 111 111 10))

; read all chunk-by-chunk
(define (get-contents-3 filename)
  (call-with-input-port (open-file-input-port filename)
    (lambda (port)
      (let loop ((accum '()))
	(cond
	 ((read-blob-some port)
	  => (lambda (blob)
	       (loop (cons blob accum))))
	 (else
	  (concatenate-blobs (reverse accum))))))))

(define (concatenate-blobs list)
  (let* ((size (fold + 0 (map blob-length list)))
	 (result (make-blob size)))
    (let loop ((index 0)
	       (blobs list))
      (if (null? blobs)
	  result
	  (let* ((b (car blobs))
		 (size (blob-length b)))
	    (blob-copy! b 0 result index size)
	    (loop (+ index size)
		  (cdr blobs)))))))

(check (blob->u8-list (get-contents-3 "test-file"))
       => '(102 111 111 10))

; Stream I/O

; from Reppy:

; read all directly
(define (get-contents/stream filename)
  (call-with-input-stream (open-file-input-stream filename)
    (lambda (stream)
      (let-values (((blob _) (input-blob-all stream)))
	blob))))

(check (blob->u8-list (get-contents/stream "test-file"))
       => '(102 111 111 10))

; read all octet-by-octet
(define (get-contents/stream-2 filename)
  (call-with-input-stream (open-file-input-stream filename)
    (lambda (stream)
      (let loop ((accum '()) (stream stream))
	(let-values (((octet stream) (input-u8 stream)))
	  (if (not octet)
	      (list->blob (reverse accum))
	      (loop (cons octet accum) stream)))))))
		      
(check (blob->u8-list (get-contents/stream-2 "test-file"))
       => '(102 111 111 10))

; read all chunk-by-chunk
(define (get-contents/stream-3 filename)
  (call-with-input-stream (open-file-input-stream filename)
    (lambda (stream)
      (let loop ((accum '()) (stream stream))
	(let-values (((chunk stream) (input-blob-some stream)))
	  (if chunk
	      (loop (cons chunk accum) stream)
	      (concatenate-blobs (reverse accum))))))))

(check (blob->u8-list (get-contents/stream-3 "test-file"))
       => '(102 111 111 10))

; from Reppy:

(define (eat-thousand stream)
  (let-values (((text new-stream)
		(input-string-n stream (string-length "thousand"))))
    (if (string=? text "thousand")
	new-stream
	stream)))

(check (call-with-values
	   (lambda ()
	     (input-string-all (eat-thousand (open-string-input-stream "thousand and one"))))
	 (lambda (all rest)
	   all))
       => " and one")

(check (call-with-values
	   (lambda ()
	     (input-string-all (eat-thousand (open-string-input-stream "hundred and one"))))
	 (lambda (all rest)
	   all))
       => "hundred and one")

; from Reppy:

(define (skip-whitespace stream)
  (let-values (((thing new-stream)
		(input-char stream)))
    (cond
     ((not thing) stream)
     ((char-whitespace? thing)
      (skip-whitespace new-stream))
     (else stream))))

(check (call-with-values
	   (lambda ()
	     (input-string-all (skip-whitespace (open-string-input-stream "  ... and now"))))
	 (lambda (all rest)
	   all))
       => "... and now")

; from Reppy:

(define (my-input-line stream)
  (let count ((n 0) (g stream))
    (let-values (((thing g*) (input-char g)))
      (cond
       ((not thing)
	(if (zero? n)
	    (values #f g*)
	    (input-string-n stream n)))
       ((char=? #\newline thing)
	(let*-values (((line _) (input-string-n stream n)))
	  (values line g*)))
       (else
	(count (+ 1 n) g*))))))


(check (call-with-input-stream (open-file-input-stream "test-file")
         (lambda (stream)
	   (let-values (((line rest) (my-input-line stream)))
	     line)))
       => "foo")

; from Reppy
(define (hello myfile)
  (call-with-output-stream (open-file-output-stream myfile (file-options truncate create))
    (lambda (stream)
      (output-string stream "Hello, ")
      (output-string stream "world!")
      (output-char stream #\newline))))

(check (begin
	 (hello "test-file-2")
	 (call-with-input-stream (open-file-input-stream "test-file-2")
           (lambda (stream)
	     (let-values (((contents _) (input-string-all stream)))
	       contents))))
       => (string-append "Hello, world!" (string #\newline)))

; extract the reader, read 1 octet from it, and then reconstruct a stream from it
(define (after-first filename)
  (let ((stream (open-file-input-stream filename)))
    (call-with-values
	(lambda () (input-stream-reader+constructor stream))
      (lambda (reader construct)
	(let ((b (make-blob 1)))
	  (reader-read! reader b 0 1)
	  (call-with-input-stream (construct reader)
            (lambda (stream-2)
	      (let-values (((contents _) (input-string-all stream-2)))
		contents))))))))

(check (after-first "test-file-2")
       => (string-append "ello, world!" (string #\newline)))

; extract the reader, set position, and then reconstruct a stream from it
(define (after-n stream n)
  (call-with-values
      (lambda () (input-stream-reader+constructor stream))
    (lambda (reader construct)
      (reader-set-position! reader n)
       (call-with-input-stream (construct reader)
         (lambda (stream-2)
	   (let-values (((contents _) (input-string-all stream-2)))
	     contents))))))

(check (after-n (open-string-input-stream "abcdefghijklmnopqrstuvwxyz") 10)
       => "klmnopqrstuvwxyz")

; translation

(define (translate-crlf-input original-input-stream wish)

  ;; state automaton

  (define (vanilla input-stream count)
    (call-with-values
	(lambda ()
	  (input-u8 input-stream))
      (lambda (octet input-stream)
	(cond
	 ((not octet) (finish count))
	 ((= 13 octet) (cr input-stream count))
	 (else (vanilla input-stream (+ 1 count)))))))
	    
  (define (cr input-stream count)
    (call-with-values
	(lambda ()
	  (input-u8 input-stream))
      (lambda (octet input-stream)
	(cond
	 ((not octet) (finish (+ 1 count)))	; CR hasn't been counted yet
	 ((= 10 octet)
	  (call-with-values
	      (lambda ()
		(input-blob-n original-input-stream (+ 1 count)))
	    (lambda (blob _)
	      (blob-u8-set! blob count 10)
	      (values blob input-stream))))
	 (else (vanilla input-stream (+ count 1)))))))

  (define (finish count)
    (if (zero? count)
	(let-values (((_ past-eof) (input-u8 original-input-stream)))
	  (values #f past-eof))
	(call-with-values
	    (lambda ()
	      (input-blob-n original-input-stream count))
	  (lambda (blob input-stream)
	    (values blob input-stream)))))
	  
  (vanilla original-input-stream 0))

(define (make-crlf-translated-input-stream input-stream)
  (make-translated-input-stream input-stream
				translate-crlf-input))

(check
 (call-with-values
     (lambda ()
       (input-blob-all
	(make-crlf-translated-input-stream
	 (open-blob-input-stream
	  (u8-list->blob '(102 111 111 13 10 102 111 111 13))))))
   (lambda (contents _)
     (blob->u8-list contents)))
 => '(102 111 111 10 102 111 111 13))

(define (make-crlf-translated-output-stream output-stream)
  (make-translated-output-stream output-stream
				 translate-crlf-output
				 #f))

(define (unspecific)
  (if #t #t))

(define (translate-crlf-output output-stream state data start count)
  (cond
   ((not data))
   ((blob? data)
    (let ((end (+ start count)))
      (let loop ((index start))
	(cond
	 ((blob-index data 10 index end)
	  => (lambda (lf-index)
	       (output-blob output-stream data index (- lf-index index))
	       (output-u8 output-stream 13)
	       (output-u8 output-stream 10)
	       (loop (+ 1 lf-index))))
	 (else
	  (output-blob output-stream data index (- end index)))))))
   ((= data 10)
    (output-u8 output-stream 13)
    (output-u8 output-stream 10))
   (else
    (output-u8 output-u8 data)))
  (unspecific))

(define (blob-index blob octet start end)
  (let loop ((index start))
    (cond
     ((>= index end)
      #f)
     ((= octet (blob-u8-ref blob index))
      index)
     (else
      (loop (+ 1 index))))))

(check
  (blob->u8-list
   (call-with-blob-output-stream
    (lambda (stream)
      (let ((stream (make-crlf-translated-output-stream stream)))
	(output-string stream "Hello!")
	(output-char stream #\newline)
	(output-string stream (string-append "Is it me you're looking for?"
					     (string #\newline)
					     "I can see it in your eyes"))))))
  => '(72 101 108 108 111 33 13 10
       73 115 32 105 116 32 109 101 32 121 111 117 39 114 101 32 108 111 111 107 105 110 103 32 102 111 114 63 13 10 
       73 32 99 97 110 32 115 101 101 32 105 116 32 105 110 32 121 111 117 114 32 101 121 101 115))


; from Reppy:
(define (make-infinite-blanks-reader)
  (make-simple-reader "<blanks, blanks, and more blanks>"
		      #f
		      4096
		      (lambda (blob start count)
			(let loop ((index 0))
			  (if (>= index count)
			      index
			      (begin
				(blob-u8-set! blob (+ start index) 32)
				(loop (+ 1 index))))))
		      (lambda ()
			1000) ; some number
		      #f #f #f
		      unspecific))

(define (make-infinite-blanks-stream)
  (open-reader-input-stream (make-infinite-blanks-reader)))

(check (call-with-values
	   (lambda () (input-string-n (make-infinite-blanks-stream) 40))
	 (lambda (blanks _)
	   blanks))
       => (make-string 40 #\space))
	       
				     
; Some more obscure tests, mostly for checking the buffering

(call-with-input-stream
 (open-reader-input-stream (make-infinite-blanks-reader)
			   (u8-list->blob '(1 2 3)))
 (lambda (stream)
   (check (let-values (((blob _) (input-blob-n stream 5)))
	    (blob->u8-list blob))
	  => '(1 2 3 32 32))))

(define (read-all-chars input-stream eof-count)
  (let loop ((input-stream input-stream)
	     (rev-chars '())
	     (eof-count eof-count))
    (call-with-values
	(lambda () (input-char input-stream))
      (lambda (c new-input-stream)
	(cond
	 (c
	  (loop new-input-stream (cons c rev-chars) eof-count))
	 ((zero? eof-count)
	  (reverse rev-chars))
	 (else
	  (call-with-values
	      (lambda () (input-string input-stream))
	    (lambda (empty input-stream)
	      (if empty
		  'error
		  (loop input-stream (cons #f rev-chars) (- eof-count 1)))))))))))

(check (read-all-chars (open-strings-input-stream '("foo" "bar" "" "baz"))
		       4)
       => '(#\f #\o #\o #\b #\a #\r #f #\b #\a #\z #f #f #f))

(check (call-with-string-output-port
	(lambda (port)
	  (write-string port "foo bar baz")))
       => "foo bar baz")

(call-with-output-port (open-file-output-port "test-output.txt" (file-options truncate create))
  (lambda (port)
    (write-string port "foo bar baz")
    (newline port)))
(call-with-output-port (open-file-output-port "test-output.txt" (file-options create append))
 (lambda (port)
   (write-string port "bla bala")
   (newline port)))

(check (blob->u8-list
	(call-with-input-port (open-file-input-port "test-output.txt") read-blob-all))
       => '(102 111 111 32 98 97 114 32 98 97 122 10 98 108 97 32 98 97 108 97 10))

(define (port->blob/1 port)
  (let loop ((rev-blob '()))
    (let ((b (read-u8 port)))
      (if (not b)
	  (reverse rev-blob)
	  (loop (cons b rev-blob))))))

(check (call-with-input-port (open-file-input-port "test-output.txt") port->blob/1)
       => '(102 111 111 32 98 97 114 32 98 97 122 10 98 108 97 32 98 97 108 97 10))

(define (port->blob/2 port)
  (let loop ((rev-blob '()))
    (cond
     ((read-blob-some port)
      => (lambda (b)
	   (loop (append (reverse (blob->u8-list b)) rev-blob))))
     (else
      (reverse rev-blob)))))

(check (call-with-input-port (open-file-input-port "test-output.txt") port->blob/2)
       => '(102 111 111 32 98 97 114 32 98 97 122 10 98 108 97 32 98 97 108 97 10))

(define (port->blob/3 port)
  (let loop ((rev-blob '()))
    (cond
     ((read-blob-n port 4)
      => (lambda (b)
	   (loop (append (reverse (blob->u8-list b)) rev-blob))))
     (else
      (reverse rev-blob)))))

(check (call-with-input-port (open-file-input-port "test-output.txt") port->blob/3)
       => '(102 111 111 32 98 97 114 32 98 97 122 10 98 108 97 32 98 97 108 97 10))


(define s1 (list->string
	    (map scalar-value->char
		 '(#x41 #xe4 #x416 #x5c71 #xe0041))))

(check (call-with-string-output-port
	(lambda (port)
	  (write-string port s1)))
       => s1)

(check (read-string-all (open-string-input-port s1))
       => s1)

(define s2
  (call-with-string-output-port
   (lambda (port)
     (let loop ((n 500))
       (if (positive? n)
	   (begin
	     (write-string port s1)
	     (newline port)
	     (write-string port "foo")
	     (newline port)
	     (loop (- n 1)))))
     (write-string port "foo"))))

(define (read-lines port)
  (let loop ((rev-lines '()))
    (let ((line (read-line port)))
      (if (not line)
	  (reverse rev-lines)
	  (loop (cons line rev-lines))))))

(check (length (read-lines (open-string-input-port s2)))
       => 1001)

(call-with-output-port
 (open-file-output-port "test-output.txt" (file-options truncate create))
 (cut write-string <> s2))

(check (length
	(call-with-input-port (open-file-input-port "test-output.txt")
	  read-lines))
       => 1001)

(check (call-with-string-output-port
	(lambda (port)
	  (write-string port "foo bar baz")
	  (set-output-port-position! port 4)
	  (write-string port "boo")))
       => "foo boo baz")

(check (call-with-input-port (open-string-input-port s1)
	 (lambda (port)
	   (read-string-n port 2)
	   (input-port-position port)))
       => 3)

(check (call-with-input-port (open-file-input-port "test-output.txt")
	 (lambda (port)
	   (cons
	    (read-string-n port 7)
	    (read-string-n port 2))))
       => (cons (string-append s1 (string #\newline) "f")
		"oo"))

; some incomplete UTF-8 encodings
(check (call-with-input-port (open-blob-input-port
			      (u8-list->blob '(#x41 #xe4 #x41 #xf0 #x81)))
	 (lambda (port)
	   (read-string-all port)))
       => "A?A??")

; EOFs in the middle of the input
(let* ((s
	(open-strings-input-stream
	 (list "" "abc" "" "cde")))
      (test (lambda (proc)
	      (let loop ((s s) (n 10) (rs '()))
		(if (zero? n)
		    rs
		    (let-values (((r s) (proc s)))
		      (loop s (- n 1) (cons r rs)))))))
      (reify (lambda (b)
	       (if b
		   (blob->u8-list b)
		   #f))))
  (check (map reify (test input-blob-some))
	 => '(#f #f #f #f #f #f (99 100 101) #f (97 98 99) #f))
  (check (map reify (test (lambda (s) (input-blob-n s 5))))
	 => '(#f #f #f #f #f #f (99 100 101) #f (97 98 99) #f))
  (check (map reify (test input-blob-all))
	 => '(#f #f #f #f #f #f (99 100 101) #f (97 98 99) #f))
  (check (test input-u8)
	 => '(#f #f 101 100 99 #f 99 98 97 #f))
  (check (test input-string)
	 => '(#f #f #f #f #f #f "cde" #f "abc" #f))
  (check (test (lambda (s) (input-string-n s 5)))
	 => '(#f #f #f #f #f #f "cde" #f "abc" #f))
  (check (test input-string-all)
	 => '(#f #f #f #f #f #f "cde" #f "abc" #f))
  (check (test input-char)
	 => '(#f #f #\e #\d #\c #f #\c #\b #\a #f)))

(call-with-input-port (open-blob-input-port
		       (u8-list->blob '(1 2 3 4 5 6 7 8 9)))
  (lambda (port)
    (for-each
     (lambda (pos)
       (set-input-port-position! port pos)
       (check (read-u8 port) => (+ pos 1)))
     '(0 8 4 3 2 5))))

; Check transcoding round trip

(define (transcoder-round-trip transcoder text)
  (let* ((coded
	  (call-with-blob-output-stream
	   (lambda (output-stream)
	     (let ((output-stream
		    (transcode-output-stream output-stream transcoder)))
	       (output-string output-stream text)))))

	 (input-stream (open-blob-input-stream coded))
	 (input-stream (transcode-input-stream input-stream transcoder)))
    (let-values (((text _) (input-string-all input-stream)))
      text)))

(check (transcoder-round-trip (transcoder (codec latin-1-codec)) "ABCDE")
       => "ABCDE")

(check (transcoder-round-trip (transcoder (codec utf-32le-codec)) "ABCDE")
       => "ABCDE")
(check (transcoder-round-trip (transcoder (codec utf-32be-codec)) "ABCDE")
       => "ABCDE")

(check (transcoder-round-trip (transcoder (codec utf-16le-codec)) "ABCDE")
       => "ABCDE")
(check (transcoder-round-trip (transcoder (codec utf-16be-codec)) "ABCDE")
       => "ABCDE")

(let ((text (list->string (map scalar-value->char '(#x100 #x400 #x1000 #x10800)))))
  (check (transcoder-round-trip (transcoder (codec utf-32le-codec)) text)
       => text))
(let ((text (list->string (map scalar-value->char '(#x100 #x400 #x1000 #x10800)))))
  (check (transcoder-round-trip (transcoder (codec utf-16le-codec)) text)
       => text))

(define (decode-utf-32le blob)
  (let* ((input-stream (open-blob-input-stream blob))
	 (input-stream (transcode-input-stream input-stream
					       (transcoder (codec utf-32le-codec)))))
    (let-values (((text _) (input-string-all input-stream)))
      text)))

(check (decode-utf-32le (u8-list->blob '(#x41 #x00 #x00 #x00 #x10 #x20)))
       => "A?")

; feed the UTF-8 bit-by-bit
(check
 (decode-utf-32le
  (call-with-blob-output-stream
   (lambda (output-stream)
     (let ((output-stream
	    (transcode-output-stream output-stream
				     (transcoder (codec utf-32le-codec)))))
       (for-each
	(cut output-u8 output-stream <>)
	'(196 128 208 128 225 128 128 240 144 160 128))))))
 =>
 (list->string (map scalar-value->char '(#x100 #x400 #x1000 #x10800))))

(check
 (decode-utf-32le
  (call-with-blob-output-port
   (lambda (output-port)
     (transcode-output-port! output-port 
			     (transcoder (codec utf-32le-codec)))
     (for-each
      (cut write-u8 output-port <>)
      '(196 128 208 128 225 128 128 240 144 160 128)))))
 =>
 (list->string (map scalar-value->char '(#x100 #x400 #x1000 #x10800))))

(check
 (call-with-values
     (lambda ()
       (input-blob-all
	(transcode-input-stream
	 (open-blob-input-stream
	  (u8-list->blob '(102 111 111 13 10 102 111 111 13)))
	 (transcoder (eol-style (eol-style crlf))))))
   (lambda (contents _)
     (blob->u8-list contents)))
 => '(102 111 111 10 102 111 111 13))

(check
 (call-with-values
     (lambda ()
       (input-blob-all
	(transcode-input-stream
	 (open-blob-input-stream
	  (u8-list->blob '(102 111 111 13 102 111 111 13)))
	 (transcoder (eol-style (eol-style cr))))))
   (lambda (contents _)
     (blob->u8-list contents)))
 => '(102 111 111 10 102 111 111 10))

(check
 (let ((port (open-blob-input-port
	      (u8-list->blob '(102 111 111 13 102 111 111 13)))))
   (transcode-input-port! port (transcoder (eol-style (eol-style cr))))
   (let ((contents (read-blob-all port)))
     (blob->u8-list contents)))
 => '(102 111 111 10 102 111 111 10))

(check
 (blob->u8-list
  (call-with-blob-output-stream
   (lambda (stream)
     (let ((stream (transcode-output-stream stream
					    (transcoder (eol-style (eol-style crlf))))))
       (output-string stream "Hello!")
       (output-char stream #\newline)
       (output-string stream (string-append "Is it me you're looking for?"
					    (string #\newline)
					    "I can see it in your eyes"))))))
 => '(72 101 108 108 111 33 13 10
	 73 115 32 105 116 32 109 101 32 121 111 117 39 114 101 32 108 111 111 107 105 110 103 32 102 111 114 63 13 10 
	 73 32 99 97 110 32 115 101 101 32 105 116 32 105 110 32 121 111 117 114 32 101 121 101 115))

(check
 (blob->u8-list
  (call-with-blob-output-stream
   (lambda (stream)
     (let ((stream (transcode-output-stream stream
					    (transcoder (eol-style (eol-style cr))))))
       (output-string stream "Hello!")
       (output-char stream #\newline)
       (output-string stream (string-append "Is it me you're looking for?"
					    (string #\newline)
					    "I can see it in your eyes"))))))
 => '(72 101 108 108 111 33 13 
	 73 115 32 105 116 32 109 101 32 121 111 117 39 114 101 32 108 111 111 107 105 110 103 32 102 111 114 63 13 
	 73 32 99 97 110 32 115 101 101 32 105 116 32 105 110 32 121 111 117 114 32 101 121 101 115))

(r5rs:newline)
(r5rs:display "correct tests: ")
(r5rs:display *correct-count*)
(r5rs:newline)
(r5rs:display "failed tests: ")
(r5rs:display *failed-count*)
(r5rs:newline)
