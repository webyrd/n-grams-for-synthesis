; Text transcoders for Comprehensive I/O SRFI

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

(define-record-type :transcoder
  (make-transcoder codec eol-style)
  transcoder?
  (codec transcoder-codec)
  (eol-style transcoder-eol-style))

(define-options-type transcoder update-transcoder
  (make-transcoder (codec transcoder-codec)
		   (eol-style transcoder-eol-style)))

(define-record-type :codec
  (make-codec name
	      input-stream-translator
	      output-stream-translator
	      output-stream-initial-state)
  codec?
  (name codec-name)
  (input-stream-translator codec-input-stream-translator)
  (output-stream-translator codec-output-stream-translator)
  (output-stream-initial-state codec-output-stream-initial-state))

(define (transcode-input-stream input-stream transcoder)
  (let ((codec (transcoder-codec transcoder))
	(eol (transcoder-eol-style transcoder)))
    (let ((utf-8-stream
	   (if (options-value-unassigned? codec)
	       input-stream
	       (make-translated-input-stream input-stream
					     (codec-input-stream-translator codec)))))
      
      (cond
       ((eq? (eol-style crlf) eol)
	(make-translated-input-stream utf-8-stream
				      translate-crlf-input))
       ((eq? (eol-style cr) eol)
	(make-translated-input-stream utf-8-stream
				      translate-cr-input))
       (else
	utf-8-stream)))))

(define (transcode-output-stream output-stream transcoder)
  (let ((codec (transcoder-codec transcoder))
	(eol (transcoder-eol-style transcoder)))
    (let ((utf-8-stream
	   (cond
	    ((eq? (eol-style crlf) eol)
	     (make-translated-output-stream output-stream
					    translate-crlf-output
					    (unspecific)))
	    ((eq? (eol-style cr) eol)
	     (make-translated-output-stream output-stream
					    translate-cr-output
					    (unspecific)))
	    (else
	     output-stream))))
      (if (options-value-unassigned? codec)
	  utf-8-stream
	  (make-translated-output-stream utf-8-stream
					 (codec-output-stream-translator codec)
					 (codec-output-stream-initial-state codec))))))

; Dealing with different EOL styles

(define-enumerated-type eol-style :eol-style
  eol-style?
  eol-styles
  eol-style-name
  eol-style-index
  (cr lf crlf))

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
	 ((not octet) (finish (+ 1 count))) ; CR hasn't been counted yet
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

(define (translate-cr-input original-input-stream wish)
  (call-with-values
      (lambda () (input-blob-some original-input-stream))
    (lambda (chunk rest)
      (if (not chunk)
	  (values chunk rest)
	  (let ((size (blob-length chunk)))
	    (let loop ((i 0))
	      (if (< i size)
		  (begin
		    (if (= (blob-u8-ref chunk i) 13)
			(blob-u8-set! chunk i 10))
		    (loop (+ 1 i)))))
	    (values chunk rest))))))

(define (translate-cr-output output-stream state data start count)
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
	       (loop (+ 1 lf-index))))
	 (else
	  (output-blob output-stream data index (- end index)))))))
   ((= data 10)
    (output-u8 output-stream 13))
   (else
    (output-u8 output-u8 data))
   (unspecific)))

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

; Dealing with different encodings of Unicode

; Note this could all easily be changed to use iconv or ICU, and
; benefit from block transcoding, which is why all this is written in
; such a goofy way.

; Utilities

(define (input-multiple-of original-input-stream n)
  (let-values (((raw-chunk raw-rest) (input-blob-some original-input-stream)))
    (if (not raw-chunk)			; EOF
	(values raw-chunk raw-rest)
	(let ((orig-size (blob-length raw-chunk)))
	  (cond
	   ((< orig-size n)
	    (input-blob-n original-input-stream n))
	   ((zero? (remainder orig-size n))
	    (values raw-chunk raw-rest))
	   (else
	    (input-blob-n original-input-stream
			      (* (quotient orig-size n) n))))))))

(define (make-translate-unicode-output output-char output-string)
  ;; As state, we use a pair of a blob and an index representing
  ;; the residual data we couldn't encode on the last go, or #f.
  ;; Generally, we should get #f most of the time---exclusively, if
  ;; people use OUTPUT-CHAR and OUTPUT-STRING.

  (lambda (output-stream state data start count)

    (define (translate-blob data start count)
      (call-with-values
	  (lambda ()
	    (bytes-string-size/utf-8 data start count #f))
	(lambda (decoding-status consumed-count decoded-count)
	  (let ((text (make-string decoded-count)))
	    (call-with-values
		(lambda ()
		  (decode-string/utf-8 data start count
				       text 0 decoded-count
				       #\?))
	      (lambda (decoding-status consumed-count decoded-count)
		(output-string output-stream text)
		(if (< consumed-count count)
		    (cons data consumed-count)
		    #f)))))))

    (cond
     ((not data)			; close
      (if state
	  (let loop ((n (- (blob-length (car state) (cdr state)))))
	    (if (> n 0)
		(begin
		  (output-char output-stream #\?)
		  (loop (- n 1)))))))
     ((blob? data)
      (if state
	  (let* ((data (blob-append (car state) (cdr state) data))
		 (size (blob-length data)))
	    (translate-blob data 0 size))
	  (translate-blob data start count)))
     (state
      (let* ((data (blob-append (car state) (cdr state) (u8-list->blob (list data))))
	     (size (blob-length data)))
	(translate-blob data 0 size)))
     ((<= data #x7f)			; 1-byte encoding
      (output-char output-stream (scalar-value->char data))
      #f)
     (else			   ; not enough data for a single char
      (cons (u8-list->blob (list data)) 0)))))

(define *failed-utf-8* (string->utf-8 "?"))

(define (make-output-string output-char)
  (lambda (output-stream s)
    (let ((size (string-length s)))
      (let loop ((i 0))
	(if (< i size)
	    (begin
	      (output-char output-stream (string-ref s i))
	      (loop (+ 1 i))))))))

; Latin-1

(define (translate-latin-1-input original-input-stream wish)
  (let-values (((chunk rest) (input-blob-some original-input-stream)))
    (if (not chunk)
	(values chunk rest)
	(let* ((size (blob-length chunk))
	       (text (make-string size)))
	  (let loop ((i 0))
	    (if (< i size)
		(begin
		  (string-set! text i
			       (scalar-value->char (blob-u8-ref chunk i)))
		  (loop (+ 1 i)))))
	  (values (string->utf-8 text) rest)))))

(define (output-char-latin-1 output-stream char)
  (let ((scalar-value (char->scalar-value char)))
    (if (<= scalar-value #xff)
	(output-u8 output-stream scalar-value)
	(output-blob output-stream *failed-utf-8*))))

(define output-string-latin-1 (make-output-string output-char-latin-1))

(define translate-latin-1-output
  (make-translate-unicode-output output-char-latin-1
				 output-string-latin-1))

(define latin-1-codec (make-codec "ISO8859-1"
				  translate-latin-1-input
				  translate-latin-1-output
				  #f))

; UTF-16

(define (translate-utf-16-input endianness original-input-stream wish)
  (let-values (((chunk rest) (input-multiple-of original-input-stream 2)))
    (if (not chunk)
	(values chunk rest)
	(let ((chunk-size (blob-length chunk)))
	  (if (< (blob-length chunk) 2)
	      (values *failed-utf-8* rest)
	      (let-values
		  (((chunk rest)
		    (let ((last-code-unit
			   (blob-u16-ref endianness chunk (- chunk-size 2))))
		      (cond 
		       ((or (< last-code-unit #xd800) 
			    (> last-code-unit #xdbff)) 
			;; no high surrogate, we're OK
			(values chunk rest))
		       ;; high (incomplete) surrogate
		       ((= 2 chunk-size)
			;; try to complete it
			(input-blob-n original-input-stream
				      4))
		       (else
			;; skip it
			(input-blob-n original-input-stream
				      (- (blob-length chunk) 2)))))))
		(values (utf-16->utf-8 chunk endianness)
			rest)))))))

(define (utf-16->utf-8 data endianness)
  (let* ((size (blob-length data))
	 (code-unit-count (quotient size 2)))
    (let loop ((i 0) (rev-chars '()))
      (if (>= i code-unit-count)
	  (string->utf-8 (list->string
			  (if (= (* 2 code-unit-count) size)
			      (reverse rev-chars)
			      (cons #\? rev-chars))))
	  (let ((code-unit0 (blob-u16-ref endianness data (* i 2))))
	    (cond
	     ((or (< code-unit0 #xd800)
		  (> code-unit0 #xdfff))
	      (loop (+ 1 i)
		    (cons (scalar-value->char code-unit0) rev-chars)))
	     ((> code-unit0 #xdbff) ; low surrogate
	      (loop (+ 1 i) (cons #\? rev-chars)))
	     ((>= (+ 1 (* (+ 1 i) 2)) size) ; no subsequent code unit
	      (loop (+ 1 i) (cons #\? rev-chars)))
	     (else
	      (let ((code-unit1 (blob-u16-ref endianness data (* (+ 1 i) 2))))
		(if (and (>= code-unit1 #xdc00)
			 (<= code-unit1 #xdfff))
		    (loop (+ 2 i)
			  (cons (scalar-value->char
				 (+ (arithmetic-shift (- code-unit0 #xd7c0) 10)
				    (bitwise-and code-unit1 #x3ff))) 
				rev-chars))
		    (loop (+ 2 i) (cons #\? rev-chars)))))))))))

(define translate-utf-16le-input
  (cut translate-utf-16-input (endianness little) <> <>))

(define translate-utf-16be-input
  (cut translate-utf-16-input (endianness big) <> <>))

(define (make-output-char-utf16 output-code-unit)
  (lambda (output-stream char)
    (let ((scalar-value (char->scalar-value char)))
      (if (<= scalar-value #xffff)
	  (output-code-unit output-stream scalar-value)
	  (begin
	    (output-code-unit output-stream
			      (+ (arithmetic-shift scalar-value -10) #xd7c0))
	    (output-code-unit output-stream
			      (+ (bitwise-and scalar-value #x3ff) #xdc00)))))))

(define (output-code-unit-le output-stream code-unit)
  (output-u8 output-stream (bitwise-and #b11111111 code-unit))
  (output-u8 output-stream (arithmetic-shift code-unit -8)))

(define (output-code-unit-be output-stream code-unit)
  (output-u8 output-stream (arithmetic-shift code-unit -8))
  (output-u8 output-stream (bitwise-and #b11111111 code-unit)))

(define output-char-utf-16be (make-output-char-utf16 output-code-unit-be))
(define output-char-utf-16le (make-output-char-utf16 output-code-unit-le))

(define output-string-utf-16le (make-output-string output-char-utf-16le))

(define translate-utf-16le-output
  (make-translate-unicode-output output-char-utf-16le output-string-utf-16le))

(define output-string-utf-16be (make-output-string output-char-utf-16be))

(define translate-utf-16be-output
  (make-translate-unicode-output output-char-utf-16be output-string-utf-16be))

(define utf-16le-codec (make-codec "UTF-16LE"
				   translate-utf-16le-input
				   translate-utf-16le-output
				   #f))

(define utf-16be-codec (make-codec "UTF-16BE"
				   translate-utf-16be-input
				   translate-utf-16be-output
				   #f))

; UTF-32

(define (make-translate-utf-32-input utf-32->utf-8)
  (lambda (original-input-stream wish)
    (let-values (((chunk rest) (input-multiple-of original-input-stream 4)))
      (cond
       ((not chunk)
	(values chunk rest))
       ((< (blob-length chunk) 4)
	(values *failed-utf-8* rest))
       (else
	(values (utf-32->utf-8 chunk) rest))))))

(define (make-utf-32->utf-8 decode-char)
  ;; we know data has size a multiple of 4
  (lambda (data)
    (let* ((size (blob-length data))
	   (text-size (quotient size 4))
	   (text (make-string text-size)))
      (let loop ((char-index 0))
	(if (>= char-index text-size)
	    (string->utf-8 text)
	    (let ((c (decode-char data (* char-index 4))))
	      (string-set! text char-index c)
	      (loop (+ 1 char-index))))))))

(define (decode-char/utf-32le buffer start)
  (let ((code-point
	 (blob-u32-ref (endianness little) buffer start)))
    (if (scalar-value? code-point)
	(scalar-value->char code-point)
	#\?)))
	
(define translate-utf-32le-input
  (make-translate-utf-32-input
   (make-utf-32->utf-8 decode-char/utf-32le)))

(define (decode-char/utf-32be buffer start)
  (let ((code-point
	 (blob-u32-ref (endianness big) buffer start)))
    (if (scalar-value? code-point)
	(scalar-value->char code-point)
	#\?)))

(define translate-utf-32be-input
  (make-translate-utf-32-input
   (make-utf-32->utf-8 decode-char/utf-32be)))

(define (blob-append source source-start back)
  (let* ((source-size (- (blob-length source) source-start))
	 (back-size (blob-length back))
	 (output (make-blob (+ source-size back-size))))
    (blob-copy! source source-start
		output 0 source-size)
    (blob-copy! back 0
		output source-size back-size)
    output))

(define (output-char-utf-32le output-stream char)
  (let ((scalar-value (char->scalar-value char)))
    (output-u8 output-stream (bitwise-and scalar-value #xff))
    (output-u8 output-stream (arithmetic-shift
			      (bitwise-and scalar-value #xff00)
			      -8))
    (output-u8 output-stream (arithmetic-shift
			      (bitwise-and scalar-value #xff0000)
			      -16))
    (output-u8 output-stream (arithmetic-shift scalar-value -24))))

(define output-string-utf-32le (make-output-string output-char-utf-32le))

(define translate-utf-32le-output
  (make-translate-unicode-output output-char-utf-32le output-string-utf-32le))

(define (output-char-utf-32be output-stream char)
  (let ((scalar-value (char->scalar-value char)))
    (output-u8 output-stream (arithmetic-shift scalar-value -24))
    (output-u8 output-stream (arithmetic-shift
			      (bitwise-and scalar-value #xff0000)
			      -16))
    (output-u8 output-stream (arithmetic-shift
			      (bitwise-and scalar-value #xff00)
			      -8))
    (output-u8 output-stream (bitwise-and scalar-value #xff))))

(define output-string-utf-32be (make-output-string output-char-utf-32be))

(define translate-utf-32be-output
  (make-translate-unicode-output output-char-utf-32be output-string-utf-32be))

(define utf-32le-codec (make-codec "UTF-32LE"
				   translate-utf-32le-input
				   translate-utf-32le-output
				   #f))

(define utf-32be-codec (make-codec "UTF-32BE"
				   translate-utf-32be-input
				   translate-utf-32be-output
				   #f))
