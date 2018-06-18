; Reference implementation for Comprehensive I/O SRFI

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

; Implementation of SRFI 66 draft

(define-interface octet-vectors-interface
  (export make-u8vector
	  u8vector?
	  list->u8vector
	  u8vector->list
	  u8vector
	  u8vector-length
	  u8vector-ref
	  u8vector-set!
	  u8vector-copy!
	  u8vector-copy
	  u8vector=?
	  u8vector-compare))

(define-structure octet-vectors octet-vectors-interface
  (open scheme
	byte-vectors)
  (files octet-vector))

; Implementation of SRFI 74 draft

(define-interface blobs-interface
  (export (endianness :syntax)
	  blob? make-blob
	  blob-length
	  blob-u8-ref blob-s8-ref
	  blob-u8-set! blob-s8-set!
	  blob-uint-ref blob-sint-ref
	  blob-uint-set! blob-sint-set!

	  blob-u16-ref blob-s16-ref
	  blob-u16-native-ref blob-s16-native-ref
	  blob-u16-set! blob-s16-set!
	  blob-s16-native-set! blob-u16-native-set!

	  blob-u32-ref blob-s32-ref
	  blob-u32-native-ref blob-s32-native-ref
	  blob-u32-set! blob-s32-set!
	  blob-s32-native-set! blob-u32-native-set!

	  blob-u64-ref blob-s64-ref
	  blob-u64-native-ref blob-s64-native-ref
	  blob-u64-set! blob-s64-set!
	  blob-s64-native-set! blob-u64-native-set!

	  blob=?
	  blob-copy! blob-copy
	  blob->u8-list u8-list->blob
	  blob->uint-list blob->sint-list
	  uint-list->blob sint-list->blob))

(define-structure blobs blobs-interface
  (open scheme
	bitwise
	octet-vectors
	srfi-23 ; ERROR
	srfi-26 ; CUT
	)
  (files blob))

; Helper module for options

(define-interface define-option-types-interface
  (export (define-options-type :syntax)
	  options-value-unassigned?))

(define-structure define-option-types define-option-types-interface
  (open scheme)
  (files option))

; Encoding and decoding UTF-8

(define-interface utf-8-interface
  (export char-encoding-length/utf-8
	  string-encoding-length/utf-8
	  (encoding-status :syntax)
	  encoding-status?
	  encode-char/utf-8
	  encode-string/utf-8
	  string->utf-8-n
	  string->utf-8

	  (decoding-status :syntax)
	  decoding-status?
	  &decoding-error decoding-error? decoding-error-encoding-name

	  bytes-string-size/utf-8
	  decode-char/utf-8
	  decode-string/utf-8
	  utf-8->string utf-8->string-n))

(define-structure utf-8 utf-8-interface
  (open scheme
	unicode
	bitwise byte-vectors
	conditions exceptions
	finite-types
	(subset silly (reverse-list->string)))
  (files utf-8))

; Strip I/O from R5RS, so we can definte our own ports

(define scheme-sans-i/o
  (modify scheme
	  (hide call-with-input-file call-with-output-file
		input-port? output-port?
		current-input-port current-output-port
		with-input-from-file with-output-to-file
		open-input-file open-output-file
		close-input-port close-output-port
		read read-char peek-char
		eof-object?
		char-ready?
		write display newline write-char)))

; Common data definitions

(define-interface i/o-data-interface
  (export &i/o-error i/o-error?
	  &i/o-read-error i/o-read-error?
	  &i/o-write-error i/o-write-error?
	  &i/o-invalid-position-error i/o-invalid-position-error?
	  i/o-error-position
	  &i/o-closed-error i/o-closed-error?
	  &i/o-filename-error i/o-filename-error?
	  i/o-error-filename
	  &i/o-operation-error i/o-operation-error?
	  i/o-error-operation
	  &i/o-operation-not-available-error i/o-operation-not-available-error?
	  &i/o-malformed-filename-error i/o-malformed-filename-error?
	  &i/o-file-protection-error i/o-file-protection-error?
	  &i/o-file-is-read-only-error i/o-file-is-read-only-error?
	  &i/o-file-already-exists-error i/o-file-already-exists-error?
	  &i/o-no-such-file-error i/o-no-such-file-error?

	  (buffer-mode :syntax)
	  buffer-mode?

	  (file-options :syntax)
	  file-options?
	  file-options-include?
	  file-options-union))

(define-structure i/o-data i/o-data-interface
  (open scheme
	srfi-35 ; conditions
	finite-types
	enum-sets
	)
  (files io-data))

; Primitive I/O layer

(define-interface primitive-i/o-interface
  (export make-i/o-buffer
	  &i/o-reader/writer-error i/o-reader/writer-error?
	  i/o-error-reader/writer

          make-simple-reader reader?
	  reader-id
	  reader-descriptor
	  reader-chunk-size
	  reader-read!
	  reader-available
	  reader-close

	  reader-has-get-position? reader-get-position
	  reader-has-set-position!? reader-set-position!
	  reader-has-end-position? reader-end-position
	  
	  open-file-reader
	  open-blob-reader
	  standard-input-reader

	  make-simple-writer writer?
	  writer-id
	  writer-descriptor
	  writer-chunk-size
	  writer-write!
	  writer-close

	  writer-has-get-position? writer-get-position
	  writer-has-set-position!? writer-set-position!
	  writer-has-end-position? writer-end-position

	  open-file-writer

	  open-blob-writer writer-blob

	  standard-output-writer
	  standard-error-writer

	  open-file-reader+writer
	  ))

(define-structure primitive-i/o primitive-i/o-interface
  (open scheme
	variable-argument-lists
	blobs
	(subset i/o (current-error-port read-byte write-byte force-output))
	(modify srfi-36 (prefix srfi-36:) (expose i/o-error?))
	(modify posix-files
		(prefix posix:)
		(expose open-file file-options get-file-info file-info-size))
	(modify posix-i/o
		(prefix posix:)
		(expose dup-switching-mode))
	cells
	(subset util (unspecific))
	ascii
	srfi-9				; DEFINE-RECORD-TYPE
	srfi-26				; CUT
	srfi-27				; random bits
	srfi-34				; exceptions
	srfi-35				; conditions
	i/o-data)
  (begin
    (define-condition-type &i/o-reader/writer-error &i/o-error
      i/o-reader/writer-error?
      (reader/writer i/o-error-reader/writer)))
  (files primitive))

; Stream I/O layer

(define-interface stream-i/o-interface
  (export &i/o-stream-error i/o-stream-error?
	  i/o-error-stream

	  input-stream?
	  input-blob-some input-string
	  input-blob-n input-blob-n!
	  input-string-n input-string-n!
	  input-blob-all input-string-all
	  input-u8 input-char
	  input-line
	  stream-eof?
	  input-stream-position
	  input-stream-underliers
	  input-stream-reader+constructor
	  close-input-stream
	  open-reader-input-stream
	  open-file-input-stream
	  open-blob-input-stream
	  open-string-input-stream
	  call-with-input-stream
	  make-translated-input-stream
	  standard-input-stream

	  output-stream?
	  output-stream-buffer-mode set-output-stream-buffer-mode!
	  output-blob output-string
	  output-u8 output-char
	  flush-output-stream
	  output-stream-position set-output-stream-position!
	  output-stream-underliers
	  output-stream-writer+constructor
	  close-output-stream
	  open-writer-output-stream
	  open-file-output-stream
	  call-with-blob-output-stream call-with-string-output-stream
	  call-with-output-stream
	  make-translated-output-stream
	  standard-output-stream
	  standard-error-stream
	  
	  open-file-input+output-streams))

(define-structure stream-i/o stream-i/o-interface
  (open scheme-sans-i/o
	srfi-9				; DEFINE-RECORD-TYPE
	unicode utf-8
	finite-types
	cells
	variable-argument-lists
	(subset srfi-13 (string-index string-concatenate-reverse string-copy!))
	srfi-34				; exceptions
	srfi-35				; conditions
	(subset util (unspecific))
	blobs
	(subset silly (reverse-list->string))
	i/o-data
	primitive-i/o)
  (begin
    (define-condition-type &i/o-stream-error &i/o-error
      i/o-stream-error?
      (stream i/o-error-stream)))
  (files input-stream
	 output-stream
	 input-output-stream))

; Transcoders

(define-interface i/o-transcoders-interface
  (export (transcoder :syntax)
	  (eol-style :syntax)
	  make-codec
	  latin-1-codec
	  utf-32le-codec utf-32be-codec
	  utf-16le-codec utf-16be-codec))

(define-interface transcoded-streams-interface
  (export transcode-input-stream transcode-output-stream))

(define-structures ((i/o-transcoders i/o-transcoders-interface)
		    (transcoded-streams transcoded-streams-interface))
  (open scheme-sans-i/o
	stream-i/o
	blobs
	utf-8
	srfi-9				; DEFINE-RECORD-TYPE
	srfi-11				; LET*-VALUES, LET-VALUES
	srfi-26				; CUT
	finite-types
	bitwise
	define-option-types
	(subset util (unspecific))
	unicode
	)
  (files transcoder))

; Imperative I/O

(define-interface port-i/o-interface
  (export &i/o-port-error i/o-port-error?
	  i/o-error-port

	  input-port?
	  standard-input-port
	  read-blob-some read-u8 read-blob-all read-blob-n read-blob-n!
	  read-char read-string read-string-all read-string-n read-string-n! read-line
	  peek-u8 peek-char
	  port-eof?
	  close-input-port
	  input-port-position set-input-port-position!
	  transcode-input-port!
	  open-file-input-port
	  open-blob-input-port open-string-input-port
	  call-with-input-port
	  output-port?
	  standard-output-port standard-error-port
	  write-blob write-u8
	  write-string write-char
	  flush-output-port
	  output-port-buffer-mode set-output-port-buffer-mode!
	  close-output-port
	  output-port-position set-output-port-position!
	  transcode-output-port!
	  open-file-output-port
	  call-with-blob-output-port call-with-string-output-port
	  call-with-output-port
	  newline

	  open-file-input+output-ports))

(define-interface stream-ports-interface
  (export make-stream-input-port
	  stream-input-port?
	  input-port-stream set-input-port-stream!
	  make-stream-output-port
	  stream-output-port?
	  output-port-stream set-output-port-stream!))

(define-interface reader/writer-ports-interface
  (export open-reader-input-port
	  open-writer-output-port))

(define-structures ((port-i/o port-i/o-interface)
		    (stream-ports stream-ports-interface)
		    (reader/writer-ports reader/writer-ports-interface))
  (open scheme-sans-i/o
	variable-argument-lists
	srfi-9				; DEFINE-RECORD-TYPE
	srfi-34				; exceptions
	srfi-35				; conditions
	i/o-data
	primitive-i/o
	stream-i/o
	transcoded-streams)
  (files port))

; Examples and rudimentary test suite

(define-structure i/o-test (export)
  (open scheme-sans-i/o
	(modify scheme (prefix r5rs:) (expose write display newline))
	unicode utf-8
	blobs
	(subset srfi-1 (fold))
	srfi-11				; LET*-VALUES, LET-VALUES
	srfi-26				; CUT
	srfi-34				; exceptions
	srfi-35				; conditions
	i/o-data
	primitive-i/o
	stream-i/o
	port-i/o
	i/o-transcoders
	transcoded-streams)
  (files example))
