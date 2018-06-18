; Common data extenstions for Comprehensive I/O SRFI

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

(define-condition-type &i/o-error &error
  i/o-error?)

(define-condition-type &i/o-read-error &i/o-error
  i/o-read-error?)

(define-condition-type &i/o-write-error &i/o-error
  i/o-write-error?)

(define-condition-type &i/o-invalid-position-error &i/o-error
  i/o-invalid-position-error?
  (position i/o-error-position))

(define-condition-type &i/o-closed-error &i/o-error
  i/o-closed-error?)

(define-condition-type &i/o-filename-error &i/o-error
  i/o-filename-error?
  (filename i/o-error-filename))

(define-condition-type &i/o-malformed-filename-error &i/o-filename-error
  i/o-malformed-filename-error?)

(define-condition-type &i/o-operation-error &i/o-error
  i/o-operation-error?
  (operation i/o-error-operation))

(define-condition-type &i/o-operation-not-available-error &i/o-operation-error
  i/o-operation-not-available-error?)

(define-condition-type &i/o-file-protection-error &i/o-filename-error
  i/o-file-protection-error?)

(define-condition-type &i/o-file-is-read-only-error &i/o-file-protection-error
  i/o-file-is-read-only-error?)

(define-condition-type &i/o-file-already-exists-error &i/o-filename-error
  i/o-file-already-exists-error?)

(define-condition-type &i/o-no-such-file-error &i/o-filename-error
  i/o-no-such-file-error?)

(define-enumerated-type buffer-mode :buffer-mode
  buffer-mode?
  buffer-modes
  buffer-mode-name
  buffer-mode-index
  (none line block))

(define-enumerated-type file-option :file-option
  file-option?
  all-file-options
  file-option-name
  file-option-index
  (create exclusive truncate append))

(define-enum-set-type file-options :file-options
  file-options?
  make-file-options
  
  file-option
  file-option?
  all-file-options
  file-option-index)

; returns true if the first argument includes all of the options in
; the second one
(define (file-options-include? options-1 options-2)
  (enum-set=? (enum-set-intersection options-1 options-2)
	      options-2))

(define (file-options-union . options-list)
  (apply enum-set-union options-list))
