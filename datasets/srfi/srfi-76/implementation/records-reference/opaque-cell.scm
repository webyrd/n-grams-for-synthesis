; Opaque cells for Records SRFI

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

(define-record-type :opaque-cell
  (make-opaque-cell key-predicate contents)
  opaque-cell?
  (key-predicate opaque-cell-key-predicate)
  (contents real-opaque-cell-ref real-opaque-cell-set!))

(define (ensure-opaque-cell-access key cell)
  (if (not ((opaque-cell-key-predicate cell) key))
      (error "access to opaque cell denied" cell key)))

(define (opaque-cell-ref key cell)
  (ensure-opaque-cell-access key cell)
  (real-opaque-cell-ref cell))

(define (opaque-cell-set! key cell val)
  (ensure-opaque-cell-access key cell)
  (real-opaque-cell-set! key val))

(define (opaque-cell-key-fits? key cell)
  (and ((opaque-cell-key-predicate cell) key) #t))
