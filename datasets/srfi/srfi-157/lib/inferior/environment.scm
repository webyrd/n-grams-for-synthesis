;; Copyright (C) Marc Nieper-Wißkirchen (2017).  All Rights Reserved. 

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.


;;; Bindings

(define (make-binding identifier denotation)
  (cons identifier denotation))

(define (binding-identifier binding)
  (car binding))

(define (binding-denotation binding)
  (cdr binding))

(define (lookup-binding identifier bindings)
  (assq identifier bindings))

;;; Frames

(define (make-frame identifiers denotations)
  (map make-binding identifiers denotations))

(define (frame-add-binding frame identifier denotation)
  (cons (make-binding identifier denotation)
	frame))

(define (frame-lookup frame identifier)
  (cond
   ((assq identifier frame) => cdr)
   (else
    #f)))

(define (frame-map-bindings proc frame)
  (map (lambda (binding)
	 (proc (binding-identifier binding)
	       (binding-denotation binding)))
       frame))

;;; Environments

(define-record-type <environment>
  (%make-environment parent frame)
  environment?
  (parent enclosing-environment)
  (frame first-frame set-first-frame!))

(define (make-environment)
  (%make-environment #f (make-frame '() '())))

(define (environment-extend environment identifiers denotations)
  (%make-environment environment (make-frame identifiers denotations)))

(define (environment-add-binding! environment identifier denotation)
  (set-first-frame! environment (frame-add-binding (first-frame environment) identifier denotation)))

(define (environment-lookup environment identifier)
  (or (lookup-identifier identifier environment)
      (and (alias? identifier)
	   (environment-lookup (alias-environment identifier)
			       (alias-identifier identifier)))))

(define (lookup-identifier identifier environment)
  (let loop ((environment environment))
    (and environment
	 (or (frame-lookup (first-frame environment) identifier)
	     (loop (enclosing-environment environment))))))

(define (environment-map-bindings proc environment)
  (frame-map-bindings proc (first-frame environment)))
