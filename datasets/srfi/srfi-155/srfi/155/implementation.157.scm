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


;;; Data types

(define-record-type <promise>
  (%%make-promise content)
  promise?
  (content promise-content promise-set-content!))

(define (%make-promise done? value dynamic-extent)
  (%%make-promise (vector done? value dynamic-extent)))

(define (promise-done? promise)
  (vector-ref (promise-content promise) 0))

(define (promise-value promise)
  (vector-ref (promise-content promise) 1))

(define (promise-dynamic-extent promise)
  (vector-ref (promise-content promise) 2))

(define (promise-set-done?! promise done?)
  (vector-set! (promise-content promise) 0 done?))

(define (promise-set-value! promise value)
  (vector-set! (promise-content promise) 1 value))

(define (promise-set-dynamic-extent! promise dynamic-extent)
  (vector-set! (promise-content promise) 2 dynamic-extent))

(define key (vector #f))

;;; Internal parameters

(define current-forcing-extent (make-parameter #f))

;;; Internal procedures

(define (promise-update! new old)
  (promise-set-done?! old (promise-done? new))
  (promise-set-value! old (promise-value new))
  (promise-set-dynamic-extent! old (promise-dynamic-extent new))
  (promise-set-content! new (promise-content old)))

;;; Exported syntax

(define-syntax delay
  (syntax-rules ()
    ((delay expr)
     (%make-promise #f (lambda () expr) (current-dynamic-extent)))))

(define-syntax delay-force
  (syntax-rules ()
    ((delay-force promise)
     (delay (force promise)))))

;;; Exported procedures

(define (make-promise obj)
  (%make-promise #t obj #f))

(define (force promise)
  (let ((forcing-extent (current-dynamic-extent)))
    (let loop ((promise promise))
      (if (promise-done? promise)
	  (promise-value promise)
	  (call-with-immediate-continuation-mark
	   key
	   (lambda (c)
	     (if c
		 (c promise)
		 (let ((promise*
			(call-with-current-continuation
			 (lambda (c)
			   (make-promise
			    (with-dynamic-extent
			     (promise-dynamic-extent promise)
			     (lambda ()
			       (parameterize ((current-forcing-extent forcing-extent))
				 (with-continuation-mark key c
							 ((promise-value promise)))))))))))
		   (unless (promise-done? promise)
		     (promise-update! promise* promise))
		   (loop promise)))))))))

(define (forcing-extent)
  (unless (current-forcing-extent)
    (error "forcing-extent: there is no promise being forced"))
  (current-forcing-extent))

