;; Copyright (C) Marc Nieper-Wißkirchen (2016).  All Rights Reserved. 

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

(define-syntax free-identifier=?
  (syntax-rules ()
    ((free-identifier=? id1 id2 kt kf)
     (begin
       (define-syntax m
	 (syntax-rules :::1 ()
	   ((m %kt %kf)
	    (begin
	      (define-syntax test
		(syntax-rules :::2 (id1)
		  ((test id1 %%kt %%kf) %%kt)
		  ((test x %%kt %%kf) %%kf)))
	      (test id2 %kt %kf)))))
       (m kt kf)))))

(define-syntax bound-identifier=?
  (syntax-rules ()
    ((bound-identifier=? id v kt kf)
     (begin
       (define-syntax m
	 (syntax-rules :::1 ()				       
	   ((m %kt %kf)
	    (begin
	      (define-syntax id
		(syntax-rules :::2 ()
		  ((id %%kt %%kf) %%kf)))
	      (define-syntax ok
		(syntax-rules ()
		  ((ok %%kt %%kf) %%kt)))
	      (define-syntax test
		(syntax-rules :::2 ()
		  ((test v %%kt %%kf) (id %%kt %%kf))
   	          ((test _ %%kt %%kf) (id %%kt %%kf))))
	      (test ok %kt %kf)))))
       (m kt kf)))))
