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

(define-syntax <>
  (syntax-rules ()
    ((<> . args)
     (syntax-error "invalid use of auxiliary syntax" (<> . args)))))

(define-syntax <...>
  (syntax-rules ()
    ((<...> . args)
     (syntax-error "invalid use of auxiliary syntax" (<...> . args)))))

(define-syntax cut
  (syntax-rules ()
    ((cut slot-or-expr ...)
     (cut-aux (slot-or-expr ...) () ()))
    ((cut . args)
     (syntax-error "invalid cut syntax" (cut . args)))))

(define-syntax cut-aux
  (syntax-rules (<> <...>)
    ((cut-aux () formals body)
     (lambda formals body))
    ((cut-aux (<> slot-or-expr ...) (formal ...) (arg ...))
     (cut-aux (slot-or-expr ...) (formal ... tmp) (arg ... tmp)))
    ((cut-aux (<...>) (formal ...) (arg ...))
     (cut-aux () (formal ... . tmp) (apply arg ... tmp)))
    ((cut-aux (expr slot-or-expr ...) (formal ...) (arg ...))
     (cut-aux (slot-or-expr ...) (formal ...) (arg ... expr)))))

(define-syntax cute
  (syntax-rules ()
    ((cute slot-or-expr ...)
     (cute-aux (slot-or-expr ...) () () () ()))
    ((cute . args)
     (syntax-error "invalid cute syntax" (cute . args)))))

(define-syntax cute-aux
  (syntax-rules (<> <...>)
    ((cute-aux () (tmp ...) (expr ...) formals body)
     (let ((tmp expr) ...)       
       (lambda formals body)))
    ((cute-aux (<> slot-or-expr ...) tmp* expr* (formal ...) (arg ...))
     (cute-aux (slot-or-expr ...) tmp* expr* (formal ... tmp) (arg ... tmp)))
    ((cute-aux (<...>) tmp* expr* (formal ...) (arg ...))
     (cute-aux () tmp* expr* (formal ... . tmp) (apply arg ... tmp)))
    ((cute-aux (expr slot-or-expr ...) (tmp1 ...) (expr1 ...) (formal ...) (arg ...))
     (cute-aux (slot-or-expr ...) (tmp1 ... tmp) (expr1 ... expr) (formal ...) (arg ... tmp)))))
