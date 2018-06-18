;; strings.sld -- cursor-oriented string library
;; Copyright (c) 2012-2015 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; Procedures from Chibi's SRFI 1 implementation

(define (any pred ls . lol)
  (define (any1 pred ls)
    (if (pair? (cdr ls))
        ((lambda (x) (if x x (any1 pred (cdr ls)))) (pred (car ls)))
        (pred (car ls))))
  (define (anyn pred lol)
    (if (every pair? lol)
        ((lambda (x) (if x x (anyn pred (map cdr lol))))
         (apply pred (map car lol)))
        #f))
  (if (null? lol)
      (if (pair? ls) (any1 pred ls) #f)
      (anyn pred (cons ls lol))))

(define (every pred ls . lol)
  (define (every1 pred ls)
    (if (null? (cdr ls))
        (pred (car ls))
        (if (pred (car ls)) (every1 pred (cdr ls)) #f)))
  (if (null? lol)
      (if (pair? ls) (every1 pred ls) #t)
      (not (apply any (lambda xs (not (apply pred xs))) ls lol))))


;; Procedures defined in chibi/string.sld for non-Chibi implementations

(define (string-cursor->index str i) i)
(define (string-index->cursor str i) i)
(define string-cursor? integer?)
(define string-cursor<? <)
(define string-cursor>? >)
(define string-cursor=? =)
(define string-cursor<=? <=)
(define string-cursor>=? >=)
(define string-cursor-ref string-ref)
(define (string-cursor-start s) 0)
(define string-cursor-end string-length)
(define (string-cursor-next s i) (+ i 1))
(define (string-cursor-prev s i) (- i 1))

(define (substring-cursor s start . o)
  (substring s start (if (pair? o) (car o) (string-length s))))

;(define (string-concatenate ls) (apply string-append ls))

(define string-size string-length)

(define (%string-contains a b astart)
 (let ((alen (string-length a))
       (blen (string-length b)))
   (let lp ((i astart))
     (and (<= (+ i blen) alen)
          (if (string=? b (substring a i (+ i blen)))
              i
              (lp (+ i 1)))))))

