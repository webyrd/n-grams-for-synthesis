;; -*- coding: utf-8 -*-

;;; Copyright (C) Per Bothner (2017).
;;; Copyright (C) William D Clinger (2016).
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE. 

#|
(import (scheme base)
        (scheme write)
        (scheme char)
        (srfi 140))
|#
(test-begin "strings")

#|
;;; Help functions for testing.

(define (as-string . args)
  (string-concatenate (map (lambda (x)
                              (cond ((string? x) x)
                                   ((char? x) (string x))
                                    (else
                                     (error "as-string: illegal argument" x))))
                            args)))
|#

;;; Unicode is a strong motivation for immutable strings, so we ought
;;; to use at least some non-ASCII strings for testing.
;;; Some systems would blow up if this file were to contain non-ASCII
;;; characters, however, so we have to be careful here.
;;;
;;; FIXME: need more tests with really high code points

(cond-expand ((or sagittarius
                  chibi
                  kawa
                  full-unicode-strings)
              (define ABC
                (list->string (map integer->char
                                   '(#x3b1 #x3b2 #x3b3))))
              (define ABCDEF
                (list->string (map integer->char
                                   '(#x0c0 #x062 #x0c7 #x064 #x0c9 #x066))))
              (define DEFABC
                 (list->string (map integer->char
                                    '(#x064 #x0c9 #x066 #x0c0 #x062 #x0c7))))
              (define eszett (integer->char #xDF))
              (define fuss (string #\F #\u eszett))
              (define chaos0
                (list->string (map integer->char
                                   '(#x39E #x391 #x39F #x3A3))))
              (define chaos1
                (list->string (map integer->char
                                   '(#x3BE #x3B1 #x3BF #x3C2))))
              (define chaos2
                (list->string (map integer->char
                                   '(#x3BE #x3B1 #x3BF #x3C3))))
              (define beyondBMP
                (list->string (map integer->char
                                   '(#x61 #xc0 #x3bf
                                          #x1d441 #x1d113 #x1d110 #x7a)))))
             (else
              (define ABC "abc")
              (define ABCDEF "ABCdef")
              (define DEFABC "defabc")))


;;; Predicates

(test-assert (string? (string)))

(test-assert (not (string? #\a)))

(test-assert (string-null? (string)))

(test-assert (not (string-null? ABC)))

(define (check-istring str)
  (list (istring? str) (string-length str)))

(test-equal '(#t 0) (check-istring ""))
(test-equal '(#t 4) (check-istring "abcd"))
(test-equal '(#t 4) (check-istring (string #\A #\b #\c #\d)))
(test-equal '(#t 3) (check-istring (substring (make-string 4 #\X) 1 4)))
(test-equal '(#f 4) (check-istring (make-string 4 #\X)))
(test-equal '(#f 4) (check-istring (string-copy (make-string 4 #\X))))
(test-equal '(#f 3) (check-istring (string-copy (make-string 4 #\X) 1 4)))
(test-equal '(#t 3) (check-istring (vector->string #(#\x #\y #\z))))
(test-equal '(#t 3) (check-istring (vector->string #(#\x #\y #\z))))
(test-equal '(#t 3) (check-istring (list->string '(#\x #\y #\z))))
(test-equal '(#t 3) (check-istring (reverse-list->string '(#\x #\y #\z))))
(test-equal '(#t 3) (check-istring (utf8->string (string->utf8 "abc"))))
(test-equal '(#t 3) (check-istring (utf16->string (string->utf16 "abc"))))
(test-equal '(#t 3) (check-istring (utf16be->string (string->utf16be "abc"))))
(test-equal '(#t 3) (check-istring (utf16le->string (string->utf16le "abc"))))
(test-equal '(#t 2) (check-istring (string-take "abcd" 2)))
(test-equal '(#t 2) (check-istring (string-drop "abcd" 2)))
(test-equal '(#t 2) (check-istring (string-take-right "abcd" 2)))
(test-equal '(#t 2) (check-istring (string-drop-right "abcd" 2)))
(test-equal '(#t 5) (check-istring (string-pad "abcd" 5)))
(test-equal '(#t 3) (check-istring (string-pad-right "abcd" 3)))
(test-equal '(#t 2) (check-istring (string-trim "  A ")))
(test-equal '(#t 3) (check-istring (string-trim-right "  A ")))
(test-equal '(#t 1) (check-istring (string-trim-both "  A ")))
(test-equal '(#t 3) (check-istring (string-replace "AB" "X" 1 1)))
(test-equal '(#t 3) (check-istring (string-upcase (make-string 3 #\X))))
(test-equal '(#t 3) (check-istring (string-downcase (make-string 3 #\x))))
(test-equal '(#t 3) (check-istring (string-foldcase (make-string 3 #\x))))
(test-equal '(#t 3) (check-istring (string-titlecase (make-string 3 #\X))))
(test-equal '(#t 6) (check-istring (string-append "abcd" "XY")))
(test-equal '(#t 6) (check-istring (string-concatenate (list "abcd" "XY"))))
(test-equal '(#t 6) (check-istring
                     (string-concatenate-reverse  (list "abcd" "XY"))))
(test-equal '(#t 7) (check-istring (string-join (list "abc" "xyz"))))
(test-equal '(#t 3) (check-istring (string-map char-upcase "abc")))
(test-equal '(#t 6) (check-istring (string-repeat "ab" 3)))
(test-equal '(#t 14) (check-istring (xsubstring "abcdef" -4 10)))
(test-equal '(#t 3) (check-istring (cadr (string-split "ab cef" " "))))
(test-expect-fail 1)
(test-equal '(#t 5) (check-istring (symbol->string 'Hello)))

(test-equal #t (string-every (lambda (c) (if (char? c) c #f))
                            (string)))

(test-equal #\c (string-every (lambda (c) (if (char? c) c #f))
                             "abc"))

(test-equal #f (string-every (lambda (c) (if (char>? c #\b) c #f))
                            "abc"))

(test-equal #\c (string-every (lambda (c) (if (char>? c #\b) c #f))
                             "abc" 2))

(test-equal #t (string-every (lambda (c) (if (char>? c #\b) c #f))
                             "abc" 1 1))

(test-equal #f (string-any (lambda (c) (if (char? c) c #f))
                          (string)))

(test-equal #\a (string-any (lambda (c) (if (char? c) c #f))
                           "abc"))

(test-equal #\c (string-any (lambda (c) (if (char>? c #\b) c #f))
                            "abc"))

(test-equal #\c (string-any (lambda (c) (if (char>? c #\b) c #f))
                            "abc" 2))

(test-equal #f (string-any (lambda (c) (if (char>? c #\b) c #f))
                           "abc" 0 2))


(test-equal #t (string-every (lambda (c) (if (char? c) c #f)) ""))

(test-equal #\c (string-every (lambda (c) (if (char? c) c #f)) "abc"))

(test-equal #f (string-every (lambda (c) (if (char>? c #\b) c #f)) "abc"))

(test-equal #\c (string-every (lambda (c) (if (char>? c #\b) c #f)) "abc" 2))

(test-equal #t (string-every (lambda (c) (if (char>? c #\b) c #f)) "abc" 1 1))

(test-equal #f (string-any (lambda (c) (if (char? c) c #f)) ""))

(test-equal #\a (string-any (lambda (c) (if (char? c) c #f)) "abc"))

(test-equal #\c (string-any (lambda (c) (if (char>? c #\b) c #f)) "abc"))

(test-equal #\c (string-any (lambda (c) (if (char>? c #\b) c #f)) "abc" 2))

(test-equal #f (string-any (lambda (c) (if (char>? c #\b) c #f)) "abc" 0 2))

;;; Constructors

(test-equal ""
            (string-tabulate (lambda (i)
                               (integer->char (+ i (char->integer #\a))))
                             0))

(let ((r (string-tabulate (lambda (i)
                            (integer->char (+ i (char->integer #\a))))
                          3)))
  (test-equal '(#t 3) (check-istring r))
  (test-equal "abc" r))

(let* ((p (open-input-string "abc"))
       (r (string-unfold eof-object?
                         values
                         (lambda (x) (read-char p))
                         (read-char p))))
  (test-equal '(#t 3) (check-istring r))
  (test-equal "abc" r))

(test-equal "" (string-unfold null? car cdr '()))

(test-equal "abc"
              (string-unfold null? car cdr (string->list "abc")))

(test-equal "def"
              (string-unfold null? car cdr '() "def"))

(test-equal "defabcG"
              (string-unfold null?
                           car
                           cdr
                           (string->list "abc")
                           "def"
                           (lambda (x) (if (null? x) (string #\G) ""))))

(test-equal "" (string-unfold-right null? car cdr '()))

(test-equal "cba"
              (string-unfold-right null? car cdr (string->list "abc")))

(test-equal "def"
              (string-unfold-right null? car cdr '() "def"))
(test-equal '(#t 3)
            (check-istring (string-unfold-right null? car cdr '() "def")))

(test-equal "Gcbadef"
              (string-unfold-right null?
                                 car
                                 cdr
                                 (string->list "abc")
                                 "def"
                                 (lambda (x) (if (null? x) (string #\G) ""))))

(test-equal "def"
              (string-unfold null? car cdr '() "def"))

(test-equal "defabcG"
              (string-unfold null?
                           car
                           cdr
                           (string->list "abc")
                           "def"
                           (lambda (x) (if (null? x) "G" ""))))

(test-equal "dabcG"
              (string-unfold null?
                           car
                           cdr
                           (string->list "abc")
                           #\d
                           (lambda (x) (if (null? x) "G" ""))))

(test-equal (string-append "%="
                             (make-string 200 #\*)
                             "A B C D E F G H I J K L M "
                             "N O P Q R S T U V W X Y Z "
                             (make-string (* 200 (- (char->integer #\a)
                                                    (char->integer #\Z)
                                                    1))
                                          #\*)
                             "abcdefghijklmnopqrstuvwxyz"
                             " ")
              (string-unfold (lambda (n) (char>? (integer->char n) #\z))
                           (lambda (n)
                             (let ((c (integer->char n)))
                               (cond ((char<=? #\a c #\z) c)
                                     ((char<=? #\A c #\Z) (string c #\space))
                                     (else (make-string 200 #\*)))))
                           (lambda (n) (+ n 1))
                           (char->integer #\@)
                           "%="
                           (lambda (n) #\space)))

(test-equal "def"
              (string-unfold-right null? car cdr '() "def"))

(test-equal "Gcbadef"
              (string-unfold-right null?
                                 car
                                 cdr
                                 (string->list "abc")
                                 "def"
                                 (lambda (x) (if (null? x) "G" ""))))

(test-equal "Gcbad"
              (string-unfold-right null?
                                 car
                                 cdr
                                 (string->list "abc")
                                 #\d
                                 (lambda (x) (if (null? x) "G" ""))))

(test-equal (string-append " "
                             (list->string
                              (reverse
                               (string->list "abcdefghijklmnopqrstuvwxyz")))
                             (make-string (* 200 (- (char->integer #\a)
                                                    (char->integer #\Z)
                                                    1))
                                          #\*)
                             "Z Y X W V U T S R Q P O N "
                             "M L K J I H G F E D C B A "
                             (make-string 200 #\*)
                             "%=")
              (string-unfold-right
               (lambda (n) (char>? (integer->char n) #\z))
               (lambda (n)
                 (let ((c (integer->char n)))
                   (cond ((char<=? #\a c #\z) c)
                         ((char<=? #\A c #\Z) (string c #\space))
                         (else (make-string 200 #\*)))))
               (lambda (n) (+ n 1))
               (char->integer #\@)
               "%="
               (lambda (n) #\space)))

(test-equal " The English alphabet: abcdefghijklmnopqrstuvwxyz "
              (string-unfold-right (lambda (n) (< n (char->integer #\A)))
                                 (lambda (n)
                                   (char-downcase (integer->char n)))
                                 (lambda (n) (- n 1))
                                 (char->integer #\Z)
                                 #\space
                                 (lambda (n) " The English alphabet: ")))

;;; Conversion

(let ((txt (string #\s #\t #\r)))
  (test-assert (and (string? txt) (string=? txt "str"))))

(test-equal "" (string))

(test-equal "" (substring (string) 0 0))

(test-equal "abc" (string #\a #\b #\c))

(test-equal "" (substring (string #\a #\b #\c) 3 3))

(test-equal "bc" (substring (string #\a #\b #\c) 1 3))


;(test-equal "" (substring "" 0))

(test-equal "" (substring "" 0 0))

(test-equal "" (substring "abc" 3 3))

(test-equal "bc" (substring "abc" 1 3))


(test-equal '#() (string->vector (string)))

;(test-equal '#() (string->vector (string) 0))

(test-equal '#() (string->vector (string) 0 0))

(test-equal '#(#\a #\b #\c) (string->vector (string #\a #\b #\c)))

(test-equal '#() (string->vector (string #\a #\b #\c) 3))

(test-equal '#(#\b #\c) (string->vector (string #\a #\b #\c) 1 3))


(test-equal '#() (string->vector ""))

(test-equal '#() (string->vector "" 0))

(test-equal '#() (string->vector "" 0 0))

(test-equal '#(#\a #\b #\c) (string->vector "abc"))

(test-equal '#() (string->vector "abc" 3))

(test-equal '#(#\b #\c) (string->vector "abc" 1 3))


(test-equal '() (string->list (string)))

(test-equal '() (string->list (string) 0))

(test-equal '() (string->list (string) 0 0))

(test-equal '(#\a #\b #\c) (string->list (string #\a #\b #\c)))

(test-equal '() (string->list (string #\a #\b #\c) 3))

(test-equal '(#\b #\c) (string->list (string #\a #\b #\c) 1 3))


(test-equal '() (string->list ""))

(test-equal '() (string->list "" 0))

(test-equal '() (string->list "" 0 0))

(test-equal '(#\a #\b #\c) (string->list "abc"))

(test-equal '() (string->list "abc" 3))

(test-equal '(#\b #\c) (string->list "abc" 1 3))


(test-equal "" "")

(test-equal "" (substring "" 0 0))
(test-equal "bc" (substring "abc" 1 3))
(test-equal "" (substring "abc" 3 3))
(test-equal "b" (substring "abc" 1 2))
(test-equal "bc" (substring "abc" 1 3))


(test-equal "" (vector->string '#()))
(test-equal "" (vector->string '#() 0))
(test-equal "" (vector->string '#() 0 0))
(test-equal "abc" (vector->string '#(#\a #\b #\c)))
(test-equal "bc" (vector->string '#(#\a #\b #\c) 1))
(test-equal "" (vector->string '#(#\a #\b #\c) 3))
(test-equal "b" (vector->string '#(#\a #\b #\c) 1 2))
(test-equal "bc" (vector->string '#(#\a #\b #\c) 1 3))


(test-equal "" (list->string '()))

#| FIXME TODO
(test-equal "" (list->string '() 0))

(test-equal "" (list->string '() 0 0))

(test-equal "abc" (list->string '(#\a #\b #\c)))

(test-equal "bc" (list->string '(#\a #\b #\c) 1))

(test-equal "" (list->string '(#\a #\b #\c) 3))

(test-equal "b" (list->string '(#\a #\b #\c) 1 2))

(test-equal "bc" (list->string '(#\a #\b #\c) 1 3))
|#

(test-equal "" (reverse-list->string '()))

(test-equal "cba" (reverse-list->string '(#\a #\b #\c)))


(test-equal '#u8(97 98 99)
            (string->utf8 "abc"))

(test-equal '#u8(97 98 99 121 121 121 122 122 122)
            (string->utf8 "xxxabcyyyzzz" 3))

(test-equal '#u8(97 98 99)
            (string->utf8 "xxxabcyyyzzz" 3 6))


(test-equal (cond-expand (big-endian '#u8(254 255 0 97 0 98 0 99))
                         (else '#u8(255 254 97 0 98 0 99 0)))
            (string->utf16 "abc"))

(test-equal (cond-expand (big-endian '#u8(254 255 0 97 0 98 0 99 0 121 0 121 0 121 0 122 0 122 0 122))
                         (else '#u8(255  254 97 0 98 0 99 0 121 0 121 0 121 0 122 0 122 0 122 0)))
            (string->utf16 "xxxabcyyyzzz" 3))

(test-equal (cond-expand (big-endian '#u8(254 255 0 97 0 98 0 99))
                         (else '#u8(255 254 97 0 98 0 99 0)))
            (string->utf16 "xxxabcyyyzzz" 3 6))


(test-equal '#u8(0 97 0 98 0 99)
            (string->utf16be "abc"))

(test-equal '#u8(0 97 0 98 0 99 0 121 0 121 0 121 0 122 0 122 0 122)
            (string->utf16be "xxxabcyyyzzz" 3))

(test-equal '#u8(0 97 0 98 0 99)
            (string->utf16be "xxxabcyyyzzz" 3 6))


(test-equal '#u8(97 0 98 0 99 0)
            (string->utf16le "abc"))

(test-equal '#u8(97 0 98 0 99 0 121 0 121 0 121 0 122 0 122 0 122 0)
            (string->utf16le "xxxabcyyyzzz" 3))

(test-equal '#u8(97 0 98 0 99 0)
            (string->utf16le "xxxabcyyyzzz" 3 6))


(test-equal "abc"
              (utf8->string '#u8(97 98 99)))

(test-equal "abcyyyzzz"
              (utf8->string '#u8(0 1 2 97 98 99 121 121 121 122 122 122) 3))

(test-equal "abc"
              (utf8->string '#u8(41 42 43 97 98 99 100 101 102) 3 6))


(test-equal "abc"
              (utf16->string '#u8(254 255 0 97 0 98 0 99)))

(test-equal "abc"
              (utf16->string '#u8(255 254 97 0 98 0 99 0)))

(test-equal "abc"
              (utf16->string (string->utf16 "abc") 2))

(test-equal "bcdef"
              (utf16->string (string->utf16 "abcdef") 4))

(test-equal "bcd"
              (utf16->string (string->utf16 "abcdef") 4 10))


(test-equal "abc"
              (utf16be->string '#u8(0 97 0 98 0 99)))

(test-equal "bc"
              (utf16be->string (string->utf16be "abc") 2))

(test-equal "bcd"
              (utf16be->string (string->utf16be "abcdef") 2 8))


(test-equal "abc"
              (utf16le->string '#u8(97 0 98 0 99 0)))

(test-equal "bc"
              (utf16le->string (string->utf16le "abc") 2))

(test-equal "bcd"
              (utf16le->string (string->utf16le "abcdef") 2 8))


(cond-expand
 ((or sagittarius
      chibi
      kawa
      full-unicode-strings)

  (test-equal '#u8(97 195 128 206 191
               240 157 145 129 240 157 132 147 240 157 132 144 122)
              (string->utf8 beyondBMP))

  (let ((bv (string->utf16 beyondBMP)))
    (test-assert
     (or (equal? bv
                 '#u8(254 255 0 97 0 192 3 191
                          216 53 220 65 216 52 221 19 216 52 221 16 0 122))
         (equal? bv
                 '#u8(255 254 97 0 192 0 191 3
                          53 216 65 220 52 216 19 221 52 216 16 221 122 0)))))

  (test-equal
   '#u8(0 97 0 192 3 191 216 53 220 65 216 52 221 19 216 52 221 16 0 122)
   (string->utf16be beyondBMP))

  (test-equal
   '#u8(97 0 192 0 191 3 53 216 65 220 52 216 19 221 52 216 16 221 122 0)
       (string->utf16le beyondBMP))

  (test-equal
       beyondBMP
       (utf8->string
        '#u8(97 195 128 206 191
                240 157 145 129 240 157 132 147 240 157 132 144 122)))

  (test-equal beyondBMP (utf16->string (string->utf16 beyondBMP)))

  (test-equal beyondBMP
              (utf16->string (string->utf16 beyondBMP) 2))
  
  (test-equal beyondBMP (utf16be->string (string->utf16be beyondBMP)))

  (test-equal beyondBMP (utf16le->string (string->utf16le beyondBMP)))

  (test-equal (string-append (string (integer->char #xfeff)) "abc")
              (utf16be->string '#u8(254 255 0 97 0 98 0 99)))

  (test-equal (string-append (string (integer->char #xfeff)) "abc")
              (utf16le->string '#u8(255 254 97 0 98 0 99 0)))
)

 (else))

;;; Selection

(test-equal 0 (string-length (string)))

(test-equal 6 (string-length ABCDEF))

(test-equal 1234 (string-length (make-string 1234 (string-ref ABC 0))))


(test-equal #\a (string-ref (string #\a #\b #\c) 0))

(test-equal #\c (string-ref (string #\a #\b #\c) 2))

(test-equal 0 (string-length (string)))

(test-equal 6 (string-length ABCDEF))

(test-equal 1234 (string-length (make-string 1234 (string-ref ABC 0))))



(test-equal #\a (string-ref (string #\a #\b #\c) 0))

(test-equal #\c (string-ref (string #\a #\b #\c) 2))

(test-equal ""
              (substring (string) 0 0))

(test-equal ""
              (substring "abcdef" 0 0))

(test-equal "" (substring "abcdef" 4 4))

(test-equal "" (substring "abcdef" 6 6))

(test-equal "abcd" (substring "abcdef" 0 4))

(test-equal "cde" (substring "abcdef" 2 5))

(test-equal "cdef" (substring "abcdef" 2 6))

(test-equal "abcdef" (substring "abcdef" 0 6))


(test-equal "" (substring (string) 0 0))
(test-equal "" (substring "abcdef" 0 0))

(test-equal "" (substring "abcdef" 4 4))
(test-equal "" (substring "abcdef" 6 6))
(test-equal "abcd" (substring "abcdef" 0 4))
(test-equal "cde" (substring "abcdef" 2 5))
(test-equal "cdef" (substring "abcdef" 2 6))
(test-equal "abcdef" (substring "abcdef" 0 6))


(test-equal "" (substring "" 0 0))
(test-equal "" (substring "abcdef" 0 0))
(test-equal "" (substring "abcdef" 4 4))
(test-equal "" (substring "abcdef" 6 6))
(test-equal "abcd" (substring "abcdef" 0 4))
(test-equal "cde" (substring "abcdef" 2 5))
(test-equal "cdef" (substring "abcdef" 2 6))
(test-equal "abcdef" (substring "abcdef" 0 6))


(test-equal "" (string-copy (string)))

(let* ((txt "abcdef")
       (copy (string-copy txt)))
  (test-equal "abcdef" copy)
  (test-assert (not (eqv? txt copy))))


(test-equal "" (string-copy ""))

(test-equal "abcdef" (string-copy "abcdef"))


(test-equal "" (string-copy (string) 0))
(test-equal "abcdef" (string-copy "abcdef" 0))
(test-equal "ef" (string-copy "abcdef" 4))
(test-equal "" (string-copy "abcdef" 6))


(test-equal "" (string-copy "" 0))
(test-equal "abcdef" (string-copy "abcdef" 0))
(test-equal "ef" (string-copy "abcdef" 4))
(test-equal "" (string-copy "abcdef" 6))


(test-equal "" (string-copy (string) 0 0))
(test-equal "" (string-copy "abcdef" 0 0))
(test-equal "" (string-copy "abcdef" 4 4))

(test-equal "" (string-copy "abcdef" 6 6))
(test-equal "abcd" (string-copy "abcdef" 0 4))
(test-equal "cde" (string-copy "abcdef" 2 5))
(test-equal "cdef" (string-copy "abcdef" 2 6))
(test-equal "abcdef" (string-copy "abcdef" 0 6))


(test-equal ""
              (string-copy "" 0 0))

(test-equal ""
              (string-copy "abcdef" 0 0))

(test-equal ""
              (string-copy "abcdef" 4 4))

(test-equal ""
              (string-copy "abcdef" 6 6))

(test-equal "abcd"
              (string-copy "abcdef" 0 4))

(test-equal "cde"
              (string-copy "abcdef" 2 5))

(test-equal "cdef"
              (string-copy "abcdef" 2 6))

(test-equal "abcdef"
              (string-copy "abcdef" 0 6))


(test-equal "" (string-take (string) 0))

(test-equal "" (string-take "abcdef" 0))

(test-equal "ab" (string-take "abcdef" 2))

(test-equal "" (string-drop "" 0))

(test-equal "abcdef" (string-drop "abcdef" 0))

(test-equal "cdef" (string-drop "abcdef" 2))

(test-equal "" (string-take-right (string) 0))

(test-equal "" (string-take-right "abcdef" 0))

(test-equal "ef" (string-take-right "abcdef" 2))

(test-equal "" (string-drop-right (string) 0))

(test-equal "abcdef"
              (string-drop-right "abcdef" 0))

(test-equal "abcd"
              (string-drop-right "abcdef" 2))


(test-equal "" (string-take "" 0))

(test-equal "" (string-take "abcdef" 0))

(test-equal "ab" (string-take "abcdef" 2))

(test-equal "" (string-drop "" 0))

(test-equal "abcdef" (string-drop "abcdef" 0))

(test-equal "cdef" (string-drop "abcdef" 2))

(test-equal "" (string-take-right "" 0))

(test-equal "" (string-take-right "abcdef" 0))

(test-equal "ef" (string-take-right "abcdef" 2))

(test-equal "" (string-drop-right "" 0))

(test-equal "abcdef" (string-drop-right "abcdef" 0))

(test-equal "abcd" (string-drop-right "abcdef" 2))


(test-equal "" 
              (string-pad "" 0))

(test-equal "     " 
              (string-pad "" 5))

(test-equal "  325" 
              (string-pad "325" 5))

(test-equal "71325" 
              (string-pad "71325" 5))

(test-equal "71325" 
              (string-pad "8871325" 5))

(test-equal "" 
              (string-pad "" 0 #\*))

(test-equal "*****" 
              (string-pad "" 5 #\*))

(test-equal "**325" 
              (string-pad "325" 5 #\*))

(test-equal "71325" 
              (string-pad "71325" 5 #\*))

(test-equal "71325" 
              (string-pad "8871325" 5 #\*))

(test-equal "" 
              (string-pad "" 0 #\* 0))

(test-equal "*****" 
              (string-pad "" 5 #\* 0))

(test-equal "**325" 
              (string-pad "325" 5 #\* 0))

(test-equal "71325" 
              (string-pad "71325" 5 #\* 0))

(test-equal "71325" 
              (string-pad "8871325" 5 #\* 0))

(test-equal "***25" 
              (string-pad "325" 5 #\* 1))

(test-equal "*1325" 
              (string-pad "71325" 5 #\* 1))

(test-equal "71325" 
              (string-pad "8871325" 5 #\* 1))

(test-equal "" 
              (string-pad "" 0 #\* 0 0))

(test-equal "*****" 
              (string-pad "" 5 #\* 0 0))

(test-equal "**325" 
              (string-pad "325" 5 #\* 0 3))

(test-equal "**713" 
              (string-pad "71325" 5 #\* 0 3))

(test-equal "**887" 
              (string-pad "8871325" 5 #\* 0 3))

(test-equal "***25" 
              (string-pad "325" 5 #\* 1 3))

(test-equal "**132" 
              (string-pad "71325" 5 #\* 1 4))

(test-equal "*8713" 
              (string-pad "8871325" 5 #\* 1 5))

(test-equal "" 
              (string-pad-right "" 0))

(test-equal "     " 
              (string-pad-right "" 5))

(test-equal "325  " 
              (string-pad-right "325" 5))

(test-equal "71325" 
              (string-pad-right "71325" 5))

(test-equal "88713" 
              (string-pad-right "8871325" 5))

(test-equal "" 
              (string-pad-right "" 0 #\*))

(test-equal "*****" 
              (string-pad-right "" 5 #\*))

(test-equal "325**" 
              (string-pad-right "325" 5 #\*))

(test-equal "71325" 
              (string-pad-right "71325" 5 #\*))

(test-equal "88713" 
              (string-pad-right "8871325" 5 #\*))

(test-equal "" 
              (string-pad-right "" 0 #\* 0))

(test-equal "*****" 
              (string-pad-right "" 5 #\* 0))

(test-equal "325**" 
              (string-pad-right "325" 5 #\* 0))

(test-equal "71325" 
              (string-pad-right "71325" 5 #\* 0))

(test-equal "88713" 
              (string-pad-right "8871325" 5 #\* 0))

(test-equal "25***" 
              (string-pad-right "325" 5 #\* 1))

(test-equal "1325*" 
              (string-pad-right "71325" 5 #\* 1))

(test-equal "87132" 
              (string-pad-right "8871325" 5 #\* 1))

(test-equal "" 
              (string-pad-right "" 0 #\* 0 0))

(test-equal "*****" 
              (string-pad-right "" 5 #\* 0 0))

(test-equal "325**" 
              (string-pad-right "325" 5 #\* 0 3))

(test-equal "713**" 
              (string-pad-right "71325" 5 #\* 0 3))

(test-equal "887**" 
              
              (string-pad-right "8871325" 5 #\* 0 3))

(test-equal "25***" 
              (string-pad-right "325" 5 #\* 1 3))

(test-equal "132**" 
              (string-pad-right "71325" 5 #\* 1 4))

(test-equal "8713*" 
              
              (string-pad-right "8871325" 5 #\* 1 5))


(test-equal "" (string-pad "" 0))

(test-equal "     " (string-pad "" 5))

(test-equal "  325" (string-pad "325" 5))

(test-equal "71325" (string-pad "71325" 5))

(test-equal "71325" (string-pad "8871325" 5))

(test-equal "" (string-pad "" 0 #\*))

(test-equal "*****" (string-pad "" 5 #\*))

(test-equal "**325" (string-pad "325" 5 #\*))

(test-equal "71325" (string-pad "71325" 5 #\*))

(test-equal "71325" (string-pad "8871325" 5 #\*))

(test-equal "" (string-pad "" 0 #\* 0))

(test-equal "*****" (string-pad "" 5 #\* 0))

(test-equal "**325" (string-pad "325" 5 #\* 0))

(test-equal "71325" (string-pad "71325" 5 #\* 0))

(test-equal "71325" (string-pad "8871325" 5 #\* 0))

(test-equal "***25" (string-pad "325" 5 #\* 1))

(test-equal "*1325" (string-pad "71325" 5 #\* 1))

(test-equal "71325" (string-pad "8871325" 5 #\* 1))

(test-equal "" (string-pad "" 0 #\* 0 0))

(test-equal "*****" (string-pad "" 5 #\* 0 0))

(test-equal "**325" (string-pad "325" 5 #\* 0 3))

(test-equal "**713" (string-pad "71325" 5 #\* 0 3))

(test-equal "**887" (string-pad "8871325" 5 #\* 0 3))

(test-equal "***25" (string-pad "325" 5 #\* 1 3))

(test-equal "**132" (string-pad "71325" 5 #\* 1 4))

(test-equal "*8713" (string-pad "8871325" 5 #\* 1 5))

(test-equal "" (string-pad-right "" 0))

(test-equal "     " (string-pad-right "" 5))

(test-equal "325  " (string-pad-right "325" 5))

(test-equal "71325" (string-pad-right "71325" 5))

(test-equal "88713" (string-pad-right "8871325" 5))

(test-equal "" (string-pad-right "" 0 #\*))

(test-equal "*****" (string-pad-right "" 5 #\*))

(test-equal "325**" (string-pad-right "325" 5 #\*))

(test-equal "71325" (string-pad-right "71325" 5 #\*))

(test-equal "88713" (string-pad-right "8871325" 5 #\*))

(test-equal "" (string-pad-right "" 0 #\* 0))

(test-equal "*****" (string-pad-right "" 5 #\* 0))

(test-equal "325**" (string-pad-right "325" 5 #\* 0))

(test-equal "71325" (string-pad-right "71325" 5 #\* 0))

(test-equal "88713" (string-pad-right "8871325" 5 #\* 0))

(test-equal "25***" (string-pad-right "325" 5 #\* 1))

(test-equal "1325*" (string-pad-right "71325" 5 #\* 1))

(test-equal "87132" (string-pad-right "8871325" 5 #\* 1))

(test-equal "" (string-pad-right "" 0 #\* 0 0))

(test-equal "*****" (string-pad-right "" 5 #\* 0 0))

(test-equal "325**" (string-pad-right "325" 5 #\* 0 3))

(test-equal "713**" (string-pad-right "71325" 5 #\* 0 3))

(test-equal "887**" (string-pad-right "8871325" 5 #\* 0 3))

(test-equal "25***" (string-pad-right "325" 5 #\* 1 3))

(test-equal "132**" (string-pad-right "71325" 5 #\* 1 4))

(test-equal "8713*" (string-pad-right "8871325" 5 #\* 1 5))


(test-equal ""
              (string-trim ""))

(test-equal "a  b  c  "
              (string-trim "  a  b  c  "))

(test-equal ""
              (string-trim "" char-whitespace?))

(test-equal "a  b  c  "
              (string-trim "  a  b  c  " char-whitespace?))

(test-equal ""
              (string-trim "  a  b  c  " char?))

(test-equal ""
              (string-trim "" char-whitespace? 0))

(test-equal "a  b  c  "
              (string-trim "  a  b  c  " char-whitespace? 0))

(test-equal ""
              (string-trim "  a  b  c  " char? 0))

(test-equal "b  c  "
              (string-trim "  a  b  c  " char-whitespace? 3))

(test-equal ""
              (string-trim "  a  b  c  " char? 3))

(test-equal ""
              (string-trim "  a  b  c  " char? 0 11))

(test-equal "b  c  "
              (string-trim "  a  b  c  "
                            char-whitespace? 3 11))

(test-equal "" (string-trim "  a  b  c  " char? 3 11))

(test-equal ""
              (string-trim "  a  b  c  " char? 0 8))

(test-equal "b  "
              (string-trim "  a  b  c  "
                            char-whitespace? 3 8))

(test-equal ""
              (string-trim "  a  b  c  " char? 3 8))

(test-equal ""
              (string-trim-right ""))

(test-equal "  a  b  c" (string-trim-right "  a  b  c  "))

(test-equal "" (string-trim-right "" char-whitespace?))

(test-equal "  a  b  c"
            (string-trim-right "  a  b  c  " char-whitespace?))

(test-equal ""
            (string-trim-right "  a  b  c  " char?))

(test-equal ""
              (string-trim-right "" char-whitespace? 0))

(test-equal "  a  b  c"
              (string-trim-right "  a  b  c  "
                                  char-whitespace? 0))

(test-equal ""
              (string-trim-right "  a  b  c  " char? 0))

(test-equal "  b  c"
              (string-trim-right "  a  b  c  "
                                  char-whitespace? 3))

(test-equal ""
              (string-trim-right "  a  b  c  " char? 3))

(test-equal ""
              (string-trim-right "  a  b  c  " char? 0 11))

(test-equal "  b  c"
              (string-trim-right "  a  b  c  "
                                  char-whitespace? 3 11))

(test-equal ""
              (string-trim-right "  a  b  c  " char? 3 11))

(test-equal ""
              (string-trim-right "  a  b  c  " char? 0 8))

(test-equal "  b"
              (string-trim-right "  a  b  c  "
                                  char-whitespace? 3 8))

(test-equal ""
              (string-trim-right "  a  b  c  " char? 3 8))

(test-equal ""
              (string-trim-both ""))

(test-equal "a  b  c"
              (string-trim-both "  a  b  c  "))

(test-equal ""
              (string-trim-both "" char-whitespace?))

(test-equal "a  b  c"
              (string-trim-both "  a  b  c  "
                                 char-whitespace?))

(test-equal ""
              (string-trim-both "  a  b  c  " char?))

(test-equal ""
              (string-trim-both "" char-whitespace? 0))

(test-equal "a  b  c"
              (string-trim-both "  a  b  c  "
                                 char-whitespace? 0))

(test-equal ""
              (string-trim-both "  a  b  c  " char? 0))

(test-equal "b  c"
              (string-trim-both "  a  b  c  "
                                 char-whitespace? 3))

(test-equal ""
              (string-trim-both "  a  b  c  " char? 3))

(test-equal ""
              (string-trim-both "  a  b  c  " char? 0 11))

(test-equal "b  c"
              (string-trim-both "  a  b  c  "
                                 char-whitespace? 3 11))

(test-equal ""
              (string-trim-both "  a  b  c  " char? 3 11))

(test-equal ""
              (string-trim-both "  a  b  c  " char? 0 8))

(test-equal "b"
              (string-trim-both "  a  b  c  "
                                 char-whitespace? 3 8))

(test-equal ""
              (string-trim-both "  a  b  c  " char? 3 8))

(test-equal ""
              (string-trim ""))

(test-equal "a  b  c  "
              (string-trim "  a  b  c  "))

(test-equal ""
              (string-trim "" char-whitespace?))

(test-equal "a  b  c  "
              (string-trim "  a  b  c  " char-whitespace?))

(test-equal ""
              (string-trim "  a  b  c  " char?))

(test-equal ""
              (string-trim "" char-whitespace? 0))

(test-equal "a  b  c  "
              (string-trim "  a  b  c  " char-whitespace? 0))

(test-equal ""
              (string-trim "  a  b  c  " char? 0))

(test-equal "b  c  "
              (string-trim "  a  b  c  " char-whitespace? 3))

(test-equal ""
              (string-trim "  a  b  c  " char? 3))

(test-equal ""
              (string-trim "  a  b  c  " char? 0 11))

(test-equal "b  c  "
              (string-trim "  a  b  c  " char-whitespace? 3 11))

(test-equal ""
              (string-trim "  a  b  c  " char? 3 11))

(test-equal ""
              (string-trim "  a  b  c  " char? 0 8))

(test-equal "b  "
              (string-trim "  a  b  c  " char-whitespace? 3 8))

(test-equal ""
              (string-trim "  a  b  c  " char? 3 8))


(test-equal ""
              (string-trim-right ""))

(test-equal "  a  b  c"
              (string-trim-right "  a  b  c  "))

(test-equal ""
              (string-trim-right "" char-whitespace?))

(test-equal "  a  b  c"
              (string-trim-right "  a  b  c  " char-whitespace?))

(test-equal ""
              (string-trim-right "  a  b  c  " char?))

(test-equal ""
              (string-trim-right "" char-whitespace? 0))

(test-equal "  a  b  c"
              (string-trim-right "  a  b  c  " char-whitespace? 0))

(test-equal ""
              (string-trim-right "  a  b  c  " char? 0))

(test-equal "  b  c"
              (string-trim-right "  a  b  c  " char-whitespace? 3))

(test-equal ""
              (string-trim-right "  a  b  c  " char? 3))

(test-equal ""
              (string-trim-right "  a  b  c  " char? 0 11))

(test-equal "  b  c"
              (string-trim-right "  a  b  c  " char-whitespace? 3 11))

(test-equal ""
              (string-trim-right "  a  b  c  " char? 3 11))

(test-equal ""
              (string-trim-right "  a  b  c  " char? 0 8))

(test-equal "  b"
              (string-trim-right "  a  b  c  " char-whitespace? 3 8))

(test-equal ""
              (string-trim-right "  a  b  c  " char? 3 8))


(test-equal ""
              (string-trim-both ""))

(test-equal "a  b  c"
              (string-trim-both "  a  b  c  "))

(test-equal ""
              (string-trim-both "" char-whitespace?))

(test-equal "a  b  c"
              (string-trim-both "  a  b  c  " char-whitespace?))

(test-equal ""
              (string-trim-both "  a  b  c  " char?))

(test-equal ""
              (string-trim-both "" char-whitespace? 0))

(test-equal "a  b  c"
              (string-trim-both "  a  b  c  " char-whitespace? 0))

(test-equal ""
              (string-trim-both "  a  b  c  " char? 0))

(test-equal "b  c"
              (string-trim-both "  a  b  c  " char-whitespace? 3))

(test-equal ""
              (string-trim-both "  a  b  c  " char? 3))

(test-equal ""
              (string-trim-both "  a  b  c  " char? 0 11))

(test-equal "b  c"
              (string-trim-both "  a  b  c  " char-whitespace? 3 11))

(test-equal ""
              (string-trim-both "  a  b  c  " char? 3 11))

(test-equal ""
              (string-trim-both "  a  b  c  " char? 0 8))

(test-equal "b"
              (string-trim-both "  a  b  c  " char-whitespace? 3 8))

(test-equal ""
              (string-trim-both "  a  b  c  " char? 3 8))

;;; Replacement

(test-equal "It's lots of fun to code it up in Scheme."
              (string-replace "It's easy to code it up in Scheme."
                               "lots of fun"
                               5 9))

(test-equal "The miserable perl programmer endured daily ridicule."
              (string-replace "The TCL programmer endured daily ridicule."
                               "another miserable perl drone"
                               4 7 8 22))

(test-equal "It's really easy to code it up in Scheme."
              (string-replace "It's easy to code it up in Scheme."
                               "really "
                               5 5))

(test-equal "Runs in O(1) time." ; for strings (using sample implementations)
              (string-replace "Runs in O(n) time." (string #\1) 10 11))

;;; Comparison
;;;
;;; The comparison tests aren't perfectly black-box because the
;;; specification of these comparison procedures allows them to
;;; use an ordering other than the usual lexicographic ordering.
;;; The sample implementations use lexicographic ordering, however,
;;; and a test program that discourages implementations from using
;;; orderings that differ from the usual on such simple cases is
;;; probably doing a public service.

(test-assert (string=? "Strasse" "Strasse"))

(test-assert (string=? "Strasse" "Strasse" "Strasse"))

(test-equal #f (string<? "z" "z"))
(test-assert (string<? "z" "zz"))
(test-equal #f (string<? "z" "Z"))
(test-assert (string<=? "z" "zz"))
(test-equal #f (string<=? "z" "Z"))
(test-assert (string<=? "z" "z"))

(test-equal #f (string<? "z" "z"))
(test-equal #f (string>? "z" "zz"))
(test-equal #t (string>? "z" "Z"))
(test-equal #f (string>=? "z" "zz"))
(test-equal #t (string>=? "z" "Z"))
(test-assert (string>=? "z" "z"))


(let* ((w "a")
       (x "abc")
       (y "def")
       (z (string #\a #\b #\c)))

  (test-equal (string=? x y z)                           #f)
  (test-equal (string=? x x z)                           #t)
  (test-equal (string=? w x y)                           #f)
  (test-equal (string=? y x w)                           #f)

  (test-equal (string<? x y z)                           #f)
  (test-equal (string<? x x z)                           #f)
  (test-equal (string<? w x y)                           #t)
  (test-equal (string<? y x w)                           #f)

  (test-equal (string>? x y z)                           #f)
  (test-equal (string>? x x z)                           #f)
  (test-equal (string>? w x y)                           #f)
  (test-equal (string>? y x w)                           #t)

  (test-equal (string<=? x y z)                          #f)
  (test-equal (string<=? x x z)                          #t)
  (test-equal (string<=? w x y)                          #t)
  (test-equal (string<=? y x w)                          #f)

  (test-equal (string>=? x y z)                          #f)
  (test-equal (string>=? x x z)                          #t)
  (test-equal (string>=? w x y)                          #f)
  (test-equal (string>=? y x w)                          #t)


  (test-equal (string=? x x)                             #t)
  (test-equal (string=? w x)                             #f)
  (test-equal (string=? y x)                             #f)

  (test-equal (string<? x x)                             #f)
  (test-equal (string<? w x)                             #t)
  (test-equal (string<? y x)                             #f)

  (test-equal (string>? x x)                             #f)
  (test-equal (string>? w x)                             #f)
  (test-equal (string>? y x)                             #t)

  (test-equal (string<=? x x)                            #t)
  (test-equal (string<=? w x)                            #t)
  (test-equal (string<=? y x)                            #f)

  (test-equal (string>=? x x)                            #t)
  (test-equal (string>=? w x)                            #f)
  (test-equal (string>=? y x)                            #t)
)

(test-equal #t (string-ci<? "a" "Z"))
(test-equal #t (string-ci<? "A" "z"))
(test-equal #f (string-ci<? "Z" "a"))
(test-equal #f (string-ci<? "z" "A"))
(test-equal #f (string-ci<? "z" "Z"))
(test-equal #f (string-ci<? "Z" "z"))
(test-equal #f (string-ci>? "a" "Z"))
(test-equal #f (string-ci>? "A" "z"))
(test-equal #t (string-ci>? "Z" "a"))
(test-equal #t (string-ci>? "z" "A"))
(test-equal #f (string-ci>? "z" "Z"))
(test-equal #f (string-ci>? "Z" "z"))
(test-equal #t (string-ci=? "z" "Z"))
(test-equal #f (string-ci=? "z" "a"))
(test-equal #t (string-ci<=? "a" "Z"))
(test-equal #t (string-ci<=? "A" "z"))
(test-equal #f (string-ci<=? "Z" "a"))
(test-equal #f (string-ci<=? "z" "A"))
(test-equal #t (string-ci<=? "z" "Z"))
(test-equal #t (string-ci<=? "Z" "z"))
(test-equal #f (string-ci>=? "a" "Z"))
(test-equal #f (string-ci>=? "A" "z"))
(test-equal #t (string-ci>=? "Z" "a"))
(test-equal #t (string-ci>=? "z" "A"))
(test-equal #t (string-ci>=? "z" "Z"))
(test-equal #t (string-ci>=? "Z" "z"))

;;; The full-unicode feature doesn't imply full Unicode in strings,
;;; so these tests might fail even in a conforming implementation.
;;; Implementations that support full Unicode strings often have
;;; this feature, however, even though it isn't listed in the R7RS.

(cond-expand
 (full-unicode
  (test-equal #f (string=? ABCDEF DEFABC))
  (test-equal #f (string=? DEFABC ABCDEF))
  (test-equal #t (string=? DEFABC DEFABC))

  (test-equal #f (string<? ABCDEF DEFABC))
  (test-equal #t (string<? DEFABC ABCDEF))
  (test-equal #f (string<? DEFABC DEFABC))

  (test-equal #t (string>? ABCDEF DEFABC))
  (test-equal #f (string>? DEFABC ABCDEF))
  (test-equal #f (string>? DEFABC DEFABC))

  (test-equal #f (string<=? ABCDEF DEFABC))
  (test-equal #t (string<=? DEFABC ABCDEF))
  (test-equal #t (string<=? DEFABC DEFABC))

  (test-equal #t (string>=? ABCDEF DEFABC))
  (test-equal #f (string>=? DEFABC ABCDEF))
  (test-equal #t (string>=? DEFABC DEFABC))

  (test-equal #f (string=? "Fuss" fuss))
  (test-equal #f (string=? "Fuss" "Fuss" fuss))
  (test-equal #f (string=? "Fuss" fuss "Fuss"))
  (test-equal #f (string=? fuss "Fuss" "Fuss"))
  (test-equal #t (string<? "z" (string eszett)))
  (test-equal #f (string<? (string eszett) "z"))
  (test-equal #t (string<=? "z" (string eszett)))
  (test-equal #f (string<=? (string eszett) "z"))
  (test-equal #f (string>? "z" (string eszett)))
  (test-equal #t (string>? (string eszett) "z"))
  (test-equal #f (string>=? "z" (string eszett)))
  (test-equal #t (string>=? (string eszett) "z"))
  (test-assert (string-ci=? fuss "Fuss"))
  (test-assert (string-ci=? fuss "FUSS"))
  (test-assert (string-ci=? chaos0 chaos1 chaos2)))
 (else))


;;; Prefixes and suffixes

(test-equal 0 (string-prefix-length ABC ABCDEF))

(test-equal 0 (string-prefix-length ABCDEF ABC))

(test-equal 0 (string-prefix-length ABCDEF DEFABC))

(test-equal 6 (string-prefix-length DEFABC DEFABC))

(test-equal 0 (string-prefix-length "" ""))

(test-equal 0 (string-prefix-length "" "aabbccddee"))

(test-equal 0 (string-prefix-length "aisle" ""))

(test-equal 0 (string-prefix-length "" "aabbccddee"))

(test-equal 1 (string-prefix-length "aisle" "aabbccddee"))

(test-equal 0 (string-prefix-length "bail" "aabbccddee"))

(test-equal 4 (string-prefix-length "prefix" "preface"))

(test-equal 0 (string-prefix-length "" "" 0))

(test-equal 0 (string-prefix-length "" "aabbccddee" 0))

(test-equal 0 (string-prefix-length "aisle" "" 0))

(test-equal 1 (string-prefix-length "aisle" "aabbccddee" 0))

(test-equal 0 (string-prefix-length "bail" "aabbccddee" 0))

(test-equal 4 (string-prefix-length "prefix" "preface" 0))

(test-equal 0 (string-prefix-length "aisle" "" 1))

(test-equal 0 (string-prefix-length "aisle" "aabbccddee" 1))

(test-equal 1 (string-prefix-length "bail" "aabbccddee" 1))

(test-equal 0 (string-prefix-length "prefix" "preface" 1))

(test-equal 0 (string-prefix-length "" "" 0 0))

(test-equal 0 (string-prefix-length "" "aabbccddee" 0 0))

(test-equal 0 (string-prefix-length "aisle" "" 0 4))

(test-equal 1 (string-prefix-length "aisle" "aabbccddee" 0 4))

(test-equal 0 (string-prefix-length "bail" "aabbccddee" 0 1))

(test-equal 0 (string-prefix-length "aisle" "" 1 4))

(test-equal 0 (string-prefix-length "aisle" "aabbccddee" 1 4))

(test-equal 1 (string-prefix-length "bail" "aabbccddee" 1 4))

(test-equal 0 (string-prefix-length "prefix" "preface" 1 5))

(test-equal 0 (string-prefix-length "" "" 0 0 0))

(test-equal 0 (string-prefix-length "" "aabbccddee" 0 0 0))

(test-equal 0 (string-prefix-length "aisle" "" 0 4 0))

(test-equal 0 (string-prefix-length "aisle" "aabbccddee" 0 4 2))

(test-equal 1 (string-prefix-length "bail" "aabbccddee" 0 1 2))

(test-equal 0 (string-prefix-length "prefix" "preface" 0 5 1))

(test-equal 0 (string-prefix-length "aisle" "" 1 4 0))

(test-equal 0 (string-prefix-length "aisle" "aabbccddee" 1 4 3))

(test-equal 0 (string-prefix-length "bail" "aabbccddee" 1 4 3))

(test-equal 3 (string-prefix-length "prefix" "preface" 1 5 1))

(test-equal 0 (string-prefix-length "" "" 0 0 0 0))

(test-equal 0 (string-prefix-length "" "aabbccddee" 0 0 0 0))

(test-equal 0 (string-prefix-length "aisle" "" 0 4 0 0))

(test-equal 0 (string-prefix-length "aisle" "aabbccddee" 0 4 2 10))

(test-equal 1 (string-prefix-length "bail" "aabbccddee" 0 1 2 10))

(test-equal 0 (string-prefix-length "prefix" "preface" 0 5 1 6))

(test-equal 0 (string-prefix-length "aisle" "" 1 4 0 0))

(test-equal 0 (string-prefix-length "aisle" "aabbccddee" 1 4 3 3))

(test-equal 0 (string-prefix-length "bail" "aabbccddee" 1 4 3 6))

(test-equal 3 (string-prefix-length "prefix" "preface" 1 5 1 7))


(test-equal 0 (string-suffix-length ABC ABCDEF))

(test-equal 0 (string-suffix-length ABCDEF ABC))

(test-equal 0 (string-suffix-length ABCDEF DEFABC))

(test-equal 6 (string-suffix-length DEFABC DEFABC))

(test-equal 0 (string-suffix-length "" ""))

(test-equal 0 (string-suffix-length "" "aabbccddee"))

(test-equal 0 (string-suffix-length "aisle" ""))

(test-equal 0 (string-suffix-length "" "aabbccddee"))

(test-equal 1 (string-suffix-length "aisle" "aabbccddee"))

(test-equal 0 (string-suffix-length "bail" "aabbccddee"))

(test-equal 3 (string-suffix-length "place" "preface"))

(test-equal 0 (string-suffix-length "" "" 0))

(test-equal 0 (string-suffix-length "" "aabbccddee" 0))

(test-equal 0 (string-suffix-length "aisle" "" 0))

(test-equal 1 (string-suffix-length "aisle" "aabbccddee" 0))

(test-equal 0 (string-suffix-length "bail" "aabbccddee" 0))

(test-equal 3 (string-suffix-length "place" "preface" 0))

(test-equal 0 (string-suffix-length "aisle" "" 1))

(test-equal 1 (string-suffix-length "aisle" "aabbccddee" 1))

(test-equal 0 (string-suffix-length "bail" "aabbccddee" 1))

(test-equal 3 (string-suffix-length "place" "preface" 1))

(test-equal 0 (string-suffix-length "" "" 0 0))

(test-equal 0 (string-suffix-length "" "aabbccddee" 0 0))

(test-equal 0 (string-suffix-length "aisle" "" 0 4))

(test-equal 0 (string-suffix-length "aisle" "aabbccddee" 0 4))

(test-equal 0 (string-suffix-length "bail" "aabbccddee" 0 1))

(test-equal 0 (string-suffix-length "aisle" "" 1 4))

(test-equal 0 (string-suffix-length "aisle" "aabbccddee" 1 4))

(test-equal 1 (string-suffix-length "aisle" "aabbccddee" 1 5))

(test-equal 0 (string-suffix-length "bail" "aabbccddee" 1 4))

(test-equal 3 (string-suffix-length "place" "preface" 1 5))

(test-equal 0 (string-suffix-length "" "" 0 0 0))

(test-equal 0 (string-suffix-length "" "aabbccddee" 0 0 0))

(test-equal 0 (string-suffix-length "aisle" "" 0 4 0))

(test-equal 0 (string-suffix-length "aisle" "aabbccddee" 0 4 2))

(test-equal 0 (string-suffix-length "bail" "aabbccddee" 0 1 2))

(test-equal 3 (string-suffix-length "place" "preface" 0 5 1))

(test-equal 0 (string-suffix-length "aisle" "" 1 4 0))

(test-equal 0 (string-suffix-length "aisle" "aabbccddee" 1 4 3))

(test-equal 0 (string-suffix-length "bail" "aabbccddee" 1 4 3))

(test-equal 3 (string-suffix-length "place" "preface" 1 5 1))

(test-equal 0 (string-suffix-length "" "" 0 0 0 0))

(test-equal 0 (string-suffix-length "" "aabbccddee" 0 0 0 0))

(test-equal 0 (string-suffix-length "aisle" "" 0 4 0 0))

(test-equal 1 (string-suffix-length "aisle" "aabbccddee" 0 5 2 10))

(test-equal 1 (string-suffix-length "bail" "aabbccddee" 0 1 2 4))

(test-equal 0 (string-suffix-length "place" "preface" 0 5 1 6))

(test-equal 2 (string-suffix-length "place" "preface" 0 4 1 6))

(test-equal 0 (string-suffix-length "aisle" "" 1 4 0 0))

(test-equal 0 (string-suffix-length "aisle" "aabbccddee" 1 4 3 3))

(test-equal 0 (string-suffix-length "bail" "aabbccddee" 1 4 3 6))

(test-equal 3 (string-suffix-length "place" "preface" 1 5 1 7))


(test-equal #f (string-prefix? ABC ABCDEF))

(test-equal #f (string-prefix? ABCDEF ABC))

(test-equal #f (string-prefix? ABCDEF DEFABC))

(test-equal #t (string-prefix? DEFABC DEFABC))

(test-equal #t (string-prefix? "" ""))

(test-equal #t (string-prefix? "" "abc"))

(test-equal #t (string-prefix? "a" "abc"))

(test-equal #f (string-prefix? "c" "abc"))

(test-equal #t (string-prefix? "ab" "abc"))

(test-equal #f (string-prefix? "ac" "abc"))

(test-equal #t (string-prefix? "abc" "abc"))

(test-equal #f (string-suffix? ABC ABCDEF))

(test-equal #f (string-suffix? ABCDEF ABC))

(test-equal #f (string-suffix? ABCDEF DEFABC))

(test-equal #t (string-suffix? DEFABC DEFABC))

(test-equal #t (string-suffix? "" ""))

(test-equal #t (string-suffix? "" "abc"))

(test-equal #f (string-suffix? "a" "abc"))

(test-equal #t (string-suffix? "c" "abc"))

(test-equal #f (string-suffix? "ac" "abc"))

(test-equal #t (string-suffix? "bc" "abc"))

(test-equal #t (string-suffix? "abc" "abc"))

(test-equal #t (string-prefix? "" "" 0))

(test-equal #t (string-prefix? "" "abc" 0))

(test-equal #t (string-prefix? "a" "abc" 0))

(test-equal #f (string-prefix? "c" "abc" 0))

(test-equal #t (string-prefix? "ab" "abc" 0))

(test-equal #f (string-prefix? "ac" "abc" 0))

(test-equal #t (string-prefix? "abc" "abc" 0))

(test-equal #t (string-suffix? "" "" 0))

(test-equal #t (string-suffix? "" "abc" 0))

(test-equal #f (string-suffix? "a" "abc" 0))

(test-equal #t (string-suffix? "c" "abc" 0))

(test-equal #f (string-suffix? "ac" "abc" 0))

(test-equal #t (string-suffix? "bc" "abc" 0))

(test-equal #t (string-suffix? "abc" "abc" 0))

(test-equal #t (string-prefix? "ab" "abc" 2))

(test-equal #t (string-prefix? "ac" "abc" 2))

(test-equal #f (string-prefix? "abc" "abc" 2))

(test-equal #t (string-suffix? "ac" "abc" 2))

(test-equal #t (string-suffix? "bc" "abc" 2))

(test-equal #t (string-suffix? "abc" "abc" 2))


(test-equal #t (string-prefix? "" "" 0 0))

(test-equal #t (string-prefix? "" "abc" 0 0))

(test-equal #t (string-prefix? "a" "abc" 0 0))

(test-equal #f (string-prefix? "c" "abc" 0 1))

(test-equal #t (string-prefix? "ab" "abc" 0 1))

(test-equal #t (string-prefix? "ab" "abc" 0 2))

(test-equal #f (string-prefix? "ac" "abc" 0 2))

(test-equal #t (string-prefix? "abc" "abc" 0 3))

(test-equal #t (string-suffix? "" "" 0 0))

(test-equal #t (string-suffix? "" "abc" 0 0))

(test-equal #f (string-suffix? "a" "abc" 0 1))

(test-equal #t (string-suffix? "c" "abc" 0 1))

(test-equal #t (string-suffix? "ac" "abc" 1 2))

(test-equal #f (string-suffix? "ac" "abc" 0 2))

(test-equal #t (string-suffix? "bc" "abc" 0 2))

(test-equal #t (string-suffix? "abc" "abc" 0 3))

(test-equal #t (string-prefix? "ab" "abc" 2 2))

(test-equal #t (string-prefix? "ac" "abc" 2 2))

(test-equal #f (string-prefix? "abc" "abc" 2 3))

(test-equal #t (string-suffix? "ac" "abc" 2 2))

(test-equal #t (string-suffix? "bc" "abc" 2 2))

(test-equal #t (string-suffix? "abc" "abc" 2 3))


(test-equal #t (string-prefix? "" "" 0 0 0))

(test-equal #t (string-prefix? "" "abc" 0 0 0))

(test-equal #t (string-prefix? "a" "abc" 0 0 0))

(test-equal #f (string-prefix? "c" "abc" 0 1 0))

(test-equal #t (string-prefix? "ab" "abc" 0 1 0))

(test-equal #t (string-prefix? "ab" "abc" 0 2 0))

(test-equal #f (string-prefix? "ac" "abc" 0 2 0))

(test-equal #t (string-prefix? "abc" "abc" 0 3 0))

(test-equal #t (string-suffix? "" "" 0 0 0))

(test-equal #t (string-suffix? "" "abc" 0 0 0))

(test-equal #f (string-suffix? "a" "abc" 0 1 0))

(test-equal #t (string-suffix? "c" "abc" 0 1 0))

(test-equal #t (string-suffix? "ac" "abc" 1 2 0))

(test-equal #f (string-suffix? "ac" "abc" 0 2 0))

(test-equal #t (string-suffix? "bc" "abc" 0 2 0))

(test-equal #t (string-suffix? "abc" "abc" 0 3 0))

(test-equal #t (string-prefix? "ab" "abc" 2 2 0))

(test-equal #t (string-prefix? "ac" "abc" 2 2 0))

(test-equal #f (string-prefix? "abc" "abc" 2 3 0))

(test-equal #t (string-suffix? "ac" "abc" 2 2 0))

(test-equal #t (string-suffix? "bc" "abc" 2 2 0))

(test-equal #t (string-suffix? "abc" "abc" 2 3 0))

(test-equal #t (string-prefix? "" "abc" 0 0 1))

(test-equal #t (string-prefix? "a" "abc" 0 0 1))

(test-equal #t (string-prefix? "c" "abc" 0 1 2))

(test-equal #f (string-prefix? "ab" "abc" 0 1 2))

(test-equal #f (string-prefix? "ab" "abc" 0 2 1))

(test-equal #f (string-prefix? "ac" "abc" 0 2 1))

(test-equal #f (string-prefix? "abc" "abc" 0 3 1))

(test-equal #f (string-suffix? "a" "abc" 0 1 2))

(test-equal #t (string-suffix? "c" "abc" 0 1 1))

(test-equal #t (string-suffix? "ac" "abc" 1 2 2))

(test-equal #t (string-suffix? "bc" "abc" 0 2 1))

(test-equal #f (string-suffix? "bc" "abc" 0 2 2))


(test-equal #t (string-prefix? "" "" 0 0 0 0))

(test-equal #t (string-prefix? "" "abc" 0 0 0 3))

(test-equal #t (string-prefix? "a" "abc" 0 0 0 3))

(test-equal #f (string-prefix? "c" "abc" 0 1 0 3))

(test-equal #t (string-prefix? "ab" "abc" 0 1 0 3))

(test-equal #t (string-prefix? "ab" "abc" 0 2 0 3))

(test-equal #f (string-prefix? "ac" "abc" 0 2 0 3))

(test-equal #t (string-prefix? "abc" "abc" 0 3 0 3))

(test-equal #t (string-suffix? "" "abc" 0 0 0 3))

(test-equal #f (string-suffix? "a" "abc" 0 1 0 3))

(test-equal #t (string-suffix? "c" "abc" 0 1 0 3))

(test-equal #t (string-suffix? "ac" "abc" 1 2 0 3))

(test-equal #f (string-suffix? "ac" "abc" 0 2 0 3))

(test-equal #t (string-suffix? "bc" "abc" 0 2 0 3))

(test-equal #t (string-suffix? "abc" "abc" 0 3 0 3))

(test-equal #t (string-prefix? "ab" "abc" 2 2 0 3))

(test-equal #t (string-prefix? "ac" "abc" 2 2 0 3))

(test-equal #f (string-prefix? "abc" "abc" 2 3 0 3))

(test-equal #t (string-suffix? "ac" "abc" 2 2 0 3))

(test-equal #t (string-suffix? "bc" "abc" 2 2 0 3))

(test-equal #t (string-suffix? "abc" "abc" 2 3 0 3))

(test-equal #t (string-prefix? "" "abc" 0 0 1 3))

(test-equal #t (string-prefix? "a" "abc" 0 0 1 3))

(test-equal #t (string-prefix? "c" "abc" 0 1 2 3))

(test-equal #f (string-prefix? "ab" "abc" 0 1 2 3))

(test-equal #f (string-prefix? "ab" "abc" 0 2 1 3))

(test-equal #f (string-prefix? "ac" "abc" 0 2 1 3))

(test-equal #f (string-prefix? "abc" "abc" 0 3 1 3))

(test-equal #f (string-suffix? "a" "abc" 0 1 2 3))

(test-equal #t (string-suffix? "c" "abc" 0 1 1 3))

(test-equal #t (string-suffix? "ac" "abc" 1 2 2 3))

(test-equal #t (string-suffix? "bc" "abc" 0 2 1 3))

(test-equal #f (string-suffix? "bc" "abc" 0 2 2 3))


(test-equal #t (string-prefix? "" "abc" 0 0 0 2))

(test-equal #t (string-prefix? "a" "abc" 0 0 0 2))

(test-equal #f (string-prefix? "c" "abc" 0 1 0 2))

(test-equal #t (string-prefix? "ab" "abc" 0 1 0 2))

(test-equal #f (string-prefix? "abc" "abc" 0 3 0 2))

(test-equal #t (string-suffix? "" "abc" 0 0 0 2))

(test-equal #f (string-suffix? "c" "abc" 0 1 0 2))

(test-equal #f (string-suffix? "ac" "abc" 1 2 0 2))


;;; Searching

(test-equal #f (string-index "" char?))

(test-equal 0 (string-index "abcdef" char?))

(test-equal 4 (string-index "abcdef" (lambda (c) (char>? c #\d))))

(test-equal #f (string-index "abcdef" char-whitespace?))

(test-equal #f (string-index-right "" char?))

(test-equal 5 (string-index-right "abcdef" char?))

(test-equal 5 (string-index-right "abcdef"
                                 (lambda (c) (char>? c #\d))))


(test-equal #f (string-index-right "abcdef" char-whitespace?))

(test-equal #f (string-skip "" string?))

(test-equal 0 (string-skip "abcdef" string?))

(test-equal 4 (string-skip "abcdef" (lambda (c) (char<=? c #\d))))

(test-equal #f (string-skip "abcdef" char?))

(test-equal #f (string-skip-right "" string?))

(test-equal 5 (string-skip-right "abcdef" string?))

(test-equal 5 (string-skip-right "abcdef"
                                (lambda (c) (char<=? c #\d))))

(test-equal #f (string-skip-right "abcdef" char?))


(test-equal 2 (string-index "abcdef" char? 2))

(test-equal 4 (string-index "abcdef" (lambda (c) (char>? c #\d)) 2))

(test-equal #f (string-index "abcdef" char-whitespace? 2))

(test-equal 5 (string-index-right "abcdef" char? 2))

(test-equal 5 (string-index-right "abcdef"
                                 (lambda (c)
                                   (char>? c #\d)) 2))

(test-equal #f (string-index-right "abcdef" char-whitespace? 2))

(test-equal 2 (string-skip "abcdef" string? 2))

(test-equal 4 (string-skip "abcdef"
                          (lambda (c)
                            (char<=? c #\d)) 2))

(test-equal #f (string-skip "abcdef" char? 2))

(test-equal 5 (string-skip-right "abcdef" string? 2))

(test-equal 5 (string-skip-right "abcdef"
                                (lambda (c)
                                  (char<=? c #\d)) 2))

(test-equal #f (string-skip-right "abcdef" char? 2))


(test-equal 2 (string-index "abcdef" char? 2 5))

(test-equal 4 (string-index "abcdef"
                           (lambda (c) (char>? c #\d)) 2 5))

(test-equal #f (string-index "abcdef" char-whitespace? 2 5))

(test-equal 4 (string-index-right "abcdef" char? 2 5))

(test-equal 4 (string-index-right "abcdef"
                                 (lambda (c)
                                   (char>? c #\d)) 2 5))

(test-equal #f (string-index-right "abcdef"
                                  char-whitespace? 2 5))


(test-equal 2 (string-skip "abcdef" string? 2 5))

(test-equal 4 (string-skip "abcdef"
                          (lambda (c) (char<=? c #\d)) 2 5))

(test-equal #f (string-skip "abcdef" char? 2 5))

(test-equal 4 (string-skip-right "abcdef" string? 2 5))

(test-equal 4 (string-skip-right "abcdef"
                                (lambda (c)
                                  (char<=? c #\d)) 2 5))

(test-equal #f (string-skip-right "abcdef" char? 2 5))


(test-equal 0 (string-contains "" ""))

(test-equal 0 (string-contains "abcdeffffoo" ""))

(test-equal 0 (string-contains "abcdeffffoo" "a"))

(test-equal 5 (string-contains "abcdeffffoo" "ff"))

(test-equal 4 (string-contains "abcdeffffoo" "eff"))

(test-equal 8 (string-contains "abcdeffffoo" "foo"))

(test-equal #f (string-contains "abcdeffffoo" "efffoo"))

(test-equal 0 (string-contains-right "" ""))

(test-equal 11 (string-contains-right "abcdeffffoo" ""))

(test-equal 0 (string-contains-right "abcdeffffoo" "a"))

(test-equal 7 (string-contains-right "abcdeffffoo" "ff"))

(test-equal 4 (string-contains-right "abcdeffffoo" "eff"))

(test-equal 8 (string-contains-right "abcdeffffoo" "foo"))

(test-equal #f (string-contains-right "abcdeffffoo"
                                     "efffoo"))


(test-equal 0 (string-contains "" "" 0))

(test-equal 2 (string-contains "abcdeffffoo" "" 2))

(test-equal #f (string-contains "abcdeffffoo" "a" 2))

(test-equal 5 (string-contains "abcdeffffoo" "ff" 2))

(test-equal 4 (string-contains "abcdeffffoo" "eff" 2))

(test-equal 8 (string-contains "abcdeffffoo" "foo" 2))

(test-equal #f (string-contains "abcdeffffoo" "efffoo" 2))

(test-equal 0 (string-contains-right "" "" 0))

(test-equal 11 (string-contains-right "abcdeffffoo" "" 2))

(test-equal #f (string-contains-right "abcdeffffoo" "a" 2))

(test-equal 7 (string-contains-right "abcdeffffoo" "ff" 2))

(test-equal 4 (string-contains-right "abcdeffffoo" "eff" 2))

(test-equal 8 (string-contains-right "abcdeffffoo" "foo" 2))

(test-equal #f (string-contains-right "abcdeffffoo" "efffoo" 2))


(test-equal 0 (string-contains "" "" 0 0))

(test-equal 2 (string-contains "abcdeffffoo" "" 2 10))

(test-equal #f (string-contains "abcdeffffoo" "a" 2 10))

(test-equal 5 (string-contains "abcdeffffoo" "ff" 2 10))

(test-equal 4 (string-contains "abcdeffffoo" "eff" 2 10))

(test-equal #f (string-contains "abcdeffffoo" "foo" 2 10))

(test-equal #f (string-contains "abcdeffffoo" "efffoo" 2 10))

(test-equal 0 (string-contains-right "" "" 0 0))

(test-equal 10 (string-contains-right "abcdeffffoo" "" 2 10))

(test-equal #f (string-contains-right "abcdeffffoo" "a" 2 10))

(test-equal 7 (string-contains-right "abcdeffffoo" "ff" 2 10))

(test-equal 4 (string-contains-right "abcdeffffoo" "eff" 2 10))

(test-equal #f (string-contains-right "abcdeffffoo" "foo" 2 10))

(test-equal #f (string-contains-right "abcdeffffoo" "efffoo" 2 10))


(test-equal 0 (string-contains "" "" 0 0 0))

(test-equal 2 (string-contains "abcdeffffoo" "" 2 10 0))

(test-equal 2 (string-contains "abcdeffffoo" "a" 2 10 1))

(test-equal 5 (string-contains "abcdeffffoo" "ff" 2 10 1))

(test-equal 5 (string-contains "abcdeffffoo" "eff" 2 10 1))

(test-equal #f (string-contains "abcdeffffoo" "foo" 2 10 1))

(test-equal #f (string-contains "abcdeffffoo" "efffoo" 2 10 1))

(test-equal 0 (string-contains-right "" "" 0 0 0))

(test-equal 10 (string-contains-right "abcdeffffoo" "" 2 10 0))

(test-equal 10 (string-contains-right "abcdeffffoo" "a" 2 10 1))

(test-equal 8 (string-contains-right "abcdeffffoo" "ff" 2 10 1))

(test-equal 7 (string-contains-right "abcdeffffoo" "eff" 2 10 1))

(test-equal #f (string-contains-right "abcdeffffoo" "foo" 2 10 1))

(test-equal #f (string-contains-right "abcdeffffoo" "efffoo" 2 10 1))


(test-equal 0 (string-contains "" "" 0 0 0 0))

(test-equal 2 (string-contains "abcdeffffoo" "" 2 10 0 0))

(test-equal 2 (string-contains "abcdeffffoo" "a" 2 10 1 1))

(test-equal 5 (string-contains "abcdeffffoo" "ff" 2 10 1 2))

(test-equal 5 (string-contains "abcdeffffoo" "eff" 2 10 1 2))

(test-equal 9 (string-contains "abcdeffffoo" "foo" 2 10 1 2))

(test-equal 4 (string-contains "abcdeffffoo" "efffoo" 2 10 0 2))

(test-equal 0 (string-contains-right "" "" 0 0 0 0))

(test-equal 10 (string-contains-right "abcdeffffoo" "" 2 10 0 0))

(test-equal 10 (string-contains-right "abcdeffffoo" "a" 2 10 1 1))

(test-equal 8  (string-contains-right "abcdeffffoo" "ff" 2 10 1 2))

(test-equal 8 (string-contains-right "abcdeffffoo" "eff" 2 10 1 2))

(test-equal 9 (string-contains-right "abcdeffffoo" "foo" 2 10 1 2))

(test-equal 7 (string-contains-right "abcdeffffoo" "efffoo" 2 10 1 3))


;;; Case conversion

;;; FIXME: should test some non-ASCII cases here.

(test-equal "1234STRIKES" (string-upcase "1234Strikes"))

(test-equal "1234STRIKES" (string-upcase "1234strikes"))

(test-equal "1234STRIKES" (string-upcase "1234STRIKES"))

(test-equal "1234strikes" (string-downcase "1234Strikes"))

(test-equal "1234strikes" (string-downcase "1234strikes"))

(test-equal "1234strikes" (string-downcase "1234STRIKES"))

(test-equal "1234strikes" (string-foldcase "1234Strikes"))

(test-equal "1234strikes" (string-foldcase "1234strikes"))

(test-equal "1234strikes" (string-foldcase "1234STRIKES"))

(test-equal "And With Three Strikes You Are Out"
              (string-titlecase
               "and with THREE STRIKES you are oUT"))

;;; Concatenation

(test-equal "" (string-append))

(test-equal "abcdef"
              
              (string-append ""
                              "a"
                              "bcd"
                              "" "ef" "" ""))

(test-equal "" (string-concatenate '()))

(test-equal "abcdef"
            (string-concatenate '("" "a" "bcd" "" "ef" "" "")))

;;; string-concatenate is likely to have special cases for longer strings.

(let* ((alphabet "abcdefghijklmnopqrstuvwxyz")
       (str1 alphabet)
       (str10 (apply string-append (vector->list (make-vector 10 str1))))
       (str100 (apply string-append (vector->list (make-vector 10 str10))))
       (str100-500 (substring str100 100 500))
       (str600-999 (substring str100 600 999))
       (alph1 (string-copy alphabet))
       (alph10 (string-concatenate (vector->list (make-vector 10 alph1))))
       (alph100 (string-concatenate (vector->list (make-vector 10 alph10))))
       (t100-500 (substring alph100 100 500))
       (t600-999 (substring alph100 600 999)))

  (test-equal str10 alph10)

  (test-equal str100 alph100)

  (test-equal str100-500 t100-500)

  (test-equal str600-999 t600-999)

  ;; concatenating a short string with a long string

  (test-equal (string-append str1 str600-999)
              (string-concatenate (list alph1 t600-999)))

  (test-equal (string-append str1 str600-999)
              (string-concatenate (list alph1 (string-copy t600-999))))

  (test-equal (string-append str600-999 str1)
              (string-concatenate (list t600-999 alph1)))

  (test-equal (string-append str600-999 str1)
              (string-concatenate (list (string-copy t600-999) alph1))))


(test-equal "" (string-concatenate-reverse '()))

(test-equal "efbcda"
            (string-concatenate-reverse '("" "a" "bcd" "" "ef" "" "")))

(test-equal "huh?"
              (string-concatenate-reverse '() "huh?"))

(test-equal "efbcdaxy"
              (string-concatenate-reverse '("" "a" "bcd" "" "ef" "" "") "xy"))

(test-equal "huh"
              (string-concatenate-reverse '() "huh?" 3))

(test-equal "efbcdax"
              (string-concatenate-reverse
               '("" "a" "bcd" "" "ef" "" "") "x" 1))


(test-equal "" (string-join '()))

(test-equal " ab cd  e f "
              (string-join '("" "ab" "cd" "" "e" "f" "")))

(test-equal ""
              (string-join '() ""))

(test-equal "abcdef"
              (string-join '("" "ab" "cd" "" "e" "f" "") ""))

(test-equal ""
              (string-join '() "xyz"))

(test-equal "xyzabxyzcdxyzxyzexyzfxyz"
              (string-join '("" "ab" "cd" "" "e" "f" "") "xyz"))

(test-equal ""
              (string-join '() "" 'infix))

(test-equal "abcdef"
              (string-join '("" "ab" "cd" "" "e" "f" "") "" 'infix))

(test-equal ""
              (string-join '() "xyz" 'infix))

(test-equal "xyzabxyzcdxyzxyzexyzfxyz"
              (string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'infix))

(test-equal "foo bar baz" (string-join '("foo" "bar" "baz")))
(test-equal "foobarbaz" (string-join '("foo" "bar" "baz") ""))
(test-equal "foo:bar:baz" (string-join '("foo" "bar" "baz") ":"))
(test-equal "foo:bar:baz:" (string-join '("foo" "bar" "baz") ":" 'suffix))
(test-equal "" (string-join '() ":"))
(test-equal "" (string-join '("") ":"))
(test-equal "" (string-join '()  ":" 'infix))
(test-error (string-join '()  ":" 'strict-infix))
(test-equal "A" (string-join '("A")  ":" 'strict-infix))
(test-equal "A:B" (string-join '("A" "B")  ":" 'strict-infix))
(test-equal "" (string-join '()  ":" 'suffix))
(test-equal ":" (string-join '("") ":" 'suffix))

(test-equal 'horror
            (guard (exn (#t 'horror))
                   (string-join '() "" 'strict-infix)))

(test-equal "abcdef"
              (string-join '("" "ab" "cd" "" "e" "f" "") "" 'strict-infix))

(test-equal 'wham
            (guard (exn (else 'wham))
                   (string-join '() "xyz" 'strict-infix)))

(test-equal "xyzabxyzcdxyzxyzexyzfxyz"
              (string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'strict-infix))

(test-equal ""
              (string-join '() "" 'suffix))

(test-equal "abcdef"
              (string-join '("" "ab" "cd" "" "e" "f" "") "" 'suffix))

(test-equal ""
              (string-join '() "xyz" 'suffix))

(test-equal "xyzabxyzcdxyzxyzexyzfxyzxyz"
              (string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'suffix))

(test-equal ""
              (string-join '() "" 'prefix))

(test-equal "abcdef"
              (string-join '("" "ab" "cd" "" "e" "f" "") "" 'prefix))

(test-equal ""
              (string-join '() "xyz" 'prefix))

(test-equal "xyzxyzabxyzcdxyzxyzexyzfxyz"
              (string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'prefix))


;;; Fold & map & friends

(test-equal 8
            (string-fold (lambda (c count)
                       (if (char-whitespace? c)
                           (+ count 1)
                           count))
                     0
                     " ...a couple of spaces in this one... "))

(test-equal 7 (string-fold (lambda (c count)
                       (if (char-whitespace? c)
                           (+ count 1)
                           count))
                     0
                     " ...a couple of spaces in this one... "
                     1))

(test-equal 6 (string-fold (lambda (c count)
                       (if (char-whitespace? c)
                           (+ count 1)
                           count))
                     0
                     " ...a couple of spaces in this one... "
                     1
                     32))

(test-equal (string->list "abcdef")
            (string-fold-right cons '() "abcdef"))

(test-equal (string->list "def")
            (string-fold-right cons '() "abcdef" 3))

(test-equal (string->list "cde")
            (string-fold-right cons '() "abcdef" 2 5))

(test-equal "aabraacaadaabraa"
              (let* ((s "abracadabra")
                     (ans-len (string-fold (lambda (c sum)
                                              (+ sum (if (char=? c #\a) 2 1)))
                                            0 s))
                     (ans (make-string ans-len)))
                (string-fold (lambda (c i)
                                (let ((i (if (char=? c #\a)
                                             (begin (string-set! ans i #\a)
                                                    (+ i 1))
                                             i)))
                                  (string-set! ans i c)
                                  (+ i 1)))
                              0 s)
                ans))


(test-equal "abc" (string-map string "abc"))

(test-equal "ABC" (string-map char-upcase "abc"))

(test-equal "Hear-here!"
              (string-map (lambda (c0 c1 c2)
                             (case c0
                               ((#\1) c1)
                               ((#\2) (string c2))
                               ((#\-) (string #\- c1))))
                           "1222-1111-2222"
                           "Hi There!"
                           "Dear John"))

(test-equal "abc"
              (let ((q (open-output-string)))
                (string-for-each (lambda (c) (write-char c q))
                                  "abc")
                (get-output-string q)))

(test-equal '("cfi" "beh" "adg")
            (let ((x '()))
              (string-for-each (lambda (c1 c2 c3)
                                  (set! x (cons (string c1 c2 c3) x)))
                                "abc"
                                "defxyz"
                                "ghijklmnopqrstuvwxyz")
              x))

(test-equal "abc"
              (string-map-index (lambda (i)
                                   (integer->char (+ i (char->integer #\a))))
                                 "xyz"))

(let ((r (string-map-index (lambda (i)
                             (integer->char (+ i (char->integer #\a))))
                           "xyz***" 3)))
  (test-equal '(#t 3) (check-istring r))
  (test-equal "def" r))

(test-equal "cde"
              (string-map-index (lambda (i)
                                   (integer->char (+ i (char->integer #\a))))
                                 "......" 2 5))

(test-equal '(101 100 99 98 97)
            (let ((s "abcde")
                  (v '()))
              (string-for-each-index
               (lambda (i)
                 (set! v (cons (char->integer (string-ref s i)) v)))
               s)
              v))

(test-equal '(101 100 99)
            (let ((s "abcde")
                  (v '()))
              (string-for-each-index
               (lambda (i)
                 (set! v (cons (char->integer (string-ref s i)) v)))
               s 2)
              v))

(test-equal '(99 98)
            (let ((s "abcde")
                  (v '()))
              (string-for-each-index
               (lambda (i)
                 (set! v (cons (char->integer (string-ref s i)) v)))
               s 1 3)
              v))

(test-equal 6 (string-count "abcdef" char?))

(test-equal 4 (string-count "counting  whitespace, again " char-whitespace? 5))

(test-equal 3 (string-count "abcdefwxyz"
                        (lambda (c) (odd? (char->integer c)))
                        2 8))


(let ((r (string-filter (lambda (c) (memv c (string->list "aeiou")))
                        "What is number, that man may know it?")))
  (test-equal "aiueaaaoi" r)
  (test-equal '(#t 9) (check-istring r)))

(let ((r (string-remove (lambda (c) (memv c (string->list "aeiou")))
                        "And woman, that she may know number?")))
  (test-equal "And wmn, tht sh my knw nmbr?" r)
  (test-equal '(#t 28) (check-istring r)))

(test-equal "iueaaaoi"
              (string-filter (lambda (c) (memv c (string->list "aeiou")))
                              "What is number, that man may know it?"
                              4))

(test-equal "mn, tht sh my knw nmbr?"
              (string-remove (lambda (c) (memv c (string->list "aeiou")))
                              "And woman, that she may know number?"
                              6))

(test-equal "aaao"
              (string-filter (lambda (c) (memv c (string->list "aeiou")))
                              "What is number, that man may know it?"
                              16 32))

(test-equal "And woman, that sh may know"
              (string-remove (lambda (c) (memv c (string->list "eiu")))
                              "And woman, that she may know number?"
                              0 28))


#|
(test-equal "" (string-reverse ""))

(test-equal  "fedcba" (string-reverse "abcdef"))

(test-equal "" (string-reverse "" 0))

(test-equal "fedcba" (string-reverse "abcdef" 0))

(test-equal "fedc" (string-reverse "abcdef" 2))

(test-equal "" (string-reverse "" 0 0))

(test-equal "fedcba" (string-reverse "abcdef" 0 6))

(test-equal "edc" (string-reverse "abcdef" 2 5))
|#


;;; Replication and splitting

(test-equal "" (string-repeat #\X 0))
(test-equal "XXX" (string-repeat #\X 3))
(test-equal "" (string-repeat "abc" 0))
(test-equal "abcabcabc" (string-repeat "abc" 3))

(test-equal "cdefabcdefabcd"
              (xsubstring "abcdef" -4 10))

(test-equal "bcdefbcdefbcd"
              (xsubstring "abcdef" 90 103 1))

(test-equal "ecdecdecde"
              (xsubstring "abcdef" -13 -3 2 5))

(test-equal "cdefab" (xsubstring "abcdef" 2 8))
(test-equal "efabcd" (xsubstring "abcdef" -2 4))
(test-equal "abcabca" (xsubstring "abc" 0 7))

(test-equal '() (string-split "" ""))

(test-equal '("a" "b" "c") (string-split "abc" ""))

(test-equal '("too" "" "much" "" "data")
            (string-split "too  much  data" " "))

(test-equal '("" "there" "ya" "go" "")
            (string-split "***there***ya***go***" "***"))

(test-equal '() (string-split "" "" 'infix))

(test-equal '("a" "b" "c")
            (string-split "abc" "" 'infix))

(test-equal '("too" "" "much" "" "data")
            (string-split "too  much  data" " " 'infix))

(test-equal '("" "there" "ya" "go" "")
            (string-split "***there***ya***go***" "***" 'infix))

(test-equal 'error
            (guard (exn (else 'error))
                   (string-split "" "" 'strict-infix)))

(test-equal '("a" "b" "c")
            (string-split "abc" "" 'strict-infix))

(test-equal '("too" "" "much" "" "data")
            (string-split "too  much  data" " " 'strict-infix))

(test-equal '("" "there" "ya" "go" "")
            (string-split "***there***ya***go***" "***" 'strict-infix))

(test-equal '()
                 (string-split "" "" 'prefix))

(test-equal '("a" "b" "c")
            (string-split "abc" "" 'prefix))

(test-equal '("too" "" "much" "" "data")
            (string-split "too  much  data" " " 'prefix))

(test-equal '("there" "ya" "go" "")
            (string-split "***there***ya***go***" "***" 'prefix))

(test-equal '()
            (string-split "" "" 'suffix))

(test-equal '("a" "b" "c")
            (string-split "abc" "" 'suffix))

(test-equal '("too" "" "much" "" "data")
            (string-split "too  much  data" " " 'suffix))

(test-equal '("" "there" "ya" "go")
            (string-split "***there***ya***go***" "***" 'suffix))


(test-equal '() (string-split "" "" 'infix #f))

(test-equal '("a" "b" "c") (string-split "abc" "" 'infix #f))

(test-equal '("too" "" "much" "" "data")
            (string-split "too  much  data" " " 'infix #f))

(test-equal '("" "there" "ya" "go" "")
            (string-split "***there***ya***go***" "***" 'infix #f))

(test-equal 'error
            (guard (exn (else 'error))
                   (string-split "" "" 'strict-infix #f)))

(test-equal '("a" "b" "c")
            (string-split "abc" "" 'strict-infix #f))

(test-equal '("too" "" "much" "" "data")
            (string-split "too  much  data" " " 'strict-infix #f))

(test-equal '("" "there" "ya" "go" "")
            (string-split "***there***ya***go***" "***" 'strict-infix #f))

(test-equal '() (string-split "" "" 'prefix #f))

(test-equal '("a" "b" "c")
            (string-split "abc" "" 'prefix #f))

(test-equal '("too" "" "much" "" "data")
            (string-split "too  much  data" " " 'prefix #f))

(test-equal '("there" "ya" "go" "")
            (string-split "***there***ya***go***" "***" 'prefix #f))

(test-equal '()
            (string-split "" "" 'suffix #f))

(test-equal '("a" "b" "c")
            (string-split "abc" "" 'suffix #f))

(test-equal '("too" "" "much" "" "data")
            (string-split "too  much  data" " " 'suffix #f))

(test-equal '("" "there" "ya" "go")
            (string-split "***there***ya***go***" "***" 'suffix #f))


(test-equal 'error
            (guard (exn (else 'error))
                   (string-split "" "" 'strict-infix 3)))

(test-equal '("a" "b" "c")
            (string-split "abc" "" 'strict-infix 3))

(test-equal '("too" "" "much" " data")
            (string-split "too  much  data" " " 'strict-infix 3))

(test-equal '("" "there" "ya" "go***")
            (string-split "***there***ya***go***" "***" 'strict-infix 3))

(test-equal '()
            (string-split "" "" 'prefix 3))

(test-equal '("a" "b" "c")
            (string-split "abc" "" 'prefix 3))

(test-equal '("too" "" "much" " data")
            (string-split "too  much  data" " " 'prefix 3))

(test-equal '("there" "ya" "go***")
            (string-split "***there***ya***go***" "***" 'prefix 3))

(test-equal '()
            (string-split "" "" 'suffix 3))

(test-equal '("a" "b" "c")
            (string-split "abc" "" 'suffix 3))

(test-equal '("too" "" "much" " data")
            (string-split "too  much  data" " " 'suffix 3))

(test-equal '("" "there" "ya" "go***")
            (string-split "***there***ya***go***" "***" 'suffix 3))

(test-equal 'error
            (guard (exn (else 'error))
                   (string-split "" "" 'strict-infix 3 0)))

(test-equal '("b" "c")
            (string-split "abc" "" 'strict-infix 3 1))

(test-equal '("oo" "" "much" " data")
            (string-split "too  much  data" " " 'strict-infix 3 1))

(test-equal '("**there" "ya" "go" "")
            (string-split "***there***ya***go***" "***" 'strict-infix 3 1))

(test-equal '()
                 (string-split "" "" 'prefix 3 0))

(test-equal '("b" "c")
            (string-split "abc" "" 'prefix 3 1))

(test-equal '("oo" "" "much" " data")
            (string-split "too  much  data" " " 'prefix 3 1))

(test-equal '("**there" "ya" "go" "")
            (string-split "***there***ya***go***" "***" 'prefix 3 1))

(test-equal '()
            (string-split "" "" 'suffix 3 0))

(test-equal '("b" "c")
            (string-split "abc" "" 'suffix 3 1))

(test-equal '("oo" "" "much" " data")
            (string-split "too  much  data" " " 'suffix 3 1))

(test-equal '("**there" "ya" "go")
            (string-split "***there***ya***go***" "***" 'suffix 3 1))


(test-equal 'error
            (guard (exn (else 'error))
                   (string-split "" "" 'strict-infix 3 0 0)))

(test-equal '("b")
            (string-split "abc" "" 'strict-infix 3 1 2))

(test-equal '("oo" "" "much" " ")
            (string-split "too  much  data" " " 'strict-infix 3 1 11))

(test-equal '()
            (string-split "" "" 'prefix 3 0 0))

(test-equal '("b")
            (string-split "abc" "" 'prefix 3 1 2))

(test-equal '("oo" "" "much" " ")
            (string-split "too  much  data" " " 'prefix 3 1 11))

(test-equal '()
            (string-split "" "" 'suffix 3 0 0))

(test-equal '("b")
            (string-split "abc" "" 'suffix 3 1 2))

(test-equal '("oo" "" "much" " ")
            (string-split "too  much  data" " " 'suffix 3 1 11))

(define (translate-space-to-newline str)
  (let ((result (make-string 0)))
    (string-for-each
     (lambda (ch)
       (string-append! result
                       (if (char=? ch #\space) #\newline ch)))
     str)
    result))
(test-equal "ab\ncd\nx"
            (translate-space-to-newline "ab cd x"))

;; begin section with UTF-8 literals
(cond-expand
 (full-unicode
  (let ((str (make-string 3 #\😂)))
  (test-equal 3 (string-length str))
 ;; (test-equal 6 (str:length))
  (string-replace! str 1 2 "abc")
  (test-equal "😂abc😂" str)
  (string-replace! str 5 5 str 3)
  (test-equal "😂abc😂c😂" str)
  (string-replace! str 0 2 "ABC" 1 2)
  (test-equal "Bbc😂c😂" str)
  (test-equal 6 (string-length str))
  (test-equal #\c (string-ref str 2))
  (test-equal #\x1f602 (string-ref str 3))
  (test-equal #\c (string-ref str 4)))

  (test-equal "c😼b😂a" (reverse-list->string '(#\a #\😂 #\b #\😼 #\c)))

  (test-equal "y😂a😼xy" (xsubstring "a😼xy😂" 3 9))
  (test-equal "y😂a😼" (xsubstring "a😼xy😂" -2 2))
))
;; end section with UTF-8 literals

(test-end)
