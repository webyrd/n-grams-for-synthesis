;; strings.scm -- cursor-oriented string library
;; Copyright (c) 2012-2015 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; Modified for SRFI 130 implementation

;;> \section{High-level API}

;;> The procedures below are similar to those in SRFI 13 or other
;;> string libraries, except instead of receiving and returning
;;> character indexes they use opaque string cursors.

;;> \procedure{(string-null? str)}
;;> Returns true iff \var{str} is equal to the empty string \scheme{""}.

(define (string-null? str)
  (equal? str ""))

(define (make-char-predicate x)
  (cond ((procedure? x) x)
        ((char? x) (lambda (ch) (eq? ch x)))
        #;((char-set? x) (lambda (ch) (char-set-contains? x ch)))
        (else (error "invalid character predicate" x))))

(define (complement pred) (lambda (x) (not (pred x))))

;;> Returns true iff \var{check} is true for any character in
;;> \var{str}.  \var{check} can be a procedure, char (to test for
;;> \scheme{char=?} equivalence) or char-set (to test for
;;> \var{char-set-contains?}).  Always returns false if \var{str} is
;;> empty.

(define (string-any check str . rest)
  (let* ((str (apply string-copy/cursors str rest))
         (pred (make-char-predicate check))
         (end (string-cursor-end str)))
    (and (string-cursor>? end (string-cursor-start str))
         (let lp ((i (string-cursor-start str)))
           (let ((i2 (string-cursor-next str i))
                 (ch (string-cursor-ref str i)))
             (if (string-cursor>=? i2 end)
                 (pred ch)  ;; tail call
                 (or (pred ch) (lp i2))))))))

;;> Returns true iff \var{check} is true for every character in
;;> \var{str}.  \var{check} can be a procedure, char or char-set as in
;;> \scheme{string-any}.  Always returns true if \var{str} is empty.

;;; Must return the true value returned by the predicate,
;;; so the following definition is commented out.
#;
(define (string-every check str . FIXME)
  (not (string-any (complement (make-char-predicate check)) str)))

(define (string-every check str . rest)
  (let* ((str (apply string-copy/cursors str rest))
         (n (string-length str))
         (pred (make-char-predicate check)))
    (let loop ((result #t)
               (i 0))
      (if (= i n)
          result
          (let ((x (pred (string-ref str i))))
            (if x
                (loop x (+ i 1))
                #f))))))

;;> Returns a cursor pointing to the first position from the left in
;;> string for which \var{check} is true.  \var{check} can be a
;;> procedure, char or char-set as in \scheme{string-any}.  The
;;> optional cursors \var{start} and \var{end} can specify a substring
;;> to search, and default to the whole string.  Returns a cursor just
;;> past the end of \var{str} if no character matches.

(define (string-find str check . o)
  (let ((pred (make-char-predicate check))
        (end (if (and (pair? o) (pair? (cdr o)))
                 (cadr o)
                 (string-cursor-end str))))
    (let lp ((i (if (pair? o) (car o) (string-cursor-start str))))
      (cond ((string-cursor>=? i end) end)
            ((pred (string-cursor-ref str i)) i)
            (else (lp (string-cursor-next str i)))))))

;;> As above, ignoring the position and returning true iff any
;;> character matches.

(define (string-find? str check . o)
  (let ((start (if (pair? o) (car o) (string-cursor-start str)))
        (end (if (and (pair? o) (pair? (cdr o)))
                 (cadr o)
                 (string-cursor-end str))))
    (string-cursor<? (string-find str check start end) end)))

;;> As \scheme{string-find}, but returns the position of the first
;;> character from the right of \var{str}.  If no character matches,
;;> returns a string cursor pointing just before \var{start}.

(define (string-find-right str check . o)
  (let ((pred (make-char-predicate check))
        (start (if (pair? o) (car o) (string-cursor-start str))))
    (let lp ((i (if (and (pair? o) (pair? (cdr o)))
                    (cadr o)
                    (string-cursor-end str))))
      (let ((i2 (string-cursor-prev str i)))
        (cond ((string-cursor<? i2 start) start)
              ((pred (string-cursor-ref str i2)) i)
              (else (lp i2)))))))

;;> As \scheme{string-find}, but inverts the check, returning the
;;> position of the first character which doesn't match.

(define (string-skip str check . o)
  (apply string-find str (complement (make-char-predicate check)) o))

;;> As \scheme{string-find-right}, but inverts the check, returning
;;> the position of the first character which doesn't match.

(define (string-skip-right str check . o)
  (apply string-find-right str (complement (make-char-predicate check)) o))

;;> \procedure{(%string-join list-of-strings [separator])}
;;>
;;> Concatenates the \var{list-of-strings} and return the result as a
;;> single string.  If \var{separator} is provided it is inserted
;;> between each pair of strings.

(define (%string-join strings sep)
  (cond ((null? strings) "")
        ((null? (cdr strings)) (car strings))
        (else
         (string-append (car strings) sep (%string-join (cdr strings) sep)))))

;;> Split \var{str} into a list of substrings separated by \var{pred},
;;> which defaults to \scheme{#\\space}.  Multiple adjacent characters
;;> which satisy \var{pred} will result in empty strings in the list.
;;> If the optional \var{limit} is provided, splits into at most that
;;> many substrings starting from the left.
#;
(define (string-split str . o)
  (let ((pred (make-char-predicate (if (pair? o) (car o) #\space)))
        (limit (if (and (pair? o) (pair? (cdr o)))
                   (cadr o)
                   (+ 1 (string-size str))))
        (start (string-cursor-start str))
        (end (string-cursor-end str)))
    (if (string-cursor>=? start end)
        '()
        (let lp ((i start) (n 1) (res '()))
          (cond
           ((>= n limit)
            (reverse (cons (substring-cursor str i) res)))
           (else
            (let* ((j (string-find str pred i))
                   (res (cons (substring-cursor str i j) res)))
              (if (string-cursor>=? j end)
                  (reverse res)
                  (lp (string-cursor-next str j) (+ n 1) res)))))))))

;;> Returns a copy of the string \var{str} with all characters
;;> matching \var{pred} (default \scheme{#\\space}) removed from the
;;> left.

(define (string-trim-left str . o)
  (let ((pred (make-char-predicate (if (pair? o) (car o) #\space))))
    (substring-cursor str (string-skip str pred))))

;;> Returns a copy of the string \var{str} with all characters
;;> matching \var{pred} (default \scheme{#\\space}) removed from the
;;> right.
#;
(define (string-trim-right str . o)
  (let ((pred (make-char-predicate (if (pair? o) (car o) #\space))))
    (substring-cursor str
                      (string-cursor-start str)
                      (string-skip-right str pred))))

;;> Returns a copy of the string \var{str} with all characters
;;> matching \var{pred} (default \scheme{#\\space}) removed from both
;;> sides.
#;
(define (string-trim-both str . o)
  (let* ((pred (if (pair? o) (car o) #\space))
         (left (string-skip str pred))
         (right (string-skip-right str pred)))
    (if (string-cursor>=? left right)
        ""
        (substring-cursor str left right))))

(define (string-mismatch prefix str)
  (let ((end1 (string-cursor-end prefix))
        (end2 (string-cursor-end str)))
    (let lp ((i (string-cursor-start prefix))
             (j (string-cursor-start str)))
      (if (or (string-cursor>=? i end1)
              (string-cursor>=? j end2)
              (not (eq? (string-cursor-ref prefix i) (string-cursor-ref str j))))
          j
          (lp (string-cursor-next prefix i) (string-cursor-next str j))))))

(define (string-mismatch-right suffix str)
  (let ((end1 (string-cursor-start suffix))
        (end2 (string-cursor-start str)))
    (let lp ((i (string-cursor-prev suffix (string-cursor-end suffix)))
             (j (string-cursor-prev str (string-cursor-end str))))
      (if (or (string-cursor<? i end1)
              (string-cursor<? j end2)
              (not (eq? (string-cursor-ref suffix i) (string-cursor-ref str j))))
          j
          (lp (string-cursor-prev suffix i) (string-cursor-prev str j))))))

;;> The fundamental string iterator.  Calls \var{kons} on each
;;> character of \var{str} and an accumulator, starting with
;;> \var{knil}.  If multiple strings are provided, calls \var{kons} on
;;> the corresponding characters of all strings, with the accumulator
;;> as the final argument, and terminates when the shortest string
;;> runs out.

(define (%string-fold kons knil str . los)
  (if (null? los)
      (let ((end (string-cursor-end str)))
        (let lp ((i (string-cursor-start str)) (acc knil))
          (if (string-cursor>=? i end)
              acc
              (lp (string-cursor-next str i)
                  (kons (string-cursor-ref str i) acc)))))
      (let ((los (cons str los)))
        (let lp ((is (map string-cursor-start los))
                 (acc knil))
          (if (any (lambda (str i)
                     (string-cursor>=? i (string-cursor-end str)))
                   los is)
              acc
              (lp (map string-cursor-next los is)
                  (apply kons (append (map string-cursor-ref los is)
                                      (list acc)))))))))

;;> Equivalent to \scheme{%string-fold}, but iterates over \var{str}
;;> from right to left.

(define (%string-fold-right kons knil str)
  (let ((end (string-cursor-end str)))
    (let lp ((i (string-cursor-start str)))
      (if (string-cursor>=? i end)
          knil
          (kons (string-cursor-ref str i) (lp (string-cursor-next str i)))))))

;;> \procedure{(string-count str check)}
;;>
;;> Count the number of characters in \var{str} for which \var{check}
;;> is true.
#;
(define (string-count str check)
  (let ((pred (make-char-predicate check)))
    (%string-fold (lambda (ch count) (if (pred ch) (+ count 1) count)) 0 str)))

(define (string-cursor-forward str cursor n)
  (if (zero? n)
      cursor
      (string-cursor-forward str (string-cursor-next str cursor) (- n 1))))

(define (string-cursor-back str cursor n)
  (if (zero? n)
      cursor
      (string-cursor-back str (string-cursor-prev str cursor) (- n 1))))

;;> \procedure{(string-cursor<? i j)}
;;> \procedure{(string-cursor>? i j)}
;;> \procedure{(string-cursor=? i j)}
;;> \procedure{(string-cursor<=? i j)}
;;> \procedure{(string-cursor>=? i j)}
;;>
;;> String cursor comparators.
;;/
