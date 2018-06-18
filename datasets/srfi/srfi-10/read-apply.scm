;				Read-time _apply_
;
; See my message posted on comp.lang.scheme, Tue Apr 13 03:44:41 1999,
; for motivation, explanation and details.
; http://www.deja.com/[ST_rn=ps]/viewthread.xp?AN=465207316&search=thread&recnum=%3c7er28h$5i0$1@nnrp1.dejanews.com%3e%231/3
;
; Read-time constructors are stored in a _readtable_. As explained in
; gambc30/lib/_io.scm, a readtable structure stores parsing information
; for the Scheme Reader. For example, readtable contains flags indicating
; if symbols should be converted to uppercase or if keywords are allowed. A
; readtable also has a table of characters that act as delimiters as well as
; a named character (eg, #\tab) table. The last slot in a readtable
; is a character-handler table. The latter contains functions
; called when a specific character (eg., #\#, #\(, #\\ ) is encountered.
; The handler is to process that (and perhaps a few following characters)
; and to return the corresponding Scheme object.
;
; To implement read-time application, we replace Gambit's handler for a
; character #\# with our own. Our handler detects a character sequence
; "#,", looks up the corresponding read-time constructor, and applies it.
;
; A table of read-time constructors in effect ought to have its own slot
; in a readtable. This is the most logical way of implementing this feature.
; However, if we add a new slot to a readtable, we must recompile the
; whole Gambit system.
;
; Therefore, for expedience (as a hack) we hide the table of read-time
; ctors inside our sharp-handler (our replacement for the #\#-character
; handler). While the original sharp-handler was a procedure, the modified
; one is a closure. Gambit reader always calls a character handler with two
; arguments: read-time environment and a character. Our handler
; takes two more, _optional_ arguments: a tag and the corresponding ctor.
; Note that extending a procedure to take optional arguments is similar
; to _inheritance_. We substitute our sharp handler at the moment when
; a new reader-ctor is defined (and the current readtable happens to
; point to the old handler). Thus until the first reader-ctor is defined, no
; modifications to the Gambit system are made.
;
; See vread-apply.scm for verification tests of this feature.
;
; $Id$

(declare 
 (block)
 (standard-bindings)
  (extended-bindings)		; needed for #!optional to be recognized...
 (fixnum)
)
(##include "myenv.scm")


; Macros included from gambc30/lib/_io.scm and gambc30/lib/header.scm
(##define-macro (##read-next-char-or-eof re) ; possibly returns end-of-file
  `(port-read-char (##readenv-port ,re)))
(##define-macro (##peek-next-char-or-eof re) ; possibly returns end-of-file
  `(port-peek-char (##readenv-port ,re)))
(##define-macro (port-read-char port)
  `(let ((port ,port))
     ((##vector-ref port 5) port)))
(##define-macro (port-peek-char port)
  `(let ((port ,port))
     ((##vector-ref port 6) port)))
(##define-macro (force-vars vars expr)
  `(let ,(map (lambda (x) `(,x (##force ,x))) vars) ,expr))

; This is the regular #\#-character handler from Gambit; it's modified
; as noted to handle "#," and to maintain a table of tag - ctor associations
; To report an error, a Gambit procedure (##read-error read-env msg . args)
; is called. This procedure makes a closure that encapsulates a
; tag . reader-ctor association table.
; The part of this code lifted from the Gambit system requires
; a few macros defined above.

(define (make-sharp-handler-closure)
  (let ((reader-ctor-table '()))
			; Add or replace an association between a tag
			; and a read-time-ctor procedure
    (define (add-ctor! tag ctor)
      (let ((old-association (assq tag reader-ctor-table)))
        (if old-association
          (set-cdr! old-association ctor)
          (push! (cons tag ctor) reader-ctor-table))))

    (lambda (re c . ctor-maintenance-args)

      (if (pair? ctor-maintenance-args)
        (apply add-ctor! ctor-maintenance-args)

        ; This is the regular #\#-character handler from Gambit, ##read-sharp.
        ; it's modified as noted to handle "#,"
  (let ((start-pos (##readenv-current-filepos re)))
    (##read-next-char-or-eof re) ; skip #\#
    (let ((next (##peek-next-char re)))
      (cond ((char=? next #\()
             (##read-next-char-or-eof re) ; skip #\(
             (##readenv-filepos-set! re start-pos) ; set pos to start of datum
             (let ((vect (##build-vector re 'vector start-pos #\))))
               (##readenv-wrap re vect)))
            ((char=? next #\\)
             (##read-next-char-or-eof re) ; skip #\\
             (##readenv-filepos-set! re start-pos) ; set pos to start of datum
             (let ((c (##read-next-char re)))
               (if (##readtable-char-delimiter?
                    (##readenv-readtable re)
                    (##peek-next-char-or-eof re))
                 (##readenv-wrap re c)
                 (let ((name (##build-delimited-string re c 1)))
                   (let ((x (##read-assoc-string-ci=?
                             name
                             (##readtable-named-char-table
                              (##readenv-readtable re)))))
                     (if x
                       (##readenv-wrap re (cdr x))
                       (let ((n (string->number name 10)))
                         (if (and n
                                  (integer? n)
                                  (exact? n))
                           (if (in-unicode-range? n)
                             (##readenv-wrap re (unicode->character n))
                             (##read-error-char-range re))
                           (##read-error-char-name re name)))))))))
            ((char=? next #\|)
             (let ((old-pos (##readenv-filepos re)))
               (##readenv-filepos-set! re start-pos) ; in case error in comment
               (##read-next-char-or-eof re) ; skip #\|
               (##skip-extended-comment re #\# #\| #\| #\#)
               (##readenv-filepos-set! re old-pos) ; restore pos
               (##read-datum-or-none-or-dot re))) ; read what follows comment
;;; Begin added part
           ((char=? next #\,)
             (##read-next-char-or-eof re) ; skip #\,
             (##readenv-filepos-set! re start-pos) ; set pos to start of datum
             (let ((obj (##desourcify (##read-datum re))))
               (cond
                 ((not (pair? obj)) 
                   ((##readenv-error-proc re) re "invalid #, form: " obj))
                 ((assq (car obj) reader-ctor-table) =>
                   (lambda (ctor-assoc) 
                     (##readenv-wrap re
                       (apply (cdr ctor-assoc) (cdr obj)))))
                 (else
                   ((##readenv-error-proc re) re "Unknown reader-ctor tag: "
                     (car obj))))))
;;; End added part

            ((char=? next #\!)
             (##read-next-char-or-eof re) ; skip #\!
             (##readenv-filepos-set! re start-pos) ; set pos to start of datum
             (let ((name (##build-delimited-string re #\space 0)))
               (let ((x (##read-assoc-string-ci=?
                         name
                         (##readtable-sharp-bang-table
                          (##readenv-readtable re)))))
                 (if x
                   (##readenv-wrap re (cdr x))
                   (##read-error-sharp-bang-name re name)))))
            ((char=? next #\#)
             (##read-next-char-or-eof re) ; skip #\#
             (##readenv-filepos-set! re start-pos) ; set pos to start of datum
             (let ((sym (##build-delimited-symbol re #\# 2)))
               (##readenv-wrap re sym)))
            (else
             (##readenv-filepos-set! re start-pos) ; set pos to start of datum
             (let* ((s
                     (##build-delimited-string re c 1))
                    (obj
                     (or (string->number s 10)
                         (let ()

                           (define (build-vect re kind)
                             (let ((c (##read-next-char re)))
                               (if (char=? c #\()
                                 (##build-vector re kind start-pos #\))
                                 (##read-error-vector re))))

                           (cond ((string-ci=? s "#f")
				  #f)
                                 ((string-ci=? s "#t")
                                  #t)
                                 ((string-ci=? s "#u8")
                                  (build-vect re 'u8vector))
                                 ((string-ci=? s "#u16")
                                  (build-vect re 'u16vector))
                                 ((string-ci=? s "#u32")
                                  (build-vect re 'u32vector))
                                 ((string-ci=? s "#f32")
                                  (build-vect re 'f32vector))
                                 ((string-ci=? s "#f64")
                                  (build-vect re 'f64vector))
                                 (else
                                  (##read-error-sharp-token re s)))))))
               (##readenv-wrap re obj))))))))))



; A top-level procedure to define new read-time constructors


(define define-reader-ctor

  (let ((original-sharp-handler		; see gambc30/lib/_io.scm
        (##readtable-char-handler (##current-readtable) #\#)))

    (define (##readtable? x)		; Lifted from gambc30/lib/_io.scm
      (and (##structure? x)
        (##eq? (##vector-ref x 0) ##readtable-tag)))

    (define (original-sharp-handler? readtable)
      (eq? (##readtable-char-handler readtable #\#)
          original-sharp-handler))

    (define (set-our-sharp-handler! readtable)
      (##readtable-char-handler-set! readtable #\#
        (make-sharp-handler-closure)))

    (define (readtable-read-ctor-set! readtable tag ctor)
      ((##readtable-char-handler readtable #\#) #f #f tag ctor)
      )

    (lambda (tag ctor #!optional (readtable (##current-readtable)))
      (force-vars (tag ctor readtable)
        (if (not (##symbol? tag))
          (##trap-check-symbol 'define-reader-ctor tag ctor) 
          (if (##readtable? readtable) 
            (begin
              (if (original-sharp-handler? readtable)
                (set-our-sharp-handler! readtable))
              (readtable-read-ctor-set! readtable tag ctor)
              (##void))
            (##trap-check-readtable 'define-reader-ctor tag ctor readtable))))
)))

