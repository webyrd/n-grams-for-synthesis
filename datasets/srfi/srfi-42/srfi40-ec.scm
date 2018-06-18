; <PLAINTEXT>
; Eager Comprehensions and SRFI40-Streams
; =======================================
;
; sebastian.egner@philips.com, Eindhoven, The Netherlands, Feb-2003.
; Scheme R5RS (incl. macros), SRFI-23 (error).
; 
; Loading the implementation into Scheme48 0.57:
;   ,open srfi-23
;   ,load ec.scm
;   ; load SRFI-40 by whatever means
;   ,load srfi40-ec.scm
;
; Loading the implementation into PLT/DrScheme 202:
;   ; File > Open ... "ec.scm", click Execute
;   ; load SRFI-40 by whatever means
;   (load "srfi40-ec.scm")
;
; Loading the implementation into SCM 5d7:
;   (require 'macro) (require 'record) 
;   (load "ec.scm")
;   ; load SRFI-40 by whatever means
;   (load "srfi40-ec.scm")


; The Extension
; =============

; (:stream <vars> <arg1> <arg>*)
;   evaluates <arg1> <arg>*, which must result in one or more streams,
;   and runs <vars> through the concatenation of these streams.
;   Use :parallel, :while, and :until to limit enumeration.

(define-syntax :stream   ; very similar to :list
  (syntax-rules (index)
    ((:stream cc var (index i) arg1 arg ...)
     (:parallel cc (:stream var arg1 arg ...) (:integers i)) )
    ((:stream cc var arg1 arg2 arg ...)
     (:stream cc var (stream-append arg1 arg2 arg ...)) )
    ((:stream cc var arg)
     (:do cc
          (let ())
          ((t arg))
          (not (stream-null? t))
          (let ((var (stream-car t))))
          #t
          ((stream-cdr t)) ))))


; (stream-ec <qualifier>* <expression>)
;   constructs a SRFI40-stream for the sequence of values obtained
;   from <expression> as specified by <qualifiers>*. The values are
;   computed when they are needed.
;     The implementation uses call-with-current-continuation in a
;   non-trivial way.

(define-syntax stream-ec
  (syntax-rules (nested)
    ((stream-ec (nested q1 ...) q etc1 etc ...)
     (stream-ec (nested q1 ... q) etc1 etc ...) )
    ((stream-ec q1 q2             etc1 etc ...)
     (stream-ec (nested q1 q2)    etc1 etc ...) )
    ((stream-ec expression)
     (stream-ec (nested) expression) )

    ((stream-ec qualifier expression)
     (let ((value #f) (produce-value #f) (next-value #f))

       (stream-define (tail)
         (if (call-with-current-continuation
               (lambda (cc)
                 (set! produce-value cc)
                 (next-value #f)
                 #f ))
             (stream-cons value (tail))
             stream-null ))

       (stream-define (make-stream)
         (if (call-with-current-continuation
               (lambda (cc)
                 (set! produce-value cc)
                 (do-ec 
                   qualifier
                   (call-with-current-continuation
                     (lambda (cc)
                       (set! next-value cc)
                       (set! value expression)
                       (produce-value #t) )))
                 (produce-value #f) ))
             (stream-cons value (tail))
             stream-null ))

       (make-stream) ))))


; An Example
; ==========
;
; (define s1
;   (stream-ec (: x 10) (begin (display "<") (display x) (display ">")) x) )
;
; then
;
;   (stream-null? s1) => <0> #f
;   (stream-null? (stream-cdr s1)) => <1> #f
;   (stream->list s1) => <2><3>..<9> '(0 1 .. 9)

