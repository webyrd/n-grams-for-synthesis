;;;; Portable (generic) versions of Chicken fixnum operations.

;;; All of these should be redefined on all systems that have actual
;;; fixnum support.  The names aren't necessarily the native Chicken
;;; names, but the names by which they are imported from the `chicken`
;;; module by srfi-143.scm, the Chicken module definition.

;;; Fixnum limits

;; 24 is the SRFI minimum width.  Here are some appropriate values for
;; other Schemes, 32-bit and 64-bit, that don't implement R6RS:
;; Gambit 30/61, Scheme48 30/61, Chibi 30/61, SCM 30/61,
;; STklos 30/61, Gauche 29/61, RScheme 29, MIT 26/56.

(define fx-width 24)
(define fx-greatest 8388607)
(define fx-least -8388608)

(define (fixnum? x)
  (and (exact-integer? x) (<= fx-least x fx-greatest)))

;;; Basic arithmetic

(define (fx+ i j) (+ i j))
(define (fx- i j) (- i j))
(define (fx* i j) (* i j))
(define (fxquotient i j) (quotient i j))
(define (fxremainder i j) (remainder i j))
(define (fxneg i) (- i))

;;; The following are defined as syntax, because they are never exported
;;; and are not recursive, so they will be automatically inlined.
;;; But if you have efficient procedural versions, by all means use them.

(define-syntax chicken:fxmax
  (syntax-rules ()
    ((chicken:fxmax i j) (if (> i j) i j))))

(define-syntax chicken:fxmin
  (syntax-rules ()
    ((chicken:fxmin i j) (if (< i j) i j))))

(define-syntax chicken:fx=
  (syntax-rules ()
    ((chicken:fx= i j) (= i j))))

(define-syntax chicken:fx<
  (syntax-rules ()
    ((chicken:fx< i j) (< i j))))

(define-syntax chicken:fx>
  (syntax-rules ()
    ((chicken:fx> i j) (> i j))))

(define-syntax chicken:fx<=
  (syntax-rules ()
    ((chicken:fx<= i j) (<= i j))))

(define-syntax chicken:fx>=
  (syntax-rules ()
    ((chicken:fx>= i j) (>= i j))))

(define (fxodd? i) (odd? i))

(define (fxeven? i) (even? i))
