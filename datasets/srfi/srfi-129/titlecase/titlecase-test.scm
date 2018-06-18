;;;; Chicken-specific tests for titlecase library

;;; Note:  The strings embed Unicode characters using the Chicken-specific lexical
;;; syntax "\u1234" rather than the R7RS syntax "\x1234;"

(use utf8)
(use titlecase)
(use test)
(test-group "titlecase"
  (test-group "titlecase/predicate"
    (test-assert (char-title-case? #\u01C5))
    (test-assert (char-title-case? #\u1FFC))
    (test-assert (not (char-title-case? #\Z)))
    (test-assert (not (char-title-case? #\z)))
  ) ; end titlecase/predicate

  (test-group "titlecase/char"
    (test #\u01C5 (char-titlecase #\u01C4))
    (test #\u01C5 (char-titlecase #\u01C6))
    (test #\Z (char-titlecase #\Z))
    (test #\Z (char-titlecase #\z))
  ) ; end titlecase/char

  (test-group "titlecase/string"
    (test "\u01C5" (string-titlecase "\u01C5"))
    (test "\u01C5" (string-titlecase "\u01C4"))
    (test "Ss" (string-titlecase "\u00DF"))
    (test "Xi\u0307" (string-titlecase "x\u0130"))
    (test "\u1F88" (string-titlecase "\u1F80"))
    (test "\u1F88" (string-titlecase "\u1F88"))
    (define Floo "\uFB02oo")
    (define Floo-bar "\uFB02oo bar")
    (define Baffle "Ba\uFB04e")
    (define LJUBLJANA "\u01C7ub\u01C7ana")
    (define Ljubljana "\u01C8ub\u01C9ana")
    (define ljubljana "\u01C9ub\u01C9ana")
    (test "Bar Baz" (string-titlecase "bAr baZ"))
    (test "Floo" (string-titlecase "floo"))
    (test "Floo" (string-titlecase "FLOO"))
    (test "Floo" (string-titlecase Floo))
    (test "Floo Bar" (string-titlecase "floo bar"))
    (test "Floo Bar" (string-titlecase "FLOO BAR"))
    (test "Floo Bar" (string-titlecase Floo-bar))
    (test Baffle (string-titlecase Baffle))
    (test Ljubljana (string-titlecase LJUBLJANA))
    (test Ljubljana (string-titlecase Ljubljana))
    (test Ljubljana (string-titlecase ljubljana))
  ) ; end titlecase/string

) ; end titlecase

(test-exit)
