;;;; Shim to provide UTF-8-aware R7RS-compatible routines for Chicken

;;; Get the necessary UTF-8-aware routines

;; Override basic string access procedures using the utf8 module
(use (only utf8 string string-ref string-length))

;; Override character case predicates using the utf8-srfi-14 and unicode-char-sets modules
(use (only utf8-srfi-14 char-set-contains?))
(use (only unicode-char-sets char-set:uppercase char-set:lowercase))
;; Note: upper-case and lower-case are ASCII-only, we want the Unicode versions
(define (char-upper-case? ch) (char-set-contains? char-set:uppercase ch))
(define (char-lower-case? ch) (char-set-contains? char-set:lowercase ch))

;; Override char-upcase and char-downcase using the utf8-case-map egg
;; Version 3.4.1 and earlier do not export char-upcase-single and char-downcase-single
;; You can retrieve the egg and patch utf8-case-map.scm to export them
(use (rename utf8-case-map (char-upcase-single char-upcase) (char-downcase-single char-downcase)))
