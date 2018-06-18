;;;; Module for Chicken's native 8-bit strings

(module utf8-srfi-152 ()

  ;; R5RS+ and utf8 procedures must not be imported, as we redefine them
  (import (rename (except scheme
                          string-length string-ref string-set! make-string string substring
                          string-copy string->list list->string string-fill!)
                  (string=? base-string=?)
                  (string<? base-string<?)
                  (string>? base-string>?)
                  (string<=? base-string<=?)
                  (string>=? base-string>=?)
                  (string-ci=? base-string-ci=?)
                  (string-ci<? base-string-ci<?)
                  (string-ci>? base-string-ci>?)
                  (string-ci<=? base-string-ci<=?)
                  (string-ci>=? base-string-ci>=?)))
  (import (only chicken include error use case-lambda
                        open-input-string open-output-string get-output-string))

  (import (only chicken include error use case-lambda
                        open-input-string open-output-string get-output-string))

  ;; Cherry-pick utf8 procedures and re-export them
  (use (only utf8
    string-length string-ref string-set! make-string string substring list->string display))
  (export string-length string-ref string-set! make-string string substring list->string)

  ;; Don't export R5RS procedures
  #;(no-export string? make-string list->string
               string-length string-ref substring
              string=? string<? string>? string<=? string>=?
              string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
              string-set!)

  ;; Export R5RS+ procedures
  (export string->list string-copy string-fill!
          string=? string<? string>? string<=? string>=?
          string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=? )

  ;; Export R7RS procedures (defined in r7rs-shim file and chicken module)
  (import (only utf8 read-string))
  (export string->vector vector->string string-map string-for-each
          read-string write-string string-copy! write-string)

  ;; Remaining exports, grouped as in the SRFI
  (export string-null? string-every string-any)
  (export string-tabulate string-unfold string-unfold-right)
  (export reverse-list->string)
  (export string-take string-drop string-take-right string-drop-right
          string-pad string-pad-right
          string-trim string-trim-right string-trim-both)
  (export string-replace)
  (export string-prefix-length string-suffix-length
          string-prefix? string-suffix?)
  (export string-index string-index-right string-skip string-skip-right
          string-contains string-contains-right
          string-take-while string-take-while-right
          string-drop-while string-drop-while-right
          string-break string-span)
  (export string-append string-concatenate string-concatenate-reverse
          string-join)
  (export string-fold string-fold-right string-count
          string-filter string-remove)
  (export string-replicate string-segment string-split)


  (include "macros.scm")
  (include "portable.scm")
  (include "extend-comparisons.scm")
  (include "r7rs-shim.scm")
)
