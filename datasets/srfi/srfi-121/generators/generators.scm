(module srfi-121 ()
  (import scheme)
  ;; Provide necessary R7RS-small identifiers
  (import (only chicken
            use include case-lambda call/cc when define-values open-input-string))
  (import (only extras read-line))
  (require-library srfi-4)
  (import (only srfi-4 u8vector-ref u8vector-length))
  (export generator make-iota-generator make-range-generator
          make-coroutine-generator list->generator vector->generator
          reverse-vector->generator string->generator
          bytevector->generator
          make-for-each-generator make-unfold-generator)
  (export gcons* gappend gcombine gfilter gremove
          gtake gdrop gtake-while gdrop-while
          gdelete gdelete-neighbor-dups gindex gselect)
  (export generator->list generator->reverse-list
          generator->vector generator->vector!  generator->string
          generator-fold generator-for-each generator-find
          generator-count generator-any generator-every generator-unfold)
  (include "generators/r7rs-shim.scm")
  (include "generators/generators-impl.scm")
)
