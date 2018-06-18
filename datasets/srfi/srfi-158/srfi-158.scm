(module srfi-158 ()
  (import scheme)
  ;; Provide necessary R7RS-small identifiers
  (import (only chicken
            use include case-lambda call/cc when error
            let-values define-values open-input-string))
  (use (only srfi-4 make-u8vector u8vector u8vector-ref
             u8vector-set! u8vector-length))
  (export generator circular-generator make-iota-generator make-range-generator
          make-coroutine-generator list->generator vector->generator
          reverse-vector->generator string->generator
          bytevector->generator
          make-for-each-generator make-unfold-generator)
  (export gcons* gappend gcombine gfilter gremove
          gtake gdrop gtake-while gdrop-while
          gflatten ggroup gmerge gmap gstate-filter
          gdelete gdelete-neighbor-dups gindex gselect)
  (export generator->list generator->reverse-list
          generator->vector generator->vector!  generator->string
          generator-fold generator-map->list generator-for-each generator-find
          generator-count generator-any generator-every generator-unfold)
  (export make-accumulator count-accumulator list-accumulator
          reverse-list-accumulator vector-accumulator
          reverse-vector-accumulator vector-accumulator!
          string-accumulator bytevector-accumulator bytevector-accumulator!
          sum-accumulator product-accumulator)
  (include "r7rs-shim.scm")
  (include "srfi-158-impl.scm")
)
