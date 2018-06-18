(module srfi-127 ()
  (import scheme)
  ;; Provide necessary R7RS-small identifiers
  (import (only chicken
            use include open-input-string case-lambda))
  (export generator->lseq lseq? lseq=?)
  (export lseq-car lseq-first lseq-cdr lseq-rest lseq-ref lseq-take lseq-drop)
  (export lseq-realize lseq->generator lseq-length lseq-append lseq-zip)
  (export lseq-map lseq-for-each lseq-filter lseq-remove)
  (export lseq-find lseq-find-tail lseq-take-while lseq-drop-while
          lseq-any lseq-every lseq-index lseq-member lseq-memq lseq-memv)
  (include "lseqs/r7rs-shim.scm")
  (include "lseqs/lseqs-impl.scm")
)
