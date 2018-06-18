(module titlecase ()
  (import (except scheme string-ref string-length
                         char-upper-case? char-lower-case? char-upcase char-downcase))
  (import (only chicken export include use))
  (export char-title-case? char-titlecase string-titlecase)
  (include "titlemaps.scm")
  (include "chicken-shim.scm")
  (include "titlecase-impl.scm")
)
