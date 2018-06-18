(cond-expand
  (custom-macro-transformers
   (import (scheme base)))
  (else
   (import (except (scheme base)
		   define-syntax
		   let-syntax
		   letrec-syntax
		   syntax-rules))
   (import (srfi 147))))
