(define (eval-expo expr env val context)
  ; for debugging build-and-run-code

  ;(conde
    ;((quote-evalo expr env val))
    ;((num-evalo expr env val))
    ;((bool-evalo expr env val))
    ;((var-evalo expr env val))
    ;((lambda-evalo expr env val))
    ;((app-evalo expr env val))
    ;((car-evalo expr env val))
    ;((cdr-evalo expr env val))
    ;((null?-evalo expr env val))
    ;((cons-evalo expr env val))
    ;((if-evalo expr env val))
    ;((equal?-evalo expr env val))
    ;((and-evalo expr env val))
    ;((or-evalo expr env val))
    ;((list-evalo expr env val))
    ;((symbol?-evalo expr env val))
    ;((not-evalo expr env val))
    ;((letrec-evalo expr env val))
    ;((match-evalo expr env val)))

  (build-and-run-conde expr env val
                       expert-ordering
                       ))
