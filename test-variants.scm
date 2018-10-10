(load "pmatch.scm")
(load "prelude.scm")


(unless (file-exists? "test-variants.scm")
  (error 'test-variants "test-variants.scm doesn't exist--you are probably in the wrong folder"))


(system "rm -rf tmp")
(mkdir "tmp")


;; first, generate the statistics
(printf "$$$$$$ generating statistics\n")
(system "scheme n-grams.scm")


;; then, run benchmarks for different interpreters


(define variants-to-run '(
                          
                          ;;; !!! IMPORTANT !!!
                          ;;;
                          ;;; Interpreters for ML-like languages
                          ;;; should be run separately than the non-ML
                          ;;; interpreters, since the tests are
                          ;;; distinct.  Similarly, the type
                          ;;; inferencers should be run separately.

                          #| Combined inferencer/evaluator for ML-like language |#
                          ;"combined-dynamic-ordering-with-application-and-lookup-optimizations-ml-infer-evalo"
                          "combined-dynamic-ordering-ml-infer-evalo"
                          ;"combined-expert-ordering-ml-infer-evalo"
                          
                          
                          #| Separate evaluators/inferencers for ML-like language, curried: |#
                          ;"variant-dynamic-ordering-ml-curried-interp-and-infer"
                          ;"variant-dynamic-ordering-with-lookup-optimization-ml-curried-interp-and-infer"
                          ;"variant-expert-ordering-ml-curried-interp-and-infer"
                          
                          
                          #| Separate evaluators/inferencers for ML-like language: |#
                          #|
                          "variant-dynamic-ordering-with-application-and-lookup-optimizations-ml-interp-and-infer"
                          "variant-dynamic-ordering-ml-interp-and-infer"
                          "variant-expert-ordering-ml-interp-and-infer"
                          |#

                          
                          #| Type inferencers for ML-like language: |#
                          
                          ;"variant-expert-ordering-ml-infer"
                          ;"variant-dynamic-ordering-ml-infer"
                          ;"variant-dynamic-ordering-with-application-optimization-ml-infer"
                          ;"variant-dynamic-ordering-with-application-and-lookup-optimizations-ml-infer"
                          
                          
                          #| Interpreters for ML-like language: |#
                          
                          ;"variant-dynamic-ordering-with-application-and-lookup-optimizations-ml"
                          ;"variant-dynamic-ordering-ml"
                          ;"variant-expert-ordering-with-application-and-lookup-optimizations-ml"
                          
                          #|
                          "variant-dynamic-ordering-with-application-optimization-ml"                          
                          "variant-expert-ordering-with-application-optimization-ml"
                          "variant-expert-ordering-ml"                          
                          |#


                          
                          #| Interpreters for Schemely language: |#

                          ;"variant-dynamic-ordering-with-application-and-lookup-optimizations"
                          ;"variant-barliman-interpreter"
                          
                          #|
                          "variant-expert-ordering-with-application-and-lookup-optimizations"
                          "variant-expert-ordering-with-application-optimization"
                          
                          "variant-barliman-interpreter-incomplete"
                          
                          
                          "variant-dynamic-ordering-with-application-and-lookup-optimizations-incomplete"
                          
                          "variant-dynamic-ordering-with-application-optimization"
                          "variant-dynamic-ordering-with-application-optimization-incomplete"
                          
                          "variant-dynamic-ordering"
                          "variant-expert-ordering"
                          
                          "variant-old-skool"
                          "variant-old-skool-incomplete"
                          |#
                          ))

(for-each (lambda (v)
            (printf "===== ~a\n" v)
            (system (format "scheme ~a.scm" v)))
          variants-to-run)


(define variant-data-file (lambda (v) (format "tmp/~a-table.scm" v)))

(define test-names (map cadr (read-data-from-file (variant-data-file (car variants-to-run)))))

(define test-data (map (lambda (v)
                         (let ((data (read-data-from-file (variant-data-file v))))
                           (map (lambda (d)
                                  (pmatch d
                                    [(,status ,title ,tested-expression ,expected ,produced ,stats-real ,stats-bytes)
                                     (case status
                                       [(success) (number->string stats-real)]
                                       [(failure) "FAIL"]
                                       [(timeout) "TIME"])]))
                                data)))
                       variants-to-run))

(define transpose
  (lambda (l)
    (let loop ((acc '())
               (l l))
      (cond
        ((null? (car l)) (reverse acc))
        (else (loop (cons (map car l) acc) (map cdr l)))))))

(define summary-scm (cons (cons "" variants-to-run) (transpose (cons test-names test-data))))

(define to-csv
  (lambda (summary)
    (apply string-append
           (map
            (lambda (row)
              (string-append
               (apply string-append
                      (map
                       (lambda (entry)
                         (string-append entry ", "))
                       row))
               "\n"))
            summary))))

(define to-csv-tidy
  (lambda (summary)
    (let ((variant-names (cdar summary)))
      (let ((tests (cdr summary)))
        (apply string-append
               (cons
                (string-append "test-name" ", " "variant-name" "," "time" "\n")
                (map
                 (lambda (test)
                   (let ((test-name (car test))
                         (variant-times (cdr test)))
                     (apply string-append
                            (map
                             (lambda (variant-name variant-time)
                               (string-append test-name ", " variant-name ", " variant-time "\n"))
                             variant-names
                             variant-times))))
                 tests)))))))

(define summary-csv (to-csv summary-scm))

(define summary-csv-tidy (to-csv-tidy summary-scm))

(write-data-to-file summary-scm "tmp/summary.scm")

(display-data-to-file summary-csv "tmp/summary.csv")

(display-data-to-file summary-csv-tidy "tmp/summary-tidy.csv")

(exit)

#|
(define evalo-files '("interp-simplified.scm" "interp-old-style.scm" "interp-old-style-with-list-as-prim.scm"))

(eval '(begin (load "mk-vicare.scm") (load "mk.scm") (load "test-check.scm") (load "interp-simplified.scm") (load "simplified-interp-tests.scm")) (copy-environment (scheme-environment)))

(eval 'evalo (copy-environment (scheme-environment)))

(module m1 ()
  (import scheme)
  (load "mk-vicare.scm")
  (load "mk.scm")
  (load "test-check.scm")
  (load "interp-simplified.scm")
  (load "simplified-interp-tests.scm"))

(system "scheme mk-vicare.scm mk.scm test-check.scm interp-simplified.scm simplified-interp-tests.scm")


;; could modify the test runner to print the output table as a s-expression, and then read from the output port
;; returned by process or open-process-ports
(process "scheme mk-vicare.scm mk.scm test-check.scm interp-simplified.scm simplified-interp-tests.scm")

(open-process-ports "scheme mk-vicare.scm mk.scm test-check.scm interp-simplified.scm simplified-interp-tests.scm")
|#
