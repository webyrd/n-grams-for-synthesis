;;
;; small test tool for Scheme
;;

(define *test-total-count*  0)
(define *test-failed-count* 0)

(define (test-start txt)
  (display "<TEST-START>: ")
  (display txt)
  (newline)
  (set! *test-total-count*  0)
  (set! *test-failed-count* 0))

(define (test-section txt)
  (display "<TEST-SECTION>: ")
  (display txt)
  (newline))

(define (test-end)
  (display "<TEST-END>")
  (newline)
  (display "TOTAL  : ")
  (display *test-total-count*)
  (newline)
  (display "PASSED : ")
  (display (- *test-total-count* *test-failed-count*))
  (newline)
  (display "FAILED : ")
  (display *test-failed-count*)
  (newline)
  (display "RESULT : ")
  (display (if (= *test-failed-count* 0) "OK" "** NG **"))
  (newline))

(define-syntax expect
  (syntax-rules ()
    ((expect ans expr)
     (expect ans expr equal?))
    ((expect ans expr check)
     (let ((expected ans)
           (result   expr))
       (set! *test-total-count* (+ *test-total-count* 1))
       (cond
        ((check expected result)
         (display "PASSED: ")
         (write   'expr)
         (display ", expects ")
         (write   expected)
         (newline))
        (else
         (set! *test-failed-count* (+ *test-failed-count* 1))
         (newline)
         (display "** FAILED **: ")
         (write   'expr)
         (display ", expects ")
         (write   expected)
         (display ", but got ")
         (write   result)
         (newline)
         (newline)))))))

