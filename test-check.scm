(load "pmatch.scm")
(load "prelude.scm")

(define equal-to?
  (lambda (expected)
    (lambda (produced)
      (equal? expected produced))))

(define one-of?
  (lambda (expected*)
    (lambda (produced)
      (not (not (member produced expected*))))))

(define each-one-of?
  (lambda (expected*)
    (lambda (produced*)
      (andmap
        (lambda (produced)
          (not (not (member produced expected*))))
        produced*))))

(define-syntax test-p
  (lambda (stx)    
    (syntax-case stx ()
      ((_ title tested-expression pred-expr) (string? (syntax->datum #'title))
       #'(lambda (time-out-in-seconds)
           (let ((pred pred-expr))
             (run-for-max-time time-out-in-seconds
                               (lambda () tested-expression)
                               (lambda (produced stats-diff)
                                 (let ((success-indicator (if (pred produced)
                                                              'success
                                                              'failure)))
                                   (list success-indicator title 'tested-expression 'pred-expr produced (time->inexact-seconds (sstats-real stats-diff)) (sstats-bytes stats-diff))))
                               (lambda (stats-diff)
                                 (list 'timeout title 'tested-expression 'pred-expr 'timeout (time->inexact-seconds (sstats-real stats-diff)) (sstats-bytes stats-diff))))))))))

(define-syntax test
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (test-p title tested-expression (equal-to? expected-result)))))

(define run-for-max-time
  (lambda (max-time expr-th success-f timeout-f)
    (let ((begin-stats (statistics)))
      (let ((e (make-engine (lambda ()
                              (let ((produced (expr-th)))
                                (let ((end-stats (statistics)))
                                  (let ((stats-diff (sstats-difference end-stats begin-stats)))
                                    (success-f produced stats-diff))))))))
        (let loop ((e e))
          (e (expt 10 7) ; ticks
             (lambda (_ value)
               value)
             (lambda (e)
               (let ((end-stats (statistics)))
                 (let ((stats-diff (sstats-difference end-stats begin-stats)))
                   (if (< (time->inexact-seconds (sstats-real stats-diff)) max-time)
                     (loop e)
                     (timeout-f stats-diff)))))))))))

(define test-runner
  (lambda (timeout . test*)
    (let ((table (test-runner-aux test* '() timeout print-test-result)))
      (write-data-to-file table *output-table-file-name*))))

(define test-runner-build-table
  (lambda (timeout . test*)
    (test-runner-aux test* '() timeout (lambda (_) (void)))))


(define test-runner-aux
  (lambda (tests acc timeout f)
    (cond
      ((null? tests) (reverse acc))
      (else (let ((res ((car tests) timeout)))
              (f res)
              (test-runner-aux (cdr tests) (cons res acc) timeout f))))))

(define print-test-result
  (lambda (res)
    (pmatch res
      [(,status ,title ,tested-expression ,expected ,produced ,stats-real ,stats-bytes)
       (case status
         [(success)
          (printf "~s ~s ~s ~s\n" status title stats-real stats-bytes)]
         [(failure)
          (printf "!! ~s ~s ~s ~s\n" status title stats-real stats-bytes)
          (printf "!!   expected predicate: ~s\n" expected)
          (printf "!!   produced value: ~s\n" produced)]
         [(timeout)
          (printf "!! ~s ~s ~s ~s\n" status title stats-real stats-bytes)])])))

(define time->inexact-seconds
  (lambda (time)
    (let ((s (time-second time))
          (ns (time-nanosecond time)))
      (exact->inexact (+ s (/ ns (expt 10 9))))))) 

