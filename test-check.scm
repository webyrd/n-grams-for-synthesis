(load "pmatch.scm")

(define-syntax test
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (lambda (time-out-in-seconds)
       (let ((expected expected-result))
         (run-for-max-time time-out-in-seconds
                           (lambda () tested-expression)
                           (lambda (produced stats-diff)
                             (let ((success-indicator (if (equal? expected produced)
                                                          'success
                                                          'failure)))
                               (list success-indicator title 'tested-expression expected produced stats-diff)))
                           (lambda (stats-diff)
                             (list 'timeout title 'tested-expression expected 'timeout stats-diff))))))))

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
    (test-runner-aux test* '() timeout print-test-result)))

(define test-runner-build-table
  (lambda (timeout . test*)
    (test-runner-aux test* '() timeout (lambda (_) (void)))))


(define test-runner-aux
  (lambda (tests acc timeout f)
    (cond
      ((null? tests) acc)
      (else (let ((res ((car tests) timeout)))
              (f res)
              (test-runner-aux (cdr tests) (cons res acc) timeout f))))))

(define print-test-result
  (lambda (res)
    (pmatch res
      [(,status ,title ,tested-expression ,expected ,produced ,stats)
       (when (not (eqv? 'success status))
         (display "!! "))
       (printf "~s ~s ~s ~s\n" status title (time->inexact-seconds (sstats-real stats)) (sstats-bytes stats))])))

(define time->inexact-seconds
  (lambda (time)
    (let ((s (time-second time))
          (ns (time-nanosecond time)))
      (exact->inexact (+ s (/ ns (expt 10 9)))))))
