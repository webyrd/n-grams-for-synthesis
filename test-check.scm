(load "pmatch.scm")

(define-syntax test
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (lambda (time-out-in-ticks)
       (let ((expected expected-result))
         (let ((begin-stats (statistics)))
           (let ((e (make-engine (lambda ()
                                   (let ((produced tested-expression))
                                     (let ((end-stats (statistics)))
                                       (let ((stats-diff (sstats-difference end-stats begin-stats)))
                                         (let ((success-indicator (if (equal? expected produced)
                                                                      'success
                                                                      'failure)))
                                           (list success-indicator title 'tested-expression expected produced stats-diff)))))))))
             (e time-out-in-ticks
                (lambda (_ value)
                  value)
                (lambda (_)
                  (let ((end-stats (statistics)))
                    (let ((stats-diff (sstats-difference end-stats begin-stats)))
                      (list 'timeout title 'tested-expression expected 'timeout stats-diff))))))))))))

(define test-runner
  (lambda (timeout . test*)
    (test-runner-aux test* '() timeout)))

(define test-runner-aux
  (lambda (tests acc timeout)
    (cond
      ((null? tests) acc)
      (else (let ((res ((car tests) timeout)))
              (print-test-result res)
              (test-runner-aux (cdr tests) (cons res acc) timeout))))))

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

#|
(define-syntax test
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (printf "Testing ~s\n" title)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (printf "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
                     'tested-expression expected produced)))))))
|#
