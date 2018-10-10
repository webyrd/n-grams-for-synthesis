(load "prelude.scm")

;; ngrams-statistics structure:
;;
;; (((context form) . count) ...)
(define ngrams-statistics (read-data-from-file "tmp/statistics.scm"))

(define unique
  (lambda (l)
    (if (null? l)
      '()
      (cons (car l) (remove (car l) (unique (cdr l)))))))

(define all-contexts (unique (map caar ngrams-statistics)))

;; orderings-alist structure:
;;
;; ((context . (eval-relation ...)) ...)
(define orderings-alist
  (let ((ordering-for-context
          (lambda (ctx)
            (let ((ctx-stats (map (lambda (entry) (cons (cadar entry) (cdr entry)))
                                  (filter (lambda (entry) (equal? ctx (caar entry))) ngrams-statistics))))
              ;; ctx-stats has the structure:
              ;;
              ;; ((form . count) ...)
              ;;
              ;; For example,
              ;;
              ;; ((app . 33) ...)
              (let ((compare
                      (lambda (a b)
                        (> (alist-ref ctx-stats (car a) 0)
                           (alist-ref ctx-stats (car b) 0)))))
                (map cdr (list-sort compare expert-ordering-alist-ml-!-/evalo)))))))
    (map (lambda (ctx)
           (cons ctx (ordering-for-context ctx)))
         all-contexts)))

;; context -> list of eval-relations
(define order-eval-relations
  (lambda (context)
    (cond
      ((assoc context orderings-alist) => cdr)
      (else
        ;(error 'eval-expo (string-append "bad context " (symbol->string context)))

        ; symbol? doesn't appear in the data, so we'll return the expert ordering
        ; for such cases.
        expert-ordering-ml-!-/evalo))))

(define (!-/eval-expo expr gamma env type val context)
  (build-and-run-conde expr gamma env type val context
                       (order-eval-relations context)
                       ;expert-ordering
                       ))
