(load "prelude.scm")

;; ngrams-statistics structure:
;;
;; (((context form) . count) ...)
(define ngrams-statistics-ml-infer (read-data-from-file "tmp/statistics.scm"))

(define unique-ml-infer
  (lambda (l)
    (if (null? l)
      '()
      (cons (car l) (remove (car l) (unique-ml-infer (cdr l)))))))

(define all-contexts-ml-infer (unique-ml-infer (map caar ngrams-statistics-ml-infer)))

;; orderings-alist structure:
;;
;; ((context . (eval-relation ...)) ...)
(define orderings-alist-ml-infer
  (let ((ordering-for-context
          (lambda (ctx)
            (let ((ctx-stats (map (lambda (entry) (cons (cadar entry) (cdr entry)))
                                  (filter (lambda (entry) (equal? ctx (caar entry))) ngrams-statistics-ml-infer))))
              ;; ctx-stats has the structure:
              ;;
              ;; ((form . count) ...)
              ;;
              ;; For example,
              ;;
              ;; ((app . 33) ...)
              (let ((compare
                      (lambda (a b)
                        (> (alist-ref-ml-infer ctx-stats (car a) 0)
                           (alist-ref-ml-infer ctx-stats (car b) 0)))))
                (map cdr (list-sort compare expert-ordering-alist-ml-infer)))))))
    (map (lambda (ctx)
           (cons ctx (ordering-for-context ctx)))
         all-contexts-ml-infer)))

;; context -> list of !-o-relations
(define order-!-o-relations-ml-infer
  (lambda (context)
    (cond
      ((assoc context orderings-alist-ml-infer) => cdr)
      (else
        ;(error '!-o (string-append "bad context " (symbol->string context)))

        ; symbol? doesn't appear in the data, so we'll return the expert ordering
        ; for such cases.
        expert-ordering-ml-infer))))

(define (!-o expr gamma type context)
  (build-and-run-conde-ml-infer expr gamma type
                                (order-!-o-relations-ml-infer context)
                                ;;expert-ordering
                                ))
