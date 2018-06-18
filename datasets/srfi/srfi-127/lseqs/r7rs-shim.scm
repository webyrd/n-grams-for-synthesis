(define *eof-object* (read (open-input-string "")))
(define (eof-object) *eof-object*)
