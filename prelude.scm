(define write-data-to-file
  (lambda (data file-name)
    (let ((op (open-file-output-port file-name (file-options no-fail) (buffer-mode block) (make-transcoder (utf-8-codec)))))
      (write data op)
      (close-output-port op))))

(define display-data-to-file
  (lambda (data file-name)
    (let ((op (open-file-output-port file-name (file-options no-fail) (buffer-mode block) (make-transcoder (utf-8-codec)))))
      (display data op)
      (close-output-port op))))

(define read-data-from-file
  (lambda (file-name)
    (let ((op (open-file-input-port
               file-name
               (file-options no-fail)
               (buffer-mode block)
               (make-transcoder (utf-8-codec)))))
      (let ((res (read op)))
        (close-input-port op)
        res))))

