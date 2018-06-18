;;; arrays as-plt-struct

(define-values (array:make
                array:array?
                array:vector
                array:index
                array:shape)
  (call-with-values
   (lambda () (struct array (vec idx shp)))
   (lambda (_ make array? vec set-vec idx set-idx shp set-shp)
     (values make
             array?
             vec
             idx
             shp))))
