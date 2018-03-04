(define-shader-subject cube (vertex-entity located-entity)
  ()
  (:default-initargs :vertex-array (make-cube 10)))

(enter-instance 'cube)
