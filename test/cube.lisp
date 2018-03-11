(in-package "cube")

(define-asset (trial::workbench cube) mesh
    (make-cube 100))

(define-shader-subject cube (vertex-entity located-entity colored-entity)
  ()
  (:default-initargs
   :vertex-array (asset 'trial::workbench 'cube)
   :color (vec 1 0 0)))

(enter-instance 'cube)

(h "Fuck")
