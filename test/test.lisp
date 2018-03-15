(in-package "test")
(use-package :cl+trial)

(define-slide welcome
  (h "Welcome to Beamer")
  (p "This is a paragraph")
  (p "Pellentesque dapibus suscipit ligula.  Donec posuere augue in quam.  Etiam vel tortor sodales tellus ultricies commodo.  Suspendisse potenti.  Aenean in sem ac leo mollis blandit.  Donec neque quam, dignissim in, mollis nec, sagittis eu, wisi.  Phasellus lacus.  Etiam laoreet quam sed arcu.  Phasellus at dui in ligula mollis ultricies.  Integer placerat tristique nisl."))

(define-slide cube
  (define-asset (trial::workbench cube) mesh
    (make-cube 100))

  (define-shader-subject cube (vertex-entity located-entity colored-entity)
    ()
    (:default-initargs
     :vertex-array (asset 'trial::workbench 'cube)
     :color (vec 1 0 0)))

  (enter-instance 'cube))
