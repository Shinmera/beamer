(in-package "test")
(use-package :cl+trial)

(define-slide welcome
  (h "Welcome to Beamer")
  (p "Pellentesque dapibus suscipit ligula.  Donec posuere augue in quam.  Etiam vel tortor sodales tellus ultricies commodo.  Suspendisse potenti.  Aenean in sem ac leo mollis blandit.  Donec neque quam, dignissim in, mollis nec, sagittis eu, wisi.  Phasellus lacus.  Etiam laoreet quam sed arcu.  Phasellus at dui in ligula mollis ultricies.  Integer placerat tristique nisl.")
  (c "(dolist (foo bar) 
  (see foo))"))

(define-slide cube
  (define-asset (trial::workbench cube) mesh
    (make-cube 100))

  (define-shader-subject cube (vertex-entity located-entity rotated-entity colored-entity)
    ()
    (:default-initargs
     :location (vec 400 300 0)
     :vertex-array (asset 'trial::workbench 'cube)
     :color (vec 1 0 0)))

  (define-handler (cube tick) (ev dt)
    (incf (vx (rotation cube)) dt)
    (incf (vy (rotation cube)) dt))

  (enter-instance 'cube))
