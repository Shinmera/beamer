(in-package "test")
(use-package :cl+trial)

(define-slide welcome
  (h "Welcome to Beamer")
  (p "Paragraph! With some more words and things in it now. Maybe even another line.
HARD BREAK!")
  (items
   "Something something"
   "Well this is going to be a rather large list entry so hopefully it'll line wrap."
   "Fart"
   "How about another list item?")
  (c "(michael-rosen:click :nice)"))

(define-slide cube
  (define-asset (trial::workbench cube) mesh
      (make-cube 100))

  (Define-asset (trial::workbench cat) image
      #p"cat.png")

  (define-shader-subject cube (vertex-entity located-entity rotated-entity textured-entity)
    ((zoom :initform 1 :accessor zoom))
    (:default-initargs
     :location (vec 400 300 0)
     :vertex-array (asset 'trial::workbench 'cube)
     :texture (asset 'trial::workbench 'cat)))

  (define-handler (cube tick) (ev dt tt)
    (incf (vx (rotation cube)) (* 10 dt))
    (incf (vy (rotation cube)) dt)
    (setf (zoom cube) (sin tt)))

  (defmethod paint :around ((cube cube) target)
    (with-pushed-matrix (model-matrix)
      (scale-by (zoom cube) (zoom cube) (zoom cube))
      (call-next-method)))

  (h "This is a spinning cube.")
  (p "Neat, right?")
  (p "Oh man.")
  (p "wow.")

  (enter-instance 'cube))
