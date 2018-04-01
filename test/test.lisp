(in-package "test")
(use-package :cl+trial)

(define-shader-subject slide-subject (located-entity rotated-entity selectable)
  ()
  (:default-initargs
   :location (vec 400 300 0)))

(defmethod initialize-instance :after ((subject slide-subject) &key &allow-other-keys))

(define-slide welcome
  (h "Welcome to Beamer")
  (p "Paragraph!")
  (items
   "Something something"
   "Well this is going to be a rather large list entry so hopefully it'll line wrap."
   "Fart")
  (c "(michael-rosen:click :nice)"))

(define-slide cube
  (define-asset (trial::workbench cube) mesh
      (make-cube 100))

  (Define-asset (trial::workbench cat) image
      #p"cat.png")

  (define-shader-subject cube (slide-subject vertex-entity textured-entity)
    ()
    (:default-initargs
     :vertex-array (asset 'trial::workbench 'cube)
     :texture (asset 'trial::workbench 'cat)))

  (define-handler (cube tick) (ev dt tt)
    (incf (vx (rotation cube)) (* 10 dt))
    (incf (vy (rotation cube)) dt))

  (h "This is a cube")
  (enter-instance 'cube))

(define-slide editor
  (h "Hi there")

  (define-shader-subject cube (vertex-entity
                               textured-entity
                               ;colored-entity
                               slide-subject)
    ())

  (enter-instance 'cube
                  :location (vec 800 300 0)
                  :vertex-array (asset 'trial::workbench 'cube)
                  :texture (asset 'trial::workbench 'cat)
                  :color (vec 0.2 0.2 0.8))
  
  (editor "test.lisp" :start 40 :size 16)
  (c (getf (effective-shaders 'cube) :fragment-shader) :size 16 :margin 0 :language :glsl))
