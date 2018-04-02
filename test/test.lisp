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

(define-pool slide
  :base :trial)

(define-asset (slide cube) mesh
    (make-cube 100))

(Define-asset (slide cat) image
    #p"cat.png")

(define-shader-subject cube (vertex-entity
                             textured-entity
                             colored-entity
                             slide-subject)
  ())

(define-handler (cube tick) (ev dt tt)
  (incf (vx (rotation cube)) (* dt 0.1))
  (incf (vy (rotation cube)) dt))

(define-slide editor
  (p "Lisp Source:" :size 30 :margin (vec 20 0 0 0))
  (define-shader-subject cube (vertex-entity
  ;                             textured-entity
  ;                             colored-entity
                               slide-subject)
    ())
  
  (enter-instance 'cube
                  :location (vec 600 200 0)
                  :vertex-array (asset 'slide 'cube)
                  :texture (asset 'slide 'cat)
                  :color (vec 0.2 0.2 0.8))
  
  (editor "test.lisp" :start 41 :end 52 :trim 2 :size 22 :language :lisp :margin (vec 0 20))
  (p "Fragment Shader:" :size 30 :margin (vec 20 0 0 0))
  (c (getf (effective-shaders 'cube) :fragment-shader) :size 22 :language :glsl))
