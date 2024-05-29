(in-package #:beamer-user)

(define-slide welcome
  (h "Welcome to Beamer")
  (p "Paragraph!")
  (items
   "Something something"
   "Well this is going to be a rather large list entry so hopefully it'll line wrap."
   "Fart")
  (c "(michael-rosen:click :nice)"))

(define-pool slide)

(define-asset (slide cube) mesh
    (make-cube 100))

(define-shader-subject cube (vertex-entity
                             textured-entity
                             colored-entity
                             transformed-entity)
  ())

(define-handler (cube tick) (dt)
  (incf (vx (rotation cube)) (* dt 0.1))
  (incf (vy (rotation cube)) dt))

(define-slide editor
  (p "Lisp Source:" :size 30 :margin (vec 20 0 0 0))
  ;; EDITOR-START
  (define-shader-subject cube (vertex-entity
                               ;;textured-entity
                               ;;colored-entity
                               slide-subject)
    ())
  
  (enter-instance 'cube
                  :location (vec 600 200 0)
                  :vertex-array (asset 'slide 'cube)
                  :texture (asset 'trial 'cat)
                  :color (vec 0.2 0.2 0.8))
  ;; EDITOR-END
  
  (editor "test.lisp" :start "  ;; EDITOR-START"
                      :end   "  ;; EDITOR-END"
                      :trim 2 :language :lisp)
  (p "Fragment Shader:" :size 30)
  (c (getf (effective-shaders 'cube) :fragment-shader) :size 22 :language :glsl))
