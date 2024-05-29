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
    (make-cube-mesh 100))

(define-shader-entity cube (vertex-entity
                            textured-entity
                            colored-entity
                            transformed-entity
                            listener)
  ())

(defmethod shared-initialize :after ((cube cube) slots &key &allow-other-keys))

(define-handler (cube tick) (dt)
  (incf (vx (rotation cube)) (* dt 0.1))
  (incf (vy (rotation cube)) dt))

(define-slide editor
  (p "Lisp Source:")
  ;; EDITOR-START
  (define-shader-entity cube (vertex-entity
                              ;;textured-entity
                              ;;colored-entity
                              transformed-entity
                              listener)
    ())
  
  (enter-instance 'cube
                  :location (vec 600 200 0)
                  :vertex-array (asset 'slide 'cube)
                  :texture (asset 'trial:trial 'trial::cat)
                  :color (vec 0.2 0.2 0.8))
  ;; EDITOR-END
  
  (editor "test.lisp" :start "  ;; EDITOR-START"
                      :end   "  ;; EDITOR-END"
                      :trim 2 :language :lisp)
  (p "Fragment Shader:" :size 30)
  (c (second (assoc :fragment-shader (shader-source (find-class 'cube)))) :size 22 :language :glsl))
