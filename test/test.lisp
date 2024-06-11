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
  (nq* (orientation cube) (qfrom-angle +vx+ dt))
  (nq* (orientation cube) (qfrom-angle +vy+ (* 2 dt))))

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
                  :location (vec 500 100 0)
                  :vertex-array (// 'slide 'cube)
                  :texture (// 'trial:trial 'trial::cat)
                  :color (vec 0.2 0.2 0.8))
  ;; EDITOR-END
  
  (editor "test.lisp" :start "  ;; EDITOR-START"
                      :end   "  ;; EDITOR-END"
                      :trim 2 :language :lisp)
  (p "Fragment Shader:" :size 30)
  (c (second (assoc :fragment-shader (shader-source (find-class 'cube)))) :size 18 :language :glsl))
