(in-package "els")
(use-package :cl+trial)

(define-shader-subject slide-subject (vertex-entity located-entity rotated-entity selectable)
  ()
  (:default-initargs
   :location (vec 600 200 100)))

(defmethod initialize-instance :after ((subject slide-subject) &key &allow-other-keys))

(defmethod paint ((subject slide-subject) target)
  (with-pushed-matrix (model-matrix)
    (scale-by 10 10 10)
    (call-next-method)))

(define-pool els
  :base :trial)

(define-asset (els teapot) mesh
    #p"teapot.vf"
  :geometry-name :teapotmesh)

(define-asset (els wall) image
    #p"brickwall.jpg"
  :wrapping :repeat)

(define-slide title
  (h "Modular Graphics with CLOS" :size 62)
  (image #p"avatar.png" :margin (vec 300 50 300 0))
  (p "@Shinmera" :margin (vec 302 0))
  (c "https://shinmera.com" :language :link :size 32 :margin (vec 220 30)))

(define-slide intro
  (h "Introduction to Modern Graphics")
  (items
   "CPU <-> GPU sync is slow"
   "Upload as much ahead of time as possible"
   "When drawing everything is done GPU-side"
   "Customise drawing behaviour through code"))

(define-slide glsl
  (h "GLSL")
  (items
   "C-like language for GPU programs"
   "Specifically about graphics"
   "Pass strings to driver, compiles to GPU"))

(define-slide glsl-example
  (h "GLSL Example")
  (c "out vec4 color;

void main() {
  vec3 p = gl_FragCoord.xyz;
  color.gb *= clamp(abs(pow(tan((p.y+p.x)/50)+1, 20)), 0, 1);
  color.rgb *= 1-p.z*2000-1998.4;
}" :language :glsl :size 24))

(define-slide pipeline
  (h "OpenGL Render Pipeline")
  (image "pipeline.png" :margin (vec 0 100)))

(define-slide problem
  (h "Here's the Problem")
  (items
   "Want to define behaviour modularly"
   "Want to die draw behaviour to objects"
   "Only one program per stage at a time"))

(define-slide solution
  (h "Here's a Solution")
  (items
   "Parse GLSL into AST"
   "Use semantic analysis to merge programs"
   "Emit single program with combined behaviour"
   "Use CLOS to attach shaders to classes"))

(define-slide glsl-toolkit
  (h "GLSL-Toolkit")
  (items
   "Implements a full GLSL 4.1 parser"
   "GLSL code-walker for semantic analysis"
   "User just calls MERGE-SHADER-SOURCES"))

(define-slide where-lisp
  (h "Where's the Lisp?"))

(define-slide clos
  (h "CLOS & MOP Primer")
  (items
   "Classes allow multiple inheritance"
   "Class behaviour is defined by metaclasses"
   "MOP can attach new information to classes"))

(define-slide connecting
  (h "Connecting Shaders and Classes")
  (items
   "Metaclass that holds shader sources"
   "On inheritance, sources are merged"
   "CLOS' class-precedence defines merge order"))

(define-slide teapot
  (h "This is a teapot")

  (define-shader-subject teapot (slide-subject)
    ())

  (define-handler (teapot tick) (ev dt tt)
    (incf (vz (rotation teapot)) dt)
    (setf (vx (rotation teapot)) (/ PI -2)))

  (remove-class-shader :fragment-shader 'teapot)
  (enter-instance 'teapot :vertex-array (asset 'els 'teapot))
  (editor "els.lisp" :start 104 :end 109 :language :lisp :trim 2 :margin (vec 0 20)))

(define-slide teapot-shader
  (h "A Simple Fragment Shader")
  
  (define-shader-subject teapot (slide-subject)
    ())

  (define-class-shader (teapot :fragment-shader)
    "out vec4 color;

  void main() {
    vec4 p = gl_FragCoord;
    color.gb *= clamp(abs(pow(tan((p.y+p.x)/50)+1, 20)), 0, 1);
    color.rgb *= -p.z/2+0.5;
  }")

  (enter-instance 'teapot :vertex-array (asset 'els 'teapot))
  (editor "els.lisp" :start 118 :end 128 :language :lisp :trim 2 :margin (vec 0 20)))

(define-slide teapot-mixins
  (remove-class-shader :fragment-shader 'teapot)
  
  (define-shader-subject teapot (slide-subject
  ;;                             textured-entity
  ;;                             colored-entity
                               )
    ())

  (enter-instance 'teapot :vertex-array (asset 'els 'teapot)
                          :texture (asset 'els 'wall)
                          :color (vec 0 0 1 1))
  (p "Lisp Source:" :size 30 :margin (vec 20 0 0 0))
  (editor "els.lisp" :start 136 :end 140 :trim 2 :size 22 :language :lisp :margin (vec 0 20))
  
  (p "Fragment Shader:" :size 30 :margin (vec 20 0 0 0))
  (c (getf (effective-shaders 'teapot) :fragment-shader) :size 22 :language :glsl))

(define-slide problems
  (h "Great, What's the Catch?")
  (items
   "Some shaders not automatically mergeable"
   "Currently very primitive strategy"
   "How effects are combined can be unintuitive"
   "Shaders might need to be adapted"))

(define-slide future
  (h "Future Ideas")
  (items
   "Verify correctness of GLSL code"
   "Optimisation passes"
   "Offer automatic correction for problems"
   "More exploration of composition strategies"))