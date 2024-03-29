(in-package #:org.shirakumo.beamer)

(define-pool beamer
  :base :beamer)

(define-asset (beamer header) font
    #p"Concourse C6 Regular.ttf"
  :size 72 :oversample 2)

(define-asset (beamer text) font
    #p"Concourse T3 Regular.ttf"
  :size 48 :oversample 2)

(define-asset (beamer code) font
    #p"Triplicate T4 Code Regular.ttf"
  :size 48 :oversample 2)

(define-asset (beamer rect) mesh
    (make-rectangle 1 1 :align :bottomleft))

(define-shader-entity slide-text (text ui-element)
  ()
  (:default-initargs
   :color (vec 1 1 1 1)
   :wrap T
   :width 800))

(defmethod paint :around ((text slide-text) target)
  (with-pushed-matrix (model-matrix)
    (translate-by 0 (- (getf (text-extent text "") :t)) 0)
    (call-next-method)))

(define-shader-entity header (slide-text)
  ()
  (:default-initargs :font (asset 'beamer 'header)
                     :margin (vec4 20 5 0 20)))

(defun h (text &rest initargs)
  (apply #'enter-instance 'header :text (princ-to-string text) initargs))

(define-shader-entity paragraph (slide-text)
  ()
  (:default-initargs :font (asset 'beamer 'text)
                     :margin (vec2 0 24)))

(defun p (text &rest initargs)
  (apply #'enter-instance 'paragraph :text (princ-to-string text) initargs))

(define-shader-entity code (slide-text highlighted-text)
  ()
  (:default-initargs :font (asset 'beamer 'code)
                     :margin (vec2 0 24)
                     :wrap NIL))

(defun c (text &rest initargs &key (language *default-language*) (theme *default-theme*) &allow-other-keys)
  (remf initargs :language)
  (remf initargs :theme)
  (multiple-value-bind (base regions) (determine-regions text :language language :theme theme)
    (apply #'enter-instance 'code :text text :color base :color-regions regions initargs)))

(define-asset (beamer bullet) mesh
    (make-sphere 6))

(define-shader-entity bullet (vertex-entity colored-entity)
  ()
  (:default-initargs
   :vertex-array (asset 'beamer 'bullet)
   :color (vec4 1 1 1 1)))

(defclass items (pane)
  ((bullet :initarg :bullet :accessor bullet))
  (:default-initargs
   :layout (make-instance 'vertical-layout :padding (vec4 50 24 0 24))
   :bullet (make-instance 'bullet)))

(defmethod initialize-instance :after ((items items) &key entries)
  (dolist (entry entries)
    (enter (make-instance 'paragraph :text entry :margin (vec2 0 10)) items)))

(defmethod register-object-for-pass :after (pass (items items))
  (register-object-for-pass pass (bullet items)))

(defmethod child-changed :after ((items items) child)
  (setf (slot-value items 'height)
        (+ (vy (padding (layout items)))
           (for:for ((item over items)
                     (sum summing (+ (vy (margin item))
                                     (or (height item) 0)
                                     (vw (margin item))))))
           (vw (padding (layout items)))))
  (unless (eq items (pane items))
    (child-changed (pane items) items)))

(defmethod paint ((items items) target)
  (with-pushed-matrix (model-matrix)
    (translate-by 0 (- (height items)) 0)
    (for:for ((item over items))
      (when (bullet items)
        (with-pushed-matrix (model-matrix)
          (translate-by (/ (vx (padding (layout items))) 2)
                        (- (vy (location item)) (/ (size item) 2))
                        0)
          (paint (bullet items) target)))
      (paint item target))))

(defun items (&rest body)
  (form-fiddle:with-body-options (entries opts (bullet-points T)) body
    (if bullet-points
        (enter-instance 'items :entries entries)
        (enter-instance 'items :entries entries :bullet NIL))))

(define-shader-entity img (vertex-entity textured-entity ui-element)
  ()
  (:default-initargs
   :margin (vec 10 20)
   :vertex-array (asset 'beamer 'rect)
   :texture NIL))

(defmethod (setf width) :after (width (img img))
  (let* ((texture (texture img))
         (aspect (/ (or (height texture) 1)
                    (or (width texture) 1))))
    (setf (height img) (* aspect width))))

(defmethod paint ((img img) target)
  (with-pushed-matrix (model-matrix)
    (with-pushed-attribs
      (disable :cull-face)
      (translate-by 0 (- (height img)) 0)
      (scale-by (width img) (- (height img)) 1)
      (call-next-method))))

(defun image (file &rest initargs)
  (apply #'enter-instance 'img :texture (make-instance 'image :input (slide-file file) :pool NIL)
         initargs))

(defmacro on-show (&body body)
  `(push (lambda () ,@body) (on-show-functions *slide*)))

(defun note (format &rest format-args)
  (push (lambda () (format T "~&    NOTE: ~?~%" format format-args))
        (on-show-functions *slide*)))

(defun show-time (minutes)
  (setf (max-time (current-show)) (* 60 minutes)))
