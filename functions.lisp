(in-package #:org.shirakumo.beamer)

(defparameter *header-font* "Concourse C6")
(defparameter *body-font* "Concourse T3")
(defparameter *code-font* "Triplicate T4")

(defclass slide-text (alloy:label*)
  ((size :initarg :size :initform (alloy:un 30) :accessor size)
   (halign :initarg :halign :initform :left :accessor halign)))

(presentations:define-realization (alloy:ui slide-text)
  ((:label simple:text)
   (alloy:margins 0 5) alloy:text
   :wrap T
   :halign (halign alloy:renderable)
   :pattern colors:white
   :font *body-font*
   :size (size alloy:renderable)))

(presentations:define-update (alloy:ui slide-text)
  (:label
   :pattern colors:white))

(defclass header (slide-text)
  ((size :initform (alloy:un 48))))

(presentations:define-update (alloy:ui header)
  (:label
   :font (simple:request-font alloy:renderer *header-font*)))

(defclass paragraph (slide-text) ())

(defclass code (slide-text)
  ((color :initarg :color :initform colors:white :accessor color)
   (markup :initarg :markup :initform () :accessor markup)))

(presentations:define-update (alloy:ui code)
  (:label
   :pattern (color alloy:renderable)
   :markup (markup alloy:renderable)
   :font (simple:request-font alloy:renderer *code-font*)))

(defun h (text &rest initargs)
  (apply #'enter-instance 'header :value (princ-to-string text) initargs))

(defun p (text &rest initargs)
  (apply #'enter-instance 'paragraph :value (princ-to-string text) initargs))

(defun c (text &rest initargs &key (language *default-language*) (theme *default-theme*) &allow-other-keys)
  (remf initargs :language)
  (remf initargs :theme)
  (multiple-value-bind (base regions) (determine-regions text :language language :theme theme)
    (apply #'enter-instance 'code :value text :color base :markup regions initargs)))

(defclass items (alloy:grid-layout)
  ()
  (:default-initargs :col-sizes '(60 T) :row-sizes '(70)
                     :cell-margins (alloy:margins 5)))

(defclass bullet-point (alloy:layout-element alloy:renderable)
  ())

(presentations:define-realization (alloy:ui bullet-point)
  ((bullet simple:ellipse)
   (alloy:extent (alloy:u- (alloy:pw 0.5) 10) (alloy:u- (alloy:ph 0.5) 10) 10 10)
   :pattern colors:white))

(defmacro items (&body body)
  (form-fiddle:with-body-options (entries opts) body
    `(let ((*layout-parent* (enter-instance 'items ,@opts)))
       ,@(loop for el in entries
               collect `(enter-instance 'bullet-point)
               collect `(let ((*layout-parent* (enter-instance 'alloy:vertical-linear-layout :cell-margins (alloy:margins))))
                          ,(if (stringp el)
                               `(p ,el)
                               el))))))

(defclass image (alloy:direct-value-component alloy:icon)
  ((padding :initarg :padding :initform (alloy:margins 10) :accessor padding)))

(presentations:define-realization (alloy:ui image)
  ((:icon simple:icon)
   (padding alloy:renderable)
   alloy:value
   :sizing :contain))

(defun coerce-size (a b)
  (alloy:size (case a
                ((* T) (alloy:vw 1))
                (T a))
              (case b
                ((* T) (alloy:vh 1))
                (T b))))

(defun image (file size &rest initargs)
  (apply #'enter-instance 'image :value (slide-file file) :sizing-strategy (make-instance 'alloy:fixed-size :fixed-size (apply #'coerce-size size))
         initargs))

(defmacro arrange (&body body)
  (form-fiddle:with-body-options (entries opts) body
    `(let ((*layout-parent* (enter-instance 'alloy:grid-bag-layout ,@opts :growth-policy :vertical)))
       ,@(loop for el in entries
               collect (if (stringp el)
                           `(p ,el)
                           el)))))

(defmacro on-show (&body body)
  `(push (lambda () ,@body) (on-show-functions *slide*)))

(defun note (format &rest format-args)
  (push (lambda () (format T "~&    NOTE: ~?~%" format format-args))
        (on-show-functions *slide*)))

(defun show-time (minutes)
  (when +main+
    (setf (max-time +main+) (* 60 minutes))))
