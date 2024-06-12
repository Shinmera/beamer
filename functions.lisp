(in-package #:org.shirakumo.beamer)

(defparameter *header-font* "Concourse C6")
(defparameter *body-font* "Concourse T3")
(defparameter *code-font* "Triplicate T4")

(defclass slide-text (alloy:label*)
  ((size :initarg :size :initform 30 :accessor size)
   (halign :initarg :halign :initform :left :accessor halign)))

(presentations:define-realization (alloy:ui slide-text)
  ((:label simple:text)
   (alloy:margins 0 10) alloy:text
   :wrap T
   :halign (halign alloy:renderable)
   :pattern colors:white
   :font *body-font*
   :size (alloy:px (size alloy:renderable))))

(presentations:define-update (alloy:ui slide-text)
  (:label
   :pattern colors:white))

(defclass header (slide-text)
  ((size :initform 48)))

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

(defclass items (alloy:vertical-linear-layout)
  ((alloy:cell-margins :initform (alloy:margins 50 2 2 2))))

(defmethod initialize-instance :after ((items items) &key entries)
  (dolist (entry entries)
    (alloy:enter (make-instance 'paragraph :value entry) items)))

(defun items (&rest body)
  (form-fiddle:with-body-options (entries opts) body
    (enter-instance 'items :entries entries)))

(defclass image (alloy:direct-value-component alloy:icon)
  ((padding :initarg :padding :initform (alloy:margins 10) :accessor padding)))

(presentations:define-realization (alloy:ui image)
  ((:icon simple:icon)
   (padding alloy:renderable)
   alloy:value
   :sizing :contain))

(defun image (file size &rest initargs)
  (apply #'enter-instance 'image :value (slide-file file) :sizing-strategy (make-instance 'alloy:fixed-size :fixed-size (alloy:size (first size) (second size)))
         initargs))

(defmacro on-show (&body body)
  `(push (lambda () ,@body) (on-show-functions *slide*)))

(defun note (format &rest format-args)
  (push (lambda () (format T "~&    NOTE: ~?~%" format format-args))
        (on-show-functions *slide*)))

(defun show-time (minutes)
  (when +main+
    (setf (max-time +main+) (* 60 minutes))))
