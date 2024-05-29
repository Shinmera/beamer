(in-package #:org.shirakumo.beamer)

(defvar *header-font* "Concourse C6 Regular")
(defvar *body-font* "Concourse T3 Regular")
(defvar *code-font* "Triplicate T4 Code Regular")

(defclass slide-text (alloy:label*) ())

(presentations:define-realization (alloy:ui slide-text)
  ((:label simple:text)
   (alloy:margins) alloy:text
   :wrap T
   :pattern colors:white
   :font *body-font*
   :size (alloy:px 48)))

(defclass header (slide-text) ())

(presentations:define-update (alloy:ui slide-text)
  (:label
   :font *header-font*
   :size (alloy:px 72)))

(defclass paragraph (slide-text) ())

(defclass code (slide-text highlighted-text) ())

(presentations:define-update (alloy:ui slide-text)
  (:label
   :font *code-font*))

(defun h (text &rest initargs)
  (apply #'enter-instance 'header :value (princ-to-string text) initargs))

(defun p (text &rest initargs)
  (apply #'enter-instance 'paragraph :value (princ-to-string text) initargs))

(defun c (text &rest initargs &key (language *default-language*) (theme *default-theme*) &allow-other-keys)
  (remf initargs :language)
  (remf initargs :theme)
  (multiple-value-bind (base regions) (determine-regions text :language language :theme theme)
    (apply #'enter-instance 'code :text text :color base :color-regions regions initargs)))

(defclass items (alloy:vertical-linear-layout)
  ((alloy:cell-margins :initform (alloy:margins 2 2 2 10))))

(defmethod initialize-instance :after ((items items) &key entries)
  (dolist (entry entries)
    (alloy:enter (make-instance 'paragraph :value entry) items)))

(defun items (&rest body)
  (form-fiddle:with-body-options (entries opts) body
    (enter-instance 'items :entries entries)))

(defclass image (alloy:direct-value-component alloy:icon)
  ())

(presentations:define-update (alloy:ui image)
  (:icon
   :image alloy:value
   :sizing :contain))

(defun image (file &rest initargs)
  (apply #'enter-instance 'image :texture (make-instance 'image :input (slide-file file))
         initargs))

(defmacro on-show (&body body)
  `(push (lambda () ,@body) (on-show-functions *slide*)))

(defun note (format &rest format-args)
  (push (lambda () (format T "~&    NOTE: ~?~%" format format-args))
        (on-show-functions *slide*)))

(defun show-time (minutes)
  (setf (max-time (current-show)) (* 60 minutes)))
