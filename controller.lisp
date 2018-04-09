#|
 This file is a part of beamer
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.beamer)

(define-action slideshow ())

(define-action next (slideshow)
  (key-release (one-of key :page-down))
  (gamepad-release (one-of button :r1)))

(define-action prev (slideshow)
  (key-release (one-of key :page-up))
  (gamepad-release (one-of button :l1)))

(define-action reload (slideshow)
  (key-release (one-of key :f5))
  (gamepad-release (one-of button :select)))

(define-action exit (slideshow)
  (key-release (or (one-of key :esc :escape)
                   (and (eql key :q) (find :control modifiers))))
  (gamepad-release (one-of button :home)))

(define-subject beamer-controller ()
  ((display :initform NIL :accessor display :accessor slide-show)
   (dragging :initform NIL :accessor dragging))
  (:default-initargs
   :name :controller))

(defmethod compute-resources ((controller beamer-controller) resources readying cache))

(define-handler (beamer-controller next) (ev)
  (next-slide (slide-show beamer-controller)))

(define-handler (beamer-controller prev) (ev)
  (prev-slide (slide-show beamer-controller)))

(define-handler (beamer-controller reload) (ev)
  (change-scene (slide-show beamer-controller)
                (current-slide (slide-show beamer-controller))))

(define-handler (beamer-controller exit) (ev)
  (quit *context*))

(define-handler (beamer-controller mapping T 100) (ev)
  (map-event ev *scene*)
  (retain-event ev))

(define-handler (beamer-controller mouse-press) (ev button pos)
  (when (eql button :left)
    (let ((buffer (selection-buffer *scene*)))
      (paint buffer buffer)
      (setf (dragging beamer-controller) (object-at-point pos buffer)))))

(define-handler (beamer-controller mouse-release) (ev button)
  (when (eql button :left)
    (setf (dragging beamer-controller) NIL)))

(define-handler (beamer-controller mouse-move) (ev pos)
  (when (dragging beamer-controller)
    (let ((ratio (zoom (unit :camera *scene*))))
      (setf (vx (location (dragging beamer-controller))) (/ (vx pos) ratio))
      (setf (vy (location (dragging beamer-controller))) (/ (vy pos) ratio)))))
