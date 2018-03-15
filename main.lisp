#|
 This file is a part of beamer
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.beamer)

(defclass beamer (main fullscreenable)
  ((slide-show :initform NIL :accessor slide-show)
   (scene :initform NIL)
   (controller :initform (make-instance 'beamer-controller)))
  (:default-initargs
   :clear-color (vec 1 1 1)))

(defmethod initialize-instance ((beamer beamer) &key slide-show)
  (call-next-method)
  (setf (slide-show beamer)
        (etypecase slide-show
          (slide-show slide-show)
          ((or pathname string) (load-slide-show slide-show))))
  (setf (scene beamer) (current-slide (slide-show beamer))))

(defmethod finalize :before ((beamer beamer))
  (finalize (slide-show beamer)))

(define-action slideshow ())

(define-action next (slideshow)
  (key-release (one-of key :right :n :space :enter :return :pgdn :page-down))
  (mouse-release (one-of button :left))
  (gamepad-release (one-of button :dpad-r :r1 :r2)))

(define-action prev (slideshow)
  (key-release (one-of key :left :p :backspace :pgup :page-up))
  (gamepad-release (one-of button :dpad-l :l1 :l2)))

(define-action reload (slideshow)
  (key-release (one-of key :f5 :r))
  (gamepad-release (one-of button :select)))

(define-action exit (slideshow)
  (key-release (one-of key :esc :escape))
  (gamepad-release (one-of button :home)))

(defmethod setup-scene ((beamer beamer) scene))

(defun change-scene (display new)
  (transition (scene display) (setup-scene display new))
  (setf (scene display) new))

(define-subject beamer-controller (controller)
  ())

(define-handler (beamer-controller next) (ev)
  (change-scene (display beamer-controller) (next-slide (slide-show (display beamer-controller)))))

(define-handler (beamer-controller prev) (ev)
  (change-scene (display beamer-controller) (prev-slide (slide-show (display beamer-controller)))))

(define-handler (beamer-controller reload) (ev)
  (change-scene (display beamer-controller) (reconstruct-slide (current-slide (slide-show (display beamer-controller))))))

(define-handler (beamer-controller exit) (ev)
  (quit *context*))

(defun start-slideshow (path)
  (launch 'beamer :slide-show path))

(defun toplevel ()
  (let ((path (first (uiop:command-line-arguments))))
    (if path
        (start-slideshow path)
        (error "Please pass a path to a slide show directory."))))
