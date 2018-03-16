#|
 This file is a part of beamer
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.beamer)

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
  (key-release (or (one-of key :esc :escape)
                   (and (eql key :q) (find :control modifiers))))
  (gamepad-release (one-of button :home)))

(define-subject beamer-controller ()
  ((display :initform NIL :accessor display :accessor slide-show))
  (:default-initargs
   :name :controller))

(defmethod compute-resources ((controller beamer-controller) resources readying cache))

(define-handler (beamer-controller next) (ev)
  (next-slide (slide-show beamer-controller)))

(define-handler (beamer-controller prev) (ev)
  (prev-slide (slide-show beamer-controller)))

(define-handler (beamer-controller reload) (ev)
  (change-scene (slide-show beamer-controller)
                (reconstruct-slide (current-slide (slide-show beamer-controller)))))

(define-handler (beamer-controller exit) (ev)
  (quit *context*))

(define-handler (beamer-controller mapping T 100) (ev)
  (map-event ev *scene*)
  (retain-event ev))
