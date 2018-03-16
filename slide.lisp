#|
 This file is a part of beamer
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.beamer)

(define-subject slide-camera (2d-camera)
  ((zoom :initarg :zoom :accessor zoom))
  (:default-initargs :zoom 1.0))

(defmethod project-view ((camera slide-camera) ev)
  (let ((z (zoom camera)))
    (reset-matrix *view-matrix*)
    (scale-by z z z *view-matrix*)
    (translate (v- (location camera)) *view-matrix*)))

(defclass slide (pipelined-scene pane)
  ((slide-show :initarg :slide-show :reader slide-show)
   (constructor :initarg :constructor :accessor constructor)))

(defmethod initialize-instance :after ((slide slide) &key)
  (enter (make-instance 'slide-camera :name :camera) slide)
  (enter (make-instance 'render-pass) slide))

(defmethod print-object ((slide slide) stream)
  (print-unreadable-object (slide stream :type T)
    (format stream "~a/~a" (name (slide-show slide)) (name slide))))

(defun enter-instance (class &rest initargs)
  (enter (apply #'make-instance class initargs)
         (or *slide* (error "Not in a slide!"))))

(defmethod reconstruct-slide ((slide slide))
  (let ((*package* (find-package (name (slide-show slide))))
        (*slide* (make-instance 'slide :name (name slide)
                                       :slide-show (slide-show slide)
                                       :constructor (constructor slide))))
    (funcall (constructor slide))
    *slide*))

(defmethod resize :after ((slide slide) width height)
  (let ((ratio (/ 600 height)))
    (setf (slot-value slide 'width) (* ratio width))
    (setf (slot-value slide 'height) (* ratio height))
    (setf (zoom (unit :camera slide)) (/ ratio))
    (child-changed (pane slide) slide)))

(defmacro define-slide (name &body body)
  (let ((constructor (gensym "SCENE-CONSTRUCTOR")))
    `(flet ((,constructor ()
              ,@body))
       (let ((*slide* (make-instance 'slide :name ',name
                                            :slide-show (current-show)
                                            :constructor #',constructor)))
         (,constructor)
         (setf (slide ',name) *slide*)))))
