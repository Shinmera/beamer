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
   (constructor :initarg :constructor :accessor constructor)
   (selection-buffer :initform NIL :accessor selection-buffer)))

(defmethod initialize-instance :after ((slide slide) &key)
  (setf (selection-buffer slide) (make-instance 'selection-buffer :scene slide :width 800 :height 600))
  (enter (make-instance 'slide-camera :name :camera) slide)
  (enter (make-instance 'render-pass) slide))

(defmethod print-object ((slide slide) stream)
  (print-unreadable-object (slide stream :type T)
    (format stream "~a/~a" (name (slide-show slide)) (name slide))))

(defun enter-instance (class &rest initargs)
  (enter (apply #'make-instance class initargs)
         (or *slide* (error "Not in a slide!"))))

(defmethod setup-scene ((show slide-show) (slide slide))
  (let ((*package* (find-package (name (slide-show slide))))
        (*slide* slide))
    (setf (width slide) (width *context*))
    (setf (height slide) (height *context*))
    (for:for ((item over slide))
      (unless (or (eq item (selection-buffer slide))
                  (typep item 'render-pass)
                  (typep item 'slide-camera))
        (leave item slide)))
    (funcall (constructor slide))
    (child-changed slide T)
    slide))

(defmethod reconstruct-slide ((slide slide))
  (make-instance 'slide :name (name slide)
                        :slide-show (slide-show slide)
                        :constructor (constructor slide)))

(defmethod resize :after ((slide slide) width height)
  (let ((ratio (/ 600 height)))
    (setf (slot-value slide 'width) (* ratio width))
    (setf (slot-value slide 'height) (* ratio height))
    (setf (zoom (unit :camera slide)) (/ ratio))
    (child-changed (pane slide) slide)))

(defmacro define-slide (name &body body)
  (let ((constructor (gensym "SCENE-CONSTRUCTOR")))
    (form-fiddle:with-body-options (body opts) body
      `(flet ((,constructor ()
                ,@body))
         (setf (slide ',name) (make-instance 'slide :name ',name
                                                    :slide-show (current-show)
                                                    :constructor #',constructor
                                                    ,@opts))))))

(defun slide-file (path &optional (slide *slide*))
  (merge-pathnames path (make-pathname :name NIL :type NIL :defaults (source (slide-show slide)))))
