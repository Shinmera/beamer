#|
 This file is a part of beamer
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.beamer)

(defvar *layout-update-in-progress* NIL)

(defgeneric child-changed (pane child))

(defclass ui-element (located-entity)
  ((pane :initform NIL :accessor pane)
   (width :initform 0 :accessor width)
   (height :initform 0 :accessor height)
   (margin :initform (vec 0 0 0 0) :accessor margin)))

(defmethod initialize-instance :after ((element ui-element) &key margin)
  (when margin (setf (margin element) margin)))

(defmethod child-changed ((null null) element))

(defmethod (setf height) :after (value (element ui-element))
  (child-changed (pane element) element))

(defmethod (setf width) :after (value (element ui-element))
  (child-changed (pane element) element))

(defmethod resize ((element ui-element) width height)
  (setf (width element) width)
  (setf (height element) height))

(defmethod (setf margin) :after ((margin number) (element ui-element))
  (setf (margin element) (vec margin margin margin margin)))

(defmethod (setf margin) :after ((margin vec2) (element ui-element))
  (setf (margin element) (vec (vx margin) (vy margin) (vx margin) (vy margin))))

(defmethod (setf margin) :after ((margin vec4) (element ui-element))
  (child-changed (pane element) element))

(defclass pane (ui-element container)
  ((layout :initarg :layout :accessor layout))
  (:default-initargs :layout (make-instance 'vertical-layout)))

(defmethod initialize-instance :after ((pane pane) &key)
  (setf (pane pane) pane))

(defmethod child-changed ((pane pane) child)
  (unless *layout-update-in-progress*
    (let ((*layout-update-in-progress* T))
      (update-layout (layout pane) pane))))

(defmethod enter :after ((element ui-element) (pane pane))
  (setf (pane element) pane)
  (child-changed pane element))

(defclass layout ()
  ((padding :initform (vec 10 10 10 10) :initarg :padding :accessor padding)))

(defmethod (setf padding) ((padding number) (layout layout))
  (setf (padding layout) (vec padding padding padding padding)))

(defmethod (setf padding) ((padding vec2) (layout layout))
  (setf (padding layout) (vec (vx padding) (vy padding) (vx padding) (vy padding))))

(defclass vertical-layout (layout)
  ())

(defmethod update-layout ((layout vertical-layout) container)
  (let* ((pad (padding layout))
         (y (- (height container) (vy pad))))
    (for:for ((element over container))
      (when (typep element 'ui-element)
        (let ((mar (margin element)))
          (setf (width element) (- (width container)
                                   (vx pad) (vz pad)
                                   (vx mar) (vz mar)))
          ;; GROSS
          (when (typep element 'pane)
            (update-layout (layout element) element))
          (setf (vx (location element)) (+ (vx pad) (vx mar)))
          (decf y (vy mar))
          (setf (vy (location element)) y)
          (decf y (or (height element) 0))
          (decf y (vw mar)))))))
