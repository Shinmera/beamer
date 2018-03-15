#|
 This file is a part of beamer
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.beamer)

(defvar *slide-show-map* (make-hash-table :test 'eq))
(defvar *slide*)

(defun split-body-options (body)
  (values (loop for list = body then rest
                for (key val . rest) = list
                while (and (cdr list) (keywordp key))
                collect key collect val
                finally (setf body list))
          body))

(defun ensure-package (name)
  (or (find-package name)
      (make-package name :use '(#:org.shirakumo.beamer.user))))

(defun current-show (&optional (package *package*))
  (or (gethash (find-package package) *slide-show-map*)
      (error "~a is not a slide show." package)))

(defun (setf current-show) (show &optional (package *package*))
  (let ((package (find-package package)))
    (if show
        (setf (gethash package *slide-show-map*) show)
        (remhash package *slide-show-map*)))
  show)

(defun load-slide-show (path)
  (let* ((name (pathname-name path))
         (*package* (ensure-package name))
         (show (make-instance 'slide-show :source path)))
    (setf (current-show) show)
    (cl:load path)
    show))

(defclass slide-show ()
  ((source :initarg :source :accessor source)
   (slides :initform (make-array 0 :adjustable T :fill-pointer T) :accessor slides)
   (index :initform 0 :accessor index)))

(defmethod initialize-instance :after ((show slide-show) &key slides)
  (dolist (slide slides) (vector-push-extend slide (slides show))))

(defmethod print-object ((show slide-show) stream)
  (print-unreadable-object (show stream :type T)
    (format stream "~a" (name show))))

(defmethod finalize ((show slide-show))
  (remhash (find-package (name show)) *slide-show-map*)
  (delete-package (name show))
  (map NIL #'finalize (slides show)))

(defmethod name ((show slide-show))
  (pathname-name (source show)))

(defmethod current-slide ((show slide-show))
  (aref (slides show) (index show)))

(defmethod advance-slide ((show slide-show) by)
  (setf (index show) (max 0 (min (+ (index show) by) (1- (length (slides show))))))
  (current-slide show))

(defmethod next-slide ((show slide-show))
  (advance-slide show 1))

(defmethod prev-slide ((show slide-show))
  (advance-slide show -1))

(defmethod slide ((index integer) &optional (show (current-show)))
  (aref (slides show) index))

(defmethod slide ((name symbol) &optional (show (current-show)))
  (find name (slides show) :key #'name))

(defmethod (setf slide) (slide (index integer) &optional (show (current-show)))
  (finalize (shiftf (aref (slides show) index) slide))
  slide)

(defmethod (setf slide) (slide (name symbol) &optional (show (current-show)))
  (let ((pos (position name (slides show) :key #'name)))
    (if pos
        (setf (slide pos show) slide)
        (vector-push-extend slide (slides show)))))

(defmethod (setf slide) ((null null) (index integer) &optional (show (current-show)))
  (finalize (array-utils:vector-pop-position (slides show) index))
  null)

(defmethod (setf slide) ((null null) (name symbol) &optional (show (current-show)))
  (let ((pos (position name (slides show) :key #'name)))
    (when pos
      (setf (slide pos show) null))))

(defmacro define-slide (name &body body)
  (let ((constructor (gensym "SCENE-CONSTRUCTOR")))
    `(flet ((,constructor ()
              ,@body))
       (let ((*slide* (make-instance 'slide :name ',name
                                            :slide-show (current-show)
                                            :constructor #',constructor)))
         (,constructor)
         (setf (slide ',name) *slide*)))))

(defclass slide (pipelined-scene)
  ((slide-show :initarg :slide-show :reader slide-show)
   (constructor :initarg :constructor :accessor constructor)))

(defmethod initialize-instance :after ((slide slide) &key)
  (enter (make-instance '2d-camera :name :camera) slide)
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
