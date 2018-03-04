#|
 This file is a part of slide-beam
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.slide-beam)

(defvar *slide*)

(defun split-body-options (body)
  (values (loop for list = body then rest
                for (key val . rest) = list
                while (and (cdr list) (keywordp key))
                collect key collect val
                finally (setf body list))
          body))

(defun load-slide-show (path)
  (let* ((path (merge-pathnames path "slideshow.lisp"))
         (source (with-open-file (stream path)
                   (loop with eof = (make-symbol "EOF")
                         for expr = (read stream NIL eof)
                         until (eq expr eof)
                         collect expr))))
    (multiple-value-bind (kargs slides) (split-body-options source)
      (apply #'make-instance 'slide-show
             :base (uiop:pathname-directory-pathname path)
             :slides slides
             kargs))))

(defclass slide-show ()
  ((base :initarg :base :accessor base)
   (slide-package :initarg :package :accessor slide-package)
   (slides :initform (make-array 0 :adjustable T :fill-pointer T) :reader slides)
   (index :initform 0 :accessor index))
  (:default-initargs
   :package '#:slide-show))

(defmethod initialize-instance :after ((show slide-show) &key package slides use)
  (etypecase package
    (package)
    ((or symbol string)
     (setf (slide-package show)
           (or (find-package package)
               (make-package package :use (list* '#:org.shirakumo.slide-beam.user
                                                 use))))))
  (dolist (slide slides)
    (load-slide show source)))

(defmethod load-slide ((show slide-show) (source string))
  (load-slide show (merge-pathnames source (make-pathname :type "lisp" :defaults (base show)))))

(defmethod load-slide ((show slide-show) (source pathname))
  (let ((*package* (slide-package show))
        (*slide* (make-instance 'slide :slide-show show
                                       :source source
                                       :name (pathname-name source))))
    (load source)))

(defmethod current-slide ((show slide-show))
  (aref (slides show) (index show)))

(defmethod advance-slide ((show slide-show) by)
  (setf (index show) (max 0 (min (+ (index show) by) (length (slides show)))))
  (current-slide show))

(defmethod next-slide ((show slide-show))
  (advance-slide show 1))

(defmethod prev-slide ((show slide-show))
  (advance-slide show -1))

(defclass slide (scene)
  ((slide-show :initarg :slide-show :reader slide-show)
   (source :initarg :source :accessor source)
   (name :initarg :name :accessor name)))

(defmethod initialize-instance :after ((slide slide) &key slide-show)
  (vector-push-extend slide (slides slide-show)))

(defun enter-instance (class &rest initargs)
  (enter (apply #'make-instance class initargs) *slide*))

(defmacro define-slide-function (name lambda-list &body body)
  (let ((name (intern (string name) '#:org.shirakumo.slide-beam.user)))
    `(progn
       (export ,name '#:org.shirakumo.slide-beam.user)
       (defun ,name ,labmda-list
         ,@body))))
