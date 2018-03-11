#|
 This file is a part of beamer
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.beamer)

(defvar *package-slide-map* (make-hash-table :test 'eq))

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
   (slides :initform (make-array 0 :adjustable T :fill-pointer T) :reader slides)
   (index :initform 0 :accessor index)))

(defmethod initialize-instance :after ((show slide-show) &key slides)
  (dolist (slide slides)
    (load-slide show slide)))

(defmethod load-slide ((show slide-show) (source string))
  (load-slide show (merge-pathnames source (make-pathname :type "lisp" :defaults (base show)))))

(defmethod load-slide ((show slide-show) (source pathname))
  (let* ((name (pathname-name source))
         (*package* (or (find-package name)
                        (make-package name :use '("CL+TRIAL" "BEAMER"))))
         (slide (make-instance 'slide :slide-show show :source source)))
    (setf (gethash *package* *package-slide-map*) slide)
    (cl:load source)
    (enter (make-instance '2d-camera) slide)
    (enter (make-instance 'render-pass) slide)
    (print-container-tree slide)
    slide))

(defmethod current-slide ((show slide-show))
  (aref (slides show) (index show)))

(defmethod advance-slide ((show slide-show) by)
  (setf (index show) (max 0 (min (+ (index show) by) (1- (length (slides show))))))
  (current-slide show))

(defmethod next-slide ((show slide-show))
  (advance-slide show 1))

(defmethod prev-slide ((show slide-show))
  (advance-slide show -1))

(defclass slide (pipelined-scene)
  ((slide-show :initarg :slide-show :reader slide-show)
   (source :initarg :source :accessor source)))

(defmethod initialize-instance :after ((slide slide) &key slide-show)
  (vector-push-extend slide (slides slide-show)))

(defmethod print-object ((slide slide) stream)
  (print-unreadable-object (slide stream :type T)
    (format stream "~a" (name slide))))

(defun enter-instance (class &rest initargs)
  (enter (apply #'make-instance class initargs)
         (or (gethash *package* *package-slide-map*)
             (error "Not in a slide package: ~a" *package*))))
