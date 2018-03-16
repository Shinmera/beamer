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

(defclass slide-show (main)
  ((scene :initform NIL)
   (controller :initform (make-instance 'beamer-controller))
   (source :initarg :source :accessor source)
   (slides :initform (make-array 0 :adjustable T :fill-pointer T) :accessor slides)
   (index :initform -1 :accessor index))
  (:default-initargs
   :clear-color (vec 1 1 1)))

(defmethod initialize-instance ((show slide-show) &key slides source)
  (call-next-method)
  (let* ((name (pathname-name source))
         (*package* (ensure-package name)))
    (setf (name show) (intern (pathname-name (source show))))
    (setf (current-show) show)
    (when source
      (cl:load source))
    (dolist (slide slides)
      (vector-push-extend slide (slides show)))
    (setf (slot-value show 'index) 0)
    (setf (scene show) (current-slide show))))

(defmethod print-object ((show slide-show) stream)
  (print-unreadable-object (show stream :type T)
    (format stream "~a" (name show))))

(defmethod finalize :before ((show slide-show))
  (remhash (find-package (name show)) *slide-show-map*)
  (delete-package (name show))
  (map NIL #'finalize (slides show)))

(defmethod setup-scene ((show slide-show) scene))

(defmethod (setf index) :after (value (show slide-show))
  (change-scene show (aref (slides show) value)))

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
  (when (= (index show) index)
    (change-scene show slide))
  slide)

(defmethod (setf slide) (slide (name symbol) &optional (show (current-show)))
  (let ((pos (position name (slides show) :key #'name)))
    (if pos
        (setf (slide pos show) slide)
        (vector-push-extend slide (slides show)))))

(defmethod (setf slide) ((null null) (index integer) &optional (show (current-show)))
  (finalize (array-utils:vector-pop-position (slides show) index))
  (when (= (index show) index)
    (setf (index show) (max 0 (1- (index show)))))
  null)

(defmethod (setf slide) ((null null) (name symbol) &optional (show (current-show)))
  (let ((pos (position name (slides show) :key #'name)))
    (when pos
      (setf (slide pos show) null))))

(defun start-slideshow (path)
  (launch 'slide-show :source path))

(defun toplevel ()
  (let ((path (first (uiop:command-line-arguments))))
    (if path
        (start-slideshow path)
        (error "Please pass a path to a slide show directory."))))
