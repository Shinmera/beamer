(in-package #:org.shirakumo.beamer)

(defclass slide-panel (trial-alloy:menuing-panel) ())

(defmethod initialize-instance :after ((panel slide-panel) &key)
  (let ((layout (make-instance 'alloy:vertical-linear-layout))
        (focus (make-instance 'alloy:vertical-focus-list)))
    (alloy:finish-structure panel layout focus)))

(defclass slide (pipelined-scene)
  ((slide-show :initarg :slide-show :reader slide-show)
   (ui :initform (make-instance 'trial-alloy:base-ui) :accessor ui)
   (constructor :initarg :constructor :accessor constructor)
   (on-show-functions :initarg :on-show :initform () :accessor on-show-functions)))

(defmethod initialize-instance :after ((slide slide) &key)
  (let ((*package* (find-package (name (slide-show slide))))
        (*slide* slide))
    (funcall (constructor slide))
    (unless (node :camera slide)
      (enter (make-instance 'sidescroll-camera :name :camera) slide))
    (unless (nodes slide)
      (enter (make-instance 'render-pass) slide))))

(defmethod print-object ((slide slide) stream)
  (print-unreadable-object (slide stream :type T)
    (format stream "~a/~a" (name (slide-show slide)) (name slide))))

(defun enter-instance (instance/class &rest initargs)
  (alloy:enter (etypecase instance/class
                 ((or class symbol) (apply #'make-instance instance/class initargs))
                 (standard-object instance/class))
               (let ((ui (ui (or *slide* (error "Not in a slide!")))))
                 (unless (trial-alloy:find-panel 'slide-panel ui)
                   (trial:show (make-instance 'slide-panel) :ui ui)))))

(defmethod setup-scene ((show slide-show) (slide slide))
  (let ((*package* (find-package (name (slide-show slide))))
        (*slide* slide))
    (mapc #'funcall (on-show-functions slide)))
  (let ((output (car (nodes slide)))
        (ui (ui slide))
        (combine (make-instance 'blend-pass :name 'blend-pass)))
    (connect (port output 'color) (port combine 'a-pass) slide)
    (connect (port ui 'color) (port combine 'b-pass) slide))
  slide)

(defmethod reconstruct-slide ((slide slide))
  (make-instance 'slide :name (name slide)
                        :slide-show (slide-show slide)
                        :constructor (constructor slide)))

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
