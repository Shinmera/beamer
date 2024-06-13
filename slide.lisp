(in-package #:org.shirakumo.beamer)

(defvar *layout-parent* NIL)
(defclass slide-panel (trial-alloy:menuing-panel) ())

(defmethod initialize-instance :after ((panel slide-panel) &key)
  (let ((layout (make-instance 'alloy:vertical-linear-layout :cell-margins (alloy:margins 30 5)))
        (focus (make-instance 'alloy:vertical-focus-list)))
    (alloy:finish-structure panel layout focus)))

(defmethod alloy:enter ((component alloy:layout-element) (panel slide-panel) &key)
  (when (typep component 'alloy:focus-element)
    (alloy:enter component (alloy:focus-element panel)))
  (alloy:enter component (alloy:layout-element panel))
  component)

(defclass slide (pipelined-scene)
  ((name :initarg :name :reader name)
   (clock :initform 0.0 :accessor clock)
   (slide-show :initarg :slide-show :initform +main+ :reader slide-show)
   (ui :initform (make-instance 'trial-alloy:base-ui :scales '((3840 T 3.0)
                                                               (2800 T 2.0)
                                                               (1920 T 1.5)
                                                               (1280 T 1.0)
                                                               (1000 T 0.8)
                                                               (T T 0.5))) :accessor ui)
   (constructor :initarg :constructor :accessor constructor)
   (on-show-functions :initarg :on-show :initform () :accessor on-show-functions)))

(defmethod print-object ((slide slide) stream)
  (print-unreadable-object (slide stream :type T)
    (format stream "~a/~a" (name (slide-show slide)) (name slide))))

(defun enter-instance (instance/class &rest initargs)
  (let ((slide (or *slide* (error "Not in a slide!")))
        (instance (etypecase instance/class
                    ((or class symbol) (apply #'make-instance instance/class initargs))
                    (standard-object instance/class))))
    (etypecase instance
      (trial:entity
       (enter instance slide))
      (alloy:layout-element
       (alloy:enter instance
                    (or *layout-parent*
                        (trial-alloy:find-panel 'slide-panel (ui slide))
                        (trial:show (make-instance 'slide-panel) :ui (ui slide))))))
    instance))

(defmethod setup-scene ((show slide-show) (slide slide))
  (setf (active-p (action-set 'trial-alloy:ui-actions)) T)
  (when (= (length slide) 0)
    (let ((*package* (find-package '#:org.shirakumo.beamer.user))
          (*slide* slide))
      (funcall (constructor slide))
      (unless (node :camera slide)
        (enter (make-instance 'sidescroll-camera :name :camera) slide))
      (unless (nodes slide)
        (enter (make-instance 'render-pass) slide))
      (unless (node 'blend-pass slide)
        (let ((output (or (car (nodes slide)) (aref (passes slide) (1- (length (passes slide))))))
              (ui (ui slide))
              (combine (make-instance 'blend-pass :name 'blend-pass)))
          (setf (clear-color output) (vec 20/255 25/255 28/255 0))
          (connect (port output 'color) (port combine 'a-pass) slide)
          (connect (port ui 'color) (port combine 'b-pass) slide)))))
  (let ((*package* (find-package '#:org.shirakumo.beamer.user))
        (*slide* slide))
    (mapc #'funcall (on-show-functions slide)))
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
                                                    :constructor #',constructor
                                                    ,@opts))))))

(defun slide-file (path &optional (slide *slide*))
  (merge-pathnames path (make-pathname :name NIL :type NIL :defaults (source (slide-show slide)))))

(define-action start (trial-alloy:ui-actions))
(define-action end (trial-alloy:ui-actions))
(define-action next (trial-alloy:ui-actions))
(define-action prev (trial-alloy:ui-actions))
(define-action reload (trial-alloy:ui-actions))
(define-action exit (trial-alloy:ui-actions))

(define-handler (slide start) ()
  (first-slide +main+))

(define-handler (slide end) ()
  (last-slide +main+))

(define-handler (slide next) ()
  (next-slide +main+))

(define-handler (slide prev) ()
  (prev-slide +main+))

(define-handler (slide reload) ()
  (change-scene +main+ (current-slide +main+)))

(define-handler (slide exit) ()
  (quit *context*))
