(in-package #:org.shirakumo.beamer)

(defvar *slide*)

(defclass slide-show (main)
  ((name :initform NIL :accessor name)
   (scene :initform NIL)
   (clock :initform 0.0 :accessor clock)
   (source :initarg :source :accessor source)
   (slides :initform (make-array 0 :adjustable T :fill-pointer T) :accessor slides)
   (index :initarg :index :initform 0 :accessor index)
   (max-time :initarg :max-time :initform NIL :accessor max-time)))

(defmethod initialize-instance ((show slide-show) &key slides source)
  (call-next-method)
  (let* ((*package* (find-package '#:org.shirakumo.beamer.user)))
    (setf (name show) (intern (pathname-name (source show))))
    (when source
      (cl:load source))
    (dolist (slide slides)
      (vector-push-extend slide (slides show)))
    (setf (scene show) (or (current-slide show)
                           (error "There are no slides defined.")))
    (format T "~&Slide sequence:~%")
    (loop for slide across (slides show)
          for i from 0
          do (format T "~& ~2d: ~a~%" i (name slide)))))

(defmethod print-object ((show slide-show) stream)
  (print-unreadable-object (show stream :type T)
    (format stream "~a" (name show))))

(defmethod finalize :before ((show slide-show))
  (map NIL #'finalize (slides show)))

(defmethod setup-scene ((show slide-show) scene))

(defmethod change-scene :before ((show slide-show) scene &key old)
  (declare (ignore old))
  (incf (clock show) (clock (scene show)))
  (format T "~&~% ~2d/~2d (~3d%)   ~a~@[ -~a~] (+~a) ~a~%"
          (1+ (index show)) (length (slides show))
          (round (/ (1+ (index show)) (length (slides show)) 0.01))
          (format-clock (clock show))
          (when (max-time show) (format-clock (- (max-time show) (clock show))))
          (format-clock (clock (scene show)))
          (name (scene show))))

(defmethod (setf index) :before (value (show slide-show))
  (unless (= value (index show))
    (change-scene show (aref (slides show) value))))

(defmethod current-slide ((show slide-show))
  (when (< (index show) (length (slides show)))
    (aref (slides show) (index show))))

(defmethod advance-slide ((show slide-show) by)
  (setf (index show) (max 0 (min (+ (index show) by) (1- (length (slides show))))))
  (current-slide show))

(defmethod at-end-p ((show slide-show))
  (<= (length (slides show)) (1+ (index show))))

(defmethod first-slide ((show slide-show))
  (setf (index show) 0)
  (current-slide show))

(defmethod last-slide ((show slide-show))
  (setf (index show) (1- (length (slides show))))
  (current-slide show))

(defmethod next-slide ((show slide-show))
  (advance-slide show 1))

(defmethod prev-slide ((show slide-show))
  (advance-slide show -1))

(defmethod slide ((index integer))
  (aref (slides +main+) index))

(defmethod slide ((name symbol))
  (find name (slides +main+) :key #'name))

(defmethod (setf slide) (slide (index integer))
  (when +main+
    (finalize (shiftf (aref (slides +main+) index) slide))
    (when (= (index +main+) index)
      (change-scene +main+ slide))
    slide))

(defmethod (setf slide) (slide (name symbol))
  (when +main+
    (let ((pos (position name (slides +main+) :key #'name)))
      (if pos
          (setf (slide pos +main+) slide)
          (vector-push-extend slide (slides +main+))))))

(defmethod (setf slide) ((null null) (index integer))
  (when +main+
    (finalize (array-utils:vector-pop-position (slides +main+) index))
    (when (= (index +main+) index)
      (setf (index +main+) (max 0 (1- (index +main+))))))
  null)

(defmethod (setf slide) ((null null) (name symbol))
  (when +main+
    (let ((pos (position name (slides +main+) :key #'name)))
      (when pos
        (setf (slide pos +main+) null)))))

(defun start-slideshow (path &key (index 0) (muffle-logging NIL))
  (load-keymap :package #.*package*)
  (if muffle-logging
      (let ((level (v:repl-level)))
        (setf (v:repl-level) :error)
        (unwind-protect
             (launch 'slide-show :source path :index index)
          (setf (v:repl-level) level)))
      (launch 'slide-show :source path :index index)))

(defun toplevel ()
  #+sbcl (sb-ext:disable-debugger)
  (handler-case
      (let ((path (first (uiop:command-line-arguments))))
        (if path
            (start-slideshow (merge-pathnames path (uiop:getcwd)) :muffle-logging T)
            (error "Please pass a path to a slide show file.")))
    (error (e)
      (format *error-output* "~&~a~%" e)
      (uiop:quit 1))))

(deploy:define-hook (:deploy beamer) ()
  (load-mapping (merge-pathnames "keymap.lisp" (data-root)) :package #.*package*)
  (push :deploy-console *features*))
