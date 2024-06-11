(in-package #:org.shirakumo.beamer)

(defvar *slide-show-map* (make-hash-table :test 'eq))
(defvar *slide*)

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
  ((name :initform NIL :accessor name)
   (scene :initform NIL)
   (clock :initform 0.0 :accessor clock)
   (source :initarg :source :accessor source)
   (slides :initform (make-array 0 :adjustable T :fill-pointer T) :accessor slides)
   (index :initarg :index :initform 0 :accessor index)
   (max-time :initarg :max-time :initform NIL :accessor max-time)))

(defmethod initialize-instance ((show slide-show) &key slides source)
  (call-next-method)
  (let* ((name (pathname-name source))
         (*package* (find-package '#:org.shirakumo.beamer.user)))
    (setf (name show) (intern (pathname-name (source show))))
    (setf (current-show) show)
    (when source
      (cl:load source))
    (dolist (slide slides)
      (vector-push-extend slide (slides show)))
    (setf (scene show) (or (current-slide show)
                           (error "There are no slides defined.")))))

(defmethod print-object ((show slide-show) stream)
  (print-unreadable-object (show stream :type T)
    (format stream "~a" (name show))))

(defmethod finalize :before ((show slide-show))
  (remhash (find-package (name show)) *slide-show-map*)
  (map NIL #'finalize (slides show)))

(defmethod setup-scene ((show slide-show) scene))

(defmethod change-scene :before ((show slide-show) scene &key old)
  (declare (ignore old))
  (incf (clock show) (clock (scene show)))
  (format T "~&~% ~2d/~2d (~3d%)   ~a~@[ -~a~] (+~a)~%"
          (1+ (index show)) (length (slides show))
          (round (/ (1+ (index show)) (length (slides show)) 0.01))
          (format-clock (clock show))
          (when (max-time show) (format-clock (- (max-time show) (clock show))))
          (format-clock (clock (scene show)))))

(defmethod (setf index) :before (value (show slide-show))
  (change-scene show (aref (slides show) value)))

(defmethod current-slide ((show slide-show))
  (when (< (index show) (length (slides show)))
    (aref (slides show) (index show))))

(defmethod advance-slide ((show slide-show) by)
  (setf (index show) (max 0 (min (+ (index show) by) (1- (length (slides show))))))
  (current-slide show))

(defmethod at-end-p ((show slide-show))
  (<= (length (slides show)) (1+ (index show))))

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
