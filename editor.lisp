(in-package #:org.shirakumo.beamer)

(defclass editor (alloy:input-box)
  ((file :initarg :file :accessor file)
   (lines :initform NIL :accessor lines)
   (start :initarg :start :accessor start)
   (end :initarg :end :accessor end)
   (trim :initarg :trim :accessor trim)
   (language :initarg :language :accessor language)
   (theme :initarg :theme :accessor theme)
   (color :initform colors:white :accessor color)
   (markup :initform () :accessor markup))
  (:default-initargs :size 24
                     :start NIL
                     :end NIL
                     :trim 0
                     :language *default-language*
                     :theme *default-theme*))

(defmethod initialize-instance :after ((editor editor) &key file)
  (when file (load-text editor)))

(presentations:define-realization (alloy:ui editor)
  ((:label simple:text)
   (alloy:margins) alloy:text
   :wrap NIL
   :pattern (color editor)
   :font *code-font*
   :size (alloy:px 24)
   :markup (markup editor)
   :valign :top)
  ((:cursor simple:cursor)
   (find-shape :label alloy:renderable)
   0
   :composite-mode :source-over
   :pattern colors:black)
  ((:selection simple:selection)
   (find-shape :label alloy:renderable)
   0 0))

(presentations:define-update (alloy:ui editor)
  (:cursor
   :hidden-p (null alloy:focus)
   :start (alloy:pos (alloy:cursor alloy:renderable)))
  (:selection
   :hidden-p (null (alloy:anchor (alloy:cursor alloy:renderable)))
   :start (min (or (alloy:anchor (alloy:cursor alloy:renderable)) 0)
               (alloy:pos (alloy:cursor alloy:renderable)))
   :end (max (or (alloy:anchor (alloy:cursor alloy:renderable)) 0)
             (alloy:pos (alloy:cursor alloy:renderable))))
  (:label
   :text alloy:text
   :pattern (color editor)
   :markup (markup editor)))

(defmethod (setf file) :after (file (editor editor))
  (load-text editor))

(defmethod (setf start) :after (start (editor editor))
  (load-text editor))

(defmethod (setf end) :after (end (editor editor))
  (load-text editor))

(defmethod load-text ((editor editor))
  (with-open-file (s (file editor))
    (loop repeat (1- (or (start editor) 0))
          do (read-line s))
    (let ((count (if (end editor)
                     (- (1+ (end editor)) (or (start editor) 0))
                     most-positive-fixnum)))
      (setf (text editor) (join-lines (split-lines s :trim (trim editor) :count count))))))

(defmethod save-text ((editor editor))
  (let ((full (with-output-to-string (o)
                (with-open-file (s (file editor))
                  (loop repeat (1- (or (start editor) 0))
                        do (write-line (read-line s) o))
                  (join-lines (split-lines (text editor)) :out o :indent (trim editor))
                  (when (end editor)
                    (loop repeat (- (1+ (end editor)) (or (start editor) 0))
                          do (read-line s NIL))
                    (loop for line = (read-line s NIL)
                          while line
                          do (write-line line o)))))))
    (with-open-file (s (file editor) :direction :output :if-exists :supersede)
      (write-string full s))))

(defmethod (setf text) :after (text (editor editor))
  (multiple-value-bind (base regions) (determine-regions text :language (language editor) :theme (theme editor))
    (setf (color editor) base)
    (setf (markup editor) regions)))

(defmethod alloy:handle ((event alloy:key-down) (component editor))
  (if (find :control (modifiers event))
      (case (key event)
        (:s (save-text editor))
        (:l (load-text editor))
        (:c (let ((*package* (ensure-package (name (slide-show *scene*)))))
              (cl:load (file editor))))
        (T
         (call-next-method)))
      (call-next-method)))

(defun editor (source &rest initargs)
  (apply #'enter-instance 'editor
         :file (slide-file source)
         initargs))
