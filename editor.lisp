(in-package #:org.shirakumo.beamer)

(defclass editor (alloy:direct-value-component alloy:input-box)
  ((alloy:value :initform "")
   (file :initarg :file :accessor file)
   (start :initarg :start :initform NIL :accessor start)
   (end :initarg :end :initform NIL :accessor end)
   (trim :initarg :trim :initform 0 :accessor trim)
   (language :initarg :language :initform *default-language* :accessor language)
   (theme :initarg :theme :initform *default-theme* :accessor theme)
   (color :initform colors:white :accessor color)
   (markup :initform () :accessor markup)
   (size :initarg :size :initform 18 :accessor size)))

(defmethod initialize-instance :after ((editor editor) &key file)
  (when file (load-text editor)))

(presentations:define-realization (alloy:ui editor)
  ((:label simple:text)
   (alloy:margins) alloy:text
   :wrap NIL
   :pattern (color alloy:renderable)
   :font *code-font*
   :size (alloy:px (size alloy:renderable))
   :markup (markup alloy:renderable)
   :valign :top)
  ((:cursor simple:cursor)
   (presentations:find-shape :label alloy:renderable)
   0
   :composite-mode :source-over
   :pattern colors:black)
  ((:selection simple:selection)
   (presentations:find-shape :label alloy:renderable)
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
   :pattern (color alloy:renderable)
   :markup (markup alloy:renderable)))

(defmethod (setf file) :after (file (editor editor))
  (load-text editor))

(defmethod (setf start) :after (start (editor editor))
  (load-text editor))

(defmethod (setf end) :after (end (editor editor))
  (load-text editor))

(defmethod load-text ((editor editor))
  (with-open-file (s (file editor))
    (read-until (start editor) s)
    (let ((end (etypecase (end editor)
                 (null T)
                 (integer (- (1+ (end editor)) (or (start editor) 0)))
                 (string (end editor)))))
      (setf (alloy:value editor) (join-lines (split-lines s :trim (trim editor) :end end))))))

(defmethod save-text ((editor editor))
  (let ((full (with-output-to-string (o)
                (with-open-file (s (file editor))
                  (read-until (start editor) s (lambda (line) (write-line line o)))
                  (join-lines (split-lines (alloy:value editor)) :out o :indent (trim editor))
                  (read-until (etypecase (end editor)
                                (null T)
                                (integer (- (1+ (end editor)) (or (start editor) 0)))
                                (string (end editor)))
                              s)
                  (read-until T s (lambda (line) (write-line line o)))))))
    (with-open-file (s (file editor) :direction :output :if-exists :supersede)
      (write-string full s))))

(defmethod (setf alloy:value) :after (text (editor editor))
  (multiple-value-bind (base regions) (determine-regions text :language (language editor) :theme (theme editor))
    (setf (color editor) base)
    (setf (markup editor) regions)))

(defmethod alloy:handle ((event alloy:key-down) (editor editor))
  (if (find :control (alloy:modifiers event))
      (case (key event)
        (:s (save-text editor))
        (:l (load-text editor))
        (:c (let ((*package* (find-package '#:org.shirakumo.beamer.user)))
              (cl:load (file editor))))
        (T
         (call-next-method)))
      (call-next-method)))

(defun editor (source &rest initargs)
  (apply #'enter-instance 'editor
         :file (slide-file source)
         initargs))
