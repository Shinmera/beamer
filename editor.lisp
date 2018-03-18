#|
 This file is a part of beamer
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.beamer)

(define-shader-entity cursor (vertex-entity)
  ((editor :initarg :editor :accessor editor)
   (pos :initform 0 :accessor pos)
   (cursor-size :initform (vec4 0 0 0.5 1) :reader cursor-size))
  (:default-initargs
   :editor (error "EDITOR required.")
   :vertex-array (asset 'trial 'fullscreen-square)))

(defmethod initialize-instance :after ((cursor cursor) &key editor)
  (let* ((extent (extent editor))
         (u (getf extent :t))
         (b (getf extent :b)))
    (setf (vy (cursor-size cursor)) 0)
    (setf (vz (cursor-size cursor)) (/ (size editor)
                                       (size (font editor))
                                       2))
    (setf (vw (cursor-size cursor)) (/ (- u b) 2))))

(defmethod (setf pos) :around (value (cursor cursor))
  (call-next-method (min (length (text (editor cursor))) (max 0 value)) cursor))

(defmethod (setf pos) :after (value (cursor cursor))
  (let ((rs 0) (r 0))
    (dotimes (i value)
      (when (eql #\Linefeed (aref text i))
        (setf rs i) (incf r)))
    (setf (vy (cursor-size cursor)) (* r (+ (getf extent :t) (getf extent :gap))))
    (setf (vx (cursor-size cursor))
          (getf (text-extent (editor cursor) (subseq (text (editor cursor)) rs (pos cursor)))
                :r))))

(defmethod paint :before ((cursor cursor) target)
  (let ((size (cursor-size cursor)))
    (translate-by (vx size) (+ (vy size) (vw size)) 0)
    (scale-by (vz size) (vw size) 1)))

(define-class-shader (cursor :fragment-shader)
  "out vec4 color;

void main(){
    color = vec4(0,0,0,1);
}")

(define-shader-subject editor (slide-text)
  ((file :initarg :file :accessor file)
   (start :initarg :start :accessor start)
   (end :initarg :end :accessor end)
   (cursor :initform NIL :accessor cursor)))

(defmethod initialize-instance :after ((editor editor) &key)
  (setf (cursor editor) (make-instance 'cursor :editor editor))
  (setf (text editor) (load-text editor)))

(defmethod (setf file) :after (file (editor editor))
  (setf (text editor) (load-text editor)))

(defmethod (setf start) :after (start (editor editor))
  (setf (text editor) (load-text editor)))

(defmethod (setf end) :after (end (editor editor))
  (setf (text editor) (load-text editor)))

(defmethod load-text ((editor editor))
  (with-open-file (s (file editor))
    (loop repeat (or (start editor) 0)
          do (read-line s))
    (with-output-to-string (o)
      (loop repeat (if (end editor)
                       (- (end editor) (or (start editor) 0))
                       most-positive-fixnum)
            for line = (read-line s NIL)
            while line do (write-line line o)))))

(defmethod save-text ((editor editor))
  (let ((full (with-output-to-string (o)
                (with-open-file (s (file editor))
                  (loop repeat (or (start editor) 0)
                        do (write-line (read-line s) o))
                  (format o "~&~a~%" (text editor))
                  (when (end editor)
                    (loop repeat (- (end editor) (or (start editor) 0))
                          do (read-line s NIL))
                    (loop for line = (read-line s NIL)
                          while line
                          do (write-line line o)))))))
    (with-open-file (s (file editor) :direction :output :if-exists :supersede)
      (write-string full s))))

(defun string-remove-pos (string pos)
  (let ((new (make-array (1- (length string)) :element-type 'character)))
    (replace new string :end1 pos)
    (replace new string :start1 pos :start2 (1+ pos))
    new))

(defun string-insert-pos (string pos stuff)
  (let ((new (make-array (+ (length string) (length stuff)) :element-type 'character)))
    (replace new string :end1 pos)
    (replace new stuff :start1 pos)
    (replace new string :start1 (+ pos (length stuff)) :start2 pos)
    new))

(define-handler (editor key-release) (ev key)
  (with-accessors ((pos pos)) (cursor editor)
    (case key
      (:backspace
       (when (<= 1 pos (length (text editor)))
         (setf (text editor) (string-remove-pos (text editor) (1- pos)))
         (decf pos)))
      (:delete
       (when (< -1 pos (length (text editor)))
         (setf (text editor) (string-remove-pos (text editor) pos))))
      (:left
       (decf pos))
      (:right
       (incf pos))
      (:home
       (setf pos 0))
      (:end
       (setf pos (length (text editor)))))))

(define-handler (editor text-entered) (ev text)
  (setf (text editor) (string-insert-pos (text editor) (pos (cursor editor)) text))
  (incf (pos (cursor editor)) (length text)))

(defun editor (source &key start end)
  (enter-instance 'editor :file source :start start :end end))
