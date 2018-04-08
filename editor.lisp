#|
 This file is a part of beamer
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.beamer)

(define-action editor-keys ())

(define-action save (editor-keys)
  (key-press (and (eql key :s) (find :control modifiers))))

(define-action load (editor-keys)
  (key-press (and (eql key :l) (find :control modifiers))))

(define-action recompile (editor-keys)
  (key-press (and (eql key :c) (find :control modifiers))))

(define-action forward (editor-keys)
  (key-press (and (eql key :f) (find :control modifiers))))

(define-action backward (editor-keys)
  (key-press (and (eql key :b) (find :control modifiers))))

(define-shader-entity cursor (vertex-entity)
  ((parent :initarg :parent :accessor parent)
   (col :initform 0 :accessor col)
   (row :initform 0 :accessor row)
   (cursor-size :initform (vec4 0 0 0.5 1) :reader cursor-size))
  (:default-initargs
   :parent (error "PARENT required.")
   :vertex-array (asset 'trial 'trial::fullscreen-square)))

(defmethod init ((cursor cursor))
  (let* ((parent (parent cursor))
         (extent (extent parent)))
    (setf (vy (cursor-size cursor)) 0)
    (setf (vz (cursor-size cursor)) 1)
    (setf (vw (cursor-size cursor)) (/ (getf extent :t) 2))
    (setf (row cursor) 0)))

(defmethod (setf row) :around (value (cursor cursor))
  (call-next-method (min (1- (length (lines (parent cursor)))) (max 0 value)) cursor))

(defmethod (setf row) :after (row (cursor cursor))
  (let ((extent (text-extent (parent cursor) (subseq (line (parent cursor) row) 0 (col* cursor)))))
    (setf (vy (cursor-size cursor)) (- 0 (getf extent :b)
                                       (* row (+ (getf extent :t) (getf extent :gap)))))
    (setf (vx (cursor-size cursor)) (getf extent :r))))

(defmethod (setf col) :around (value (cursor cursor))
  (call-next-method (min (length (line (parent cursor) (row cursor))) (max 0 value)) cursor))

(defmethod (setf col) :after (col (cursor cursor))
  (let ((extent (text-extent (parent cursor) (subseq (line (parent cursor) (row cursor)) 0 col))))
    (setf (vx (cursor-size cursor))
          (* (getf extent :r)))))

(defmethod col* ((cursor cursor))
  (min (col cursor) (length (line (parent cursor) (row cursor)))))

(defmethod paint :before ((cursor cursor) target)
  (let ((size (cursor-size cursor)))
    (translate-by (vx size) (+ (vy size) (vw size)) 0)
    (scale-by (vz size) (vw size) 1)))

(define-class-shader (cursor :fragment-shader)
  "out vec4 color;

void main(){
    color = vec4(1,1,1,1);
}")

(define-shader-subject editor (slide-text highlighted-text)
  ((file :initarg :file :accessor file)
   (lines :initform NIL :accessor lines)
   (start :initarg :start :accessor start)
   (end :initarg :end :accessor end)
   (trim :initarg :trim :accessor trim)
   (cursor :initform NIL :accessor cursor)
   (language :initarg :language :accessor language)
   (theme :initarg :theme :accessor theme))
  (:default-initargs :font (asset 'beamer 'code)
                     :size 24
                     :wrap NIL
                     :start NIL
                     :end NIL
                     :trim 0
                     :margin (vec2 0 0)
                     :language *default-language*
                     :theme *default-theme*))

(defmethod initialize-instance :after ((editor editor) &key file)
  (setf (cursor editor) (make-instance 'cursor :parent editor))
  (when file
    (load-text editor)))

(defmethod (setf file) :after (file (editor editor))
  (load-text editor))

(defmethod (setf start) :after (start (editor editor))
  (load-text editor))

(defmethod (setf end) :after (end (editor editor))
  (load-text editor))

(defmethod paint :after ((editor editor) target)
  (paint (cursor editor) target))

(defmethod register-object-for-pass :after (pass (editor editor))
  (register-object-for-pass pass (cursor editor)))

(defmethod line ((editor editor) n)
  (aref (lines editor) n))

(defmethod (setf line) (value (editor editor) n)
  (setf (aref (lines editor) n) value))

(defmethod load-text ((editor editor))
  (with-open-file (s (file editor))
    (loop repeat (1- (or (start editor) 0))
          do (read-line s))
    (let ((lines (make-array 0 :adjustable T :fill-pointer T))
          (trim (trim editor)))
      (loop repeat (if (end editor)
                       (- (1+ (end editor)) (or (start editor) 0))
                       most-positive-fixnum)
            for line = (read-line s NIL)
            while line
            do (vector-push-extend (if (<= trim (length line))
                                       (subseq line trim)
                                       "")
                                   lines))
      (setf (lines editor) lines)
      (setf (text editor) (join-lines lines)))))

(defmethod save-text ((editor editor))
  (let ((full (with-output-to-string (o)
                (with-open-file (s (file editor))
                  (loop repeat (1- (or (start editor) 0))
                        do (write-line (read-line s) o))
                  (loop for line across (lines editor)
                        do (format o "~v{ ~}~a~%" (trim editor) 0 line))
                  (when (end editor)
                    (loop repeat (- (1+ (end editor)) (or (start editor) 0))
                          do (read-line s NIL))
                    (loop for line = (read-line s NIL)
                          while line
                          do (write-line line o)))))))
    (with-open-file (s (file editor) :direction :output :if-exists :supersede)
      (write-string full s))))

(defmethod resources-ready :after ((editor editor))
  (init (cursor editor)))

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

(defun %prev-line-pos (editor &optional (pos (pos (cursor editor))))
  (loop for i downfrom pos to 0
        do (when (char= (aref (text editor) i) #\Linefeed)
             (return i))
        finally (return 0)))

(defun %next-line-pos (editor &optional (pos (pos (cursor editor))))
  (loop for i from pos below (length (text editor))
        do (when (char= (aref (text editor) i) #\Linefeed)
             (return i))
        finally (return (length (text editor)))))

(defun join-lines (vec)
  (with-output-to-string (out)
    (when (< 0 (length vec))
      (write-string (aref vec 0) out)
      (loop for i from 1 below (length vec)
            do (format out "~%~a" (aref vec i))))))

(defmethod (setf text) :before (text (editor editor))
  (multiple-value-bind (base regions) (determine-regions text :language (language editor) :theme (theme editor))
    (setf (color editor) base)
    (setf (color-regions editor) regions)))

(define-handler (editor key-release) (ev key)
  (with-accessors ((col col) (row row) (col* col*)) (cursor editor)
    (flet ((del ()
             (cond ((<= (length (line editor row)) col)
                    (when (< row (1- (length (lines editor))))
                      (setf (line editor row) (format NIL "~a~a" (line editor row) (line editor (1+ row))))
                      (array-utils:vector-pop-position (lines editor) (1+ row))))
                   (T
                    (setf (line editor row) (string-remove-pos (line editor row) col*))))
             (setf (text editor) (join-lines (lines editor)))))
      (case key
        (:enter
         (let ((old (line editor row)))
           (setf (line editor row) (subseq old 0 col*))
           (array-utils:vector-push-extend-position (subseq old col*) (lines editor) (1+ row))
           (setf col 0)
           (incf row)
           (setf (text editor) (join-lines (lines editor)))))
        (:backspace
         (cond ((= 0 col)
                (when (< 0 row)
                  (decf row)
                  (setf col (length (line editor row)))
                  (del)))
               (T
                (setf col (1- col*))
                (del))))
        (:delete
         (del))
        (:left
         (if (= 0 col)
             (setf row (1- row) col (length (line editor row)))
             (decf col)))
        (:right
         (if (<= (length (line editor row)) col)
             (setf row (1+ row) col 0)
             (incf col)))
        (:up
         (decf row))
        (:down
         (incf row))
        (:home
         (setf col 0))
        (:end
         (setf col (length (line editor row))))))))

(define-handler (editor text-entered) (ev text)
  (with-accessors ((col col) (col* col*) (row row)) (cursor editor)
    (setf (line editor row) (string-insert-pos (line editor row) col* text))
    (setf (text editor) (join-lines (lines editor)))
    (setf (col (cursor editor)) (+ col* (length text)))))

(define-handler (editor save) (ev)
  (save-text editor))

(define-handler (editor load) (ev)
  (load-text editor))

(define-handler (editor forward) (ev)
  ;; Primitive forward
  (with-accessors ((col col) (col* col*) (row row)) (cursor editor)
    (let* ((row (row (cursor editor)))
           (col (col* (cursor editor)))
           (line (line editor row)))
      (cond ((or (<= (length line) col) (char= #\Space (char line col)))
             (loop for c from col
                   do (cond ((<= (length line) c)
                             (when (= row (1- (length (lines editor))))
                               (setf col c)
                               (return))
                             (incf row) (setf c 0))
                            ((char/= #\Space (char line c))
                             (setf col c)
                             (return)))))
            (T
             (loop for c from col
                   do (cond ((<= (length line) c)
                             (setf col c)
                             (return))
                            ((char= #\Space (char line c))
                             (setf col c)
                             (return))))))
      (setf (row (cursor editor)) row)
      (setf (col (cursor editor)) col))))

(define-handler (editor recompile) (ev)
  (let ((*package* (ensure-package (name (slide-show *scene*)))))
    (cl:load (file editor))))

(defun editor (source &rest initargs)
  (apply #'enter-instance 'editor
         :file (slide-file source)
         initargs))
