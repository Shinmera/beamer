(in-package #:org.shirakumo.beamer)

(define-global +app-system+ "weiss")

(defun join-lines (vec &key (indent 0) (out NIL))
  (etypecase out
    (null
     (with-output-to-string (out)
       (join-lines vec :indent indent :out out)))
    (stream
     (when (< 0 (length vec))
       (write-string (aref vec 0) out)
       (loop for i from 1 below (length vec)
             do (format out "~%~v{ ~}~a" indent 0 (aref vec i)))))))

(defun split-lines (in &key (trim 0) count)
  (etypecase in
    (string
     (with-input-from-string (in in)
       (split-lines in :trim trim :count count)))
    (T
     (let ((lines (make-array 0 :adjustable T :fill-pointer T)))
       (loop repeat (or count most-positive-fixnum)
             for line = (read-line in NIL)
             while line
             do (vector-push-extend (if (<= trim (length line))
                                        (subseq line trim)
                                        "")
                                    lines))
       lines))))

(defun hex->color (hex)
  (colored:color 
   (/ (ldb (byte 8 16) hex) 255)
   (/ (ldb (byte 8 8) hex) 255)
   (/ (ldb (byte 8 0) hex) 255)))

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

(defun format-clock (clock)
  (let ((clock (round clock)))
    (format NIL "~2d:~2,'0d" (floor clock 60) (mod clock 60))))
