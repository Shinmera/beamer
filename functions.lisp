#|
 This file is a part of beamer
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.beamer)

(define-pool beamer
  :base :beamer)

(define-asset (beamer header) font
    #p"Concourse C6 Regular.ttf"
  :size 48)

(define-asset (beamer text) font
    #p"Concourse T3 Regular.ttf"
  :size 24)

(define-asset (beamer code) font
    #p"Triplicate T4 Code Regular.ttf"
  :size 24)

(define-shader-subject slide-text (ui-element text)
  ()
  (:default-initargs :width 500
                     :wrap T))

(define-handler (slide-text resize) (ev width height)
  (setf (width slide-text) width))

(defun h (text)
  (enter-instance 'header :text text))

(define-shader-subject header (slide-text)
  ()
  (:default-initargs :font (asset 'beamer 'header)))

(defun p (text)
  (enter-instance 'paragraph :text text))

(define-shader-subject paragraph (slide-text)
  ()
  (:default-initargs :font (asset 'beamer 'text)))

(define-shader-subject code (slide-text)
  ()
  (:default-initargs :font (asset 'beamer 'code)))

;; (defun list (&body entries)
;;   (enter-instance 'list :entries entries))

;; (defun editor (source)
;;   (enter-instance 'editor :source source))
