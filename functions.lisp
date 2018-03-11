#|
 This file is a part of beamer
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.beamer)

(defun h (text)
  (enter-instance 'header :text text :size 24))

(define-shader-entity header (text)
  ()
  (:default-initargs :font (asset 'trial:trial 'trial::noto-sans)))

(defun p (text)
  (enter-instance 'paragraph :text text))

(define-shader-entity paragraph (text)
  ()
  (:default-initargs :font (asset 'trial:trial 'trial::noto-sans)))

;; (defun list (&body entries)
;;   (enter-instance 'list :entries entries))

;; (defun editor (source)
;;   (enter-instance 'editor :source source))
