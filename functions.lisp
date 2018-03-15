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
  :size 72)

(define-asset (beamer text) font
    #p"Concourse T3 Regular.ttf"
  :size 48)

(define-asset (beamer code) font
    #p"Triplicate T4 Code Regular.ttf"
  :size 48)

(define-shader-entity slide-text (text ui-element)
  ()
  (:default-initargs :wrap T))

(defmethod paint :around ((text slide-text) target)
  (with-pushed-matrix (model-matrix)
    (translate-by 0 (- (getf (text-extent text "") :t)) 0)
    (call-next-method)))

(define-shader-entity header (slide-text)
  ()
  (:default-initargs :font (asset 'beamer 'header)
                     :margin (vec4 10 5 0 20)))

(defun h (text)
  (enter-instance 'header :text text))

(define-shader-entity paragraph (slide-text)
  ()
  (:default-initargs :font (asset 'beamer 'text)
                     :margin (vec2 0 24)))

(defun p (text)
  (enter-instance 'paragraph :text text))

(define-shader-entity code (slide-text)
  ()
  (:default-initargs :font (asset 'beamer 'code)))

(defun c (text)
  (enter-instance 'code :text text))

;; (defun list (&body entries)
;;   (enter-instance 'list :entries entries))

;; (defun editor (source)
;;   (enter-instance 'editor :source source))
