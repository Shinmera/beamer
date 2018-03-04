#|
 This file is a part of slide-beam
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.slide-beam)

(define-slide-function h (text)
  (enter-instance 'header :text text))

(define-slide-function p (text)
  (enter-instance 'paragraph :text text))

(define-slide-function list (&body entries)
  (enter-instance 'list :entries entries))

(define-slide-function editor (source)
  (enter-instance 'editor :source source))
