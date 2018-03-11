#|
 This file is a part of beamer
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:beamer
  (:nicknames #:org.shirakumo.beamer)
  (:use #:cl+trial)
  ;; functions.lisp
  (:export
   #:h
   #:header
   #:p
   #:paragraph
   #:list
   #:editor)
  ;; main.lisp
  (:export
   #:start-slideshow
   #:toplevel)
  ;; slide-show.lisp
  (:export
   #:*slide*
   #:load-slide-show
   #:slide-show
   #:base
   #:slide-package
   #:slides
   #:index
   #:load-slide
   #:current-slide
   #:advance-slide
   #:next-slide
   #:prev-slide
   #:slide
   #:slide-show
   #:source
   #:name
   #:enter-instance
   #:define-slide-function))
