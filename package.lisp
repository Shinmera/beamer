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
   #:c
   #:code
   #:items
   #:editor)
  ;; main.lisp
  (:export
   #:start-slideshow
   #:toplevel)
  ;; slide-show.lisp
  (:export
   #:*slide*
   #:load-slide-show
   #:current-show
   #:slide-show
   #:source
   #:slides
   #:index
   #:current-slide
   #:advance-slide
   #:next-slide
   #:prev-slide
   #:slide
   #:define-slide
   #:slide
   #:slide-show
   #:constructor
   #:enter-instance
   #:reconstruct-slide))

(defpackage #:beamer-user
  (:nicknames #:org.shirakumo.beamer.user)
  (:import-from #:cl #:in-package #:use-package))

(dolist (name '(#:define-slide #:enter-instance #:p #:h #:c #:items #:editor))
  (let ((symbol (find-symbol (string name) '#:org.shirakumo.beamer)))
    (import symbol '#:org.shirakumo.beamer.user)))

(do-symbols (symbol '#:org.shirakumo.beamer.user)
  (export symbol '#:org.shirakumo.beamer.user))
