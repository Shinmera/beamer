(defpackage #:beamer
  (:nicknames #:org.shirakumo.beamer)
  (:use #:cl+trial)
  (:shadow #:language #:image)
  (:local-nicknames
   (#:v #:org.shirakumo.verbose)
   (#:alloy #:org.shirakumo.alloy)
   (#:trial-alloy #:org.shirakumo.fraf.trial.alloy)
   (#:simple #:org.shirakumo.alloy.renderers.simple)
   (#:presentations #:org.shirakumo.alloy.renderers.simple.presentations)
   (#:opengl #:org.shirakumo.alloy.renderers.opengl)
   (#:colored #:org.shirakumo.alloy.colored)
   (#:colors #:org.shirakumo.alloy.colored.colors)
   (#:animation #:org.shirakumo.alloy.animation))
  ;; functions.lisp
  (:export
   #:h
   #:header
   #:p
   #:paragraph
   #:c
   #:code
   #:items
   #:editor
   #:image
   #:on-show
   #:note
   #:show-time)
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
  (:import-from #:cl #:in-package #:use-package)
  (:shadowing-import-from #:beamer #:define-slide #:enter-instance #:p #:h #:c #:items #:editor #:image #:on-show #:note #:show-time)
  (:use #:cl+trial))

(do-symbols (symbol '#:org.shirakumo.beamer.user)
  (export symbol '#:org.shirakumo.beamer.user))
