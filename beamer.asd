#|
 This file is a part of beamer
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem beamer
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A slide show software based on Trail"
  :homepage "https://github.com/Shinmera/beamer"
  :serial T
  :components ((:file "package")
               (:file "ui")
               (:file "slide-show")
               (:file "functions")
               (:file "main"))
  :defsystem-depends-on (:deploy)
  :depends-on (:trial-glfw
               :array-utils)
  :build-operation "deploy-op"
  :build-pathname "beamer"
  :entry-point "beamer:toplevel")
