#|
 This file is a part of beamer
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem beamer
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A slide show software based on Trail"
  :homepage "https://Shinmera.github.io/beamer/"
  :bug-tracker "https://github.com/Shinmera/beamer/issues"
  :source-control (:git "https://github.com/Shinmera/beamer.git")
  :serial T
  :components ((:file "package")
               (:file "highlight")
               (:file "ui")
               (:file "slide-show")
               (:file "functions")
               (:file "editor")
               (:file "slide")
               (:file "controller"))
  :defsystem-depends-on (:deploy)
  :depends-on (:trial-glfw
               :array-utils
               :form-fiddle)
  :build-operation "deploy-op"
  :build-pathname "beamer"
  :entry-point "beamer:toplevel")
