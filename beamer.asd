(asdf:defsystem beamer
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A slide show software based on Trial"
  :homepage "https://Shinmera.github.io/beamer/"
  :bug-tracker "https://github.com/Shinmera/beamer/issues"
  :source-control (:git "https://github.com/Shinmera/beamer.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "highlight")
               (:file "slide-show")
               (:file "functions")
               (:file "editor")
               (:file "slide")
               (:file "documentation"))
  :defsystem-depends-on (:deploy)
  :depends-on (:trial-glfw
               :trial-alloy
               :trial-png
               :trial-jpeg
               :array-utils
               :form-fiddle)
  :build-operation "deploy-op"
  :build-pathname "beamer"
  :entry-point "beamer:toplevel")
