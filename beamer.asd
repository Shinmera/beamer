(asdf:defsystem beamer
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A slide show software based on Trial"
  :homepage "https://shinmera.com/docs/beamer/"
  :bug-tracker "https://shinmera.com/project/beamer/issues"
  :source-control (:git "https://shinmera.com/project/beamer.git")
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
