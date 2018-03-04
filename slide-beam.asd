#|
 This file is a part of slide-beam
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem slide-beam
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A slide show software based on Trail"
  :homepage "https://github.com/Shinmera/slide-beam"
  :serial T
  :components ((:file "slide-beam"))
  :depends-on (:trial-glfw))
