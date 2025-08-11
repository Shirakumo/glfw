(asdf:defsystem glfw
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "An up-to-date bindings library to the most recent GLFW OpenGL context management library"
  :homepage "https://shirakumo.org/docs/glfw/"
  :bug-tracker "https://shirakumo.org/project/glfw/issues"
  :source-control (:git "https://shirakumo.org/project/glfw.git")
  :serial T
  :components ((:file "package")
               (:file "low-level")
               (:file "wrapper")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :float-features
               :cl-opengl
               :cffi))
