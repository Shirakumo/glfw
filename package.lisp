(defpackage #:org.shirakumo.fraf.glfw.cffi
  (:use #:cl)
  (:shadow #:error #:char)
  (:export
   #:libglfw))

(defpackage #:org.shirakumo.fraf.glfw
  (:use #:cl)
  (:local-nicknames
   (#:glfw #:org.shirakumo.fraf.glfw.cffi))
  (:export
   #:init
   #:shutdown))
