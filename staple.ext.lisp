(defmethod staple:packages ((system (eql (asdf:find-system :glfw))))
  (list (find-package "ORG.SHIRAKUMO.FRAF.GLFW")))

(defmethod staple:images ((system asdf:system))
  ())
