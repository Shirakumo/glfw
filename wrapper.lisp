(in-package #:org.shirakumo.fraf.glfw)

(defvar *window-table* (make-hash-table :test 'eql))
(defvar *initialized* NIL)

(defun glfw:resolve-window (ptr)
  (gethash (cffi:pointer-address ptr) *window-table*))

(define-condition glfw-error (error)
  ((operation :initarg :operation :initform NIL :reader operation)
   (code :initarg :code :initform NIL :reader code)
   (message :initarg :message :initform NIL :reader message))
  (:report (lambda (c s) (format s "GLFW call~@[ to ~a~] failed~@[ with ~a~]~@[:~%  ~a~]"
                                 (operation c) (code c) (message c)))))

(defmacro glfw (call &rest args)
  `(prog1 (,(find-symbol (string call) "GLFW") ,@args)
     (cffi:with-foreign-object ((str :pointer))
       (let ((code (glfw:get-error str)))
         (unless (eql :no-error code)
           (error 'glfw-error :operation ',call :code code :message (cffi:foreign-string-to-lisp str :encoding :utf-8)))))))

(defun init (&rest args &key)
  (unless *initialized*
    (unless (cffi:foreign-library-loaded-p 'glfw:libglfw)
      (cffi:load-foreign-library 'glfw:libglfw))
    (loop for (k v) on args by #'cddr
          do (glfw init-hint k v))
    (glfw init)
    (setf *initialized* T)))

(defun shutdown ()
  (when *initialized*
    (loop for window being the hash-values of *window-table*
          do (destroy window))
    (glfw terminate)
    (setf *initialized* NIL)))

(defclass window ()
  ((pointer :initform NIL :accessor pointer)))

(defmethod make-instance :after ((window window) &rest args &key width height title monitor share)
  (init)
  (loop for (k v) on args by #'cddr
        unless (find k '(:width :height :title :monitor :share))
        do (if (stringp v)
               (glfw window-hint-string k v)
               (glfw window-hint k v)))
  (let ((pointer (glfw create-window
                       (or width 800)
                       (or height 600)
                       (or title "GLFW")
                       (or monitor (cffi:null-pointer))
                       (or share (cffi:null-pointer))))
        ok)
    (setf (pointer window) pointer)
    (setf (gethash (cffi:pointer-address pointer) *window-table*) window)
    (unwind-protect
         (progn
           (register-callbacks window)
           (setf ok T))
      (unless ok
        (destroy window)))))

(defmethod register-callbacks ((window window))
  (glfw:set-error-callback pointer (cffi:callback glfw:error))
  (glfw:set-monitor-callback pointer (cffi:callback glfw:monitor))
  (glfw:set-winow-pos-callback pointer (cffi:callback glfw:winow-pos))
  (glfw:set-window-size-callback pointer (cffi:callback glfw:window-size))
  (glfw:set-window-close-callback pointer (cffi:callback glfw:window-close))
  (glfw:set-window-refresh-callback pointer (cffi:callback glfw:window-refresh))
  (glfw:set-window-focus-callback pointer (cffi:callback glfw:window-focus))
  (glfw:set-window-iconify-callback pointer (cffi:callback glfw:window-iconify))
  (glfw:set-window-maximize-callback pointer (cffi:callback glfw:window-maximize))
  (glfw:set-framebuffer-size-callback pointer (cffi:callback glfw:framebuffer-size))
  (glfw:set-window-content-scale-callback pointer (cffi:callback glfw:window-content-scale))
  (glfw:set-key-callback pointer (cffi:callback glfw:key))
  (glfw:set-char-callback pointer (cffi:callback glfw:char))
  (glfw:set-char-mods-callback pointer (cffi:callback glfw:char-mods))
  (glfw:set-mouse-button-callback pointer (cffi:callback glfw:mouse-button))
  (glfw:set-cursor-pos-callback pointer (cffi:callback glfw:cursor-pos))
  (glfw:set-cursor-enter-callback pointer (cffi:callback glfw:cursor-enter))
  (glfw:set-scroll-callback pointer (cffi:callback glfw:scroll))
  (glfw:set-drop-callback pointer (cffi:callback glfw:drop))
  (glfw:set-joystick-callback pointer (cffi:callback glfw:joystick)))

(defmethod destroy ((window window))
  (when (pointer window)
    (remhash (cffi:pointer-address (pointer window)) *window-table*)
    (glfw destroy-window (pointer window))
    (setf (pointer window) NIL)))

(defmethod glfw:allocate (size (window window))
  (cffi:foreign-funcall "malloc" :size size :pointer))

(defmethod glfw:reallocate (ptr size (window window))
  (cffi:foreign-funcall "realloc" :pointer ptr :size size :pointer))

(defmethod glfw:deallocate (ptr size (window window))
  (cffi:foreign-funcall "free" :pointer ptr :void))

(defmethod glfw:window-position ((window window) xpos ypos))
(defmethod glfw:window-size ((window window) width height))
(defmethod glfw:window-close ((window window))
  (glfw:set-window-should-close (pointer window) T))
(defmethod glfw:window-refresh ((window window)))
(defmethod glfw:window-focus ((window window) focused))
(defmethod glfw:window-iconify ((window window) iconified))
(defmethod glfw:window-maximize ((window window) maximized))
(defmethod glfw:framebuffer-size ((window window) width height))
(defmethod glfw:window-content-scale ((window window) xscale yscale))
(defmethod glfw:mouse-button ((window window) button action modifiers))
(defmethod glfw:mouse-position ((window window) xpos ypos))
(defmethod glfw:mouse-enter ((window window) entered))
(defmethod glfw:mouse-scroll ((window window) xoffset yoffset))
(defmethod glfw:key ((window window) key scan-code action modifiers))
(defmethod glfw:char ((window window) code-point))
(defmethod glfw:char-modifiers ((window window) code-point modifiers))
(defmethod glfw:drop ((window window) path-count paths))
