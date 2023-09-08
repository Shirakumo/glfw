(in-package #:org.shirakumo.fraf.glfw)

(defvar *object-table* (make-hash-table :test 'eql))
(defvar *monitors* ())
(defvar *initialized* NIL)

(defun ptr-object (ptr)
  (gethash (cffi:pointer-address ptr) *object-table*))

(defun (setf ptr-object) (value ptr)
  (if value
      (setf (gethash (cffi:pointer-address ptr) *object-table*) value)
      (remhash (cffi:pointer-address ptr) *object-table*))
  value)

(defun glfw:resolve-window (ptr)
  (gethash (cffi:pointer-address ptr) *object-table*))

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
    (list-monitors :refresh T)
    (setf *initialized* T)))

(defun shutdown ()
  (when *initialized*
    (loop for window being the hash-values of *object-table*
          do (destroy window))
    (loop for monitor in *monitors*
          do (destroy monitor))
    (glfw terminate)
    (setf *initialized* NIL)))

(defclass monitor ()
  ((pointer :initform NIL :initarg :pointer :accessor pointer)
   (video-modes :initform NIL :reader video-modes)))

(defmethod initialize-instance :after ((monitor monitor) &key)
  (setf (slot-value monitor 'video-modes)
        (cffi:with-foreign-object (count :int)
          (let ((array (glfw get-monitors count)))
            (loop for i from 0 below (cffi:mem-ref count :int)
                  for ptr = (cffi:mem-aref array :pointer i)
                  collect (cffi:mem-ref ptr '(:struct glfw:video-mode))))))
  (setf (ptr-object (pointer monitor)) monitor)
  (push monitor *monitors*))

(defmethod destroy ((monitor monitor))
  (when (pointer monitor)
    (setf (ptr-object (pointer monitor)) NIL)
    (setf *monitors* (remove monitor *monitors*))
    (setf (pointer monitor) NIL)))

(defun glfw:monitor-connected (ptr)
  (make-instance 'monitor :pointer ptr))

(defun glfw:monitor-disconnected (ptr)
  (let ((monitor (ptr-object ptr)))
    (when monitor (destroy monitor))))

(defun list-monitors (&key refresh)
  (when refresh
    (cffi:with-foreign-object (count :int)
      (let ((array (glfw get-monitors count)))
        (loop for monitor in *monitors*
              do (destroy monitor))
        (loop for i from 0 below (cffi:mem-ref count :int)
              for ptr = (cffi:mem-aref array :pointer i)
              do (make-instance 'monitor :pointer ptr)))))
  *monitors*)

(defun primary-monitor ()
  (or (ptr-object (glfw get-primary-monitor))
      (list-monitors :refresh T)
      (ptr-object (glfw get-primary-monitor))
      (error 'glfw-error :message "Getting a primary monitor that is not part of the monitors list.")))

(defmacro extract-values (bindings &body body)
  `(cffi:with-foreign-objects ,bindings
     ,@body
     (list ,@(loop for (var type) in bindings
                   collect `(cffi:mem-ref ,var ,type)))))

(defmethod location ((monitor monitor))
  (extract-values ((x :int) (y :int))
    (glfw get-monitor-pos (pointer monitor) x y)))

(defmethod work-area ((monitor monitor))
  (extract-values ((x :int) (y :int) (w :int) (h :int))
    (glfw get-monitor-workarea (pointer monitor) x y w h)))

(defmethod physical-size ((monitor monitor))
  (extract-values ((x :int) (y :int))
    (glfw get-monitor-physical-size (pointer monitor) x y)))

(defmethod content-scale ((monitor monitor))
  (extract-values ((x :float) (y :float))
    (glfw get-monitor-content-scale (pointer monitor) x y)))

(defmethod name ((monitor monitor))
  (glfw get-monitor-name (pointer monitor)))

(defmethod video-mode ((monitor monitor))
  (let ((mode (glfw get-video-mode monitor)))
    (cffi:mem-ref mode '(:struct glfw:video-mode))))

(defmethod gamma ((monitor monitor))
  (let* ((ramp (glfw get-gamma-ramp (pointer monitor)))
         (midpoint (cffi:mem-aref (glfw:gamma-ramp-red ramp) :ushort (truncate (glfw:gamma-ramp-size ramp) 2))))
    (log midpoint 0.5)))

(defmethod (setf gamma) (gamma (monitor monitor))
  (glfw set-gamma (pointer monitor) (float gamma 0f0))
  gamma)

(defmethod gamma-ramp ((monitor monitor))
  (let* ((ramp (glfw get-gamma-ramp (pointer monitor)))
         (count (glfw:gamma-ramp-size ramp)))
    (list :red (cffi:foreign-array-to-lisp (glfw:gamma-ramp-red ramp) (list :array :ushort count))
          :green (cffi:foreign-array-to-lisp (glfw:gamma-ramp-green ramp) (list :array :ushort count))
          :blue (cffi:foreign-array-to-lisp (glfw:gamma-ramp-blue ramp) (list :array :ushort count)))))

(defmethod (setf gamma-ramp) (ramps (monitor monitor))
  (destructuring-bind (&key red green blue) ramps
    (assert (= (length red) (length green) (length blue)))
    (cffi:with-foreign-objects ((ramp '(:struct glfw:gamma-ramp))
                                (r :ushort (length red))
                                (g :ushort (length red))
                                (b :ushort (length red)))
      (cffi:lisp-array-to-foreign red r (list :array :ushort (length red)))
      (cffi:lisp-array-to-foreign red g (list :array :ushort (length red)))
      (cffi:lisp-array-to-foreign red b (list :array :ushort (length red)))
      (setf (glfw:gamma-ramp-red ramp) r)
      (setf (glfw:gamma-ramp-green ramp) g)
      (setf (glfw:gamma-ramp-blue ramp) b)
      (setf (glfw:gamma-ramp-size ramp) (length red))
      (glfw set-gamma-ramp (pointer monitor) ramp)
      ramps)))

(defclass window ()
  ((pointer :initform NIL :accessor pointer)))

(defmethod initialize-instance :after ((window window) &rest args &key width height title monitor share)
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
    (setf (ptr-object ptr) window)
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
    (setf (ptr-object (pointer window)) NIL)
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
