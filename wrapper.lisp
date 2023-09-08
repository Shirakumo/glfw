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
    (let ((lib (or (when (uiop:getenvp "WAYLAND_DISPLAY")
                     'glfw:libglfw-wayland)
                   'glfw:libglfw)))
      (unless (cffi:foreign-library-loaded-p lib)
        (cffi:load-foreign-library lib)))
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
  ((pointer :initform NIL :accessor pointer)
   (width :initarg :width :initform 800 :reader width)
   (height :initarg :height :initform 600 :reader height)
   (aspect-ratio :initform NIL :accessor aspect-ratio)
   (size-limits :initform (list -1 -1 -1 -1) :accessor size-limits)
   (swap-interval :initform 0 :accessor swap-interval)
   (title :initarg :title :initform "GLFW" :accessor title)))

(defmethod initialize-instance :after ((window window) &rest args &key monitor share)
  (init)
  (loop for (k v) on args by #'cddr
        unless (find k '(:width :height :title :monitor :share))
        do (if (stringp v)
               (glfw window-hint-string k v)
               (glfw window-hint k v)))
  (let ((pointer (glfw create-window
                       (width window)
                       (height window)
                       (title window)
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

(defmethod allocate (size (window window))
  (cffi:foreign-funcall "malloc" :size size :pointer))

(defmethod reallocate (ptr size (window window))
  (cffi:foreign-funcall "realloc" :pointer ptr :size size :pointer))

(defmethod deallocate (ptr size (window window))
  (cffi:foreign-funcall "free" :pointer ptr :void))

(defmethod window-size :before ((window window) width height)
  (setf (slot-value window 'width) width)
  (setf (slot-value window 'height) height))

(defmethod window-close ((window window))
  (glfw:set-window-should-close (pointer window) T))

(defmethod window-position ((window window) xpos ypos))
(defmethod window-size ((window window) width height))
(defmethod window-refresh ((window window)))
(defmethod window-focus ((window window) focused))
(defmethod window-iconify ((window window) iconified))
(defmethod window-maximize ((window window) maximized))
(defmethod framebuffer-size ((window window) width height))
(defmethod window-content-scale ((window window) xscale yscale))
(defmethod mouse-button ((window window) button action modifiers))
(defmethod mouse-position ((window window) xpos ypos))
(defmethod mouse-enter ((window window) entered))
(defmethod mouse-scroll ((window window) xoffset yoffset))
(defmethod key ((window window) key scan-code action modifiers))
(defmethod char ((window window) code-point))
(defmethod char-modifiers ((window window) code-point modifiers))
(defmethod drop ((window window) path-count paths))

(defmethod should-close-p ((window window))
  (glfw:window-should-close (pointer window)))

(defmethod (setf should-close-p) (bool (window window))
  (glfw:set-window-should-close (pointer window) bool))

(defmethod (setf title) :before (title (window window))
  (glfw set-window-title (pointer window) title))

(defmethod location ((window window))
  (extract-values ((x :int) (y :int))
    (glfw get-window-pos (pointer window) x y)))

(defmethod (setf location) (pos (window window))
  (destructuring-bind (x y) pos
    (glfw set-window-pos (pointer window) (round x) (round y))
    pos))

(defmethod size ((window window))
  (cons (width window) (height window)))

(defmethod (setf size) (size (window window))
  (destructuring-bind (w h) size
    (glfw set-window-size (pointer window) w h)
    (setf (slot-value window 'width) w)
    (setf (slot-value window 'height) h)
    size))

(defmethod (setf width) (size (window window))
  (setf (size window) (list size (height window)))
  size)

(defmethod (setf height) (size (window window))
  (setf (size window) (list (width window) size))
  size)

(defmethod (setf size-limits) :before (size (window window))
  (destructuring-bind (min-w min-h max-w max-h) size
    (glfw set-window-size-limits (pointer window) (or min-w -1) (or min-h -1) (or max-w -1) (or max-h -1))))

(defmethod (setf aspect-ratio) :before (ratio (window window))
  (glfw set-window-aspect-ratio (pointer window) (numerator ratio) (denominator ratio)))

(defmethod (setf aspect-ratio) :before ((none null) (window window))
  (glfw set-window-aspect-ratio (pointer window) -1 -1))

(defmethod framebuffer-size ((window window))
  (extract-values ((w :int) (h :int))
    (glfw get-framebuffer-size (pointer window) w h)))

(defmethod frame-size ((window window))
  (extract-values ((l :int) (u :int) (r :int) (b :int))
    (glfw get-window-frame-size (pointer window) l u r b)))

(defmethod content-scale ((window window))
  (extract-values ((x :float) (y :float))
    (glfw get-window-content-scale (pointer window) x y)))

(defmethod opacity ((window window))
  (glfw get-window-opacity (pointer window)))

(defmethod (setf opacity) (value (window window))
  (glfw set-window-opacity (pointer window) (float value 0f0))
  value)

(defmethod iconify ((window window))
  (glfw iconify-window (pointer window))
  window)

(defmethod restore ((window window))
  (glfw restore-window (pointer window))
  window)

(defmethod maximize ((window window))
  (glfw maximize-window (pointer window))
  window)

(defmethod show ((window window))
  (glfw show-window (pointer window))
  window)

(defmethod hide ((window window))
  (glfw hide-window (pointer window))
  window)

(defmethod focus ((window window))
  (glfw focus-window (pointer window))
  window)

(defmethod request-attention ((window window))
  (glfw request-window-attention (pointer window))
  window)

(defmethod monitor ((window window))
  (ptr-object (glfw get-window-monitor (pointer window))))

(defmethod (setf monitor) ((monitor monitor) (window window))
  (glfw set-window-monitor (pointer window) (pointer monitor))
  monitor)

(defmethod (setf monitor) ((default (eql T)) (window window))
  (setf (monitor window) (primary-monitor)))

(defmethod attribute (attribute (window window))
  (glfw get-window-attrib (pointer window) attribute))

(defmethod (setf attribute) (value attribute (window window))
  (glfw set-window-attrib (pointer window) attribute value)
  value)

(defmethod input-mode (mode (window window))
  (glfw get-input-mode (pointer window) mode))

(defmethod (setf input-mode) (value mode (window window))
  (glfw set-input-mode (pointer window) mode value)
  value)

(defun poll-events (&key (timeout NIL))
  (etypecase timeout
    (null (glfw poll-events))
    ((eql T) (glfw wait-events))
    (real (glfw wait-events-timeout (float timeout 0d0)))))

(defmethod key-state (key (window window))
  (glfw get-key (pointer window) key))

(defmethod mouse-button-state (button (window window))
  (glfw get-mouse-button (pointer window) button))

(defmethod cursor-location ((window window))
  (extract-values ((x :double) (y :double))
    (glfw get-cursor-pos (pointer window) x y)))

(defmethod (setf cursor-location) (pos (window window))
  (destructuring-bind (x y) pos
    (glfw set-cursor-pos (pointer window) (float x 0d0) (float y 0d0))))

(defmethod clipboard-string ((window window))
  (glfw get-clipboard-string (pointer window)))

(defmethod (setf clipboard-string) (string (window window))
  (glfw set-clipboard-string (pointer window) string)
  string)

(defun time ()
  (glfw:get-time))

(defun (setf time) (time)
  (glfw:set-time (float time 0d0))
  time)

(defun timestamp ()
  (glfw:get-timer-value))

(defun timestamp-resolution ()
  (glfw:get-timer-frequency))

(defmethod make-current ((window window))
  (glfw make-context-current (pointer window)))

(defmethod get-current ()
  (ptr-object (glfw get-current-context)))

(defmethod swap-buffers ((window window))
  (glfw swap-buffers (pointer window)))

(defmethod (setf swap-interval) :before (interval (window window))
  (glfw swap-interval interval))
