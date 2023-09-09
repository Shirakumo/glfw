(in-package #:org.shirakumo.fraf.glfw)

(defvar *object-table* (make-hash-table :test 'eql))
(defvar *cursor-table* (make-hash-table :test 'eql))
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

(defmacro extract-values (bindings &body body)
  `(cffi:with-foreign-objects ,bindings
     ,@body
     (list ,@(loop for (var type) in bindings
                   collect `(cffi:mem-ref ,var ,type)))))

(define-condition glfw-error (error)
  ((operation :initarg :operation :initform NIL :reader operation)
   (code :initarg :code :initform NIL :reader code)
   (message :initarg :message :initform NIL :reader message))
  (:report (lambda (c s) (format s "GLFW call~@[ to ~a~] failed~@[ with ~a~]~@[:~%  ~a~]"
                                 (operation c) (code c) (message c)))))

(defmacro glfw (call &rest args)
  `(prog1 (,(find-symbol (string call) "GLFW") ,@args)
     (cffi:with-foreign-objects ((str :pointer))
       (let ((code (glfw:get-error str)))
         (unless (eql :no-error code)
           (error 'glfw-error :operation ',call :code code :message (cffi:foreign-string-to-lisp str :encoding :utf-8)))))))

(defun init (&rest args &key platform joystick-hat-buttons angle-platform-type cocoa-chdir-resources cocoa-menubar x11-xcb-vulkan-surface)
  (declare (ignore platform joystick-hat-buttons angle-platform-type cocoa-chdir-resources cocoa-menubar x11-xcb-vulkan-surface))
  (unless *initialized*
    (unless (cffi:foreign-library-loaded-p 'glfw:libglfw)
      (cffi:load-foreign-library 'glfw:libglfw))
    (loop for (k v) on args by #'cddr
          do (with-simple-restart (continue "Ignore the init hint.")
               (glfw init-hint k v)))
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

(defclass foreign-object ()
  ((pointer :initform NIL :initarg :pointer :accessor pointer)))

(defmethod print-object ((object foreign-object) stream)
  (print-unreadable-object (object stream :type T)
    (if (pointer object)
        (format stream "@~8,'0x" (cffi:pointer-address (pointer object)))
        (format stream "DEAD"))))

(defgeneric destroy (foreign-object))

(defclass cursor (foreign-object)
  ())

(defmethod initialize-instance :after ((cursor cursor) &key pixels width height (xhot 0) (yhot 0))
  (when pixels
    (cffi:with-foreign-objects ((image '(:struct glfw:image))
                                (buffer :char (length pixels)))
      (setf (glfw:image-width image) width)
      (setf (glfw:image-height image) height)
      (setf (glfw:image-pixels image) buffer)
      (loop for i from 0 below (length pixels)
            do (setf (cffi:mem-aref buffer :char i) (aref pixels i)))
      (setf (pointer cursor) (glfw create-cursor image (round xhot) (round yhot))))))

(defmethod destroy ((cursor cursor))
  (when (pointer cursor)
    (glfw destroy-cursor (pointer cursor))
    (setf (pointer cursor) NIL)))

(defclass standard-cursor (cursor)
  ((name :initarg :name :reader name)))

(defmethod print-object ((cursor standard-cursor) stream)
  (print-unreadable-object (cursor stream :type T)
    (format stream "~a" (name cursor))))

(defmethod initialize-instance :after ((cursor standard-cursor) &key)
  (setf (pointer cursor) (glfw create-standard-cursor (name cursor)))
  (setf (gethash (name cursor) *cursor-table*) cursor))

(defmethod cursor ((name symbol))
  (or (gethash name *cursor-table*)
      (make-instance 'cursor :name name)))

(defmethod destroy ((cursor standard-cursor))
  (when (pointer cursor)
    (remhash (name cursor) *cursor-table*)
    (setf (pointer cursor) NIL)))

(defclass monitor (foreign-object)
  ((video-modes :initform NIL :reader video-modes)))

(defmethod print-object ((monitor monitor) stream)
  (print-unreadable-object (monitor stream :type T)
    (if (pointer monitor)
        (format stream "~a" (name monitor))
        (format stream "DEAD"))))

(defmethod list-video-modes ((monitor monitor))
  (cffi:with-foreign-object (count :int)
    (let ((array (glfw get-video-modes (pointer monitor) count)))
      (loop for i from 0 below (print (cffi:mem-ref count :int))
            for ptr = (cffi:mem-aptr array '(:struct glfw:video-mode) i)
            collect (list (glfw:video-mode-width ptr)
                          (glfw:video-mode-height ptr)
                          (glfw:video-mode-refresh-rate ptr)
                          (glfw:video-mode-red-bits ptr)
                          (glfw:video-mode-green-bits ptr)
                          (glfw:video-mode-blue-bits ptr))))))

(defmethod initialize-instance :after ((monitor monitor) &key)
  (setf (slot-value monitor 'video-modes) (list-video-modes monitor))
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
  (copy-list *monitors*))

(defun primary-monitor ()
  (or (ptr-object (glfw get-primary-monitor))
      (when (list-monitors :refresh T)
        (ptr-object (glfw get-primary-monitor)))
      (error 'glfw-error :message "Getting a primary monitor that is not part of the monitors list.")))

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
  (let ((ptr (glfw get-video-mode (pointer monitor))))
    (list (glfw:video-mode-width ptr)
          (glfw:video-mode-height ptr)
          (glfw:video-mode-refresh-rate ptr)
          (glfw:video-mode-red-bits ptr)
          (glfw:video-mode-green-bits ptr)
          (glfw:video-mode-blue-bits ptr))))

(defmethod size ((monitor monitor))
  (let ((mode (glfw get-video-mode (pointer monitor))))
    (list (glfw:video-mode-width mode)
          (glfw:video-mode-height mode))))

(defmethod width ((monitor monitor))
  (glfw:video-mode-width (glfw get-video-mode (pointer monitor))))

(defmethod height ((monitor monitor))
  (glfw:video-mode-height (glfw get-video-mode (pointer monitor))))

(defmethod refresh-rate ((monitor monitor))
  (let ((mode (glfw get-video-mode (pointer monitor))))
    (glfw:video-mode-refresh-rate mode)))

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

(defclass window (foreign-object)
  ((width :initarg :width :initform 800 :reader width)
   (height :initarg :height :initform 600 :reader height)
   (aspect-ratio :initform NIL :accessor aspect-ratio)
   (size-limits :initform (list -1 -1 -1 -1) :accessor size-limits)
   (swap-interval :initform 0 :accessor swap-interval)
   (title :initarg :title :initform "GLFW" :accessor title)
   (cursor :initform NIL)
   (icon :initform () :accessor icon)))

(defmethod initialize-instance :after ((window window) &rest args &key monitor share
                                                                       resizable visible decorated focused auto-iconify floating maximized center-cursor transparent-framebuffer focus-on-show scale-to-monitor mouse-passthrough red-bits green-bits blue-bits alpha-bits depth-bits stencil-bits accum-red-bits accum-green-bits accum-blue-bits accum-alpha-bits aux-buffers stereo samples srgb-capable doublebuffer refresh-rate client-api context-creation-api context-version-major context-version-minor opengl-forward-compat context-debug opengl-profile context-robustness context-release-behavior context-no-error win32-keyboard-menu cocoa-retina-framebuffer cocoa-frame-name cocoa-graphics-switching x11-class-name x11-instance-name wayland-app-id)
  (declare (ignore resizable visible decorated focused auto-iconify floating maximized center-cursor transparent-framebuffer focus-on-show scale-to-monitor mouse-passthrough red-bits green-bits blue-bits alpha-bits depth-bits stencil-bits accum-red-bits accum-green-bits accum-blue-bits accum-alpha-bits aux-buffers stereo samples srgb-capable doublebuffer refresh-rate client-api context-creation-api context-version-major context-version-minor opengl-forward-compat context-debug opengl-profile context-robustness context-release-behavior context-no-error win32-keyboard-menu cocoa-retina-framebuffer cocoa-frame-name cocoa-graphics-switching x11-class-name x11-instance-name wayland-app-id))
  (loop for (k v) on args by #'cddr
        unless (find k '(:width :height :title :monitor :share))
        do (with-simple-restart (continue "Ignore the window hint.")
             (if (stringp v)
                 (glfw window-hint-string k v)
                 (glfw window-hint k v))))
  (let ((pointer (glfw create-window
                       (width window)
                       (height window)
                       (title window)
                       (if monitor (pointer monitor) (cffi:null-pointer))
                       (if share (pointer share) (cffi:null-pointer))))
        ok)
    (setf (pointer window) pointer)
    (unwind-protect
         (progn
           (setf (ptr-object pointer) window)
           (register-callbacks window)
           (setf ok T))
      (unless ok
        (destroy window)))))

(defmethod print-object ((window window) stream)
  (print-unreadable-object (window stream :type T)
    (if (pointer window)
        (format stream "~dx~d ~a @~8,'0x"
                (width window) (height window) (state window)
                (cffi:pointer-address (pointer window)))
        (format stream "DEAD"))))

(defmethod register-callbacks ((window window))
  (let ((pointer (pointer window)))
    (glfw:set-error-callback (cffi:callback glfw:error))
    (glfw:set-monitor-callback (cffi:callback glfw:monitor))
    (glfw:set-joystick-callback (cffi:callback glfw:joystick))
    (glfw:set-window-pos-callback pointer (cffi:callback glfw:window-moved))
    (glfw:set-window-size-callback pointer (cffi:callback glfw:window-resized))
    (glfw:set-window-close-callback pointer (cffi:callback glfw:window-closed))
    (glfw:set-window-refresh-callback pointer (cffi:callback glfw:window-refreshed))
    (glfw:set-window-focus-callback pointer (cffi:callback glfw:window-focused))
    (glfw:set-window-iconify-callback pointer (cffi:callback glfw:window-iconified))
    (glfw:set-window-maximize-callback pointer (cffi:callback glfw:window-maximized))
    (glfw:set-framebuffer-size-callback pointer (cffi:callback glfw:framebuffer-resized))
    (glfw:set-window-content-scale-callback pointer (cffi:callback glfw:window-content-scale-changed))
    (glfw:set-key-callback pointer (cffi:callback glfw:key-changed))
    (glfw:set-char-mods-callback pointer (cffi:callback glfw:char-entered))
    (glfw:set-mouse-button-callback pointer (cffi:callback glfw:mouse-button-changed))
    (glfw:set-cursor-pos-callback pointer (cffi:callback glfw:mouse-moved))
    (glfw:set-cursor-enter-callback pointer (cffi:callback glfw:mouse-entered))
    (glfw:set-scroll-callback pointer (cffi:callback glfw:mouse-scrolled))
    (glfw:set-drop-callback pointer (cffi:callback glfw:file-dropped))))

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

(defmethod window-resized :before ((window window) width height)
  (setf (slot-value window 'width) width)
  (setf (slot-value window 'height) height))

(defmethod window-closed ((window window))
  (glfw:set-window-should-close (pointer window) T))

(defmethod window-moved ((window window) xpos ypos))
(defmethod window-resized ((window window) width height))
(defmethod window-refreshed ((window window)))
(defmethod window-focused ((window window) focused))
(defmethod window-iconified ((window window) iconified))
(defmethod window-maximized ((window window) maximized))
(defmethod framebuffer-resized ((window window) width height))
(defmethod window-content-scale-changed ((window window) xscale yscale))
(defmethod mouse-button-changed ((window window) button action modifiers))
(defmethod mouse-moved ((window window) xpos ypos))
(defmethod mouse-entered ((window window) entered))
(defmethod mouse-scrolled ((window window) xoffset yoffset))
(defmethod key-changed ((window window) key scan-code action modifiers))
(defmethod char-entered ((window window) code-point modifiers))
(defmethod file-dropped ((window window) paths))

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

(defmethod iconified-p ((window window))
  (< 0 (glfw get-window-attrib (pointer window) :iconified)))

(defmethod (setf iconified-p) (value (window window))
  (if value
      (glfw iconify-window (pointer window))
      (glfw restore-window (pointer window)))
  value)

(defmethod maximized-p ((window window))
  (< 0 (glfw get-window-attrib (pointer window) :maximized)))

(defmethod (setf maximized-p) (value (window window))
  (if value
      (glfw maximize-window (pointer window))
      (glfw restore-window (pointer window)))
  value)

(defmethod visible-p ((window window))
  (< 0 (glfw get-window-attrib (pointer window) :visible)))

(defmethod (setf visible-p) (value (window window))
  (if value
      (glfw show-window (pointer window))
      (glfw hide-window (pointer window)))
  value)

(defmethod state ((window window))
  (cond ((iconified-p window)
         :iconified)
        ((not (visible-p window))
         :hidden)
        ((maximized-p window)
         :maximized)
        (T
         :normal)))

(defmethod (setf state) (state (window window))
  (ecase state
    (:iconified
     (iconify window))
    (:hidden
     (hide window))
    (:maximized
     (maximize window))
    (:normal
     (show window)
     (restore window))))

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
  (glfw set-window-monitor (pointer window) (pointer monitor) 0 0 (width monitor) (height monitor) (refresh-rate monitor))
  monitor)

(defmethod (setf monitor) ((monitor cons) (window window))
  (destructuring-bind (monitor &key width height (x 0) (y 0) refresh-rate) monitor
    (when (eql T monitor)
      (setf monitor (primary-monitor)))
    (glfw set-window-monitor (pointer window) (pointer monitor)
          x y (or width (width monitor)) (or height (height monitor)) (or refresh-rate (refresh-rate monitor))))
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

(defun version ()
  (extract-values ((major :int) (minor :int) (rev :int))
    (glfw:get-version major minor rev)))

(defun platform ()
  (glfw:get-platform))

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

(defmethod cursor ((window window))
  (let ((cursor (slot-value window 'cursor)))
    (unless cursor
      (setf cursor (cursor :arrow))
      (setf (slot-value window 'cursor) cursor))
    cursor))

(defmethod (setf cursor) ((cursor cursor) (window window))
  (glfw set-cursor (pointer window) (pointer cursor))
  (setf (slot-value window 'cursor) cursor))

(defmethod (setf cursor) (thing (window window))
  (setf (cursor window) (cursor thing)))

(defmethod (setf icon) :before (icons (window window))
  (cffi:with-foreign-objects ((images '(:struct glfw:image) (length icons))
                              (pixel-data :char (loop for (pixels) in icons sum (length pixels))))
    (loop with p = 0
          for i from 0
          for (pixels width height) in icons
          for image = (cffi:mem-aptr images '(:struct glfw:image) i)
          do (setf (glfw:image-width image) width)
             (setf (glfw:image-height image) height)
             (setf (glfw:image-pixels image) (cffi:inc-pointer pixel-data p))
             (loop for pixel across pixels
                   do (setf (cffi:mem-aref pixel-data :char p) pixel)
                      (incf p)))
    (glfw set-window-icon (pointer window) (length icons) images)))
