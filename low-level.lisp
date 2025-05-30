(in-package #:org.shirakumo.fraf.glfw.cffi)

(defvar *here* #.(or *compile-file-pathname* *load-pathname* *default-pathname-defaults*))
(defvar *static* (make-pathname :name NIL :type NIL :defaults (merge-pathnames "static/" *here*)))
(pushnew *static* cffi:*foreign-library-directories*)

(cffi:define-foreign-library libglfw
  (:darwin (:or #+X86 "libglfw-mac-i686.dylib"
                #+X86-64 "libglfw-mac-amd64.dylib"
                #+ARM64 "libglfw-mac-arm64.dylib"))
  (:unix (:or #+X86 "libglfw-lin-i686.so"
              #+X86-64 "libglfw-lin-amd64.so"
              #+ARM64 "libglfw-lin-arm64.so"))
  (:windows (:or #+X86 "libglfw-win-i686.dll"
                 #+X86-64 "libglfw-win-amd64.dll"
                 #+ARM64 "libglfw-win-arm64.so"))
  (T (:or (:default "libglfw3") (:default "glfw3") (:default "libglfw") (:default "glfw"))))

(cffi:defcenum key-state
  (:release            0)
  (:press              1)
  (:repeat             2))

(cffi:defbitfield hat-state
  (:centered           #x00)
  (:up                 #x01)
  (:right              #x02)
  (:down               #x04)
  (:left               #x08))

(cffi:defcenum (mouse-button :int :allow-undeclared-values T)
  (:left               0)
  (:right              1)
  (:middle             2)
  (:button-4           3)
  (:button-5           4)
  (:button-6           5)
  (:button-7           6)
  (:button-8           7))

(cffi:defcenum (key :int :allow-undeclared-values T)
  (:unknown            -1)
  (:space              32)
  (:apostrophe         39)
  (:comma              44)
  (:minus              45)
  (:period             46)
  (:slash              47)
  (:0                  48)
  (:1                  49)
  (:2                  50)
  (:3                  51)
  (:4                  52)
  (:5                  53)
  (:6                  54)
  (:7                  55)
  (:8                  56)
  (:9                  57)
  (:semicolon          59)
  (:equal              61)
  (:a                  65)
  (:b                  66)
  (:c                  67)
  (:d                  68)
  (:e                  69)
  (:f                  70)
  (:g                  71)
  (:h                  72)
  (:i                  73)
  (:j                  74)
  (:k                  75)
  (:l                  76)
  (:m                  77)
  (:n                  78)
  (:o                  79)
  (:p                  80)
  (:q                  81)
  (:r                  82)
  (:s                  83)
  (:t                  84)
  (:u                  85)
  (:v                  86)
  (:w                  87)
  (:x                  88)
  (:y                  89)
  (:z                  90)
  (:left-bracket       91)
  (:backslash          92)
  (:right-bracket      93)
  (:grave-accent       96)
  (:world-1            16)
  (:world-2            16)
  (:escape             256)
  (:enter              257)
  (:tab                258)
  (:backspace          259)
  (:insert             260)
  (:delete             261)
  (:right              262)
  (:left               263)
  (:down               264)
  (:up                 265)
  (:page-up            266)
  (:page-down          267)
  (:home               268)
  (:end                269)
  (:caps-lock          280)
  (:scroll-lock        281)
  (:num-lock           282)
  (:print-screen       283)
  (:pause              284)
  (:f1                 290)
  (:f2                 291)
  (:f3                 292)
  (:f4                 293)
  (:f5                 294)
  (:f6                 295)
  (:f7                 296)
  (:f8                 297)
  (:f9                 298)
  (:f10                299)
  (:f11                300)
  (:f12                301)
  (:f13                302)
  (:f14                303)
  (:f15                304)
  (:f16                305)
  (:f17                306)
  (:f18                307)
  (:f19                308)
  (:f20                309)
  (:f21                310)
  (:f22                311)
  (:f23                312)
  (:f24                313)
  (:f25                314)
  (:kp-0               320)
  (:kp-1               321)
  (:kp-2               322)
  (:kp-3               323)
  (:kp-4               324)
  (:kp-5               325)
  (:kp-6               326)
  (:kp-7               327)
  (:kp-8               328)
  (:kp-9               329)
  (:kp-decimal         330)
  (:kp-divide          331)
  (:kp-multiply        332)
  (:kp-subtract        333)
  (:kp-add             334)
  (:kp-enter           335)
  (:kp-equal           336)
  (:left-shift         340)
  (:left-control       341)
  (:left-alt           342)
  (:left-super         343)
  (:right-shift        344)
  (:right-control      345)
  (:right-alt          346)
  (:right-super        347)
  (:menu               348))

(cffi:defbitfield modifier
  (:shift              #x01)
  (:control            #x02)
  (:alt                #x04)
  (:super              #x08)
  (:caps-lock          #x10)
  (:num-lock           #x20))

(cffi:defcenum gamepad-button
  (:a                  0)
  (:b                  1)
  (:x                  2)
  (:y                  3)
  (:l1                 4)
  (:r1                 5)
  (:select             6)
  (:start              7)
  (:home               8)
  (:l3                 9)
  (:r3                 10)
  (:dpad-up            11)
  (:dpad-right         12)
  (:dpad-down          13)
  (:dpad-left          14))

(cffi:defcenum gamepad-axis
  (:l-h                0)
  (:l-v                1)
  (:r-h                2)
  (:r-v                3)
  (:l2                 4)
  (:r2                 5))

(cffi:defcenum (error :int :allow-undeclared-values T)
  (:no-error               0)
  (:not-initialized        #x00010001)
  (:no-current-context     #x00010002)
  (:invalid-enum           #x00010003)
  (:invalid-value          #x00010004)
  (:out-of-memory          #x00010005)
  (:api-unavailable        #x00010006)
  (:version-unavailable    #x00010007)
  (:platform-error         #x00010008)
  (:format-unavailable     #x00010009)
  (:no-window-context      #x0001000a)
  (:cursor-unavailable     #x0001000b)
  (:feature-unavailable    #x0001000c)
  (:feature-unimplemented  #x0001000d)
  (:platform-unavailable   #x0001000e))

(cffi:defcenum (flag :int :allow-undeclared-values T)
  ;; (NIL                               0)
  ;; (T                                 1)
  (:focused                 #x00020001)
  (:iconified               #x00020002)
  (:resizable               #x00020003)
  (:visible                 #x00020004)
  (:decorated               #x00020005)
  (:auto-iconify            #x00020006)
  (:floating                #x00020007)
  (:maximized               #x00020008)
  (:center-cursor           #x00020009)
  (:transparent-framebuffer #x0002000a)
  (:hovered                 #x0002000b)
  (:focus-on-show           #x0002000c)

  (:mouse-passthrough      #x0002000d)

  (:position-x             #x0002000e)
  (:position-y             #x0002000f)

  (:red-bits               #x00021001)
  (:green-bits             #x00021002)
  (:blue-bits              #x00021003)
  (:alpha-bits             #x00021004)
  (:depth-bits             #x00021005)
  (:stencil-bits           #x00021006)
  (:accum-red-bits         #x00021007)
  (:accum-green-bits       #x00021008)
  (:accum-blue-bits        #x00021009)
  (:accum-alpha-bits       #x0002100a)
  (:aux-buffers            #x0002100b)
  (:stereo                 #x0002100c)
  (:samples                #x0002100d)
  (:srgb-capable           #x0002100e)
  (:refresh-rate           #x0002100f)
  (:doublebuffer           #x00021010)

  (:client-api               #x00022001)
  (:context-version-major    #x00022002)
  (:context-version-minor    #x00022003)
  (:context-revision         #x00022004)
  (:context-robustness       #x00022005)
  (:opengl-forward-compat    #x00022006)
  (:context-debug            #x00022007)
  (:opengl-debug-context     #x00022007)
  (:opengl-profile           #x00022008)
  (:context-release-behavior #x00022009)
  (:context-no-error         #x0002200a)
  (:context-creation-api     #x0002200b)
  (:scale-to-monitor         #x0002200c)
  (:cocoa-retina-framebuffer #x00023001)
  (:cocoa-frame-name         #x00023002)
  (:cocoa-graphics-switching #x00023003)
  (:x11-class-name           #x00024001)
  (:x11-instance-name        #x00024002)
  (:win32-keyboard-menu      #x00025001)
  (:wayland-app-id           #x00026001)

  (:no-api                          0)
  (:opengl-api             #x00030001)
  (:opengl-es-api          #x00030002)

  (:no-robustness                   0)
  (:no-reset-notification  #x00031001)
  (:lose-context-on-reset  #x00031002)

  (:opengl-any-profile              0)
  (:opengl-core-profile    #x00032001)
  (:opengl-compat-profile  #x00032002)

  (:cursor                 #x00033001)
  (:sticky-keys            #x00033002)
  (:sticky-mouse-buttons   #x00033003)
  (:lock-key-mods          #x00033004)
  (:raw-mouse-motion       #x00033005)

  (:cursor-normal          #x00034001)
  (:cursor-hidden          #x00034002)
  (:cursor-disabled        #x00034003)
  (:cursor-captured        #x00034004)

  (:any-release-behavior            0)
  (:release-behavior-flush #x00035001)
  (:release-behavior-none  #x00035002)

  (:native-context-api     #x00036001)
  (:egl-context-api        #x00036002)
  (:osmesa-context-api     #x00036003)

  (:angle-platform-type-none     #x00037001)
  (:angle-platform-type-opengl   #x00037002)
  (:angle-platform-type-opengles #x00037003)
  (:angle-platform-type-d3d9     #x00037004)
  (:angle-platform-type-d3d11    #x00037005)
  (:angle-platform-type-vulkan   #x00037007)
  (:angle-platform-type-metal    #x00037008)

  (:wayland-prefer-libdecor    #x00038001)
  (:wayland-disable-libdecor   #x00038002)

  (:any-position           #x80000000)

  (:arrow-cursor           #x00036001)
  (:ibeam-cursor           #x00036002)
  (:crosshair-cursor       #x00036003)
  (:pointing-hand-cursor   #x00036004)
  (:resize-ew-cursor       #x00036005)
  (:resize-ns-cursor       #x00036006)
  (:resize-nwse-cursor     #x00036007)
  (:resize-nesw-cursor     #x00036008)
  (:resize-all-cursor      #x00036009)
  (:not-allowed-cursor     #x0003600a)

  (:connected              #x00040001)
  (:disconnected           #x00040002)

  (:joystick-hat-buttons   #x00050001)
  (:angle-platform-type    #x00050002)
  (:platform               #x00050003)
  (:cocoa-chdir-resources  #x00051001)
  (:cocoa-menubar          #x00051002)
  (:x11-xcb-vulkan-surface #x00052001)
  (:wayland-libdecor       #x00053001)
  (:any-platform           #x00060000)
  (:win32                  #x00060001)
  (:cocoa                  #x00060002)
  (:wayland                #x00060003)
  (:x11                    #x00060004)
  (:null-platform          #x00060005)

  (:dont-care              -1))

(cffi:defcenum joystick-event
  (:connected              #x00040001)
  (:disconnected           #x00040002))

(cffi:defcenum monitor-event
  (:connected              #x00040001)
  (:disconnected           #x00040002))

(cffi:defcenum cursor
  (:arrow           #x00036001)
  (:ibeam           #x00036002)
  (:crosshair       #x00036003)
  (:pointing-hand   #x00036004)
  (:resize-ew       #x00036005)
  (:resize-ns       #x00036006)
  (:resize-nwse     #x00036007)
  (:resize-nesw     #x00036008)
  (:resize-all      #x00036009)
  (:not-allowed     #x0003600a))

(cffi:defcstruct (video-mode :conc-name video-mode-)
  (width :int)
  (height :int)
  (red-bits :int)
  (green-bits :int)
  (blue-bits :int)
  (refresh-rate :int))

(cffi:defcstruct (gamma-ramp :conc-name gamma-ramp-)
  (red :pointer)
  (green :pointer)
  (blue :pointer)
  (size :uint))

(cffi:defcstruct (image :conc-name image-)
  (width :int)
  (height :int)
  (pixels :pointer))

(cffi:defcstruct (gamepad-state :conc-name gamepad-state-)
  (buttons :uchar :count 15)
  (axes :float :count 6))

(cffi:defcstruct (allocator :conc-name allocator-)
  (allocate :pointer)
  (reallocate :pointer)
  (deallocate :pointer)
  (user :pointer))

;;; Callbacks
(declaim (ftype (function (t) (values t &optional nil))
                resolve-window))

(defmacro defglfwcallback (name return args)
  `(progn
     (defgeneric ,name ,(mapcar #'first args))

     (cffi:defcallback ,name ,return ,args
       (restart-case
           (let ((window-ptr window)
                 (window (resolve-window window)))
             (if window
                 (,name ,@(mapcar #'first args))
                 (format *error-output* "~&[GLFW] ~a callback to unmapped window pointer ~8,'0x, ignoring.~%"
                         ',name (cffi:pointer-address window-ptr))))
         (abort ()
           :report "Abort the callback."
           ,(case return
              (:void NIL)
              (:pointer '(cffi:null-pointer))
              (T 0)))))))

(defun error (code description))

(cffi:defcallback error :void ((code :int) (description :string))
  (error code description))

(defun monitor-connected (monitor))
(defun monitor-disconnected (monitor))

(cffi:defcallback monitor :void ((monitor :pointer) (event monitor-event))
  (case event
    (:connected (monitor-connected monitor))
    (:disconnected (monitor-disconnected monitor))))

(defun joystick-connected (id))
(defun joystick-disconnected (id))

(cffi:defcallback joystick :void ((id :int) (event joystick-event))
  (case event
    (:connected (joystick-connected id))
    (:disconnected (joystick-disconnected id))))

(defglfwcallback allocate :pointer ((size :size) (window :pointer)))
(defglfwcallback reallocate :pointer ((ptr :pointer) (size :size) (window :pointer)))
(defglfwcallback deallocate :void ((ptr :pointer) (size :size) (window :pointer)))
(defglfwcallback window-moved :void ((window :pointer) (xpos :int) (ypos :int)))
(defglfwcallback window-resized :void ((window :pointer) (width :int) (height :int)))
(defglfwcallback window-closed :void ((window :pointer)))
(defglfwcallback window-refreshed :void ((window :pointer)))
(defglfwcallback window-focused :void ((window :pointer) (focused :bool)))
(defglfwcallback window-iconified :void ((window :pointer) (iconified :bool)))
(defglfwcallback window-maximized :void ((window :pointer) (maximized :bool)))
(defglfwcallback framebuffer-resized :void ((window :pointer) (width :int) (height :int)))
(defglfwcallback window-content-scale-changed :void ((window :pointer) (xscale :float) (yscale :float)))
(defglfwcallback mouse-button-changed :void ((window :pointer) (button mouse-button) (action key-state) (modifiers modifier)))
(defglfwcallback mouse-moved :void ((window :pointer) (xpos :double) (ypos :double)))
(defglfwcallback mouse-entered :void ((window :pointer) (entered :bool)))
(defglfwcallback mouse-scrolled :void ((window :pointer) (xoffset :double) (yoffset :double)))
(defglfwcallback key-changed :void ((window :pointer) (key key) (scan-code :int) (action key-state) (modifiers modifier)))
(defglfwcallback char-entered :void ((window :pointer) (code-point :uint)))

(defgeneric file-dropped (window paths))
(cffi:defcallback file-dropped :void ((window :pointer) (path-count :int) (paths :pointer))
  (restart-case (let ((window-ptr window) (window (resolve-window window)))
                  (if window
                      ;; Special handling to decode the paths
                      (file-dropped window (loop for i from 0 below path-count
                                                 collect (cffi:foreign-string-to-lisp (cffi:mem-aref paths :pointer i) :encoding :utf-8)))
                      (format *error-output*
                              "~&[GLFW] ~a callback to unmapped window pointer ~8,'0x, ignoring.~%"
                              'file-dropped (cffi-sys:pointer-address window-ptr))))
    (abort ()
      :report "Abort the callback."
      NIL)))

(defgeneric debug-log (window source type id severity message))
(cffi:defcallback debug-log :void ((source %gl::enum) (type %gl::enum) (id :uint) (severity %gl::enum)
                                   (length :size) (message :pointer) (window :pointer))
  (restart-case (let ((window-ptr window) (window (resolve-window window)))
                  (if window
                      ;; Special handling to decode the message
                      (debug-log window
                                 (case source
                                   (:debug-source-api-khr :api)
                                   (:debug-source-window-system-khr :window-system)
                                   (:debug-source-shader-compiler-khr :shader-compiler)
                                   (:debug-source-third-party-khr :third-party)
                                   (:debug-source-application-khr :application)
                                   (:debug-source-other-khr :other)
                                   (T source))
                                 (case type
                                   (:debug-type-error-khr :error)
                                   (:debug-type-deprecated-behavior-khr :deprecated-behavior)
                                   (:debug-type-undefined-behavior-khr :undefined-behavior)
                                   (:debug-type-portability-khr :portability)
                                   (:debug-type-performance-khr :performance)
                                   (:debug-type-other-khr :other)
                                   (:debug-type-marker-khr :marker)
                                   (T type))
                                 id
                                 (case severity
                                   (:debug-severity-high-khr :high)
                                   (:debug-severity-medium-khr :medium)
                                   (:debug-severity-low-khr :low)
                                   (:debug-severity-notification-khr :notification)
                                   (T severity))
                                 (cffi:foreign-string-to-lisp message :count length))
                      (format *error-output*
                              "~&[GLFW] ~a callback to unmapped window pointer ~8,'0x, ignoring.~%"
                              'file-dropped (cffi-sys:pointer-address window-ptr))))
    (abort ()
      :report "Abort the callback."
      NIL)))

;;; Main interface
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun translate-name (name)
    (with-output-to-string (out)
      (loop with was-uppercase = T
            for char across name
            do (cond ((or (digit-char-p char)
                          (lower-case-p char))
                      (setf was-uppercase NIL))
                     ((not was-uppercase)
                      (setf was-uppercase T)
                      (write-char #\- out)))
               (write-char (char-upcase char) out)))))

(defmacro defglfwfun (name return args)
  (let ((fun (intern (translate-name (subseq name (length "glfw"))))))
    `(progn
       (declaim (inline ,fun))
       (cffi:defcfun (,fun ,name) ,return ,@args))))

(defglfwfun "glfwInit" :bool ())
(defglfwfun "glfwTerminate" :void ())
(defglfwfun "glfwInitHint" :void ((hint flag) (value flag)))
(defglfwfun "glfwInitAllocator" :void ((allocator :pointer)))
(defglfwfun "glfwInitVulkanLoader" :void ((loader :pointer)))
(defglfwfun "glfwGetVersion" :void ((major :pointer) (minor :pointer) (rev :pointer)))
(defglfwfun "glfwGetVersionString" :string ())
(defglfwfun "glfwGetError" error ((description :pointer)))
(defglfwfun "glfwGetPlatform" flag ())
(defglfwfun "glfwPlatformSupported" :bool ((platform flag)))

(defglfwfun "glfwGetMonitors" :pointer ((count :pointer)))
(defglfwfun "glfwGetPrimaryMonitor" :pointer ())
(defglfwfun "glfwGetMonitorPos" :void ((monitor :pointer) (xpos :pointer) (ypos :pointer)))
(defglfwfun "glfwGetMonitorWorkarea" :void ((monitor :pointer) (xpos :pointer) (ypos :pointer) (width :pointer) (height :pointer)))
(defglfwfun "glfwGetMonitorPhysicalSize" :void ((monitor :pointer) (widthMM :pointer) (heightMM :pointer)))
(defglfwfun "glfwGetMonitorContentScale" :void ((monitor :pointer) (xscale :pointer) (yscale :pointer)))
(defglfwfun "glfwGetMonitorName" :string ((monitor :pointer)))
(defglfwfun "glfwSetMonitorUserPointer" :void ((monitor :pointer) (pointer :pointer)))
(defglfwfun "glfwGetMonitorUserPointer" :pointer ((monitor :pointer)))
(defglfwfun "glfwGetVideoModes" :pointer ((monitor :pointer) (count :pointer)))
(defglfwfun "glfwGetVideoMode" :pointer ((monitor :pointer)))
(defglfwfun "glfwSetGamma" :void ((monitor :pointer) (gamma :float)))
(defglfwfun "glfwGetGammaRamp" :pointer ((monitor :pointer)))
(defglfwfun "glfwSetGammaRamp" :void ((monitor :pointer) (ramp :pointer)))

(defglfwfun "glfwDefaultWindowHints" :void ())
(defglfwfun "glfwWindowHint" :void ((hint flag) (value flag)))
(defglfwfun "glfwWindowHintString" :void ((hint flag) (value :string)))
(defglfwfun "glfwCreateWindow" :pointer ((width :int) (height :int) (title :string) (monitor :pointer) (share :pointer)))
(defglfwfun "glfwDestroyWindow" :void ((window :pointer)))
(defglfwfun "glfwWindowShouldClose" :bool ((window :pointer)))
(defglfwfun "glfwSetWindowShouldClose" :void ((window :pointer) (value :bool)))
(defglfwfun "glfwSetWindowTitle" :void ((window :pointer) (title :string)))
(defglfwfun "glfwSetWindowIcon" :void ((window :pointer) (count :int) (images :pointer)))
(defglfwfun "glfwGetWindowPos" :void ((window :pointer) (xpos :pointer) (ypos :pointer)))
(defglfwfun "glfwSetWindowPos" :void ((window :pointer) (xpos :int) (ypos :int)))
(defglfwfun "glfwGetWindowSize" :void ((window :pointer) (width :pointer) (height :pointer)))
(defglfwfun "glfwSetWindowSize" :void ((window :pointer) (width :int) (height :int)))
(defglfwfun "glfwSetWindowSizeLimits" :void ((window :pointer) (minwidth :int) (minheight :int) (maxwidth :int) (maxheight :int)))
(defglfwfun "glfwSetWindowAspectRatio" :void ((window :pointer) (numer :int) (denom :int)))
(defglfwfun "glfwGetFramebufferSize" :void ((window :pointer) (width :pointer) (height :pointer)))
(defglfwfun "glfwGetWindowFrameSize" :void ((window :pointer) (left :pointer) (top :pointer) (right :pointer) (bottom :pointer)))
(defglfwfun "glfwGetWindowContentScale" :void ((window :pointer) (xscale :pointer) (yscale :pointer)))
(defglfwfun "glfwGetWindowOpacity" :float ((window :pointer)))
(defglfwfun "glfwSetWindowOpacity" :void ((window :pointer) (opacity :float)))
(defglfwfun "glfwIconifyWindow" :void ((window :pointer)))
(defglfwfun "glfwRestoreWindow" :void ((window :pointer)))
(defglfwfun "glfwMaximizeWindow" :void ((window :pointer)))
(defglfwfun "glfwShowWindow" :void ((window :pointer)))
(defglfwfun "glfwHideWindow" :void ((window :pointer)))
(defglfwfun "glfwFocusWindow" :void ((window :pointer)))
(defglfwfun "glfwRequestWindowAttention" :void ((window :pointer)))
(defglfwfun "glfwGetWindowMonitor" :pointer ((window :pointer)))
(defglfwfun "glfwSetWindowMonitor" :void ((window :pointer) (monitor :pointer) (xpos :int) (ypos :int) (width :int) (height :int) (refreshRate :int)))
(defglfwfun "glfwGetWindowAttrib" :int ((window :pointer) (attrib flag)))
(defglfwfun "glfwSetWindowAttrib" :void ((window :pointer) (attrib flag) (value flag)))
(defglfwfun "glfwSetWindowUserPointer" :void ((window :pointer) (pointer :pointer)))
(defglfwfun "glfwGetWindowUserPointer" :pointer ((window :pointer)))
(defglfwfun "glfwPollEvents" :void ())
(defglfwfun "glfwWaitEvents" :void ())
(defglfwfun "glfwWaitEventsTimeout" :void ((timeout :double)))
(defglfwfun "glfwPostEmptyEvent" :void ())
(defglfwfun "glfwGetInputMode" :int ((window :pointer) (mode flag)))
(defglfwfun "glfwSetInputMode" :void ((window :pointer) (mode flag) (value flag)))
(defglfwfun "glfwRawMouseMotionSupported" :bool ())
(defglfwfun "glfwGetKeyName" :string ((key key) (scancode :int)))
(defglfwfun "glfwGetKeyScancode" :int ((key key)))
(defglfwfun "glfwGetKey" key-state ((window :pointer) (key key)))
(defglfwfun "glfwGetMouseButton" key-state ((window :pointer) (button mouse-button)))
(defglfwfun "glfwGetCursorPos" :void ((window :pointer) (xpos :pointer) (ypos :pointer)))
(defglfwfun "glfwSetCursorPos" :void ((window :pointer) (xpos :double) (ypos :double)))

(defglfwfun "glfwCreateCursor" :pointer ((image :pointer) (xhot :int) (yhot :int)))
(defglfwfun "glfwCreateStandardCursor" :pointer ((shape cursor)))
(defglfwfun "glfwDestroyCursor" :void ((cursor :pointer)))
(defglfwfun "glfwSetCursor" :void ((window :pointer) (cursor :pointer)))
(defglfwfun "glfwJoystickPresent" :bool ((jid :int)))
(defglfwfun "glfwGetJoystickAxes" :pointer ((jid :int) (count :pointer)))
(defglfwfun "glfwGetJoystickButtons" :pointer ((jid :int) (count :pointer)))
(defglfwfun "glfwGetJoystickHats" :pointer ((jid :int) (count :pointer)))
(defglfwfun "glfwGetJoystickName" :string ((jid :int)))
(defglfwfun "glfwGetJoystickGUID" :string ((jid :int)))
(defglfwfun "glfwSetJoystickUserPointer" :void ((jid :int) (pointer :pointer)))
(defglfwfun "glfwGetJoystickUserPointer" :pointer ((jid :int)))
(defglfwfun "glfwJoystickIsGamepad" :bool ((jid :int)))
(defglfwfun "glfwUpdateGamepadMappings" :bool ((string :string)))
(defglfwfun "glfwGetGamepadName" :string ((jid :int)))
(defglfwfun "glfwGetGamepadState" :bool ((jid :int) (state :pointer)))

(defglfwfun "glfwSetClipboardString" :void ((window :pointer) (string :string)))
(defglfwfun "glfwGetClipboardString" :string ((window :pointer)))
(defglfwfun "glfwGetTime" :double ())
(defglfwfun "glfwSetTime" :void ((time :double)))
(defglfwfun "glfwGetTimerValue" :uint64 ())
(defglfwfun "glfwGetTimerFrequency" :uint64 ())
(defglfwfun "glfwMakeContextCurrent" :void ((window :pointer)))
(defglfwfun "glfwGetCurrentContext" :pointer ())
(defglfwfun "glfwSwapBuffers" :void ((window :pointer)))
(defglfwfun "glfwSwapInterval" :void ((interval :int)))
(defglfwfun "glfwExtensionSupported" :bool ((extension :string)))
(defglfwfun "glfwGetProcAddress" :pointer ((procname :string)))
(defglfwfun "glfwVulkanSupported" :bool ())
(defglfwfun "glfwGetRequiredInstanceExtensions" :pointer ((count :pointer)))
(defglfwfun "glfwGetInstanceProcAddress" :pointer ((instance :pointer) (procname :string)))
(defglfwfun "glfwGetPhysicalDevicePresentationSupport" :bool ((instance :pointer) (device :pointer) (queuefamily :uint32)))
(defglfwfun "glfwCreateWindowSurface" :int ((instance :pointer) (window :pointer) (allocator :pointer) (surface :pointer)))

(defglfwfun "glfwSetErrorCallback" :pointer ((callback :pointer)))
(defglfwfun "glfwSetMonitorCallback" :pointer ((callback :pointer)))
(defglfwfun "glfwSetWindowPosCallback" :pointer ((window :pointer) (callback :pointer)))
(defglfwfun "glfwSetWindowSizeCallback" :pointer ((window :pointer) (callback :pointer)))
(defglfwfun "glfwSetWindowCloseCallback" :pointer ((window :pointer) (callback :pointer)))
(defglfwfun "glfwSetWindowRefreshCallback" :pointer ((window :pointer) (callback :pointer)))
(defglfwfun "glfwSetWindowFocusCallback" :pointer ((window :pointer) (callback :pointer)))
(defglfwfun "glfwSetWindowIconifyCallback" :pointer ((window :pointer) (callback :pointer)))
(defglfwfun "glfwSetWindowMaximizeCallback" :pointer ((window :pointer) (callback :pointer)))
(defglfwfun "glfwSetFramebufferSizeCallback" :pointer ((window :pointer) (callback :pointer)))
(defglfwfun "glfwSetWindowContentScaleCallback" :pointer ((window :pointer) (callback :pointer)))
(defglfwfun "glfwSetKeyCallback" :pointer ((window :pointer) (callback :pointer)))
(defglfwfun "glfwSetCharCallback" :pointer ((window :pointer) (callback :pointer)))
(defglfwfun "glfwSetCharModsCallback" :pointer ((window :pointer) (callback :pointer))) ;; DEPRECATED: upstream deprecation https://www.glfw.org/docs/3.3/deprecated.html
(defglfwfun "glfwSetMouseButtonCallback" :pointer ((window :pointer) (callback :pointer)))
(defglfwfun "glfwSetCursorPosCallback" :pointer ((window :pointer) (callback :pointer)))
(defglfwfun "glfwSetCursorEnterCallback" :pointer ((window :pointer) (callback :pointer)))
(defglfwfun "glfwSetScrollCallback" :pointer ((window :pointer) (callback :pointer)))
(defglfwfun "glfwSetDropCallback" :pointer ((window :pointer) (callback :pointer)))
(defglfwfun "glfwSetJoystickCallback" :pointer ((callback :pointer)))

;;; Native interface
(defglfwfun "glfwGetWin32Adapter" :string ((monitor :pointer)))
(defglfwfun "glfwGetWin32Monitor" :string ((monitor :pointer)))
(defglfwfun "glfwGetWin32Window" :pointer ((window :pointer)))
(defglfwfun "glfwGetWGLContext" :pointer ((window :pointer)))
(defglfwfun "glfwGetCocoaMonitor" :uint32 ((monitor :pointer)))
(defglfwfun "glfwGetCocoaWindow" :pointer ((window :pointer)))
(defglfwfun "glfwGetNSGLContext" :pointer ((window :pointer)))
(defglfwfun "glfwGetX11Display" :pointer ())
(defglfwfun "glfwGetX11Adapter" :ulong ((monitor :pointer)))
(defglfwfun "glfwGetX11Monitor" :ulong ((monitor :pointer)))
(defglfwfun "glfwGetX11Window" :ulong ((window :pointer)))
(defglfwfun "glfwSetX11SelectionString" :void ((string :string)))
(defglfwfun "glfwGetX11SelectionString" :string ())
(defglfwfun "glfwGetGLXContext" :pointer ((window :pointer)))
(defglfwfun "glfwGetGLXWindow" :ulong ((window :pointer)))
(defglfwfun "glfwGetWaylandDisplay" :pointer ())
(defglfwfun "glfwGetWaylandMonitor" :pointer ((monitor :pointer)))
(defglfwfun "glfwGetWaylandWindow" :pointer ((window :pointer)))
(defglfwfun "glfwGetEGLDisplay" :pointer ())
(defglfwfun "glfwGetEGLContext" :pointer ((window :pointer)))
(defglfwfun "glfwGetEGLSurface" :pointer ((window :pointer)))
(defglfwfun "glfwGetOSMesaColorBuffer" :int ((window :pointer) (width :pointer) (height :pointer) (format :pointer) (buffer :pointer)))
(defglfwfun "glfwGetOSMesaDepthBuffer" :int ((window :pointer) (width :pointer) (height :pointer) (bytesPerValue :pointer) (buffer :pointer)))
(defglfwfun "glfwGetOSMesaContext" :pointer ((window :pointer)))
