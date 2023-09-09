(in-package #:org.shirakumo.fraf.glfw)

(docs:define-docs
  (type glfw-error
    "Error signalled when GLFW fails to execute an operation.

See OPERATION
See CODE
See MESSAGE")
  
  (function operation
    "Returns the operation that failed to execute.

See GLFW-ERROR")
  
  (function code
    "Returns the error code that was returned.

See GLFW-ERROR")
  
  (function message
    "Returns the error message that was returned.

See GLFW-ERROR")
  
  (function init
    "Initialise the library.

This will load libglfw and initialize it. You may pass keyword
arguments to control the library initialization. You can find more
information on the initialization options and their effects in the
GLFW documentation:

https://www.glfw.org/docs/latest/intro_guide.html#init_hints

It is safe to call this function multiple times. Once you are done
with the library, you should call SHUTDOWN to clean things up.

Calling any other function in the library before INIT is called will
lead to undefined behaviour.

See SHUTDOWN")
  
  (function shutdown
    "Shuts the library down.

This will clean up all the windows and other objects, and call GLFW's
terminate function.

It is safe to call this function multiple times.

Calling any other function in the library after SHUTDOWN is called
and before INIT is called again will lead to undefined behaviour.

See INIT")
  
  (type foreign-object
    "Base class for all objects wrapping a foreign GLFW object.

Usually a foreign-object can be allocated simply by calling
MAKE-INSTANCE, which will construct the GLFW object as well.

See POINTER
See DESTROY")
  
  (function pointer
    "Accesses the pointer that ties the object to the foreign resource.

This may be NIL if the object has been destroyed.

See DESTROY
See FOREIGN-OBJECT (type)")
  
  (function destroy
    "Finalize and clean up the object.

After this function has been called, the POINTER will be NIL.
It is safe to call this function multiple times.

Using the object in any other way after DESTROY has been called on it
will lead to undefined behaviour.

See POINTER
See FOREIGN-OBJECT (type)")
  
  (type cursor
    "Representation of a cursor.

When constructing a pointer, you should pass PIXELS, WIDTH, HEIGHT,
and optionally XHOT and YHOT. WIDTH and HEIGHT should be the size of
the image in PIXELS, and PIXELS should be a
 (simple-array (unsigned-byte 8) (*)) with the size (* width height 4)
this is the RGBA encoded pixel data of the cursor, top-left aligned.

See FOREIGN-OBJECT (type)
See CURSOR")
  
  (type monitor
    "Representation of a display monitor.

You should not directly create instances of this class. Instead use
LIST-MONITORS, PRIMARY-MONITOR, and MONITOR to access existing ones.

See FOREIGN-OBJECT (type)
See VIDEO-MODES
See LIST-MONITORS
See PRIMARY-MONITOR
See LOCATION
See WORK-AREA
See PHYSICAL-SIZE
See CONTENT-SCALE
See NAME
See VIDEO-MODE
See SIZE
See WIDTH
See HEIGHT
See REFRESH-RATE
See GAMMA
See GAMMA-RAMP
See MONITOR")
  
  (function video-modes
    "Returns a list of available video modes for the monitor.

Each video mode is a property list with the following keys:

  :WIDTH        --- The width in pixels
  :HEIGHT       --- The height in pixels
  :RED-BITS     --- The bit-depth of the red channel (usually 8)
  :GREEN-BITS   --- The bit-depth of the green channel (usually 8)
  :BLUE-BITS    --- The bit-depth of the blue channel (usually 8)
  :REFRESH-RATE --- The refresh rate in Hertz

See MONITOR (type)")
  
  (function list-monitors
    "List all available monitors.

This returns a fresh list of available MONITOR instances.

If REFRESH is passed, the list of available monitors is refreshed
directly. This is typically not necessary, as changes in the monitor
listing are instead dynamically monitored and the value of
LIST-MONITORS should always be accurate.

INIT must be called before this function returns anything useful.

See MONITOR (type)")
  
  (function primary-monitor
    "Returns the monitor that is configured as the \"primary\".

See MONITOR (type)")
  
  (function location
    "Returns the location of the monitor's area.

This is a LIST of (X Y).

See MONITOR (type)")
  
  (function work-area
    "Returns the work area of the monitor.

This is a LIST of (X Y W H).

See MONITOR (type)")
  
  (function physical-size
    "Returns the physical size of the monitor in mm.

This is a LIST of (W H).

See MONITOR (type)")
  
  (function content-scale
    "Returns the content scale of the monitor.

This is a LIST of (X Y).

These scale factors are used for HiDPI handling.

See MONITOR (type)")
  
  (function name
    "Returns a human-readable description of the monitor.

See MONITOR (type)")
  
  (function video-mode
    "Returns the current video-mode of the monitor.

See VIDEO-MODES
See MONITOR (type)")

  (function refresh-rate
    "Returns the current refresh rate of the monitor.

See VIDEO-MODE
See MONITOR (type)")
  
  (function gamma
    "Accesses the gamma of the monitor.

Note that this is in addition to any operating system gamma
ramping. This function is a simplified version of the more detailed
GAMMA-RAMP function. The returned GAMMA is estimated based on the
ramp's midpoint.

See GAMMA-RAMP
See MONITOR (type)")
  
  (function gamma-ramp
    "Accesses the gamma ramp of the monitor.

Note that this is in addition to any operating system gamma
ramping. The value must be a plist composed of the following entries:

  :RED
  :GREEN
  :BLUE

Each entry must be a (vector (unsigned-byte 16)) of the same
length. Which length is acceptable and whether the ramp can be set
depends on the operating system. A safe length is 256 entries.

See GAMMA
See MONITOR (type)")
  
  (type window
    "Representation of a window and its context.

The properties of the window and the context are configured via
initargs:

  :RESIZABLE         --- Whether the user can resize the window
  :VISIBLE           --- Whether the window starts visible
  :DECORATED         --- Whether the window has borders
  :FOCUSED           --- Whether the window starts focused
  :AUTO-ICONIFY      --- Whether the window should iconify on focus
                         loss
  :FLOATING          --- Whether the window should be \"always on top\"
  :MAXIMIZED         --- Whether the window starts maximized
  :CENTER-CURSOR     --- Whether to center the mouse cursor
  :TRANSPARENT-FRAMEBUFFER
                     --- Whether the framebuffer is transparent
  :FOCUS-ON-SHOW     --- Whether to request focus when the window is
                         made visible
  :SCALE-TO-MONITOR  --- Whether to resize the window area based on
                         DPI scaling factors of the monitor
  :MOUSE-PASSTHROUGH --- 
  :RED-BITS :GREEN-BITS :BLUE-BITS :ALPHA-BITS :DEPTH-BITS :STENCIL-BITS
                     --- The bit depth of the back buffer
  :ACCUM-RED-BITS :ACCUM-GREEN-BITS :ACCUM-BLUE-BITS :ACCUM-ALPHA-BITS
                     --- The bit depth of the accumulation buffer
  :AUX-BUFFERS       --- The desired number of auxiliary buffers
  :STEREO            --- Whether to enable stereo rendering
  :SAMPLES           --- The MSAA value to use for the back buffer
  :SRGB-CAPABLE      --- Whether the back buffer is srgb scaled
  :DOUBLEBUFFER      --- Whether to double-buffer the back buffer
  :REFRESH-RATE      --- The intended refresh rate to use
  :CLIENT-API        --- Which OpenGL API to use. May be one of:
    :no-api
    :opengl-api
    :opengl-es-api
  :CONTEXT-CREATION-API
                     --- Which API to use to create the context:
    :native-context-api
    :egl-context-api
    :osmesa-context-api
  :CONTEXT-VERSION-MAJOR :CONTEXT-VERSION-MINOR
                     --- The version of the context to request
  :OPENGL-FORWARD-COMPAT
                     --- Whether the context should be
                         forward-compatible.
  :CONTEXT-DEBUG     --- Whether to enable context debugging info
  :OPENGL-PROFILE    --- The profile of the context to request:
    :opengl-any-profile
    :opengl-core-profile
    :opengl-compat-profile
  :CONTEXT-ROBUSTNESS--- The robustness strategy of the context:
    :no-robustness
    :no-reset-notification
    :lose-context-on-reset
  :CONTEXT-RELEASE-BEHAVIOR
                     --- The release behaviour of the context:
    :any-release-behavior
    :release-behavior-flush
    :release-behavior-none
  :CONTEXT-NO-ERROR  --- Whether to generate errors on failures
  :COCOA-RETINA-FRAMEBUFFER
                     --- Whether to use full-resolution framebuffers
                         on retina displays
  :COCOA-FRAME-NAME  --- The frame name to use for Cocoa
  :COCOA-GRAPHICS-SWITCHING
                     --- Whether to allow Cocoa to move the context
                         between different GPUs.
  :X11-CLASS-NAME    --- The window class name to use for X11
  :X11-INSTANCE-NAME --- The window instance name to use for X11
  :WAYLAND-APP-ID    --- The App ID to pass to Wayland

You can find more information on the available initialisation flags
and their effects can be found in the GLFW documentation:

https://www.glfw.org/docs/latest/window_guide.html#window_hints

In addition to the window hints described, the following initargs may
be passed:

  :WIDTH   --- The initial width of the window. Defaults to 800
  :HEIGHT  --- The initial height of the window. Defaults to 600
  :TITLE   --- The title of the window. Defaults to \"GLFW\"
  :MONITOR --- The monitor on which to full-screen the window
  :SHARE   --- The context to share with

In order to react to events sent to the window, you should create a
subclass and implement methods on the following functions. By default
the corresponding events are simply discarded.

  WINDOW-RESIZED    --- When the window size has been changed
  WINDOW-CLOSED     --- When a close is requested
  WINDOW-MOVED      --- When the window was moved
  WINDOW-REFRESHED  --- When a refresh of the window is requested
  WINDOW-FOCUSED    --- When the window changes focus
  WINDOW-ICONIFIED  --- When the window's icon status changes
  WINDOW-MAXIMIZED  --- When the window's maximization status changes
  FRAMEBUFFER-RESIZED
                    --- When the back framebuffer's size changed
  WINDOW-CONTENT-SCALE-CHANGED
                    --- When the window's DPI scaling factors changed
  MOUSE-BUTTON-CHANGED
                    --- When a mouse button's active state changed
  MOUSE-MOVED       --- When the mouse has been moved
  MOUSE-ENTERED     --- When the mouse has entered or left the window
  MOUSE-SCROLLED    --- When the mouse scrolled
  KEY-CHANGED       --- When a keyboard key's active state changed
  CHAR-ENTERED      --- When a text entry is made
  FILE-DROPPED      --- When one or more files are dropped onto the
                        window

See FOREIGN-OBJECT (type)
See REGISTER-CALLBACKS
See ALLOCATE
See REALLOCATE
See DEALLOCATE
See WINDOW-RESIZED
See WINDOW-CLOSED
See WINDOW-MOVED
See WINDOW-REFRESHED
See WINDOW-FOCUSED
See WINDOW-ICONIFIED
See WINDOW-MAXIMIZED
See FRAMEBUFFER-RESIZED
See WINDOW-CONTENT-SCALE-CHANGED
See MOUSE-BUTTON-CHANGED
See MOUSE-MOVED
See MOUSE-ENTERED
See MOUSE-SCROLLED
See KEY-CHANGED
See CHAR-ENTERED
See FILE-DROPPED
See SHOULD-CLOSE-P
See TITLE
See LOCATION
See SIZE
See WIDTH
See HEIGHT
See SIZE-LIMITS
See ASPECT-RATIO
See FRAMEBUFFER-SIZE
See FRAME-SIZE
See CONTENT-SCALE
See OPACITY
See ICONIFIED-P
See MAXIMIZED-P
See VISIBLE-P
See STATE
See ICONIFY
See RESTORE
See MAXIMIZE
See SHOW
See HIDE
See FOCUS
See REQUEST-ATTENTION
See MONITOR
See ATTRIBUTE
See INPUT-MODE
See KEY-STATE
See MOUSE-BUTTON-STATE
See CURSOR-LOCATION
See CLIPBOARD-STRING
See MAKE-CURRENT
See GET-CURRENT
See SWAP-BUFFERS
See SWAP-INTERVAL
See CURSOR")
  
  (function width
    "Accesses the width of the window or monitor in pixels.

See SIZE
See MONITOR (type)
See WINDOW (type)")
  
  (function height
    "Accesses the height of the window or monitor in pixels.

See SIZE
See MONITOR (type)
See WINDOW (type)")
  
  (function aspect-ratio
    "Accesses the forced aspect ratio of the window.

If NIL, no aspect ratio is enforced.

See WINDOW (type)")
  
  (function size-limits
    "Accesses the size limits of the window.

This should be a list of four elements:

  MIN-WIDTH
  MIN-HEIGHT
  MAX-WIDTH
  MAX-HEIGHT

If any element is NIL, it is considered to have no limit.

See WINDOW (type)")
  
  (function swap-interval
    "Accesses the swap synchronisation interval.

A value of 1 is typically called \"vsync\".
May be negative on some drivers, which allows \"adaptive vsync\".

See WINDOW (type)")
  
  (function title
    "Accesses the window title.

See WINDOW (type)")
  
  (function cursor
    "Accesses the cursor of the window

You may also call this function with a keyword to retrieve one of
several standard cursor shapes, or directly set a keyword to set one
of those shapes.

See WINDOW (type)
See CURSOR (type)")
  
  (function register-callbacks
    "This function is used to register all of the GLFW callback functions.

If for some reason you need to override a callback, you should define
a method on this function to do so.

See WINDOW (type)")
  
  (function allocate
    "Allocate a block of memory.

This callback is only used if the window is initialised as an
allocator.

Should return pointer to the allocated block if successful, or a null
pointer if not.

See WINDOW (type)")
  
  (function reallocate
    "Resize a block of memory.

This callback is only used if the window is initialised as an
allocator.

Should return pointer to the allocated block if successful, or a null
pointer if not.

See WINDOW (type)")
  
  (function deallocate
    "Free a block of memory.

This callback is only used if the window is initialised as an
allocator.

See WINDOW (type)")
  
  (function window-resized
    "Callback for when the window has been resized.

Users may implement a primary method on this function to react to the
event.

See FRAMEBUFFER-RESIZED
See WINDOW (type)")
  
  (function window-closed
    "Callback for when the user requested the window to close.

Users may implement a primary method on this function to react to the
event. The default primary method sets the SHOULD-CLOSE-P flag to T.

See SHOULD-CLOSE-P
See WINDOW (type)")
  
  (function window-moved
    "Callback for when the window has been moved.

Users may implement a primary method on this function to react to the
event.

See WINDOW (type)")
  
  (function window-refreshed
    "Callback for when the system has requested the window contents be refreshed.

Users may implement a primary method on this function to react to the
event.

See WINDOW (type)")
  
  (function window-focused
    "Callback for when the window has changed focus.

Users may implement a primary method on this function to react to the
event.

See WINDOW (type)")
  
  (function window-iconified
    "Callback for when the window has changed iconified status.

Users may implement a primary method on this function to react to the
event.

See WINDOW (type)")
  
  (function window-maximized
    "Callback for when the window has changed maximized status.

Users may implement a primary method on this function to react to the
event.

See WINDOW (type)")
  
  (function framebuffer-resized
    "Callback for when the underlying framebuffer has been resized.

Users may implement a primary method on this function to react to the
event.

See WINDOW (type)")
  
  (function window-content-scale-changed
    "Callback for when the window's DPI scaling factor has changed.

Users may implement a primary method on this function to react to the
event.

See WINDOW (type)")
  
  (function mouse-button-changed
    "Callback for when a mouse button's state has changed.

The ACTION may be one of:

  :PRESS
  :RELEASE

The MODIFIERS is a list of active modifier keys at the time, which may
include:

  :SHIFT
  :CONTROL
  :ALT
  :SUPER
  :CAPS-LOCK
  :NUM-LOCK

Users may implement a primary method on this function to react to the
event.

See WINDOW (type)")
  
  (function mouse-moved
    "Callback for when the mouse cursor has been moved.

Users may implement a primary method on this function to react to the
event.

See WINDOW (type)")
  
  (function mouse-entered
    "Callback for when the cursor has entered or left the window.

Users may implement a primary method on this function to react to the
event.

See WINDOW (type)")
  
  (function mouse-scrolled
    "Callback for when the mouse wheel has scrolled.

Users may implement a primary method on this function to react to the
event.

See WINDOW (type)")
  
  (function key-changed
    "Callback for when a keyboard keys's state has changed.

The ACTION may be one of:

  :PRESS
  :RELEASE
  :REPEAT

The MODIFIERS is a list of active modifier keys at the time, which may
include:

  :SHIFT
  :CONTROL
  :ALT
  :SUPER
  :CAPS-LOCK
  :NUM-LOCK

Users may implement a primary method on this function to react to the
event.

See WINDOW (type)")
  
  (function char-entered
    "Callback for when text has been entered.

The CODE-POINT is the unicode code point of the character that has
been entered. You may convert it to a CHARACTER via CL:CODE-CHAR.

The MODIFIERS is a list of active modifier keys at the time, which may
include:

  :SHIFT
  :CONTROL
  :ALT
  :SUPER
  :CAPS-LOCK
  :NUM-LOCK

Users may implement a primary method on this function to react to the
event.

See CL:CODE-CHAR
See WINDOW (type)")
  
  (function file-dropped
    "Callback for when one or more files have been dropped onto the window.

PATHS is a list of the dropped files' paths as strings, typically in
the format of a native namestring.

Users may implement a primary method on this function to react to the
event.

See WINDOW (type)")
  
  (function should-close-p
    "Accesses whether the window should be closed and destroyed.

This flag by itself does nothing. You should read it out in your event
loop and depending on it and possibly other factors, decide to close
and DESTROY the window, or do whatever else is considered appropriate
at the time.

See DESTROY
See WINDOW (type)")
  
  (function location
    "Accesess the window's location on screen.

This is a list of (X Y).

See WINDOW (type)")
  
  (function size
    "Accesses the window or monitor's size on screen.

This is a list of (W H).

See WIDTH
See HEIGHT
See MONITOR (type)
See WINDOW (type)")
  
  (function framebuffer-size
    "Returns the window's backing framebuffer resolution.

This is a list of (W H).

See WINDOW (type)")
  
  (function frame-size
    "Returns the window's frame size including decorations.

This is a list of (LEFT TOP RIGHT BOTTOM)

See WINDOW (type)")
  
  (function content-scale
    "Returns the window's content scaling factor.

This is a list of (X Y).

See WINDOW (type)")
  
  (function opacity
    "Accesses the window's opacity factor.

This should be a float in the range [0,1].

See WINDOW (type)")

  (function iconified-p
    "Accesses whether the window is iconified.

See ICONIFY
See RESTORE
See STATE
See WINDOW (type)")

  (function maximized-p
    "Accesses whether the window is maximized.

See MAXIMIZE
See RESTORE
See STATE
See WINDOW (type)")

  (function visible-p
    "Accesses whether the window is visible.

Note that an iconified window may also be treated as invisible.

See SHOW
See HIDE
See STATE
See WINDOW (type)")

  (function state
    "Accesses the current state of the window.

The state may be one of the following:

  :ICONIFIED
  :HIDDEN
  :MAXIMIZED
  :NORMAL

See ICONIFIED-P
See MAXIMIZED-P
See VISIBLE-P
See WINDOW (type)")
  
  (function iconify
    "Request the window to be iconified.

See RESTORE
See WINDOW (type)")
  
  (function restore
    "Request to restore the window.

See ICONIFY
See MAXIMIZE
See WINDOW (type)")
  
  (function maximize
    "Request to maximize the window.

See RESTORE
See WINDOW (type)")
  
  (function show
    "Request to make the window visible.

See HIDE
See WINDOW (type)")
  
  (function hide
    "Request to make the window invisible.

see SHOW
See WINDOW (type)")
  
  (function focus
    "Request the window to gain focus.

See WINDOW (type)")
  
  (function request-attention
    "Request attention for the window.

See WINDOW (type)")
  
  (function monitor
    "Access the current monitor the window resides on.

When a monitor is set, the monitor's current video mode is retained
and the window is instead fullscreened to that monitor's
resolution. If you need to set the video mode of the monitor as well,
you must supply a list as the value fitting this lambda-list:

  (MONITOR &key WIDTH HEIGHT X Y REFRESH-RATE)

WIDTH, HEIGHT, and REFRESH-RATE default to the values of the monitor's
current video mode. X and Y default to 0.

MONITOR may also be T in which case the primary monitor is used.

See MONITOR (type)
See WINDOW (type)")
  
  (function attribute
    "Access an attribute of the window.

The attribute may be one of:

  :DECORATED        --- Whether the window has a border
  :RESIZABLE        --- Whether the window is resizable
  :FLOATING         --- Whether the window should be \"always on top\"
  :AUTO-ICONIFY     --- Whether the window should iconify on focus
                        loss
  :FOCUS-ON-SHOW    --- Whether to request focus when the window is
                        made visible.

See WINDOW (type)")
  
  (function input-mode
    "Access the state of an input mode of the window.

The mode may be one of:

  :CURSOR           --- Set the cursor state:
    :cursor-normal    --- Show the cursor and unlock it.
    :cursor-disabled  --- Hide the cursor and lock it into the window.
    :cursor-hidden    --- Just make the cursor invisible.
  :STICKY-KEYS      --- Whether a key's press state should be retained
                        until KEY-STATE is called for that key.
  :STICKY-MOUSE-BUTTONS
                    --- Whether a button's press state should be
                        retained until MOUSE-BUTTON-STATE is called
                        for that button.
  :LOCK-KEY-MODS    --- This will enable tracking caps lock and num
                        lock for the modifier key sets.
  :RAW-MOUSE-MOTION --- Whether to supply the window with mouse motion
                        data that's closer to the driver, unaffected
                        by desktop cursor smoothing and so on.

See WINDOW (type)")
  
  (function poll-events
    "Process new events synchronously.

Returns after processing events. This will cause event callbacks to be
called in the same thread.

The TIMEOUT may be one of:
  T    --- Blocks until *some* events have been processed
  NIL  --- Returns immediately if there are no events to process
  REAL --- Blocks until some events were processed or at most the
           given number of seconds have elapsed

See WINDOW (type)")
  
  (function key-state
    "Returns the current state of the requested key.

May be one of:
  :RELEASE
  :PRESS
  :REPEAT

See WINDOW (type)")
  
  (function mouse-button-state
    "Returns the current state of the requested mouse button.

May be one of:
  :RELEASE
  :PRESS

See WINDOW (type)")
  
  (function cursor-location
    "Accesses the current location of the mouse cursor.

This is a list of (X Y).

See WINDOW (type)")
  
  (function clipboard-string
    "Accesses the current clipboard contents.

See WINDOW (type)")
  
  (function version
    "Returns the version of the GLFW library.

This is a list of (MAJOR MINOR REVISION)")
  
  (function platform
    "Returns the currently used platform.

This may be one of:
  :WIN32
  :COCOA
  :WAYLAND
  :X11
  :NULL-PLATFORM")
  
  (function time
    "Accesses the current time in seconds as a double-float.

If this was not explicitly set, the time is measured since INIT has
been called.")
  
  (function timestamp
    "Returns a monotonically increasing timestamp.

See TIMESTAMP-RESOLUTION")
  
  (function timestamp-resolution
    "Returns the resolution of the timestamp in units/second.

See TIMESTAMP")
  
  (function make-current
    "Makes the context current in the calling thread.

See WINDOW (type)")
  
  (function get-current
    "Retrieves the current window of the calling thread, if any.

See WINDOW (type)")
  
  (function swap-buffers
    "Swaps the back buffers to make the drawn content visible.

See WINDOW (type)")

  (function icon
    "Access the icons of the application.

Should be a list of images, where each icon image is a list of the
format (PIXELS WIDTH HEIGHT), and where pixels is a 
 (simple-array (unsigned-byte 8) (*)) with the length (* width height 4)
holding the RGBA encoded pixel data of the icon, top-left aligned.

See WINDOW (type)"))

