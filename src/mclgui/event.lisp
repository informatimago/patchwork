;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               event.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Event processing
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-16 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
;;;;    
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

(in-package "MCLGUI")


(defgeneric view-activate-event-handler (view)
  (:documentation "
The generic function view-activate-event-handler is called by the
event system when the window containing the view is made active.
The definition for simple-view does nothing. The definition for view calls
view-activate-event-handler on each subview. Specialize this generic
function if your view needs to indicate visually that it is active.

VIEW:           A simple view or view.
"))


(defgeneric view-deactivate-event-handler (view)
  (:documentation "
The generic function view-deactivate-event-handler is called by
the event system to deactivate a view. It is called when the window
containing the view is active and a different window is made active.
The definition for simple-view does nothing. The definition for view calls
view-deactivate-event-handler on each subview. Specialize this
generic function if your view needs to indicate visually that it has been
deactivated.

VIEW:           A simple view or view.
"))


(defgeneric view-click-event-handler (view where)
  (:documentation "
The generic function VIEW-CLICK-EVENT-HANDLER is called by the
event system when a mouse click occurs. The SIMPLE-VIEW method does
nothing. The view method calls VIEW-CONVERT-COORDINATES-AND-CLICK
on the first subview for which POINT-IN-CLICK-REGION-P
returns T.

The VIEW-CLICK-EVENT-HANDLER function is not called when the user
clicks the title bar, close box, zoom box, or size box of a
window. The method for SIMPLE-VIEW does nothing.  Specialized windows
provided by the system, such as FRED-WINDOW, have special behavior.

The function VIEW-CLICK-EVENT-HANDLER scans subviews in the opposite
order as does VIEW-DRAW-CONTENTS.  The first view added is the first one
drawn but the last one to be queried during clicking.
If you define any VIEW-CLICK-EVENT-HANDLER methods for window, they
must call CALL-NEXT-METHOD.

VIEW:           A simple view or view.

WHERE:          For a view, the mouse click position (the position when
                the mouse is clicked) of the view in the local coordinate
                system. For a simple view, the mouse click position of the
                simple view in the local coordinate system of the view’s
                container.
"))


(defgeneric view-key-event-handler (view key)
  (:documentation "
The methods of the generic function VIEW-KEY-EVENT-HANDLER examine the
current keystroke and determine what is to be done with it.  The
method for SIMPLE-VIEW calls ED-BEEP.  The method for WINDOW
determines whether the key indicates the selection of a default button
or indicates a change of the current key handler, then selects the
button or passes the keystroke to the appropriate key handler.  The
method for FRED-MIXIN binds the *CURRENT-KEYSTROKE* variable to the
keystroke of the current event and runs the Fred command associated
with the keystroke.  The method for FRED-DIALOG-ITEM calls
call-next-method inside WITH-FOCUSED-VIEW and WITH-FORE-COLOR.

VIEW:           A simple view.

KEY:            The current keystroke character.
"))





(defgeneric window-null-event-handler (window)
  (:documentation "
The generic function WINDOW-NULL-EVENT-HANDLER is called on the top
window (if there is one) whenever the system is idle.  It updates the
cursor, runs system tasks, and forces output from *TERMINAL-IO*.  If
there is no top window, the unspecialized method simply updates the
cursor.

WINDOW:         A window.
"))


(defgeneric window-select-event-handler (window)
  (:documentation "
The generic function WINDOW-SELECT-EVENT-HANDLER is called whenever
the user clicks an inactive window.  The WINDOW-SELECT-EVENT-HANDLER
function may be specialized, for example, to make a window
unselectable.

WINDOW:         A window.
"))


(defgeneric window-key-up-event-handler (window)
  (:documentation "
The generic function WINDOW-KEY-UP-EVENT-HANDLER is called whenever a
key is released after being pressed.  The method for WINDOW does
nothing.

Every key pressed by the user actually generates two events: one when
the key is pressed and another when the key is released.

The default Macintosh event mask filters out key-up events.  To allow
key-up events, call #_SetEventMask with an appropriate mask.  Note
that you must reset the event mask before exiting Lisp.  For details
on event masks, see Macintosh Technical Note 202 and Inside Macintosh.

WINDOW:         A window.
"))


(defgeneric window-mouse-up-event-handler (window)
  (:documentation "
The WINDOW-MOUSE-UP-EVENT-HANDLER generic function is called whenever
the user releases the mouse button.  The method for WINDOW does
nothing.

WINDOW:         A window.
"))


(defgeneric window-grow-event-handler (window where)
  (:documentation "
The generic function WINDOW-GROW-EVENT-HANDLER is called by the event
system whenever the user clicks a window’s grow box.  The method for
window calls #_GrowWindow, then calls SET-VIEW-SIZE on the window and
the new size.

WINDOW:         A window.

WHERE:          The position in screen coordinates of the cursor when
                the mouse button was pressed down.
"))


(defgeneric window-drag-event-handler (window where)
  (:documentation "
The generic function WINDOW-DRAG-EVENT-HANDLER is called by the event
system whenever a window needs to be dragged.  It calls #_SetClip and
#_ClipAbove on the region of the window, copies the contents of the
region to the new location of window, and calls set-viewposition on
the window and the new position of the upper-left corner of the
window.

WINDOW:         A window.

WHERE:          The position in screen coordinates of the cursor when
                the mouse button was pressed down.
"))


(defgeneric window-zoom-event-handler (window message)
  (:documentation "
The generic function WINDOW-ZOOM-EVENT-HANDLER is called by the event
system when the user clicks the window’s zoom box.  It executes the
Toolbox calls to zoom the window, then calls WINDOW-SIZE-PARTS.  The
function WINDOW-SIZE-PARTS should be specialized if you want to change
the contents of a window whenever the window changes size.

WINDOW:         A window.

MESSAGE:        An keyword, :inZoomOut if the window should move to
                the window’s zoom position and size, or :inZoomIn if
                the window should move to the position and size it had
                before zooming out.
"))


(defgeneric window-close-event-handler (window)
  (:documentation "
The generic function WINDOW-CLOSE-EVENT-HANDLER is called by the event
system whenever a window needs to be closed.  In the method for
WINDOW, if the Meta key was pressed when the command was given, the
command closes all windows in the class of window.  If the Control key
was pressed, window is hidden.  Otherwise, WINDOW-CLOSE is called on
WINDOW.

WINDOW:         A window.
"))


(defgeneric window-do-first-click (window)
  (:documentation "
The generic function WINDOW-DO-FIRST-CLICK determines whether the
click that selects a window is also passed to VIEW-CLICK-EVENT-HANDLER.

The default value is NIL, meaning that the click that selects a window
generates no further action.  You can give a window instance or
subclass of window its own value for WINDOW-DO-FIRST-CLICK.

WINDOW:         A window.
")
  (:method (window)
    (declare (ignore window))
    nil))


(defgeneric window-updapte-event-handler (window)
  (:documentation "
The generic function WINDOW-UPDATE-EVENT-HANDLER is called by the
event system whenever any portion of the window needs to be redrawn.
The window version calls #_BeginUpdate to make the VisRgn field of the
GrafPort the portion that needs to be redrawn, calls
VIEW-DRAW-CONTENTS, and then calls #_EndUpdate to restore the GrafPort
VisRgn field.

Because event processing occurs asynchronously,
WINDOW-UPDATE-EVENT-HANDLER may not be called until a moment after a
window is created or uncovered.  (In the default environment, this may
take up to one-third of a second; see event-ticks in “The event
management system” on page 375.)  This means that anything drawn in
the window immediately after it is created or uncovered may be erased
when WINDOW-UPDATE-EVENT-HANDLER is first called.

To fix this problem, simply call EVENT-DISPATCH before drawing in the
window.  The function EVENT-DISPATCH forces the processing of any
pending events. Note that it is necessary to call EVENT-DISPATCH only
when drawing occurs soon after a window is created or uncovered.

You should not specialize this function except to note that the window
has been updated. To get special drawing behavior, you should instead
specialize VIEW-DRAW-CONTENTS.

WINDOW:         A window.
"))


(defgeneric view-draw-contents (view)
  (:documentation "
The generic function VIEW-DRAW-CONTENTS is called whenever a view
needs to redraw any portion of its contents.  The view method for
VIEW-DRAW-CONTENTS erases the area in the window’s erase region (for
new windows, this is the entire content area) and then calls
view-drawcontents on each subview.  You can specialize this function
so that a user-defined view can be redrawn when portions of it are
covered and uncovered.

When VIEW-DRAW-CONTENTS is called by the event system, the view’s clip
region is set so that drawing occurs only in the portions that need to
be updated.  This normally includes areas that have been covered by
other windows and then uncovered.

VIEW:           A simple view.
"))


(defgeneric window-draw-grow-icon (window)
  (:documentation "
The generic function WINDOW-DRAW-GROW-ICON is called when the size box
in the lower-right corner of a window must be redrawn. You may need to
call this function explicitly if you draw over the size box.

When a window is inactive (that is, not the frontmost window),
WINDOW-DRAW-GROW-ICON erases the inside of the size box.

WINDOW:         A window.
"))


(defgeneric view-mouse-position (view)
  (:documentation "
The generic function VIEW-MOUSE-POSITION returns the cursor position
as a point expressed in the view’s local coordinates.  The point is
returned as an integer (for a description of points, see “Chapter 2:
Points and Fonts”).  This function may be called at any time, not just
during event processing. The coordinates may be negative, or outside
of the view’s PortRect, depending on the position of the cursor.

The function (VIEW-MOUSE-POSITION NIL) returns the cursor position
expressed in screen coordinates.

VIEW:           A simple view.
"))




(defun mouse-down-p ()
  "
RETURN:         T if the mouse button is pressed and NIL
                otherwise. This function may be called at any time,
                not only during event processing.
"
  (niy mouse-down-p)
  nil)



(defun double-click-p ()
  "

RETURN:         T if the click currently being processed was the
                second half of a double-click.  Double-clicks take into
                account the timing as well as the spacing of
                consecutive clicks.

                The DOUBLE-CLICK-P function always returns NIL if
                called from outside event processing. It also returns
                false if the first click activated the window and
                WINDOW-DO-FIRST-CLICK is false.

"
  (niy double-click-p)
  nil)



(defun double-click-spacing-p (point1 point2)
  "
DESCRIPTION:    The function DOUBLE-CLICK-SPACING-P is called by
                DOUBLE-CLICK-P to see whether two clicks should count
                as a DOUBLE-CLICK.  It is also used to determine
                whether to increment *MULTI-CLICK-COUNT*.  Macintosh
                guidelines specify that if the cursor is moved
                excessively between clicks, the clicks do not count as
                a DOUBLE-CLICK.

RETURN:         NIL if POINT1 and POINT2 are separated by more than 4
                pixels, horizontally or vertically.  If they are within
                4 pixels of each other, both horizontally and
                vertically, the function returns true.

POINT1:         The cursor position during the first click.

POINT2:         The cursor position during the second click.
"
  (niy double-click-spacing-p)
  nil)


(defun command-key-p ()
  "
RETURN:         If called during event processing, return true if the
                command key was pressed during the event;
                otherwise, return NIL.

                If called outside of event processing, return true if
                the command key is currently pressed; otherwise,
                return NIL.
"
  (niy command-key-p)
  nil)


(defun control-key-p ()
  "
RETURN:         If called during event processing, return true if the
                control key was pressed during the event;
                otherwise, return NIL.

                If called outside of event processing, return true if
                the control key is currently pressed; otherwise,
                return NIL.
"
  (niy control-key-p)
  nil)


(defun option-key-p ()
  "
RETURN:         If called during event processing, return true if the
                option  key was pressed during the event;
                otherwise, return NIL.

                If called outside of event processing, return true if
                the option key is currently pressed; otherwise,
                return NIL.
"
  (niy option-key-p)
  nil)


(defun shift-key-p ()
  "
RETURN:         If called during event processing, return true if the
                shift key was pressed during the event;
                otherwise, return NIL.

                If called outside of event processing, return true if
                the shift key is currently pressed; otherwise,
                return NIL.
"
  (niy shift-key-p)
  nil)


(defun caps-lock-key-p ()
  "
RETURN:         If called during event processing, return true if the
                caps-lock key was pressed during the event;
                otherwise, return NIL.

                If called outside of event processing, return true if
                the caps-lock key is currently pressed; otherwise,
                return NIL.
"
  (niy caps-lock-key-p)
  nil)




(defstruct event
  (what      0 :type integer)
  (message   0 :type integer)
  (when      0 :type integer)
  (where     0 :type point)
  (modifiers 0 :type integer))




(defun event-dispatch (&optional (idle *idle*))
  "
The EVENT-DISPATCH function is called periodically as a background
process.  The EVENT-DISPATCH function calls #_WaitNextEvent and binds
the value of *CURRENT-EVENT* for the duration of the event processing.
It then calls *EVENTHOOK* if *EVENTHOOK* is not NIL.  If *EVENTHOOK*
returns true, the processing of the event stops.  If *EVENTHOOK*
returns NIL, the event is passed to the system event handlers.
Finally, EVENT-DISPATCH checks for deferred Apple events.

If you create a program with a loop that checks for events, you should
probably include a call to EVENT-DISPATCH inside the loop.  This
improves the response time when events occur.


IDLE:           An argument representing whether the main Lisp process
                is idle. The default is the value of *IDLE*, which is
                true when the main Lisp process is idle and NIL
                otherwise.  The function EVENT-DISPATCH calls
                GET-NEXT-EVENT with an event and the value of IDLE.
"
  (niy event-dispatch idle)
  nil)


(defun get-next-event (event &optional (idle *idle*) (sleep-ticks 1))
  "
DESCRIPTION:    The GET-NEXT-EVENT function calls #_WaitNextEvent to
                get an event.  It disables and reenables the clock
                sampled by GET-INTERNAL-RUNTIME.  (MultiFinder may do
                a context switch.)  After #_WaitNextEvent returns, the
                function reschedules the EVENT-DISPATCH task, which is
                the usual caller of GET-NEXT-EVENT.

EVENT:          An event record allocated on the stack or the heap.

IDLE:           Used to determine the default value of
                SLEEP-TICKS. The default value is *IDLE*, which is
                true if GET-NEXT-EVENT is called via EVENT-DISPATCH
                from the top-level loop when the Listener is waiting
                for input.

MASK:           This is the EventMask argument for #_WaitNextEvent, a
                fixnum.  The default is #$everyEvent.

SLEEP-TICKS:    This is the Sleep argument to #_WaitNextEvent.  It
                determines how many ticks are given to other
                applications under MultiFinder if no event is pending.
                The default is determined by the values of the idle
                argument and the global variables *IDLE-SLEEP-TICKS*,
                *FOREGROUND-SLEEP-TICKS*, and
                *BACKGROUND-SLEEP-TICKS*.  If Macintosh Common Lisp is
                running in the foreground, then the default is
                *IDLE-SLEEP-TICKS* if the value of idle is true;
                otherwise, the default is *FOREGROUND-SLEEPTICKS*.  If
                Macintosh Common Lisp is running in the background,
                then the default is *BACKGROUND-SLEEPTICKS* unless
                that value is NIL, in which case the default is the
                same as when Macintosh Common Lisp is running in the
                foreground.
"
  (niy get-next-event event idle sleep-ticks)
  nil)


(defun event-ticks ()
  "
RETURN:         The number of ticks (sixtieths of a second) between
                calls to EVENT-DISPATCH. This number is applicable
                when code is running. When Lisp is idling in the main
                read-eval-print loop, EVENT-DISPATCH is called as
                close to continuously as possible.  This value is
                reset on every suspend and resume event, according to
                the values in *FOREGROUND-EVENT-TICKS* and
                *BACKGROUND-EVENT-TICKS*.
"
  (niy event-ticks)
  ;; (let ((task *event-dispatch-task*))
  ;;   (when task (pref (ptask.state task) ptaskstate.interval)))
  1)


(defun set-event-ticks (n)
  "
DO:             Set the number of ticks between calls to
                EVENT-DISPATCH to N.

                If N is too low, EVENT-DISPATCH is called too often,
                and the system may get bogged down by event
                processing.  If it is too high, the system may not
                respond smoothly to events. To keep the insertion bar
                blinking smoothly, for example, a sleep time of 12 to
                20 ticks is recommended.  This will yield 3 to 5 idle
                events per second.

                This function is called on every suspend and resume
                event, with the argument *FOREGROUND-EVENT-TICKS* or
                *BACKGROUND-EVENT-TICKS*.

N:              An integer determining the number of ticks.
"
  (niy set-event-ticks n)
  ;; (setq n (require-type n '(integer 0 3767)))   ;  Why this weird limit ? - do we mean 32767
  ;; (let ((task *event-dispatch-task*))
  ;;   (when task (setf (pref (ptask.state task) ptaskstate.interval) n)))
  )


(defgeneric window-event (window)
  (:documentation "
The WINDOW-EVENT generic function is called by EVENT-DISPATCH to get a
window to handle an event.  This function is called only when the
event system determines the appropriate window.  The method of
WINDOW-EVENT for WINDOW checks the type of the event and calls the
appropriate event handler.  The WINDOW-EVENT function should be
specialized in windows that need to do something in addition to or
different from the default behavior for many types of events.

WINDOW:         A window.
"))


(defmacro without-interrupts (&body body)
  "
The WITHOUT-INTERRUPTS special form executes form with all event
processing disabled, including abort.

You should use WITHOUT-INTERRUPTS sparingly because anything executed
dynamically within it cannot be aborted or easily debugged.  However,
you must often use WITHOUT-INTERRUPTS in code that causes a window to
be redisplayed.  If you need to invalidate a number of regions in a
window, do it inside a without-interrupts form to prevent multiple
redisplays.
"
  ;; Note: the mcl implementation doesn't seem to do anything more:
  `(progn ,@body))


(defgeneric view-cursor (view point)
  (:documentation "
The VIEW-CURSOR generic function determines the cursor shape whenever
the window containing the view is active and the cursor is over it.
The VIEW-CURSOR function is called by WINDOW-UPDATE-CURSOR.

VIEW:           A view or simple view.
POINT:          The position of the cursor, expressed as a point.
"))


(defgeneric window-update-cursor (window point)
  (:documentation "
DESCRIPTION:    The generic function WINDOW-UPDATE-CURSOR is called by UPDATE-CURSOR
                whenever the cursor is over the window.

                When the mouse is over the front window or any floating window, the
                WINDOW-UPDATE-CURSOR method for the window class sets the variable
                *MOUSE-VIEW* to the view containing the mouse, using FIND-CLICKED-SUBVIEW.
                The WINDOW-NULL-EVENT-HANDLER method for the window class
                calls UPDATE-CURSOR, which calls *CURSORHOOK*. The function that is the
                initial value of *CURSORHOOK* calls WINDOW-UPDATE-CURSOR, which sets
                the cursor using the value returned by view-cursor.
                The method for window simply sets the cursor to the result of calling the
                generic function view-cursor on the clicked subview of window if there is
                one; otherwise it sets the cursor to the result of calling window-cursor on the
                window.

                The null method sets the cursor to the value of *ARROW-CURSOR*.

                The WINDOW-UPDATE-CURSOR function should be shadowed if the cursor
                must change according to what part of the window it is over.

WINDOW:         A window or Fred window.

POINT:          The position of the cursor, given in the window’s
                local coordinates.
"))



(defgeneric view-mouse-enter-event-handler (view)
  (:documentation "
The methods of these generic functions for SIMPLE-VIEW do nothing.
You specialize them to create mouse-sensitive items.

VIEW:           A simple view.
")
  (:method ((view simple-view))
    view))


(defgeneric view-mouse-leave-event-handler (view)
  (:documentation "
The methods of these generic functions for SIMPLE-VIEW do nothing.
You specialize them to create mouse-sensitive items.

VIEW:           A simple view.
")
  (:method ((view simple-view))
    view))



(defun initialize/event ()
  (niy initialize/event))


;;;; THE END ;;;;
