;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               variables.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines the global variables of the MCLGUI.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-09 <PJB> Created.
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
(enable-sharp-at-reader-macro)


;; Fonts:

(defvar *font-list* '()
  "Contains a list of all the fonts installed in the
current Macintosh Operating System, sorted alphabetically.")

(defvar *default-font-spec* '(:monaco 9 :plain))


;; Quickdraw:

(defparameter *pen-modes*
  '(:srcCopy    :srcOr    :srcXor    :srcBic
    :notSrcCopy :notSrcOr :notSrcXor :notsrcbic
    :patCopy    :patOr    :patXor    :patBic
    :notPatCopy :notPatOr :notPatXor :notPatBic)
  "Pen modes as integers.  These integers match the zero-based numeric
position of the keyword in the *PEN-MODES* list.  So, for example, the
number of :SRCOR pen mode could be coded as (POSITION :SRCOR *PEN-MODES*).
The inverse operation (turning a pen-mode integer into
a keyword) can be performed with the Common Lisp function ELT.")


(defparameter *text-modes*
  '(:blend :addPin :addOver :subPin :transparent
    :addMax :subOver :addMin :grayishTextOr
    :hilite :ditherCopy))



(defparameter *transfer-modes*
  ;; NOTE: the encoding for text-mode may not have been that, there
  ;;       was #$subOver etc constants.
  (let ((modes  (append *pen-modes* *text-modes*)))
    (make-array (length modes) :initial-contents modes)))


(define-condition unknown-transfer-mode (error)
  ((mode :initarg :mode :accessor unknown-transfer-mode))
  (:report (lambda (err stream)
             (format stream "Unknown transfer mode ~S"
                     (unknown-transfer-mode err)))))

(defun xfer-mode-arg (name &optional error-p)
  (or (position name *transfer-modes*)
      (when error-p (error 'unknown-transfer-mode :mode name))
      0)) ; :srcCopy by default.


(define-condition unknown-transfer-value (error)
  ((value :initarg :value :accessor unknown-transfer-value))
  (:report (lambda (err stream)
             (format stream "Unknown transfer mode value ~S"
                     (unknown-transfer-value err)))))

(defun xfer-mode-to-name (mode)
  (if (array-in-bounds-p *transfer-modes* mode)
      (aref *transfer-modes* mode)
      (error 'unknown-transfer-value :value mode)))



(defparameter *style-alist* 
  '((:plain     . 0)
    (:bold      . 1)
    (:italic    . 2)
    (:underline . 4)
    (:outline   . 8)
    (:shadow    . 16)
    (:condense  . 32)
    (:extend    . 64))
  "An association list of font-style keywords and numbers that the
Macintosh computer uses to encode these styles.  The Macintosh
Operating System encodes styles as a byte, with each style represented
by a bit (this encoding allows multiple styles).  You can derive a byte
to pass to the Macintosh computer by adding the numbers corresponding
to the styles listed here.")



;; Patterns:

(defvar *black-pattern*  nil)
(defvar *dkgray-pattern* nil)
(defvar *gray-pattern*   nil)
(defvar *ltgray-pattern* nil)
(defvar *white-pattern*  nil)


;; Screens:

(defvar *screen-width*  512
  "Width in pixels of the main screen.")
(defvar *screen-height* 342
  "Height in pixels of the main screen.")

(defvar *pixels-per-inch-x* 72
  "Number of pixels per inch in the horizontal direction on the main screen.")
(defvar *pixels-per-inch-y* 72
  "Number of pixels per inch in the vertical direction on the main screen.")



;; Views:

(defvar *current-view* nil
  "
The *CURRENT-VIEW* variable is bound to the view where drawing
currently occurs. See FOCUS-VIEW and WITH-FOCUSED-VIEW.
")


(defvar *current-font-view* nil)


(defvar *mouse-view* nil
  "
The *MOUSE-VIEW* variable is bound to the view that the mouse is
over.  This variable is updated by the window-update-cursor generic
function.
The *mouse-view* view is the one whose view-cursor method decides
which cursor to select.
")


;; Windows:

(defvar *last-mouse-click-window* nil)

(defvar *window-default-position*      #@(10 44)
  "The default position of a newly opened window. The initial value is #@(6 44).")

(defvar *window-default-size*          #@(502 188)
  "The default size of a newly opened window. The initial value is #@(502 150).")

(defvar *window-default-zoom-position* #@(6 44)
  "The *WINDOW-DEFAULT-ZOOM-POSITION* variable stores the default
zoom position of a window, that is, its new position after the user
clicks the zoom box.
This variable and *WINDOW-DEFAULT-ZOOM-SIZE* are initialized at
startup to make a zoomed window fill the screen containing the menu
bar with a 3-pixel border all around.")

(defvar *window-default-zoom-size*     #@(502 150)
  "The *WINDOW-DEFAULT-ZOOM-SIZE* variable stores the default zoom
size of a window, that is, its new size after the user clicks the zoom
box.
This variable and *WINDOW-DEFAULT-ZOOM-POSITION* are initialized at
startup to make a zoomed window fill the screen containing the menu
bar with a 3-pixel border all around.")

(defvar *selected-window* nil)

(defvar *windoid-count*   0
  "The number of visible floating windows currently in the MCL environment.")


;; Events:

(defvar *current-character* nil)

(defvar *top-listener* nil)

(defvar *multi-click-count* 0
  "
The *MULTI-CLICK-COUNT* variable is incremented during event
processing if the current event is part of a series of multiple
clicks. It is reset to 1 when there is a mouse click that is not part
of a series.

Determination of whether a click is part of a series is done as for
DOUBLE-CLICK-P.
")


(defvar *current-event* nil
  "
The *CURRENT-EVENT* variable holds the event record currently being
processed.  This is bound by EVENT-DISPATCH and is valid only during
event processing.
")


(defvar *eventhook* nil
  "
The *EVENTHOOK* variable provides a user hook into the event system.
A program can store a function of no arguments in this global
variable.  The stored function is given the first opportunity to
handle all event processing.  If the function returns true, the event
system assumes the event has been handled and no further processing is
done.  If the function returns NIL, the event system assumes the event
hasn’t been handled and the normal event handlers are invoked.
")


(defvar *idle* t
  "
The *IDLE* variable signals the event system that the main Lisp
process is idle. This changes the sleep time that event dispatch gives
to the trap #_WaitNextEvent.  This variable is normally bound to true
by the read loop as the loop waits for input, and by MODAL-DIALOG.
")


(defvar *idle-sleep-ticks* 5
  "
The *IDLE-SLEEP-TICKS* variable holds the value of the sleep time
given to #_WaitNextEvent when Macintosh Common Lisp is idle. The
initial value is 5.
")


(defvar *foreground-sleep-ticks* 0
  "
The *FOREGROUND-SLEEP-TICKS* variable holds the value of the sleep
time given to #_WaitNextEvent when Macintosh Common Lisp is
running. The initial value is 0
")


(defvar *background-sleep-ticks* 5
  "
The *BACKGROUND-SLEEP-TICKS* variable holds the value of the sleep
time given to #_WaitNextEvent when Macintosh Common Lisp is in the
background. The initial value is 5.
")


(defvar *foreground-event-ticks* 20
  "
The *FOREGROUND-EVENT-TICKS* variable holds the appropriate value for
event-ticks when Lisp is the foreground application. The initial value
is 20.
")


(defvar *background-event-ticks* 5
  "
The *BACKGROUND-EVENT-TICKS* variable holds the appropriate value for
event-ticks when Lisp is a background application. The initial value
is 5.
")


(defvar *break-look-when-uninterruptable* t
  "
Controls the interaction of break loops and WITHOUT-INTERRUPTS.  If
set to true, a break loop can occur during a without-interrupts.  The
WITHOUT-INTERRUPTS is suspended for the duration of the break loop.
The default value is t.
")


(defvar *event-dispatch-task* nil)


;; Cursors:

(defvar *arrow-cursor*  nil
  "
The *arrow-cursor* variable specifies the standard north-northwestarrow
cursor shape.
")


(defvar *i-beam-cursor* nil
  "
The *i-beam-cursor* variable specifies the I-beam shape used when
the cursor is over an area of editable text.
")


(defvar *watch-cursor*  nil
  "
The *watch-cursor* variable specifies the watch-face shape shown
during time-consuming operations, when event processing is disabled.
")


(defvar *cursorhook* nil
  "
The *CURSORHOOK* variable may be bound to a function or cursor, giving
you complete control over the appearance of the cursor.  This variable
is bound by WITH-CURSOR.

If the value of this variable is non-nil, then no other cursor
functions are called.  If the value of *CURSORHOOK* IS a function, it
is called repeatedly in the background and has complete control over
the state of the cursor at all times.

If it is not a function, it should be a cursor record or a 'CURS'
resource ID.

Its initial value is an internal function that allows events to alter
the cursor normally.
")

;; Scrap:

(defvar *scrap-state* nil
  "
The *SCRAP-STATE* variable contains a list of scrap types and
indicates which types currently have a valid scrap.  This variable is
modified by calls to PUT-SCRAP.
")


(defvar *scrap-handler-alist* nil
  "
The *SCRAP-HANDLER-ALIST* variable contains an association list of
scrap-type keywords and scrap-handler objects.  Initially, this
association list has three entries (one for :TEXT, one for :FRED, and
one for :LISP).  If you define new scrap handlers, you should add
entries for them to this list.
")


;; Application:

(defconstant default-app-creator :|????|)


(defvar *application* nil)

(defvar *lisp-startup-functions* '()
  "
The *LISP-STARTUP-FUNCTIONS* variable contains a list of functions of
no arguments on which funcall is run after Macintosh Common Lisp
starts, just before it enters the top-level function (usually the
Listener’s read loop).  The functions contained in
*LISP-STARTUP-FUNCTIONS* are run after the functions specified by
DEF-LOAD-POINTERS and before the init file is loaded.  The functions
are called in reverse order from the order in which they appear in the
list. 
")


(defvar *lisp-cleanup-functions* '()
  "
The *LISP-CLEANUP-FUNCTIONS* variable contains a list of functions of
no arguments on which funcall is run just before Macintosh Common Lisp
exits (via QUIT or SAVE-APPLICATION).  These functions are called just
after the windows are closed.

When saving an application, the functions in *LISP-CLEANUP-FUNCTIONS*
are run, then the functions in *SAVE-EXIT-FUNCTIONS* are run.
")

(defvar *save-exit-functions* '()
  "
The *SAVE-EXIT-FUNCTIONS* variable contains a list of functions to be
called when an image is saved.  These functions should perform any
preparation necessary for the image saving.  The functions are called
in the order in which they appear in the list.

When saving an application, the functions in *LISP-CLEANUP-FUNCTIONS*
are run, then the functions in *SAVE-EXIT-FUNCTIONS* are run.
")


(defvar *hide-windoids-on-suspend* t)



;;;; THE END ;;;;
