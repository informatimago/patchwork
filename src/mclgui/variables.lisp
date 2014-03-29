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
  "Contains a list of the names of all the fonts installed in the
current Macintosh Operating System, sorted alphabetically.")

(defvar *default-font-spec* '("Monaco" 9 :plain))


;; Quickdraw:


(define-condition invalid-value-designator-error (error)
  ())

(define-condition invalid-value-error (error)
  ())




(define-condition invalid-pen-mode-error (invalid-value-designator-error)
  ((mode :initarg :mode :accessor invalid-pen-mode))
  (:report (lambda (err stream)
             (format stream "Invalid pen mode ~S"
                     (invalid-pen-mode err)))))

(define-condition invalid-pen-mode-value-error (invalid-value-error)
  ((value :initarg :value :accessor invalid-pen-mode-value))
  (:report (lambda (err stream)
             (format stream "Invalid pen mode value ~S"
                     (invalid-pen-mode-value err)))))


(define-condition invalid-transfer-mode-error (invalid-value-designator-error)
  ((mode :initarg :mode :accessor invalid-transfer-mode))
  (:report (lambda (err stream)
             (format stream "Invalid transfer mode ~S"
                     (invalid-transfer-mode err)))))

(define-condition invalid-transfer-value-error (invalid-value-error)
  ((value :initarg :value :accessor invalid-transfer-mode-value))
  (:report (lambda (err stream)
             (format stream "Invalid transfer mode value ~S"
                     (invalid-transfer-mode-value err)))))



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


(defun pen-mode-arg (name &optional error-p)
  (or (and (keywordp name) (position name *pen-modes*))
      (and (integerp name) (< -1 name (length *pen-modes*)) name)
      (when error-p (error 'invalid-pen-mode-error :mode name))
      0)) ; :srcCopy by default.


(defun pen-mode-to-name (mode)
  (if (< -1 mode (length *pen-modes*))
      (elt *pen-modes* mode)
      (error 'invalid-transfer-value-error :value mode)))


(defparameter *text-modes*
  '(:blend :addPin :addOver :subPin :transparent
    :addMax :subOver :addMin :grayishTextOr
    :hilite :ditherCopy))


(defparameter *transfer-modes*
  ;; NOTE: the encoding for text-mode may not have been that, there
  ;;       was #$subOver etc constants.
  (let ((modes  (append *pen-modes* *text-modes*)))
    (make-array (length modes) :initial-contents modes)))


(defun xfer-mode-arg (name &optional error-p)
  (or (and (keywordp name) (position name *transfer-modes*))
      (and (integerp name) (< -1 name (length *transfer-modes*)) name)
      (when error-p (error 'invalid-transfer-mode-error :mode name))
      0)) ; :srcCopy by default.


(defun xfer-mode-to-name (mode)
  (if (array-in-bounds-p *transfer-modes* mode)
      (aref *transfer-modes* mode)
      (error 'invalid-transfer-value-error :value mode)))



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

;; Colors:

(defvar *color-available* nil
  "
The *COLOR-AVAILABLE* variable returns a value indicating whether the
Macintosh computer on which Macintosh Common Lisp is running supports
Color QuickDraw.

If the value of this variable is non-nil, then the Macintosh computer
supports the Color QuickDraw command set. If 32-bit QuickDraw is
available, its value is 32.

If the value of this variable is NIL, then Color QuickDraw is not
available.

This variable should never be changed by a program.
")


(defvar *black-color*        nil) 
(defvar *white-color*        nil) 
(defvar *pink-color*         nil) 
(defvar *red-color*          nil) 
(defvar *orange-color*       nil) 
(defvar *yellow-color*       nil) 
(defvar *green-color*        nil) 
(defvar *dark-green-color*   nil) 
(defvar *light-blue-color*   nil) 
(defvar *blue-color*         nil) 
(defvar *purple-color*       nil) 
(defvar *brown-color*        nil) 
(defvar *tan-color*          nil) 
(defvar *gray-color*         nil) 
(defvar *light-gray-color*   nil) 
(defvar *lighter-gray-color* nil) 
(defvar *dark-gray-color*    nil)

(defvar *tool-back-color*    nil)

;; Patterns:

(defvar *black-pattern*      nil)
(defvar *dark-gray-pattern*  nil)
(defvar *gray-pattern*       nil)
(defvar *light-gray-pattern* nil)
(defvar *white-pattern*      nil)


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
  "
The *WINDOW-DEFAULT-ZOOM-POSITION* variable stores the default zoom
position of a window, that is, its new position after the user clicks
the zoom box.

This variable and *WINDOW-DEFAULT-ZOOM-SIZE* are initialized at
startup to make a zoomed window fill the screen containing the menu
bar with a 3-pixel border all around.
")


(defvar *window-default-zoom-size*     #@(502 150)
  "
The *WINDOW-DEFAULT-ZOOM-SIZE* variable stores the default zoom size
of a window, that is, its new size after the user clicks the zoom box.

This variable and *WINDOW-DEFAULT-ZOOM-POSITION* are initialized at
startup to make a zoomed window fill the screen containing the menu
bar with a 3-pixel border all around.
")


(defvar *selected-window* nil)

(defvar *window-list* '())
;; *selected-window* should be (first *window-list*)


(defvar *modal-dialog-on-top* nil
  "
The *MODAL-DIALOG-ON-TOP* variable is true when a modal dialog is
the frontmost window.  It is bound during the event processing done by
the MODAL-DIALOG function.  Its value is used by the MCL window
system code to determine the behavior of floating windows.  This value
should not be modified by the user, but can be used to determine whether
a modal dialog is being processed.
")


(defvar *windoid-count*   0
  "The number of visible floating windows currently in the MCL environment.")



;; Events:

(defvar *current-character* nil)

(defvar *top-listener* nil)



(defvar *current-event* nil
  "
The *CURRENT-EVENT* variable holds the event record currently being
processed.  This is bound by EVENT-DISPATCH and is valid only during
event processing.
")

(defvar *multi-click-count* 0
  "
The *MULTI-CLICK-COUNT* variable is incremented during event
processing if the current event is part of a series of multiple
clicks. It is reset to 1 when there is a mouse click that is not part
of a series.

Determination of whether a click is part of a series is done as for
DOUBLE-CLICK-P.
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


(defvar *idle* nil
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

;; pane-splitter cursors:
(defvar *top-ps-cursor*        nil)
(defvar *bottom-ps-cursor*     nil)
(defvar *left-ps-cursor*       nil)
(defvar *right-ps-cursor*      nil)
(defvar *vertical-ps-cursor*   nil)
(defvar *horizontal-ps-cursor* nil)


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

(defconstant default-appl-creator :|????|)

(defvar *application* nil)

(defvar *hide-windoids-on-suspend* t)


;;;; THE END ;;;;
