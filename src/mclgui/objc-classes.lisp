;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               objc-classes.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines a few Objective-C/CLOS classes.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-06-20 <PJB> Created.
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
(objcl:enable-objcl-reader-macros)



;;; MCLGUI use the coordinates system of the Macintosh OS,
;;; that is, origin is at the top-left corner, and Y axis goes downward.
;;;
;;; OpenStep uses the normal mathematical coordinates system, with the
;;; origin on the bottom-left corner, and Y axis going upward.
;;; Furthermore, the main screen may not be positionned at the origin
;;; of the coordinates system.

;;;------------------------------------------------------------
;;;
;;; Representation of NSPoint, NSSize and NSRect in lisp,
;;; with conversion between MCLGUI:POINT.

(defun xor (a b)
  "Return A ⊻ B"
  (or (and a (not b)) (and (not a) b)))

(defun cgfloat    (value) (coerce value 'ns:cgfloat))
(defun fontsize   (value) (round  value))
(defun coord      (value) (round  value))
(declaim (inline cgfloat fontsize coord))


(defstruct (nspoint
             (:constructor %make-nspoint))
  (x      0.0d0 :type double-float)
  (y      0.0d0 :type double-float))

(defun make-nspoint (&key (x 0.0d0) (y 0.0d0))
  (%make-nspoint :x (cgfloat x) :y (cgfloat y)))


(defstruct (nssize
             (:constructor %make-nssize))
  (width  0.0d0 :type double-float)
  (height 0.0d0 :type double-float))

(defun make-nssize (&key (width 0.0d0) (height 0.0d0))
  (%make-nssize :width (cgfloat width) :height (cgfloat height)))


(defstruct (nsrect
             (:constructor %make-nsrect))
  (x      0.0d0 :type double-float)
  (y      0.0d0 :type double-float)
  (width  0.0d0 :type double-float)
  (height 0.0d0 :type double-float))


(defun make-nsrect (&key (x 0.0d0 xp) (y 0.0d0 yp) (width 0.0d0 widthp) (height 0.0d0 heightp) origin size)
  (assert (xor (or xp yp) origin))
  (assert (xor (or widthp heightp) size))
  (if origin
      (if size
          (%make-nsrect :x     (nspoint-x origin)  :y      (nspoint-y origin)
                        :width (nssize-width size) :height (nssize-height size))
          (%make-nsrect :x     (nspoint-x origin)  :y      (nspoint-y origin)
                        :width (cgfloat width)     :height (cgfloat height)))
      (if size
          (%make-nsrect :x     (cgfloat x)         :y      (cgfloat y)
                        :width (nssize-width size) :height (nssize-height size))
          (%make-nsrect :x     (cgfloat x)         :y      (cgfloat y)
                        :width (cgfloat width)     :height (cgfloat height)))))


(defun point-to-nspoint (point)   (make-nspoint :x (cgfloat (point-h point)) :y (cgfloat (point-v point))))
(defun nspoint-to-point (nspoint) (make-point (coord (nspoint-x nspoint)) (coord (nspoint-y nspoint))))

(defun size-to-nssize (size)   (make-nssize :width (cgfloat (point-h size)) :height (cgfloat (point-v size))))
(defun nssize-to-size (nssize) (make-point (coord (nssize-width nssize)) (coord (nssize-height nssize))))

(defun nsrect-origin (nsrect) (make-nspoint :x     (nsrect-x nsrect)     :y      (nsrect-y nsrect)))
(defun nsrect-size   (nsrect) (make-nssize  :width (nsrect-width nsrect) :height (nsrect-height nsrect)))

(defun (setf nsrect-origin) (nspoint nsrect)
  (setf (nsrect-x nsrect) (nspoint-x nspoint)
        (nsrect-y nsrect) (nspoint-y nspoint)))
(defun (setf nsrect-size)   (nssize  nsrect)
  (setf (nsrect-width  nsrect) (nssize-width  nssize)
        (nsrect-height nsrect) (nssize-height nssize)))

(defun rect-to-nsrect (position size)
  (make-nsrect :x (cgfloat (point-h position))
               :y (cgfloat (point-v position))
               :width  (cgfloat (point-h size))
               :height (cgfloat (point-v size))))

(defun nsrect-to-rect (nsrect)
  "RETURN: A list of POINTs: position and size."
  (list (make-point (coord (nsrect-x nsrect))      (coord (nsrect-y nsrect)))
        (make-point (coord (nsrect-width nsrect))  (coord (nsrect-height nsrect)))))



;;;------------------------------------------------------------
;;; Conversions between ns:ns-point, ns:ns-size, ns:ns-rect and
;;; nspoint nssize and nsrect.

(defun wrap-nspoint (nspoint)
  (wrapping
   (make-nspoint :x     (ns:ns-point-x nspoint)
                 :y     (ns:ns-point-y nspoint))))

(defun wrap-nssize (nssize)
  (wrapping
   (make-nssize :width  (ns:ns-size-width nssize)
                :height (ns:ns-size-height nssize))))

(defun wrap-nsrect (nsrect)
  (wrapping
   (make-nsrect :x      (ns:ns-rect-x nsrect)
                :y      (ns:ns-rect-y nsrect)
                :width  (ns:ns-rect-width nsrect)
                :height (ns:ns-rect-height nsrect))))


(defmethod unwrap ((nspoint nspoint))
  (unwrapping nspoint
              (ns:make-ns-point (nspoint-x nspoint) (nspoint-y nspoint))))

(defmethod unwrap ((nssize nssize))
  (unwrapping nssize
              (ns:make-ns-size (nssize-width nssize) (nssize-height nssize))))

(defmethod unwrap ((nsrect nsrect))
  (unwrapping nsrect
              (ns:make-ns-rect (nsrect-x nsrect) (nsrect-y nsrect)
                               (nsrect-width nsrect) (nsrect-height nsrect))))

;; Shortcuts:

(defun nsrect (pos siz)
  (ns:make-ns-rect (point-h pos) (point-v pos) (point-h siz) (point-v siz)))

(defun nspoint (pos)
  (ns:make-ns-point (point-h pos) (point-v pos)))

(defun nssize (siz)
  (ns:make-ns-size (point-h siz) (point-v siz)))

(declaim (inline nsrect-to-list nsrect nspoint nssize))


(defmacro get-nspoint (call)
  (let ((vpoint (gensym)))
    `(oclo:slet ((,vpoint ,call)) (wrap-nspoint ,vpoint))))

(defmacro get-nssize (call)
  (let ((vsize (gensym)))
    `(oclo:slet ((,vsize ,call)) (wrap-nssize ,vsize))))

(defmacro get-nsrect (call)
  (let ((vframe (gensym)))
    `(oclo:slet ((,vframe ,call)) (wrap-nsrect ,vframe))))



;;;------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; event what:
  (defconstant null-event    0)
  (defconstant mouse-down    1)
  (defconstant mouse-up      2)
  (defconstant key-down      3)
  (defconstant key-up        4)
  (defconstant auto-key      5)
  (defconstant update-evt    6)
  (defconstant disk-evt      7)
  (defconstant activate-evt  8)
  (defconstant network-evt  10)
  (defconstant driver-evt   11)
  (defconstant app1-evt     12)
  (defconstant app2-evt     13)
  (defconstant app3-evt     14)
  (defconstant app4-evt     15)

  (defconstant every-event     #xffff)
  (defconstant mouse-down-mask (ash 1  1))
  (defconstant mouse-up-mask   (ash 1  2))
  (defconstant key-down-mask   (ash 1  3))
  (defconstant key-up-mask     (ash 1  4))
  (defconstant auto-key-mask   (ash 1  5))
  (defconstant update-mask     (ash 1  6))
  (defconstant disk-mask       (ash 1  7))
  (defconstant activate-mask   (ash 1  8))
  (defconstant network-mask    (ash 1 10))
  (defconstant driver-mask     (ash 1 11))
  (defconstant app1-mask       (ash 1 12))
  (defconstant app2-mask       (ash 1 13))
  (defconstant app3-mask       (ash 1 14))
  (defconstant app4-mask       (ash 1 15))

  (defconstant active-flag   1)
  (defconstant btn-state     128)
  (defconstant cmd-key       256)
  (defconstant shift-key     512)
  (defconstant alpha-lock    1024)
  (defconstant option-key    2048)
  );;eval-when

(defparameter *event-map*
  `((,#$NSLeftMouseDown          . ,mouse-down)
    (,#$NSLeftMouseUp            . ,mouse-up)
    (,#$NSRightMouseDown         . ,mouse-down)
    (,#$NSRightMouseUp           . ,mouse-up)
    (,#$NSMouseMoved             . ,null-event)
    (,#$NSLeftMouseDragged       . ,null-event)
    (,#$NSRightMouseDragged      . ,null-event)
    (,#$NSMouseEntered           . ,null-event)
    (,#$NSMouseExited            . ,null-event)
    (,#$NSKeyDown                . ,key-down)
    (,#$NSKeyDown                . ,auto-key)
    (,#$NSKeyUp                  . ,key-up)
    (,#$NSFlagsChanged           . ,null-event)
    (,#$NSAppKitDefined          . ,null-event)
    (,#$NSSystemDefined          . ,null-event)
    (,#$NSApplicationDefined     . ,null-event)
    (,#$NSPeriodic               . ,null-event)
    (,#$NSCursorUpdate           . ,null-event)
    (,#$NSScrollWheel            . ,null-event)
    (,#$NSTabletPoint            . ,null-event)
    (,#$NSTabletProximity        . ,null-event)
    (,#$NSOtherMouseDown         . ,mouse-down)
    (,#$NSOtherMouseUp           . ,mouse-up)
    (,#$NSOtherMouseDragged      . ,null-event)
    (,#$NSEventTypeGesture       . ,null-event)
    (,#$NSEventTypeMagnify       . ,null-event)
    (,#$NSEventTypeSwipe         . ,null-event)
    (,#$NSEventTypeRotate        . ,null-event)
    (,#$NSEventTypeBeginGesture  . ,null-event)
    (,#$NSEventTypeEndGesture    . ,null-event)))

(defun mac-event-mask-to-ns-event-mask (mac-mask)
  (loop
    :with ns-mask = 0
    :for (ns-event . mac-event) :in *event-map*
    :do (when (and (/= null-event mac-event)
                   (plusp (logand (ash 1 mac-event) mac-mask)))
          (setf ns-mask (logior ns-mask (ash 1 ns-event))))
    :finally (return ns-mask)))




(defparameter *modifier-map*
  `((,#$NSAlphaShiftKeyMask . ,alpha-lock)
    (,#$NSShiftKeyMask      . ,shift-key)
    (,#$NSControlKeyMask    . 0) 
    (,#$NSAlternateKeyMask  . ,option-key) 
    (,#$NSCommandKeyMask    . ,cmd-key) 
    (,#$NSNumericPadKeyMask . 0) 
    (,#$NSHelpKeyMask       . 0) 
    (,#$NSFunctionKeyMask   . 0)))

(defun nsmodifier-to-macmodifier (nsmodifier)
  (loop
    :for (nsmod . macmod) :in *modifier-map*
    :sum (if (zerop (logand nsmod nsmodifier))
             0
             macmod)))

(defun macmodifier-to-nsmodifier (macmodifier)
  (loop
    :for (nsmod . macmod) :in *modifier-map*
    :sum (if (zerop (logand macmod macmodifier))
             0
             nsmod)))
 


(defstruct event
  (what      0 :type integer)
  (message   0 :type integer)
  (when      0 :type integer)
  (where     0 :type point)
  (modifiers 0 :type integer))


(defun assign-event (dst src)
  "
DO:             Copies the fields of SRC event to DST event.
RETURN:         DST.
"
  (setf (event-what      dst) (event-what      src)
        (event-message   dst) (event-message   src)
        (event-when      dst) (event-when      src)
        (event-where     dst) (event-where     src)
        (event-modifiers dst) (event-modifiers src))
  dst)

(defconstant +tick-per-second+ 60 "Number of ticks per second.")

(defun wrap-nsevent (nsevent)
  (wrapping
   (let ((what (case [nsevent type]
                 ((#.#$NSLeftMouseDown)         mouse-down)
                 ((#.#$NSLeftMouseUp)           mouse-up)
                 ((#.#$NSRightMouseDown)        mouse-down)
                 ((#.#$NSRightMouseUp)          mouse-up)
                 ((#.#$NSMouseMoved)            null-event)
                 ((#.#$NSLeftMouseDragged)      null-event)
                 ((#.#$NSRightMouseDragged)     null-event)
                 ((#.#$NSMouseEntered)          null-event)
                 ((#.#$NSMouseExited)           null-event)
                 ((#.#$NSKeyDown)               (if [nsevent isARepeat]
                                                    auto-key
                                                    key-down))
                 ((#.#$NSKeyUp)                 key-up)
                 ((#.#$NSFlagsChanged)          null-event)
                 ((#.#$NSAppKitDefined)         null-event)
                 ((#.#$NSSystemDefined)         null-event)
                 ((#.#$NSApplicationDefined)    null-event)
                 ((#.#$NSPeriodic)              null-event)
                 ((#.#$NSCursorUpdate)          null-event)
                 ((#.#$NSScrollWheel)           null-event)
                 ((#.#$NSTabletPoint)           null-event)
                 ((#.#$NSTabletProximity)       null-event)
                 ((#.#$NSOtherMouseDown)        mouse-down)
                 ((#.#$NSOtherMouseUp)          mouse-up)
                 ((#.#$NSOtherMouseDragged)     null-event)
                 ((#.#$NSEventTypeGesture)      null-event)
                 ((#.#$NSEventTypeMagnify)      null-event)
                 ((#.#$NSEventTypeSwipe)        null-event)
                 ((#.#$NSEventTypeRotate)       null-event)
                 ((#.#$NSEventTypeBeginGesture) null-event)
                 ((#.#$NSEventTypeEndGesture)   null-event)
                 (otherwise                     null-event))))
     (make-event
      :what      what
      :message   (case what
                   ((#.key-down #.key-up #.auto-key)
                    (dpb (ldb [nsevent keyCode] (byte 8 0))
                         (byte 8 8)
                         (let ((characters (objcl:lisp-string [nsevent characters])))
                           (if (zerop (length characters))
                               0
                               (char-code (aref characters 0))))))
                   (otherwise 0))
      :when      (truncate [nsevent timestamp] (/ +tick-per-second+))
      :where     (let ((winh [nsevent window]))
                   (if (nullp winh)
                     ;; If the event has no window, let's leave it in
                     ;; screen coordinates (not quite useful yet,
                     ;; since screen coordinates are not flipped).
                     (nspoint-to-point (get-nspoint [nsevent locationInWindow]))
                     ;; If the event has a window, convert the
                     ;; coordinates to contentView coordinate system.
                     (let ((viewh [winh contentView]))
                       (nspoint-to-point (get-nspoint [viewh convertPoint:[nsevent locationInWindow]
                                                             fromView:*null*])))))
      :modifiers (nsmodifier-to-macmodifier [nsevent modifierFlags])))))

;;;------------------------------------------------------------


(defmacro report-errors (&body body)
  `(handler-case
       (progn ,@body)
     (error (err)
       (format *trace-output* "~%ERROR while ~S:~%~A~2%"
               ',(if (= 1 (length body)) body `(progn ,@body))
               err)
       (finish-output *trace-output*)
       nil)))


(defun format-trace (method &rest arguments)
  (format *trace-output* "~&~40A ~{~S~^ ~}~%" method arguments)
  (force-output *trace-output*))


;;;------------------------------------------------------------


(defmacro frame (call)
  (let ((vframe (gensym)))
    `(oclo:slet ((,vframe ,call)) 
                (values
                 (ns:ns-rect-x ,vframe)
                 (ns:ns-rect-y ,vframe)
                 (ns:ns-rect-width  ,vframe)
                 (ns:ns-rect-height ,vframe)))))




;; wx = sx + vh
;; wy = sy - vv - sv
;; 
;; vh = wx - sx
;; vv = sy - wy - sv


(defun nswindow-to-window-position (frame-coordinates size-point)
  "
RETURN: The view-position POINT.
"
  (let ((screen-pos (main-screen-frame)))
    (destructuring-bind (x y &rest size) frame-coordinates
      (declare (ignore size))
      (make-point (- (round x) (point-h screen-pos))
                  (- (point-v screen-pos) (round y) (point-v size-point))))))


(defun window-to-nswindow-origin (position size)
  "
RETURN: A NSPoint containing the origin of the nswindow.
"
  (multiple-value-bind (screen-pos screen-siz) (main-screen-frame)
    (ns:make-ns-point (+ (point-h screen-pos) (point-h position))
                      (- (+ (point-v screen-pos) (point-v screen-siz))
                         (point-v position) (point-v size)))))


(defun window-to-nswindow-frame (position size)
  "
RETURN: A NSRect containing the frame of the window.
"
  (multiple-value-bind (screen-pos screen-siz) (main-screen-frame)
    (ns:make-ns-rect (+ (point-h screen-pos) (point-h position))
                     (- (+ (point-v screen-pos) (point-v screen-siz))
                        (point-v position) (point-v size))
                     (point-h size)
                     (point-v size))))



(defun main-screen-frame ()
  "
RETURN:         Position and size of the main screen.
"
  (multiple-value-bind (x y w h) (frame [[NSScreen mainScreen] frame])
    (values (make-point (round x) (round y))
            (make-point (round w) (round h)))))


;;;------------------------------------------------------------
;;; Types.

#+ccl (ccl:def-foreign-type ns-rect-ptr (:* :<NSR>ect))

;;;------------------------------------------------------------
;;; MclguiWindowDelegate
;;; Not anymore, MclguiWindow is its own delegate.

;; @[NSObject subClass:MclguiWindowDelegate
;;            slots:((window :initform nil
;;                           :initarg :window
;;                           :reader delegate-window))]
;; 
;; 
;; @[MclguiWindowDelegate
;;   method:(windowDidMove:(:id)nsnotification)
;;   resultType:(:void)
;;   body:
;;   (declare (ignore nsnotification))
;;   (report-errors
;;       (let* ((window (delegate-window self)))
;;         (format-trace "-[MclguiWindowDelegate windowDidMove:]" window)
;;         (with-handle (handle  window)
;;           (window-move-event-handler window
;;                                      (nswindow-to-window-position (multiple-value-list (frame [handle frame]))
;;                                                                   (view-size window))))))]
;; 
;; 
;; @[MclguiWindowDelegate
;;   method:(windowDidResize:(:id)nsnotification)
;;   resultType:(:void)
;;   body:
;;   (declare (ignore nsnotification))
;;   (report-errors
;;       (let ((window (delegate-window self)))
;;         (format-trace "-[MclguiWindowDelegate windowDidResize:]" window)
;;         (window-grow-event-handler window (add-points (view-position window) (view-size window)))))]




;;;------------------------------------------------------------
;;; MclguiWindow

@[NSWindow subClass:MclguiWindow
           slots:((window :initform nil
                          :initarg :view
                          :reader nswindow-window))]

@[MclguiWindow
  method:(setFrame:(:<NSR>ect)rect)
  resultType:(:void)
  body:
  (format-trace "-[MclguiWindow setFrame:]")
  [self setFrame:rect display:YES]]


@[MclguiWindow
  method:(orderBelow:(:id)otherWindow)
  resultType:(:void)
  body:
  (format-trace "-[MclguiWindow orderBelow:]")
  [self orderWindow:#$NSWindowBelow relativeTo:[otherWindow windowNumber]]]



@[MclguiWindow
  method:(windowDidMove:(:id)nsnotification)
  resultType:(:void)
  body:
  (declare (ignore nsnotification))
  (report-errors
      (let* ((window (nswindow-window self)))
        (format-trace "-[MclguiWindow windowDidMove:]" window)
        (with-handle (handle  window)
          (window-move-event-handler window
                                     (nswindow-to-window-position (multiple-value-list (frame [handle frame]))
                                                                  (view-size window))))))]


@[MclguiWindow
  method:(windowDidResize:(:id)nsnotification)
  resultType:(:void)
  body:
  (declare (ignore nsnotification))
  (report-errors
      (let ((window (nswindow-window self)))
        (format-trace "-[MclguiWindow windowDidResize:]" window)
        (window-grow-event-handler window (add-points (view-position window) (view-size window)))))]



@[MclguiWindow
  method:(windowShouldClose:(:id)nsnotification)
  resultType:(:<bool>)
  body:
  (declare (ignore nsnotification))
  (report-errors
      (let* ((window (nswindow-window self)))
        (format-trace "-[MclguiWindow windowShouldClose:]" window)
        (window-close-event-handler window)))]


@[MclguiWindow
  method:(doClose)
  resultType:(:void)
  body:
  (format-trace "-[MclguiWindow doClose]")
  [super close]]



@[MclguiWindow
  method:(close)
  resultType:(:void)
  body:
  (let ((window  (nswindow-window self)))
    (format-trace "-[MclguiWindow close]" window)
    (report-errors
        (catch :cancel (window-close window))))]



@[MclguiWindow
  method:(windowShouldZoom:(:id)nswindow toFrame:(:<NSR>ect)newFrame)
  resultType:(:<BOOL>)
  body:
  (declare (ignore nswindow newframe))
  (let ((window (nswindow-window self)))
    (format-trace "-[MclguiWindow windowShouldZoom:toFrame:]" window)
    ;; TODO: if newFrame is in frame, then :inZoomIn, if it's out frame, then :inZoomOut.
    ;; (window-zoom-event-handler window :inZoomOut)
    (eq (window-type window) :document-with-zoom))]


;; @[MclguiWindow
;;   method:(windowWillUseStandardFrame:(:id)window defaultFrame:(:<NSR>ect)newFrame)
;;   resultType:(:<NSR>ect)
;;   body:
;;   (let* ((window (nswindow-window self)))
;;     (format *trace-output* "~&window should close     ~S~%" window)
;;     (window-zoom-event-handler window :inZoomIn))]

@[MclguiWindow
  method:(zoom:(:id)sender)
  resultType:(:void)
  body:
  [super zoom:sender]
  (let ((window (nswindow-window self)))
    (format-trace "-[MclguiWindow zoom:]" window)
    (when window
     (report-errors (window-do-zoom window))))]





@[MclguiWindow
  method:(becomeMainWindow)
  resultType:(:void)
  body:
  [super becomeMainWindow]
  (report-errors
      (let* ((window (nswindow-window self)))
        (format-trace "-[MclguiWindow becomeMainWindow]" window)
        ;; TODO: move after windoids.
        (when window
          (delete-from-list *window-list* window)
          (insert-into-list *window-list* 0 window)
          (view-activate-event-handler window))))]


@[MclguiWindow
  method:(resignMainWindow)
  resultType:(:void)
  body:
  [super resignMainWindow]
  (report-errors
      (let ((window (nswindow-window self)))
        (format-trace "-[MclguiWindow resignMainWindow]" window)
        (when window
         (view-deactivate-event-handler window))))]



@[MclguiWindow
  method:(keyDown:(:id)event)
  resultType:(:void)
  body:
  (let ((window (nswindow-window self))
        (key (let ((chars (objcl:lisp-string [event characters])))
               (if (zerop (length chars))
                   nil
                   (aref chars 0)))))
    (format-trace "-[MclguiWindow keyDown:]" window key)
    (when (and window key)
      (view-key-event-handler window key)))]



;;;------------------------------------------------------------
;;; MclguiView

@[NSView subClass:MclguiView
         slots:((view :initform nil
                      :initarg :view
                      :reader nsview-view))]

@[MclguiView
  method:(isFlipped)
  resultType:(:<bool>)
  body:YES]


(defvar *view-draw-contents-from-drawRect* nil)

@[MclguiView
  method:(drawRect:(:<nsr>ect)rect)
  resultType:(:void)
  body:
  (declare (ignore rect))
  (format-trace "-[MclguiView drawRect:]" self (nsview-view self))
  (when (nsview-view self)
    (let ((*view-draw-contents-from-drawRect* t))
      (view-draw-contents (nsview-view self))))]


@[MclguiView
  method: (mouseDown:(:id)theEvent)
  resultType: (:void)
  body:
  (format-trace "-[MclguiView mouseDown:]" self (nsview-view self) theEvent)
  (when (nsview-view self)
    (let ((*current-event* (wrap-nsevent theEvent)))
      (view-click-event-handler (nsview-view self)
                                (nspoint-to-point
                                 (get-nspoint
                                  [self convertPoint:[theEvent locationInWindow]
                                        fromView:*null*])))))]

@[MclguiView
  method: (mouseUp:(:id)theEvent)
  resultType: (:void)
  body:
  (format-trace "-[MclguiView mouseUp:]" self (nsview-view self) theEvent)
  (when (nsview-view self)
    (let ((*current-event* (wrap-nsevent theEvent)))
      (window-mouse-up-event-handler (view-window (nsview-view self)))))]

@[MclguiView
  method: (mouseMoved:(:id)theEvent)
  resultType: (:void)
  body:
  (format-trace "-[MclguiView mouseMoved:]" self (nsview-view self) theEvent)
  (when (nsview-view self)
    (let ((*current-event* (wrap-nsevent theEvent)))
      (window-null-event-handler (view-window (nsview-view self)))))]

@[MclguiView
  method: (mouseDragged:(:id)theEvent)
  resultType: (:void)
  body:
  (format-trace "-[MclguiView mouseDragged]" self (nsview-view self) theEvent)
  (when (nsview-view self)
    (let ((*current-event* (wrap-nsevent theEvent)))
      (window-null-event-handler (view-window (nsview-view self)))))]


;;;------------------------------------------------------------
;;; MclguiEvaluator


@[NSObject subClass:MclguiEvaluator
           slots:((thunk :initform nil
                         :initarg :think
                         :accessor evaluator-thunk))]

@[MclguiView
  method:(evaluate)
  resultType:(:void)
  body:
  (format-trace "evaluate")
  (format-trace "evaluate"  (evaluator-thunk self))
  (funcall (evaluator-thunk self))]


;;;; THE END ;;;;
