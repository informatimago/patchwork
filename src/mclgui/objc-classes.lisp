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


(defmacro frame (call)
  (let ((vframe (gensym)))
    `(oclo:slet ((,vframe ,call)) 
               (values
                (ns:ns-rect-x ,vframe)
                (ns:ns-rect-y ,vframe)
                (ns:ns-rect-width  ,vframe)
                (ns:ns-rect-height ,vframe)))))


(defun main-screen-frame ()
  "
RETURN:         Position and size of the main screen.
"
  (multiple-value-bind (x y w h) (frame [[NSScreen mainScreen] frame])
    (values (make-point (round x) (round y))
            (make-point (round w) (round h)))))


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





;;; Window Delegate

@[NSObject subClass:MclguiWindowDelegate
           slots:((window :initform nil
                          :initarg :window
                          :accessor delegate-window))]


@[MclguiWindowDelegate
  method:(windowDidMove:(:id)nsnotification)
  resultType:(:void)
  body:
  (declare (ignore nsnotification))
  (let* ((window (delegate-window self))
         (handle (handle window)))
    (format *trace-output* "~&window did move         ~S~%" window)
    (setf (slot-value window 'view-position)
          (nswindow-to-window-position (multiple-value-list (frame [handle frame]))
                                       (view-size window))))]



@[MclguiWindowDelegate
  method:(windowDidResize:(:id)nsnotification)
  resultType:(:void)
  body:
  (declare (ignore nsnotification))
  (let* ((window (delegate-window self))
         (handle (handle window)))
    (format *trace-output* "~&window did resize       ~S~%" window)
    (multiple-value-bind (x y w h) (frame [handle frame])
      (setf (slot-value window 'view-size) (make-point (round w) (round h)))))]


@[MclguiWindowDelegate
  method:(windowWillClose:(:id)nsnotification)
  resultType:(:void)
  body:
  (declare (ignore nsnotification))
  (let* ((window (delegate-window self)))
    (format *trace-output* "~&window will close       ~S~%" window)
    (window-close-event-handler window))]


@[MclguiWindowDelegate
  method:(windowDidBecomeMain:(:id)nsnotification)
  resultType:(:void)
  body:
  (declare (ignore nsnotification))
  (let* ((window (delegate-window self)))
    (format *trace-output* "~&window did become main  ~S~%" window)
    ;; TODO: move after windoids.
    (delete-from-list *window-list* window)
    (insert-into-list *window-list* 0 window)
    (view-activate-event-handler window))]


@[MclguiWindowDelegate
  method:(windowDidResignMain:(:id)nsnotification)
  resultType:(:void)
  body:
  (declare (ignore nsnotification))
  (let* ((window (delegate-window self)))
    (format *trace-output* "~&window did resign main  ~S~%" window)
    (view-deactivate-event-handler window))]




;;; MclguiView

@[NSView subClass:MclguiView
         slots:((view :initform nil
                      :initarg :view
                      :accessor nsview-view))]

@[MclguiView
  method:(isFlipped)
  resultType:(:<bool>)
  body:t]


@[MclguiView
  method:(drawRect:(:<NSR>ect)rect)
  resultType:(:void)
  body:
  (declare (ignorable rect))
  (format *trace-output* "~&view drawRect:          ~S ~S~%"  (nsview-view self) rect)
  (view-draw-contents (nsview-view self))]


;;;; THE END ;;;;
