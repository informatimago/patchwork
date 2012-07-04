;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               graphics.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements a few graphic primitives.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-07-04 <PJB> Extracted from pw-graphics.lisp for mclgui usage.
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


(defun draw-char (x y cn)
  (draw-string x y (string cn)))

(defun draw-string (x y str)
  (format *trace-output* "~&draw-string                  ~A ~A ~S~%" x y str)
  [(objcl:objcl-string str)
   drawAtPoint: (ns:make-ns-point x y)
   withAttributes: (destructuring-bind (ff ms) ui::*current-font-codes*
                     (multiple-value-bind (descriptor mode) (ui::font-descriptor-from-codes ff ms)
                       (declare (ignore mode))          ; TODO: manage mode (:srcOr …)
                       ;; (print descriptor)
                       [descriptor fontAttributes]))]
  str)


(defun draw-point (x y)
  (format *trace-output* "~&draw-point                   ~A ~A~%" x y)
  (#_NSRectFill (ns:make-ns-rect x y 1 1)))


(defun draw-line (x1 y1 x2 y2)
  (format *trace-output* "~&draw-line                    ~A ~A ~A ~A~%" x1 y1 x2 y2)
  (let ((path [NSBezierPath bezierPath]))
    [path moveToPoint:(ns:make-ns-point x1 y1)]
    [path lineToPoint:(ns:make-ns-point x2 y2)]
    [path stroke]))



(defun draw-rect (x y w h)
  (format *trace-output* "~&draw-rect                    ~A ~A ~A ~A~%" x y w h)
  (#_NSFrameRect (ns:make-ns-rect x y w h)))

(defun fill-rect (x y w h)
  (format *trace-output* "~&fill-rect*                   ~A ~A ~A ~A~%" x y w h)
  (#_NSRectFill (ns:make-ns-rect x y w h)))

(defun erase-rect (x y w h)
  (format *trace-output* "~&erase-rect                   ~A ~A ~A ~A~%" x y w h)
  (#_NSEraseRect (ns:make-ns-rect x y w h)))



(defun draw-ellipse (x y w h)
  (format *trace-output* "~&draw-ellipse                 ~A ~A ~A ~A~%" x y w h)
  [[NSBezierPath bezierPathWithOvalInRect: (ns:make-ns-rect x y w h)] stroke])

(defun fill-ellipse (x y w h)
  (format *trace-output* "~&fill-ellipse                 ~A ~A ~A ~A~%" x y w h)
  [[NSBezierPath bezierPathWithOvalInRect: (ns:make-ns-rect x y w h)] fill])

;;;; THE END ;;;;
