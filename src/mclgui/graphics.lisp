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
(enable-sharp-at-reader-macro)


(defun draw-char (x y cn)
  (draw-string x y (string cn)))

(defun draw-string (x y str)
  (format-trace "draw-string" x y str *current-view* (when *current-view* (view-window *current-view*)))
  [(objcl:objcl-string str)
   drawAtPoint: (ns:make-ns-point x y)
   withAttributes: (destructuring-bind (ff ms) ui::*current-font-codes*
                     (multiple-value-bind (descriptor mode) (ui::font-descriptor-from-codes ff ms)
                       (declare (ignore mode))          ; TODO: manage mode (:srcOr …)
                       ;; (print descriptor)
                       ;; [context setCompositingOperation:(mode-to-compositing-operation (pen-mode pen))] 
                       [descriptor fontAttributes]))]
  str)


(defun draw-point (x y)
  (format-trace "draw-point" x y *current-view* (when *current-view* (view-window *current-view*)))
  (when *current-view*
    (let ((window  (view-window *current-view*)))
      (when window
        (let ((siz     (pen-size (view-pen window))))
          (#_NSRectFill (ns:make-ns-rect x y (point-h siz) (point-v siz))))))))


(defun draw-line (x1 y1 x2 y2)
  (format-trace "draw-line" x1 y1 x2 y2 *current-view* (when *current-view* (view-window *current-view*)))
  (when *current-view*
    (let ((window  (view-window *current-view*)))
      (when window
        (let* ((pen  (view-pen window))
               (size (pen-size pen))
               (path [NSBezierPath bezierPath]))
          [path setLineCapStyle:#$NSSquareLineCapStyle]
          (if (and (= #@(1 1) size)
                   (eq *black-pattern* (pen-state-pattern pen)))
            (let ((sx (point-h size))
                  (sy (point-v size)))
              (unless (< x1 x2)
                (rotatef x1 x2)
                (rotatef y1 y2))
              (if (< y1 y2)
                (progn
                  [path moveToPoint:(ns:make-ns-point x1 y1)]
                  [path lineToPoint:(ns:make-ns-point (+ x1 sx) y1)]
                  [path lineToPoint:(ns:make-ns-point (+ x2 sx) y2)]
                  [path lineToPoint:(ns:make-ns-point (+ x2 sx) (+ y2 sy))]
                  [path lineToPoint:(ns:make-ns-point x2 (+ y2 sy))]
                  [path lineToPoint:(ns:make-ns-point x1 (+ y1 sy))])
                (progn
                  [path moveToPoint:(ns:make-ns-point x1 y1)]
                  [path lineToPoint:(ns:make-ns-point x2 y2)]
                  [path lineToPoint:(ns:make-ns-point (+ x2 sx) y2)]
                  [path lineToPoint:(ns:make-ns-point (+ x2 sx) (+ y2 sy))]
                  [path lineToPoint:(ns:make-ns-point (+ x1 sx) (+ y1 sy))]
                  [path lineToPoint:(ns:make-ns-point x1 (+ y1 sy))]))
              [path closePath]
              [path fill])))))))



(defun draw-rect (x y w h)
  (format-trace "draw-rect" x y w h *current-view* (when *current-view* (view-window *current-view*)))
  (#_NSFrameRect (ns:make-ns-rect x y w h)))

(defun fill-rect (x y w h)
  (format-trace "fill-rect" x y w h *current-view* (when *current-view* (view-window *current-view*)))
  (when *current-view*
    (let ((window  (view-window *current-view*)))
      (when window
        (let* ((pen  (view-pen window)))
          (#_NSRectFillUsingOperation (ns:make-ns-rect x y w h)
                                      (mode-to-compositing-operation (pen-mode pen)))))))
  #-(and) (#_NSRectFill (ns:make-ns-rect x y w h)))

(defun erase-rect (x y w h)
  (format-trace "erase-rect" x y w h *current-view* (when *current-view* (view-window *current-view*)))
  (#_NSEraseRect (ns:make-ns-rect x y w h)))



(defun draw-ellipse (x y w h)
  (format-trace "draw-ellipse-rect" x y w h *current-view* (when *current-view* (view-window *current-view*)))
  [[NSBezierPath bezierPathWithOvalInRect: (ns:make-ns-rect x y w h)] stroke])

(defun fill-ellipse (x y w h)
  (format-trace "fill-ellipse-rect" x y w h *current-view* (when *current-view* (view-window *current-view*)))
  [[NSBezierPath bezierPathWithOvalInRect: (ns:make-ns-rect x y w h)] fill])

;;;; THE END ;;;;
