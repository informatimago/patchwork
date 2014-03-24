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
  ;; (format-trace "draw-string" x y str *current-view* (when *current-view* (view-window *current-view*)))
  ;; (with-fore-color *yellow-color*
  ;;   (let* ((o (view-origin *current-view*))
  ;;          (x (point-h o))
  ;;          (y (point-v o))
  ;;          (s (view-size *current-view*))
  ;;          (w (point-h s))
  ;;          (h (point-v s)))
  ;;     (draw-rect* x y w h)))
  (destructuring-bind (ff ms) *current-font-codes*
    (multiple-value-bind (descriptor mode) (font-descriptor-from-codes ff ms)
      (declare (ignore mode)) ; TODO: manage mode (:srcOr …)
      ;; (print descriptor)
      ;; [context setCompositingOperation:(mode-to-compositing-operation (pen-mode pen))]
      ;; (format-trace "draw-string" x y str [descriptor fontAttributes])
      (multiple-value-bind (a d w l) (font-codes-info ff ms)
        (declare (ignore w l))
        ;; (format-trace "draw-string" a d w l)
        ;; the origin of the bounding box.  topleft in flipped coordinates.
        [(objcl:objcl-string str)
         drawAtPoint: (ns:make-ns-point x (- y a d))
         withAttributes:  [descriptor fontAttributes]])))
  str)



(defun draw-point (x y)
  ;; (format-trace "draw-point" x y *current-view* (when *current-view* (view-window *current-view*)))
  (when *current-view*
    (let ((window  (view-window *current-view*)))
      (when window
        (let ((siz     (pen-size (view-pen window))))
          (#_NSRectFill (ns:make-ns-rect x y (point-h siz) (point-v siz))))))))


(defun draw-line (x1 y1 x2 y2)
  ;; (format-trace "draw-line" x1 y1 x2 y2 *current-view* (when *current-view* (view-window *current-view*)))
  (when *current-view*
    (let ((window  (view-window *current-view*)))
      (when window
        (let* ((pen  (view-pen window))
               (size (pen-size pen))
               (path [NSBezierPath bezierPath]))
          [path setLineCapStyle:#$NSSquareLineCapStyle]
          ;; stroke draws between the pixels, so we'll fill the line always.
          ;; (if (and (= #@(1 1) size)
          ;;          (eq *black-pattern* (pen-state-pattern pen)))
          ;;     (progn
          ;;       [path moveToPoint:(ns:make-ns-point x1 y1)]
          ;;       [path lineToPoint:(ns:make-ns-point x2 y2)]
          ;;       [path stroke])
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
            [path fill]))))))


(defun draw-rect* (x y w h)
  ;; (format-trace "draw-rect*" x y w h *current-view* (when *current-view* (view-window *current-view*)))
  (when *current-view*
    (let ((window  (view-window *current-view*)))
      (when window
        (let* ((pen  (view-pen window))
               (size (pen-size pen)))
          
          (if (and nil (= #@(1 1) size)
                   (eq *black-pattern* (pen-state-pattern pen)))
            (#_NSFrameRect (ns:make-ns-rect x y w h))
            (let ((path [NSBezierPath bezierPath])
                  (sx (point-h size))
                  (sy (point-v size)))
              [path setLineCapStyle:#$NSSquareLineCapStyle]
              ;; external border
              [path moveToPoint:(ns:make-ns-point x y)]
              [path lineToPoint:(ns:make-ns-point (+ x w) y)]
              [path lineToPoint:(ns:make-ns-point (+ x w) (+ y h))]
              [path lineToPoint:(ns:make-ns-point x (+ y h))]
              [path lineToPoint:(ns:make-ns-point x y)]
              ;; internal border
              [path moveToPoint:(ns:make-ns-point (+ x sx)       (+ y sy))]
              [path lineToPoint:(ns:make-ns-point (+ x sx)       (- (+ y h) sy))]
              [path lineToPoint:(ns:make-ns-point (- (+ x w) sx) (- (+ y h) sy))]
              [path lineToPoint:(ns:make-ns-point (- (+ x w) sx) (+ y sy))]
              [path lineToPoint:(ns:make-ns-point (+ x sx)       (+ y sy))]
              [path closePath]
              [path fill])))))))


#-(and)
(loop
  :for mode :in '(:srcCopy :srcOr :srcXor :srcBic
                  :notSrcCopy :notSrcOr :notSrcXor :notSrcBic
                  :patCopy :patOr :patXor :patBic
                  :notPatCopy :notPatOr :notPatXor :notPatBic)
  :do (with-focused-view (front-window)
        (with-pen-state (:pattern *gray-pattern* :size (make-point 10 10)
                                  :mode :srcCopy)
          (draw-rect* 10 20 100 200))
        (with-pen-state (:pattern *gray-pattern* :size (make-point 10 10)
                                  :mode mode)
          (draw-rect* 10 20 100 200)))
  (sleep 3))

#-(and)
(with-focused-view (front-window)
  (with-pen-state (:pattern *light-gray-pattern* :size (make-point 20 10)
                            :mode :srcCopy)
    (draw-rect* 10 20 200 100)))


(defun fill-rect* (x y w h)
  ;; (format-trace "fill-rect*" x y w h *current-view* (when *current-view* (view-window *current-view*)))
  (when *current-view*
    (let ((window  (view-window *current-view*)))
      (when window
        (let* ((pen  (view-pen window)))
          (#_NSRectFillUsingOperation (ns:make-ns-rect x y w h)
                                      (mode-to-compositing-operation (pen-mode pen))))))))

(defun erase-rect* (x y w h)
  ;; (format-trace "erase-rect*" x y w h *current-view* (when *current-view* (view-window *current-view*)))
  (let ((color (unwrap (or (and *current-view*
                                (view-window *current-view*)
                                (slot-value (view-window *current-view*) 'back-color))
                           *background-color*))))
    [NSGraphicsContext saveGraphicsState]
    (unwind-protect
        (progn
          [color setFill]
          (#_NSRectFill (ns:make-ns-rect x y w h)))
      [NSGraphicsContext restoreGraphicsState])))


(defun draw-rect (rect)
  (draw-rect* (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect)))

;; (defun fill-rect (rect)
;;   (fill-rect* (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect)))
;; 
;; (defun erase-rect (rect)
;;   (erase-rect* (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect)))

;; (setf *color-available* t)
;; (with-focused-view (front-window)
;;   (with-back-color *orange-color*
;;    (erase-rect* 0 0 100 200)))
;; (with-focused-view (front-window)
;;   (with-pen-state (:pattern *light-gray-pattern* :size (make-point 20 10))
;;     (with-fore-color *blue-color*
;;       (draw-ellipse 20 20 200 100))))


(defun draw-ellipse (x y w h)
  ;; (format-trace "draw-ellipse-rect" x y w h *current-view* (when *current-view* (view-window *current-view*)))
  (when *current-view*
    (let ((window  (view-window *current-view*)))
      (when window
        (let* ((pen  (view-pen window))
               (size (pen-size pen)))
          (if (and (= #@(1 1) size)
                   (eq *black-pattern* (pen-state-pattern pen)))
            [[NSBezierPath bezierPathWithOvalInRect: (ns:make-ns-rect x y w h)] stroke]
            (let ((path [NSBezierPath bezierPath])
                  (sx (point-h size))
                  (sy (point-v size)))
              ;; TODO: use the pen-pattern
              [path setWindingRule:#$NSEvenOddWindingRule]
              ;; external border
              [path appendBezierPathWithOvalInRect:(ns:make-ns-rect x y w h)]
              ;; internal border
              [path appendBezierPathWithOvalInRect:(ns:make-ns-rect (+ x sx) (+ y sy)
                                                                    (- w sx sx) (- h sy sy))]
              [path fill])))))))


(defun fill-ellipse (x y w h)
  ;; (format-trace "fill-ellipse-rect" x y w h *current-view* (when *current-view* (view-window *current-view*)))
  (when *current-view*
    (let ((window  (view-window *current-view*)))
      (when window
        ;; TODO: use pen-pattern
        [[NSBezierPath bezierPathWithOvalInRect: (ns:make-ns-rect x y w h)] fill]))))

;;;; THE END ;;;;
