
(defpackage "MCLGUI.TEST.GRAPHICS"
  (:use  "COMMON-LISP" "MCLGUI"))
(in-package "MCLGUI.TEST.GRAPHICS")

(defun draw-char (x y cn)
  (niy draw-char '(x y cn))
  ;; (#_MoveTo :long (make-point x y))
  ;; (#_DrawChar cn)
  )


(defun draw-string (x y str)
  (niy draw-string x y str)
  ;; (#_MoveTo :long (make-point x y)) 
  ;; (dotimes (i (length str)) (#_DrawChar (elt str i)))
  )


(defun draw-line (x1 y1 x2 y2)
  (let ((path [NSBezierPath bezierPath]))
    [path moveToPoint:(ns:make-ns-point x1 y1)]
    [path lineToPoint:(ns:make-ns-point x2 y2)]
    [path stroke]))

(defun erase-rect* (x y w h)
  (#_NSEraseRect (ns:make-ns-rect x y w h)))

(defun draw-rect* (x y w h)
  (#_NSFrameRect (ns:make-ns-rect x y w h)))

(defun fill-rect* (x y w h)
  (#_NSRectFill (ns:make-ns-rect x y w h)))

(defun draw-point (x y)
  (#_NSRectFill (ns:make-ns-rect x y 1 1)))


(defun draw-ellipse (x y w h)
  [[NSBezierPath bezierPathWithOvalInRect: (ns:make-ns-rect x y w h)] stroke])

(defun fill-ellipse (x y w h)
  [[NSBezierPath bezierPathWithOvalInRect: (ns:make-ns-rect x y w h)] fill])





(defclass graphic-view (view)
  ())


(defmethod draw-contents ((view graphic-view))
  (with-focused-view view
    (view-draw-contents view)))

(defmethod view-draw-contents ((view graphic-view))
  (draw-rect* 10 10 290 190)
  (fill-rect* 13 13 287 187))


(defun create-window (&optional (title "Graphic"))
  (let ((wind (make-instance 'window
                  :view-position (make-point 30 30)
                  :view-size     (make-point 300 200)
                  :window-title  title))
        (view (make-instance 'graphic-view
                  :view-position (make-point 0 0)
                  :view-size     (make-point 300 200))))
    (set-view-container view wind)
    wind))


