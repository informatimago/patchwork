(in-package "MCLGUI")



(defun draw-char (x y cn)
  (draw-string x y (string cn)))

(defun draw-string (x y str)
  (format-trace "draw-string" x y str)
  [(objcl:objcl-string str) drawAtPoint: (ns:make-ns-point x y) withAttributes: ui::*null*])


(defun draw-line (x1 y1 x2 y2)
  (format-trace "draw-line" x1 y1 x2 y2)
  (let ((path [NSBezierPath bezierPath]))
    [path moveToPoint:(ns:make-ns-point x1 y1)]
    [path lineToPoint:(ns:make-ns-point x2 y2)]
    [path stroke]))

(defun erase-rect (x y w h)
  (format-trace "erase-rect" x y w h)
  (#_NSEraseRect (ns:make-ns-rect x y w h)))

(defun frame-rect (x y w h)
  (format-trace "frame-rect" x y w h)
  (#_NSFrameRect (ns:make-ns-rect x y w h)))

(defun draw-rect (x y w h)
  (format-trace "draw-rect" x y w h)
  (#_NSFrameRect (ns:make-ns-rect x y w h)))

(defun fill-rect* (x y w h)
  (format-trace "fill-rect*" x y w h)
  (#_NSRectFill (ns:make-ns-rect x y w h)))

(defun draw-point (x y)
  (format-trace "draw-point                   ~A ~A~%" x y)
  (#_NSRectFill (ns:make-ns-rect x y 1 1)))


(defun draw-ellipse (x y w h)
  (format-trace "draw-ellipse" x y w h)
  [[NSBezierPath bezierPathWithOvalInRect: (ns:make-ns-rect x y w h)] stroke])

(defun fill-ellipse (x y w h)
  (format-trace "fill-ellipse" x y w h)
  [[NSBezierPath bezierPathWithOvalInRect: (ns:make-ns-rect x y w h)] fill])





(defparameter *application-handle* nil)
(defparameter *test-menu-title-handle* nil)
(defparameter *test-menu-handle* nil)
(defparameter *test-menu-item-handle* nil)


(defun test ()
  
  (setf *application-handle*
        (or *application-handle*
            [NSApplication sharedApplication]))

  (setf *test-menu-title-handle*
        (or *test-menu-title-handle*
            [[NSMenuItem alloc] initWithTitle:@"Test"
             action:*null*
             keyEquivalent:@"T"]))

  (setf *test-menu-handle*
        (or *test-menu-handle*
            [[NSMenu alloc] initWithTitle:@"Test"]))
  [*test-menu-title-handle* setSubmenu:*test-menu-handle*]
  
  (setf *test-menu-item-handle*
        (or *test-menu-item-handle*
            [[NSMenuItem alloc] initWithTitle:@"Test Item"
             action:*null*
             keyEquivalent:@"T"]))
  [*test-menu-handle* addItem:*test-menu-item-handle*]
  
  [[*application-handle* mainMenu] addItem:*test-menu-title-handle*]
  
  [*test-menu-item-handle* setKeyEquivalent: @"t"]
  [*test-menu-item-handle* setState:1]
  
  )





(defclass test-window (window)
  ())

(defmethod set-view-size ((window test-window) h &optional v)
  (declare (ignorable h v))
  (let ((old-size (view-size window)))
    (call-next-method)
    (let ((new-size (view-size window)))
      (when (/= old-size new-size)
        (let ((diff (subtract-points new-size old-size))
              (subview (aref (view-subviews window) 0)))
          (set-view-size subview (add-points (view-size subview) diff)))))))


(defclass test-view (view)
  ())

(defmethod view-draw-contents ((view test-view))
  (with-focused-view view
    (flet ((line (x1 y1 x2 y2)
             (draw-line x1 y1 (- x2 x1) (- y2 y1))))
     (let* ((pos (view-position view))
            (siz (view-size     view))
            (br  (add-points pos siz)))
       (format-trace "drawing test-view contents" (point-to-list pos) (point-to-list  siz) (point-to-list  br))
       (line (+ (point-h pos) 10) (+ (point-v pos) 10) (- (point-h br)  10) (+ (point-v pos) 10))
       (line (+ (point-h pos) 10) (+ (point-v pos) 10) (+ (point-h pos) 10) (- (point-v br)  10))
       (line (+ (point-h pos) 10) (- (point-v br)  10) (- (point-h br)  10) (- (point-v br)  10))
       (line (- (point-h br)  10) (+ (point-v pos) 10) (- (point-h br)  10) (- (point-v br)  10))))))

(defun test/1 ()
  (let* ((view (make-instance 'test-view
                   :view-position (make-point 0 0)
                   :view-size     (make-point 200 100)))
         (win  (make-instance 'test-window
                   :window-title  "Test Window"
                   :view-position (make-point 30 30)
                   :view-size     (make-point 200 100)
                   :view-subviews (list view))))
    win))



(defun test/draw-string ()
 (set-view-font  (first (windows)) '("Times" 12))
 (with-font-focused-view (first (windows))
   (pw::draw-string 10 20 "Hello World")))


(view-subviews) (front-window)



;; write a test with subviews to check the bounds.
(defclass lv (view)
  ((pos :initform (make-point 0 0)
        :accessor little-pos)
   (vel :initform (make-point (random 10) (random 10))
        :accessor little-vel)))


(defmethod update-ball ((self lv))
  (let* ((pos (view-position self))
        (siz (view-size self))
        (br  (add-points pos siz)))
   (setf (little-pos self) (add-points (little-pos self) (little-vel self)))
   (when (and (< (point-h (little-pos self)) (nsrect-x r))
              (minusp (point-h (little-vel self))))
     (setf  (little-vel self) (make-point (- (point-h (little-vel self)))
                                          (point-v (little-vel self)))))
   (when (and (< (+ (nsrect-x r) (nsrect-width r)) (point-h (little-pos self)))
              (plusp (point-h (little-vel self))))
     (setf (little-vel self) (make-point (- (point-h (little-vel self)))
                                         (point-v (little-vel self)))))
   (when (and (< (point-v (little-pos self)) (nsrect-y r))
              (minusp (point-v (little-vel self))))
     (setf (little-vel self) (make-point (point-h (little-vel self))
                                         (- (point-v (little-vel self))))))
   (when (and (< (+ (nsrect-y r) (nsrect-height r)) (point-v (little-pos self)))
              (plusp (point-v (little-vel self))))
     (setf (little-vel self) (make-point (point-h (little-vel self))
                                         (- (point-v (little-vel self))))))))

(let ((lv (make-instance 'lv)))
  (loop repeat 100 do
   (print lv)
   (update-ball lv (make-nsrect :x 10 :y 10 :width 80 :height 80))))
