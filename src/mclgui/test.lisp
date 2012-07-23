
(in-package "MCLGUI")
(objcl:enable-objcl-reader-macros)


(defparameter *application-handle* nil)
(defparameter *test-menu-title-handle* nil)
(defparameter *test-menu-handle* nil)
(defparameter *test-menu-item-handle* nil)


(defun test/cocoa/menu/1 ()
  
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

  :success)

(let ((viewh [[[NSApplication sharedApplication] mainWindow] contentView])
      (unlock nil))
  ;; (print (list (get-nsrect [viewh frame]) (get-nsrect [viewh bounds])))
  (unwind-protect
       (when (setf unlock [viewh lockFocusIfCanDraw])
         ;; (#_NSEraseRect (unwrap (get-nsrect [viewh bounds])))
         ;; [[NSColor colorWithCalibratedRed:1d0 green:0d0 blue:0d0 alpha:1.0d0] set]
         ;; (#_NSRectFill (ns:make-ns-rect 10 100 200 100))
         [(objcl:objcl-string "Hello")
          drawAtPoint: (ns:make-ns-point 20 20)
          withAttributes: (print (destructuring-bind (ff ms)
                                     ; (font-codes '("Monaco" 9))
                                     ui::*current-font-codes*
                                    (multiple-value-bind (descriptor mode) (ui::font-descriptor-from-codes ff ms)
                                      (declare (ignore mode)) ; TODO: manage mode (:srcOr …)
                                      ;; (print descriptor)
                                      ;; [context setCompositingOperation:(mode-to-compositing-operation (pen-mode pen))] 
                                      [descriptor fontAttributes])))]
         [[NSGraphicsContext currentContext] flushGraphics]
         ;; [[NSColor colorWithCalibratedRed:0.33d0 green:0.33d0 blue:0.33d0 alpha:1.0d0] set]
         ;; (print 'fill)
         ;; (#_NSRectFill (ns:make-ns-rect 10 100 200 100))
         ;; [[NSGraphicsContext currentContext] flushGraphics]
         )
    (when unlock [viewh unlockFocus])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass spring-view (view)
  ((top-spring        :initarg :top-spring        :accessor spring-view-top-spring        :initform nil)
   (vertical-spring   :initarg :vertical-spring   :accessor spring-view-vertical-spring   :initform nil)
   (bottom-spring     :initarg :bottom-spring     :accessor spring-view-bottom-spring     :initform nil)
   (left-spring       :initarg :left-spring       :accessor spring-view-left-spring       :initform nil)
   (horizontal-spring :initarg :horizontal-spring :accessor spring-view-horizontal-spring :initform nil)
   (right-spring      :initarg :right-spring      :accessor spring-view-right-spring      :initform nil)
   (spring-changing   :initform nil))
  (:documentation "Implements the spring resizing of the view."))

(defmethod initialize-instance :after ((view spring-view) &key &allow-other-keys)
  (update-springs view))


(defun sprint-percentages (min-spring mid-spring max-spring pos siz old)
  "Return: min; mid; max."
  (if min-spring
      (if mid-spring
          (if max-spring
              (let* ((min% (/ pos old))
                     (mid% (/ siz old))
                     (max% (- 1 min% mid%)))
                (values min% mid% max%))
              (let* ((min% (/ pos (+ pos siz)))
                     (mid% (/ siz (+ pos siz)))
                     (max% nil))
                (values min% mid% max%)))
          (if max-spring
              (let* ((min% (/ pos (- old siz)))
                     (mid% nil)
                     (max% (1- min%)))
                (values min% mid% max%))
              (values 1 nil nil)))
      (if mid-spring
          (if max-spring
              (let* ((min% nil)
                     (mid% (/ siz (- old pos)))
                     (max% (1- mid%)))
                (values min% mid% max%))
              (values nil 1 nil))
          (values nil nil 1))))


(defun spring (min% mid% max% pos siz old new)
  "RETURN: new-pos; new-siz"
  (if (= old new)
      (values pos siz)
      (if min%
          (if mid%
              (if max%
                  ;; /\/\/\ |---/\/\/\---| /\/\/\
                  (values (* new min%) (* new mid%))
                  ;; /\/\/\ |---/\/\/\---| ------
                  (let* ((width (- new (- old pos siz)))
                         (pos   (* width min%)))
                    (values pos (- width pos))))
              (if max%
                  ;; /\/\/\ |------------| /\/\/
                  (let* ((width (- new (- old siz)))
                         (pos   (* width min%)))
                    (values pos siz))
                  ;; /\/\/\ |------------| ------
                  (values (- new (- old pos)) siz)))
          (if mid%
              (if max%
                  ;; ------ |---/\/\/\---| /\/\/\
                  (let ((width (- new pos)))
                    (values pos (* width mid%)))
                  ;; ------ |---/\/\/\---| ------
                  (let ((margins (- old siz)))
                    (values pos (- new margins))))
              ;; ------ |------------| /\/\/\
              ;; ------ |------------| ------
              (values pos          siz)))))


(defgeneric update-springs (view)
  (:method ((view spring-view))
    (when (view-container view)
      (let* ((old-size (view-size (view-container view)))
             (oh (point-h old-size))
             (ov (point-v old-size))
             (ph (point-h (view-position view)))
             (pv (point-v (view-position view)))
             (sh (point-h (view-size view)))
             (sv (point-v (view-size view))))
        (multiple-value-bind (left horizontal right)
            (sprint-percentages (spring-view-left-spring view)
                                (spring-view-horizontal-spring view)
                                (spring-view-right-spring view)
                                ph sh oh)
          (format-trace "horizontal" left horizontal right)
          (setf (slot-value view 'left-spring)       left
                (slot-value view 'horizontal-spring) horizontal
                (slot-value view 'right-spring)      right))
        (multiple-value-bind (top vertical bottom)
            (sprint-percentages (spring-view-top-spring view)
                                (spring-view-vertical-spring view)
                                (spring-view-bottom-spring view)
                                pv sv ov)
          (format-trace "vertical" top vertical bottom)
          (setf (slot-value view 'top-spring)      top
                (slot-value view 'vertical-spring) vertical
                (slot-value view 'bottom-spring)   bottom))))
    view))


(defmethod (setf spring-view-top-spring)        :after (new-value (view spring-view))
  (declare (ignorable new-value))
  (update-springs view))
(defmethod (setf spring-view-vertical-spring)   :after (new-value (view spring-view))
  (declare (ignorable new-value))
  (update-springs view))
(defmethod (setf spring-view-bottom-spring)     :after (new-value (view spring-view))
  (declare (ignorable new-value))
  (update-springs view))
(defmethod (setf spring-view-left-spring)       :after (new-value (view spring-view))
  (declare (ignorable new-value))
  (update-springs view))
(defmethod (setf spring-view-horizontal-spring) :after (new-value (view spring-view))
  (declare (ignorable new-value))
  (update-springs view))
(defmethod (setf spring-view-right-spring)      :after (new-value (view spring-view))
  (declare (ignorable new-value))
  (update-springs view))

(defmethod add-view-to-container :after ((view spring-view) container)
  (declare (ignorable view container))
  (update-springs view))

(defmethod set-view-position :after ((view spring-view) h &optional v)
  (declare (ignorable view h v))
  (unless (slot-value view 'spring-changing)
   (update-springs view)))

(defmethod set-view-size ((view spring-view) h &optional v)
  (declare (ignorable h v))
  (let ((old-size (view-size view)))
    (call-next-method)
    (unless (slot-value view 'spring-changing)
      (update-springs view))
    (let ((new-size (view-size view)))
      (format-trace "set-view-size" 'spring-view (point-to-list old-size) (point-to-list new-size))
      (when (/= old-size new-size)
        (do-subviews (subview view)
          (container-view-size-changed subview old-size))))))


(defclass spring-window (window)
  ())

;; (defmethod window-size-parts ((view spring-window))
;;   (do-subviews (subview view)
;;     (container-view-size-changed subview old-size))
;;   (call-next-method))


(defmethod set-view-size ((view spring-window) h &optional v)
  (declare (ignorable h v))
  (let ((old-size (view-size view)))
    (call-next-method)
    (let ((new-size (view-size view)))
      (format-trace "set-view-size" 'spring-window (point-to-list old-size) (point-to-list new-size))
      (when (/= old-size new-size)
        (do-subviews (subview view)
          (container-view-size-changed subview old-size))))))



(defgeneric container-view-size-changed (view old-size)
  (:method ((view view) old-size)
    (declare (ignore old-size))
    ;; Do nothing, will be clipped.
    (values))
  (:method ((view spring-view) old-size)
    ;; Change the size of the VIEW according to its spring settings
    ;; and the size change of its content view.
    (let* ((new-size (view-size (view-container view)))
           (oh (point-h old-size))
           (ov (point-v old-size))
           (nh (point-h new-size))
           (nv (point-v new-size))
           (ph (point-h (view-position view)))
           (pv (point-v (view-position view)))
           (sh (point-h (view-size view)))
           (sv (point-v (view-size view))))
      (multiple-value-bind (rph rsh) (spring (spring-view-left-spring view)
                                             (spring-view-horizontal-spring view)
                                             (spring-view-right-spring view)
                                             ph sh oh nh)
        (multiple-value-bind (rpv rsv) (spring (spring-view-top-spring view)
                                               (spring-view-vertical-spring view)
                                               (spring-view-bottom-spring view)
                                               pv sv ov nv)
          (let ((old-changing  (slot-value view 'spring-changing)))
            (unwind-protect
                 (progn
                   (setf (slot-value view 'spring-changing) t)
                   (set-view-position view (round rph) (round rpv))
                   (set-view-size     view (round rsh) (round rsv)))
              (setf (slot-value view 'spring-changing) old-changing))))))))


;; min%   |   view%    | max%
;; ----------------------------> size
;; /\/\/\ |---/\/\/\---| /\/\/\
;;                               min%  = old-pos / old-size
;;                               view% = old-view-size / old-size
;;                               max%  = 1 - min% - view%
;; 
;;                               min% = new-pos / new-size
;;                               ==> new-pos = new-size * old-pos / old-size
;; 
;;                               (min%+view%)=(old-pos+old-view-size)/old-size
;;                               ==> (new-pos + new-view-size) = new-size * (min% + view%)
;;                               ==> new-view-size = new-size * (min% + view%) - new-size * min%
;;                               ==> new-view-size = new-size * view%
;;                               ==> new-view-size = new-size * old-view-size / old-size
;; 
;; /\/\/\ |---/\/\/\---| ------
;;                               min%  = old-pos / (old-pos + old-view-size) 
;;                               view% = 1 - min%
;; 
;;                               new-pos + new-view-size = old-pos + old-view-size - old-size + new-size
;; 
;;                               min%  = new-pos / (new-pos + new-view-size) 
;;                               min%  = new-pos / (old-pos + old-view-size - old-size + new-size) 
;;                               new-pos = min% * (old-pos + old-view-size - old-size + new-size) 
;;                               new-pos = (old-pos / (old-pos + old-view-size)) * (old-pos + old-view-size - old-size + new-size)
;;                               new-view-size =  (old-pos + old-view-size - old-size + new-size) - new-pos
;; 
;; /\/\/\ |------------| /\/\/\
;;                               min% = old-pos / ( old-size - old-view-size) 
;;                               max% = 1 - min%
;; 
;;                               new-pos / (new-size - new-view-size) = min%
;;                               new-pos = old-pos * (new-size - old-view-size) / ( old-size - old-view-size) 
;; 
;; /\/\/\ |------------| ------
;;                               new-pos = old-pos - old-size + new-size
;; 
;;
;; ------ |---/\/\/\---| /\/\/\
;;                               view% = old-view-size / (old-size - old-pos) 
;;                               max%  = 1 - view%
;; 
;;                               new-view-size / (new-size - new-pos) = old-view-size / (old-size - old-pos)
;;                               ==> new-view-size = old-view-size * (new-size - new-pos) / (old-size - old-pos)
;; 
;; ------ |---/\/\/\---| ------
;;                               new-view-size = old-view-size + new-size - old-size
;; 
;; ------ |------------| /\/\/\
;;                               ==> no change
;; 
;; ------ |------------| ------
;;                               ==> no change


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass test-window (spring-window)
  ())


(defgeneric view-mouse-up-event-handler (view)
  (:method ((view view))
    (dovector (subview (view-subviews view))
      (view-mouse-up-event-handler subview))))

(defmethod window-mouse-up-event-handler ((window test-window))
  (view-mouse-up-event-handler window))


(defgeneric view-mouse-moved-event-handler (view where)
  (:method ((view view) where)
    (loop
      :for subview :across (reverse (view-subviews view))
      :when (point-in-click-region-p subview where)
      :do (progn
            (view-mouse-moved-event-handler subview (convert-coordinates where view subview))
            (return t))
      :finally (return nil))))

(defmethod window-null-event-handler ((window test-window))
  (view-mouse-moved-event-handler window (view-mouse-position window)))




(defvar *id* 0)


(defclass test-view (spring-view)
  ((id      :initform (incf *id*) :accessor test-view-id      :initarg :id)
   (lastpos :initform nil         :accessor test-view-lastpos)))


(defmethod print-object ((view test-view) stream)
  (print-parseable-object (view stream :type t :identity t)
                          id
                          (:view-position        (point-to-list (view-position view)))
                          (:view-size            (point-to-list (view-size view)))
                          (:view-scroll-position (point-to-list (view-scroll-position view)))))


(defmethod view-click-event-handler ((view test-view) where)
  (format-trace "view-click-event-handler" 'test-view :where (point-to-list where) :size (point-to-list (view-size view)))
  (setf (test-view-lastpos view) where)
  (with-focused-view view
    (draw-point (point-h where) (point-v where))))


(defmethod view-mouse-moved-event-handler ((view test-view) where)
  (format-trace "view-mouse-moved-event-handler" 'test-view :down  (mouse-down-p) :lastpos (and (test-view-lastpos view) (point-to-list  (test-view-lastpos view))) :where (point-to-list where) :size (point-to-list (view-size view)))
  (when (mouse-down-p)
    (with-focused-view view
      (let ((lastpos (test-view-lastpos view)))
        (when lastpos
          (draw-line (point-h lastpos) (point-v lastpos)
                     (point-h where) (point-v where)))
        (setf (test-view-lastpos view) where)))))


(defmethod view-mouse-up-event-handler ((view test-view))
  (setf (test-view-lastpos view) nil))


(defmethod view-draw-contents ((view test-view))
  (with-focused-view view
    (flet ((line (x1 y1 x2 y2)
             (draw-line x1 y1 x2 y2)))
     (let* ((pos (view-position view))
            (siz (view-size     view))
            (br  (add-points pos siz)))
       (format-trace "drawing test-view contents" (point-to-list pos) (point-to-list  siz) (point-to-list  br))
       (erase-rect 0 0 (point-h siz) (point-v siz))
       (draw-rect 3 3  (- (point-h siz) 6) (- (point-v siz) 6))
       ;; (line (+ (point-h pos) 10) (+ (point-v pos) 10) (- (point-h br)  10) (+ (point-v pos) 10))
       ;; (line (+ (point-h pos) 10) (+ (point-v pos) 10) (+ (point-h pos) 10) (- (point-v br)  10))
       ;; (line (+ (point-h pos) 10) (- (point-v br)  10) (- (point-h br)  10) (- (point-v br)  10))
       ;; (line (- (point-h br)  10) (+ (point-v pos) 10) (- (point-h br)  10) (- (point-v br)  10))
       ))))


(defun test/1 ()
  ;; 1 2 3
  ;; 4 5 6
  ;; 7 8 9
  (labels ((make-test-subviews (h v level)
             (let* ((h/3  (truncate h 3))
                    (v/3  (truncate v 3))
                    (2h/3 (* 2 h/3))
                    (2v/3 (* 2 v/3))
                    (view1 (make-instance 'test-view
                               :view-position (make-point   0   0)
                               :view-size     (make-point h/3 v/3)
                               :top-spring  nil :vertical-spring   nil :bottom-spring t
                               :left-spring nil :horizontal-spring nil :right-spring  t
                               :id (+ (* (- 1 level) 10) 1)))
                    (view2 (make-instance 'test-view
                               :view-position (make-point h/3   0)
                               :view-size     (make-point h/3 v/3)
                               :top-spring  nil :vertical-spring   nil :bottom-spring t
                               :left-spring t   :horizontal-spring t   :right-spring  t
                               :id (+ (* (- 1 level) 10) 2)))
                    (view3 (make-instance 'test-view
                               :view-position (make-point 2h/3  0)
                               :view-size     (make-point h/3 v/3)
                               :top-spring  nil :vertical-spring   nil :bottom-spring t
                               :left-spring t   :horizontal-spring nil :right-spring  nil
                               :id (+ (* (- 1 level) 10) 3)))

                    (view4 (make-instance 'test-view
                               :view-position (make-point   0 v/3)
                               :view-size     (make-point h/3 v/3)
                               :top-spring  t   :vertical-spring   t   :bottom-spring t
                               :left-spring nil :horizontal-spring nil :right-spring  t
                               :id (+ (* (- 1 level) 10) 4)))
                    (view5 (make-instance 'test-view
                               :view-position (make-point h/3 v/3)
                               :view-size     (make-point h/3 v/3)
                               :top-spring  t   :vertical-spring   t   :bottom-spring t
                               :left-spring t   :horizontal-spring t   :right-spring  t
                               :view-subviews (if (plusp level)
                                                  (make-test-subviews (/ h 3) (/ v 3) (1- level))
                                                  '())                               
                               :id (+ (* (- 1 level) 10) 5)))
                    (view6 (make-instance 'test-view
                               :view-position (make-point 2h/3 v/3)
                               :view-size     (make-point h/3 v/3)
                               :top-spring  t   :vertical-spring   t   :bottom-spring t
                               :left-spring t   :horizontal-spring nil :right-spring  nil
                               :id (+ (* (- 1 level) 10) 6)))

                    (view7 (make-instance 'test-view
                               :view-position (make-point   0 2v/3)
                               :view-size     (make-point h/3 v/3)
                               :top-spring  t   :vertical-spring   nil :bottom-spring nil
                               :left-spring nil :horizontal-spring nil :right-spring  t
                               :id (+ (* (- 1 level) 10) 7)))
                    (view8 (make-instance 'test-view
                               :view-position (make-point h/3 2v/3)
                               :view-size     (make-point h/3 v/3)
                               :top-spring  t   :vertical-spring   nil :bottom-spring nil
                               :left-spring t   :horizontal-spring t   :right-spring  t
                               :id (+ (* (- 1 level) 10) 8)))
                    (view9 (make-instance 'test-view
                               :view-position (make-point 2h/3 2v/3)
                               :view-size     (make-point h/3 v/3)
                               :top-spring  t   :vertical-spring   nil :bottom-spring nil
                               :left-spring t   :horizontal-spring nil :right-spring  nil
                               :id (+ (* (- 1 level) 10) 9))))
               (list view1 view2 view3
                     view4 view5 view6
                     view7 view8 view9))))
    (make-instance 'test-window
        :window-title  "Test Window"
        :view-position (make-point 30 30)
        :view-size     (make-point 300 300)
        :view-subviews (make-test-subviews 300 300 1))))


;; (test/1) 
;; (view-subviews (front-window))
;; [[[(handle(front-window)) contentView] subviews] count]
;;(dump-nswindow-subviews (handle (first (windows))))

#||
(make-instance 'test-window
    :window-title  "Test Window"
    :view-position (make-point 1000 100)
    :view-size     (make-point 30 30))

(window-close (front-window))
(set-view-scroll-position (front-window) -10 -10)
(with-focused-view (front-window)
  (fill-rect 1 1 28 28))

||#

(defun test/draw-string (&optional (string "Hello World!"))
  (com.informatimago.common-lisp.cesarum.utility:tracing
   (set-view-font (front-window) '("Monaco" 9 :srcCopy))
   (with-font-focused-view (aref (view-subviews (front-window)) 0)
     (draw-string 10 20 string))))

;; (test/draw-string)


;;; write a test with subviews to check the bounds.

(defclass lv (view)
  ((pos :initform (make-point 0 0)
        :accessor little-pos)
   (vel :initform (make-point (random 10) (random 10))
        :accessor little-vel)))


(defmethod update-ball ((self lv))
  (let* ((pos (view-position self))
         (siz (view-size self)))
    (setf (little-pos self) (add-points (little-pos self) (little-vel self)))
    (when (and (< (point-h (little-pos self)) (point-h pos))
               (minusp (point-h (little-vel self))))
      (setf (little-vel self) (make-point (- (point-h (little-vel self)))
                                           (point-v (little-vel self)))))
    (when (and (< (+ (point-h pos) (point-h siz)) (point-h (little-pos self)))
               (plusp (point-h (little-vel self))))
      (setf (little-vel self) (make-point (- (point-h (little-vel self)))
                                          (point-v (little-vel self)))))
    (when (and (< (point-v (little-pos self)) (point-v pos))
               (minusp (point-v (little-vel self))))
      (setf (little-vel self) (make-point (point-h (little-vel self))
                                          (- (point-v (little-vel self))))))
    (when (and (< (+ (point-v pos) (point-v siz)) (point-v (little-pos self)))
               (plusp (point-v (little-vel self))))
      (setf (little-vel self) (make-point (point-h (little-vel self))
                                          (- (point-v (little-vel self))))))))



(defmethod view-draw-contents ((view lv))
  (with-focused-view view
    (let ((pos (view-position view))
          (siz (view-size     view))
          (bal (little-pos    view)))
      (erase-rect (point-h pos) (point-v pos) (point-h siz) (point-v siz))
      (draw-rect  (point-h pos) (point-v pos) (point-h siz) (point-v siz))
      (fill-ellipse (- (point-h bal) 4) (- (point-v bal) 4) 8 8))))


#||
(add-subviews (front-window) (make-instance 'lv
                               :view-position (make-point 20 20)
                               :view-size     (make-point 100 100)))

(loop repeat 100 do
     (let ((view (aref (view-subviews (front-window)) 1)))
       (update-ball view)
       (view-draw-contents view)))



(with-focused-view (first (windows))
  (draw-line 20 20  250 150))

(with-focused-view (first (windows))
  (with-pen-state (:pattern *gray-pattern* :size (make-point 20 10))
    (draw-line 20 20  200 100) ))

(with-focused-view (first (windows)) 
  (with-pen-state (:pattern *light-gray-pattern* :size (make-point 20 10))
    (draw-rect 20 200 200 300)))

(with-focused-view (first (windows)) 
  (with-pen-state (:pattern *light-gray-pattern* :size (make-point 20 10))
    (fill-rect 20 200 200 300)))


(with-focused-view (first (windows)) 
  (erase-rect 220 200 300 300)
  (with-pen-state (:pattern *light-gray-pattern* :size (make-point 20 10))
    (fill-ellipse 220 200 200 100))
  (with-pen-state (:pattern *dark-gray-pattern* :size (make-point 20 10))
    (fill-ellipse 250 300 100 50)))

(with-focused-view (first (windows)) 
  (with-pen-state (:pattern *light-gray-pattern* :size (make-point 20 10))
    (erase-rect 40 220 160 260)))

(with-focused-view (first (windows)) 
  (with-back-color *black-pattern*
    (with-pen-state (:pattern *light-gray-pattern* :size (make-point 20 10))
      (erase-rect 40 220 160 260))))


;; (dump-nswindow-subviews (handle (first (windows))))

(aref (view-subviews (front-window)) 8)

(with-focused-view (front-window)
  (draw-rect 32 48 127 102)
  (draw-string 10 20 "Hello")
  (draw-string 10 30 "World")
  (draw-string 10 80 (format nil "How do ~%You Do?")))

(with-focused-view (front-window)
  (erase-rect 0 0 1000 1000))
(move-to (front-window) 0 10)
(format (front-window)  "~%How do ~%You Do?~%")
(format (front-window)  "How do ~%You Do?~%")
(format (front-window)  "How do you Do?")
(terpri (front-window))
(princ "Hello" (front-window))
(point-to-list (pen-position (view-pen (front-window))))
(0 185)
(222 176)

(let ((view (front-window)))
(move-to view 0 (+ (point-v (pen-position (view-pen view)))
                     (font-line-height))))

(with-focused-view (aref (view-subviews (front-window)) 8)
  (draw-rect 10 10 107 82))

(test-view :view-position (293 239) :view-size (128 125) :view-scroll-position (0 0) "#x30200367203D")
#((test-view :view-position (0 0) :view-size (100 100) :view-scroll-position (0 0) "#x30200365294D")
  (test-view :view-position (128 0) :view-size (128 100) :view-scroll-position (0 0) "#x3020036524ED")
  (test-view :view-position (293 0) :view-size (128 100) :view-scroll-position (0 0) "#x302003671EAD")
  (test-view :view-position (0 125) :view-size (100 125) :view-scroll-position (0 0) "#x302003671D1D")
  (test-view :view-position (128 125) :view-size (128 125) :view-scroll-position (0 0) "#x302003671B8D")
  (test-view :view-position (293 125) :view-size (128 125) :view-scroll-position (0 0) "#x3020036719FD")
  (test-view :view-position (0 239) :view-size (100 125) :view-scroll-position (0 0) "#x30200367235D")
  (test-view :view-position (128 239) :view-size (128 125) :view-scroll-position (0 0) "#x3020036721CD")
  (test-view :view-position (293 239) :view-size (128 125) :view-scroll-position (0 0) "#x30200367203D"))

(test/1)

(defun test/mv ()
  (values 1 2 3))
(trace test/mv)
(test/mv)
(setf *color-available* t)

(trace mclgui:draw-string mclgui::draw-string-in-rect
       mclgui::font-code-draw-string
       mclgui::font-codes-string-width
       mclgui::stream-tyo)

||#
