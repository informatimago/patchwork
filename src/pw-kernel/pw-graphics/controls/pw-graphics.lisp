;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;=========================================================
;;;;
;;;;  PATCH-WORK
;;;;  By Mikael Laurson, Jacques Duthen, Camilo Rueda.
;;;;  © 1986-1992 IRCAM 
;;;;
;;;;=========================================================
(in-package :pw)

;=================================================================================================
(export '(draw-char draw-string draw-line draw-rect fill-rect* draw-point draw-ellipse
         fill-ellipse with-pen-state x y w h))
;=================================================================================================
;(with-pen-saved   
;  (#_PenMode :word (position :patxor *pen-modes*))
;  (#_PenPat :ptr *gray-pattern*))


(defun draw-char (x y cn)  
  (#_MoveTo :long (make-point x y))
  (#_DrawChar cn))


;;;  GA 17/5/94
;;; this crashes sometimes due to the fact that _drawString builds the image
;;; in the stack. 
;(defun draw-string (x y str)  
;   (with-pstrs ((s str))
;     (#_MoveTo :long (make-point x y))
;     (#_DrawString :ptr s)))

(defun draw-string (x y str)
  (#_MoveTo :long (make-point x y)) 
  (dotimes (i (length str)) (#_DrawChar (elt str i))))


(defun draw-line (x1 y1 x2 y2)  
   (#_MoveTo :long (make-point x1 y1))
   (#_LineTo :long (make-point x2 y2)))

(defun draw-rect (x y w h) 
    (CCL:rlet ((r :rect :left x :right (+ x w) :top y :bottom (+ y h)))
      (#_FrameRect :ptr r)))

(defun fill-rect* (x y w h) 
    (CCL:rlet ((r :rect :left x :right (+ x w) :top y :bottom (+ y h)))
      (#_PaintRect :ptr r)))

(defun draw-point (x y)  
  (let ((p (CCL:make-point x y)))
      (#_MoveTo :long p)
      (#_LineTo :long p)))

(defun draw-ellipse (x y w h)  
  (CCL:rlet ((r :rect :left (- x w) :right (+ x w) :top (- y h) :bottom (+ y h)))
     (#_FrameOval :ptr r)))

(defun fill-ellipse (x y w h)  
  (CCL:rlet ((r :rect :left (- x w) :right (+ x w) :top (- y h) :bottom (+ y h)))
     (#_PaintOval :ptr r)))

; let-window-pen,let-window-font
; with-font-codes  
   
(defun port-set-pen-state (&key location size mode pattern)
  (rlet ((ps :PenState))
    (#_GetPenState ps)
    (when location
      (rset ps PenState.pnLoc location))
    (when size
      (rset ps PenState.pnSize size))
    (when mode
      (rset ps PenState.pnMode (position mode *pen-modes*)))
    (when pattern
      (rset ps PenState.pnPat pattern))
    (#_SetPenState ps)))

(defmacro with-pen-state ((&rest states) &body body)
  (let ((ps (gensym)))
    `(rlet ((,ps :PenState))
       (require-trap #_GetPenState ,ps)
       (unwind-protect
         (progn
           (port-set-pen-state ,@states)
           ,@body)
         (require-trap #_SetPenState ,ps)))))

(defun make-pattern (&rest bytes)
  (let ((res (#_NewPtr 8))
        (i 0))
    (dolist (b bytes)
      (%put-byte res b i)
      (if (>= (incf i) 8) (return)))
    (loop 
      (unless (< i 8) (return))
      (%put-byte res 0 i)
      (incf i))
    res))



(defvar *r-view-temp-region* nil)
(def-load-pointers r-view-temp-region ()
  (ccl::without-interrupts (setq *r-view-temp-region* (#_NewRgn))))

;======================================

(defun grow-gray-rect (anchor float port limit)
  (setq float (add-points float anchor))
  (let* ((old-mouse (view-mouse-position nil))
         (new-mouse old-mouse)
         (offset (subtract-points float old-mouse))
         (delta (add-points offset old-mouse)))
    (when limit
      (setq limit (add-points anchor
                              (make-point limit limit))))
    (with-port port
      (with-pen-saved
        (with-clip-rect (rref port :grafport.portrect)
          (#_PenMode :word (position :patxor *pen-modes*))
          (#_PenPat :ptr *gray-pattern*)
          (rlet ((rect :rect :topleft anchor
                       :bottomright float)
                 (event :eventRecord))
            (#_frameRect :ptr rect)              ;draw the rectangle first time
            ;repeat while the button is down
            (ccl::while (mouse-down-p)
              (setq new-mouse (view-mouse-position nil))
              (unless (eq old-mouse new-mouse)  ;has the mouse moved?
                (#_FrameRect :ptr rect)          ;erase the old rect
                (setq delta (add-points offset new-mouse))
                (when limit (setq delta (point-max delta limit)))
                (#_pt2rect :long anchor
                          :long delta
                          :ptr rect)
                (#_FrameRect :ptr rect)          ;draw the new rect
                (get-next-event event nil 0 1)
                (setq old-mouse new-mouse)))
            (#_FrameRect :ptr rect)              ;a final erasure
            delta))))))

(defun point-max (point-1 point-2)
    (make-point (max (point-h point-1) (point-h point-2))
                (max (point-v point-1) (point-v point-2))))


;======================================
; simple-view methods

(defmethod x ((self simple-view)) (point-h (view-position self)))
(defmethod y ((self simple-view)) (point-v (view-position self)))
(defmethod w ((self simple-view)) (point-h (view-size self)))
(defmethod h ((self simple-view)) (point-v (view-size self)))
(defmethod x+w ((self simple-view)) (+ (x self)(w self)))
(defmethod y+h ((self simple-view)) (+ (y self)(h self)))

(defmethod active-point ((self simple-view)))
(defmethod active-chord ((self simple-view)))
(defmethod active-note ((self simple-view)))
(defmethod reset-active-point ((self simple-view)))
(defmethod reset-active-chord ((self simple-view)))
(defmethod view-mouse-dragged ((self simple-view) mouse)
  (declare (ignore mouse)))
(defmethod view-mouse-moved ((self simple-view) mouse)
  (declare (ignore mouse)))
(defmethod view-mouse-up ((self simple-view)))
; check !

(defmethod view-contains-point-p+self ((self simple-view) mouse)
  (when (view-contains-point-p self mouse)
    self))

(defmethod erase-draw-contents ((self simple-view))
  (let* ((pos (view-position self))
        (end (add-points pos (view-size self))))
    (rlet ((r :rect
              :topleft pos
              :bottomright end))
      (#_EraseRect r))))

(defmethod erase-view-inside-rect ((self simple-view))
  (with-pen-state (:pattern *white-pattern* :mode :patcopy)
    (fill-rect* 1 1 (- (w self) 2)(- (h self) 2))))

(defmethod erase-view-inside-rect ((self view))
  (with-pen-state (:pattern *white-pattern* :mode :patcopy)
    (fill-rect* (1+ (point-h (view-scroll-position self))) 
                (1+ (point-v (view-scroll-position self))) (- (w self) 2)(- (h self) 2))))

(defmethod erase+view-draw-contents ((self simple-view))
  (with-focused-view self
    (erase-view-inside-rect self))
  (view-draw-contents self))

(defmethod view-draw-out-line ((self simple-view))
  (let ((topleft (view-position  self)))
    (rlet ((rect :rect :topleft topleft
                 :bottomright (add-points topleft (view-size self))))
      (#_FrameRect rect))))

(defun inside-rectangle? (x1 y1 x y w h)
  (let ((topleft (make-point x y)))
    (rlet ((rect :rect :topleft (make-point x y)
                 :bottomright (add-points topleft (make-point w h))))
     (point-in-rect-p rect x1 y1))))

(defmethod view-window-grown ((self simple-view)))

(defmethod dmove-view-scroll-position ((self view) dx dy)
  (set-view-scroll-position self (+ (x self) dx)(+ (y self) dy)))


;(time (repeat 1000 (inside-rectangle? 77 77  50 60 28 18)))
;================================================
;pop-up-menu

(defvar *pw-pop-menu-window* ())
(defvar *pw-pop-menu-window-TABLE-SEQUENCE* ())

(defun make-pop-up-menu-instance ()
  (setf *pw-pop-menu-window*
     (MAKE-INSTANCE 'DIALOG
               :WINDOW-TYPE :DOCUMENT 
               :VIEW-POSITION #@(426 42)
               :VIEW-SIZE #@(118 128)
               :CLOSE-BOX-P NIL
               :VIEW-FONT '("Chicago" 12 :SRCOR :PLAIN)
               :window-show nil
               :window-title "Browse"
               :VIEW-SUBVIEWS
               (LIST (MAKE-DIALOG-ITEM 'SEQUENCE-DIALOG-ITEM
                                       #@(2 24)
                                       #@(114 95)
                                       "Untitled"
                                       #'(LAMBDA
                                          (ITEM)
                                          (window-hide *pw-pop-menu-window*)
                                          (call-pw-pop-up-function 
                                            (cell-contents item 
                                             (car (selected-cells ITEM)))))
                                       :VIEW-FONT '("Monaco" 9 :SRCCOPY :PLAIN)
                                       :CELL-SIZE #@(200 12)
                                       :TABLE-HSCROLLP NIL
                                       :TABLE-VSCROLLP T
                                       :TABLE-SEQUENCE '(FOO GLORP NJ))
                     (MAKE-DIALOG-ITEM 'BUTTON-DIALOG-ITEM
                                       #@(4 4)
                                       #@(46 17)
                                       "Cancel"
                                       #'(LAMBDA (item) item (window-hide *pw-pop-menu-window*))
                                       :VIEW-FONT '("Monaco" 9 :SRCCOPY :PLAIN)
                                       :DEFAULT-BUTTON NIL))))) 


(defun call-pw-pop-up-function (res-string)
 (when res-string
   (eval (cdr (assoc res-string *pw-pop-menu-window-TABLE-SEQUENCE* :test #'string=)))))

(defun make-pw-pop-up (lst)
 (setf *pw-pop-menu-window-TABLE-SEQUENCE* lst)
; (set-view-size  (car (subviews *pw-pop-menu-window*)) (make-point (max  (* (length lst) 12) 220) 55))
 (set-TABLE-SEQUENCE (car (subviews *pw-pop-menu-window*)) (mapcar #'car lst))
 (window-select *pw-pop-menu-window*))



 
