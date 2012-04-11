;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;=========================================================
;;;;
;;;;  PATCH-WORK
;;;;  By Mikael Laurson, Jacques Duthen, Camilo Rueda.
;;;;  © 1986-1992 IRCAM 
;;;;
;;;;=========================================================  

(in-package :pw)

;==================================================================================================================
(defvar *xy-windoid* ())
(defvar *x-value-text* ())
(defvar *y-value-text* ())

(defvar *prev-point-h-val* ())
(defvar *next-point-h-val* ())
(defvar *bpf-view-draw-lock* ())

;==================================================================================================================

(defun make-xy-windoid-instance ()
  (setf *xy-windoid* (make-instance 'windoid :view-size #@(90 32) :close-box-p nil :window-show nil))
  (add-subviews *xy-windoid*
    (make-instance 'static-text-dialog-item  
                     :view-position #@(10 0)
                     :dialog-item-text "x-val"
                     :VIEW-FONT '("Monaco" 9 :SRCOR :PLAIN))
    (make-instance 'static-text-dialog-item  
                     :view-position #@(50 0)
                     :dialog-item-text "y-val"
                     :VIEW-FONT '("Monaco" 9 :SRCOR :PLAIN))
    (setf *x-value-text*
      (make-instance 'static-text-dialog-item  
                     :view-position #@(5 15)
                     :dialog-item-text "    0"
                     :VIEW-FONT '("Monaco" 9 :SRCOR :PLAIN)))
    (setf *y-value-text*
      (make-instance 'static-text-dialog-item  
                     :view-position #@(40 15)
                     :dialog-item-text "    0"
                     :VIEW-FONT '("Monaco" 9 :SRCOR :PLAIN)))))

;======================================
(defvar *last-mouse* ())

(defclass C-mouse-window (window) ())

(defmethod window-null-event-handler ((self C-mouse-window))
 (call-next-method)
 (when (and (subviews self) (not (eq *last-mouse* (view-mouse-position self))))
  (let* ((mouse (view-mouse-position self))
        (active-subview (ask (subviews self) #'view-contains-point-p+self mouse)))
    (setq *last-mouse* mouse)
    (if active-subview
      (if (mouse-down-p)
        (view-mouse-dragged active-subview mouse)
        (view-mouse-moved active-subview mouse))
      (no-active-mouse-moved self)))))
             
(defmethod no-active-mouse-moved ((self C-mouse-window)))

(defmethod window-update-cursor :around ((self C-mouse-window) where) 
  (declare (ignore where))
  (call-next-method))

(defmethod window-grow-event-handler ((self C-mouse-window) where)
  (declare (ignore where))
  (call-next-method)
  (tell (subviews self) 'view-window-grown))

(defmethod window-zoom-event-handler ((self C-mouse-window) where)
  (declare (ignore where))
  (call-next-method)
  (tell (subviews self) 'view-window-grown))

(defmethod window-mouse-up-event-handler ((self C-mouse-window))
  (tell (subviews self) #'view-mouse-up))

#|    
(CCL:defobfun (CCL:window-null-event-handler *CCL-internal-window*) ()
   (declare (CCL:object-variable clpf-object clicked))
   (let ((mouse (CCL:window-mouse-position)))
     (unless (eq *last-mouse* mouse)
       (setq *last-mouse* mouse)
       (if clicked
         (mouse-dragged clpf-object (CCL:point-h mouse) (CCL:point-v mouse))
         (mouse-moved clpf-object (CCL:point-h mouse) (CCL:point-v mouse))))))
|#    

