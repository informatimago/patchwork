;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               bpf-window.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;    
;;;;    XXX
;;;;    
;;;;AUTHORS
;;;;    Mikael Laurson, Jacques Duthen, Camilo Rueda.
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-07 <PJB> Changed license to GPL3; Added this header.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright IRCAM 1986 - 2012
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
(in-package :pw)
(enable-patchwork-reader-macros)

;;====================================================================================================
(defclass C-numbox-continuous-no-double-click (C-numbox-continuous) ())

(defmethod view-double-click-event-handler ((self C-numbox-continuous-no-double-click) where)
  (declare (ignore where)))
;;====================================================================================================

(defclass C-BPF-window (C-mouse-window C-application-window) 
  ((bpf-lib-pointer :initform 0 :allocation :class :accessor bpf-lib-pointer)
   (BPF-editor-object :initform nil  :accessor BPF-editor-object)
   (bpf-radio-ctrls :initform nil  :accessor bpf-radio-ctrls)
   (text-disp-ctrl :initform nil  :accessor text-disp-ctrl)
   (x-origo-ctrl :initform nil  :accessor x-origo-ctrl)
   (y-origo-ctrl :initform nil  :accessor y-origo-ctrl)
   (x-disp-ctrl :initform nil  :accessor x-disp-ctrl)
   (y-disp-ctrl :initform nil  :accessor y-disp-ctrl)
   (x-zoom-ctrl :initform nil  :accessor x-zoom-ctrl)
   (y-zoom-ctrl :initform nil  :accessor y-zoom-ctrl)))

(defmethod decompile ((self C-BPF-window))
  `(let ((win (make-instance 'C-BPF-window :close-box-p nil :window-show nil
                             :window-title ,(window-title self) 
                             :view-position #@(10 40) 
                             :view-size #@(240 235)))) 
      (setf (BPF-editor-object win)
         (make-instance 
           'C-bpf-view 
               :view-container win
               :view-position #@(2 2) :view-size #@(230 230) 
               :track-thumb-p t))
     win))

(defmethod initialize-instance :after ((self C-BPF-window) &rest l)
  (declare (ignore l))
  (add-subviews self 
     (setf (text-disp-ctrl self)
       (make-instance 'static-text-dialog-item
         :view-font '("Monaco" 9 :SRCOR :PLAIN)
         :dialog-item-text "x-off  y-off  x-val  y-val xzoom  yzoom"))
     (setf (x-origo-ctrl self) 
        (make-instance 'C-numbox-continuous-no-double-click 
           :view-size (make-point 40 15) :dialog-item-text "  0" :min-val -9999 
           :dialog-item-action (lambda (item) (set-bpf-x-origo (editor-view-object self) item))))
     (setf (y-origo-ctrl self) 
        (make-instance 'C-numbox-continuous-no-double-click 
           :view-size (make-point 40 15) :dialog-item-text "  0" :min-val -9999
           :dialog-item-action (lambda (item) (set-bpf-y-origo (editor-view-object self) item))))
     (setf (x-disp-ctrl self) 
        (make-instance 'C-ttybox :view-size (make-point 40 15) :dialog-item-text "  0"))
     (setf (y-disp-ctrl self) 
        (make-instance 'C-ttybox :view-size (make-point 40 15) :dialog-item-text "  0"))
     (setf (x-zoom-ctrl self) 
        (make-instance 'C-numbox-continuous-no-double-click 
           :view-size (make-point 40 15) :dialog-item-text "  0" 
           :value 100 :min-val 35
           :dialog-item-action (lambda (item) (set-bpf-x-zoom (editor-view-object self) item))))
     (setf (y-zoom-ctrl self) 
        (make-instance 'C-numbox-continuous-no-double-click 
            :view-size (make-point 40 15) :dialog-item-text "  0" 
            :value 100 :min-val 35
           :dialog-item-action (lambda (item) (set-bpf-y-zoom (editor-view-object self) item)))) )
  (push (add-bpf-editor-radio-cluster self 0 0 "drag") (bpf-radio-ctrls self))
  (push (add-bpf-editor-radio-cluster self 0 0 "sel") (bpf-radio-ctrls self))
  (push (add-bpf-editor-radio-cluster self 0 0 "zoom") (bpf-radio-ctrls self))
  (push (add-bpf-editor-radio-cluster self 0 0 "edit") (bpf-radio-ctrls self))
  (radio-button-push (car (bpf-radio-ctrls self)))
  (make-extra-bpf-view-ins-controls self)
  (set-ctrl-positions self))

;;================

(defgeneric add-bpf-editor-radio-cluster (self x y txt))
(defmethod add-bpf-editor-radio-cluster ((self C-BPF-window) x y txt)
  (make-instance 
   'radio-button-dialog-item
   :view-container (view-window self)
   :view-position (make-point x y)
   :dialog-item-text txt
   :view-font '("monaco"  9  :srcor) 
   :dialog-item-action
   (lambda (item)
     (set-bpf-edit-mode self item (dialog-item-text item)))))

(defgeneric set-bpf-edit-mode (self item text))
(defmethod set-bpf-edit-mode ((self C-BPF-window) item text)
  (tell (bpf-radio-ctrls self) #'radio-button-unpush)
  (radio-button-push item)
  (setf (edit-mode (editor-view-object self)) text))

;;================

(defmethod editor-view-object ((self C-BPF-window)) (BPF-editor-object self))

;;================

(defmethod key-pressed-extra ((self C-BPF-window) char)
  (key-pressed-BPF-editor (editor-view-object self) char))

(defmethod open-application-help-window ((self C-BPF-window))
   (if *BPF-help-window*
         (unless (wptr  *BPF-help-window*) (make-BPF-help-window))
         (make-BPF-help-window))
   (window-select *BPF-help-window*))

;;==================

(defmethod view-activate-event-handler :after ((self C-BPF-window))
  (when (pw-object self)
    (draw-appl-label (pw-object self) #\*))
  (setq *active-BPF-window* self) 
  (ui:set-menubar *BPF-menu-root*)
  (enable-all-apps-menu-items)
  (menu-item-disable *apps-BPF-menu-item*))

;;(defmethod give-mini-view-class-name ((self C-BPF-window))
;;  (class-name (class-of (mini-view (car (editor-objects (car (controls self))))))))
;;  (when (eql (give-mini-view-class-name self) 'patchwork::c-mini-bpf-view-chant)
;;    (setf (window-visible-p self) nil))

(defmethod view-deactivate-event-handler :after ((self C-BPF-window))
  (when (pw-object self)
    (erase-BPF-label? (pw-object self))
    (draw-appl-label (pw-object self) #\A))
  (when (eql *active-BPF-window* self)  ; no BPF window selected
     (menu-item-enable *apps-BPF-menu-item*)
     (enable-Lisp-apps-menu-item)))

;;==================
;; PW interface

(defgeneric set-mini-view (self mini-view))
(defmethod set-mini-view ((self C-BPF-window) mini-view)
  (setf (mini-view (editor-view-object self)) mini-view))

(defgeneric set-pw-win (self win))
(defmethod set-pw-win ((self C-BPF-window) win)
  (setf (pw-win self) win))

(defgeneric add-bpf-to-bpf-editor-from-PW (self bpf))
(defmethod add-bpf-to-bpf-editor-from-PW ((self C-BPF-window) bpf)  
  (setf (break-point-function (editor-view-object self)) bpf)
  (scale-to-fit-in-rect (editor-view-object self)) 
  (view-draw-contents self))

;;==========================================
;; layout

(defgeneric BPF-window-ctrl-1st-y (self)
  (:method ((self C-BPF-window))
    (- (h self) 47)))
(defgeneric BPF-window-ctrl-2nd-y (self)
  (:method ((self C-BPF-window))
    (- (h self) 35)))
(defgeneric BPF-window-ctrl-3rd-y (self)
  (:method ((self C-BPF-window))
    (- (h self) 18)))

(defgeneric set-ctrl-positions-extra (self)
  (:method ((self C-BPF-window))
    (values)))
(defgeneric make-extra-bpf-view-ins-controls (self)
  (:method ((self C-BPF-window))
    (values)))

(defgeneric set-ctrl-positions (self)
  (:method ((self C-BPF-window))
    (let ((y1 (BPF-window-ctrl-1st-y self)) 
          (y2 (BPF-window-ctrl-2nd-y self))
          (y3 (BPF-window-ctrl-3rd-y self)))
      (set-view-position (text-disp-ctrl self) (make-point 5 y1)) 
      (set-view-position (x-origo-ctrl self)   (make-point 2 y2)) 
      (set-view-position (y-origo-ctrl self)   (make-point 42 y2))
      (set-view-position (x-disp-ctrl self)    (make-point 82 y2)) 
      (set-view-position (y-disp-ctrl self)    (make-point 122 y2))
      (set-view-position (x-zoom-ctrl self)    (make-point 162 y2)) 
      (set-view-position (y-zoom-ctrl self)    (make-point 202 y2))
      (for (i 0 1 (1- (length (bpf-radio-ctrls self))))
        (set-view-position (nth  i (bpf-radio-ctrls self)) (make-point (* i 60)  y3)))
      (set-ctrl-positions-extra self))))

;;==========================================

(defmethod window-grow-event-handler ((self C-BPF-window) where)
  (declare (ignore where))
  (call-next-method)
  (set-ctrl-positions self))

(defmethod window-zoom-event-handler ((self C-BPF-window) where)
  (declare (ignore where))
  (call-next-method)
  (set-ctrl-positions self))

;;==========================================

(defmethod no-active-mouse-moved ((self C-BPF-window))
  (tell (subviews self) #'reset-active-point))

(defmethod window-update-cursor :around ((self C-BPF-window) where)
;;  (declare (ignore where))
  (cond ((string= (edit-mode (editor-view-object self)) "edit")
            (if (ask (subviews self) #'active-point)
              (set-cursor *cross-hair-cursor*)
              (set-cursor *arrow-cursor*)))
        ((string= (edit-mode (editor-view-object self)) "sel")
           (if (view-contains-point-p+self (editor-view-object self) where)
             (set-cursor *i-beam-cursor*)
             (set-cursor *arrow-cursor*)))
        (t (call-next-method))))

;;==========================================

(defvar *BPF-window-counter* 0)

(defun make-BPF-editor (bp &optional editor-view-class)
  (let* ((win-string (mk-nuevo-name-box "bpf"))
         (win        (make-instance 'C-BPF-window
                                    :window-title win-string
                                    :close-box-p nil
                                    :window-show nil
                                    :view-position #@(10 40)
                                    :view-size #@(250 275)))
         (bp-view     (make-instance (or editor-view-class 'C-bpf-view) 
                                     :view-container win
                                     :view-position #@(2 2)
                                     :view-size #@(240 217) 
                                     :break-point-function bp 
                                     :track-thumb-p t)))
      (add-subviews win bp-view)
      (setf (BPF-editor-object win) bp-view)
      (scale-to-fit-in-rect bp-view)
      win))

;;(window-select (make-BPF-editor (make-break-point-function '(0 100) '(0 100)))) 

;;;; THE END ;;;;
