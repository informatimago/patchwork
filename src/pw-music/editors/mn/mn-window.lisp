;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               mn-window.lisp
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

(provide 'MN-window)

;;============================================================================

;;(defmethod decompile ((self simple-view)))
;;(defmethod decompile ((self scroll-bar-dialog-item)))

(defun make-MN-popUpMenus ()
  (make-chord-ed-pops)
  (make-chord-collector-pops)
  (make-bpf-pops))

(defclass C-mn-window (C-mouse-window C-application-window)  
  ((super-win :initform nil :accessor super-win)
   (super-note :initform nil :accessor super-note)))

(defmethod decompile ((self C-mn-window))
  `(let* ((mn-window (make-instance ',(class-name (class-of self)) :window-show nil
                            :window-title ,(window-title self)
                            :view-position ,(view-position self)
                            :view-size ,(view-size self)
                            :view-subviews (list ,(decompile (car (subviews self))))))) 
      ;(set-view-container (external-controls (editor-view-object mn-window)) mn-window)
      mn-window))

(defgeneric window-killed-p (self))
(defmethod window-killed-p ((self C-mn-window))
  (not (wptr self)))

;;================

(defgeneric editor-view-object (self))
(defmethod editor-view-object ((self C-MN-window))
  (car (subviews self)))

;;================
;;application

(defmethod open-application-help-window ((self C-MN-window))
   (if *MN-help-window*
         (unless (wptr  *MN-help-window*) (make-MN-help-window))
         (make-MN-help-window))
   (window-select *MN-help-window*))

(defmethod remove-yourself-control ((self C-mn-window)) 
  (tell (ask-all (editor-objects (car (subviews self))) 'chord-line) 'kill-chords)
  (window-close  self))

(defmethod key-pressed-extra ((self C-MN-window) char)
  (case char
    ((:Enter) 
     (if (pw-win self)
         (if (super-win (pw-win self)) 
             (window-select (super-win (pw-win self)))
             (ui:ed-beep))
         (ui:ed-beep)))
    ((#\C) 
     (record-structured-process self))
    ((#\Q) 
     (make-structured-score self))
    ((#\L) 
     (pretty-visible-layout  (editor-view-object self)))
    (otherwise
     (key-pressed-MN-editor (editor-view-object self) char))))

(defmethod cut ((self C-MN-window))
  (cut (editor-view-object self)))

(defmethod copy ((self C-MN-window))
  (copy (editor-view-object self)))

(defmethod paste ((self C-MN-window))
  (paste (editor-view-object self)))
;;========================
(defmethod view-activate-event-handler :after ((self C-MN-window))
  (when (pw-object self)
     (draw-appl-label (pw-object self) #\*))
  (setf *active-MN-window* self)
  (unless (equal (ui:menubar)  *MN-menu-root*)
    (ui:set-menubar *MN-menu-root*)
    (enable-all-apps-menu-items))
  (menu-item-disable *apps-MN-menu-item*))
  
#|(defmethod view-deactivate-event-handler :after ((self C-MN-window))
  (when (pw-object self)
    (draw-appl-label (pw-object self) #\A))
  (when (eq *active-MN-window* self)  ; no MN window selected
    (menu-item-enable *apps-MN-menu-item*)
    (enable-Lisp-apps-menu-item?)))|#

(defmethod view-deactivate-event-handler :after ((self C-MN-window))
  (if (pw-object self)
    (draw-appl-label (pw-object self) #\A))
  (menu-item-enable *apps-MN-menu-item*))

;;========================
;;================
;;mouse

(defmethod no-active-mouse-moved ((self C-mn-window))
  (tell (subviews (car (subviews self))) #'reset-active-chord))

;;(window-hide *xy-windoid*))
(defvar *default-MN-cursor* 110)
(defconstant *cross-line-cursor* 604)
(defmethod window-update-cursor :around ((self C-mn-window) where)
  (declare (ignore where))
  (if (subviews self)
    (if (ask (subviews (car (subviews self))) #'active-chord)
      (if (ask (subviews (car (subviews self))) #'active-note)
          (set-cursor *default-MN-cursor*)
          (set-cursor  *cross-line-cursor*))                          ;*I-beam-cursor*))
      (call-next-method))
     (call-next-method)))

