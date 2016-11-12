;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               mouse-window.lisp
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


(defvar *xy-windoid* ())
(defvar *x-value-text* ())
(defvar *y-value-text* ())

(defvar *prev-point-h-val* ())
(defvar *next-point-h-val* ())
(defvar *bpf-view-draw-lock* ())


(defun make-xy-windoid-instance ()
  (setf *xy-windoid* (make-instance 'windoid :view-size #@(90 32) :close-box-p nil :window-show nil))
  (add-subviews *xy-windoid*
    (make-instance 'static-text-dialog-item
                     :view-position #@(10 0)
                     :dialog-item-text "x-val"
                     :VIEW-FONT *patchwork-font-spec*)
    (make-instance 'static-text-dialog-item
                     :view-position #@(50 0)
                     :dialog-item-text "y-val"
                     :VIEW-FONT *patchwork-font-spec*)
    (setf *x-value-text*
      (make-instance 'static-text-dialog-item
                     :view-position #@(5 15)
                     :dialog-item-text "    0"
                     :VIEW-FONT *patchwork-font-spec*))
    (setf *y-value-text*
      (make-instance 'static-text-dialog-item
                     :view-position #@(40 15)
                     :dialog-item-text "    0"
                     :VIEW-FONT *patchwork-font-spec*))))

;;======================================
(defvar *last-mouse* ())

(defclass C-mouse-window (window) ())

(defmethod window-null-event-handler ((self C-mouse-window))
 (call-next-method)
 (when (and (subviews self) (not (eql *last-mouse* (view-mouse-position self))))
  (let* ((mouse (view-mouse-position self))
        (active-subview (ask (subviews self) #'view-contains-point-p+self mouse)))
    (setq *last-mouse* mouse)
    (if active-subview
      (if (mouse-down-p)
        (view-mouse-dragged active-subview mouse)
        (view-mouse-moved active-subview mouse))
      (no-active-mouse-moved self)))))

(defgeneric no-active-mouse-moved (self))
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
(defobfun (window-null-event-handler *CCL-internal-window*) ()
   (declare (ccl::object-variable clpf-object clicked))
   (let ((mouse (ui:window-mouse-position)))
     (unless (eql *last-mouse* mouse)
       (setq *last-mouse* mouse)
       (if clicked
         (mouse-dragged clpf-object (ui:point-h mouse) (ui:point-v mouse))
         (mouse-moved clpf-object (ui:point-h mouse) (ui:point-v mouse))))))
|#

