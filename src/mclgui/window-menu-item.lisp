;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               window-menu-item.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This implements the MCL window-menu-item API.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-11 <PJB> Created.
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


;;;---------------------------------------------------------------------

(defclass window-menu-item (menu-item)
  ())


(defmethod menu-item-action ((item window-menu-item))
  (let ((action (slot-value item 'menu-item-action))
        (w      (get-window-event-handler)))
    (when action
      (funcall action w))))


(defgeneric dim-if-undefined (item sym)
  (:method ((item window-menu-item) sym)
    (let ((wob  (get-window-event-handler)))
      (if (and wob
               (or (method-exists-p sym wob)
                   (let ((handler (current-key-handler wob)))
                     (and handler
                          #+ccl (method-exists-p sym handler)
                          #-ccl (niy method-exists-p sym handler)))))
          (menu-item-enable item)
          (menu-item-disable item)))))


(defmethod menu-item-update ((item window-menu-item))
  (unless (call-next-method) ; i.e. does menu-element method deal with it
    (let ((action (slot-value item 'menu-item-action)))
      (when (or (and action (symbolp action))
                (typep action 'generic-function))
        (dim-if-undefined item action)))))


;;;---------------------------------------------------------------------

(defclass windows-menu-menu-item (menu-item)
  ((my-window :initarg :window)))

(defmethod menu-item-action ((item windows-menu-menu-item))
  (let ((window (slot-value item 'my-window)))
    ;; [window makeKeyAndOrderFront:self]
    (when (shift-key-p)
      (window-ensure-on-screen window))
    (when (control-key-p)
      (let* ((windows (windows :class (class-of window)))
             (n 1))
        (dolist (win windows)
          (unless (eq win window)
            (set-window-layer win n)
            (incf n)))))))



;;;---------------------------------------------------------------------

(defmethod window-menu-item ((w window))
  (when (null (slot-value w 'my-item))
    (setf (slot-value w 'my-item) (make-instance 'windows-menu-menu-item :window w)))
  (let* ((enable (and (window-shown-p w)
                      (or (not (eq w (front-window)))
                          (not (window-active-p w)))))
         (my-item (slot-value w 'my-item))
         (name (window-title w)))
    (when (> (length name) 60) ; chosen at random sort of - doesnt send it left on powerbook
      (setf name (concatenate 'string (subseq name 0 60) "…")))
    (set-menu-item-title my-item name)
    (if enable
      (menu-item-enable my-item)
      (menu-item-disable my-item))
    my-item))

;;;; THE END ;;;;
