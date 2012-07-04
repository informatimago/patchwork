;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               view-event.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    View events.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-16 <PJB> Created.
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


(defmethod view-activate-event-handler ((view simple-view))
  (values))

(defmethod view-deactivate-event-handler ((view simple-view))
  (values))



(defmethod view-click-event-handler ((view simple-view) where)
  (declare (ignore where))
  view)

(defmethod view-click-event-handler ((view view) where)
  (loop
    :for subview :across (reverse (view-subviews view))
    :when (point-in-click-region-p subview where)
    :do (progn
          (view-convert-coordinates-and-click subview where view)
          (return t))
    :finally (return (call-next-method))))



(defmethod view-key-event-handler ((view simple-view) key)
  (declare (ignore key))
  (ed-beep))




(defmacro %get-current-key-handler (window)
  `(view-get ,window '%current-key-handler))

(defmethod current-key-handler ((w window))
  (%get-current-key-handler w))

(defmethod current-key-handler ((w null))
  nil)


(defmacro %get-key-handler-list (window)
  `(view-get ,window '%key-handler-list))

(defmethod key-handler-list ((view simple-view))
  (let ((w (view-window view)))
    (and w (%get-key-handler-list w))))

(defmacro %get-default-button (window)
  `(view-get ,window '%default-button))

(defmacro %get-cancel-button (window)
  `(view-get ,window '%cancel-button))



(defmethod add-key-handler ((item simple-view) &optional (dialog (view-window item)))
  (let ((items (%get-key-handler-list dialog)))
    (unless (member item items)
      (setf (%get-key-handler-list dialog) (nconc items (list item)))))
  (when (key-handler-p item)
    (unless (current-key-handler dialog)
      (set-current-key-handler dialog item))))


(defmethod remove-key-handler ((item simple-view) &optional (dialog (view-window item)))
  (without-interrupts
      (when dialog
        (when (eq item (%get-current-key-handler dialog))
          (change-key-handler dialog)
          (when (eq item (%get-current-key-handler dialog)) ;still current, so only one
            (set-current-key-handler dialog nil)))
        (setf (%get-key-handler-list dialog) (delete item (%get-key-handler-list dialog))))))



;;;; THE END ;;;;
