;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               default-button-mixin.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Default Button Mixin.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-19 <PJB> Created.
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


(defclass default-button-mixin ()
  ())


(defgeneric dont-throb (button))


(defmethod button-props-to-window ((item default-button-mixin) window)
  (niy button-props-to-window item window)
  ;; (cond ((view-get item 'default-button-p)
  ;;        (setf (%get-default-button window) item)
  ;;        (when (dialog-item-handle item) ;; might be a 3d-button
  ;;          (if (dont-throb item)
  ;;              nil
  ;;              (#_setwindowdefaultbutton (wptr window) (dialog-item-handle item)))))
  ;;       ((view-get item 'cancel-button-p)
  ;;        (setf (%get-cancel-button window) item)))
  )


(defmethod initialize-instance :after ((item default-button-mixin) &key default-button cancel-button view-container)  
  (when default-button
    (setf (view-get item 'default-button-p) t))
  (when cancel-button
    (setf (view-get item 'cancel-button-p) t)
    (when (not (dialog-item-action-function item))
      (setf (dialog-item-action-function item) 
            (lambda (item) (declare (ignore item)) (return-from-modal-dialog :cancel)))))
  (when view-container
    (let ((window (view-window view-container)))
      (when (and window (or default-button cancel-button))
        (button-props-to-window item window)))))



(defmethod install-view-in-window :after ((item default-button-mixin) view)
  (let ((window (view-window view)))
    (when window
      (button-props-to-window item window))))

(defmethod remove-view-from-window :before ((item default-button-mixin))
  (niy remove-view-from-window item)
  #-(and)
  (let ((dialog (view-window item)))
    (when dialog
      (cond ((eq item (%get-default-button dialog))
             (setf (%get-default-button dialog) nil)
             ;; dunno if this matters
             (when (and (wptr dialog)(dialog-item-handle item))
               (#_setwindowdefaultbutton (wptr dialog) (%null-ptr))))
            ((eq item (%get-cancel-button dialog))
             (setf (%get-cancel-button dialog) nil))))))



;;;; THE END ;;;;
