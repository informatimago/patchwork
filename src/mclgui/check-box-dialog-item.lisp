;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               check-box-dialog-item.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Check box dialog item.
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


(defclass check-box-dialog-item (control-dialog-item)
  ((width-correction    :allocation :class
                        :initform    20)
   (procid              :allocation :class
                        :initform    0 ; #.(+ #$checkBoxProc #$kControlUsesOwningWindowsFontVariant)
                        )
   (check-box-checked-p :initform    nil
                        :initarg    :check-box-checked-p
                        :accessor   check-box-checked-p))
  (:documentation "
Checkboxes are small squares that toggle an X mark on and off when
clicked. The following class and functions govern the behavior of
checkboxes.
"))


(defmethod view-default-font ((view check-box-dialog-item))
  (sys-font-spec))



(defmethod view-default-size ((item check-box-dialog-item))
  (let ((size (call-next-method)))
    ;; required h fudge seems to depend on length of dialog-item-text - maybe font too - maybe fixed now
    (make-point (point-h size) (max 16 (point-v size)))))


(defmethod install-view-in-window :after ((item check-box-dialog-item) dialog)
  (declare (ignore dialog))
  (when (check-box-checked-p item)
    (check-box-check item)))



(defgeneric check-box-check (item)
  (:documentation "
The check-box-check generic function places an X in the checkbox. The
function merely places an X in the box; it does not run the action of the
dialog item.
")
  (:method ((item check-box-dialog-item))
    (setf (check-box-checked-p item) t)
    (when (installed-item-p item)
      (with-focused-view (view-container item)
        (niy check-box-check item)
        ;; (#_SetControlValue (dialog-item-handle item) 1)
        ))))


(defgeneric check-box-uncheck (item)
  (:documentation "
The check-box-uncheck generic function removes the X from the
checkbox. The function merely removes the X from the box; it does not run
the action of the dialog item. The function returns nil.
")
  (:method ((item check-box-dialog-item))
    (setf (check-box-checked-p item) nil)
    (when (installed-item-p item)
      (with-focused-view (view-container item)
        (niy check-box-uncheck item)
        ;; (#_SetControlValue (dialog-item-handle item) 0)
        ))))


(defmethod dialog-item-action ((item check-box-dialog-item))
  "
The check-box-dialog-item primary method for dialog-itemaction
toggles the state of the box from unchecked to checked or vice
versa, then calls call-next-method.
"
  (if (check-box-checked-p item)
      (check-box-uncheck item)
      (check-box-check item))
  (call-next-method))                   ; dispatch to user's dialog-item-action code



;;;; THE END ;;;;
