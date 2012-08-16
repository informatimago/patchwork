;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               message-dialog.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Message Dialog
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-30 <PJB> Created.
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


(defgeneric dialog-item-text-length (item)
  (:method ((item dialog-item))
    (length (dialog-item-text item)))
  (:method ((item static-text-dialog-item))
    (niy dialog-item-text-length item)
    #-(and)
    (let ((h (dialog-item-handle item)))
      (if h
        (#_GetHandleSize h)
        (length (slot-value item 'dialog-item-text))))))


(defun message-dialog (message &key (ok-text "OK")
                       (size #@(335 100))
                       (modal t)   ; if not modal its callers job to select
                       (title "Warning")
                       window-type
                       (back-color *tool-back-color*)
                       (theme-background t)
                       (position (list :top (+ *menubar-bottom* 10))))
  "

The MESSAGE-DIALOG function displays a dialog box containing the
string message and a single button.  The function returns T when the
user clicks this button or presses Return or Enter.

MESSAGE:        A string to be displayed as the message in the dialog box.

OK-TEXT:        The text to be displayed in the button. The default
                button text is OK. If the text is too long, this
                string is clipped (that is, the button is not enlarged
                to accommodate the longer string). You can set the
                size with the :size keyword.

SIZE:           The size of the dialog box. The default size is #@(335
                100).  A larger size provides more room for text.

POSITION:       The position of the dialog box. The default position
                is the top center of the screen.

"
  (let* ((message-width (- (point-h size) 85))
         (new-dialog
          (make-instance
              'dialog
              :view-position position
              :view-size size
              :window-title title
              :window-type (or window-type (if modal :movable-dialog :document))
              :close-box-p (if modal nil t)
              :window-show nil
              :back-color back-color
              :theme-background theme-background
              :view-subviews
              `(,(make-instance 
                  'static-text-dialog-item
                  :dialog-item-text message
                  :view-size (make-point message-width (- (point-v size) 30)))
                 ,@(when modal
                         (list (make-dialog-item 'default-button-dialog-item
                                                 (subtract-points size #@(75 35))
                                                 #@(62 20)
                                                 ok-text
                                                 (lambda (item)
                                                   (declare (ignore item))
                                                   (return-from-modal-dialog t)))))))))
    (if modal
        (modal-dialog new-dialog)
        new-dialog)))


;;;; THE END ;;;;
