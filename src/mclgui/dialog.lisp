;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               dialog.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Dialogs.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-17 <PJB> Created.
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


(defgeneric default-button (window))


(defgeneric cancel-button (window))
(defgeneric look-for-a-button-named-cancel (window))

(defgeneric press-button (dialog-item))
(defgeneric dialog-item-enabled-p (dialog-item))



(defun message-dialog (message &key ok-text (size #@(330 110)) position)
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
  (niy message-dialog message ok-text size position)
  (format *query-io* "~&~72,,,'-<~>~%Message: ~A~%~72,,,'-<~>~%" message)
  (force-output  *query-io*))



;;;; THE END ;;;;
