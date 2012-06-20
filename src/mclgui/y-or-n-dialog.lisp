;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               y-or-n-dialog.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    y-or-n-dialog
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


(defclass keystroke-action-dialog (dialog)
  ())


(defmethod view-key-event-handler ((dialog keystroke-action-dialog) char)
  (if (or (eq char #\return) (eq char :enter) (EQ CHAR #\ESC))
      (call-next-method)
      (let ((item (find char (dialog-items dialog 'dialog-item t)
                        :test (lambda (char item)
                                (and (dialog-item-enabled-p item)
                                     (dialog-item-action-p item)
                                     (let ((text  (dialog-item-text item)))
                                       (and (plusp (length  text))
                                            (char-equal char (char text 0)))))))))
        (when item
          (dialog-item-action item)))))
                                           

(defvar *yorn-dlg-size* #@(350 160))

(defun y-or-n-dialog (message &key (size *yorn-dlg-size*)
                      (position (list :top (+ 22 *menubar-bottom*)))
                      (yes-text "Yes")
                      (no-text "No")
                      (cancel-text "Cancel")
                      help-spec
                      (back-color *tool-back-color*)
                      (theme-background t)
                      window-type
                      (window-title "" window-title-supp))
  "

The Y-OR-N-DIALOG function displays a dialog box containing Yes, No,
and Cancel buttons. The display of the dialog box is modal.
If the user clicks the Yes button, the function returns t. If the user clicks the No
button, the function returns nil. If the user clicks the Cancel button, a throw cancel
occurs. The default button is the Yes button.

MESSAGE: A string to be displayed as the message in the dialog box.

:size The size of the dialog box. The default size is
#@(318 145).

:position The position of the dialog box. The default position is the
top center of the screen.

:yes-text The text to be displayed in the Yes button. The default is
Yes. This is the default button of the dialog box.

:no-text The text to be displayed in the No button. The default text
is No.

:cancel-text
The text to be displayed in the Cancel button. The default
text is Cancel. If this argument is nil instead of a string,
no Cancel button will appear in the dialog box.

:help-spec A value describing the Balloon Help for the item. This
may be a string or one of a number of more complicated
specifications, which are documented in the file helpmanager.
lisp in your Library folder. The default value
is nil.

Typing the initial character of the text of a button activates it. For
example, typing Y activates the Yes button, whereas typing N activates
the No button. In the following example, typing R activates the Cancel
button.

"
  (declare (ignore size position help-spec back-color theme-background window-type))
  (case (#_NSRunInformationalAlertPanel (objcl:objcl-string window-title)
                                        (objcl:objcl-string message)
                                        (objcl:objcl-string (or yes-text "OK"))
                                        (when cancel-text
                                          (objcl:objcl-string cancel-text))
                                        (when no-text
                                          (objcl:objcl-string no-text)))
    ((-1) nil)
    ((0)  (throw-cancel))
    ((+1) t))
  #-(and)
  (if window-title-supp
      (let ((the-dialog (make-instance 'keystroke-action-dialog
                            :window-type (or window-type :movable-dialog) 
                            :view-size size
                            :view-position position
                            :window-show nil
                            :window-title window-title
                            :back-color back-color
                            :theme-background theme-background
                            :window-show nil
                            :help-spec (getf help-spec :dialog))))
        (multiple-value-bind (ff ms) (view-font-codes the-dialog)
          (let* ((fudge        16)
                 (button-hsize (max 74
                                    (if yes-text (+ fudge (font-codes-string-width-for-control yes-text ff ms)) 0)
                                    (if no-text  (+ fudge (font-codes-string-width-for-control no-text  ff ms)) 0)))
                 (button-vsize (max 18 (+ 2 (font-codes-line-height ff ms))))
                 (cb-hsize     (max 74 (if cancel-text (+ fudge (font-codes-string-width-for-control cancel-text ff ms)) 0))))
            (apply (function add-subviews) the-dialog 
                   `(,(make-dialog-item 'static-text-dialog-item
                                        #@(20 12) (subtract-points size #@(38 72))
                                        message nil :help-spec (getf help-spec :dialog))
                      ,(make-dialog-item 'default-button-dialog-item               
                                         (subtract-points size (make-point (+ button-hsize 20) (+ button-vsize 9)))  ;; position
                                         (make-point button-hsize button-vsize)  ;; size
                                         (or yes-text "OK")  ; ????
                                         (lambda (item) 
                                           (declare (ignore item))
                                           (return-from-modal-dialog t))
                                         :help-spec (getf help-spec :yes-text))
                      ,@(if cancel-text
                            (list (make-dialog-item 'button-dialog-item                                        
                                                    (subtract-points size (make-point (+ button-hsize button-hsize 60)(+ button-vsize 9))) ; (- (point-v size) (+ button-vsize 9)))  ;; POSITION
                                                    (make-point cb-hsize button-vsize) cancel-text
                                                    (lambda (item)
                                                      (declare (ignore item)) 
                                                      (return-from-modal-dialog :cancel))
                                                    :cancel-button t
                                                    :help-spec (getf help-spec :cancel-text))))
                      ,@(if no-text
                            (list (make-dialog-item 'button-dialog-item
                                                    (subtract-points size (make-point (+ button-hsize 20) (+ (* 2 button-vsize) 17))) ;#@(102 53))  ;;  POSITION
                                                    (make-point button-hsize button-vsize) no-text
                                                    (lambda (item)
                                                      (declare (ignore item))
                                                      (return-from-modal-dialog nil))
                                                    :help-spec (getf help-spec :no-text))))))
            
            (modal-dialog the-dialog))))
      (standard-alert-dialog message
                             :position :main-screen
                             :yes-text yes-text
                             :no-text no-text
                             :cancel-text cancel-text)))

;;;; THE END ;;;;
