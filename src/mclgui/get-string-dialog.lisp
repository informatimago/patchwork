;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               get-string-dialog.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Get String From User Dialog
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


(defclass string-dialog (dialog)
  ((allow-empty-strings :initform nil :initarg :allow-empty-strings)))

(defclass get-string-dialog (string-dialog)
  ())



(defmethod set-view-size ((dialog get-string-dialog) h &optional v)
  (declare (ignore h v))
  (let* ((old-size (view-size dialog)))
    (call-next-method)
    (let* ((new-size (view-size dialog))
           (hdelta (make-point (- (point-h old-size)(point-h new-size)) 0))
           (subs (view-subviews dialog))
           (len (length subs)))
      (dotimes (i len)
        (let ((sub (elt subs i)))
          (if (typep sub 'button-dialog-item)
            (set-view-position sub (subtract-points (view-position sub) hdelta))
            (if (typep sub 'editable-text-dialog-item)
              (set-view-size sub (subtract-points (view-size sub) hdelta)))))))))


;; for dialogs which require non-empty strings to enable the default-button.
;; used by apropos, get-string-from-user and search-files

(defmethod view-key-event-handler :after ((d string-dialog) ch)
  (declare (ignore ch))
  (update-default-button d))


(defgeneric update-default-button (d)
  (:method ((d string-dialog))
    (let ((debutton (default-button d)))
      (when debutton
        (let ((text-items (subviews d 'editable-text-dialog-item)))
          (when text-items
            (let ((empties (slot-value d 'allow-empty-strings)))
              (if (or (eq empties t)
                      (dolist (item text-items t) ; enables if no text-items but there should be some
                        (unless (and (consp empties)
                                     (member (view-nick-name item) empties))
                          (when (eq 0 (dialog-item-text-length item))
                            (return nil)))))
                (dialog-item-enable debutton)
                (dialog-item-disable debutton)))))))))





(defun get-string-from-user (message 
                             &key
                             (initial-string "")
                             (size #@(365 100))
                             (position '(:bottom 140))
                             (ok-text "OK")
                             (cancel-text "Cancel")
                             (modeless nil)
                             (window-title "")
                             (window-type :document-with-grow)
                             (back-color *tool-back-color*)
                             (allow-empty-strings nil)
                             (action-function #'identity)
                             (cancel-function nil)
                             (theme-background t))
  (let ((dialog)
        (delta 20)
        (message-item)
        (message-len 0))
    (when message 
      (setf message-item (make-instance 'static-text-dialog-item
                           :text-truncation :end
                           :view-position (make-point 6 (- (point-v size) 54 delta))
                           :dialog-item-text message))
      (let* ((msize (view-default-size message-item))
             (mh    (min (point-h msize) (- (point-h size) 100)))) 
        (set-view-size message-item (make-point mh (point-v msize))))
      (setf message-len (+ 6 (point-h (view-size message-item)))))
    (flet ((act-on-text (item)
             (let ((e-item (find-subview-of-type (view-container item)
                                                 'editable-text-dialog-item)))
               (funcall action-function (dialog-item-text e-item)))))
      (setf dialog (make-instance 'get-string-dialog
                      :view-position position
                     :view-size size
                     :close-box-p (if modeless t nil)
                     :grow-box-p t
                     :window-type window-type
                     :window-title window-title
                     :window-show nil
                     :back-color back-color
                     :theme-background theme-background
                     :allow-empty-strings allow-empty-strings
                     :view-subviews
                     (list
                      (make-dialog-item 'default-button-dialog-item
                                        (make-point (- (point-h size) 74)
                                                    (- (point-v size) 20 delta))
                                        #@(62 20)
                                        ok-text
                                        (if (not modeless)
                                          (lambda (item)
                                                (return-from-modal-dialog (act-on-text item)))
                                          #'act-on-text))                     
                      (make-dialog-item 'button-dialog-item
                                        (make-point (- (point-h size) 154)
                                                    (- (point-v size) 20 delta))
                                        #@(62 20)
                                        cancel-text
                                        (or cancel-function
                                            (lambda (item)
                                                  (if (not modeless) 
                                                    (return-from-modal-dialog :cancel)
                                                    (window-close (view-window item)))))
                                        :cancel-button t)
                      (make-dialog-item 'editable-text-dialog-item
                                        (make-point (+ 6 message-len) (- (point-v size) 54 delta))
                                        (make-point (- (point-h size) delta message-len) 16)
                                        initial-string))))
      (when message
        (add-subviews dialog message-item))
      (update-default-button dialog)
      (cond ((not modeless)         
             (modal-dialog dialog))
            (t (window-show dialog)
               dialog)))))



;;;; THE END ;;;;
