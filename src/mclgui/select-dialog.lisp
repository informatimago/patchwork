;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               select-dialog.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Select Dialog
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
;;;;    

(in-package "MCLGUI")

(defclass select-dialog (window)
  ())

;; wouldnt need these if sequence-dialog-item were guaranteed to be a key-handler

(defmethod window-can-do-operation ((d select-dialog) op &optional item)
  (let ((s-item (or (current-key-handler d)(find-subview-of-type d 'sequence-dialog-item))))
    (when s-item (window-can-do-operation s-item op item))))

(defmethod copy ((dialog select-dialog))
  (let* ((s-item (or (current-key-handler dialog)
                     (find-subview-of-type  dialog 'sequence-dialog-item))))
    (when s-item (copy s-item))))

(defmethod set-view-size ((view select-dialog) h &optional v)
  (declare (ignore h v))
  (let* ((old-size (view-size view)))
    (call-next-method)    
    (let* ((new-size (view-size view))
           (delta (subtract-points new-size old-size)))      
      (dovector (v (view-subviews view))
                (if (typep v 'sequence-dialog-item)
                    (set-view-size v (add-points (view-size v) delta))
                    (if (typep v 'button-dialog-item)
                        (set-view-position v (add-points (view-position v) delta)))))
      new-size)))

(defun select-item-from-list (the-list &key (window-title "Select an Item")
                              (selection-type :single)
                              table-print-function 
                              (action-function #'identity)
                              (default-button-text "OK")
                              (sequence-item-class 'arrow-dialog-item)
                              (view-size #@(400 138))
                              (view-position '(:top 90) pos-p)
                              (theme-background t)
                              dialog-class
                              modeless
                              (help-spec 14086)
                              (list-spec 14087)
                              (button-spec 14088))
  "Displays the elements of a list, and returns the item chosen by the user"
  (let (debutton dialog)
    (flet ((act-on-items (item)
             (let ((s-item (find-subview-of-type (view-container item)
                                                 'sequence-dialog-item)))
               (funcall action-function 
                        (mapcar (lambda (c) (cell-contents s-item c))
                                (selected-cells s-item))))))
      (when (and dialog-class (not pos-p) modeless)
        (let ((w (front-window :class 'select-dialog)))  ; or dialog-class?
          (when w (setf view-position (add-points (view-position w) #@(15 15))))))
      (setf debutton
            (make-instance 
                'default-button-dialog-item
                :dialog-item-text default-button-text
                :dialog-item-enabled-p the-list
                :help-spec button-spec
                :dialog-item-action
                (cond 
                  ((not modeless)
                   (lambda (item)
                       (return-from-modal-dialog (act-on-items item))))
                  (t
                   #'act-on-items))))
      (let* ((bsize (view-default-size debutton))
             bpos)
        (setf bsize (make-point (max 60 (point-h bsize)) (point-v bsize))
              bpos (make-point (- (point-h view-size) 25 (point-h bsize))
                               (- (point-v view-size) 7 (point-v bsize))))
        (set-view-size debutton bsize)
        (set-view-position debutton bpos)
        (setf dialog
              (make-instance
                  (or dialog-class 'select-dialog)
                  :window-type :document-with-grow
                  :close-box-p (if modeless t nil)
                  :window-title window-title
                  :view-size view-size
                  :view-position view-position
                  :window-show nil ;modeless
                  :back-color *tool-back-color*
                  :theme-background theme-background
                  :help-spec help-spec
                  :view-subviews
                  (list*
                   (make-instance
                       sequence-item-class
                       :view-position #@(4 4)
                       :view-size (make-point (- (point-h view-size) 8)
                                              (- (point-v view-size) (point-v bsize) 20))
                                        ;:table-hscrollp nil
                       :table-sequence the-list
                       :table-print-function table-print-function
                       :selection-type selection-type
                       :help-spec list-spec)
                   debutton
                   (if (not modeless)
                       (list
                        (make-dialog-item 'button-dialog-item
                                          (make-point (- (point-h bpos) 80)
                                                      (point-v bpos))
                                          (make-point (if t #|(osx-p)|# 64 60) (point-v bsize))
                                          "Cancel"
                                          #'return-cancel 
                                          :cancel-button t
                                          :help-spec 15012))
                       nil))))
        (cond (modeless ; select first then show is prettier
               (window-show dialog)
               dialog)
              
              (t
               (niy select-item-from-list)
               #-(and) (#_changewindowattributes (wptr dialog) 0 #$kWindowCollapseBoxAttribute)
               (modal-dialog dialog)))))))


;;;; THE END ;;;;
