;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               list-editor.lisp
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
(in-package "C-LIST-ITEM-H")

;;;======================================================
;;; A list editor
;;;
;;; The editor consists of a window (class C-table-window) and a table dialog item
;;; (class C-list-item). The associated patch box is the class C-patch-list-editor.
;;; Mouse clicking on a cell selects it, double clicking opens it for edition. The
;;; arrow keys adds cells in positions indicated by the direction of the arrow.
;;;======================================================


;;;==========================================
;;; The class of table dialog items with editable cells
;;;
;;; Class: C-list-item
;;; inherits from: C-array-item
;;; methods:
;;;  cell-contents         ;displays the table element indicated by h & v
;;;  set-array             ;resets the table with a whole new list, then redisplays it.
;;;  dialog-item-action    ;handles cell selection or opening
;;;  edit-selected-cell    ;opens an edit dialog for a cell
;;;  out-side-list-p       ;true if selection is out of table's current dimensions
;;;  set-array-item        ;changes the value of a cell
;;;  add-backward-element  ;adds a new cell to the left of a selection
;;;  add-forward-element   ;same, to the right
;;;  add-upward-element
;;;  add-downward-element
;;;==========================================


(defparameter *cell-width*  60)
(defparameter *cell-height* 12)

(defclass C-list-item (C-array-item) ())

(defmethod cell-contents ((self C-list-item) h &optional v)
  (let ((point (if v (make-point h v) h)))
    (if (out-side-list-p self point)
        #\space
        (nth (point-v point) (nth (point-h point) (my-array self))))))

(defmethod set-array ((self C-list-item) the-array &optional cell)
  (if the-array
      (let* ((dim-v  (length the-array))
             (dim-h (apply #'max (mapcar #'length the-array)))
             (dialog (view-container self)))
        (remove-subviews dialog self)
        (set-table-dimensions self  dim-v dim-h)
        (setf (my-array self) the-array)
        (add-subviews dialog self)
        (when cell
          (cell-select self cell)
          (edit-selected-cell self)) )))

(defmethod update-size ((self C-list-item) size)
  (set-view-size self (subtract-points  size (make-point *cell-width* (* 2 *cell-height*)))))

(defmethod dialog-item-action ((self C-list-item))
  (if (double-click-p)
      (edit-selected-cell self)
      (call-next-method)))

(defmethod edit-selected-cell ((self C-list-item))
  (let ((selection (car (selected-cells self))))
    (set-dialog-item-text self (format () "~A" (cell-contents self selection)))
    (open-pw-controls-dialog
     self
     (make-point
      (* (- (point-h selection) (point-h (scroll-position self)))
          *cell-width*)
      (* (- (point-v selection) (point-v (scroll-position self)))
         *cell-height*))
     (make-point *cell-width* *cell-height*))))

(defmethod set-dialog-item-text-from-dialog ((self C-list-item) text)
  (set-array-item self (car (selected-cells self))
                  (if (zerop (length text)) 0 (read-from-string text)))
  (cell-select self (car (selected-cells self))))

(defmethod out-side-list-p ((self C-list-item) point)
  (let ((h (point-v point))
        (v (point-h point)))
    (or (>= v (length (my-array self)))
        (>= h (length (nth v (my-array self)))))))

(defmethod set-array-item ((self C-list-item) point val)
  (and point
       (if (out-side-list-p self point)
           (update-list self point val)
           (setf (nth (point-v point) (nth (point-h point) (my-array self))) val))))

(defmethod update-list ((self C-list-item) point val)
  (let* ((sublist (nth (point-h point) (my-array self)))
         (length (length sublist))
         (added-items (make-list (1+ (- (point-v point) length)) :initial-element 0)))
    (setf (nthcdr length sublist) added-items)
    (setf (nth (point-v point) sublist) val)))

(defmethod next-right-element ((self  C-list-item) point)
  (let* ((the-list (my-array self))
         (place (1+ (point-h point)))
         (length (length the-list)))
    (if (= length place)
        (add-forward-element self point)
        (progn
          (progn
            (cell-select self (make-point place (point-v point)))
            (cell-deselect self point))))))

(defmethod next-left-element ((self  C-list-item) point)
  (let ((place (1- (point-h point))))
    (if (minusp place)
        (add-backward-element self point)
        (progn
          (cell-select self (make-point place (point-v point)))
          (cell-deselect self point)))))

(defmethod next-up-element ((self  C-list-item) point)
  (let ((place (1- (point-v point))))
    (if (minusp place)
        (add-upward-element self point)
        (progn
          (cell-select self (make-point (point-h point) place))
          (cell-deselect self point)))))

(defmethod next-down-element ((self  C-list-item) point)
  (let* ((sublist (nth (point-h point) (my-array self)))
         (place (1+ (point-v point)))
         (length (length sublist)))
    (if (= length place)
        (add-downward-element self point)
        (progn
          (cell-select self (make-point (point-h point) place))
          (cell-deselect self point)))))

(defmethod add-upward-element ((self C-list-item) point)
  (let* ((sublist (nth (point-h point) (my-array self)))
         (place (point-v point))
         (added-items (if (zerop place)
                          (copy-list (nthcdr place sublist))
                          (cons 0 (copy-list (nthcdr place sublist))))))
    (if (zerop place)
        (progn
          (setf (nthcdr (1+ place) sublist) added-items)
          (setf (nth 0 sublist) 0))
        (setf (nthcdr place sublist) added-items))
    (set-array self (my-array self) point)))

(defmethod add-upward-row ((self C-list-item) point)
  (for (i 0 1 (point-h point))
       (add-upward-element self (make-point i (point-v point)))))

(defmethod add-downward-element ((self C-list-item) point)
  (let* ((sublist (nth (point-h point) (my-array self)))
         (place (1+ (point-v point)))
         (length (length sublist))
         (added-items
          (if (= length place) (list 0) (cons 0 (copy-list (nthcdr place sublist))))))
    (setf (nthcdr place sublist) added-items)
    (set-array self (my-array self) (make-point (point-h point)  place))))

(defmethod add-downward-row ((self C-list-item) point)
  (for (i 0 1 (point-h point))
       (add-downward-element self (make-point i (point-v point)))))

(defmethod add-forward-element ((self C-list-item) point)
  (let* ((the-list (my-array self))
         (place (1+ (point-h point)))
         (length (length the-list))
         (new-list (make-list (1+ (point-v point)) :initial-element 0))
         (added-items
          (if (= length place)
              (list new-list)
              (cons new-list (copy-list (nthcdr place the-list))))))
    (setf (nthcdr place the-list) added-items)
    (set-array self the-list (make-point place (point-v point)))))

(defmethod add-backward-element ((self C-list-item) point)
  (let* ((the-list (my-array self))
         (place (point-h point))
         (new-list (make-list (1+ (point-v point)) :initial-element 0))
         (added-items
          (if (zerop place)
              (copy-list (nthcdr place the-list))
              (cons new-list (copy-list (nthcdr place the-list))))))
    (if (zerop place)
        (progn
          (setf (nthcdr (1+ place) the-list) added-items)
          (setf (nth 0 the-list) new-list))
        (setf (nthcdr place the-list) added-items))
    (set-array self the-list point)))

(defmethod cut-element ((self C-list-item) point)
  (let* ((sublist (nth (point-h point) (my-array self)))
         (place (point-v point)))
    (if (zerop place)
        (if (cdr sublist)
            (setf (nth (point-h point) (my-array self)) (cdr sublist))
            (erase-line self (point-h point)))
        (setf (nthcdr place sublist) (nthcdr (1+ place) sublist)))
    (set-array self (my-array self))))

(defmethod erase-line ((self C-list-item) line)
  (if (zerop line)
      (and (cdr (my-array self)) (setf (my-array self) (cdr (my-array self))))
      (setf (nthcdr line (my-array self)) (nthcdr (1+ line) (my-array self)))))


;;;==================================================
;;; The class of windows with a table dialog sub-view
;;;
;;; Class: C-table-window
;;; Inherits from: C-application-window
;;; Methods:
;;;  key-pressed-extra   ; responds to arrow keys
;;;==================================================


(in-package "C-TABLE-WINDOW-H")

(defclass C-table-window (C-application-window) ())



(defmethod key-pressed-extra ((self C-table-window) char)
  (let* ((table (first (subviews self)))
         (selection (car (selected-cells table))))
    (if (and selection (not (out-side-list-p table selection)))
        (case char
          ((:ForwardArrow)
           (if (option-key-p)
               (add-forward-element table selection)
               (next-right-element table selection)))
          ((:BackArrow)
           (if (option-key-p)
               (add-backward-element table selection)
               (next-left-element table selection)))
          ((:UpArrow)
           (cond ((shift-key-p)
                  (add-upward-row table selection))
                 ((option-key-p)
                  (add-upward-element table selection))
                 (t (next-up-element table selection))))
          ((:DownArrow)
           (cond ((shift-key-p)
                  (add-downward-row table selection))
                 ((option-key-p)
                  (add-downward-element table selection))
                 (t (next-down-element table selection))))
          ((:Backspace) (cut-element table selection))
          ((#\h) (open-application-help-window self))
          (otherwise
           (set-array-item table selection (string char))
           (edit-selected-cell table)  ;;;(ui:ed-beep)
           )))))

(defvar *lst-ed-box-help-window* ())

(defmethod pw::open-application-help-window ((self C-table-window))
  (if *lst-ed-box-help-window*
      (unless (wptr  *lst-ed-box-help-window*) (make-lst-ed-help-window))
      (make-lst-ed-help-window))
  (window-select *lst-ed-box-help-window*))

(defmethod window-grow-event-handler ((self C-table-window) where)
  (declare (ignore where))
  (call-next-method)
  (update-size (first (subviews self)) (view-size self)))

;; (defmethod view-activate-event-handler :after ((self C-table-window))
;;   ())
;; (let ((table (first (subviews self))))
;;   (when (pw-object self)
;;     (set-array table (the-list (pw-object self)))
;;     (set-view-size self
;;                    (add-points (view-size table)
;;                                (make-point *cell-width* (* 2 *cell-height*))))))

(defmethod open-application-window ((self C-table-window))
  (let ((table (first (subviews self))))
    (when (pw-object self)
      (set-array table (the-list (pw-object self)))
      (set-view-size self
                     (add-points (view-size table)
                                 (make-point *cell-width* (* 2 *cell-height*)))))))

(defmethod window-close ((self C-table-window))
  (when (and pw::*pw-controls-dialog* (wptr pw::*pw-controls-dialog*))
    (window-hide pw::*pw-controls-dialog*))
  (when (and *lst-ed-box-help-window* (wptr *lst-ed-box-help-window*))
    (window-close *lst-ed-box-help-window*))
  (call-next-method))


;;;=====================================
;;; The class of list editor patch boxes
;;;
;;; Class: C-patch-list-editor
;;; Inherits from: C-patch-application
;;; Methods:
;;;  make-application-object     ;constructs the edit window with a new table
;;;  patch-value                 ;returns the currently stored list or that of its
;;;                              ;connected input (if any)
;;;  open-patch-win              ;opens the editor window
;;;======================================


(in-package "C-PATCH-LIST-EDITOR")

(defclass C-patch-list-editor (C-patch-application)
  ((the-list :initform (list (list 0 0) (list 0 0)) :initarg  :the-list :accessor the-list)
   (popUpBox :accessor popUpBox)
   (lock     :initform nil                          :accessor lock)
   (value    :initform nil                          :accessor value)))

(defmethod make-application-object ((self C-patch-list-editor))
  (make-instance 'C-table-window
      :view-subviews
    (list (make-instance 'C-list-item
              :view-font pw::*patchwork-font-spec*
              :table-dimensions (make-point 2 2)
              :cell-size (make-point *cell-width* *cell-height*)
              :view-size (make-point 100 80)))
    :window-show nil
    :window-title (pw-function-string self)))

(defmethod decompile ((self C-patch-list-editor))
  (append (call-next-method)
          `(nil (list ',(copy-tree (the-list self))
                      ,(if (wptr (application-object self))
                           (window-title (application-object self))
                           (pw-function-string self))))))

(defmethod complete-box ((self C-patch-list-editor) args)
  (setf (the-list self) (first args))
  (setf (pw-function-string self) (second args))
  (set-window-title (application-object self) (second args)))

(defmethod initialize-instance :after ((self C-patch-list-editor) &key controls)
  (declare (ignore controls))
  (setf (popUpBox self)
        (make-popUpbox "" self
                       *collector-popUp-menu*
                       :view-position (make-point (- (w self) 10)
                                                  (- (h self) 14))
                       :view-container self
                       :view-font pw::*patchwork-font-spec*))
  (-make-lock self (make-point (- (w self) 37) (- (h self) 9))))

(defmethod save ((self C-patch-list-editor))
  (call-next-method)
  (if (and (application-object self) (wptr (application-object self)))
      (set-window-title (application-object self) (pw-function-string self))))


(defmethod patch-value ((self C-patch-list-editor) obj)
  (if (or (value self) (eql (first (pw-controls self)) (first (input-objects self))))
      (the-list self)
      (let ((entry (patch-value (first (input-objects self)) obj)))
        (cond ((null entry)
               (setf (the-list self) (list (list 0 0) (list 0 0)))
               nil)
              ((consp (car entry))
               (setf (the-list self) (copy-tree entry)))
              (t (setf (the-list self) (list entry)) entry)))))

(defmethod open-patch-win ((self C-patch-list-editor))
  (when (not (wptr (application-object self)))
    (setf (application-object self) (make-application-object self))
    (set-pw-win+pw-obj (application-object self) pw::*active-patch-window* self))
  (let ((table (first (subviews (application-object self)))))
    (set-array table (the-list self))
    (set-view-size (application-object self)
                   (add-points (view-size table)
                               (make-point *cell-width* (* 2 *cell-height*))))
    (call-next-method)))

(in-package :pw)

(defunp lst-ed ((list (list (:dialog-item-text "()")))) list
        "This module is an editor for graphic tables. To open the window associated
with this module, click twice within the module (but not on the input window!).
To obtain more information, type ‘o’ with the module open."
        (declare (ignore list)))


;; (in-package :pw)
;; (add-patch-box *active-patch-window*
;; (make-patch-box  'C-patch-list-editor:C-patch-list-editor 'listEd
;; '(*symbol-eval-pw-type* "list") '(list)))

