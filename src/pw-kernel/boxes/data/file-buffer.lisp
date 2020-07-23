;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               file-buffer.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;
;;;;    A module with a Fred-Window
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
(in-package "C-PATCH-FILE-BUFFER")

(defparameter *file-box-popUpMenu*
  (new-menu " "
            (new-leafmenu "New"
                          (lambda () (get-new *target-action-object*)))
            (new-leafmenu "Open"
                          (lambda () (get-file *target-action-object*)))
            (new-leafmenu "Open File"
                          (lambda () (get-different-file *target-action-object*)))
            (new-leafmenu "Save Win"
                          (lambda () (Save-win *target-action-object*)))
            (new-leafmenu "-" ())))

(defvar *ascii-option*
  (new-leafmenu "Ascii"
              (lambda () (set-ascii-win *target-action-object*))))

(defvar *lisp-option*
  (new-leafmenu "Lisp"
              (lambda () (set-lisp-win *target-action-object*))))

(defvar *add-mode-option*
  (new-leafmenu "Add"
              (lambda () (set-add-mode *target-action-object*))))

(defvar *replace-mode-option*
  (new-leafmenu "Replace"
              (lambda () (set-replace-mode *target-action-object*))))

(defvar *add-option* t)

(defvar *lisp-win-option* t)

(defclass C-patch-file-buffer (C-patch)
  ((fred-win :initform nil :initarg :fred-win :accessor fred-win)
   (popUpBox :initform nil :accessor popUpBox)
   (file-name :initform nil :initarg :file-name :accessor file-name)))

(defmethod decompile ((self C-patch-file-buffer))
  (append (call-next-method) `(nil ',(file-name self))))

(defmethod complete-box ((self C-patch-file-buffer) filename)
  (setf (file-name self) filename))

(defmethod initialize-instance :after ((self C-patch-file-buffer) &key ctrl)
  (declare (ignore ctrl))
  (set-lisp-win self)
  (set-add-mode self)
  (setf (popUpBox self)
        (make-popUpbox  "A" self
                       *file-box-popUpMenu*
                       :view-position (make-point (- (w self) 13)
                                                  (- (h self) 14))
                       :view-container self
                       :view-font pw::*patchwork-font-spec*)))

(defmethod patch-value ((self C-patch-file-buffer) obj)
  (when (not (eql (car (pw-controls self)) (car (input-objects self))))
    (unless (and (fred-win self) (wptr (fred-win self)))
      (get-new self))
    (let ((fmat (patch-value (second (input-objects self)) obj)))
      (add-to-file self
                   (patch-value (first (input-objects self)) obj)
                   (if (zerop fmat) most-positive-fixnum fmat))))
  (let ((win (fred-win self)))
    (if (and win (wptr win))
      (let* ((fred-buff (fred-buffer win))
             (size (buffer-size fred-buff))
             (pos (buffer-skip-fwd-wsp&comments fred-buff 0 size))
             result)
        (when pos
          (with-cursor *watch-cursor*
            (multiple-value-bind (beg end) (selection-range win)
              (unless (= beg end)
                (setq pos beg size end)))
            (do  (i) ((> pos size) result)
              (declare (ignore i))
              (multiple-value-bind (exp length) (buffer-current-sexp fred-buff pos)
                (if (<= length pos) (return))
                (push exp result)
                (setq pos
                      (or (buffer-skip-fwd-wsp&comments fred-buff length size)
                          (1+ size))))))
          (nreverse result))))))

(defmethod update-box-name ((self C-patch-file-buffer) name)
  (setf (pw-function-string self)  (pathname-name name)))

(defmethod get-file ((self C-patch-file-buffer))
  (cond ((and (fred-win self) (wptr (fred-win self)))
         (window-select (fred-win self)))
        ((fred-win self) (get-selected-file self))
        ((file-name self)
         (let* ((f-name (file-name self))
                (w-name (file-namestring f-name))
                (win (find-window w-name)))
           (if (and win (wptr win))
             (window-select win)
             (get-selected-file self))))
        (t (get-selected-file self))))

(defmethod get-different-file ((self C-patch-file-buffer))
  (setf (file-name self) nil)
  (get-selected-file self))

(defmethod remove-yourself-control ((self C-patch-file-buffer))
  (let ((win (fred-win self)))
    (if (and win (wptr win))
      (window-close win))
    (call-next-method)))

(defmethod get-selected-file ((self C-patch-file-buffer))
  (niy get-selected-file self)
  ;; (let ((name (if (and (file-name self)
  ;;                       (not (string= (file-namestring (file-name self)) "New")))
  ;;                (file-name self)
  ;;                (CHOOSE-FILE-DIALOG))))
  ;;    (ui:with-cursor *watch-cursor*
  ;;     (setf (fred-win self)
  ;;           (make-instance 'fred-window :window-show nil))
  ;;     (setf (file-name self) name)
  ;;     (set-window-filename (fred-win self) name)
  ;;     (buffer-insert-file (fred-buffer (fred-win self)) name)
  ;;     (update-box-name self name)
  ;;     (window-select (fred-win self))))
  )

(defmethod get-new ((self C-patch-file-buffer))
  (setf (file-name self) "New")
  (setf (fred-win self) (make-instance 'fred-window))
  (update-box-name self (file-name self)))

(defmethod save-win ((self C-patch-file-buffer))
  (niy save-win self)
  ;; (let ((win (fred-win self)))
  ;;   (when (and win (wptr win))
  ;;     (unless (and (file-name self) (pathnamep (file-name self)))  ;;(not (string= "New" (file-name self))))
  ;;       (setf (file-name self) (CHOOSE-NEW-FILE-DIALOG))
  ;;       (set-window-filename win (file-name self)))
  ;;     (buffer-write-file (fred-buffer win) (file-name self) :if-exists :supersede)
  ;;     (update-box-name self (file-name self))))
  )

(defmethod add-to-file ((self C-patch-file-buffer) list format)
  (declare (ignorable list format))
  (niy add-to-file self list format)
  ;; (let ((count 0)
  ;;       (format (if (zerop format) (length list) format))
  ;;       (mark (fred-buffer
  ;;              (if (not (and (fred-win self) (wptr (fred-win self))))
  ;;                (get-new self) (fred-win self)))))
  ;;   (unless *add-option*
  ;;   (select-all (fred-win self))
  ;;   (ed-kill-selection (fred-win self)))
  ;;   (dolist (item (list! list))
  ;;     (if (characterp item)
  ;;       (buffer-insert mark (format nil (coerce (list item) 'string)))
  ;;       (progn
  ;;         (buffer-insert mark (format nil " ~S" item))
  ;;         (if (zerop (rem (incf count) format))
  ;;           (buffer-insert mark (format nil "~%")))))))
  )

(defmethod set-ascii-win ((self C-patch-file-buffer))
  (setf *lisp-win-option* ())
  (remove-menu-items *file-box-popUpMenu* *ascii-option*)
  (ui:add-menu-items *file-box-popUpMenu* *lisp-option*))

(defmethod set-lisp-win ((self C-patch-file-buffer))
  (setf *lisp-win-option* t)
  (remove-menu-items *file-box-popUpMenu* *lisp-option*)
  (unless (member *ascii-option* (menu-items *file-box-popUpMenu*))
    (ui:add-menu-items *file-box-popUpMenu* *ascii-option*)))

(defmethod set-add-mode ((self C-patch-file-buffer))
  (setf *add-option* t)
   (remove-menu-items *file-box-popUpMenu* *add-mode-option*)
   (unless (member *replace-mode-option* (menu-items *file-box-popUpMenu*))
    (ui:add-menu-items *file-box-popUpMenu* *replace-mode-option*)))

(defmethod set-replace-mode ((self C-patch-file-buffer))
  (setf *add-option* ())
   (remove-menu-items *file-box-popUpMenu* *replace-mode-option*)
   (unless (member *add-mode-option* (menu-items *file-box-popUpMenu*))
    (ui:add-menu-items *file-box-popUpMenu* *add-mode-option*)))

(in-package :pw)

(defunp Text-win ((list list) (fmat (fix>=0 (:value 4)))) list
    " This module lets one create and communicate with a Lisp text window. The
new window is created by choosing NEW in the front menu (click on A at right
to open the menu). The new window appears and makes PatchWork switch to
Lisp. To return to PatchWork click on the PatchWork window or type Ù-1. It is
possible to write in this window, either directly (returning to Lisp) or by entering
data by the input at the left of the module. The connection of a patch at the left
input list and the evaluation of the module  text-win ;also makes a window and
switches to Lisp.

To save data, evaluate the text-win module.

The front menu presents six options:

new     creates a new window and links it to the text-win module
open    opens (selects) the window linked to the module.
open-file       opens a Macintosh dialog box for retrieving a text window to be
linked to the module.
 save-win;      lets one save the current window in a file,
 ASCII ;or  Lisp;       an option that selects the format of the data written on the
window.

When this module is selected the default format is Lisp expressions. In this
case the third option is ASCII, which lets one change the format of the data. If
the ASCII option is chosen, the third option is LISP, which makes it possible to
return to the format of Lisp expressions.

Replace or Add   an option that selects whether adding or replacing data on the
linked window. When this module is selected the default option is Add ,  this
meaning that  input data is added to the end of the window. In this case the
option is Replace .

If the Replace option is chosen ,'input data replaces all previous contents of the
window, and in this case the option is Add .

The second entry of the module  fmat ;(at the right) determines how many
elements will be written per line in the corresponding text window. For further
information; type h with the selected module open. "
  (declare (ignore list fmat)))

(in-package "C-PATCH-FILE-BUFFER")

(defclass C-patch-ascii-buffer (C-patch-file-buffer) ())

(defmethod patch-value ((self C-patch-ascii-buffer) obj)
  (declare (ignorable obj))
  (if *lisp-win-option*
      (call-next-method)
      (niy patch-value self obj)
      ;; (when (and (fred-win self) (wptr (fred-win self)))
      ;;   (if (not (eql (car (pw-controls self)) (car (input-objects self))))
      ;;     (add-to-file self
      ;;                  (patch-value (first (input-objects self)) obj)
      ;;                  (patch-value (second (input-objects self)) obj)))
      ;;   (let ((win (fred-win self)))
      ;;     (if (and win (wptr win))
      ;;       (let* ((fred-buff (fred-buffer win))
      ;;              (size (buffer-size fred-buff))
      ;;              (pos (get-buffer-next-pos fred-buff 0 size))
      ;;              result)
      ;;         (when pos
      ;;           (with-cursor *watch-cursor*
      ;;             (multiple-value-bind (beg end) (selection-range win)
      ;;               (unless (= beg end)
      ;;                 (setq size end)
      ;;                 (setq pos (get-buffer-next-pos fred-buff beg size))))
      ;;             (if pos
      ;;               (do  (i) ((>= pos size) result)
      ;;                 (declare (ignore i))
      ;;                 (multiple-value-bind (exp pos1)
      ;;                                      (get-buffer-next-token fred-buff pos size)
      ;;                   (push exp result)
      ;;                   (setq pos
      ;;                         (or (get-buffer-next-pos fred-buff pos1 size) size))))))
      ;;           (nreverse result))))))
      ))

(defvar *token-separators* '(#\Space #\Tab #\Newline))

(defun get-buffer-next-pos (fred-buff pos size)
  (if (>= pos size)   nil
      (if (member (buffer-char fred-buff pos) *token-separators*)
        (get-buffer-next-pos fred-buff (1+ pos) size)
        pos)))

(defun get-buffer-next-token (fred-buff pos size)
  (let ((result (list (buffer-char fred-buff pos)))
        (pos1 (1+ pos)) ch)
    (while (and (< pos1 size)
              (not (member (setq ch (buffer-char fred-buff pos1)) *token-separators*)))
      (push ch result)
      (incf pos1))
    (values (coerce (nreverse result) 'string) pos1)))

(defmethod add-to-file ((self C-patch-ascii-buffer) list format)
  (if *lisp-win-option*
    (call-next-method)
    (let* ((list (list! list))
           (count 0)
           (format (if (zerop format) (length list) format))
           (mark (fred-buffer
                  (if (not (and (fred-win self) (wptr (fred-win self))))
                    (get-new self) (fred-win self)))))
      (dolist (item list)
        (buffer-insert mark (format nil " ~A" item))
        (if (zerop (rem (incf count) format))
          (buffer-insert mark (format nil "~%")))))))

(defunp ascii-win ((list list) (fmat (fix>0 (:value 4)))) list
    "A box linked with an ASCII file. outputs (as a list of LISP strings)
either the whole file contents or the selected zone (if any).
Menu options are:
  New:  Links a new LISP editing window with the box.
  Open: Gets a user selected file and links it with the box"
  (declare (ignore list fmat)))


;; (setq foo (make-instance 'fred-window))
;; (setq fi (find-window "New"))
;; (setq fa (fred-buffer foo))
;;
;; (buffer-current-sexp fa 20)
;; (buffer-size fa)
;; (in-package :pw)
;; (add-patch-box *active-patch-window*
;;                (make-patch-box  'C-patch-file-buffer:C-patch-file-buffer 'f-buff
;;                                () '(list)))
;; (setq fu (car (subviews pw::*active-patch-window* )))
;; (setq fuwin (C-patch-file-buffer::fred-win fu))
;; (setq fumark (fred-buffer fuwin))
;; (setq fuexp (buffer-size fumark))
;; (multiple-value-bind (a b) (buffer-current-sexp fumark 11789) (list a b))
;; (buffer-current-sexp-bounds fumark)
;; (buffer-skip-fwd-wsp&comments fumark 516 400)
;; (selection-range fuwin)
