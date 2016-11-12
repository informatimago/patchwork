;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pw-debug.lisp
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
(in-package :pw)

(defvar *global-current-value-patch* ())

;; (defparameter *pw-debug-mode-menu*
;;   (new-leafmenu "show error box" 'activate-current-patch-value-patch))
;; (defparameter *pw-remove-debug-mode-menu*
;;   (new-leafmenu "remove pw debug" 'remove-PW-mode-method))
;;
;; (let ((rem-menu (find-menu-item *PWoper-menu* "remove pw debug"))
;;       (show-menu (find-menu-item *PWoper-menu* "show error box")))
;;   (if rem-menu (remove-menu-items *PWoper-menu* rem-menu))
;;   (if show-menu (remove-menu-items *PWoper-menu* show-menu)))
;;
;; (ui:add-menu-items  *PWoper-menu* *pw-debug-mode-menu*)
;; (ui:add-menu-items  *PWoper-menu* *pw-remove-debug-mode-menu* )
;;
;; ;;==========================================================================================
;; (setf *break-on-errors* ())


(defun flip-pw-debug ()
  (setf *global-current-value-patch* ())
  (if (menu-item-check-mark *pw-debug-menu*)
      (remove-PW-mode-method)
      (set-patch-value-methods)))

(defun set-patch-value-methods ()
  (set-menu-item-check-mark *pw-debug-menu* t)
  (defmethod patch-value :before ((self C-patch) obj)
    (declare (ignore obj))
                                        ;  (activate-control self)
    (setf *global-current-value-patch* (adjoin self *global-current-value-patch*)))

  (defmethod patch-value :after ((self C-patch) obj)
    (declare (ignore obj))
                                        ;   (print 'after)
                                        ;  (deactivate-control self)
    (setf *global-current-value-patch* (delete self *global-current-value-patch* :test #'eq))))

(defun activate-current-patch-value-patch ()
  (if (menu-item-check-mark *pw-debug-menu*)
      (when *global-current-value-patch*
        (window-select (view-window (car *global-current-value-patch*)))
        (tell (controls (view-window (car *global-current-value-patch*)))
              #'deactivate-control)
        (activate-control (car *global-current-value-patch*))
        (setf *global-current-value-patch* ()))
      (ui:ed-beep)))


(defun remove-PW-mode-method ()
  (set-menu-item-check-mark *pw-debug-menu* ())
                                        ;(setf *break-on-errors* t)
  (let* ((generic-function (symbol-function 'patch-value))
         (method (find-method generic-function
                              '(:before)
                              (list (find-class 'C-patch)(find-class t)))))
    (remove-method generic-function method))
  (let* ((generic-function (symbol-function 'patch-value))
         (method (find-method generic-function
                              '(:after)
                              (list (find-class 'C-patch)(find-class t)))))
    (remove-method generic-function method)
    (setf *global-current-value-patch* ())))

;;;; THE END ;;;;
