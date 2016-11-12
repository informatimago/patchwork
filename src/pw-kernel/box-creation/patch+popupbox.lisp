;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               patch+popupbox.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;  
;;;;    XXX
;;;;  
;;;;AUTHORS
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
;;;;  
;;;; -*- mode:lisp; coding:utf-8 -*-
;;;; A class for PW boxes with PopUp menu
;;;; User should subclass C-patch&popUp with a class having methods:
;;;;   get-local-menu :    returns a menu object. This will be the PopUp menu.
;;;;   get-default-char:   The default char to appear first in the box.
;;;;   do-menu-action:     This is passed the menu STRING of the menu item selected in the popUp menu.
;;;;                       Should perform whatever action is appropriate for the option.
;;;; The predefined function make-local-menu constructs a menu object given a list of strings for the items
;;;; in the popUp menu.

(in-package :pw)

(defclass C-patch&popUp (C-patch)
  ((popUpBox :initform nil :accessor popUpBox)
   (local-menu :initform '(("ONE" "O") ("TWO" "T") ("THREE" "H")) :accessor local-menu)
   (default-char :initform "O" :accessor default-char)
   (current-str :initform "M" :initarg :current-str :accessor current-str)))

(defmethod initialize-instance :after ((self C-patch&popUp) &key controls)
  (declare (ignore controls))
  (setf (current-str self) (default-char self))
  (setf (popUpBox self) 
        (make-popUpbox  (current-str self) self
                       (make-local-menu (local-menu self))
                       :view-position (make-point (- (w self) 13)
                                                  (- (h self) 14))
                       :view-container self
                       :view-font *patchwork-font-spec*)))

(defmethod decompile ((self C-patch&popUp))
  (append (call-next-method)
          `(nil ,(current-str self) )))

(defmethod complete-box ((self C-patch&popUp) output-type)
  (set-output self output-type))

(defun make-local-menu (menu-a-list) 
  (let (res)
    (apply #'new-menu " "
           (dolist (item menu-a-list (nreverse res))
             (push (new-leafmenu (menu-name item)
                                 (let ((menu-char (menu-char item)))
                                   (lambda ()
                                     (set-output *target-action-object* menu-char))))
                   res)))))

(defun menu-name (item) (first item))
(defun menu-char (item) (second item))

(defmethod set-output ((self C-patch&popUp) o-type)
  (setf (current-str self) o-type)
  (set-box-title (popUpBox self) (current-str self))
  (do-menu-action self o-type))

(defgeneric get-default-char (self)
  (:method ((self C-patch&popUp))
    (current-str self)))

(defgeneric do-menu-action (self o-type)
  (:method ((self C-patch&popUp) o-type)
    (declare (ignore o-type))
    nil))

(defgeneric get-local-menu (self)
  (:method ((self C-patch&popUp))
    nil))

#|
    TEST

    (defclass C-my-box (C-patch&popUp)
      ((local-menu :initform '(("ONE" "O") ("TWO" "T") ("THREE" "H")) :accessor local-menu)
       (default-char :initform "O" :accessor default-char)))

    (defmethod do-menu-action ((self C-my-box) str)
      (cond ((string= str "O") (print "OK"))
            (t (print str))))

    (defmethodp my-box C-my-box ((entry fix)) fix " "
      (+ entry 
         (cond ((string= (current-str self) "O") 1)
               ((string= (current-str self) "T") 2)
               ((string= (current-str self) "H") 3))))

    (pw-addmenu-fun (the-user-menu) 'my-box 'C-my-box)

|#
