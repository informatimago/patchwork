;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               popupmenu.lisp
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

;;;=============================================================
;;;
;;;       Pop-Up  Box class
;;;
;;;The class of boxes triggering a Pop-Up-Menu dialog when clicked at. If within a box,
;;;then coordinates must be relative to box origin. The box class is responsible for 
;;;building up the Pop up menu hierarchy
;;;       
;;;==============================================================


(defclass C-PopUpbox (static-text-dialog-item)
   ((menu :initarg :menu :accessor menu)
    (patch-box :initform nil :initarg :patch-box :accessor patch-box) ) )

;;;Non de-compilable class for the moment (5/3/91). Decompilation shouldn't really be necessary
;;;since popUpBoxes are esentially static entities. The Menu is always known to the object.

(defvar *target-action-object* ())  ; holds the patch object owning the PopUpBox

(defun make-popUpbox(name object menu &rest initargs)                         
  "creates a popUpbox called <name>, hooked up to a popUpMenu <menu>
located in the x,y position specified by the &rest key arguments"

  (let ((the-box (apply #'make-instance 'C-popUpbox 
                        :dialog-item-text name
                        :menu menu
                        initargs)))    
    (setf (patch-box the-Box) object)
    the-Box) )

(defmethod view-click-event-handler ((self C-PopUpbox) mouse)
  (unwind-protect
    (progn
      (ui:add-menu-items *pw-menu-apps* (menu self))    ;adds the PUMenu in the menu-bar
      (niy view-click-event-handler self mouse)
      ;; (let ((hdl (menu-handle (menu self)))
      ;;       (selection) (ID) (item) (menu-object)
      ;;       (global (local-to-global self (view-mouse-position  self))))
      ;;   (when hdl                                  ; just in case of weird menu deallocation
      ;;     (#_insertmenu  :ptr hdl :word -1)        ;this actually inserts the popUpMenu     
      ;;     (setq selection 
      ;;           (#_popUpMenuSelect 
      ;;             :ptr hdl 
      ;;             :word (point-v global)
      ;;             :word (point-h global)
      ;;             :word 1 :long))           ;selection is an encoding of ID plus ITEM chosen
      ;;     (setq ID (ldb (byte 16 16) selection) item (ldb (byte 16 0) selection))
      ;;     (unless (zerop ID)                                    ;0=no selection
      ;;       (setq menu-object  (gethash ID *menu-id-object-table*))
      ;;       (setf *target-action-object* (patch-box self))  ;necessary for proper funcalling of menu's methods
      ;;       (if (leafmenu-p menu-object)
      ;;         (menu-item-action menu-object)
      ;;         (menu-item-action (nth (1- item) (menu-items menu-object)) )))))
      )
    (remove-menu-items *pw-menu-apps*  (menu self))))
    
(defun leafmenu-p (menu)
  (string= (type-of menu) 'MENU-ITEM))

(defgeneric set-box-title (self title)
  (:method ((self C-PopUpbox) title)
    (set-dialog-item-text self title)))

#|
(setq fi (make-instance 'window :window-title "fi"))
(setq foo (make-popupbox "A" fi *MN-popUpMenu* :view-position (make-point 100 100)
                         :view-container fi))
|#
