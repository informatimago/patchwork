;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;=========================================================
;;;;
;;;;  PATCH-WORK
;;;;  By Mikael Laurson, Jacques Duthen, Camilo Rueda.
;;;;  © 1986-1992 IRCAM 
;;;;
;;;;========================================================= 

;;;;=============================================================
;;;;
;;;;       Pop-Up  Box class
;;;;
;;;;The class of boxes triggering a Pop-Up-Menu dialog when clicked at. If within a box,
;;;;then coordinates must be relative to box origin. The box class is responsible for 
;;;;building up the Pop up menu hierarchy
;;;;       
;==============================================================
;(defpackage :popUp-menu)

(in-package :patch-work)

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
  (declare (special *target-action-object*)
           (ignore mouse))
  (unwind-protect
    (progn
      (add-menu-items *pw-menu-apps* (menu self))    ;adds the PUMenu in the menu-bar 
      (let ((hdl (menu-handle (menu self)))
            (selection) (ID) (item) (menu-object)
            (global (local-to-global self (view-mouse-position  self))))
        (when hdl                                  ; just in case of weird menu deallocation
          (#_insertmenu  :ptr hdl :word -1)        ;this actually inserts the popUpMenu     
          (setq selection 
                (#_popUpMenuSelect 
                  :ptr hdl 
                  :word (point-v global)
                  :word (point-h global)
                  :word 1 :long))           ;selection is an encoding of ID plus ITEM chosen
          (setq ID (ldb (byte 16 16) selection) item (ldb (byte 16 0) selection))
          (unless (zerop ID)                                    ;0=no selection
            (setq menu-object  (gethash ID *menu-id-object-table*))
            (setf *target-action-object* (patch-box self))  ;necessary for proper funcalling of menu's methods
            (if (leafmenu-p menu-object)
              (menu-item-action menu-object)
              (menu-item-action (nth (1- item) (menu-items menu-object)) ))))))
    (remove-menu-items *pw-menu-apps*  (menu self))))
    
(defun leafmenu-p (menu)
  (string= (type-of menu) 'MENU-ITEM))

(defmethod set-box-title ((self C-PopUpbox) title)
  (set-dialog-item-text self title))

#|
(setq fi (make-instance 'window :window-title "fi"))
(setq foo (make-popupbox "A" fi *MN-popUpMenu* :view-position (make-point 100 100)
                         :view-container fi))
|#
