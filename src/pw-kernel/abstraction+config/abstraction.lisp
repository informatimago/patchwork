;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               abstraction.lisp
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

(defclass C-abstract-in (C-patch)
  ((in-index :initform nil :accessor in-index)
   (abstract-box :initform nil :accessor abstract-box)))

(defgeneric connect-to-nth-input (self nth-connection abstract-box)
  (:method ((self C-abstract-in) nth-connection abstract-box)
    (setf (in-index self) nth-connection)
    (setf (abstract-box self) abstract-box)))

(defmethod update-absin-doc-string ((self C-abstract-in))
  (when (abstract-box self)
    (setf (doc-string (nth (in-index self) (input-objects (abstract-box self))))
          (doc-string self))))

(defmethod doc-string ((self C-abstract-in)) (dialog-item-text (car (pw-controls self))))

(defmethod patch-value ((self C-abstract-in) obj)
  (when (abstract-box self)
    (patch-value (nth (in-index self) (input-objects (abstract-box self))) obj)))

;;so that clocked modules can be connected to abstractions
(defmethod init-patch ((self C-abstract-in))
  (when (abstract-box self)
    (tell (input-objects (abstract-box self)) 'init-patch)))

(defunp absin ((name absin) (order (fix>0 (:value 1)))) nil
"An abstraction (or a subpatch) must have one and only one  absout box,
 but it can have as many absin boxes as one might want (including none). 
Absins specify the input boxes of an abstraction-box. 
They are sorted by the value given by the second input  box. 
All  absin  boxes, in an abstraction must have diferent names and diferent numbers.

Please remember that when patching an absin module to another module, 
the name input of absin takes the same name as the variable linked to the patched window.

It is advised to give a name to the absin module after having made the patch connection. 
"
  (declare (ignore name order)))
;;=================================
;;    

(defclass C-abstract-out (C-patch) 
  ((abstract-obj :initform nil :accessor abstract-obj)))

(defgeneric update-absout-doc-string (self)
  (:method ((self C-abstract-out))
    (when (abstract-obj self)
      (let* ((new-string (dialog-item-text (car (pw-controls self))))
             (the-abstraction (abstract-obj self)))
        (set-window-title (view-window self) new-string)
        (setf (pw-function-string the-abstraction) new-string)
        (view-focus-and-draw-contents the-abstraction)))))

(defmethod toggle-patch-active-mode ((self C-abstract-out))
  (when (not (abstract-obj self))(call-next-method)))

(defmethod draw-patch-extra ((self C-abstract-out))
  (when (abstract-obj self)
    (draw-char (- (w self) 7) (- (h self) 4) #\†)))

(defunp absout ((name absout)) no-connection
"An abstraction (or a subpatch) must have one and only one absout-box, but 
it can have as many absin-boxes as you want (including none).
Absout is used to specify the output of an abstraction-box.
You can edit the name of the abstraction-box and the abstraction
window by editing the input-box of the absout-box."
name)

;;=================================

(defclass C-abstract (C-patch) 
  ((patch-win :initform nil :initarg :patch-win :accessor patch-win)
   (out-obj :initform nil :accessor out-obj)
   (abstract-in-list :initform nil :accessor abstract-in-list)))

(defmethod decompile ((self C-abstract))
  (let ((fun (pw-function self)))
  `(abst ',(type-of self) ',fun ,(pw-function-string self) ,(active-mode self)
          ,(view-position self) (list ,@(ask-all (pw-controls self) 'value))
          ',(cdar (get-intypes fun)) ',(get-out-type-list fun)
          ,(and (patch-win self) (decompile (patch-win self))))))

(defun abstr (class fun box-name posn values in-types out-type win-patches)
  (let ((arg-names (mapcar #'car in-types)))
    (set-PW-symbolic-type-data fun (list (cons '&required in-types)
                                           '(&optional) '(&rest)) out-type)
    ;; GA 190996 bug with arglist 
    ;(setf (fdefinition fun)
    ;      (eval `(function (lambda ,arg-names (declare (ignore ,@arg-names)) nil))))
    (setf (symbol-function fun)
          (eval `(function (lambda ,arg-names (declare (ignore ,@arg-names)) nil))))
    (setf (ccl:arglist fun) arg-names)
    ;; GA
    (box class fun box-name posn values nil win-patches)))

(defun abst (class fun box-name active? posn values in-types out-type win-patches)
  (let ((box (abstr class fun box-name posn values in-types out-type win-patches)))
    (setf (active-mode box) active?)
    (setf *pw-window-list*
          (remove (patch-win box) *pw-window-list*)) 
          box))

(defmethod complete-box ((self C-abstract) args)
  (when args
    (setf (patch-win self) args)
    (let ((out-box (car (find-abstract-out-box (patch-win self)
                                               (controls (patch-win self)))))
          (in-boxes (find-abstract-in-boxes (patch-win self)
                                            (controls (patch-win self)))))
      (setf (out-obj self) out-box)
      (setf (abstract-obj out-box) self)
      (for (i 0 1 (1- (length in-boxes)))
        (connect-to-nth-input (nth i in-boxes) i self))
      (window-hide (patch-win self))
      (tell (controls (patch-win self)) 'update-win-pointers (patch-win self))
      (setf (abstract-box (patch-win self)) self))))

(defmethod patch-value ((self C-abstract) obj) (patch-value (out-obj self) obj))

(defmethod init-patch ((self C-abstract)) (init-patch (out-obj self)))

(defmethod open-patch-win ((self C-abstract))
  (window-select (patch-win self)))

(defmethod draw-patch-extra ((self C-abstract))
  (draw-appl-label self
    (if (eql (front-window) (patch-win self)) #\* #\A))) 

(defmethod remove-yourself-control ((self C-abstract)) 
  (when (patch-win self)
    (tell (controls (patch-win self)) 'remove-yourself-control)  
    (window-close (patch-win self))))  


(defmethod draw-appl-label ((self C-abstract) label)
  (if (and (view-container  self) (wptr (view-container  self)))
    (with-focused-view self
      (set-view-font  (view-container  self) '(:srccopy))
      (draw-char (- (w self) 8) (- (h self) 4) label) 
      (set-view-font  (view-container  self) '(:srcor)))))

;;=================================

(defgeneric find-active-rectangle (self)
  (:method ((self C-pw-window))
    (let ((ctrls (active-patches self)) min-x min-y)
      (list (setq min-x (apply 'min (ask-all ctrls 'x))) 
            (setq min-y (apply 'min (ask-all ctrls 'y))) 
            (- (apply 'max (ask-all ctrls 'x+w)) min-x) 
            (- (apply 'max (ask-all ctrls 'y+h)) min-y))))) 


(defgeneric find-abstract-out-box (self patches)
  (:method ((self C-pw-window) patches)
    (let ((out-box))
      (while patches
        (when (eql (pw-function (car patches)) 'absout)
          (push (car patches) out-box))
        (pop patches))
      out-box)))

(defgeneric find-abstract-in-boxes (self patches)
  (:method ((self C-pw-window) patches)
    (let ((in-boxes))
      (while patches
        (when 
            (eql (pw-function (car patches)) 'absin)
          (push (car patches) in-boxes))
        (pop patches))
      (sort in-boxes '< :key (lambda (a) (patch-value (second (pw-controls a)) ()))))))

(defun make-pair-list-pw-types (type1 lst)
  (let ((res))
    (while lst (push type1 res)(push (pop lst) res) )
    (nreverse res)))
