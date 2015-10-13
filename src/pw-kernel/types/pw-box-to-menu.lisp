;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pw-box-to-menu.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;    
;;;;    The PW add-box-to-menu function.
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

(defun PW-addmenu (menu funs)
  "append to the menu <menu> the PW module generators from the list <funs>"
  (mapc (lambda (fun) (PW-addmenu-fun menu fun)) funs) )

(defun PW-addmenu-fun (menu fun &optional box-class)
  "append to the menu <menu> the PW module generator <fun>"
  (assert (or (null fun)
              (typep fun 'menu-element)
              (and (symbolp fun) (fboundp fun)))
          (fun) "~S is not a function or nil or a menu." fun)
  (cond
    ((null fun)
     (ui:add-menu-items menu (make-instance 'ui:menu-item
                                            :menu-item-title "-"
                                            :menu-item-action nil)))
    ((typep fun 'menu-element)
     (ui:add-menu-items menu fun))
    (t (if (defunp-function? fun)
           (new-PW-box-menu-item menu (string-downcase fun) fun box-class)
           (ui:add-menu-items menu
                              (make-instance 'ui:menu-item 
                                             :menu-item-title (string-downcase fun)
                                             :menu-item-action
                                             (lambda () 
                                               (make-lisp-pw-boxes
                                                fun *active-patch-window*))))))))

(defun new-PW-box-menu-item (main-menu mtitle function &optional box-class) 
  (if (not (fboundp function))
      (format t "~15A~25A" function "no such function !" )
      (multiple-value-bind (args extensible?) (make-defunp-function-arg-list function)
        ;; TODO: refactor to avoid eval:
        (let ((body `(make-PW-standard-box
                      ,(if box-class
                           `',box-class
                           (if extensible? ''C-pw-functional
                               (if (= (length args) 2) ''C-pw-resize-x ''C-patch)))
                      ',function))
              (sub-menu (find-menu-item main-menu mtitle)))
          (unless sub-menu
            (ui:add-menu-items main-menu
                               (setq sub-menu (make-instance 'menu-item :menu-item-title mtitle))))
          (push (eval `(function(lambda () ,body))) *PW-box-instance-list*)
          (set-menu-item-action-function sub-menu
                                         (lambda () (add-patch-box *active-patch-window* (eval body))))
          sub-menu))))


;; =============================================================================-======
;;
;;corrections to basic boxes
 
(defclass C-pw-functional (C-pw-extend) ())  

(defmethod give-new-extended-title ((self C-pw-functional)) )

#|
(defmethod generate-extended-inputs ((self C-pw-functional))
  (make-defunp-function-arg-list (pw-function self) (1+ (length (pw-controls self)))))
(defmethod mouse-pressed-no-active-extra ((self C-pw-functional) x y) 
  (declare (ignore x y) ) 
  (if (option-key-p) 
   (let ((box-now
           (make-PW-standard-box  (type-of self) (pw-function self)
              (make-point (x self) (y self))
              (append (ask-all (pw-controls self) 'value) (list :default))))
         (values (ask-all (pw-controls self) 'patch-value ())))
    (for (i 0 1 (1- (length values)))
      (when (not (eql (nth i (input-objects self)) (nth i (pw-controls self))))
        (setf (nth i (input-objects box-now)) (nth i (input-objects self)))
        (setf (open-state (nth i (pw-controls box-now))) nil)))
    (correct-extension-box self box-now values)
    (tell (controls (view-window self)) 'draw-connections t)
    (remove-subviews *active-patch-window* self)
    (add-patch-box *active-patch-window* box-now)
    (tell (controls *active-patch-window*) 'connect-new-patch? self box-now)
    (tell (controls *active-patch-window*) 'draw-connections))
    nil))
|#

(defmethod mouse-pressed-no-active-extra ((self C-pw-functional) x y) 
  (declare (ignore x y) ) 
  (if (option-key-p) 
   (let ((box-now
           (make-PW-standard-box  (type-of self) (pw-function self)
              (make-point (x self) (y self))
              (append (ask-all (pw-controls self) 'value) (list :default))))
         (values (ask-all (pw-controls self) 'patch-value ())))
     (if (and *current-small-inBox* (eql (view-container *current-small-inBox*) self))
       (kill-text-item))
    (for (i 0 1 (1- (length values)))
      (when (not (eql (nth i (input-objects self)) (nth i (pw-controls self))))
        (setf (nth i (input-objects box-now)) (nth i (input-objects self)))
        (setf (open-state (nth i (pw-controls box-now))) nil)))
    (correct-extension-box self box-now values)
    (tell (controls (view-window self)) 'draw-connections t)
    (remove-subviews *active-patch-window* self)
    (add-patch-box *active-patch-window* box-now)
    (tell (controls *active-patch-window*) 'connect-new-patch? self box-now)
    (tell (controls *active-patch-window*) 'draw-connections))
    nil))

(defmethod decompile ((self C-patch))
  (if (and (pw-function self) (defunp-function? (pw-function self)))
  `(sbox ',(type-of self) ',(pw-function self) ,(pw-function-string self)
         ,(active-mode self)
        ,(view-position self) (list ,@(ask-all (pw-controls self) 'value)))
  `(make-instance ',(class-name (class-of self))
               :view-position ,(view-position self)
               :view-size ,(view-size self)
               :active-mode  ,(active-mode self)
               :pw-function  ',(pw-function self)
               :type-list ',(type-list self)  
               :view-subviews (list ,@(ask-all (pw-controls self) 'decompile)))))

(defmethod decompile ((self C-pw-resize-x))
  (if (and (pw-function self) (defunp-function? (pw-function self)))
    `(sbox ',(type-of self) ',(pw-function self) ,(pw-function-string self) 
          ,(active-mode self) ,(view-position self)
          (list ,@(ask-all (pw-controls self) 'value)) ,(view-size self))
    (call-next-method)))

(defmethod complete-box ((self C-patch) args)
  (declare (ignore args)))

;;;; THE END ;;;;

