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
                       :view-font '("monaco"  9  :srcor))))

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
                                 (eval `(function 
                                         (lambda () (set-output *target-action-object* ,(menu-char item))))))
                   res)))))

(defun menu-name (item) (first item))
(defun menu-char (item) (second item))

(defmethod set-output ((self C-patch&popUp) o-type)
  (setf (current-str self) o-type)
  (set-box-title (popUpBox self) (current-str self))
  (do-menu-action self o-type))

(defmethod get-default-char ((self C-patch&popUp)) (current-str self))

(defmethod do-menu-action ((self C-patch&popUp) o-type) (declare (ignore o-type)) nil)

(defmethod get-local-menu ((self C-patch&popUp)) )

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
