;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;=========================================================
;;;;
;;;;  PATCH-WORK
;;;;  By Mikael Laurson, Jacques Duthen, Camilo Rueda.
;;;;  © 1986-1992 IRCAM 
;;;;
;;;;=========================================================

(in-package :pw)
 
;==========================================================================================
(defvar *global-current-value-patch* ())

#|
(defparameter *pw-debug-mode-menu*    
  (new-leafmenu "show error box" 'activate-current-patch-value-patch))
(defparameter *pw-remove-debug-mode-menu*    
  (new-leafmenu "remove pw debug" 'remove-PW-mode-method))

(let ((rem-menu (find-menu-item *PWoper-menu* "remove pw debug"))
      (show-menu (find-menu-item *PWoper-menu* "show error box")))
  (if rem-menu (remove-menu-items *PWoper-menu* rem-menu))
  (if show-menu (remove-menu-items *PWoper-menu* show-menu)))

(add-menu-items  *PWoper-menu* *pw-debug-mode-menu*)
(add-menu-items  *PWoper-menu* *pw-remove-debug-mode-menu* )

;==========================================================================================
(setf *break-on-errors* ())
|#

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
    (ed-beep)))


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
