;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;=========================================================
;;;;
;;;;  PATCH-WORK
;;;;  By Mikael Laurson, Jacques Duthen, Camilo Rueda.
;;;;  © 1986-1992 IRCAM 
;;;;
;;;;=========================================================

(in-package :pw)

(provide 'application-window)

;====================================================================================================
;(export '(C-application-window set-pw-win+pw-obj))
;(import '(patch-work:draw-appl-label-win))
;====================================================================================================

(defclass  C-application-window (window)  
  ((pw-win :initform nil :accessor pw-win)
   (pw-object :initform nil :accessor pw-object)))

(defmethod set-pw-win+pw-obj ((self C-application-window) win pw-object) 
  (setf (pw-win self) win)
  (when pw-object (setf (pw-object self) pw-object)))

;(defvar *active-application-rename-dialog* ())

; key-pressed
(defmethod view-key-event-handler ((self C-application-window) char)
     (cond 
        ((eq char  #\Newline)
           (when (pw-win self)
              (window-hide self)           
              (window-select (pw-win self))))
        ((eq char  #\Enter)  
           (when (pw-win self)
              (window-select (pw-win self))))
        ((eq char  #\R)
           (let ((string
              (get-string-from-user  "New name" :size (make-point 200 85) :position :centered
                                  :initial-string (window-title self))))
             (when string
               (set-window-title self string)
               (when (pw-object self)
                 (setf (pw-function-string (pw-object self)) string)
                 (with-focused-view (pw-object self) 
                   (view-draw-contents (pw-object self))))))) 
        ((eq char  #\h) (open-application-help-window self))
        ((key-pressed-extra self char))))

; this method should be inherited by subclasses
(defmethod key-pressed-extra ((self C-application-window) char) (declare (ignore char)) nil)


; saving
(defmethod decompile ((self C-application-window))
  (call-next-method))

; menus
(defmethod view-activate-event-handler :after ((self C-application-window))
  (when (pw-object self)
   (draw-appl-label (pw-object self) #\*)))

(defmethod view-deactivate-event-handler :after ((self C-application-window))
  (when (pw-object self)
   (draw-appl-label (pw-object self) #\A)))

; help window
(defmethod open-application-help-window ((self C-application-window)))

; patch-value -> giving a pointer to PW
(defmethod patch-value ((self C-application-window) obj) (declare (ignore obj)) self)

(defmethod window-close ((self C-application-window))
  (if (and (pw-object self) (wptr self)) (save-window-state (pw-object self) self))
  (call-next-method))

