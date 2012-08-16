;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               application-window.lisp
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
;;;;=========================================================
;;;;
;;;;  PATCH-WORK
;;;;  By Mikael Laurson, Jacques Duthen, Camilo Rueda.
;;;;  © 1986-1992 IRCAM 
;;;;
;;;;=========================================================

(in-package :pw)

(defclass  C-application-window (window)  
  ((pw-win :initform nil :accessor pw-win)
   (pw-object :initform nil :accessor pw-object)))

(defmethod set-pw-win+pw-obj ((self C-application-window) win pw-object) 
  (setf (pw-win self) win)
  (when pw-object (setf (pw-object self) pw-object)))

;;(defvar *active-application-rename-dialog* ())

;; key-pressed
(defmethod view-key-event-handler ((self C-application-window) char)
  (case char 
    ((:Newline)
     (when (pw-win self)
       (window-hide self)           
       (window-select (pw-win self))))
    ((:Enter)  
     (when (pw-win self)
       (window-select (pw-win self))))
    ((#\R)
     (let ((string
            (get-string-from-user  "New name" :size (make-point 200 85) :position :centered
                                   :initial-string (window-title self))))
       (when string
         (set-window-title self string)
         (when (pw-object self)
           (setf (pw-function-string (pw-object self)) string)
           (with-focused-view (pw-object self) 
             (view-draw-contents (pw-object self))))))) 
    ((#\h) (open-application-help-window self))
    (otherwise (key-pressed-extra self char))))

;; this method should be inherited by subclasses
(defmethod key-pressed-extra ((self C-application-window) char) (declare (ignore char)) nil)


;; saving
(defmethod decompile ((self C-application-window))
  (call-next-method))

;; menus
(defmethod view-activate-event-handler :after ((self C-application-window))
  (when (pw-object self)
    (draw-appl-label (pw-object self) #\*)))

(defmethod view-deactivate-event-handler :after ((self C-application-window))
  (when (pw-object self)
    (draw-appl-label (pw-object self) #\A)))

;; help window
(defmethod open-application-help-window ((self C-application-window)))

;; patch-value -> giving a pointer to PW
(defmethod patch-value ((self C-application-window) obj) (declare (ignore obj)) self)

(defmethod window-close ((self C-application-window))
  (if (and (pw-object self) (wptr self)) (save-window-state (pw-object self) self))
  (call-next-method))

