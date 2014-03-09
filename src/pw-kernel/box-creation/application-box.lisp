;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               application-box.lisp
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


(defclass C-radio-button (static-text-dialog-item) ())

(defmethod type-list ((self C-radio-button)) )

(defmethod decompile ((self C-radio-button)) )

;;====================================================================================================
;; application-object is a pointer to a application-window object
;;====================================================================================================

(defclass  C-patch-application (C-pw-functional)
  ((application-object :initform nil :initarg :application-object 
                       :accessor application-object)
   (lock :initform nil :accessor lock)
   (value :initform nil :accessor value)
   (window-state :initform nil :accessor window-state)))

(defmethod initialize-instance :after ((self C-patch-application) &key controls)
  (declare (ignore controls))
  (unless (application-object self)
    (setf (application-object self) (make-application-object self)))
  (when (application-object self)
     (set-pw-win+pw-obj (application-object self) *active-patch-window* self))
   self)

(defgeneric -make-lock (self &optional position)
  (:method ((self C-patch-application) 
            &optional position)
    (setf (lock self)
          (make-instance 'C-radio-button
                         :view-position (or position (make-point 4 (- (h self) 9)))
                         :view-size (make-point 8 8)
                         :dialog-item-text (get-initial-button-text self)
                         :view-font '("Monaco" 8)
                         :view-container self
                         :dialog-item-action (get-lock-button-fun self)))))

(defgeneric get-lock-button-fun (self)
  (:method ((self C-patch-application))
    (lambda (item)
      (if (value (view-container item))
          (progn 
            (set-dialog-item-text item "o")
            (record-event :|PWst| :|cann| `((,:|----| ,(mkSO :|cbox| nil :|name| (pw-function-string self))))))
          (progn
            (set-dialog-item-text item "x")
            (record-event :|PWst| :|cand| `((,:|----| ,(mkSO :|cbox| nil :|name| (pw-function-string self)))))))
      (setf (value (view-container item))
            (not (value (view-container item)))))))

(defgeneric get-initial-button-text (self)
  (:method ((self C-patch-application))
    "o"))

(defgeneric save-window-state (self win)
  (:method ((self C-patch-application) win)
    (setf (window-state self) (get-window-state self win))))

(defgeneric get-window-state (self win)
  (:method ((self C-patch-application) win)
    (declare (ignore self win))))

(defgeneric put-window-state (self win state)
  (:method ((self C-patch-application) win state)
    (declare (ignore self win state))))

(defgeneric restore-window-state (self win)
  (:method ((self C-patch-application) win)
    (put-window-state self win (window-state self))))

;; this method should be redefined by subclasses and should return the pointer to the window
(defmethod make-application-object ((self C-patch-application))
  nil) 

(defgeneric set-pw-window-pointers (self win)
  (:method ((self C-patch-application) win) 
    (update-win-pointers self win)))

(defgeneric update-win-pointers (self win-ptr)
  (:method ((self C-patch-application) win-ptr) 
    (set-pw-win+pw-obj (application-object self) win-ptr ())))

(defmethod remove-yourself-control ((self C-patch-application)) 
  (when (application-object self) (window-close (application-object self))))  

(defmethod open-patch-win ((self C-patch-application))
  (let ((win (application-object self)))
    (unless (and win (wptr win))
      (setf (application-object self) (setq win (make-application-object self)))
      (set-pw-win+pw-obj win *active-patch-window* self)
      (restore-window-state self win))
    (window-select win)
    (draw-appl-label self #\*)))

(defmethod patch-value ((self C-patch-application) obj)
  (patch-value (application-object self) obj))

;;(defmethod draw-control-win ((self C-patch-application))
;;  (draw-control self (win self)))

(defgeneric draw-function-name (self x y))
(defmethod draw-function-name ((self C-patch-application) x y)
  (let* ((win (application-object self))
         (str (if (and win (wptr win))
                  (window-title win)  (pw-function-string self))))
    (if (second (pw-controls self))
        (draw-string x y str)
        (draw-string x y (subseq  str 0 (min 5 (length str) ))))))

(defmethod draw-patch-extra ((self C-patch-application))
  (when (application-object self)
    (draw-appl-label self
                     (if (eq (front-window) (application-object self)) #\* #\A)))) 
 
(defmethod draw-appl-label ((self C-patch-application) label)
  (when (view-container  self)
    (with-focused-view self
      (set-view-font  (view-container  self) '(:srccopy))
      (draw-char (- (w self) 8) (- (h self) 4) label) 
      (set-view-font  (view-container  self) '(:srcor)))))

(defmethod set-dialog-item-text-from-dialog ((view C-patch-application) str)
  (let ((win (application-object view)))
    (setf (pw-function-string view) (string-downcase str))
    (if (and win (wptr win)) (set-window-title win (string-downcase str)))
  (with-focused-view view
    (draw-function-name view 2 (- (h view) 5)))))

(defmethod save ((self C-patch-application))
  (call-next-method)
  (let ((win (application-object self)))
    (if (and win (wptr win)) 
        (set-window-title win (pw-function-string self)))))
;;====================================================================================================

#|

;;__________________
(defclass  C-patch-application-test (C-patch-application) ())

(defmethod make-application-object ((self C-patch-application-test))
  (make-test-application))

;;==================

(progn 
 (setq pw-appl (make-patch-box 'C-patch-application-test 'T-Appl ()))
 (add-patch-box *active-patch-window* pw-appl))

|#
