;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               mn-editor-polif.lisp
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

;;====================================================================================================

(defun make-polif-arg-list (count)
  (let ((arg-list))
    (for (i 0 1 (1- count)) 
         (push  '*MN-collector-type* arg-list)
         (push  (string-downcase 
                  (concatenate  'string  "coll" (format nil "~D" (1+ i)))) arg-list))
    (nreverse arg-list)))

;;(make-polif-arg-list 6)
        
;;====================================================================================================
(defclass  C-patch-PolifMN (C-patch-application)
  ((chord-line-list :initform nil :initarg :chord-line-list :accessor chord-line-list)))

(defmethod make-application-object ((self C-patch-PolifMN))
  (setf (application-object self)
        (make-n-music-notation-editors (length (pw-controls self))
                                       self
                                       'C-mn-window-mod 'C-MN-view-mod 'C-MN-panel-Mod
                                       (make-point 350 200))))

;;_________

(defmethod draw-patch-extra :after ((self C-patch-PolifMN))
  (draw-char (+ -16 (w self)) (- (h self) 4) #\E)) 
;;_________
;; extend

(defmethod correct-extension-box ((self C-patch-PolifMN) new-box values)
  (declare (ignore values))
  (let ((editors (editor-objects (car (subviews (application-object self)))))
        (new-editors-list (editor-objects (car (subviews (application-object new-box))))))
    (for (i 0 1 (1- (length editors)))
       (setf (chord-line (nth i new-editors-list)) (chord-line (nth i editors))))))

;;(defmethod generate-extended-inputs ((self C-patch-PolifMN)) (call-next-method))

(defmethod give-new-extended-title ((self C-patch-PolifMN)) 'pmnn) 

(defmethod mouse-pressed-no-active-extra :after ((self C-patch-PolifMN) x y) 
  (declare (ignore x y))
  (when (option-key-p) 
    (remove-yourself-control self)))
;;_________

(defmethod patch-value ((self C-patch-PolifMN) obj)
  (declare (ignore obj))
  (let ((editors (editor-objects (car (subviews (application-object self))))))
    (for (i 0 1 (1- (length editors)))
      (when (not (eql (nth i (pw-controls self))(nth i (input-objects self))))
        (setf (chord-line (nth i editors))
          (give-MN-editor-chord-line (nth i (input-objects self)) i))
        (view-draw-contents (nth i editors)))))
    (application-object self))

;;=================================================================
