;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               structured-time2.lisp
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


(defclass C-make-structured-win (C-patch) ()) 

(defmethod patch-value ((self C-make-structured-win) obj)
  (declare (ignore obj))
  (make-new-structured-window (patch-value (car (input-objects self)) ())))

(defvar strwin ()
"Strwin (structured window) allows collecting super-notes from Patch Work.
It should be connected to the ins input of the collector box
while collecting.The super-notes will contain a plain PW window.
Strwin box is normally used in context of structured time.
")
;;========================================================================================
(defclass C-make-structured-win+collector (C-patch) ()) 

(defmethod patch-value ((self C-make-structured-win+collector) obj)
  (declare (ignore obj))
  (let ((pw-win (make-new-structured-window 
                   (patch-value (car (input-objects self)) ())
                   (incf *pw-struct-collector-counter*))))
      (add-subviews pw-win (make-new-note-collector))
      pw-win))

(defvar strcoll ()
"Strcoll (structured collector) allows collecting super-notes from Patch Work.
It should be connected to the ins input of the collector box
while collecting.The super-notes will contain a PW window with a collector.
Strcoll box is normally used in context of structured time.
")

;;========================================================================================
(defclass C-make-structured-abstraction (C-patch) ()) 

(defmethod patch-value ((self C-make-structured-abstraction) obj)
  (declare (ignore obj))
    (let (pw-win) 
      (when (nth-connected-p self 0)
        (setq pw-win (eval (decompile (patch-win (car (input-objects self))))))
        (remove-subviews pw-win (car (find-abstract-out-box pw-win (controls pw-win))))
        (set-window-title pw-win 
             (concatenate  'string  (dialog-item-text (car (pw-controls self))) 
                  (format nil "~D" (incf *pw-struct-window-counter*)))))
        pw-win))

(defvar strabs ()
"Strabs (structured abstraction) allows collecting super-notes from Patch Work.
It should be connected to the ins input of the collector box while collecting.
To the input box of strabs box should be connected with an abstraction 
\(no input-boxes are allowed for the abstraction).
The super-notes will contain a PW window with copies of all PW boxes that are inside the 
original abstraction exept the 'about' box.
Strabs box is normally used in context of structured time.
")
