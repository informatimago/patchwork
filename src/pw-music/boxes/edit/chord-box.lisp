;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               chord-box.lisp
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

(defclass C-chord-box (C-ttybox)
  ((chord-line :initform 
     (make-instance 'C-chord-line 
        :chords (list
           (make-instance 'C-chord :t-time 0 
              :notes (list  (make-instance 'C-note :midic 6000))))) 
          :initarg :chord-line :accessor chord-line)))

(defmethod value ((self C-chord-box))  ;this is really the decompilation [911023]
  `(note
    ,@(mapcar #'get-useful-note-slots (notes (car (chords (chord-line self)))))))

(defmethod get-useful-note-slots ((self C-note))
  `(list ,(midic self) ,(dur self) ,(offset-time self) ,(order self) ,(comm self) ,(chan self) ,(vel self)
        ,(if (instrument self) (decompile (instrument self)))))

(defmethod (setf value) (notes (self C-chord-box))
  (setf (chord-line self)
        (make-instance 'C-chord-line 
                       :chords (list
                                (make-instance 'C-chord :t-time 0 
                                               :notes notes)))))

(defun note (&rest list) (apply 'form-note-objs list))

(defun form-note-objs (&rest list)
  (let (note instrument)
  (mapcar #'(lambda (note-form) 
              (setq note (make-instance 'C-note :midic (first note-form)
                                        :dur (second note-form)
                                        :offset-time (third note-form)
                                        :order (fourth note-form)
                                        :comm (fifth note-form)
                                        :chan (or (sixth note-form) 1)
                                        :vel (seventh note-form)
                                        :instrument  (setq instrument (eval (eighth note-form)))))
              (if instrument
                (make-super-note-connections instrument note *current-MN-window*))
              note)
          (apply #'list list))))
  
(defmethod give-mid-y ((self C-chord-box)) (+ 5 (y self)(truncate (/ (h self) 2))))

(defmethod draw-cb-staffs ((self C-chord-box) mid-y) 
  (draw-string 0 (- mid-y 31) "==")
  (draw-string 0 (- mid-y 3) "==")
  (draw-string 0 (+ mid-y 21) "==")
  (draw-string 0 (+ mid-y 49) "=="))

(defmethod view-draw-contents ((self C-chord-box))
  (let ((*mn-view-dyn-flag* nil)
        (*mn-view-dur-flag* nil)
        (*mn-view-ins-flag* nil)
        (*mn-view-offset-flag* nil)
        (*mn-view-order-flag* nil)
        (*staff-num* 6))
    (declare (special *mn-view-dyn-flag* *mn-view-dur-flag*
                      *mn-view-ins-flag* *mn-view-offset-flag*
                      *mn-view-order-flag* *staff-num*))
    (let ((mid-y (give-mid-y self))
          (pw-win-font (view-font (view-container (view-container  self)))))
      (with-focused-view self
        (set-view-font  (view-container (view-container  self))
                        '("MusNot-j"  18  :srcor))
        (if (chords (chord-line self))
         (draw-chord (car (chords (chord-line self))) 1 (+ 25 (x self)) 0 mid-y))
        (draw-cb-staffs self mid-y))
      (set-view-font  (view-container (view-container  self)) pw-win-font))))

(defmethod update-control ((self C-chord-box))
  (with-focused-view self
     (with-pen-state (:mode :srccopy :pattern *white-pattern*)
        (fill-rect* 0 0 (w self) (h self)))
     (view-draw-contents self)))

(defmethod view-double-click-event-handler ((self C-chord-box) where)
  (declare (ignore where)) )

(defmethod view-click-event-handler ((self C-chord-box) where)
  (declare (ignore where)) )

;;New value selection methods for the new chord patch box
(defmethod get-chord-midics ((self C-chord-box))
  (ask-all (notes (car (chords (chord-line self)))) 'midic))


(defmethod get-chord-offset ((self C-chord-box)) 
  (ask-all (notes (car (chords (chord-line self)))) 'offset-time))
 
(defmethod get-chord-duration ((self C-chord-box))
  (ask-all (notes (car (chords (chord-line self)))) 'dur))

(defmethod get-chord-dynamic ((self C-chord-box))
  (ask-all (notes (car (chords (chord-line self)))) 'vel))

(defmethod get-chord-order ((self C-chord-box))
  (ask-all (notes (car (chords (chord-line self)))) 'order))


