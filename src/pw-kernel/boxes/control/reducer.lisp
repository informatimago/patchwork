;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               reducer.lisp
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

;;;==============================================================
;;;
;;; Class: C-reducer
;;; Inherits from: C-patch
;;; methods:
;;; patch-value         ;gets cycle-list from a first C-enumerator and initial-value
;;;                     ;from a second C-enumerator. Cycles through the list
;;;                     ; accumulating partial values in the second enumerator.
;;;===============================================================
(in-package :patch-work)

(defclass C-reducer (C-patch) ())

(defmethod patch-value ((self C-reducer) obj)
  (store-buffer (first (input-objects self)) 
                (get-list (first (input-objects self)) obj))
  (dolist (elem (get-list (third (input-objects self)) obj))
    (store-buffer (third (input-objects self)) elem)
    (store-buffer (first (input-objects self)) 
                  (patch-value (second (input-objects self)) obj)))
  (patch-value (first (input-objects self)) obj))

(defmethod disconnect-ctrl ((self C-reducer) ctrl)
  (if (or (eq ctrl (car (pw-controls self)))(eq ctrl (nth 2 (pw-controls self))))
     (progn (ed-beep)(format t "Disconnection of the first or third input is not allowed !"))
     (call-next-method)))

(defunp loop+accum ((c-red (symbol (:type-list (loop)))) 
                     (patch (list (:type-list ())))
                     (c-enum (symbol (:type-list (loop))))) list
"behaves like the CL Reduce function with a patch instead of a function"
  (declare (ignore c-red patch c-enum)))

(defunp pwreduce ((c-red (symbol (:type-list (loop)))) 
                     (patch (list (:type-list ())))
                     (c-enum (symbol (:type-list (loop))))) list
"The pwreduce module applies a function to a list of elements.  
The list is entered in enum and the function is defined by patch.   
.accum ;gives an initial value for the function, 
and serves to accumulate the results of the function for each step 
in the loop. In other words, pwreduce ;repeatedly applies a 
function--patch--to each of a list of elements--enum;--and puts the results 
of each successive evaluation in accum.   The output is the last computed result."
  (declare (ignore c-red patch c-enum)))
 
