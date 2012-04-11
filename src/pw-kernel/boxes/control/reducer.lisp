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
 
