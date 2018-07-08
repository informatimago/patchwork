;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               buffer-box.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;
;;;;    A general buffer box
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
(in-package "C-PATCH-BUFFER")

;; (defclass C-radio-button (static-text-dialog-item) ())
;;
;; (defmethod type-list ((self C-radio-button)) )
;;
;; (defmethod decompile ((self C-radio-button)) )

(defclass C-patch-buffer (C-patch)
  ((the-buffer :initform nil :initarg :the-buffer :accessor the-buffer)
   (lock :initform nil :accessor lock)
   (value :initform nil :accessor value)))

(defmethod decompile ((self C-patch-buffer))
  `(sbox ',(type-of self) ',(pw-function self) ,(pw-function-string self)
         ,(active-mode self) ,(view-position self) '(0)
         ,(make-point (w self) (- (h self) 7))))

(defmethod initialize-instance :after ((self C-patch-buffer) &key ctrls)
  (declare (ignore ctrls))
  (set-view-size self (w self) (+ (h self) 7))
  (set-view-position (out-put self)
                     (make-point (x (out-put self)) (+ (y (out-put self)) 7)))
  (setf (lock self)
        (make-instance 'C-radio-button
                       :view-position (make-point (- (truncate (w self) 2) 5)
                                                  (- (h self) 22))
                       :view-size (make-point 8 8)
                       :dialog-item-text (get-initial-button-text self)
                       :view-font '("Monaco" 8)
                       :view-container self
                       :dialog-item-action (get-lock-button-fun self))))

(defmethod get-lock-button-fun ((self C-patch-buffer))
  (lambda (item)
    (if (value (view-container item))
      (set-dialog-item-text item "o")
      (set-dialog-item-text item "x"))
    (setf (value (view-container item))
          (not (value (view-container item))))))

(defmethod get-initial-button-text ((self C-patch-buffer)) "o")

(defmethod patch-value ((self C-patch-buffer) obj)
  (let ((in (car (input-objects self))))
    (if (value self)
        (the-buffer self)
        (setf (the-buffer self) (patch-value in obj)))))

(defunp Buffer ((buff nilNum)) nil
        "The buffer module stores the results of patch calculations connected to its
input. It has two states: open (indicated by a small o on the module) and closed
\(indicated by x). The user can switch between these two states by clicking on
the o or the x. When the module is open, it behaves exactly like the module
const. When it is closed it returns the last value evaluated. It is advisable to
close the module immediately after evaluation to avoid recalculating the input."
  (declare (ignore buff)))

;; (in-package :pw)
;; (add-patch-box *active-patch-window*
;;                (make-patch-box  'C-patch-buffer:C-patch-buffer 'Buff
;;                                '(*nil-numbox-pw-type* "value") '()))
;; (unintern 'pw-function)



(in-package "C-PATCH-ACCUM")

(defvar *accum-buffer-limit* 400)

;;changed by aaa 29-08-95 from pw-modifs
(defclass C-patch-accum (C-patch-buffer pw::C-pw-functional)
  ((my-buffer-limit :initform 400 :accessor my-buffer-limit)
   (the-buffer :initform (make-list 400 :initial-element 0)
               :accessor the-buffer)
   (end-ptr :initform 0 :accessor end-ptr)
   (end-limit :initform 0 :accessor end-limit)
   (accum-buffer-limit :initform 400 :accessor accum-buffer-limit)))



(defmethod get-lock-button-fun ((self C-patch-accum))
  (lambda (item)
    (if (value (view-container item))
        (progn (set-dialog-item-text item "o")
               (init-buffer self))
        (set-dialog-item-text item "x"))
    (setf (value (view-container item))
          (not (value (view-container item))))))

(defmethod init-buffer ((self C-patch-accum))
  (setf (end-ptr self) 0)
  (setf (end-limit self) 0))


(defmethod patch-value ((self C-patch-accum) obj)
  (let ((new-size (and (second (pw-controls self))
                       (patch-value (second (input-objects self)) obj) ))
        (limit (my-buffer-limit self)))
    (when new-size
      (if (> new-size limit)
        (setf (cdr (last (the-buffer self))) (make-list (- new-size limit))
              (my-buffer-limit self) new-size))
      (setf (accum-buffer-limit self) new-size))
    (if (value self)
      (subseq (the-buffer self) 0 (end-limit self))
      (progn
        (unless (<= (incf (end-limit self)) (accum-buffer-limit self))
          (decf (end-limit self)))
        (setf (nth (end-ptr self) (the-buffer self))
              (patch-value (first (input-objects self)) obj))
        (or (< (incf (end-ptr self)) (accum-buffer-limit self)) (setf (end-ptr self) 0))
        (subseq (the-buffer self) 0 (end-limit self))))))

(defunp accum ((data Nilnum)
               &optional (nb-elems fix>0 (:value 400))) nil
 " The accum module accumulates results of calculations of a patch that is
connected to its input. It has two states: open (indicated by a small ‘o’ on the
module) and closed (indicated by 'x'). The user can switch between these two
states by clicking on the 'o' or the 'x' When the module is open, it accumulates
in a list the result of each evaluation—of the patch connected to its input, or the
value at its input. When it is closed it returns the last value evaluated. The
module is reinitialized by a change of state from closed to open. This module
takes a list of maximum length 400 elements. This value can be modified by the
opening of the optional input to the accum module, by clicking on the 'E' found
on the right. When the list reaches its maximum value the resulting list begins to
wraparound in a circular fashion, writing over old values. "
  (declare (ignore data nb-elems)))

;;;; THE END ;;;;
