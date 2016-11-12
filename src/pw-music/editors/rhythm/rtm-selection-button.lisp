;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               rtm-selection-button.lisp
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
(in-package :pw)

;;================================================================
(defclass C-beat-selection-button ()
  ((rtm-view-obj :initform ()  :accessor rtm-view-obj)
   (view-position :initform (make-point 0 0) :initarg :view-position :accessor view-position)
   (view-size :initform (make-point 0 0) :initarg :view-size :accessor view-size)
   (selection-button-function :initform ()  :accessor selection-button-function)))

(defmethod x ((self C-beat-selection-button)) (point-h (view-position self)))
(defmethod y ((self C-beat-selection-button)) (point-v (view-position self)))
(defmethod w ((self C-beat-selection-button)) (point-h (view-size self)))
(defmethod h ((self C-beat-selection-button)) (point-v (view-size self)))
(defgeneric connect-control (self func obj))
(defmethod connect-control ((self C-beat-selection-button) func obj) (setf (selection-button-function self) (cons func obj)))

(defmethod set-view-position ((self C-beat-selection-button) h &optional v)
  (setf (view-position self) (if v  (make-point h v) h)))
(defmethod set-view-size ((self C-beat-selection-button) h &optional v)
  (setf (view-size self) (if v  (make-point h v) h)))

(defgeneric inside-rtm-selection-buttons- (self where view)
  (:method ((self C-beat-selection-button) where view)
    (when (eql (rtm-view-obj self) view)
      (when (inside-rectangle? (point-h where) (point-v where) (x self) (y self) (w self) (h self))
        (setq *selection-buttons-x* (+ (x view)(x *active-rtm-window*) (point-h where)))
        (setq *selection-buttons-y* (+ (y view)(y *active-rtm-window*) (point-v where)))
        (let ((rtm-sel1 (rtm-selection-1 (view-container view)))
              butt)
          (fill-xor-view-selections view)
          (setf (rtm-selection-2 (view-container view))
                (if  (and
                      (shift-key-p)
                      (eql (class-name (class-of rtm-sel1))
                          (class-name (class-of (cdr (selection-button-function self))))))
                     rtm-sel1     ()))
          (setf (rtm-selection-1 (view-container view)) (cdr (selection-button-function self)))
          (with-focused-view view (fill-xor-view-contents** self))
          (when (rtm-selection-2 (view-container view))
            (setq butt (ask (selection-buttons view) #'selected-rtm-button? rtm-sel1))
            (when butt  (with-focused-view view (fill-xor-view-contents** butt))))
          (when (car  (selection-button-function self))
            (funcall (car (selection-button-function self))(cdr (selection-button-function self)) self))
          self)))))

(defgeneric selected-rtm-button\? (self obj)
  (:method ((self C-beat-selection-button) obj)
    (when (eql obj (cdr (selection-button-function self))) self)))

(defgeneric fill-xor-view-contents** (self)
  (:method ((self C-beat-selection-button))
    (with-pen-state (:mode :patXOR :pattern *black-pattern*)
      (fill-rect* (x self) (y self) (w self) (h self)))))

(defgeneric draw-view-contents** (self view)
  (:method ((self C-beat-selection-button) view)
    (with-pen-state  (:mode :srccopy :pattern *gray-pattern*)
      (draw-rect* (x self) (y self)(w self) (h self)))
    (when (or (eql (cdr (selection-button-function self)) (rtm-selection-1 view))
              (eql (cdr (selection-button-function self)) (rtm-selection-2 view)))
      (with-pen-state (:mode :patXOR :pattern *black-pattern*)
        (fill-rect* (x self) (y self) (w self) (h self))))))
;;================================================================

(defclass C-selection-buttons-pool ()
  ((buttons-pool :initform () :initarg :buttons-pool :accessor buttons-pool)
   (free-beat-selection-buttons :initform () :accessor free-beat-selection-buttons)))

(defmethod initialize-instance :after ((self C-selection-buttons-pool) &rest l)
 (declare (ignore l))
 (let (ctrls)
   (repeat 50 (push (make-instance 'C-beat-selection-button) ctrls))
   (setf (buttons-pool self) ctrls)
   (init-beat-selection-buttons-pool self)))

(defgeneric init-beat-selection-buttons-pool (self)
  (:method ((self C-selection-buttons-pool))
    (setf (free-beat-selection-buttons self)(copy-list (buttons-pool self)))))

(defgeneric add-to-free-buttons-pool (self butts)
  (:method ((self C-selection-buttons-pool) butts)
    (setf (free-beat-selection-buttons self)(append (free-beat-selection-buttons self) butts))))

(defgeneric give-beat-selection-button (self win x y w h)
  (:method ((self C-selection-buttons-pool) win x y w h)
    (let (ctrl)
      (setq ctrl
            (if (free-beat-selection-buttons self)
                (pop (free-beat-selection-buttons self))
                (progn (setq ctrl (make-instance 'C-beat-selection-button))
                       (push ctrl (buttons-pool self))
                       ctrl)))
      (set-view-position ctrl x y)
      (set-view-size ctrl w h)
      (when win (setf (rtm-view-obj ctrl) win))
      ctrl)))

(defun connect-to-selection-button (panel-obj rtm-obj x y w h function)
   (let ((selection-butt (give-beat-selection-button *selection-buttons-pool* panel-obj x y w h)))
       (add-to-selection-buttons panel-obj selection-butt)
       (connect-control selection-butt function rtm-obj)))

(defparameter *selection-buttons-pool* (make-instance 'C-selection-buttons-pool))
;;(length (buttons-pool *selection-buttons-pool*))
;;(length (free-beat-selection-buttons *selection-buttons-pool*))
;;(give-beat-selection-button  *selection-buttons-pool* () 0 0 0 0)
