;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               oscilloscope.lisp
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
;;====================================================================================================
(defclass C-pw-oscilloscope (C-pw-resize-x) 
  ((clock :initform 0 :accessor clock)
   (x-now :initform 0 :accessor x-now)
   (play-flag :initform nil :accessor play-flag)))

(defmethod resize-patch-box ((self C-pw-oscilloscope) mp delta)
 (let ((point-now (make-point (add-points mp delta)))
       (min-w 84)(min-h 60))
   (when (and (< min-w (point-h point-now))(< min-h (point-v point-now))) 
      (set-view-size self point-now)
      (set-view-size (third (pw-controls self)) 
          (make-point (- (w self) 10) (- (h self) 35)))
      (set-view-position (second (pw-controls self)) 
          (make-point (- (w self) 5 (w (second (pw-controls self)))) 5))
      (init-xs-ys self)
      (set-view-position (out-put self) 
          (make-point (- (round (w self) 2) 6) (- (h self) 5))))))

;;__________
(defmethod draw-patch-extra :after ((self C-pw-oscilloscope))
  (when (play-flag self) (fill-patch-outrect (out-put self))))     

(defmethod patch-value ((self C-pw-oscilloscope) obj)(declare (ignore obj)) nil)

(defmethod play ((self C-pw-oscilloscope))
  (tell (input-objects self) 'init-patch)
  (setf (play-flag self) t)
  (with-focused-view self
     (view-draw-contents (third (pw-controls self))))       
  (fill-patch-outrect (out-put self))      
  (setf (clock self) 0)
  (setf (x-now self) 1)
  (start (dfuncall 20 'continue-play self)))

(defgeneric continue-play (self)
  (:method ((self C-pw-oscilloscope))
    (when (play-flag self)
      (let ((delay (patch-value (first (input-objects self)) self))
            (value (patch-value (second (input-objects self)) self))
            (w-now (w (third (pw-controls self))))
            (h-now (h (third (pw-controls self)))))
        (if (> value h-now)(setq value (1- h-now))) 
        (when (<= value 0)(setq value 1)) 
        (with-focused-view (third (pw-controls self))
          (draw-point (x-now self) (- h-now value)))
        (incf (x-now self))
        (when (>= (x-now self) w-now)
          (setf (x-now self) (mod (x-now self) w-now))
          (with-focused-view self
            (view-draw-contents (third (pw-controls self)))))       
        (incf (clock self) delay)
        (dfuncall delay 'continue-play self)))))

(defmethod stop-play ((self C-pw-oscilloscope))
 (when (play-flag self)
   (setf (play-flag self) ())
   (fill-patch-outrect (out-put self))))      

(defmethod clock-obj  ((self C-pw-oscilloscope)) self)
(defmethod stop-clock  ((self C-pw-oscilloscope)) (stop-play self))

(add-pw-input-type 'tty-rect 'C-ttybox
         (list :view-size (make-point 74 74)
               :dialog-item-text "" :type-list '(no-connection)))

(defunp oscilloscope ((del fix>0 (:value 10)) (value fix>0 (:value 50))
                      (points tty-rect)) no-connection
"Oscilloscope-box is used to display graphically information
that it recieves at the second input (value). This gives y-value of
the point that is drawn inside the display-rectangle.The x-value is
incremented by one pixel each time a new point is drawn. 
To start this box select it and press p (like the midi-box).
When the next point is been drawn is determined by the first 
input del (also like the midi-box).
This box can bee resized by clicking inside the small rectangle
bottom right and dragging the mouse.
"
  (declare (ignore del value points)))
;;====================================================


;; ;;;not needed?????!!  911107
;; (setq *pw-rectangle-tty-type*
;;   (make-instance 'C-pw-type
;;           :control-form
;;            `(make-instance 'C-ttybox  :dialog-item-text ""
;;                  :view-size (make-point 74 74) :type-list '(no-connection))))
