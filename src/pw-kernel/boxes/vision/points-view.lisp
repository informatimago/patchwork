;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               points-view.lisp
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
(in-package :PW)

;;================================================

(defclass C-points-collection  ()
  ((x-points :initform () :initarg :x-points :accessor x-points)
   (y-points :initform () :initarg :y-points :accessor y-points)
   (scaled-x-points :initform ())
   (scaled-y-points :initform ())))

(defmethod scale-x&y-points ((self C-points-collection) points w h)
  (setf (x-points self) (first points))
  (setf (y-points self) (second points))
  (setf (slot-value self 'scaled-x-points)
      (mapcar #'round (scale-low-high (x-points self) 0 w)))
  (setf (slot-value self 'scaled-y-points)
      (mapcar #'round (scale-low-high (y-points self) 0 h))))

;;================================================

(defclass C-points-rect (C-ttybox C-points-collection) ())

;;???
;;(defmethod resize-control :after ((self C-points-rect) w h)
;;  (when (x-points self) (set-x&y self (list (x-points self)(y-points self)))))

(defmethod view-draw-contents ((self C-points-rect))
  (with-focused-view  self
    (draw-rect* 0 0 (w self)(h self))
    (let ((x-points (slot-value self 'scaled-x-points))
          (y-points (slot-value self 'scaled-y-points))
          (h (h self)))
       (while x-points (draw-point (pop x-points) (- h (pop y-points)))))))

(defmethod set-points-list-to-rect ((self C-points-rect) points sort-mode)
  (when (string= sort-mode "sort")
    (setq points 
      (sort (mapcar #'list (first points)(second points)) #'< :key (lambda (a)(car a))))
    (setq points (list (mapcar #'first points)(mapcar #'second points))))
  (scale-x&y-points self points (w self)(h self))) 

;;=====================================

;; (setq *pw-points-rect-type*
;;   (make-instance 'C-pw-type
;;           :control-form
;;            `(make-instance 'C-points-rect  :view-size (make-point 74 74) :type-list '(no-connection))))
;; 
;; 
;; (setq *points-view-sort-pw-type*
;;   (make-instance 'C-pw-type :control-form 
;;    `(make-instance 'C-menubox  :view-size (make-point 36 14)
;;     :menu-box-list '("sort" "nosort")
;;     :type-list '(no-connection))))


;;===================================
;; save points ??
(defclass C-pw-points-view (C-pw-oscilloscope) ()) 


(defmethod patch-value ((self C-pw-points-view) obj)
  (when (nth-connected-p self 0)
     (set-points-list-to-rect (third (input-objects self)) 
          (patch-value (car (input-objects self)) obj)
          (patch-value (second (input-objects self)) ()))
     (erase+view-draw-contents (third (input-objects self)) ))
  (third (input-objects self)))

(add-pw-input-type 'pw-points 'C-points-rect
                   (list :view-size (make-point 74 74)
                          :type-list '(no-connection) :value ""))

(add-pw-input-type 'points-sort 'C-menubox
       (list :view-size (make-point 36 14) :menu-box-list '("sort" "nosort")
             :type-list '(no-connection)))

(add-output-type 'points-view '(points-view))

(defunp points-view ((plist list) (output points-sort) (points pw-points)) points-view
"Points-view accepts in its first input
a list of x-values and y-values.The second input has two
menu options sort and nosort.If sort option is on the
points are sorted in ascending x-values,if nosort is on 
the points are not sorted.Points from points-view can be accesssd
with md-get box by generic functions x-points and y-points."
  (declare (ignore plist output points)))


