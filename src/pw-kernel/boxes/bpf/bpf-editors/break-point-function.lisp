;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               break-point-function.lisp
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

;;============================================================================

(defun scale-point-in-view (point h-view-scaler v-view-scaler)
  (make-point (round (/ (point-h point) h-view-scaler))
              (round (/ (point-v point) v-view-scaler))))


;;==================================================================================================================
;;==================================================================================================================

(defclass C-break-point-function ()
  ((break-point-list :initform nil :initarg :break-point-list :accessor break-point-list)
   (x-points :initform nil :accessor x-points)
   (y-points :initform nil :accessor y-points)))

(defmethod initialize-instance :after ((self C-break-point-function) &rest points)
  (declare (ignore points))
  (setf (x-points self) (mapcar #'point-h (break-point-list self)))
  (setf (y-points self) (mapcar #'point-v (break-point-list self))))

(defmethod decompile ((self C-break-point-function))
  `(make-instance 'C-break-point-function :break-point-list ',(copy-list (break-point-list self))))

;;================== 

(defgeneric set-break-point-function (self points)
  (:method ((self C-break-point-function) points)
    (setf (break-point-list self) points)
    (setf (x-points self) (mapcar #'point-h points))
    (setf (y-points self) (mapcar #'point-v points))))

(defgeneric give-x-points (self)
  (:method ((self C-break-point-function))
    (x-points self)))
(defgeneric give-y-points (self)
  (:method ((self C-break-point-function))
    (y-points self)))

;;================== 

#|(defun draw-bpf-function-points (x-points y-points h-view-scaler v-view-scaler draw-rects-fl y)
   (let (point-x-now point-y-now)
    (when x-points
      (setq point-x-now (round (car x-points) h-view-scaler) 
            point-y-now (round (car y-points) v-view-scaler)) 
      (#_MoveTo :long (make-point point-x-now (- y point-y-now)))
      (when draw-rects-fl (draw-rect* (- point-x-now 1) (- (- y point-y-now) 1) 3 3)) 
      (pop x-points)(pop y-points)
      (while x-points
         (setq point-x-now (round (car x-points) h-view-scaler) 
               point-y-now (round (car y-points) v-view-scaler)) 
         (#_LineTo :long (make-point (make-point point-x-now (- y point-y-now))))
         (when draw-rects-fl (draw-rect* (- point-x-now 1) (- (- y point-y-now) 1) 3 3)) 
         (pop x-points)(pop y-points)))))|#

(defvar *no-line-segments* ())
;;(setf *no-line-segments* ())

#|(defun draw-bpf-function-points (x-points y-points h-view-scaler v-view-scaler draw-rects-fl y)
   (let (point-x-now point-y-now)
    (when x-points
      (setq point-x-now (round (car x-points) h-view-scaler) 
            point-y-now (round (car y-points) v-view-scaler)) 
      (unless *no-line-segments* 
        (#_MoveTo :long (make-point point-x-now (- y point-y-now))))
      (if draw-rects-fl
        (draw-rect* (- point-x-now 1) (- (- y point-y-now) 1) 3 3)
        (if *no-line-segments*
          (draw-rect* (- point-x-now 1) (- (- y point-y-now) 1) 1 1)))
      (pop x-points)(pop y-points)
      (while x-points
         (setq point-x-now (round (car x-points) h-view-scaler) 
               point-y-now (round (car y-points) v-view-scaler))
         (unless *no-line-segments*
           (#_LineTo :long (make-point (make-point point-x-now (- y point-y-now)))))
         (if draw-rects-fl
           (draw-rect* (- point-x-now 1) (- (- y point-y-now) 1) 3 3)
           (if *no-line-segments*
             (draw-rect* (- point-x-now 1) (- (- y point-y-now) 1) 1 1)))
         (pop x-points)(pop y-points)))))|#

#|(defun draw-bpf-function-points (x-points y-points h-view-scaler v-view-scaler draw-rects-fl y)
   (let (point-x-now point-y-now)
    (when x-points
      (setq point-x-now (round (car x-points) h-view-scaler) 
            point-y-now (round (car y-points) v-view-scaler)) 
      (unless *no-line-segments* 
        (#_MoveTo :long (make-point point-x-now (- y point-y-now))))
      (if draw-rects-fl
        (draw-rect* (- point-x-now 1) (- (- y point-y-now) 1) 1 1)
        (if *no-line-segments*
          (draw-rect* (- point-x-now 1) (- (- y point-y-now) 1) 1 1)))
      (pop x-points)(pop y-points)
      (while x-points
         (setq point-x-now (round (car x-points) h-view-scaler) 
               point-y-now (round (car y-points) v-view-scaler))
         (unless *no-line-segments*
           (#_LineTo :long (make-point (make-point point-x-now (- y point-y-now)))))
         (if draw-rects-fl
           (draw-rect* (- point-x-now 1) (- (- y point-y-now) 1) 1 1)
           (if *no-line-segments*
             (draw-rect* (- point-x-now 1) (- (- y point-y-now) 1) 1 1)))
         (pop x-points)(pop y-points)))))|#

(defun draw-bpf-function-points (x-points y-points h-view-scaler v-view-scaler draw-rects-fl y)
  (niy draw-bpf-function-points x-points y-points h-view-scaler v-view-scaler draw-rects-fl y)
   ;; (let (point-x-now point-y-now)
   ;;  (when x-points
   ;;    (setq point-x-now (min #.(1- (expt 2 15)) (round (car x-points) h-view-scaler)) 
   ;;          point-y-now (min #.(1- (expt 2 15)) (round (car y-points) v-view-scaler)))
   ;;    (unless *no-line-segments* 
   ;;      (#_MoveTo :long (make-point point-x-now (- y point-y-now))))
   ;;    (if draw-rects-fl
   ;;      (draw-rect* (- point-x-now 1) (- (- y point-y-now) 1) 1 1)
   ;;      (if *no-line-segments*
   ;;        (draw-rect* (- point-x-now 1) (- (- y point-y-now) 1) 1 1)))
   ;;    (pop x-points)(pop y-points)
   ;;    (while x-points
   ;;       (setq point-x-now (min #.(1- (expt 2 15)) (round (car x-points) h-view-scaler))
   ;;             point-y-now (min #.(1- (expt 2 15)) (round (car y-points) v-view-scaler)))
   ;;       (unless *no-line-segments*
   ;;         (#_LineTo :long (make-point (make-point point-x-now (- y point-y-now)))))
   ;;       (if draw-rects-fl
   ;;         (draw-rect* (- point-x-now 1) (- (- y point-y-now) 1) 1 1)
   ;;         (if *no-line-segments*
   ;;           (draw-rect* (- point-x-now 1) (- (- y point-y-now) 1) 1 1)))
   ;;       (pop x-points)(pop y-points))))
   )

(defgeneric draw-bpf-function (self view draw-rects-fl h-view-scaler v-view-scaler)
  (:method ((self C-break-point-function) view draw-rects-fl h-view-scaler v-view-scaler)
    (let ((x-points (x-points self))
          (y-points (y-points self))
          (y (point-v (view-size view))))
      (draw-bpf-function-points x-points y-points h-view-scaler v-view-scaler draw-rects-fl y))))

(defgeneric draw-bpf-function-from-point-1-to-2 (self view draw-rects-fl h-view-scaler v-view-scaler x-point-1 x-point-2)
  (:method ((self C-break-point-function) view draw-rects-fl h-view-scaler v-view-scaler
            x-point-1 x-point-2)
    (let* ((pos1 (position x-point-1 (x-points self) :test #'eql))
           (pos2 (position x-point-2 (x-points self) :test #'eql))
           (x-points (subseq (x-points self) 
                             (if pos1 pos1 0) (if pos2 (1+ pos2) (length (x-points self)))))
           (y-points (subseq (y-points self) 
                             (if pos1 pos1 0) (if pos2 (1+ pos2) (length (y-points self)))))
           (y (point-v (view-size view))))
      (with-pen-state (:mode :patxor) 
        (draw-bpf-function-points x-points y-points h-view-scaler v-view-scaler draw-rects-fl y)))))

;;==================

;;(defmethod insert-point-by-h ((self C-break-point-function) new-point)
;;  (set-break-point-function self 
;;     (sort (cons new-point (break-point-list self))  #'< :key (lambda (a) (point-h a)))))

(defgeneric insert-point-by-h (self new-bp)
  (:method ((self C-break-point-function) new-bp)
    (let ((bps (break-point-list self))
          (x1 (point-h new-bp))
          (bps1))
      (while (and bps (>= x1 (point-h (car bps)))) (push (pop bps) bps1))
      (set-break-point-function self 
                                (append  (nreverse bps1)(list new-bp) bps)))))

(defgeneric remove-point-from-bpf (self point)
  (:method ((self C-break-point-function) point)
    (set-break-point-function self (remove point (break-point-list self) :test #'eql))))      

(defgeneric kill-all-except-first (self)
  (:method ((self C-break-point-function))
    (set-break-point-function self (list (car (break-point-list self))))))    

;;point has to be included in (break-point-list self)

(defgeneric give-prev+next-x (self point)
  (:method ((self C-break-point-function) point)
    (let ((count (position  point (break-point-list self) :test #'eql))
          x1 x2)
      (when count
        (if (= count 0)
            (setq x2 (second (break-point-list self)))
            (setq x1 (nth (1- count) (break-point-list self))
                  x2 (nth (1+ count) (break-point-list self)))))
      (setq x1 (if x1 (point-h x1) -32000))
      (setq x2 (if x2 (point-h x2) 32000))
      (list x1 x2)))) 

(defgeneric give-points-in-time-range (self x1 x2)
  (:method ((self C-break-point-function) x1 x2)
    (let ((points (break-point-list self))
          res-points)
      (while (and points (> x1 (point-h (car points))))(pop points))
      (while (and points (> x2 (point-h (car points))))
        (push (pop points) res-points))
      (nreverse res-points))))

(defgeneric add-constant-x-after-x-val (self xval xincr)
  (:method ((self C-break-point-function) xval xincr)
    (let ((points (break-point-list self))
          res-points)
      (while points 
        (if (>= (point-h (car points)) xval)
            (push (make-point (+ (point-h (car points)) xincr) (point-v (car points)))
                  res-points)
            (push (car points) res-points))
        (pop points))
      (set-break-point-function self (nreverse res-points)))))

(defgeneric set-new-xs-in-range (self x1 x2 xs)
  (:method ((self C-break-point-function) x1 x2 xs)
    (let ((points (break-point-list self))
          res-points)
      (while (and xs points (> x1 (point-h (car points)))) (push (pop points) res-points))
      (while (and xs points (> x2 (point-h (car points))))
        (push (make-point (pop xs) (point-v (car points)))
              res-points)
        (pop points))
      (set-break-point-function self (append (nreverse res-points) points)))))

(defgeneric set-new-ys-in-range (self x1 x2 ys)
  (:method ((self C-break-point-function) x1 x2 ys)
    (let ((points (break-point-list self))
          res-points)
      (while (and points (> x1 (point-h (car points)))) (push (pop points) res-points))
      (while (and points (> x2 (point-h (car points))))
        (push (make-point (point-h (car points)) (pop ys))
              res-points)
        (pop points))
      (set-break-point-function self (append (nreverse res-points) points)))))

;;==================================================================================================================

;;(setq bp (make-instance 'C-break-point-function 
;;   :break-point-list (list (make-point 0 0)(make-point 10 0)(make-point 20 50)(make-point 90 10))))
;;(time (repeat 1000 
;;  (make-instance 'C-break-point-function 
;;   :break-point-list (list (make-point 0 0)(make-point 10 0)
;;                            (make-point 20 50)(make-point 90 10)))))
;;(time (repeat 1000 (eval (decompile bp))))
;;(time (repeat 100000 (give-y-points bp)))

;;(give-prev+next-x bp (make-point 20 50))

