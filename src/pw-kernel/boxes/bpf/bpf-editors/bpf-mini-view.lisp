;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               bpf-mini-view.lisp
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
(enable-patchwork-reader-macros)


#|
(defun scale-to-fit-in-rect (view)
  (let ((x-values (give-x-points (break-point-function view)))
        (y-values (give-y-points (break-point-function view)))
        (x-min)(x-max)(y-min)(y-max))
   (when x-values 
    (setq x-min (car x-values))
    (setq x-max (car (last x-values)))
    (setq y-min (apply #'min y-values))
    (setq y-max (apply #'max y-values))
    (unless (=  x-max x-min)
      (setf (h-view-scaler view) (/ (+ (- x-max x-min)(* (- x-max x-min) .01)) (w view) ))) 
    (unless (=  y-max y-min)
      (setf (v-view-scaler view) (/ (+ (- y-max y-min)(* (- y-max y-min).01)) (h view) )))  
    (set-origin view 
         (make-point (round x-min (h-view-scaler view)) 
                     (- (h view) (round y-max (v-view-scaler view))))))))

;;Safer...
(defun scale-to-fit-in-rect (view)
  (let ((x-values (give-x-points (break-point-function view)))
        (y-values (give-y-points (break-point-function view)))
        (x-min)(x-max)(y-min)(y-max))
   (when x-values 
    (setq x-min (car x-values))
    (setq x-max (car (last x-values)))
    (setq y-min (apply #'min y-values))
    (setq y-max (apply #'max y-values))
    (unless (=  x-max x-min)
      (setf (h-view-scaler view) (/ (+ (- x-max x-min)(* (- x-max x-min) .01)) (w view) ))) 
    (unless (=  y-max y-min)
      (setf (v-view-scaler view) (/ (+ (- y-max y-min)(* (- y-max y-min).01)) (h view) )))
    (setq x-min (round x-min (h-view-scaler view))
          y-min (- (h view) (round y-max (v-view-scaler view))))
    (when (or (> (abs x-min) #.(1- (expt 2 15))) (> (abs y-min) #.(1- (expt 2 15))))
      (ui:ed-beep) 
      (format t "Unable to scale BPF. Please decrease window size (hight) ~%"))
    (set-origin view 
         (make-point (if (minusp x-min) 
                       (max (- #.(1- (expt 2 15))) x-min)
                       (min #.(1- (expt 2 15)) x-min))
                     (if (minusp y-min)
                       (max (- #.(1- (expt 2 15))) y-min)
                       (min #.(1- (expt 2 15)) y-min)))))))
|#


#|
(defun scale-to-fit-in-rect (view)
  (let* ((current-bpf (break-point-function view))
         (bpfs (cons current-bpf (break-point-functions view)))
         (x-values (apply 'append (ask-all bpfs #'give-x-points)))
         (y-values (and x-values (apply 'append (ask-all bpfs #'give-y-points))))
         x-min x-max y-min y-max)
   (when x-values 
    (setq x-min (apply #'min x-values))
    (setq x-max (apply #'max x-values))
    (setq y-min (apply #'min y-values))
    (setq y-max (apply #'max y-values))
    (unless (=  x-max x-min)
      (setf (h-view-scaler view) (/ (+ (- x-max x-min)(* (- x-max x-min) .01)) (w view) ))) 
    (unless (=  y-max y-min)
      (setf (v-view-scaler view) (/ (+ (- y-max y-min)(* (- y-max y-min).01)) (h view) )))
    (setq x-min (round x-min (h-view-scaler view))
          y-min (- (h view) (round y-max (v-view-scaler view))))
    (when (or (> (abs x-min) #.(1- (expt 2 15))) (> (abs y-min) #.(1- (expt 2 15))))
      (ui:ed-beep) 
      (format t "Unable to scale BPF. Please decrease window size (hight) ~%"))
    (set-origin view 
         (make-point (if (minusp x-min) 
                       (max (- #.(1- (expt 2 15))) x-min)
                       (min #.(1- (expt 2 15)) x-min))
                     (if (minusp y-min)
                       (max (- #.(1- (expt 2 15))) y-min)
                       (min #.(1- (expt 2 15)) y-min)))))))
|#


#|
(defun scale-to-fit-in-rect (view)
  (let* ((bpfs 
           (if (eql (type-of view) 'C-mini-bpf-view) 
             (cons (break-point-function view)
               (break-point-functions 
                 (editor-view-object (application-object (view-container view)))))
             (cons (break-point-function view) (break-point-functions view))))
        (x-values (mapcar #'give-x-points bpfs))
        (y-values (mapcar #'give-y-points bpfs))
        (x-min)(x-max)(y-min)(y-max))
   (when x-values 
    (setq x-min (apply #'min (mapcar #'car x-values)))
    (setq x-max ;(car (last x-values)))
          (apply #'max (mapcar (lambda (vs) (car (last vs))) x-values)))
    (setq y-min (apply #'min (mapcar (lambda (vs) (apply #'min vs)) y-values)))
    (setq y-max ;(apply #'max y-values))
       (apply #'max (mapcar (lambda (vs) (apply #'max vs)) y-values)))
    (unless (=  x-max x-min)
      (setf (h-view-scaler view) (/ (+ (- x-max x-min)(* (- x-max x-min) .01)) (w view) ))) 
    (unless (=  y-max y-min)
      (setf (v-view-scaler view) (/ (+ (- y-max y-min)(* (- y-max y-min).01)) (h view) )))
    (setq x-min (round x-min (h-view-scaler view))
          y-min (- (h view) (round y-max (v-view-scaler view))))
    (when (or (> (abs x-min) #.(1- (expt 2 15))) (> (abs y-min) #.(1- (expt 2 15))))
      (ui:ed-beep) 
      (format t "Unable to scale BPF. Please decrease window size (hight) ~%"))
    (set-origin view 
         (make-point (if (minusp x-min) 
                       (max (- #.(1- (expt 2 15))) x-min)
                       (min #.(1- (expt 2 15)) x-min))
                     (if (minusp y-min)
                       (max (- #.(1- (expt 2 15))) y-min)
                       (min #.(1- (expt 2 15)) y-min)))))))
|#

(defun flatt (list)
  (cond ((atom list) list)
        ((listp list)
         (if (listp (first list))
           (append (flatt (first list)) (flatt (rest list)))
           (cons (first list) (flatt (rest list)))))))
        
;; scales all bps not just the active one 
;;changed by aaa 28-08-95 from pw-modif
(defun scale-to-fit-in-rect (view)
  (let* ((current-bpf (break-point-function view))
         (bpfs (cons current-bpf (break-point-functions view)))
         (x-values (flatt (ask-all bpfs #'give-x-points)))
         (y-values (and x-values (flatt (ask-all bpfs #'give-y-points))))
         x-min x-max y-min y-max)
   (when x-values 
    (setq x-min (apply #'min x-values))
    (setq x-max (apply #'max x-values))
    (setq y-min (apply #'min y-values))
    (setq y-max (apply #'max y-values))
    (unless (=  x-max x-min)
      (setf (h-view-scaler view) (/ (+ (- x-max x-min)(* (- x-max x-min) .01)) (w view) ))) 
    (unless (=  y-max y-min)
      (setf (v-view-scaler view) (/ (+ (- y-max y-min)(* (- y-max y-min).01)) (h view) )))
    (setq x-min (round x-min (h-view-scaler view))
          y-min (- (h view) (round y-max (v-view-scaler view))))
    (when (or (> (abs x-min) #.(1- (expt 2 15))) (> (abs y-min) #.(1- (expt 2 15))))
      (ui:ed-beep) 
      (format t "Unable to scale BPF. Please decrease window size (hight) ~%"))
    (set-origin view 
         (make-point (if (minusp x-min) 
                       (max (- #.(1- (expt 2 15))) x-min)
                       (min #.(1- (expt 2 15)) x-min))
                     (if (minusp y-min)
                       (max (- #.(1- (expt 2 15))) y-min)
                       (min #.(1- (expt 2 15)) y-min)))))))


;;==================================================================================================================

(defclass C-mini-bpf-view (C-ttybox)
  ((break-point-function :initform nil :initarg :break-point-function 
     :accessor break-point-function)
   (h-view-scaler :initform 1.0 :initarg :h-view-scaler :accessor h-view-scaler)
   (v-view-scaler :initform 1.0 :initarg :v-view-scaler :accessor v-view-scaler)))

(defmethod decompile ((self C-mini-bpf-view))
  `(make-instance ',(class-name (class-of self)) 
     :view-position ,(view-position self)
     :view-size ,(view-size self)
     :doc-string ,(doc-string self)
     :type-list ',(type-list self)  
     :break-point-function ,(decompile (break-point-function self))))

(defmethod initialize-instance :after ((self C-mini-bpf-view) &key controls)
  (declare (ignore controls))
  (when (break-point-function self)
    (scale-to-fit-in-rect self))) 

(defgeneric set-break-point-function-to-mini (self bpf)
  (:method ((self C-mini-bpf-view) bpf)
    (setf (break-point-function self) bpf)
    (scale-to-fit-in-rect self)))

(defgeneric update-mini-view (self)
  (:method ((self C-mini-bpf-view))
    (with-focused-view self
      (with-pen-state (:pattern *white-pattern*)
        ;; TODO: this is view-bounds
        (fill-rect*  (point-h (view-scroll-position self)) 
                     (point-v (view-scroll-position self)) (w self)(h self)))
      (scale-to-fit-in-rect self)
      (view-draw-contents self))))



;; (setq foo2 (make-instance 'window :window-title "BPF-mini"))
;; (setq bb (make-instance 'C-break-point-function
;;                    :break-point-list (list (make-point 0 20)(make-point 50 0)(make-point 80 80))))
;; (setq bb2 (make-instance 'C-break-point-function
;;      :break-point-list (list (make-point 0 10)(make-point 20 10)(make-point 80 70)(make-point 180 5))))
;; (setq bm (make-instance 'C-mini-bpf-view 
;;                  :break-point-function bb
;;                  :view-container foo2
;;                  :view-position #@(5 5)
;;                  :view-size #@(125 125)))
;; 
;; (setq bm2 (make-instance 'C-mini-bpf-view 
;;                  :break-point-function bb2
;;                  :view-container foo2
;;                  :view-position #@(145 5)
;;                  :view-size #@(80 80)))
;; 
;; (setq bm3 (make-instance 'C-mini-bpf-view 
;;                  :break-point-function bb
;;                  :view-container foo2
;;                  :view-position #@(5 145)
;;                  :view-size #@(60 100)))
;; 
;; ;;(time (repeat 400 (view-draw-contents bm)))  
;; ;;(time (repeat 100 (scale-to-fit-in-rect bm)))
;; ;;(set-break-point-function-to-mini bm bb2)

;;;; THE END ;;;;

