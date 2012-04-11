;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;=========================================================
;;;;
;;;;  PATCH-WORK
;;;;  By Mikael Laurson, Jacques Duthen, Camilo Rueda.
;;;;  © 1986-1992 IRCAM 
;;;;
;;;;=========================================================

;;=======================================
;;A text box for PW Windows
;;=======================================

(defpackage "C-PW-TEXT-INPUT"
  (:use "COMMON-LISP" "CCL")
  (:import-from "PATCH-WORK" "C-TTYBOX")
  (:export "C-PW-TEXT-INPUT")
  )

(in-package "C-PW-TEXT-INPUT")

(defclass C-pw-text-input (C-ttybox) ())

(defpackage "C-PW-TEXT-BOX"
  (:use "COMMON-LISP" "CCL")
  (:import-from "PATCH-WORK" "C-PW-RESIZE-X" "H" "PW-CONTROLS" "OUT-PUT" "W"
                "DRAW-FUNCTION-NAME" "RESIZE-PATCH-BOX" "C-PW-FUNCTIONAL")
  (:export "C-PW-TEXT-BOX")
  )

(in-package "C-PW-TEXT-BOX")

(defclass C-pw-text-box (C-pw-resize-x) ())

(defmethod initialize-instance :after ((self C-pw-text-box) &key ctrl)
  (declare (ignore ctrl))
  (let ((box (car (pw-controls self))))
    (set-view-size box (point-h (view-size box)) (- (h self) 5))   
    (set-view-position (out-put self) (w self) (+ (h self) 5))))

(defmethod resize-patch-box ((self C-pw-text-box) mp delta)
 (let ((point-now (make-point (add-points mp delta)))
       (min-w 46) (min-h 28))
   (when (and (< min-w (point-h point-now))(< min-h (point-v point-now))) 
      (set-view-size self point-now)
      (set-view-size (car (pw-controls self)) 
          (make-point (- (point-h point-now) 10)(- (point-v point-now) 5)))
      (set-view-position (out-put self) (w self) (+ (h self) 5))
      )))

(defmethod draw-function-name ((self C-pw-text-box) x y)
  (declare (ignore x y)))

(defmethod view-draw-contents ((self C-pw-text-box))
  (pw::with-pen-state (:pattern *white-pattern*) 
    (pw::fill-rect* 1 1 (- (w self) 2) (- (h self) 2)))
    (with-font-focused-view self '("Chicago" 9 :SRCOR :PLAIN) 
                            (pw::tell (subviews self) 'view-draw-contents)
                            (when (pw::active-mode self) 
                              (pw::with-pen-state (:pattern *black-pattern*) (pw::fill-rect* 0 0 (w self) 3)))
                            (pw::draw-patch-extra self)
                            ))

(defmethod pw::view-frame-patch ((self C-pw-text-box) current-pos delta prev next)
  (pw::with-pen-state (:pattern *gray-pattern* :mode :patXor)
    (with-focused-view (view-container self)
      (let* ((control (first (pw::pw-controls self)))
             (position (add-points (view-position self) (view-position control)))
             (diff-prev (add-points (subtract-points position current-pos)
                              (subtract-points prev delta)))
            (diff-next (add-points (subtract-points position current-pos)
                              (subtract-points next delta))))
      (pw::draw-rect (point-h diff-prev) (point-v diff-prev) (w control) (h control))
      (unless (= prev next)
        (pw::draw-rect (point-h diff-next) (point-v diff-next) (w control) (h control)))))))



(in-package :pw)

(defunp c-pw-text-box::text ((win string 
                                  (:type-list (no-connection) :dialog-item-text " ")))
        nil
        "a window comment can go inside this box"
  win)

(defvar *Text-input-pw-type*  ;;(setf *Text-input-pw-type*
  (make-instance 'C-pw-type
  :control-form
   `(make-instance 'C-pw-text-input:C-pw-text-input
                   :view-size #@(36 8) :dialog-item-text "" 
                   :type-list '(no-connection))))

(defun get-window-text-box () 
  (add-patch-box *active-patch-window* 
         (make-PW-standard-box 'C-pw-text-box:C-pw-text-box 'C-pw-text-box::text)))

#|

(add-patch-box *active-patch-window* 
               (make-patch-box  'C-pw-text-box:C-pw-text-box 'C-pw-text-box::text  
                               '(*Text-input-pw-type* "text") '(no-connection)))
(unintern 'controls)
|#
