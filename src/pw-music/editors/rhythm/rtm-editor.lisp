;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               rtm-editor.lisp
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


;;========================================================

(defclass C-beat-editor-panel (ui:view) 
  ((measure-line :initform () :initarg :measure-line :accessor measure-line)
   (first-beat-num :initform 0 :initarg :first-beat-num :accessor first-beat-num)
   (edit-mode :initform () :initarg :edit-mode :accessor edit-mode)
   (staff-number :initform 3  :initarg :staff-number :accessor staff-number)
   (C-5-y-pixel :initform 50  :initarg :C-5-y-pixel :accessor C-5-y-pixel)
   (rtm-y-pixel :initform 20  :initarg :rtm-y-pixel :accessor rtm-y-pixel)
   (selection-buttons :initform ()  :accessor selection-buttons)
   (selection-button :initform ()  :accessor selection-button)
   (beat-zoom :initform 1 :initarg :beat-zoom :accessor beat-zoom)))

(defmethod initialize-instance :after ((self C-beat-editor-panel) &key args)
  (declare (ignore args))
  (add-subviews self
                (setf (selection-button self)
                      (make-instance 'C-button-latched 
                                     :dialog-item-text "S"
                                     :view-position (make-point 2 10) :view-size (make-point 10 14))))
  (set-selection-button self t)) 
 

(defmethod decompile ((self C-beat-editor-panel))
  `(make-instance 'C-beat-editor-panel
        :view-position ,(view-position self) :view-size ,(view-size self) 
        :staff-number ,(staff-number self) :C-5-y-pixel ,(C-5-y-pixel self)
        :rtm-y-pixel ,(rtm-y-pixel self)
        :beat-zoom ,(beat-zoom self)
        :measure-line ,(decompile (measure-line self))))

(defmethod get-window-state ((self C-beat-editor-panel) win)
  (declare (ignore win))
  (list (view-position self) (view-size self) (staff-number self) (C-5-y-pixel self) (rtm-y-pixel self) (beat-zoom self)))

(defmethod put-window-state ((self C-beat-editor-panel) win state)
  (declare (ignore win))
  (set-view-position self (first state))
  (set-view-size self (second state))
  (setf (staff-number self) (third state))
  (setf (C-5-y-pixel self) (fourth state))
  (setf (rtm-y-pixel self) (fifth state))
  (setf (beat-zoom self) (sixth state)))

;;========================================================
(defgeneric set-selection-button (self flag)
  (:method ((self C-beat-editor-panel) flag)
    (set-value (selection-button self) flag)
    (with-focused-view self
      (erase+view-draw-contents (selection-button self)))))

(defgeneric give-C5-value (self)
  (:method ((self C-beat-editor-panel))
    (+ (C-5-y-pixel self) (round (h self) 2))))
(defgeneric give-C5-value-staff-count (self staff-count)
  (:method ((self C-beat-editor-panel) staff-count)
    (+ (C-5-y-pixel self) (round (round (h self) staff-count) 2))))

(defgeneric init-rtm-draw (self)
  (:method ((self C-beat-editor-panel))
    (setq *staff-num* (staff-number self))
    (when (edit-mode self) 
      (connect-to-selection-button self (measure-line self) 2 45 10 22 'open-measure-line-editor))))


(defmethod print-draw-contents ((self C-beat-editor-panel))
  (view-draw-contents self))

(defmethod view-draw-contents ((self C-beat-editor-panel))
  (let* ((draw-info (ask-all (rtm-radio-ctrls (view-container self)) 'check-box-checked-p))
         (*mn-view-dyn-flag* (first draw-info))
         (*mn-view-dur-flag* (second draw-info))
         (*mn-view-offset-flag* (third draw-info))
         (*mn-view-ins-flag* (fourth draw-info))
         (monof? (= (length (beat-editors (view-container self))) 1)) 
         (staff-incr 0)(staff-count 1)
         measures (first-time? t))
    (when monof?
      (setq staff-count (visible-staffs-count (view-container self)))
      (setq staff-incr (truncate (/ (- (h self) 0) staff-count))))
    (setf *MN-global-ins-y* (+ 60 (give-C5-value self)))
    ; GA 12/4/94 causes crash ?
    (setf *current-MN-editor* self)
    (add-to-free-buttons-pool *selection-buttons-pool* (selection-buttons self)) 
    (setf (selection-buttons self) nil)
    (with-focused-view self
      (view-draw-contents (selection-button self))
      (draw-string 2 40 (format () "~A" (1+ (position  self (beat-editors (view-container self))))))
      (set-view-font  (view-container (view-container  self)) *music-font-spec*)
      (init-rtm-draw self)
      (for (i 0 1 (1- staff-count))
        (tell (symbol-value (nth (1- (staff-number self)) *global-staff-list*)) 'draw-staff 
            (- (x self) 15) (if monof? (+ (* i staff-incr) (give-C5-value-staff-count  self staff-count)) (give-C5-value self))) 
        (when (or measures first-time?)
          (setq measures
                (draw-measures- (measure-line self) self
                                (if monof? (+ (* i staff-incr) (give-C5-value-staff-count  self staff-count)) (give-C5-value self))
                                (beat-zoom self)(first-beat-num self) 
                                28 (w self) 
                                (+ (* i staff-incr) 37) (+ (* i staff-incr) (- 57 (rtm-y-pixel self))) t measures))
          (setq first-time? ())))
      (tell (selection-buttons self) #'draw-view-contents** (view-container self))
      (set-view-font  (view-container (view-container  self)) *patchwork-font-spec*))
      measures))


(defgeneric add-to-selection-buttons (self butt)
  (:method ((self C-beat-editor-panel) butt)
    (push  butt (selection-buttons self))))

(defgeneric set-edit-mode (self value))
(defmethod set-edit-mode ((self C-beat-editor-panel) value)
  (setf (edit-mode self) value)) 
(defgeneric set-first-beat-num (self value)
  (:method ((self C-beat-editor-panel) value)
    (setf (first-beat-num self) value))) 
(defgeneric set-beat-zoom (self value)
  (:method ((self C-beat-editor-panel) value)
    (setf (beat-zoom self) value))) 

(defparameter *rtm-editor-velocity-list* '(100 100))

(defmethod view-click-event-handler :after ((self C-beat-editor-panel) where)
  (declare (ignore where))
  (if (option-key-p)
    (let* ((win (view-window self))     ;;;; velocity-curve
           (last-mp (view-mouse-position win))
           (values (list (point-v last-mp))))
      (loop
        (unless (mouse-down-p) (return))
        (let ((mp (view-mouse-position win)))
          (unless (eql mp last-mp)
            (setq last-mp mp)
            (push (point-v last-mp) values))))
      (setf *rtm-editor-velocity-list* (nreverse values)))
    (progn
      (let ((inside-sel-butt? (inside-rectangle? (point-h (view-mouse-position self))(point-v (view-mouse-position self))
                                                 (x (selection-button self))(y (selection-button self))(w (selection-button self))(h (selection-button self)))))
        (unless inside-sel-butt? 
          (unless  (eql (current-rtm-editor (view-container self)) self)
             (fill-xor-view-selections (current-rtm-editor (view-container self)))
             (setf (current-rtm-editor (view-container self)) self))))
      (unless (ask (selection-buttons self) #'inside-rtm-selection-buttons- (view-mouse-position self) self)
        (cond 
         ((< (point-h (view-mouse-position self)) 20)
          (let* ((win (view-window self))
                 (first-v (point-v (view-mouse-position win)))
                 (last-mp (view-mouse-position win))
                 (last-value (if (shift-key-p) (rtm-y-pixel self)(C-5-y-pixel self))))
            (loop
              (unless (mouse-down-p) (return))
              (let ((mp (view-mouse-position win)) temp)
                (unless (eql mp last-mp)
                  (setq last-mp mp
                        temp (- last-value (- first-v (point-v last-mp))))
                  (if (shift-key-p) 
                    (setf (rtm-y-pixel self) temp)
                    (setf (C-5-y-pixel self) temp))
                  (erase+view-draw-contents self))))))
         (t   ;plain click
              (fill-xor-view-selections self)))))))
 
;;=======================

(defun fill-xor-view-selections (editor)
 (when editor
  (when (rtm-selection-1 (view-container editor))
    (let ((butt1 (ask (selection-buttons editor) #'selected-rtm-button? (rtm-selection-1 (view-container editor))))
          (butt2 (ask (selection-buttons editor) #'selected-rtm-button? (rtm-selection-2 (view-container editor)))))
      (setf (rtm-selection-1 (view-container editor)) ()) ;;;;
      (setf (rtm-selection-2 (view-container editor)) ()) ;;;;
      (when butt1 (with-focused-view editor (fill-xor-view-contents** butt1)))
      (when butt2 (with-focused-view editor (fill-xor-view-contents** butt2)))))))


(defun get-every-nth-item-of-list (lst count low high)
  (cond ((= count 1) (list low))
        ((or (= (length lst) 0)(= (length lst) 1)) (make-list count :initial-element low))
        ((< (length lst) count)
          (scale-low-high (break-point-fun count (interpol (length lst) 0 100)  lst) high low t)) 
        (t
         (let ((incr (/ (length lst) (1- count))) res)
           (for (i 0 incr (1- (length lst)))(push (nth (truncate i) lst) res))
           (scale-low-high (nreverse (cons (car (last lst)) res)) high low  t))))) ;reversed y

;;(get-every-nth-item-of-list '(12 13 45 75) 3 50 100) 
;;(break-point-fun 5  '(0 100) '(78 67)) 
;;===================================
(defgeneric open-measure-line-editor (self ctrl)
  (:method ((self C-measure-line) ctrl)
    (declare (ignore ctrl))))

(defmethod paste-beat ((self C-measure-line))
  (when (current-rtm-editor (editor-collection-object *active-rtm-window*))
    (paste-beat (current-rtm-editor (editor-collection-object *active-rtm-window*))))) 

(defmethod paste-beat ((self C-beat-editor-panel))
 (when *measure-line-selection-scrap*
   (setf (measures (measure-line self)) (measures (eval *measure-line-selection-scrap*)))))

;;========================================================

(defclass C-beat-editor-collection (ui:view) 
  ((beat-editors :initform () :initarg :beat-editors :accessor beat-editors)
   (first-staff-num :initform 0 :initarg :first-staff-num :accessor first-staff-num)
   (visible-staffs-count :initform 1 :initarg :visible-staffs-count :accessor visible-staffs-count)
   (visible-staffs-count-ctrl :initform () :initarg :visible-staffs-count-ctrl :accessor visible-staffs-count-ctrl)
   (first-staff-num-ctrl :initform () :initarg :first-staff-num-ctrl :accessor first-staff-num-ctrl)
   (beat-zoom-ctrl :initform ()  :accessor beat-zoom-ctrl)
   (beat-zoom-ctrl-value :initform 100  :initarg :beat-zoom-ctrl-value :accessor beat-zoom-ctrl-value)
   (beat-edit-ctrl :initform ()  :accessor beat-edit-ctrl)
   (chord-edit-ctrl :initform ()  :accessor chord-edit-ctrl)
   (play-speed-ctrl :initform ()  :accessor play-speed-ctrl)
   (duration-scale-ctrl :initform ()  :accessor duration-scale-ctrl)
   (beat-number-ctrl :initform ()  :accessor beat-number-ctrl)
   (current-rtm-editor :initform ()  :accessor current-rtm-editor)
   (rtm-selection-1 :initform ()  :accessor rtm-selection-1)
   (rtm-selection-2 :initform ()  :accessor rtm-selection-2)
   (static-text-ctrl :initform ()  :accessor static-text-ctrl)
   (disp-mode :initform ()  :accessor disp-mode)
   (rtm-radio-ctrls :initform nil  :accessor rtm-radio-ctrls)
   (rtm-chord-lines :initform nil  :accessor rtm-chord-lines)
   (analysis-info :initform nil  :accessor analysis-info)))

(defmethod decompile ((self C-beat-editor-collection))
  `(make-instance 'C-beat-editor-collection
        :view-position ,(view-position self) :view-size ,(view-size self)
        :first-staff-num ,(first-staff-num self) :visible-staffs-count ,(visible-staffs-count self)
        :beat-zoom-ctrl-value ,(value (beat-zoom-ctrl self))
        :beat-editors (list ,@(ask-all (beat-editors self) 'decompile))))

(defmethod get-window-state ((self C-beat-editor-collection) win)
  (declare (ignore win))
  (list (view-position self) (view-size self) (first-staff-num self) (visible-staffs-count self) (value (beat-zoom-ctrl self)))) 

(defmethod put-window-state ((self C-beat-editor-collection) win state)
  (declare (ignore win))
  (set-view-position self (first state))
  (set-view-size self (second state))
  (setf (first-staff-num self) (third state))
  (set-numbox-item-text  (first-staff-num-ctrl self) (1+ (third state)))
  (setf (value (first-staff-num-ctrl self)) (1+ (third state)))
  (setf (visible-staffs-count self) (fourth state))
  (set-numbox-item-text  (visible-staffs-count-ctrl self) (fourth state))
  (setf (value (visible-staffs-count-ctrl self)) (fourth state))
  (setf (beat-zoom-ctrl-value self) (fifth state))
  (set-numbox-item-text  (beat-zoom-ctrl self) (fifth state))
  (setf (value (beat-zoom-ctrl self)) (fifth state))
  (resize-new-rtm-w self ())) 

;;(defvar *rtm-playing-option* ())

#|(defun play-rtm-with-options (self)
  (let ((editors (give-selected-editors self)))
    (if (eql *rtm-playing-option* :pb) 
      (progn
        (setf *mn-view-offset-flag* (check-box-checked-p (third (rtm-radio-ctrls self))))
        (start 
          (apdfuncall 100 (priority) 15
                      (lambda ()
                          (tell (ask-all editors 'measure-line)
                                'play-measure-line (get-play-speed self))))))     
      (play-sequence (make-instance 'C-chord-line 
                       :chords (remove nil (epw::flat (rtm-chords (ask-all editors 'measure-line))))) 
                     (cond ((eql *current-approx-scale* *1/4-tone-chromatic-scale*) 4)
                           ((eql *current-approx-scale* *1/8-tone-chromatic-scale*) 8)
                           (t 2))))))|#


;;changed by aaa 28-08-95 from pw-modif
(defun play-rtm-with-options (self)
  (let ((editors (give-selected-editors self)))
    (setf *mn-view-offset-flag* (check-box-checked-p (third (rtm-radio-ctrls self))))
    (if (eql *playing-option* :pb) 
      (progn
        (setf patchwork.scheduler::*print-on-late?* t)
        (start 
          (apdfuncall 100 (priority) 15
                      (lambda ()
                          (tell (ask-all editors 'measure-line)
                                'play-measure-line (get-play-speed self))))))     
      (let ((c-line (make-instance 'C-chord-line 
                       :chords (remove nil (epw::flat (rtm-chords (ask-all editors 'measure-line)))))))
        (if *mn-view-offset-flag*
          (play-your-chords c-line)
          (progn (setf patchwork.scheduler::*print-on-late?* t)
                 (start 
                   (apdfuncall 100 (priority) 15
                      (lambda ()
                          (tell (ask-all editors 'measure-line)
                                'play-measure-line (get-play-speed self)))))   ))))))

#|
(defun play-rtm-with-options (self)
  (let ((editors (give-selected-editors self)))
    (setf *mn-view-offset-flag* (check-box-checked-p (third (rtm-radio-ctrls self))))
    (if (eql *playing-option* :pb) 
      (progn
        (setf patchwork.scheduler::*print-on-late?* t)
        (start 
          (apdfuncall 100 (priority) 15
                      (lambda ()
                          (tell (ask-all editors 'measure-line)
                                'play-measure-line (get-play-speed self))))))     
      (let ((c-line (make-instance 'C-chord-line 
                       :chords (remove nil (epw::flat (rtm-chords (ask-all editors 'measure-line)))))))
        (if *mn-view-offset-flag*
          (play-your-chords c-line)
          (progn (setf patchwork.scheduler::*print-on-late?* t)
                 (start 
                   (apdfuncall 100 (priority) 15
                      (lambda ()
                          (tell (ask-all editors 'measure-line)
                                'play-measure-line (get-play-speed self)))))   ))))))
          ;;(start (apdfuncall 100 (priority) 15 #'play-chords c-line)))))))

|#

(defmethod initialize-instance :after ((self C-beat-editor-collection) &key controls)
 (declare (ignore controls))
 (apply #'add-subviews self (beat-editors self)) 
 ;(setf (visible-staffs-count self) (max 1 (length (beat-editors self))))
 (let ((ctrl-y (- (h self) 25)))
  (for (i 0 1 (1- (length (beat-editors self))))
    (set-view-size (nth i (beat-editors self)) (make-point (- (w self) 2) (h (nth i (beat-editors self))))))
  (push (add-rtm-editor-radio-cluster self 410 (- (h self) 10) "dyn") (rtm-radio-ctrls self))
  (push (add-rtm-editor-radio-cluster self 460 (- (h self) 10) "dur") (rtm-radio-ctrls self))
  (push (add-rtm-editor-radio-cluster self 510 (- (h self) 10) "offs") (rtm-radio-ctrls self))
  (push (add-rtm-editor-radio-cluster self 560 (- (h self) 10) "ins") (rtm-radio-ctrls self))
  (setf (rtm-radio-ctrls self) (nreverse (rtm-radio-ctrls self)))
  (add-subviews self 
                (setf (static-text-ctrl self)
                    (make-instance 'static-text-dialog-item 
                        :view-position (make-point 0 (- (h self) 10)) 
                               :view-font *patchwork-font-spec*
                               :dialog-item-text "measure staff stcnt scale   speed")) 
                (setf (beat-number-ctrl self)
                      (make-instance 'C-numbox 
                                     :view-position (make-point 2 ctrl-y) :view-size (make-point 36 14) 
                                     :min-val 1  :value 1 
                                     :dialog-item-action 
                                     (lambda (item) (scroll-beat self item))))
                (setf (first-staff-num-ctrl self)
                  (make-instance 'C-numbox 
                                     :view-position (make-point 40 ctrl-y) :view-size (make-point 36 14) 
                                     :min-val 1  :value (1+ (first-staff-num self)) 
                                     :dialog-item-action 
                                     (lambda (item) (scroll-to-staff-number self item))))
                (setf (visible-staffs-count-ctrl self)
                   (make-instance 'C-numbox 
                                     :view-position (make-point 78 ctrl-y) :view-size (make-point 36 14) 
                                     :min-val 1  :value (visible-staffs-count self)
                                     :dialog-item-action 
                                     (lambda (item) (update-staff-count-number self item))))
                (setf (beat-zoom-ctrl self)
                      (make-instance 'C-numbox 
                                     :view-position (make-point 116 ctrl-y) :view-size (make-point 36 14) 
                                     :min-val 10 :min-val 1000  :value (beat-zoom-ctrl-value self)
                                     :dialog-item-action 
                                     (lambda (item) (zoom-beat self item))))
                (setf (play-speed-ctrl self)
                      (make-instance 'C-numbox 
                                     :view-position (make-point 167 ctrl-y) :view-size (make-point 36 14) 
                                     :min-val 10 :max-val 1000  :value 100))
                (make-instance 'button-dialog-item 
                               :dialog-item-text " Play "
                               :view-position (make-point 215 ctrl-y) :view-size (make-point 50 14) 
                               :view-font *patchwork-font-spec*
                               :dialog-item-action 
                               (lambda (item) item
                                  (play-rtm-with-options self)))
                (make-instance 'button-dialog-item 
                               :dialog-item-text " SPlay "
                               :view-position (make-point 275 ctrl-y) :view-size (make-point 50 14) 
                               :view-font *patchwork-font-spec*
                               :dialog-item-action 
                               (lambda (item) item (play-rtms+scroll self)))
                (make-instance 'button-dialog-item 
                               :dialog-item-text " Stop "
                               :view-position (make-point 335 ctrl-y) :view-size (make-point 50 14) 
                               :view-font *patchwork-font-spec*
                               :dialog-item-action 
                               (lambda (item) item 
                                  (tell (ask-all (beat-editors self) 'measure-line) 'stop-measure-line)))
               (setf (beat-edit-ctrl self)
                      (make-instance 'check-box-dialog-item 
                                     :dialog-item-text "edit"
                                     :view-position (make-point 410 ctrl-y) :view-size (make-point 50 14) 
                                     :view-font *patchwork-font-spec*
                                     :dialog-item-action 
                                     (lambda (item) (set-edit-mode self item))))
               (setf (chord-edit-ctrl self)
                      (make-instance 'check-box-dialog-item 
                                     :dialog-item-text "chord"
                                     :view-font *patchwork-font-spec*
                                     :view-position (make-point 470 ctrl-y) :view-size (make-point 50 14))))))

;;================

(defgeneric add-rtm-editor-radio-cluster (self x y txt)
  (:method ((self C-beat-editor-collection) x y txt)
    (make-instance 
     'check-box-dialog-item
     :view-position (make-point x y)
     :view-container self
     :dialog-item-text txt
     :view-font *patchwork-font-spec*
     :dialog-item-action (lambda (item) item (erase+view-draw-contents (view-window self)))))) 

;;================

(defgeneric get-play-speed (self)
  (:method ((self C-beat-editor-collection))
    (float (/ (value (play-speed-ctrl self)) 100))))
(defgeneric give-selected-editors (self)
  (:method ((self C-beat-editor-collection)) 
    (let ((editors (beat-editors self))
          res)
      (while editors 
        (when (value (selection-button (car editors))) (push (car editors) res))
        (pop editors))
      (nreverse res)))) 

;;================================
(defgeneric update-staff-count-number (self item)
  (:method ((self C-beat-editor-collection) item)
    (setf (visible-staffs-count self) (max  1 (value item)))
    (resize-new-rtm-w self ())
    (erase+view-draw-contents (view-window self))))

(defgeneric scroll-to-staff-number (self item)
  (:method ((self C-beat-editor-collection) item)
    (setf (first-staff-num self) (min (1- (length (beat-editors self))) (1- (value item))))
    (resize-new-rtm-w self ())))
#|
(defmethod scroll-beat ((self C-beat-editor-collection) ctrl)
  (tell (beat-editors self) 'set-first-beat-num (1-  (value ctrl)))
  (with-focused-view self (erase+view-draw-contents self))) 
|#
(defgeneric scroll-beat (self ctrl)
  (:method ((self C-beat-editor-collection) ctrl)
    (when (numberp ctrl) (set-dialog-item-text-from-dialog (beat-number-ctrl self) (format nil "~5D" (1+ ctrl))))
    (tell (beat-editors self) 'set-first-beat-num (if (numberp ctrl) ctrl (1-  (value ctrl))))
    (with-focused-view self (erase+view-draw-contents self))))


(defgeneric zoom-beat (self ctrl)
  (:method ((self C-beat-editor-collection) ctrl)
    (tell (beat-editors self) 'set-beat-zoom (max .1 (/ (value ctrl) 100)))
    (with-focused-view self (erase+view-draw-contents self))))

;;================================

(defmethod set-edit-mode ((self C-beat-editor-collection) ctrl)
  (tell (beat-editors self) 'set-edit-mode (check-box-checked-p ctrl))
  (tell (draw-beat-editor-objects self) 'erase+view-draw-contents)) 


(defgeneric draw-control-extra-stuff (self)
  (:method ((self C-beat-editor-collection))
    (draw-rect* 0 0 (- (w self) 0) (- (h self) 25))))

(defgeneric draw-beat-editor-objects (self)
  (:method ((self C-beat-editor-collection)) 
    ;;(beat-editors self))
    (let ((editors (nthcdr (first-staff-num self) (beat-editors self))))
      (firstn  (visible-staffs-count self) editors))))


(defmethod view-draw-contents ((self C-beat-editor-collection))  ;(call-next-method))
  (let ((beat-editors (draw-beat-editor-objects self)))
    ; GA 12/4/94 causes crash ?
    ;(setf *current-MN-window* (view-container self)) ???
    (with-focused-view self
      (tell (set-difference (subviews self) (beat-editors self)) 'view-draw-contents)
      (while beat-editors
         (when (< (+ (y (car beat-editors)) (h (car beat-editors))) (h self))
              (erase+view-draw-contents (car beat-editors)))
         (pop beat-editors)))))

(defmethod view-draw-contents :after ((self C-beat-editor-collection))
  (draw-control-extra-stuff self))
 
;;(defmethod view-draw-contents :before ((self C-beat-editor-collection)))
;;  (init-beat-selection-buttons-pool *selection-buttons-pool*))

;;=================================
;;layout

;;(defmethod resize-new-rtm-w ((self C-beat-editor-panel) w)
;;  (set-view-size self (- w 30) (h self)))

(defgeneric set-view-y (self y)
  (:method ((self simple-view) y)
    (set-view-position self (x self) y)))

(defgeneric resize-new-rtm-w (self size)
  (:method ((self C-beat-editor-collection) size)
    (if size 
        (set-view-size self (subtract-points size (make-point 20 5)))
        (setq size (view-size self)))
    (let ((v-incr (round (/ (- (h self) 50) (length (draw-beat-editor-objects self)))))
          (rest-editors (set-difference (beat-editors self) (draw-beat-editor-objects self))))
      (tell rest-editors #'set-view-position -10000 -10000) 
      (for (i 0 1 (1- (length (draw-beat-editor-objects self))))
        (set-view-position (nth i  (draw-beat-editor-objects self)) (make-point 10 (* v-incr i))) 
        (set-view-size (nth i  (draw-beat-editor-objects self)) (make-point (- (point-h size) 30) v-incr))))
    (tell (set-difference 
           (set-difference (subviews self)(beat-editors self)) 
           (cons (static-text-ctrl self)(rtm-radio-ctrls self))) #'set-view-y (- (h self) 25))
    (tell (cons (static-text-ctrl self)(rtm-radio-ctrls self)) #'set-view-y (- (h self) 11))))

;;============================

(defgeneric give-rtm-range-chords (self chord-fl)
  (:method ((self C-beat-editor-collection) chord-fl)    
    (when (and (rtm-selection-1 self) (current-rtm-editor self))
      (setf *beat-leaf-objs* ())
      (let (pos1 pos2 measures beats res beats1 beats2)
        (cond ((eql 'C-measure-line (class-name (class-of (rtm-selection-1 self)))) ; measure-line
               (setq res (collect-all-chord-beat-leafs (rtm-selection-1 self))))
              ((eql 'C-measure (class-name (class-of (rtm-selection-1 self)))) ; measure
               (setq measures (measures (measure-line (current-rtm-editor self))))
               (setq pos1 (position (rtm-selection-1 self) measures)) 
               (setq pos2 (position (rtm-selection-2 self) measures))
               (unless  pos2 (setq pos2 pos1))
               (tell (subseq measures (min pos1 pos2)(1+ (max pos1 pos2))) #'collect-all-chord-beat-leafs)
               (setq res *beat-leaf-objs*))
              ((and (not (beat-chord (rtm-selection-1 self))) ; beat
                    (eql 'C-beat (class-name (class-of (rtm-selection-1 self))))
                    (setq beats (collect-all-chord-beat-leafs (measure-line (current-rtm-editor self)))))
               (setf *beat-leaf-objs* ())
               (setq beats1 (collect-all-chord-beat-leafs (rtm-selection-1 self)))
               (setf *beat-leaf-objs* ())
               (when (rtm-selection-2 self)
                 (setq beats2 (collect-all-chord-beat-leafs (rtm-selection-2 self))))
               (setq pos1 (position (car beats1) beats)) 
               (setq pos2 (position (car beats2) beats))
               (if (not pos2) 
                   (setq pos2 (position (car (last beats1)) beats))
                   (if (< pos1 pos2)
                       (setq pos2 (position (car (last beats2)) beats))
                       (setq pos1 (position (car (last beats1)) beats))))
               (tell (subseq beats (min pos1 pos2)(1+ (max pos1 pos2))) #'collect-all-chord-beat-leafs)
               (setq res *beat-leaf-objs*))
              (t                        ; beat-chords
               (setq beats (collect-all-chord-beat-leafs (measure-line (current-rtm-editor self))))
               (setq pos1 (position (rtm-selection-1 self) beats)) 
               (setq pos2 (position (rtm-selection-2 self) beats))
               (unless  pos2 (setq pos2 pos1))
               (setq res (subseq beats (min pos1 pos2)(1+ (max pos1 pos2))))))
        (if chord-fl (ask-all res #'beat-chord) res)))))
#|
(defmethod give-rtm-range-chords ((self C-beat-editor-collection) chord-fl)    
  (when (and (rtm-selection-1 self) (current-rtm-editor self))
    (setf *beat-leaf-objs* ())
    (let (pos1 pos2 measures beats res)
      (cond ((eql 'C-measure-line (class-name (class-of (rtm-selection-1 self))))
             (setq res (collect-all-chord-beat-leafs (rtm-selection-1 self))))
            ((eql 'C-measure (class-name (class-of (rtm-selection-1 self))))
             (setq measures (measures (measure-line (current-rtm-editor self))))
             (setq pos1 (position (rtm-selection-1 self) measures)) 
             (setq pos2 (position (rtm-selection-2 self) measures))
             (unless  pos2 (setq pos2 pos1))
             (tell (subseq measures (min pos1 pos2)(1+ (max pos1 pos2))) #'collect-all-chord-beat-leafs)
             (setq res *beat-leaf-objs*))
            ((and (not (beat-chord (rtm-selection-1 self)))
                  (eql 'C-beat (class-name (class-of (rtm-selection-1 self))))
             (setq beats (apply #'append (ask-all (measures (measure-line (current-rtm-editor self))) #'beat-objects))))
             (setq pos1 (position (rtm-selection-1 self) beats)) 
             (setq pos2 (position (rtm-selection-2 self) beats))
             (unless  pos2 (setq pos2 pos1))
             (tell (subseq beats (min pos1 pos2)(1+ (max pos1 pos2))) #'collect-all-chord-beat-leafs)
             (setq res *beat-leaf-objs*))
            (t ; beat-chords
             (setq beats (collect-all-chord-beat-leafs (measure-line (current-rtm-editor self))))
             (setq pos1 (position (rtm-selection-1 self) beats)) 
             (setq pos2 (position (rtm-selection-2 self) beats))
             (unless  pos2 (setq pos2 pos1))
             (setq res (subseq beats (min pos1 pos2)(1+ (max pos1 pos2))))))
             (if chord-fl (ask-all res #'beat-chord) res))))
|#
