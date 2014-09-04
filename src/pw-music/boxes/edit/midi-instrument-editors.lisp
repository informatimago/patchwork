;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               midi-instrument-editors.lisp
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



;;==============================================================================
;;  Midi instrument
;;==============================================================================

;;==============================================================================
(defclass C-ttybox-instrument (C-ttybox) ()) 

(defmethod set-dialog-item-text-from-dialog ((self C-ttybox-instrument) text)
  (set-dialog-item-text self text)
  (when (dialog-item-action-function self)
     (funcall (dialog-item-action-function  self) self)))

;;==============================================================================
;; data type editors 
;;==============================================================================
;; fix

(defclass C-fix-window (window)
  ((pw-win :initform nil :accessor pw-win)
   (status-ctrl      :initform nil  :accessor status-ctrl)
   (label-ctrl      :initform nil  :accessor label-ctrl)
   (controller-ctrl  :initform nil  :accessor controller-ctrl)
   (value-ctrl  :initform nil  :accessor value-ctrl)
   (note-play-ctrl :initform nil  :accessor note-play-ctrl)
   (midi-ins-object :initform nil :accessor midi-ins-object)))

(defmethod initialize-instance :after ((self C-fix-window) &key controls)
  (declare (ignore controls))
  (add-subviews self 
     (make-instance 'static-text-dialog-item
         :view-position (make-point 5 3) 
         :view-font '("Monaco" 9 :SRCOR :PLAIN)
         :dialog-item-text "status  contr  value   label  ")
     (setf (status-ctrl self) 
        (make-instance 'C-menubox-val
          :view-position (make-point 2 17)  
          :view-size (make-point 48 16)
          :dialog-item-action (lambda (item) (update-status (view-window item) item))
          :menu-box-list 
            '((" paftr" . #xA0) (" contr". #xB0) (" prog" . #xC0) (" maftr" . #xD0) (" ptchb" . #xE0)))) 
     (setf (controller-ctrl self) 
         (make-instance 'C-numbox
           :view-position (make-point 52 17)  
           :view-size (make-point 40 14)
           :dialog-item-action (lambda (item) (update-controller (view-window item) item))
           :min-val 0 :max-val 127)) 
     (setf (value-ctrl self) 
         (make-instance 'C-numbox
           :view-position (make-point 97 17)  
           :view-size (make-point 40 14)
           :dialog-item-action (lambda (item) (update-value (view-window item) item))
           :min-val 0 :max-val 127)) 
     (setf (label-ctrl self) 
          (make-instance 'C-ttybox-instrument
           :view-position (make-point 142 17)  
           :view-size (make-point 40 14)
           :dialog-item-action (lambda (item) (update-label (view-window item) item))
           :dialog-item-text "label")) 
     (setf (note-play-ctrl self) (make-instance 'BUTTON-DIALOG-ITEM
           :view-position (make-point 200 17)  
           :view-size (make-point 45 18)
           :dialog-item-action (lambda (item) (declare (ignore item))(play-note *global-selected-note*))
           :view-font '("Monaco" 9 :SRCOR :PLAIN)
           :dialog-item-text "Play"))))

(defmethod view-key-event-handler ((self C-fix-window) char)  
  (if (not (eql char #\Newline))
      (call-next-method)
      (progn (when (pw-win self) 
                 (window-select (pw-win self)))
              (window-hide self))))
 
;;=========================

(defgeneric update-low-limit (self ctrl))
(defgeneric update-high-limit (self ctrl))
(defgeneric update-status (self ctrl))
(defgeneric update-controller (self ctrl))
(defgeneric update-sample-rate (self ctrl))
(defgeneric update-label (self ctrl))
(defgeneric update-value (self ctrl))

(defmethod update-status ((self C-fix-window) ctrl)
  (setf (status (midi-ins-object self)) (patch-value ctrl ())))

(defmethod update-controller ((self C-fix-window) ctrl)
  (setf (controller (midi-ins-object self)) (patch-value ctrl ())))

(defmethod update-value ((self C-fix-window) ctrl)
  (setf (value (midi-ins-object self)) (patch-value ctrl ()))
  (erase+view-draw-contents *current-mn-editor*))

(defmethod update-label ((self C-fix-window) ctrl)
  (setf (label (midi-ins-object self)) (dialog-item-text ctrl))
  (erase+view-draw-contents *current-mn-editor*))

;;(defmethod play-selected-note ((self C-fix-window) ctrl) (play-note *global-selected-note*))

;;==============================================================================

(defun make-fix-ins-editor ()
  (make-instance 'C-fix-window :window-title "fix" :close-box-p nil :window-show nil
     :view-position (make-point 30 40) :view-size (make-point 254 45)))
     
;;(make-fix-ins-editor)        

;;==============================================================================
;;==============================================================================
;; midi-ins data types  
;;==============================================================================
;; abstract data type
(defclass C-midi-ins-data-type ()
  ((status     :initform 0  :initarg :status     :accessor status)
   (controller :initform 0  :initarg :controller :accessor controller)
   (label      :initform "" :initarg :label      :accessor label)))

(defgeneric make-connections-to-instrument-editor (self))
(defmethod make-connections-to-instrument-editor ((self C-midi-ins-data-type))
  (setf (midi-ins-object (editor-object self)) self))

(defmethod open-instrument-editor ((self C-midi-ins-data-type) win x y)
  (declare (ignore win x y))
  (set-window-title (editor-object self) (label self))
  (make-connections-to-instrument-editor self)
  (window-select (editor-object self)))

;;==============================================================================
;; fix

(defclass C-midi-ins-fix (C-midi-ins-data-type)
  ((editor-object :initform nil :allocation :class :accessor editor-object)
   (value :initform 0 :initarg :value :accessor value)))

(defmethod decompile ((self C-midi-ins-fix))
  `(make-instance ',(class-name (class-of self))
        :status ,(status self)
        :controller ,(controller self)
        :label ',(label self)
        :value ,(value self)))

(defmethod make-connections-to-instrument-editor :after ((self C-midi-ins-fix))
  (set-dialog-item-text-from-dialog
      (status-ctrl (editor-object self)) (format nil "~5D" (-  (/ (status self) 16) 10)))
  (set-dialog-item-text (label-ctrl (editor-object self)) (label self))
  (set-dialog-item-text-from-dialog (controller-ctrl (editor-object self)) (format nil "~5D" (controller self)))
  (set-dialog-item-text-from-dialog (value-ctrl (editor-object self)) (format nil "~5D" (value self))))

(defmethod open-instrument-editor :before ((self C-midi-ins-fix) win x y)
  (declare (ignore x y))
  (when (not (editor-object self))
    (setf (editor-object self) (make-fix-ins-editor)))
  (setf (pw-win (editor-object self)) win))

(defmethod draw-instrument ((self C-midi-ins-fix) x-now y-now dur)
  (declare (ignore y-now dur))
  (with-focused-view *current-MN-editor*
    (draw-string x-now *MN-note-ins-y* (label self))
    (incf *MN-note-ins-y* 10)
    (draw-string x-now *MN-note-ins-y* (format nil "~3d" (value self)))
    (incf *MN-note-ins-y* 10)))

#|
(defmethod play-instrument ((self C-midi-ins-fix) chan dur)
  (declare (ignore dur))
  (cond ((eql (status self) #xA0)) 
        ((eql (status self) #xB0) (write-controller-value chan (controller self) (value self)))
        ((eql (status self) #xC0) (write-program-change-value chan (value self)))
        ((eql (status self) #xD0)) 
        ((eql (status self) #xE0) (write-pitch-bend-value chan  (value self)))))
|#
(defmethod play-instrument ((self C-midi-ins-fix) note)
  (cond ((eql (status self) #xA0)) 
        ((eql (status self) #xB0) (write-controller-value (chan note) (controller self) (value self)))
        ((eql (status self) #xC0) (write-program-change-value (chan note) (value self)))
        ((eql (status self) #xD0)) 
        ((eql (status self) #xE0) (write-pitch-bend-value (chan note)  (value self)))))
;;==============================================================================
;; bpf

(defclass C-bpf-view-ins-win (C-BPF-window)
  ((pw-win :initform nil :accessor pw-win)
   (static-text-ctrl2  :initform nil  :accessor static-text-ctrl2)
   (label-ctrl   :initform nil  :accessor label-ctrl)
   (low-limit-ctrl   :initform nil  :accessor low-limit-ctrl)
   (high-limit-ctrl  :initform nil  :accessor high-limit-ctrl)
   (status-ctrl      :initform nil  :accessor status-ctrl)
   (controller-ctrl  :initform nil  :accessor controller-ctrl)
   (sample-rate-ctrl :initform nil  :accessor sample-rate-ctrl)
   (note-play-ctrl :initform nil  :accessor note-play-ctrl)
   (midi-ins-object :initform nil :accessor midi-ins-object)))


;;(defmethod initialize-instance :after ((self C-bpf-view-ins-win) &rest l))
(defmethod make-extra-bpf-view-ins-controls ((self C-bpf-view-ins-win))
  (add-subviews self 
   (setf (static-text-ctrl2 self) 
       (make-instance 'static-text-dialog-item
         :view-font '("Monaco" 9 :SRCOR :PLAIN)
         :dialog-item-text " low   high  status contr ticks "))
    (setf (label-ctrl self) (make-instance 'C-ttybox-instrument
         :view-size (make-point 40 14)
         :dialog-item-action (lambda (item) (update-label (view-window item) item))
         :dialog-item-text "label")) 
    (setf (low-limit-ctrl self) 
       (make-instance 'C-numbox
         :view-size (make-point 40 14)
         :dialog-item-action (lambda (item) (update-low-limit (view-window item) item))
         :min-val 0 :max-val 127)) 
    (setf (high-limit-ctrl self) 
        (make-instance 'C-numbox
         :view-size (make-point 40 14)
         :dialog-item-action (lambda (item) (update-high-limit (view-window item) item))
         :min-val 0 :max-val 127)) 
    (setf (status-ctrl self) 
        (make-instance 'C-menubox-val
         :view-size (make-point 40 14)
         :dialog-item-action (lambda (item) (update-status (view-window item) item))
         :menu-box-list 
            '(("paftr" . #xA0) ("contr". #xB0) ("prog" . #xC0) ("maftr" . #xD0) ("ptchb" . #xE0)))) 
    (setf (controller-ctrl self) 
        (make-instance 'C-numbox
         :view-size (make-point 40 14)
         :dialog-item-action (lambda (item) (update-controller (view-window item) item))
         :min-val 0 :max-val 127)) 
    (setf (sample-rate-ctrl self) 
         (make-instance 'C-numbox
          :view-size (make-point 40 14)
          :dialog-item-action (lambda (item) (update-sample-rate (view-window item) item))
          :min-val 1 :max-val 1000)) 
     (setf (note-play-ctrl self) 
         (make-instance 'BUTTON-DIALOG-ITEM
          :dialog-item-action (lambda (item) (declare (ignore item))(play-note *global-selected-note*))
          :view-size (make-point 40 14)
          :view-font '("Monaco" 9 :SRCOR :PLAIN)
          :dialog-item-text  " play"))))

;;=========================


(defmethod update-low-limit ((self C-bpf-view-ins-win) ctrl)
  (setf (low-limit (midi-ins-object self)) (patch-value ctrl ())))


(defmethod update-high-limit ((self C-bpf-view-ins-win) ctrl)
  (setf (high-limit (midi-ins-object self)) (patch-value ctrl ())))

(defmethod update-status ((self C-bpf-view-ins-win) ctrl)
  (setf (status (midi-ins-object self)) (patch-value ctrl ())))

(defmethod update-controller ((self C-bpf-view-ins-win) ctrl)
  (setf (controller (midi-ins-object self)) (patch-value ctrl ())))

(defmethod update-sample-rate ((self C-bpf-view-ins-win) ctrl)
  (setf (sample-rate (midi-ins-object self)) (patch-value ctrl ())))

(defmethod update-label ((self C-bpf-view-ins-win) ctrl)
  (setf (label (midi-ins-object self)) (dialog-item-text ctrl))
  (erase+view-draw-contents *current-mn-editor*))

;;==========================================
;; layout

(defmethod BPF-window-ctrl-1st-y ((self C-bpf-view-ins-win)) (- (h self) 60))
(defmethod BPF-window-ctrl-2nd-y ((self C-bpf-view-ins-win)) (- (h self) 48))


(defmethod set-ctrl-positions-extra ((self C-bpf-view-ins-win))
 (set-view-position (static-text-ctrl2 self) (make-point 5  (- (h self) 30)))
 (set-view-position (label-ctrl self)        (make-point 202  (- (h self) 31)))
 (set-view-position (low-limit-ctrl self)    (make-point 2  (- (h self) 16)))
 (set-view-position (high-limit-ctrl self)   (make-point 42 (- (h self) 16)))
 (set-view-position (status-ctrl self)       (make-point 82 (- (h self) 16)))
 (set-view-position (controller-ctrl self)   (make-point 122 (- (h self) 16))) 
 (set-view-position (sample-rate-ctrl self)  (make-point 162 (- (h self) 16)))
 (set-view-position (note-play-ctrl self)   (make-point 202 (- (h self) 16))))

;;==========================================
(defclass C-bpf-view-ins (C-bpf-view) ())

(defmethod set-size-view-window-grown ((self C-bpf-view-ins))
  (set-view-size self (subtract-points (view-size (view-window self)) (make-point 10 70)))) 

;;==========================================

(defun make-BPF-ins-editor (bp)
  (let* ((win (make-instance 
                'C-bpf-view-ins-win :window-title "BPF" :close-box-p nil :window-show nil
                :view-position #@(10 40) :view-size #@(250 305)))
         (bp-view 
          (make-instance 'C-bpf-view-ins 
               :view-container win
               :view-position #@(2 2) :view-size #@(240 235) 
               :break-point-function bp 
               :track-thumb-p t)))
      (add-subviews win bp-view)
      (setf (BPF-editor-object win) bp-view)
      (scale-to-fit-in-rect bp-view)
      win))

;; (window-select (make-BPF-ins-editor (make-break-point-function '(0 100) '(0 100))))
;;==============================================================================
;;==============================================================================
;; bpf

(defclass C-midi-ins-bpf (C-midi-ins-data-type) 
  ((editor-object :initform nil :allocation :class :accessor editor-object)
   (BPF-mini-view :initform (make-instance 'C-mini-bpf-view) :accessor BPF-mini-view)
   (break-point-function :initform nil :initarg :break-point-function :accessor break-point-function)
   (low-limit :initform 0 :initarg :low-limit :accessor low-limit)
   (high-limit :initform 0 :initarg :high-limit :accessor high-limit)
   (sample-rate :initform 0 :initarg :sample-rate :accessor sample-rate)))

(defmethod decompile ((self C-midi-ins-bpf))
  `(make-instance ',(class-name (class-of self))
        :break-point-function ,(decompile (break-point-function self))
        :low-limit ,(low-limit self)
        :high-limit ,(high-limit self)
        :status ,(status self)
        :controller ,(controller self)
        :label ',(label self)
        :sample-rate ,(sample-rate self)))

;;(defmethod update-label ((self C-midi-ins-bpf) ctrl)
;;  (declare (special *global-music-notation-panel* *global-editor-x*))
;;  (setf (label self) (str ctrl))
;;  (small-update-mus-panel *global-music-notation-panel* *global-editor-x* t))

(defmethod update-low-limit ((self C-midi-ins-bpf) ctrl)(setf (low-limit self) (value ctrl)))
(defmethod update-high-limit ((self C-midi-ins-bpf) ctrl)(setf (high-limit self) (value ctrl)))
(defmethod update-sample-rate ((self C-midi-ins-bpf) ctrl)(setf (sample-rate self) (value ctrl)))

(defmethod make-connections-to-instrument-editor :after ((self C-midi-ins-bpf))
  (add-bpf-to-bpf-editor-from-PW  (editor-object self) (break-point-function self))
  (set-dialog-item-text-from-dialog
      (status-ctrl (editor-object self)) (format nil "~5D" (-  (/ (status self) 16) 10)))
  (set-dialog-item-text (label-ctrl (editor-object self)) (label self))
  (set-dialog-item-text-from-dialog (controller-ctrl (editor-object self)) (format nil "~5D" (controller self)))
  (set-dialog-item-text-from-dialog (low-limit-ctrl (editor-object self)) (format nil "~5D" (low-limit self)))
  (set-dialog-item-text-from-dialog (high-limit-ctrl (editor-object self)) (format nil "~5D" (high-limit self)))
  (set-dialog-item-text-from-dialog (sample-rate-ctrl (editor-object self)) (format nil "~5D" (sample-rate self))))


(defvar *MN-midi-ins-bpf-object* ())

(defmethod open-instrument-editor :before ((self C-midi-ins-bpf) win x y)
  (declare (ignore x y))
  (when (not (editor-object self))
    (setf (editor-object self) (make-BPF-ins-editor (break-point-function self))))
  (setf *MN-midi-ins-bpf-object* self)
  (setf (pw-win (editor-object self)) win))

;;  (add-bpf-to-bpf-editor (car (controls (editor-object self))) 0  (break-point-function self))
;;  (setf (win (BPF-mini-view self)) win)
;;  (setf (pw-win (editor-object self)) win)
;;  (setf (mini-view (car (editor-objects (car (controls (editor-object self))))))
;;          (BPF-mini-view self))
;;  (scale-bpf-to-fit-in-rect (car (controls (editor-object self))) (break-point-function self))) 

(defmethod view-size ((self C-midi-ins-bpf)) 50)
;;view draw-rects-fl h-view-scaler v-view-scaler
(defmethod draw-instrument ((self C-midi-ins-bpf) x-now y-now dur)
  (declare (ignore y-now dur))
  (with-focused-view *current-MN-editor*
    (draw-string x-now *MN-note-ins-y* (label self))
    (incf *MN-note-ins-y* 12)))
;;    (draw-bpf-function  (break-point-function self) self nil 1.0 1.0)
;;    (set-view-position (BPF-mini-view self) (make-point x-now *MN-note-ins-y*))
;;    (incf *MN-note-ins-y* 30)
;;    (set-view-size (BPF-mini-view self) dur 20)
;;    (set-break-point-function-to-mini (BPF-mini-view self) (break-point-function self))
;;    (view-draw-contents (BPF-mini-view self))))

(defgeneric continue-play-pitchb (self delay chan points))
(defmethod continue-play-pitchb ((self C-midi-ins-bpf) delay chan points)
  (write-pitch-bend-value chan (pop points))
  (when points 
    (dfuncall delay 'continue-play-pitchb self delay chan points)))

(defgeneric continue-play-contr (self delay chan controller points))
(defmethod continue-play-contr ((self C-midi-ins-bpf) delay chan controller points)
  (write-controller-value chan controller (pop points))
  (when points 
    (dfuncall delay 'continue-play-contr self delay chan controller points)))

#|
(defmethod play-instrument ((self C-midi-ins-bpf) chan dur)
  (let ((points)
        (x-points (give-x-points (break-point-function self)))
        (y-points (give-y-points (break-point-function self)))
        (scfc)(min-val)(max-val))
   (setq min-val (apply #'min y-points))
   (setq max-val (apply #'max y-points))
   (setq scfc (/ (- (high-limit self)(low-limit self)) (- max-val min-val)))
   (setq y-points (mapcar #'round
     (mapcar #'+ (cirlist (low-limit self))
       (mapcar #'* (cirlist scfc)
         (mapcar #'- y-points (cirlist min-val))))))
   (setq points (break-point-fun  (truncate (/ dur (sample-rate self))) x-points y-points))
   (when points 
     (cond ((eql (status self) #xA0)) 
           ((eql (status self) #xB0) 
             (start 
               (continue-play-contr self (sample-rate self) chan (controller self) points)))  
           ((eql (status self) #xC0)) 
           ((eql (status self) #xD0)) 
           ((eql (status self) #xE0)
            (start (continue-play-pitchb self (sample-rate self) chan points)))))))
|#

(defmethod play-instrument ((self C-midi-ins-bpf) note)
  (let ((points)
        (x-points (give-x-points (break-point-function self)))
        (y-points (give-y-points (break-point-function self)))
        (scfc)(min-val)(max-val))
   (setq min-val (apply #'min y-points))
   (setq max-val (apply #'max y-points))
   (setq scfc (/ (- (high-limit self)(low-limit self)) (- max-val min-val)))
   (setq y-points (mapcar #'round
     (mapcar #'+ (cirlist (low-limit self))
       (mapcar #'* (cirlist scfc)
         (mapcar #'- y-points (cirlist min-val))))))
   (setq points (break-point-fun  (truncate (/ (dur note) (sample-rate self))) x-points y-points))
   (when points 
     (cond ((eql (status self) #xA0)) 
           ((eql (status self) #xB0) 
             (start 
               (continue-play-contr self (sample-rate self) (chan note) (controller self) points)))  
           ((eql (status self) #xC0)) 
           ((eql (status self) #xD0)) 
           ((eql (status self) #xE0)
            (start (continue-play-pitchb self (sample-rate self) (chan note) points)))))))
;;==============================================================================
;; data types collector used by mid-ins and C-note instrument field
;;==============================================================================


(defclass C-midi-ins-collection ()
  ((ins-objects :initform nil :initarg :ins-objects :accessor ins-objects)
   (ins-name :initform nil :initarg :ins-name :accessor ins-name)))

(defmethod decompile ((self C-midi-ins-collection))
  `(make-instance ',(class-name (class-of self))
      :ins-name ',(ins-name self)
      :ins-objects (list ,@(ask-all (ins-objects self) 'decompile))))
  
;;!!!
(defmethod draw-instrument ((self C-midi-ins-collection) x-now y-now t-scfactor)
   (set-view-font  (view-container (view-container  *current-MN-editor*)) '("Monaco"  9  :srcor))
   (draw-string x-now *MN-note-ins-y* (ins-name self))
   (incf *MN-note-ins-y* 12)
   (tell (ins-objects self) 'draw-instrument x-now y-now t-scfactor)
   (set-view-font  (view-container (view-container  *current-MN-editor*)) *music-font-spec*))

#|
(defmethod play-instrument ((self C-midi-ins-collection) chan dur)
  (tell (ins-objects self) 'play-instrument chan dur))
|#

(defmethod play-instrument ((self C-midi-ins-collection) note)
  (tell (ins-objects self) 'play-instrument note))

(defvar *global-editor-x* ())

(defmethod open-instrument-editor ((self C-midi-ins-collection) win x y)
  (setq *global-editor-x* x)
  (let ((labels (ask-all (ins-objects self) 'label))
        (objects (ins-objects self))
        (funs))
     (while objects
       (push `(open-instrument-editor ,(pop objects) ,win ,x ,y) funs)) 
     (make-pw-pop-up (pairlis (nreverse labels) funs))))

(defmethod remove-instrument-item ((self C-midi-ins-collection) x y)
  (declare (ignore x y))
  (let ((labels (ask-all (ins-objects self) 'label))
        (objects (ins-objects self))
        (funs))
     (while objects
       (push `(progn 
                (setf (ins-objects ,self) 
                  (remove  ,(pop objects) (ins-objects ,self) :test 'eq)) 
                (when (null (ins-objects ,self))(setf (instrument *global-selected-note*) ())) 
;;                (small-update-mus-panel *global-music-notation-panel* ,x)
)
              funs)) 
     (make-pw-pop-up (pairlis (nreverse labels) funs))))


(defmethod make-super-note-connections ((self C-midi-ins-collection) super-note super-win)
  (declare (ignore super-note super-win)))
;;==============================================================================
#|
(defmethod add-bpf-to-note ((self C-note) win x y)
  (declare (special *global-music-notation-panel*) (ignore y win))
  (let* (new-bpf (defaults (give-MN-BPF-defaults))
         (bpf (eval (decompile (car defaults))))
         (low (nth 1 defaults)) (high (nth 2 defaults)) (status (nth 3 defaults)) 
         (controller (nth 4 defaults)) (sample-rate (nth 5 defaults))
         (label (nth 6 defaults)) )
     (setq new-bpf
          (make-instance 'C-midi-ins-bpf
              :break-point-function  bpf
              :low-limit low
              :high-limit high
              :status status
              :controller controller
              :label label
              :sample-rate sample-rate))
      (if (instrument self)
        (setf (ins-objects (instrument self)) 
           (append (ins-objects (instrument self)) (list new-bpf)))
        (setf (instrument self)  (make-instance 'C-midi-ins-collection
           :ins-name (give-MN-ins-name-default)
           :ins-objects (list new-bpf)))))
      (small-update-mus-panel *global-music-notation-panel* x))

(defmethod add-fix-to-note ((self C-note) win x y)
  (declare (special *global-music-notation-panel*) (ignore y win))
  (let* (new-fix (defaults (give-MN-fix-defaults))
         (status (nth 0 defaults)) 
         (controller (nth 1 defaults)) (value (nth 2 defaults))
         (label (nth 3 defaults)) )
    (setq new-fix
           (make-instance 'C-midi-ins-fix
              :status        status
              :controller    controller
              :value         value
              :label         label))
      (if (instrument self)
        (setf (ins-objects (instrument self)) 
           (append (ins-objects (instrument self)) (list new-fix)))
        (setf (instrument self)  (make-instance 'C-midi-ins-collection
           :ins-name (give-MN-ins-name-default)
           :ins-objects (list new-fix)))))
      (small-update-mus-panel *global-music-notation-panel* x t))
|#
;;==========================================

