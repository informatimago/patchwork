;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pw-bpf-boxes.lisp
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

;;====================================================================================================
;;====================================================================================================
;; BPFunction

;;these two are probably redundant now... (Camilo 911114)
(defvar *bpf-pw-type*
  (make-instance 'C-pw-type :control-form `(make-instance 'C-mini-bpf-view  :view-size #@(74 72)
     :type-list '(no-connection))))

(defvar *bpf-input-pw-type*
  (make-instance 'C-pw-type
  :control-form
   `(make-instance 'C-ttybox  :view-size #@(36 14) :dialog-item-text "bpf"
     :type-list '(bpf))))
;;==================================================

(add-pw-input-type 'bpf 'C-mini-bpf-view
                   (list :view-size #@(74 72) :type-list '(no-connection)))

(add-output-type 'bpf '(bpf))
;;====================================================================================================
;; 1 = timelist 2 = valuelist 3 = bpf
(defvar *collector-popUp-menu*
  (new-menu " "
            (new-leafmenu "Save" (lambda () (save *target-action-object*)))))

(defvar *points-menu* ())

(defvar *bpf-popUp-menu* ())

(defun make-bpf-pops ()
  (setf *points-menu* (new-leafmenu "flip-mode" (lambda () (set-the-points-view *target-action-object*))))
  (setf *bpf-popUp-menu*
        (new-menu " "  *points-menu*
              (new-leafmenu "-" ())
              (new-leafmenu "bpf object" (lambda () (set-output *target-action-object* :bpf)))
              (new-leafmenu "X points" (lambda () (set-output *target-action-object* :x-points)))
              (new-leafmenu "Y points" (lambda () (set-output *target-action-object* :y-points)))
              (new-leafmenu "-" ())
              (new-leafmenu "Save" (lambda () (save *target-action-object*))))))

(defclass  C-patch-function (C-patch-application C-pw-resize-x)
  ((popUpBox :initform nil :accessor popUpBox)
   (points-state :initform nil :accessor points-state)
   (out-type :initform :bpf :accessor out-type)
   (current-str :initform #\B :accessor current-str)
   (lock :initform nil :accessor lock)
   (value :initform nil :accessor value)))

(defmethod initialize-instance :after ((self C-patch-function) &key controls)
  (declare (ignore controls))
  (setf (popUpBox self)
        (make-popUpbox "  " self
                       *bpf-popUp-menu*
                       :view-position (make-point (- (w self) 8)
                                                  (- (h self) 14))
                       :view-container self
                       :view-font *patchwork-font-spec*))
  (setf (lock self)
        (make-instance 'C-patch-buffer::C-radio-button
                       :view-position (add-points (view-position (out-put self)) #@(20 -6))
                       :view-size (make-point 8 8)
                       :dialog-item-text (get-initial-button-text self)
                       :view-font *patchwork-small-font-spec*
                       :view-container self
                       :dialog-item-action (get-lock-button-fun self)))
  (let ((mini-bpf (give-mini-bpf self)) bpf)
    (unless (break-point-function mini-bpf)
      (setq bpf (make-break-point-function '(0 100) '(0 100)))
      (set-break-point-function-to-mini mini-bpf bpf))
    (add-bpf-to-bpf-editor-from-PW (application-object self) (break-point-function mini-bpf))
    (set-mini-view (application-object self) mini-bpf)
    self))

(defmethod get-lock-button-fun ((self C-patch-function))
  (lambda (item)
      (if (value (view-container item))
        (progn
          (set-dialog-item-text item "o")
          (record-event :|PWst| :|cann| `((,:|----| ,(mkSO :|cbox| nil :|name| (pw-function-string self))))))
        (progn
          (set-dialog-item-text item "x")
          (record-event :|PWst| :|cand| `((,:|----| ,(mkSO :|cbox| nil :|name| (pw-function-string self)))))))
      (setf (value (view-container item))
            (not (value (view-container item))))))

(defmethod get-initial-button-text ((self C-patch-function)) "o")

(defmethod set-output ((self C-patch-function) o-type)
  (cond ((and (eql o-type :bpf) (not (eql (out-type self) :bpf)))
         (setf (type-list self) '(bpf))
         (erase-my-connections self))
        ((and (eql (out-type self) :bpf) (not (eql o-type :bpf)))
         (setf (type-list self) '(list))
         (erase-my-connections self)))
  (setf (out-type self) o-type)
  (setf (current-str self)
        (case o-type
          (:bpf #\B) (:x-points #\X) (:y-points #\Y)))
  (set-box-title (popUpBox self) "  ")  ;(string (current-str self)))
  (draw-appl-label self (current-str self))
  )

(defmethod open-patch-win ((self C-patch-function))
  (set-box-title (popUpBox self) "  ")
  (call-next-method))

(defgeneric erase-BPF-label? (self)
  (:method ((self C-patch-function))
    (set-box-title (popUpBox self) "  ")))

(defmethod draw-appl-label ((self C-patch-function) label)
  (when label
    (with-font-focused-view self
      (draw-char (- (w self) 8) (- (h self) 4) label))))

(defmethod draw-patch-extra ((self C-patch-function))
  (when (application-object self)
    (draw-appl-label self
      (if (eql (front-window) (application-object self)) #\* (current-str self)))))


(defmethod erase-my-connections ((self C-patch-function))
  (let ((patches (subviews *active-patch-window*)))
    (dolist (patch patches)
      (if (am-i-connected? patch self)
          (progn
            (draw-connections patch t)
            (disconnect-my-self patch self)
            (draw-connections patch ))))))

(defgeneric set-the-points-view (self)
  (:method ((self C-patch-function))
    (setf (points-state self) (not (points-state self)))
    (let ((*no-line-segments* (points-state self)))
      (update-mini-view (give-mini-bpf self))
      (view-draw-contents (application-object self)))))

(defgeneric display-only-points (self)
  (:method ((self C-patch-function))
    (points-state self)))

(defmethod decompile ((self C-patch-function))
  (let ((bpf (break-point-function (give-mini-bpf self))))
    (append (call-next-method) `(',(list (and bpf (break-point-list bpf)) (points-state self)
                                         (out-type self))))))

(defmethod complete-box ((self C-patch-function) rest)
  (if (consp (first rest)) (setf (points-state self) (second rest)))
  (let ((str (pw-function-string self))
        (mini-bpf (give-mini-bpf self))
        (args (if (consp (first rest)) (first rest) rest))
        (*no-line-segments* (display-only-points self)))
    (setf (break-point-function mini-bpf)
          (make-instance 'C-break-point-function :break-point-list args))
    (set-break-point-function-to-mini mini-bpf (break-point-function mini-bpf))
    (add-bpf-to-bpf-editor-from-PW (application-object self) (break-point-function mini-bpf))
    (set-mini-view (application-object self) mini-bpf)
    (set-window-title (application-object self) str)
    (setf (pw-function-string self) str)
    (if (third rest) (set-output self (third rest)))
    self))

(defmethod make-application-object ((self C-patch-function))
  (setf (application-object self)
        (make-BPF-editor (make-break-point-function '(0 100) '(0 100)))))

(defgeneric give-mini-bpf (self)
  (:method ((self C-patch-function)) (third (pw-controls self))))

(defmethod patch-value ((self C-patch-function) obj)
  (unless (or (value self)
              (and  (eql (first (pw-controls self)) (first (input-objects self)))
                    (eql (second (pw-controls self)) (second (input-objects self)))))
    (let* ((in-1 (patch-value (first (input-objects self)) obj))
           (in-2 (patch-value (second (input-objects self)) obj))
           (bpf
            (make-break-point-function
              (if (consp in-1) (mapcar #'round in-1) (round in-1))
              (if (consp in-2) (mapcar #'round in-2) (round in-2))))
          (*no-line-segments* (display-only-points self)))
      (setf (break-point-function (give-mini-bpf self)) bpf)
      (update-mini-view (give-mini-bpf self))
      (add-bpf-to-bpf-editor-from-PW (application-object self) bpf)))
  (case (out-type self)
    (:bpf (break-point-function (give-mini-bpf self)))
    (:x-points (slot-value (break-point-function (give-mini-bpf self)) 'X-points))
    (:y-points (slot-value (break-point-function (give-mini-bpf self)) 'Y-points))))

(defmethod resize-patch-box ((self C-patch-function) mp delta)
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
          (make-point (- (round (w self) 2) 6) (- (h self) 5)))
      (set-view-position (popUpBox self)
                         (make-point (- (w self) 8) (- (h self) 14)))
      (set-view-position (lock self) (add-points (view-position (out-put self)) #@(20 -6)))
      (let ((*no-line-segments* (display-only-points self)))
        (update-mini-view (give-mini-bpf self))))))

(defmethod view-draw-contents ((self C-patch-function))
  (let ((*no-line-segments* (display-only-points self)))
    (call-next-method)))

(defunp bpf ((tlist (fix>0s? (:value 10))) (vlist (fix>0s? (:value 100)))
             (mini-bpf bpf)) bpf
"A <bpf>-box with a breakpoint function (bpf). A function editor can be opened by
selecting this box and typing 'o' from the keyboard. The bpf can be changed from
PatchWork by connecting a value-list to the <vlist> input box and option-clicking its
output box. If there is no connection in the first input box <tlist> then the points have a
constant time-difference.
If the first input box <tlist> is connected, then the input
should be a list of ascending timepoints. One can change the representation of the
module bpf (in segments by default) to a representation in points by selecting <flip-
mode> in the front menu. Click on the "
  (declare (ignore tlist vlist mini-bpf)))

;;===========================================

(defclass  C-patch-env (C-patch)())

(defmethod patch-value ((self C-patch-env) obj)
   (let ((break-point-function (patch-value (car (input-objects self)) obj)))
      (bpf-out break-point-function (clock obj) (give-x-points break-point-function))))

(defunp env ((bpf-ob (list (:value '() :type-list (bpf))))) fix
"The input bpf should allways be connected with a BPF box .
Is used normally with a patch that is driven by a PW box with
its own clock (midi,oscilloscope).
Behaves like an envelope. If values are asked outside the timerange
of the bpf,env returns a constant value (first or last value)"
  (declare (ignore bpf-ob)))
;;________________

(defclass  C-patch-transfer (C-patch)())

#|(defunp transfer ((bpf-ob (list (:value '() :type-list (bpf list))))
                  (x-val integer (:type-list (fixnum list)))) midic
"The input <bpf> should allways be connected with a BPF box .
Behaves like a transfer function when feeded with values
to the second inputbox <x-val>. returns the y value (or list of y values)
 which corresponds to <x-val> (or list of x values) for the connected bpf"
  (if bpf-ob
    (let ((points (list! x-val))
          result)
      (setq result
            (mapcar (lambda (x) (bpf-out bpf-ob x (give-x-points bpf-ob)))
                    points))
      (if (consp x-val)  result (car result)))))|#

(defunp transfer ((bpf-ob (list (:value '() :type-list (bpf list))))
                  (x-val integer (:type-list (fixnum list)))) midic
"The input <bpf> should always be connected with a multi-bpf  box .
Behaves like a transfer function when feeded with values
to the second inputbox <x-val>. returns the y value (or list of y values)
 which corresponds to <x-val> (or list of x values) for the connected bpf"
    (get-transfer-output bpf-ob x-val))

(defgeneric get-transfer-output (self points))
(defmethod get-transfer-output ((self null) points) (declare (ignore points)) nil)

(defmethod get-transfer-output ((self C-break-point-function) points)
  (let ((result (mapcar (lambda (x) (bpf-out self x (give-x-points self)))
                    (list! points))))
    (if (consp points)  result (car result))))

(defmethod get-transfer-output ((self cons) points)
  (if (subtypep (type-of (first self)) 'C-break-point-function)
    (let ((y-vals (mapcar (lambda (bpf) (get-transfer-output bpf points)) self)))
      (if (rest y-vals) y-vals (first y-vals)))
    (progn (format t "input is not a bpf or lists of bpfs ~%") (ui:ed-beep))))

(defunp bpf-sample ((bpf-ob0 list (:value 'nil :type-list (list pw::bpf)))
                    (echan1 fix>0 (:min-val 2 :value 2)) (xinit2 fix) (xend3 fix (:value 100))
                    (fact4 fix/float (:value 1)) (nbdec5 fix)) list
"  The bpf-sample module creates a list starting by sampling a
breakpoint function table;. bpf-ob  is the input to the table,
echant is the number of samples desired, xinit et xend delimit the  sampling interval;.
The fact   variable is a multiplicative coefficient for scaling the data,
and nbdec  is the number of decimals desired in the output.
 "

  (get-bpf-sample-output bpf-ob0 echan1 xinit2 xend3 fact4 nbdec5))

(defgeneric get-bpf-sample-output (self echan1 xinit2 xend3 fact4 nbdec5))
(defmethod get-bpf-sample-output ((self null) echan1 xinit2 xend3 fact4 nbdec5)
  (declare (ignore echan1 xinit2 xend3 fact4 nbdec5)) self)

(defmethod get-bpf-sample-output ((self C-break-point-function) echan1 xinit2 xend3 fact4 nbdec5)
  (epw::g-round
   (epw::g* (transfer self (epw:arithm-ser xinit2 (/ (- xend3 xinit2) (- echan1 '1))
                                              xend3)) fact4) nbdec5))

(defmethod get-bpf-sample-output ((self cons) echan1 xinit2 xend3 fact4 nbdec5)
  (mapcar (lambda (bpf) (get-bpf-sample-output bpf echan1 xinit2 xend3 fact4 nbdec5))
          self))

(defmethod get-bpf-sample-output ((self number) echan1 xinit2 xend3 fact4 nbdec5)
  (declare (ignore echan1 xinit2 xend3 fact4 nbdec5))  self)
;;________________

(defclass  C-patch-osc (C-patch)())

(defmethod patch-value ((self C-patch-osc) obj)
   (let ((break-point-function (patch-value (car (input-objects self)) obj)))
      (bpf-out-osc break-point-function (clock obj) (give-x-points break-point-function))))

(defunp osc ((bpf-ob (list (:value '() :type-list (bpf))))) fix
"The input bpf should allways be connected with a BPF box.
Is used normally with a patch that is driven by a PW box with
its own clock (midi,oscilloscope).
Behaves like an oscillator."
  (declare (ignore bpf-ob)))
;;________________

(defclass  C-patch-osc-period (C-patch) ())

(defmethod patch-value ((self C-patch-osc-period) obj)
   (let ((break-point-function (patch-value (car (input-objects self)) obj)))
      (bpf-out-osc-period break-point-function (clock obj)
           (patch-value (nth 1 (input-objects self)) obj) (give-x-points break-point-function))))


(defunp osc-period ((bpf-ob (list (:value '() :type-list (bpf))))
                    (period (integer (:value 100)))) fix
"The input bpf should allways be connected with a BPF box .
Is used normally with a patch that is driven by a PW box with
its own clock (midi,oscilloscope).
Behaves like an oscillator with a period that is given
by the second inputbox"
  (declare (ignore bpf-ob period)))

;;________________

(defclass  C-patch-osc-phase (C-patch)
   ((osc-phase :initform 0 :accessor osc-phase)
    (old-time :initform 0 :accessor old-time)))

(defmethod init-patch ((self C-patch-osc-phase))
  (call-next-method)
  (setf (osc-phase self) 0)
  (setf (old-time self) 0))

(defmethod patch-value ((self C-patch-osc-phase) obj)
   (let* ((break-point-function (patch-value (car (input-objects self)) obj))
          (time (clock obj))
          (old-time (old-time self))
          (period (patch-value (nth 1 (input-objects self)) obj))
          (points (give-x-points break-point-function))
          (time-diff (- (car (last points)) (car points))))
      (setf (old-time self) time)
      (bpf-out break-point-function
            (+ (setf (osc-phase self)
                  (float (mod (+ (osc-phase self)
                          (/ (* time-diff (- time old-time)) period)) time-diff)))
                (car points))
             points)))

(defunp oscil-phase ((bpf-ob (list (:value '() :type-list (bpf))))
                   (period (integer (:value 100)))) fix
"The input bpf should allways be connected with a BPF box .
Is used normally with a patch that is driven by a PW box with
its own clock (collector,oscilloscope).
Behaves like an oscillator with a period that is given
by the second inputbox.The period can be controlled dynamically
but then the osc-phase should be driven only by one PW clock-object,
because the phase is stored in the phase-box each time there is a
request at input."
  (declare (ignore bpf-ob period)))

;;====================================================================================================

(defclass  C-patch-bpf-lib (C-patch)())

(defmethod patch-value ((self C-patch-bpf-lib) obj)
  (patch-value (car (pw-controls self)) obj))

;; ;;not needed??!!  [911107]
;; (setq *bpf-lib-pw-type*
;;   (make-instance 'C-pw-type :control-form `(make-instance 'C-menubox-bpf
;;     :view-size (make-point 40 40)
;;     :menu-box-list ,*pw-BPF-library* :type-list '(no-connection))))


(add-pw-input-type 'bpf-lib 'C-menubox-bpf
                   (list :view-size (make-point 40 40)
                         :menu-box-list *pw-BPF-library* :type-list '(no-connection)))

(defunp bpf-lib ((bpflib bpf-lib)) bpf
"Breakpoint functions can be stored in a library.
There is only one current  library, structured as
a circular list. The menu item add to lib
\(in the bpf  menu, when the window of multiple-bpf  module is open)
adds the current BPF to the library and reset lib resets the library to one item: a ramp.
The menu items next BPF from lib   and prev BPF from lib   allow browsing in the library.
"
  (declare (ignore bpflib)))

;;;; THE END ;;;;
