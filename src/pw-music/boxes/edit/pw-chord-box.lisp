;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pw-chord-box.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;
;;;;    The chord box with Menu chosen functionalities.
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

;;; The Class of  chord box patch, with menu chosen output type
;;;Class-name: C-patch-chord-box-M
;;;inherits from: C-patch-application
;;;Constructor: None
;;;
;;;Methods:
;;; set-output            ;sets the type of chord box patch output
;;; draw-control
;;; draw-fun-str          ;draws a char identifying the output type
;;; open-patch-win        ;opens the MN-window associated with the box
;;; update-win-pointers
;;; patch-value
;;; abstraction-p         ;tells wether an absin box is connected to its input
;;; PV-from-abstraction   ; gets the box in-value from an absin
;;; PV-from-input         ; gets input from normal input patch
;;; get-output-dimension  ; produces the appropriate output according to Menu selection


(defvar *g2-g-f-f2-staffs*)
;; (defgeneric editor-objects (object))

(defclass C-patch-chord-box-M (C-patch-application)
 ((mus-not-editor :initform nil  :accessor mus-not-editor)
  (out-type :initform :midic :initarg :out-type :accessor out-type)
  (popUpBox :initform nil :accessor popUpBox)
  (lock :initform nil :accessor lock)
  (value :initform nil :accessor value)
  (current-str :initform "M" :initarg :current-str :accessor current-str)))

(defmethod decompile ((self C-patch-chord-box-M))
  (append (call-next-method)
          `(nil ,(out-type self))))

(defmethod complete-box ((self C-patch-chord-box-M) output-type)
  (set-output self output-type))

(defun make-pw-chord-box (box outtype)
  (set-output box outtype)
  box)

;;A popUpMenu for selecting the type of chord-box output (patch-value)
(defparameter *Chord-box-popUpMenu*
  (new-menu " "
            (new-leafmenu "Midics" (lambda () (set-output *target-action-object* :midic)))
            (new-leafmenu "Durations" (lambda () (set-output *target-action-object* :duration)))
            (new-leafmenu "Velocity" (lambda () (set-output *target-action-object* :dynamic)))
            (new-leafmenu "Offsets" (lambda () (set-output *target-action-object* :offset)))
            (new-leafmenu "Reorder" (lambda () (set-output *target-action-object* :order)))
            (new-leafmenu "Chord Object" (lambda () (set-output *target-action-object* :object)))
            (new-leafmenu "Save Chord" (lambda () (save *target-action-object* )))))

(defmethod initialize-instance :after ((self C-patch-chord-box-M) &key controls)
  (declare (ignore controls))
  (setf (popUpBox self)
        (make-popUpbox  "M" self
                       *Chord-box-popUpMenu*
                       :view-position (make-point (- (w self) 13)
                                                  (- (h self) 14))
                       :view-container self
                       :view-font *patchwork-font-spec*))
  (setf (lock self)
        (make-instance 'C-patch-buffer::C-radio-button
                       :view-position (make-point 4 (- (h self) 9))
                       :view-size (make-point 8 8)
                       :dialog-item-text (get-initial-button-text self)
                       :view-font *patchwork-small-font-spec*
                       :view-container self
                       :dialog-item-action (get-lock-button-fun self)))
  (if (or (not (mus-not-editor self))
          (window-killed-p (mus-not-editor self)))
    (rebuild-editor self))
  (set-pw-win+pw-obj (mus-not-editor self) *active-patch-window* self))

(defmethod get-lock-button-fun ((self C-patch-chord-box-M))
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

(defmethod get-initial-button-text ((self C-patch-chord-box-M)) "o")


(defmethod set-output ((self C-patch-chord-box-M) o-type)
  (cond ((and (eql o-type :object) (not (eql (out-type self) :object)))
         (setf (type-list self) '(chord))
         (erase-my-connections self))
        ((and (eql (out-type self) :object) (not (eql o-type :object)))
         (setf (type-list self) '(list))
         (erase-my-connections self)))
  (setf (out-type self) o-type)
  (setf (current-str self)
        (case o-type
          (:midic "M") (:duration "D") (:dynamic "V") (:offset "O") (:order "R")
          (:object "C")))
  (setf (doc-string (car (subviews self))) (current-str self))
  (set-box-title (popUpBox self) (current-str self)) )


(defmethod draw-function-name ((self C-patch-chord-box-M) x y)
  (with-focused-view self
    (set-view-font (view-container self) *patchwork-copy-font-spec*)
    (draw-string x (- y 2) (string-downcase (pw-function-string self)))
    (set-view-font (view-container self) *patchwork-font-spec*)))


(defmethod erase-my-connections ((self C-patch-chord-box-M))
  (let ((patches (subviews *active-patch-window*)))
    (dolist (patch patches)
      (if (am-i-connected? patch self)
          (progn
            (draw-connections patch t)
            (disconnect-my-self patch self)
            (draw-connections patch ))))
    ))

(defmethod draw-patch-extra ((self C-patch-chord-box-M ))
  (values))

(defmethod draw-appl-label ((self C-patch-chord-box-M ) label)
  (declare (ignore label))
  (values))

(defmethod open-patch-win ((self C-patch-chord-box-M ))
  (if (or (not (mus-not-editor self))
              (window-killed-p (mus-not-editor self)))
            (rebuild-editor self))
  (let ((editor (car (editor-objects (car (subviews (mus-not-editor self))))))
        (in-box (car (pw-controls self))))
    (setf (chord-line editor) (chord-line in-box))
    (set-pw-win+pw-obj (mus-not-editor self) *active-patch-window* self)
    (window-select (mus-not-editor self))))

(defgeneric rebuild-editor (self))
(defmethod rebuild-editor ((self C-patch-chord-box-M))
  (setf (mus-not-editor self) (make-MN-editor-chord-box-M *g2-g-f-f2-staffs*))
  (setf (chord-line (car (editor-objects (editor-view-object (mus-not-editor self)))))
        (chord-line (car (pw-controls self))))
  (if *active-patch-window*
      (set-my-win-pos self (mus-not-editor self))))

(defmethod remove-yourself-control ((self C-patch-chord-box-M))
  (if (not (window-killed-p (mus-not-editor self)))
    (remove-yourself-control (mus-not-editor self))))

(defgeneric set-my-win-pos (self MN-win))
(defmethod set-my-win-pos ((self C-patch-chord-box-M) MN-win)
  (set-view-position MN-win
                     (add-points (view-position self)
                                 (view-position *active-patch-window*))))

(defmethod update-win-pointers ((self C-patch-chord-box-M ) win-ptr)
  (set-pw-win+pw-obj  (mus-not-editor self) win-ptr self))

(defmethod patch-value ((self C-patch-chord-box-M ) obj)
  (let ((my-in (car (pw-controls self)))
        (my-connect (car (input-objects self))))
    (unless (or (value self) (eql my-in my-connect))
      (PV-from-input self (list! (patch-value my-connect obj)))))
    (get-output-dimension self))

(defmethod patch-work-type-of ((self C-patch-chord-box-M) ctrl-index)
  (declare (ignore ctrl-index))
  (list 'list (list :value
                    (format nil "~D" (get-chord-midics (car (pw-controls self))))
                    :type-list '(list midic chord))))

(defgeneric PV-from-input (self val))
(defmethod PV-from-input ((self C-patch-chord-box-M ) val)
  (let* ((my-in (car (pw-controls self)))
         (old-chords (chords  (chord-line my-in))))
    (cond ((subtypep (type-of (car val)) (type-of (car old-chords))) ;entry is an object
           (setf (t-time (car val)) 0)
           (setf (chords  (chord-line my-in)) val))
          (t
           (setf (chords  (chord-line my-in))
                 (list (make-chord-object val 0 (type-of (car old-chords)))))
           (if (and old-chords (notes (car old-chords)))
               (mapc (lambda (new-note old-note)
                       (setf (dur new-note) (dur old-note))
                       (setf (vel new-note) (vel old-note))
                       (setf (offset-time new-note) (offset-time old-note))
                       (setf (comm new-note) (comm old-note))
                                        ;(setf (order new-note) (order old-note))
                       )
                     (notes (car (chords  (chord-line my-in)))) (notes (car old-chords))))))
    (update-control my-in)))

(defgeneric get-output-dimension (self))
(defmethod get-output-dimension ((self C-patch-chord-box-M ))
  (let ((my-in (car (pw-controls self))))
    (case (out-type self)
      (:object (make-copy
                (car (chords (chord-line my-in))) 0))
      (:midic (get-chord-midics my-in))
      (:offset (get-chord-offset my-in))
      (:duration (get-chord-duration my-in))
      (:dynamic (get-chord-dynamic my-in))
      (:order (order-chord-midics self (get-chord-midics my-in)
                                  (get-chord-order my-in))))))

(defgeneric order-chord-midics (self midics order))
(defmethod order-chord-midics ((self C-patch-chord-box-M ) midics order)
  (mapcar #'cdr (sort (pairlis order midics) #'< :key #'car)))

(defmethod play ((self C-patch-chord-box-M ))
  (play-your-chords (chord-line (car (pw-controls self)))))


(defunp chord ((chord ch-box)) list
"cThe module chord is a constructor module for chords.
Its input can be a list  of  midicents or a chord object.
In the first case, a chord object having notes with
the given pitches is created (or modified,
if a chord for the module exists already).
In the  second case, the given chord object is
copied into the module. A chord module has a
popUp  menu linked to the letter just to the right of its output box. The output of the
chord  module depends on the menu item chosen. The items in this menu are as
follows:

midics:         output is the list of midic slots of the notes.

Durations:      output is the list of dur  slots of the notes.

Velocity:       output is the list of vel  slots of the notes.

offsets:        output is the list of offset-time  slots.

Reorder:        output is the list of midic  slots, reordered according to their
              order slots.

Chord Object:   output is the whole chord object.

Save Chord:     output does not change. The module is saved in a file.

The letter to the right of the chord module's output box indicates the current
output
option.

An   editor ;for the chord object is entered either by selecting the module and
typing
the letter o, or by double-clicking on the module's name. Type h with the music
notation editor opened for more information.

"
  (declare (ignore chord)))

;;(push-to-object-types 'chord)

;;(setf *chord-box-M-pw-type*
;;  (make-instance 'C-pw-type :control-form
;;                 `(make-instance 'C-chord-box  :view-size (make-point 38 120)
;;     :type-list '(list midic chord))))

;;The new constructor function for the new chord box editor
(defclass C-chordbox-Win (C-mn-window) ())

(defmethod view-deactivate-event-handler :after ((self C-chordbox-Win))
  (menu-item-disable *undo-MN-menu*)
  (remove-param-editor (car (subviews self)))
  (if (pw-object self)
    (update-control (first (pw-controls (pw-object self))))))

(defmethod view-activate-event-handler ((self C-chordbox-Win))
  (let ((editor (car (editor-objects (car (subviews self))))))
    (call-next-method)
    (when (pw-object self)
      (setf (chord-line editor)
            (chord-line (first (pw-controls (pw-object self)))))
      (unless (zone-selection? editor) (update-view-controler (car (subviews self))))
      )))

(defmethod view-key-event-handler ((self C-chordbox-Win) char)
  (let* ((editor (car (subviews self)))
         (ctrl (param-ctrl editor)))
    (if (member ctrl (subviews editor))
      (if (eql char #\Newline)
        (exit-from-param-ctrl ctrl)
        (view-key-event-handler ctrl char))
      (call-next-method))))

(defun make-MN-editor-chord-box-M (staff)
   (make-music-notation-editor 'C-chordbox-Win
                               'C-chord-mus-not-view 'C-MN-panel-ChordBox
                               (make-point 230 200) staff))
