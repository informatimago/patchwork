;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               midi.lisp
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
;;;;  Assayag-Agon
;;;;  © 1986-1996 IRCAM 
;;;;
;;;;=========================================================

;; =============================================================================-======
;;                MIDI.Lisp
;; =============================================================================-======
(defpackage "MIDI"
  (:use "COMMON-LISP" "MCLGUI")
  (:export "MIDI-OPEN" "MIDI-CLOSE" "MIDI-WRITE" "MIDI-WRITE-TIME"
           "MIDI-WRITE-NOW" "MIDI-READ" "CLOCK-TIME" "MIDI-CLEAR"))
(in-package "MIDI")

;;(proclaim '(optimize (speed 3) (safety 0) (space 1)))

;;;;Global vars

(defvar *midi-share?* nil)  ;Is MidiShare present
(defvar *pw-refnum* nil)    ;Identifier for PatchWork
(defvar *player* NIL)           ;For play



(defun midi-new-filter (&key chan port type)
  (declare (ignore chan port type))
  (niy midi-new-filter chan port type)
  ;; (let ((f (ui::make-record :tfilter)))
  ;;   (cond ((eq chan t) (dotimes (i 16) (midishare::acceptchan f i t)))
  ;;         ((numberp chan) (midishare::acceptchan f chan t))
  ;;         (t (dolist (i chan) (midishare::acceptchan f i t))))
  ;;   (cond ((eq type t) (dotimes (i 256) (midishare::accepttype f i t)))
  ;;         ((numberp type) (midishare::accepttype f type t))
  ;;         (t (dolist (i type) (midishare::accepttype f i t))))
  ;;   (cond ((eq port t) (dotimes (i 256) (midishare::acceptport f i t)))
  ;;         ((numberp port) (midishare::acceptport f port t))
  ;;         (t (dolist (i port) (midishare::acceptport f i t))))
  ;;   f)
  )


(defmethod midi-free-filter ((f t #|ui::macptr|#))
  (declare (ignorable f))
  (niy midi-free-filter f)
  ;; (unless (ui::%null-ptr-p f)
  ;;   (ui::dispose-record f))
  )


(defmethod midi-modify-filter ((f t #|ui::macptr|#) &key accept chan port type)
  (declare (ignorable f) (ignorable accept chan port type))
  (niy midi-modify-filter f accept chan port type)
  ;; (unless (ui::%null-ptr-p f)
  ;;   (cond ((eq chan t) (dotimes (i 16) (midishare::acceptchan f i accept)))
  ;;         ((numberp chan) (midishare::acceptchan f chan accept))
  ;;         (t (dolist (i chan) (midishare::acceptchan f i accept))))
  ;;   (cond ((eq type t) (dotimes (i 256) (midishare::accepttype f i accept)))
  ;;         ((numberp type) (midishare::accepttype f type accept))
  ;;         (t (dolist (i type) (midishare::accepttype f i accept))))
  ;;   (cond ((eq port t) (dotimes (i 256) (midishare::acceptport f i accept)))
  ;;         ((numberp port) (midishare::acceptport f port accept))
  ;;         (t (dolist (i port) (midishare::acceptport f i accept)))))
  )

(defvar *filter* nil)
(on-load-and-now init/filter
 (setf *filter* (midi-new-filter :chan t :port t :type t)))



;;;;Open MidiShare and connections
(defun midi-open ()
  (niy midi-open)
  ;; (setf *pw-refnum* nil)
  ;; (setf *player* nil)
  ;; (if (setf *midi-share?* (midishare::midishare))
  ;;   (progn
  ;;     (setf *pw-refnum* (midishare::midiopen "PatchWork"))
  ;;     (setf *filter* (midi-new-filter :chan t :port t :type t))
  ;;     (midishare::midisetfilter *pw-refnum*  *filter*)
  ;;     (midi-modify-filter  *filter*  :accept nil :type t)
  ;;     (midishare::MidiConnect *pw-refnum* 0 t)
  ;;     (midishare::MidiConnect 0 *pw-refnum*  t)
  ;;     (setq  *player* (cl-user::open-player "PatchWorkPlayer"))
  ;;     ))
  (format *error-output* "~&MidiShare not present. PatchWork won't play Midi.~%"))


;;;;Close MidiShare and off the scheduler
(defun midi-close ()
  (niy midi-close)
  ;; (when *pw-refnum*
  ;;   (when *player* (cl-user::closeplayer *player*))
  ;;   (when *filter* (midi-free-filter *filter*))
  ;;   (when *pw-refnum* (midishare::MidiClose *pw-refnum*)))
  )


;;;;MidiWrite
(defun midi-write-time (event time)
  (declare (ignore event time))
  (niy midi-write-time event time)
  ;; (when *pw-refnum*
  ;;   (midishare::MidiSendAt *pw-refnum* event time))p
  )


(defun midi-write (event)
  (midi-write-time event (midishare::MidiGetTime)))


;;;;Midi-read 
(defun midi-read ()
  (niy midi-read)
  ;; (ui::without-interrupts
  ;;  (let ((ev (midishare::MidiGetEv *pw-refnum*)))
  ;;    (while (and ev (= (midishare::type ev ) 10))
  ;;      (setf ev (midishare::MidiGetEv *pw-refnum*)))
  ;;    ev))
  )


;;;;Midi-clear - Flush the MidiShare's events.
(defun midi-clear ()
  (niy midi-clear)
  ;; (midishare::MidiFlushEvs *pw-refnum*)
  )


(defconstant tick (/ internal-time-units-per-second 60))

;;;;clock-time - return the current time of. The time is expressed in ticks.
(defun clock-time ()
  ;; (round (/ (midishare::MidiGetTime) 10))
  (values (truncate (ui::timestamp) (/ ui::+tick-per-second+))))


;;;;utilities, hacks

(defun midi-notes-off () ;???
  (niy midi-notes-off)
  ;;(dotimes (chan 16)
  ;;  (midi-write (make-midievent #xb chan #x7b 0)))
  )

(defun midi-reset ()
  (niy midi-reset)
  ;;(midishare::MidiFlushEvs *pw-refnum*)
  ;;(midi-notes-off)
  )

(on-quit midi-close)
(on-load-and-now init/midi (midi-open))

;;;; THE END ;;;;

