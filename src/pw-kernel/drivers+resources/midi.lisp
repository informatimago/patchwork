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
;;;;    Assayag-Agon
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-07 <PJB> Changed license to GPL3; Added this header.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright IRCAM 1986 - 1996
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

(defpackage "MIDI"
  (:use "COMMON-LISP" "MCLGUI")
  (:export "MIDI-OPEN" "MIDI-CLOSE" "MIDI-WRITE" "MIDI-WRITE-TIME"
           "MIDI-WRITE-NOW" "MIDI-READ" "CLOCK-TIME" "MIDI-CLEAR"))
(in-package "MIDI")

;;;;Global vars

(defvar *midi-share?* nil)  ;Is MidiShare present
(defvar *pw-refnum* nil)    ;Identifier for PatchWork
(defvar *player* NIL)           ;For play



(defun midi-new-filter (&key chan port type)
  (let ((f (midishare:MidiNewFilter)))
    (cond ((eq chan t) (dotimes (i 16) (midishare::midiacceptchan f i 1)))
          ((numberp chan) (midishare::midiacceptchan f chan 1))
          (t (dolist (i chan) (midishare::midiacceptchan f i 1))))
    (cond ((eq type t) (dotimes (i 256) (midishare::midiaccepttype f i 1)))
          ((numberp type) (midishare::midiaccepttype f type 1))
          (t (dolist (i type) (midishare::midiaccepttype f i 1))))
    (cond ((eq port t) (dotimes (i 256) (midishare::midiacceptport f i 1)))
          ((numberp port) (midishare::midiacceptport f port 1))
          (t (dolist (i port) (midishare::midiacceptport f i 1))))
    f))


(defmethod midi-free-filter ((f t #|ui::macptr|#))
  (declare (ignorable f))
  (unless (midishare:nullptrp f)
    (midishare:midifreefilter f)))


(defmethod midi-modify-filter ((f t #|ui::macptr|#) &key accept chan port type)
  (declare (ignorable f) (ignorable accept chan port type))
  (unless (midishare:nullptrp f)
    (let ((accept (if accept 1 0)))
     (cond ((eq chan t) (dotimes (i 16) (midishare::midiacceptchan f i accept)))
           ((numberp chan) (midishare::midiacceptchan f chan accept))
           (t (dolist (i chan) (midishare::midiacceptchan f i accept))))
      (cond ((eq type t) (dotimes (i 256) (midishare::midiaccepttype f i accept)))
            ((numberp type) (midishare::midiaccepttype f type accept))
            (t (dolist (i type) (midishare::midiaccepttype f i accept))))
      (cond ((eq port t) (dotimes (i 256) (midishare::midiacceptport f i accept)))
            ((numberp port) (midishare::midiacceptport f port accept))
            (t (dolist (i port) (midishare::midiacceptport f i accept)))))))

(defvar *filter* nil)
(on-load-and-now init/filter
 (setf *filter* (midi-new-filter :chan t :port t :type t)))



;;;;Open MidiShare and connections
(defun midi-open ()
  (setf *pw-refnum* nil)
  (setf *player* nil)
  (if (setf *midi-share?* (midishare::midishare))
      (progn
        (setf *pw-refnum* (midishare::midiopen "PatchWork"))
        (setf *filter* (midi-new-filter :chan t :port t :type t))
        (midishare::midisetfilter *pw-refnum*  *filter*)
        (midi-modify-filter  *filter*  :accept nil :type t)
        (midishare::MidiConnect *pw-refnum* 0 1)
        (midishare::MidiConnect 0 *pw-refnum* 1)
        (setq  *player* (midi-player::open-player "PatchWorkPlayer"))
        (values *pw-refnum* *player*))
      (format *error-output* "~&MidiShare not present. PatchWork won't play Midi.~%")))


;;;;Close MidiShare and off the scheduler
(defun midi-close ()
  (when *pw-refnum*
    (when *player* (midi-player::closeplayer *player*))
    (when *filter* (midi-free-filter *filter*))
    (when *pw-refnum* (midishare::MidiClose *pw-refnum*))))


;;;;MidiWrite
(defun midi-write-time (event time)
  (when *pw-refnum*
    (midishare::MidiSendAt *pw-refnum* event time)))


(defun midi-write (event)
  (midi-write-time event (midishare::MidiGetTime)))


;;;;Midi-read 
(defun midi-read ()
  (ui::without-interrupts
   (let ((ev (midishare::MidiGetEv *pw-refnum*)))
     (loop :while (and ev (not (midishare:nullptrp ev)) (= (midishare::evtype ev) 10)) :do
       (setf ev (midishare::MidiGetEv *pw-refnum*)))
     (unless (midishare:nullptrp ev)
       ev))))


;;;;Midi-clear - Flush the MidiShare's events.
(defun midi-clear ()
  (midishare::MidiFlushEvs *pw-refnum*))


(defconstant tick (/ internal-time-units-per-second 60))

;;;;clock-time - return the current time of. The time is expressed in ticks.
(defun clock-time ()
  (values (round (/ (midishare::MidiGetTime) 10))) 
  ;; (values (truncate (ui::timestamp) (/ ui::+tick-per-second+)))
  )


;;;;utilities, hacks

(defun make-midievent (type channel arg1 arg2)
  (let ((ev (midishare:midinewev type)))
    (unless (midishare:nullptrp ev)
      (midishare:chan ev channel)
      (midishare:port ev 0)
      (midishare:field ev 0 arg1)
      (midishare:field ev 1 arg2))
    ev))

(defun midi-notes-off () ;???
  (dotimes (chan 16)
   (midi-write (make-midievent midishare:typeKeyOff #| #xb |#
                               chan #x7b 0))))

(defun midi-reset ()
  (midishare::MidiFlushEvs *pw-refnum*)
  (midi-notes-off))

(on-quit midi-close)
(on-load-and-now init/midi (midi-open))

;;;; THE END ;;;;

