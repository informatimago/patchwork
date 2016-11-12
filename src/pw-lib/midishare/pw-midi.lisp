;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               midi.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;  
;;;;    This package exports an interface to the midi bus using MIDISHARE.
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

(in-package "PATCHWORK.MIDI")



;;;---------------------------------------------------------------------
;;; Patchwork midi stuff
;;;---------------------------------------------------------------------

;;;;Global vars

(defvar *midi-share?* nil)  ; Is MidiShare present
(defvar *player*      nil)  ; For play
(defvar *pw-refnum*   nil)  ; Identifier for PatchWork
(defvar *filter*      nil)



(defun midi-new-filter (&key chan port type)
  (let ((f (MidiNewFilter)))
    (cond ((eql chan t) (dotimes (i 16) (midiacceptchan f i 1)))
          ((numberp chan) (midiacceptchan f chan 1))
          (t (dolist (i chan) (midiacceptchan f i 1))))
    (cond ((eql type t) (dotimes (i 256) (midiaccepttype f i 1)))
          ((numberp type) (midiaccepttype f type 1))
          (t (dolist (i type) (midiaccepttype f i 1))))
    (cond ((eql port t) (dotimes (i 256) (midiacceptport f i 1)))
          ((numberp port) (midiacceptport f port 1))
          (t (dolist (i port) (midiacceptport f i 1))))
    f))


(defmethod midi-free-filter ((f t #|ui::macptr|#))
  (declare (ignorable f))
  (unless (nullptrp f)
    (midifreefilter f)))


(defmethod midi-modify-filter ((f t #|ui::macptr|#) &key accept chan port type)
  (declare (ignorable f) (ignorable accept chan port type))
  (unless (nullptrp f)
    (let ((accept (if accept 1 0)))
     (cond ((eql chan t) (dotimes (i 16) (midiacceptchan f i accept)))
           ((numberp chan) (midiacceptchan f chan accept))
           (t (dolist (i chan) (midiacceptchan f i accept))))
      (cond ((eql type t) (dotimes (i 256) (midiaccepttype f i accept)))
            ((numberp type) (midiaccepttype f type accept))
            (t (dolist (i type) (midiaccepttype f i accept))))
      (cond ((eql port t) (dotimes (i 256) (midiacceptport f i accept)))
            ((numberp port) (midiacceptport f port accept))
            (t (dolist (i port) (midiacceptport f i accept)))))))




;;;;Open MidiShare and connections
(defun midi-open ()
  (midi-close)
  (setf *pw-refnum* nil)
  (setf *player* nil)
  (if (setf *midi-share?* (midishare))
      (progn
        (setf *pw-refnum* (midiopen "PatchWork"))
        (setf *filter* (midi-new-filter :chan t :port t :type t))
        (midisetfilter *pw-refnum*  *filter*)
        (midi-modify-filter  *filter*  :accept nil :type t)
        (MidiConnect *pw-refnum* 0 1)
        (MidiConnect 0 *pw-refnum* 1)
        (setq  *player* (midi-player::open-player "PatchWorkPlayer"))
        (values *pw-refnum* *player*))
      (format *error-output* "~&MidiShare not present. PatchWork won't play Midi.~%")))


;;;;Close MidiShare and off the scheduler
(defun midi-close ()
  (when *pw-refnum*
    (when *player* (midi-player::closeplayer *player*))
    (when *filter* (midi-free-filter *filter*))
    (when *pw-refnum* (MidiClose *pw-refnum*))))


;;;;MidiWrite
(defun midi-write-time (event time)
  (when *pw-refnum*
    (MidiSendAt *pw-refnum* event time)))


(defun midi-write (event)
  (midi-write-time event (MidiGetTime)))


;;;;Midi-read 
(defun midi-read ()
  (ui::without-interrupts
   (let ((ev (MidiGetEv *pw-refnum*)))
     (loop :while (and ev (not (nullptrp ev)) (= (evtype ev) 10)) :do
       (setf ev (MidiGetEv *pw-refnum*)))
     (unless (nullptrp ev)
       ev))))


;;;;Midi-clear - Flush the MidiShare's events.
(defun midi-clear ()
  (MidiFlushEvs *pw-refnum*))


(defconstant tick (/ internal-time-units-per-second 60))

;;;;clock-time - return the current time of. The time is expressed in ticks.
(defun clock-time ()
  (values (round (/ (MidiGetTime) 10))) 
  ;; (values (truncate (ui::timestamp) (/ ui::+tick-per-second+)))
  )


;;;;utilities, hacks

(defun make-midievent (type channel arg1 arg2)
  (let ((ev (midinewev type)))
    (unless (nullptrp ev)
      (chan ev channel)
      (port ev 0)
      (field ev 0 arg1)
      (field ev 1 arg2))
    ev))

(defun midi-notes-off () ;???
  (dotimes (chan 16)
   (midi-write (make-midievent typeKeyOff #| #xb |#
                               chan #x7b 0))))

(defun midi-reset ()
  (MidiFlushEvs *pw-refnum*)
  (midi-notes-off))






(defmacro define-with-temporary-macro (name initialization &optional finalization)
  "Defines a macro named NAME that takes a variable name and a body as
parameters, and expands to a LET form binding that variable to the
result of the INITIALIZATION form while executing the body.  If the
FINALIZATION is given then the body is wrapped in a UNWIND-PROTECT,
and the FINALIZATION is the clean-up form."
  (let ((vvar  (gensym "var"))
        (vbody (gensym "body")))
    `(defmacro ,name ((,vvar) &body ,vbody)
       ,(if finalization
            `(list 'ccl:rlet (list (list ,vvar ',initialization))
                  (list 'unwind-protect
                        (cons 'progn ,vbody)
                        ',finalization))
            `(list* 'ccl:rlet (list (list ,vvar ',initialization))
                    ,vbody)))))


(define-with-temporary-macro with-temporary-player-state    :<P>layer<S>tate)
(define-with-temporary-macro with-temporary-midi-file-infos :<M>idi<F>ile<I>nfos)


;;;---------------------------------------------------------------------

(on-load-and-now init/filter
 (setf *filter* (midi-new-filter :chan t :port t :type t)))
(on-quit midi-close)
(on-load-and-now init/midi (midi-open))

;;;; THE END ;;;;

