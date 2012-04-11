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
(defpackage "MIDI" (:use "COMMON-LISP"))
(in-package "MIDI")

(export
 '(midi-open midi-close midi-write midi-write-time midi-write-now   
   midi-read clock-time midi-clear))

;;;;Load  MidiShare Library

;(load "PW:PW-lib;MidiShare;MidiShare")
;(load "PW:PW-lib;MidiShare;PlayerPPC")

(in-package "MIDI")
;(proclaim '(optimize (speed 3) (safety 0) (space 1)))

;;;;Global vars

(defvar *midi-share?* nil)  ;Is MidiShare present
(defvar *pw-refnum* nil)    ;Identifier for PatchWork
(defvar *player* NIL)           ;For play
  


(defun midi-new-filter (&key chan port type)
  (let ((f (ccl::make-record :tfilter)))
    (cond ((eq chan t) (dotimes (i 16) (midishare::acceptchan f i t)))
          ((numberp chan) (midishare::acceptchan f chan t))
          (t (dolist (i chan) (midishare::acceptchan f i t))))
    (cond ((eq type t) (dotimes (i 256) (midishare::accepttype f i t)))
          ((numberp type) (midishare::accepttype f type t))
          (t (dolist (i type) (midishare::accepttype f i t))))
    (cond ((eq port t) (dotimes (i 256) (midishare::acceptport f i t)))
          ((numberp port) (midishare::acceptport f port t))
          (t (dolist (i port) (midishare::acceptport f i t))))
    f))


(defmethod midi-free-filter ((f ccl::macptr))
  (unless (ccl::%null-ptr-p f)
    (ccl::dispose-record f)))


(defmethod midi-modify-filter ((f ccl::macptr) &key accept chan port type)
  (unless (ccl::%null-ptr-p f)
    (cond ((eq chan t) (dotimes (i 16) (midishare::acceptchan f i accept)))
          ((numberp chan) (midishare::acceptchan f chan accept))
          (t (dolist (i chan) (midishare::acceptchan f i accept))))
    (cond ((eq type t) (dotimes (i 256) (midishare::accepttype f i accept)))
          ((numberp type) (midishare::accepttype f type accept))
          (t (dolist (i type) (midishare::accepttype f i accept))))
    (cond ((eq port t) (dotimes (i 256) (midishare::acceptport f i accept)))
          ((numberp port) (midishare::acceptport f port accept))
          (t (dolist (i port) (midishare::acceptport f i accept))))))

(defvar *filter* nil)

;(ccl::def-load-pointers init-filter () 
;  (setf *filter* 
;        (midi-new-filter :chan t :port t 
;                         :type t)))



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
      (midishare::MidiConnect *pw-refnum* 0 t)
      (midishare::MidiConnect 0 *pw-refnum*  t)
      (setq  *player* (cl-user::open-player "PatchWorkPlayer"))
      )
    (print "MidiShare not present. PatchWork won't play Midi.")))

;;;;Close MidiShare and off the scheduler
(defun midi-close ()
  (when *pw-refnum*
    (when *player* (cl-user::closeplayer *player*))
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
  (ccl::without-interrupts
   (let ((ev (midishare::MidiGetEv *pw-refnum*)))
     (while (and ev (= (midishare::type ev ) 10))
       (setf ev (midishare::MidiGetEv *pw-refnum*)))
     ev)))

  
;;;;Midi-clear - Flush the MidiShare's events.
(defun midi-clear ()
  (midishare::MidiFlushEvs *pw-refnum*))


;;;;clock-time - return the current time of. The time is expressed in ticks.
(defun clock-time () 
   (round (/ (midishare::MidiGetTime) 10)))

  
;;;;utilities, hacks


(defun midi-notes-off () ;???
  ;(dotimes (chan 16)
  ;  (midi-write (make-midievent #xb chan #x7b 0)))
)

(defun midi-reset ()
  (midishare::MidiFlushEvs *pw-refnum*)
  ;(midi-notes-off)
)

(push #'midi-close CCL:*lisp-cleanup-functions*)
(ccl::def-load-pointers startup-midi () (midi-open))

;(proclaim '(optimize (speed 1) (safety 1) (space 1)))

