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
  (:use "COMMON-LISP")
  (:export "MIDI-OPEN" "MIDI-CLOSE" "MIDI-WRITE" "MIDI-WRITE-TIME"
           "MIDI-WRITE-NOW" "MIDI-READ" "CLOCK-TIME" "MIDI-CLEAR"))
(in-package "MIDI")

;;(proclaim '(optimize (speed 3) (safety 0) (space 1)))

;;;;Global vars

(defvar *midi-share?* nil)  ;Is MidiShare present
(defvar *pw-refnum* nil)    ;Identifier for PatchWork
(defvar *player* NIL)           ;For play
  


(defun midi-new-filter (&key chan port type)
  (warn "~S is not implemented" 'midi-new-filter)
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
  (warn "~S is not implemented" 'midi-free-filter)
  ;; (unless (ui::%null-ptr-p f)
  ;;   (ui::dispose-record f))
  )


(defmethod midi-modify-filter ((f t #|ui::macptr|#) &key accept chan port type)
  (warn "~S is not implemented" 'midi-free-filter)
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

;;(ui::def-load-pointers init-filter () 
;;  (setf *filter* 
;;        (midi-new-filter :chan t :port t 
;;                         :type t)))



;;;;Open MidiShare and connections
(defun midi-open ()
  (warn "~S is not implemented" 'midi-open)
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
  ;;     )
  ;; (print "MidiShare not present. PatchWork won't play Midi."))
  (print "MidiShare not present. PatchWork won't play Midi."))


;;;;Close MidiShare and off the scheduler
(defun midi-close ()
  (warn "~S is not implemented" 'midi-close)
  ;; (when *pw-refnum*
  ;;   (when *player* (cl-user::closeplayer *player*))
  ;;   (when *filter* (midi-free-filter *filter*))
  ;;   (when *pw-refnum* (midishare::MidiClose *pw-refnum*)))
  )


;;;;MidiWrite
(defun midi-write-time (event time)
  (warn "~S is not implemented" 'midi-write-time)
  ;; (when *pw-refnum*
  ;;   (midishare::MidiSendAt *pw-refnum* event time))p
  )


(defun midi-write (event)
  (warn "~S is not implemented" 'midi-write)
  ;; (midi-write-time event (midishare::MidiGetTime))
  )


;;;;Midi-read 
(defun midi-read ()
  (warn "~S is not implemented" 'midi-read)
  ;; (ui::without-interrupts
  ;;  (let ((ev (midishare::MidiGetEv *pw-refnum*)))
  ;;    (while (and ev (= (midishare::type ev ) 10))
  ;;      (setf ev (midishare::MidiGetEv *pw-refnum*)))
  ;;    ev))
  )

  
;;;;Midi-clear - Flush the MidiShare's events.
(defun midi-clear ()
  (warn "~S is not implemented" 'midi-clear)
  ;; (midishare::MidiFlushEvs *pw-refnum*)
  )


;;;;clock-time - return the current time of. The time is expressed in ticks.
(defun clock-time () 
  (warn "~S is not implemented" 'clock-time)
   ;; (round (/ (midishare::MidiGetTime) 10))
  1)

  
;;;;utilities, hacks


(defun midi-notes-off () ;???
  (warn "~S is not implemented" 'midi-notes-off)
  ;;(dotimes (chan 16)
  ;;  (midi-write (make-midievent #xb chan #x7b 0)))
)

(defun midi-reset ()
  (warn "~S is not implemented" 'midi-reset)
  ;;(midishare::MidiFlushEvs *pw-refnum*)
  ;;(midi-notes-off)
)

(push #'midi-close ui:*lisp-cleanup-functions*)
(ui::def-load-pointers startup-midi () (midi-open))

;;(proclaim '(optimize (speed 1) (safety 1) (space 1)))

