;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               midirecord.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;
;;;;    Implement the midi file patch boxes.
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
;;;;    Copyright Pascal J. Bourguignon 2014 - 2020
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

(defvar *pw-recording-midi* nil)
(defvar *recording-midi-seq* nil)
(defvar *midi-tempo* 1000000)     ; noire = 60 en microsecondes

(defclass C-patch-Midi-Extern (C-pw-resize-x)
  ((popUpBox :initform nil :accessor popUpBox)
   (out-type :initform :notes :accessor out-type)
   (outseq :initform nil :accessor outseq)))

(defmethod resize-patch-box ((self C-patch-Midi-Extern) mp delta)
  (declare (ignore mp delta))
  (call-next-method)
  (set-view-position  (popUpBox self) (make-point (- (w self) 10)
                                                  (- (h self) 14))))

(defmethod initialize-instance :after ((self C-patch-Midi-Extern) &key ctrl)
  (declare (ignore ctrl))
  (setf (popUpBox self)
        (make-popUpbox  "A" self
                        (new-menu " "
                                  (new-leafmenu "Chord"
                                                (lambda () (setf (out-type self) :chord)))
                                  (new-leafmenu "Chord-line"
                                                (lambda () (setf (out-type self) :c-l))))
                       :view-position (make-point (- (w self) 10)
                                                  (- (h self) 14))
                       :view-container self
                       :view-font *patchwork-font-spec*)))

;;====Record

(defclass C-patch-record (C-patch-Midi-Extern)
  ())

(defvar *pw-recorder* -1)

(defmethod patch-value ((self C-patch-record) obj)
  (declare (ignore obj))
  (let ((delta (patch-value (first (input-objects self)) (first (input-objects self)))))
    (case (out-type self)
      (:chord (midiseq2chord (outseq self)))
      (:c-l (midiseq2cl (outseq self) delta)))))

(defmethod play ((self C-patch-record))
  (unless *pw-recording-midi*
    (when (and patchwork.midi:*pw-refnum* patchwork.midi:*player* )
      (print "Recording...")
      (setf *pw-recorder* (patchwork.midi:open-player "PatchWorkRecorder"))
      (setf *pw-recording-midi* nil)
      (when (> *pw-recorder* 0)
        (setf *pw-recording-midi* t)
        (patchwork.midi:recordplayer *pw-recorder* 1)
        (patchwork.midi:startplayer *pw-recorder*)))))

;; (patchwork.midi:closeplayer patchwork.midi:*player*)
;; (setf patchwork.midi:*player* (patchwork.midi:open-player "PatchWorkPlayer"))

(defmethod stop-play ((self c-patch-record))
  (when *pw-recording-midi*
    (print "Recording Off...")
    (patchwork.midi:stopplayer *pw-recorder*)
    (let (recording-seq )
      (setf recording-seq (patchwork.midi:getAllTrackplayer *pw-recorder*))
      (when recording-seq
        (setf *midi-tempo* 1000000)
        (setf (outseq self) (mievents2midilist recording-seq 1000))))
    (patchwork.midi:closeplayer *pw-recorder*))
  (setf *pw-recording-midi* nil))

(defunp patchwork.midi:Midi-Record ((delta fix/fl/list (:value 0 :min-val 0 :max-val 1000)))  nil
 "Midi sequences recorder.

Select the box and type 'p' to enter the record mode.
Select the box and type 's' to stop the recording.

Output Menu:

if 'chord' all the recorded notes will be collected into a chord. Output is a chord object.

if 'chord line' a sequence will be recorded. Notes falling in a time interval of 'delta'
 will be considered as chords.delta is expressed in 1/100secs. Output is a chord-line object.

Once the recording is done, the box may be evaluated as many time as required with different
delta and output mode."
  (declare (ignore delta))
  ;; TODO
  )

;;====Load & Save

;; From the menu to add patch items, C-patch-load/save-midi is instanciated
;; with a pw-function set to patchwork.midi:Midi-Load/Save
;;
;; The pw-function is called by the patch-value method of (c-patch).

;;====Save

(defclass C-patch-save-midi (C-pw-resize-x)
  ())

(defunp patchwork.midi:Midi-Save ((objs list (:value '() :type-list ())))  nil
    "MidiFile saver.

Once evaluated, issues a choose-file-dialog that lets you name a MidiFile.
Generates a single-track MidiFile with tempo QuarterNote = 60 and QuarterNote resolution = 480.
Handles correctly micro-intervals on different channels, so playing from PatchWork or
from your favorite sequencer is the same.

Input may be any PatchWork object that could be played through play-object
\(chord, chord-line, measure-line (rtm) , or a list of such)."
  (let ((path (ui:on-main-thread/sync (choose-new-file-dialog :prompt "Save a MIDI file"
                                                              :button-string "Save MIDI file"))))
    (if path
      (let ((path       (make-pathname :type "midi" :defaults path))
            (tempo-evnt (patchwork.midi:MidiNewEv patchwork.midi:typetempo)))
        (patchwork.midi:date tempo-evnt 0)
        (patchwork.midi:field tempo-evnt 0 1000000)
        (patchwork.midi:with-temporary-midi-file-infos (myInfo)
          (patchwork.midi:mf-format  myInfo  1)
          (patchwork.midi:mf-timedef myInfo 0)
          (patchwork.midi:mf-clicks  myInfo 500)
          (patchwork.midi:mf-tracks  myInfo 2)
          (let ((recording-seq (patchwork.midi:midinewseq)))
            (setf *MidiShare-start-time* 0)
            (dolist (object (if (listp objs) objs (list objs)))
              (setf *play-chseq-w/offset* t)
              (MidiPlay object 0 (compute-approx) 0 recording-seq 500))
            (patchwork.midi:link tempo-evnt (patchwork.midi:firstEv recording-seq))
            (patchwork.midi:firstEv recording-seq tempo-evnt)
            (patchwork.midi:midi-file-save path recording-seq  myInfo)
            (set-mac-file-type path :|Midi|)
            (patchwork.midi:midifreeseq recording-seq)
            recording-seq)))
      (error "Midi Save Canceled"))))

;;====Load

(defclass C-patch-load-midi (C-pw-resize-x)
  ())

(defun load-midi-file (path &optional (delta 0))
  (patchwork.midi:with-temporary-midi-file-infos (myInfo)
    (let ((recording-seq (patchwork.midi:midiNewSeq))
          (*print-circle* t))
      (patchwork.midi:midi-file-load path recording-seq myInfo)
      (format t "~&MIDI file     ~S~%" path)
      (format t "   clicks     ~S~%" (patchwork.midi:mf-clicks myInfo))
      (format t "   tracks     ~S~%" (patchwork.midi:mf-tracks myInfo))
      (format t "   MidiFormat ~S~%" (patchwork.midi:mf-format myInfo))
      ;; (format t "   recording-seq ~S~%" recording-seq)
      (finish-output)
      (let ((rep (let ((*midi-tempo* 1000000))
                   (mievents2midilist recording-seq (patchwork.midi:mf-clicks myInfo)))))
        (midiseq2cl rep delta)))))

(defunp patchwork.midi:Midi-Load ((delta fix/fl/list (:value 0 :min-val 0 :max-val 1000)))  nil
    "MidiFile loader.

Once evaluated, issues a choose-file-dialog that lets you select a MidiFile.
Any Midifile format, any number of tracks is allowed. Tempo and tempo change
is recognized. Channels are kept.

Notes falling in a time interval of 'delta' will be considered as chords.
delta is expressed in 1/100secs.

Output is a chord-line object.
"
  (let ((path (ui:on-main-thread/sync (choose-file-dialog
                                       ;; :directory ui::*default-directory*
                                       ;; :file-types '("midi" "kar" "MID")
                                       :prompt "Open a MIDI file"))))
    (if path
        (load-midi-file path delta)
        (error "Midi Load Canceled"))))


(defun logical-time (abstract-time cur-tempo tempo-change-abst-time tempo-change-log-time unit/sec)
  (+ tempo-change-log-time
     (round (* (/ 100.0 unit/sec)
               (- abstract-time tempo-change-abst-time)
               (/ cur-tempo *midi-tempo*)))))

(defun mievents2midilist (seq units/sec)
  (let* ((rep                    '())
         (event                  (patchwork.midi:firstEv seq))
         (initdate               (patchwork.midi:date event))
         (cur-tempo              *midi-tempo*)
         (tempo-change-abst-time 0)
         (tempo-change-log-time  0))
    (while (not (patchwork.midi:null-event-p event))
      (let ((date (- (patchwork.midi:date event) initdate)))
        (case (patchwork.midi:evtype event)
          (144  (unless *pw-recording-midi*
                  (setf tempo-change-log-time (logical-time date cur-tempo
                                                            tempo-change-abst-time tempo-change-log-time
                                                            units/sec)
                        cur-tempo (patchwork.midi:tempo event)
                        tempo-change-abst-time date)))
          (0
           (push (list (* 100 (patchwork.midi:pitch event))
                       (convert-time (patchwork.midi:dur event) units/sec)
                       (patchwork.midi:vel event)
                       (1+ (patchwork.midi:chan event))
                       (convert-time date units/sec))
                 rep))
          (1
           (if (= (patchwork.midi:vel event) 0)
               (close-notes-on rep
                               (* 100 (patchwork.midi:pitch event))
                               (1+ (patchwork.midi:chan event))
                               (logical-time date  cur-tempo
                                             tempo-change-abst-time tempo-change-log-time
                                             units/sec))
               (push (list (* 100 (patchwork.midi:pitch event))
                           (logical-time date   cur-tempo
                                         tempo-change-abst-time tempo-change-log-time
                                         units/sec)
                           (patchwork.midi:vel event)
                           (1+ (patchwork.midi:chan event))
                           (logical-time date  cur-tempo
                                         tempo-change-abst-time tempo-change-log-time
                                         units/sec))
                     rep)))
          (2
           (close-notes-on rep
                           (* 100 (patchwork.midi:pitch event))
                           (1+ (patchwork.midi:chan event))
                           (logical-time date cur-tempo tempo-change-abst-time tempo-change-log-time  units/sec)))))
      (setf event (patchwork.midi:link event)))
    (patchwork.midi:MidiFreeSeq seq)
    (reverse rep)))

(defun close-notes-on (list pitch chan data)
  (flet ((match (x) (and (equal (first x) pitch) (equal (fourth x) chan))))
    (let ((pos (position-if #'match list)))
      (when pos
        (setf (nth 1 (nth pos list))  (- data (nth 1 (nth pos list))))))))

(defun midiseq2chord (list)
  (make-instance 'c-chord
                 :t-time 0
                 :notes (mapcar (lambda (note)
                                  (make-instrument-note (first note) (second note) (fourth note) (third note) nil))
                                list)))

(defun midiseq2cl (list delta)
  (make-instance 'pw::c-chord-line
                 :chords (make-quanti-chords list delta)))

(defun mk-chord-at (t-time  pitch-list dur-list offset-list vel-list chan-list)
  (let ((chord (mk-chord  pitch-list dur-list offset-list vel-list chan-list)))
    (setf (t-time chord) t-time)
    chord))

(defun make-quanti-chords (note-list delta)
  (loop :while note-list
        :for note := (first note-list)
        :with pitch-list :and dur-list :and vel-list :and  chan-list :and offset-list
        :with base-time := (fifth (first note-list))
        :if (<= (- (fifth note) base-time) delta)
          :do
             (push (first note) pitch-list)
             (push (second note) dur-list)
             (push (third note) vel-list)
             (push (fourth note) chan-list)
             (push (- (fifth note) base-time) offset-list)
             (pop note-list)
        :else
          :collect (mk-chord-at base-time  pitch-list dur-list offset-list vel-list chan-list) into result
          :and :do (setf base-time (fifth note) pitch-list () dur-list () vel-list ()  chan-list () offset-list ())
        :finally (return (append result (list  (mk-chord-at base-time  pitch-list dur-list offset-list vel-list chan-list))))))


;;;; THE END ;;;;
