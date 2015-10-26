;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               midirecord.lisp
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
  (declare (ignore delta)))



;;====Save

(defclass C-patch-save-midi (C-pw-resize-x)
  ())

#|
(defmethod patch-value ((self C-patch-save-midi) obj)
  (declare (ignore obj))
  (when (and  patchwork.midi:*pw-refnum* patchwork.midi:*player* )
    (let ((name (CHOOSE-NEW-FILE-DIALOG)))
      (when name
        (let ((tempo-evnt (patchwork.midi:MidiNewEv patchwork.midi:typetempo))
              recording-seq)
          (patchwork.midi:date tempo-evnt 0)
          (patchwork.midi:field tempo-evnt 0 1000000)
          (ccl:rlet ((myInfo :<M>idi<F>ile<I>nfos))  
            (patchwork.midi:mf-format myInfo  1)
            (patchwork.midi:mf-timedef myInfo 0)
            (patchwork.midi:mf-clicks myInfo 480)
            (patchwork.midi:mf-tracks myInfo 2)
            (setf recording-seq (MidiSaveAny (patch-value (first (input-objects self)) (first (input-objects self)))))
            (patchwork.midi:link tempo-evnt (patchwork.midi:firstEv recording-seq) )
            (patchwork.midi:firstEv recording-seq tempo-evnt)
            (patchwork.midi:midi-file-save name recording-seq  myInfo)
            (set-mac-file-type name :|Midi|)
            (patchwork.midi:midifreeseq recording-seq)))))))

(defmethod MidiSaveAny ((object t))
  (when (and  patchwork.midi:*pw-refnum* patchwork.midi:*player* )
    (setf *play-chseq-w/offset* t)
    (let ((seq (patchwork.midi:midinewseq)))
      (setf *MidiShare-start-time* 0)
      (setf *play-chseq-w/offset* t)
      (MidiPlay object 0 (compute-approx) 0 seq 480)
      seq)))

;;changed from paw-modifs 140397 aaa
|#

(defgeneric save-midi (patch name))
(defmethod save-midi ((self c-patch-save-midi) name)
  (let ((tempo-evnt (patchwork.midi:MidiNewEv patchwork.midi:typetempo))
        recording-seq)
    (patchwork.midi:date tempo-evnt 0)
    (patchwork.midi:field tempo-evnt 0 1000000)
    (patchwork.midi:with-temporary-midi-file-infos (myInfo)  
      (patchwork.midi:mf-format myInfo  1)
      (patchwork.midi:mf-timedef myInfo 0)
      (patchwork.midi:mf-clicks myInfo 500)
      (patchwork.midi:mf-tracks myInfo 2)
      (setf recording-seq (MidiSaveAny (patch-value (first (input-objects self)) (first (input-objects self)))))
      (patchwork.midi:link tempo-evnt (patchwork.midi:firstEv recording-seq) )
      (patchwork.midi:firstEv recording-seq tempo-evnt)
      (patchwork.midi:midi-file-save name recording-seq  myInfo)
      (set-mac-file-type name :|Midi|)
      (patchwork.midi:midifreeseq recording-seq))))

(defmethod patch-value ((self C-patch-save-midi) obj)
  (declare (ignore obj))
  (when (and patchwork.midi:*pw-refnum* patchwork.midi:*player*)
    (let ((name (CHOOSE-NEW-FILE-DIALOG)))
      (when name
        (save-midi self (make-pathname :type "midi" :defaults name))))))


(defgeneric MidiSaveAny (object))
(defmethod MidiSaveAny ((object t))
  (when (and patchwork.midi:*pw-refnum* patchwork.midi:*player*)
    (setf *play-chseq-w/offset* t)
    (let ((seq (patchwork.midi:midinewseq)))
      (setf *MidiShare-start-time* 0)
      (setf *play-chseq-w/offset* t)
      (MidiPlay object 0 (compute-approx) 0 seq 500)
      seq)))
          

(defunp patchwork.midi:Midi-Save ((objs list (:value '() :type-list ())))  nil 
        "MidiFile saver.

Once evaluated, issues a choose-file-dialog that lets you name a MidiFile.
Generates a single-track MidiFile with tempo QuarterNote=60 and QuarterNote resolution = 480.
Handles correctly micro-intervals on different channels, so playing from PatchWork or
from your favorite sequencer is the same.

Input may be any PatchWork object that could be played through play-object
\(chord, chord-line, measure-line (rtm) , or a list of such)"
  (declare (ignore objs)))


;;====Load

(defclass C-patch-load-midi (C-pw-resize-x)
  ())

(defgeneric load-midi (patch name))
(defmethod load-midi ((self C-patch-load-midi) name)
  (let ((recording-seq (patchwork.midi:midiNewSeq))
        (delta (patch-value (first (input-objects self)) (first (input-objects self))))
        rep)
    (patchwork.midi:with-temporary-midi-file-infos (myInfo)   
      (patchwork.midi:midi-file-load name recording-seq  myInfo)
      (when recording-seq
        (print (list  "clicks" (patchwork.midi:mf-clicks myInfo)
                      "tracks" (patchwork.midi:mf-tracks myInfo)
                      "MidiFormat" (patchwork.midi:mf-format myInfo)))
        (let ((*midi-tempo* 1000000))
          (setf rep (mievents2midilist recording-seq (patchwork.midi:mf-clicks myInfo) )))
        (midiseq2cl rep delta)))))


(defmethod patch-value ((self C-patch-load-midi) obj)
  (declare (ignore obj))
  (when (and  patchwork.midi:*pw-refnum* patchwork.midi:*player* )
    (let ((name (CHOOSE-FILE-DIALOG)))
      (when name
        (load-midi self name)))))

#|
(defun logical-time (abstract-time cur-tempo tempo-change-abst-time tempo-change-log-time unit/sec)
  (+ tempo-change-log-time
     (round (* (/ 100.0 unit/sec) 
               (* (- abstract-time tempo-change-abst-time)
                  (/ cur-tempo *midi-tempo*))))))


(defun mievents2midilist (seq units/sec)
  (when (and  patchwork.midi:*pw-refnum* patchwork.midi:*player* )
    (let (event date initdate rep
                (cur-tempo *midi-tempo*)
                (tempo-change-abst-time 0)
                (tempo-change-log-time 0))
      (setf event (patchwork.midi:firstEv seq))
      (setf initdate (patchwork.midi:date event))
      (while (not (patchwork.midi:null-event-p event))
        (setf date (- (patchwork.midi:date event) initdate))
        (case (patchwork.midi:evtype event)
          (144  (unless *pw-recording-midi* 
                  (setf 
                   tempo-change-log-time (logical-time date cur-tempo tempo-change-abst-time tempo-change-log-time  units/sec)
                   cur-tempo (patchwork.midi:tempo event)
                   tempo-change-abst-time date ) )
           )
          
          (0 
           (push (list (* 100 (patchwork.midi:pitch event))
                       (convert-time (patchwork.midi:dur event) units/sec) 
                       (patchwork.midi:vel event)
                       (1+ (patchwork.midi:chan event))
                       (convert-time date units/sec))
                 rep)
           )
          (1 
           (if (= (patchwork.midi:vel event) 0)
             (close-notes-on rep 
                             (* 100 (patchwork.midi:pitch event)) 
                             (1+ (patchwork.midi:chan event))
                             (logical-time date  cur-tempo tempo-change-abst-time tempo-change-log-time  units/sec))
             (push (list  (* 100 (patchwork.midi:pitch event)) 
                          (logical-time date   cur-tempo tempo-change-abst-time tempo-change-log-time  units/sec)
                          (patchwork.midi:vel event) 
                          (1+ (patchwork.midi:chan event))
                          (logical-time date  cur-tempo tempo-change-abst-time tempo-change-log-time  units/sec))
                   rep))
           )
          (2 
           (close-notes-on rep 
                           (* 100 (patchwork.midi:pitch event)) 
                           (1+ (patchwork.midi:chan event))
                           (logical-time date cur-tempo tempo-change-abst-time tempo-change-log-time  units/sec)
                           )
           ))
        (setf event (patchwork.midi:link event)))
      (patchwork.midi:MidiFreeSeq seq)
      (reverse rep))))
            

(defun close-notes-on (list pitch chan data) 
  (flet ((match (x) (and (equal (first x) pitch) (equal (fourth x) chan))))
    (let ((pos (position-if #'match list)))
      (when pos
        (setf (nth 1 (nth pos list))  (- data (nth 1 (nth pos list))))))))
;;changed from paw-modifs 140397 aaa
|#

(defun logical-time (abstract-time cur-tempo tempo-change-abst-time tempo-change-log-time unit/sec)
  (+ tempo-change-log-time
     (round (* (/ 100.0 unit/sec) 
               (* (- abstract-time tempo-change-abst-time)
                  (/ cur-tempo *midi-tempo*))))))


(defun mievents2midilist (seq units/sec)
  (when (and  patchwork.midi:*pw-refnum* patchwork.midi:*player* )
    (let (event date initdate rep
                (cur-tempo *midi-tempo*)
                (tempo-change-abst-time 0)
                (tempo-change-log-time 0))
      (setf event (patchwork.midi:firstEv seq))
      (setf initdate (patchwork.midi:date event))
      (while (not (patchwork.midi:null-event-p event))
        (setf date (- (patchwork.midi:date event) initdate))
        (case (patchwork.midi:evtype event)
          (144  (unless *pw-recording-midi* 
                  (setf 
                   tempo-change-log-time (logical-time date cur-tempo tempo-change-abst-time tempo-change-log-time  units/sec)
                   cur-tempo (patchwork.midi:tempo event)
                   tempo-change-abst-time date ) )
           )
          
          (0 
           (push (list (* 100 (patchwork.midi:pitch event))
                       (convert-time (patchwork.midi:dur event) units/sec) 
                       (patchwork.midi:vel event)
                       (1+ (patchwork.midi:chan event))
                       (convert-time date units/sec))
                 rep)
           )
          (1 
           (if (= (patchwork.midi:vel event) 0)
             (close-notes-on rep 
                             (* 100 (patchwork.midi:pitch event)) 
                             (1+ (patchwork.midi:chan event))
                             (logical-time date  cur-tempo tempo-change-abst-time tempo-change-log-time  units/sec))
             (push (list  (* 100 (patchwork.midi:pitch event)) 
                          (logical-time date   cur-tempo tempo-change-abst-time tempo-change-log-time  units/sec)
                          (patchwork.midi:vel event) 
                          (1+ (patchwork.midi:chan event))
                          (logical-time date  cur-tempo tempo-change-abst-time tempo-change-log-time  units/sec))
                   rep))
           )
          (2 
           (close-notes-on rep 
                           (* 100 (patchwork.midi:pitch event)) 
                           (1+ (patchwork.midi:chan event))
                           (logical-time date cur-tempo tempo-change-abst-time tempo-change-log-time  units/sec)
                           )
           ))
        (setf event (patchwork.midi:link event)))
      (patchwork.midi:MidiFreeSeq seq)
      (reverse rep))))
            

(defun close-notes-on (list pitch chan data) 
  (flet ((match (x) (and (equal (first x) pitch) (equal (fourth x) chan))))
    (let ((pos (position-if #'match list)))
      (when pos
        (setf (nth 1 (nth pos list))  (- data (nth 1 (nth pos list))))))))


(defun midiseq2chord (list)
  (make-instance 'c-chord
                :t-time 0
                :notes (mapcar (lambda (note)
                                   (make-instrument-note (first note) (second note) (fourth note) (third note) nil)) list)))


(defun midiseq2cl (list delta)
  (make-instance 'pw::c-chord-line 
    :chords 
    (make-quanti-chords list delta)))


(defun mk-chord-at (t-time  pitch-list dur-list offset-list vel-list chan-list)
  (let ((chord (mk-chord  pitch-list dur-list offset-list vel-list chan-list)))
    (setf (t-time chord) t-time)
    chord))

(defun make-quanti-chords (note-list delta)
  (loop while note-list
        for note = (first note-list)
        with pitch-list and dur-list and vel-list and  chan-list and offset-list
        with base-time = (fifth (first note-list))
        if (<= (- (fifth note) base-time) delta)
        do 
        
        (push (first note) pitch-list)
        (push (second note) dur-list)    
        (push (third note) vel-list)
        (push (fourth note) chan-list)
        (push (- (fifth note) base-time) offset-list)
        (pop note-list)
        
        else
        collect (mk-chord-at base-time  pitch-list dur-list offset-list vel-list chan-list) into result
        and do (setf base-time (fifth note) pitch-list () dur-list () vel-list ()  chan-list () offset-list ())
        
        finally (return (append result (list  (mk-chord-at base-time  pitch-list dur-list offset-list vel-list chan-list))))))




   
(defunp patchwork.midi:Midi-Load ((delta fix/fl/list (:value 0 :min-val 0 :max-val 1000)))  nil
 "MidiFile loader.

Once evaluated, issues a choose-file-dialog that lets you select a MidiFile.
Any Midifile format, any number of tracks is allowed. Tempo and tempo change
is recognized. Channels are kept.

Notes falling in a time interval of 'delta' will be considered as chords.
delta is expressed in 1/100secs. 

Output is a chord-line object.
"
  (declare (ignore delta)))



;;;; THE END ;;;;










         



