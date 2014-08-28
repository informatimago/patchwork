;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               midiplay.lisp
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

(defmacro convert-time (time unit/sec)
  `(* (/  *midi-tempo* 1000000) (/ ,time ,unit/sec) 100))

(defmacro convert-time-1 (time unit/sec)
  `(* ,time  (/ ,unit/sec 100 )))


(defvar *MidiShare-start-time*  0)

(defvar *play-chseq-w/offset* nil)

(defclass c-arp-chord (c-chord)
  ())

#|
(defmethod MidiPlayANy ((object t) &optional (approx 2) (chanbase 1))
  (when (and  patchwork.midi:*pw-refnum* patchwork.midi:*player* )
    (let ((playerIdle))
      (ccl:rlet ((myState :<P>layer<S>tate))  
        (patchwork.midi:getStatePlayer patchwork.midi:*player* myState) 
        (when (= (patchwork.midi:s-state myState) kIdle) (setf playerIdle t)))
      (unless playerIdle (print "Wait end of previous play!"))
      (when playerIdle
        (let ((seq (patchwork.midi:midinewseq)))
          (setf *MidiShare-start-time* 0)
          (MidiPlay object 0 approx chanbase seq 1000)
          (patchwork.midi:setalltrackplayer patchwork.midi:*player* seq 480)
          (patchwork.midi:startplayer patchwork.midi:*player*)
          seq)))))
;;changed from paw-modifs 140397 aaa
|#

(defgeneric MidiPlayANy (object &optional approx chanbase))
(defmethod MidiPlayANy ((object t) &optional (approx 2) (chanbase 1))
  (when (and  patchwork.midi:*pw-refnum* patchwork.midi:*player* )
    (let ((playerIdle))
      (patchwork.midi:with-temporary-player-state (mystate)
        (patchwork.midi:getStatePlayer patchwork.midi:*player* myState) 
        (when (= (patchwork.midi:s-state myState) patchwork.midi:kIdle)
          (setf playerIdle t))
        (unless playerIdle
          (print "Wait end of previous play!"))
        (when playerIdle
          (let ((seq (patchwork.midi:midinewseq)))
            (setf *MidiShare-start-time* 0)
            (MidiPlay object 0 approx chanbase seq 1000)
            (patchwork.midi:setalltrackplayer patchwork.midi:*player* seq 500)
            (patchwork.midi:startplayer patchwork.midi:*player*)
            seq))))))

#|
(defmethod MidiPlay ((note c-note) at approx chanbase seq unit/sec)
  (let ((event (patchwork.midi:MidiNewEv patchwork.midi:typeNote)))	; ask for a new note event
    (when (zerop chanbase) (setf chanbase (chan note)))
    (unless (patchwork.midi:nullptrp event)	; if the allocation was succesfull
      (patchwork.midi:chan event    ; set the midi channel to 0 (means channel 1)
                       (1- (+ chanbase
                              (1- (micro-channel (epw::approx-m  (midic note) approx))))))
      (patchwork.midi:port event 0)			; set the destination port to Modem
      (patchwork.midi:field event 0 (truncate (midic note) 100))		; set the pitch field
      (patchwork.midi:field event 1 (round (vel note)))		        ; set the velocity field
      (patchwork.midi:field event 2 (round (convert-time-1 (dur note) unit/sec)))		; set the duration field to 1 second
      (patchwork.midi:date event (+  *MidiShare-start-time* at))
      (patchwork.midi:MidiAddSeq seq event)
      )))
;;changed from paw-modifs 140397 aaa
|#

(defgeneric MidiPlay (note at approx chanbase seq unit/sec))
(defmethod MidiPlay ((note c-note) at approx chanbase seq unit/sec)
  (let ((event (patchwork.midi:MidiNewEv patchwork.midi:typeNote))) ; ask for a new note event
    (when (zerop chanbase) (setf chanbase (chan note)))
    (unless (patchwork.midi:nullptrp event) ; if the allocation was succesfull
      (patchwork.midi:chan event ; set the midi channel to 0 (means channel 1)
                       (1- (+ chanbase
                              (1- (micro-channel (epw::approx-m  (midic note) approx))))))
      (patchwork.midi:port event 0)    ; set the destination port to Modem
      (patchwork.midi:field event 0 (truncate (epw::approx-m (midic note) approx) 100)) ; set the pitch field
      (patchwork.midi:field event 1 (round (vel note))) ; set the velocity field
      (patchwork.midi:field event 2 (round (convert-time-1 (dur note) unit/sec))) ; set the duration field to 1 second
      (patchwork.midi:date event (+  *MidiShare-start-time* at))
      (patchwork.midi:MidiAddSeq seq event))))


(defmethod MidiPlay ((list list) at approx chanbase seq unit/sec)
  (loop for object in list
        do (MidiPlay object at approx chanbase seq unit/sec)))

(defmethod MidiPlay ((chord c-chord) at approx chanbase seq unit/sec)
  (let ((factor (if *play-chseq-w/offset* 1 0)))
    (loop for note in (notes chord)
          do (MidiPlay note  (+  (round (convert-time-1 (* factor (offset-time note)) unit/sec)) at) approx chanbase seq unit/sec))))

(defmethod MidiPlay ((chord c-arp-chord) at approx chanbase seq unit/sec)
  (loop for note in (notes chord)
        for offset from 0 by 50
        do (MidiPlay note  (round (+  (convert-time-1 offset unit/sec) (convert-time-1 (offset-time note) unit/sec) at)) approx chanbase seq unit/sec)))
        

(defmethod MidiPlay ((chline C-CHORD-LINE) at approx chanbase seq unit/sec)
  (loop for chord in (chords chline)
        do (MidiPlay chord  (round (+  (convert-time-1 (t-time chord) unit/sec) at)) approx chanbase seq unit/sec)))

(defmethod MidiPlay ((mesline C-MEASURE-LINE) at approx chanbase seq unit/sec)
  (loop for measure in (measures mesline)
        do (MidiPlay measure at approx chanbase seq unit/sec)))

(defmethod MidiPlay ((measure C-MEASURE) at approx chanbase seq unit/sec)
  (loop for beat in (beat-objects measure)
        do (MidiPlay beat at approx chanbase seq unit/sec)))

(defmethod MidiPlay ((beat C-beat) at approx chanbase seq unit/sec)
  (if  (and (beat-chord beat)  (beat-leaf? beat))
    (MidiPlay (beat-chord beat) (+ at (round (convert-time-1 (t-time (beat-chord beat)) unit/sec))) approx chanbase seq unit/sec)
    (loop for beat2 in (rtm-list beat)
          do (MidiPlay beat2 at approx chanbase seq unit/sec))))


;;=========

(defun scale2approx (scale)
  (cond ((eq scale *1/4-tone-chromatic-scale*) 4)
                  ((eq scale *1/8-tone-chromatic-scale*) 8)
                  (t 2)))

;;====play Chord

#-(and) ; already in pw-chord-box.lisp
(defmethod play ((self C-patch-chord-box-M ))
  (let ((approx (scale2approx (local-approx (car (subviews (mus-not-editor self))))))
        (object (car (chords (chord-line (car (pw-controls self)))))))
    (when object
      (setf *play-chseq-w/offset* nil)
      (MidiPlayAny object  approx 0))))

(defmethod play-all-staffs ((self C-chord-mus-not-view))
  (let ((approx (scale2approx (local-approx self)))
        (thechord  (if (equal (class-name (class-of (pw-win (view-container self)))) 'c-pw-window)
                     (chord-line (car (editor-objects self)))
                     (car (chords (chord-line (car (editor-objects self))))))))
    (cond 
     ((setting-of self :time)
      (setf *play-chseq-w/offset* t)
      (MidiPlayAny thechord approx 0))
     ((setting-of self :arp)
      (play-arpeggiated self))
     (t (setf *play-chseq-w/offset* (get-ctrl-setting self :offs)) (MidiPlayAny thechord approx 0)))))
    
 

(defmethod play-arpeggiated ((self C-chord-mus-not-view))
  (let* ((chord (car (chords (chord-line (car (editor-objects self))))))
         (arp-chord (make-instance 'c-arp-chord))
         (approx (scale2approx (local-approx self))))
    (setf (notes arp-chord) (sort (copy-list (notes chord)) #'< :key #'order))
    (MidiPlayAny arp-chord approx 0)))


;;=====play Chordseq and Multiseq

(defmethod play ((self C-patch-midi)) 
  (play-all-staffs (car (subviews (application-object self)))))

(defmethod play-all-staffs ((self C-MN-view-mod))
  (let ((*play-chseq-w/offset* (get-ctrl-setting self :offs)))
    (call-next-method)))

(defmethod play-all-staffs ((self C-mus-not-view))
  (let ((panels (editor-objects self))
        (approx (scale2approx (local-approx self))))
    (if (monofonic-mn? self)
      (MidiPlayAny (chord-line (car panels)) approx 0)
      (MidiPlayAny (ask-all panels 'chord-line) approx 0))))



;;====play rtm

(defmethod play ((self C-patch-application-rtm-editor)) (play-from-pw self))
(defmethod play ((self C-patch-PolifRTM)) (play-from-pw self))



(defun play-from-pw (self) 
  (let* ((editor (car (subviews (application-object self))))
         (*play-chseq-w/offset* (check-box-checked-p (third (rtm-radio-ctrls editor))))
         (editors (give-selected-editors editor)))
    (MidiPlayAny (ask-all editors 'measure-line)  (compute-approx) 0)) )


#-(and) ; already in rtm-editor.lisp
(defun play-rtm-with-options (self)
  (let ((editors (give-selected-editors self)))
    (setf *mn-view-offset-flag* (check-box-checked-p (third (rtm-radio-ctrls self))))
    (let ((c-line (make-instance 'C-chord-line 
                    :chords (remove nil (flat (rtm-chords (ask-all editors 'measure-line)))))))
      (if *mn-view-offset-flag*
        (let ((*play-chseq-w/offset* t)) (MidiPlayAny c-line  (compute-approx) 0))
        (MidiPlayAny c-line  (compute-approx) 0)))))

#-(and) ; already in rtm-paging+kill
(defun play-rtms+scroll (self)
  (play-rtm-with-options self))


;;===stop-play

(defmethod stop-play ((self c-patch))
  (when patchwork.midi:*player* (patchwork.midi:stopplayer patchwork.midi:*player*)))

(defmethod stop-play ((self c-patch-polifrtm))
  (when patchwork.midi:*player* (patchwork.midi:stopplayer patchwork.midi:*player*)))

(defmethod stop-play ((self C-patch-score-voice)) 
  (when patchwork.midi:*player* (patchwork.midi:stopplayer patchwork.midi:*player*)))

(defmethod stop-play ((self C-patch-midi)) 
  (when patchwork.midi:*player* (patchwork.midi:stopplayer patchwork.midi:*player*)))

(defmethod stop-all-staffs ((self C-mus-not-view))
  (when patchwork.midi:*player* (patchwork.midi:stopplayer patchwork.midi:*player*)))

(defmethod stop-measure-line ((self C-measure-line)) 
  (when patchwork.midi:*player* (patchwork.midi:stopplayer patchwork.midi:*player*)))


;;;; THE END ;;;;
