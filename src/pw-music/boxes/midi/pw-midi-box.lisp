;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pw-midi-box.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;
;;;;    Midi boxes for PW
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

(defunp midi-o ((bytes (list (:value "(144 60 100)" :type-list (list midic))))) list
    "<midi-o> sends <bytes> out of the Macintosh serial modem port. For example,
    if we give the list (144 60 64) for bytes, our MIDI synthesizer (assuming it is
    connected) play middle-C on channel 1 with a velocity of 64.  To turn off the
    note, we would have to give bytes the argument (144 60 0)."
  (when bytes
    (let ((event (patchwork.midi:MidiNewEv patchwork.midi:typeStream)))
      (unless (patchwork.midi:null-event-p event)
        (patchwork.midi:chan event 0)
        (patchwork.midi:port event 0)
        (dolist (byte (list! bytes))
          (patchwork.midi:midiaddfield event byte))
        (patchwork.midi:MidiSendIm patchwork.midi:*pw-refnum* event)))))

(defunp pgmout1 ((son fix (:value 1 :min-val 1  :max-val 128))
                 (canal approx )) list
    "numéro de programme --> Midi out"
  (midi-o (list (+ 191 canal)(- son 1) )) )

(defunp pgmout ((pgms numbers? (:value 1)) (chans numbers? (:value 1))) list
    "<pgmout> sends a MIDI program change message out
 of the Macintosh serial modem port. <pgms>  is the program
 number and <chans>  is the MIDI channel. Both of these can be
 lists. In this case a list of MIDI program change  messages is sent out."
  (cond ((not (consp pgms))
         (mapc (lambda (canal) (pgmout1 pgms canal)) (list! chans)))
        ((not (consp chans))
         (mapc (lambda (son) (pgmout1 son chans)) (list! pgms)))
        (t (mapc #'pgmout1 pgms chans))))

(defun bendout1 (son canal)
  "pitch bend --> Midi out"
  (setq son (+ 8192 (max -8192 (min 8190 son))))
  (midi-o (list (+ #16rdf canal) (ldb (byte 7 0) son) (ldb (byte 8 7) son )) ))

(defunp bendout ((values numbers?) (chans numbers? (:value 1))) list
    "<bendout> sends a MIDI pitch bend message(s) <values> in the given MIDI
channel(s) <chans>.  <values> and <chans> can be single numbers or lists.
The range of pitch bend is between -8192 and 8190."
  (cond ((not (consp values))
         (mapc (lambda (canal) (bendout1 values canal)) (list! chans)))
        ((not (consp chans))
         (mapc (lambda (son) (bendout1 son chans)) (list! values)))
        (t (mapc #'pgmout1 values chans))))

(defun volum1 (vol canal)
  (midi-o (list (+ 175 canal) 7 vol)))

(defunp volume ((vol numbers? (:value 127))
                (chans numbers?)) list
    " volume sends a MIDI volume  message(s) values in the given MIDI channel(s)
chans.  vol   and chans  can be single numbers or lists. The range of  volume is
between
0  and 127.

    "
  (cond ((not (consp vol))
         (mapc (lambda (canal) (volum1 vol canal)) (list! chans)))
        ((not (consp chans))
         (mapc (lambda (vol) (volum1 vol chans)) (list! vol)))
        (t (mapc #'volum1 vol chans))))



(defunp play-sequence ((ch-l list (:value '() :type-list (collector)))
                       (chan fix>=0 (:value 1)) &optional (approx approx (:value 4))) nil
    "Plays a chord sequence (first argument). The second argument is a channel
for chord notes. The third (optional) argument gives the approximation (default: quarter tone)"

  (let* ((chords (chords ch-l))
         (notes (ask-all chords 'notes))
         (midics (mapcar (lambda (note) (ask-all note 'midic)) notes))
         (atimes (ask-all chords 't-time))
         (vels (mapcar (lambda (note) (ask-all note 'vel)) notes))
         (durs (mapcar (lambda (note) (ask-all note 'dur)) notes))
         (offs (mapcar (lambda (note) (ask-all note 'offset-time)) notes))
         (chans (mapcar (lambda (note) (epw::l+ (ask-all note 'chan) -1)) notes)))
    (C-pw-send-midi-note:snd-midinote  (epw::approx-m midics approx) vels
                                       (if (= chan 0)
                                           (mapcar (lambda (lis1 lis2) (epw::l+ lis1 lis2)) (epw::microtone midics approx) chans)
                                           (epw::ll-oper (epw::microtone midics approx) (1- chan) '+))
                                       durs offs atimes)))

(defvar *play-chseq-w/offset* nil)

;; GA Pw2.6.2 101096
(defunp play-object ((object list (:value '() :type-list ()))
                     (chan fix>=0 (:value 1)) &optional (approx approx (:value 4))) nil
    "play-object plays a note-object, a chord-object, a chord-line object, a measure-line object (rtm)
or a list of such, connected to it. Typing 's' while the box is selected stops the play.

chan:

if 0, plays with regard to the channel information contained in the object.
if > 0, plays the object on the specified channel. Micro-interval channels are
translated accordingly.

approx:

The pitch approximation used. This can be the whole tone (approx = 1),
semitone (approx  = 2),  quartertone (approx = 4, the default)
or eighth-tone (approx = 8).
Notes with microtonal accidentals are sent to different output channels according
to the following mapping: chan (semitones), chan + 1 (eighth-tones),
chan + 2 (quartertones),  chan + 3 (three-eighths tones).
"
  (let ((*play-chseq-w/offset* t))
    (pw::MidiPlayAny object approx chan)))



;; ;; GA Pw2.6.2 101096
;; (defunp play-object ((object list (:value '() :type-list ()))
;;                      (chan fix>=0 (:value 1)) &optional (approx approx (:value 4))) nil
;;     "play-object plays a note, chord, or chord sequence specified in its input object
;; through
;; the   MIDI ;channel specified in chan . The approx input is the approximation
;; value
;; for midicents. This can be the whole tone (approx = 1), semitone (approx  = 2),
;; quartertone (approx = 4, the default) or eighth-tone (approx = 8). The play
;; operation
;; takes the full duration specified in the given object. Notes with microtonal
;; accidentals
;; are sent to different output channels according to the following mapping: chan +
;; 1
;; (eighth-tones), chan + 2 (quartertones) or chan + 3 (three-eighths tones). For
;; example,
;; if you set chan to 8, semitones are sent out channel 8, eighth-tones are sent out
;; channel
;; 9, quartertones are sent out channel 10, and so on.
;; "
;;   (if (subtypep (type-of object) 'C-measure-line)
;;       (play-sequence (make-instance 'C-chord-line
;;                                     :chords (car (rtm-chords (list object)))) chan approx)
;;       (play-yourself-as-object object chan approx)))
;;
;; (defmethod play-yourself-as-object ((object C-chord) chan approx)
;;   (let* ((notes (notes object))
;;          (midics (ask-all notes 'midic)))
;;     (C-pw-send-midi-note:snd-midinote (approx-m midics approx)
;;                                       (ask-all (notes object) 'vel)
;;                                       (l+ (epw::microtone midics approx) (1- chan))
;;                                       (ask-all (notes object) 'dur)
;;                                       (ask-all (notes object) 'offset-time))))
;;
;; (defmethod play-yourself-as-object ((object C-note) chan approx)
;;   (C-pw-send-midi-note:snd-midinote (approx-m (midic object) approx) (vel object)
;;                                     (+ (car (epw::microtone (list (midic object)) approx))
;;                                        chan)
;;                                     (dur object) (offset-time object)))

;; (defmethod play-yourself-as-object ((object C-chord-line) chan approx)
;;   (play-sequence object chan approx))
;;
;; (defmethod play-yourself-as-object ((object C-chord) chan approx)
;;   (let* ((notes (notes object))
;;          (midics (ask-all notes 'midic))
;;          (chans (l+ (ask-all notes 'chan) -1)))
;;     (C-pw-send-midi-note:snd-midinote (epw::approx-m midics approx)
;;                                       (ask-all (notes object) 'vel)
;;                                       (l+ (epw::microtone midics approx) (if (= chan 0) chans (1- chan)))
;;                                       (ask-all (notes object) 'dur)
;;                                       (ask-all (notes object) 'offset-time))))
;;
;; (defmethod play-yourself-as-object ((object C-note) chan approx)
;;   (C-pw-send-midi-note:snd-midinote (epw::approx-m (midic object) approx) (vel object)
;;                                     (+ (car (epw::microtone (list (midic object)) approx))
;;                                        (if (= chan 0) (1- (chan object)) (1- chan)))
;;                                     (dur object) (offset-time object)))
;;
;; (defmethod play-yourself-as-object ((object cons) chan approx)
;;   (if (dolist (obj object t)
;;         (unless (subtypep (type-of obj) 'C-measure-line) (return nil)))
;;       (play-sequence (make-instance 'C-chord-line
;;                                     :chords (sort (apply #'append (rtm-chords object)) '< :key #'t-time))
;;                      chan approx)
;;       (tell object #'play-object chan approx)))
;;
;; (defmethod play-yourself-as-object ((object t) chan approx)
;;   (format t "don't know how to play object: ~S ~%" object)
;;   (ui:ed-beep))


(in-package "C-PW-SEND-MIDI-NOTE")

;;Class definition is kept only for compatibility. Should eventually be erased
(defclass C-pw-send-midi-note (C-pw-functional) ())

(defmethod keep-playing-fun-forms (note-forms prev-delay)
  (play-note-form (cdr (pop note-forms)))
  (while (and note-forms (= prev-delay (caar note-forms)))
    (play-note-form (cdr (pop note-forms))))
  (if note-forms
      (re-dfuncall (- (caar note-forms) prev-delay)  note-forms (caar note-forms))))

(defun play-note-form (note-form)
  (write-midi-note (first note-form) (second note-form) (truncate (third note-form) 100)
                   (fourth note-form)))

(defun set-note-forms (midics channel vels durs offs t-time)
  (let* ((midics midics) (channel channel) (vels vels) (durs durs) (offs offs)
         (def-vels (or (car (last vels)) 100))
         (def-chan (or (car (last channel)) 1))
         (def-dur (or (car (last durs)) 75))
         note-forms)
    (while (or midics vels channel durs offs)
      (push (list (+ t-time (or (pop offs) 0))
                  (or (pop durs) def-dur) (or (pop channel) def-chan) (or (pop midics) 6000)
                  (or (pop vels) def-vels)) note-forms))
    (nreverse note-forms)))

(defunp snd-midinote ((midics midic) (vels (midic (:value 100)))
                      (chan (approx (:type-list (fixnum list))))
                      (durs (fix>=0 (:type-list (fixnum list))))
                      &optional (offs (integer (:type-list (fixnum list))))
                      (at-time (fix>=0 (:type-list (fixnum list))))) list
    "<snd-midinote> formats and plays MIDI note events.
If <midics> is a list, then the result is a chord.  Notes are played with
a channel <chan>  velocity
<vels>  and duration <durs> as determined by the inputs. The
optional input  <offs> lets one
assign a time offset (in 100ths of a second relative to time zero) for
each note of the chord.  If
midics is a list of lists then snd-midinote will produce a sequence of
chords. The at-time
determines the start time (in 100ths of a second relative to time
zero) for each chord. If the
second optional input <at-time> is a single value, chords are equally
spaced in time by that
value. A list for <at-time> gives each chord in the list its own start
time. Note: if any of the
argument lists is shorter than <midics>, the last value of those lists
are used to play the
remaining notes."
  (let* ((midics (list! midics))
         (vels (list! vels))
         (def-vels (or (car (last vels)) 100))
         (channel (list! chan))
         (def-chan (or (car (last channel)) 1))
         (durs (list! durs))
         (def-dur (or (car (last durs)) 75))
         (offs (if offs
                   (list! offs)
                   (list 0)))
         (t-times (if at-time
                      (list! at-time)
                      (list 0)))
         (acum-t 0)
         notes-form)
    (while (or midics vels channel durs offs t-times)
      (setq notes-form
            (append
             (set-note-forms (cond ((null midics) (list 6000))
                                   ((consp (car midics))  (pop midics))
                                   (t (prog1 midics (setq midics nil))))
                             (cond ((null channel) (list def-chan))
                                   ((consp (car channel)) (pop channel))
                                   (t (prog1 channel (setq channel nil))))
                             (cond ((null vels) (list def-vels))
                                   ((consp (car vels)) (pop vels))
                                   (t (prog1 vels (setq vels nil))))
                             (cond ((null durs) (list def-dur))
                                   ((consp (car durs)) (pop durs))
                                   (t (prog1 durs (setq durs nil))))
                             (cond ((null offs) (list 0))
                                   ((consp (car offs)) (pop offs))
                                   (t (prog1 offs (setq offs nil))))
                             (setq acum-t
                                   (if t-times (pop t-times) (+ acum-t 50))))
             notes-form)))
    (setq notes-form (sort notes-form '< :key #'car))
    (start (apdfuncall 80 (priority) 82
                       'keep-playing-fun-forms notes-form (caar notes-form)))
    nil))


(in-package "C-PW-SEND-MIDI-NOTE")
(defunp play-chords ((midics midic) (vels (midic (:value 100)))
                     (chan (approx (:type-list (fixnum list))))
                     (durs (fix>=0 (:value 100 :type-list (fixnum list))))
                     &optional (offs (integer (:type-list (fixnum list))))
                     (at-time (fix>=0 (:type-list (fixnum list))))) list
    "Constructs and sends a note, list of notes, or list of lists of notes with the
given parameters to MIDI"
  (let* ((acum-t 0) chord-list
         (midics (list! midics))
         (vels (list! vels))
         (def-vels 100)
         (channel (list! chan))
         (def-chan 1)
         (durs (list! durs))
         (def-dur 75)
         (offs (if offs
                   (list! offs)
                   (list 0)))
         (t-times (if at-time
                      (list! at-time)
                      (list 0))))
    (while (or midics vels channel durs offs t-times)
      (push (pw::mk-chord-at
             (setq acum-t
                   (if t-times (pop t-times) (+ acum-t 50)))
             (cond ((null midics) (list 6000))
                   ((consp (car midics))  (pop midics))
                   (t (prog1 midics (setq midics nil))))
             (cond ((null durs) (list def-dur))
                   ((consp (car durs)) (pop durs))
                   (t (prog1 durs (setq durs nil))))
             (cond ((null offs) (list 0))
                   ((consp (car offs)) (pop offs))
                   (t (prog1 offs (setq offs nil))))
             (cond ((null vels) (list def-vels))
                   ((consp (car vels)) (pop vels))
                   (t (prog1 vels (setq vels nil))))
             (cond ((null channel) (list def-chan))
                   ((consp (car channel)) (pop channel))
                   (t (prog1 channel (setq channel nil)))))
            chord-list))
    (let ((pw::*play-chseq-w/offset* t))
      (pw::MidiPlayAny (make-instance 'pw::c-chord-line :chords (reverse chord-list))
                       (pw::compute-approx)
                       0))))




(in-package "C-PW-MIDI-IN")


(defclass C-pw-midi-in (C-PW-functional)
  ((state :initform t :accessor state)
   (popUpbox :initform nil :accessor popUpbox)))

(defmethod decompile ((self C-pw-midi-in))
  (set-output self :off)
  (call-next-method))

(defparameter *Midi-box-popUpMenu*
  (new-menu " "
            (new-leafmenu "Deactivate" (lambda () (set-output *target-action-object* :off)))))

(defmethod initialize-instance :after ((self C-pw-midi-in) &key controls)
  (declare (ignore controls))
  (setf (popUpBox self)
        (make-popUpbox  "D" self
                        *Midi-box-popUpMenu*
                        :view-position (ui:make-point (- (w self) 19)
                                                      (- (h self) 13))
                        :view-container self
                        :view-font pw::*patchwork-font-spec*)))

(defmethod set-output ((self C-pw-midi-in) type)
  (case  type (:on (setf (state self) nil) (set-box-title (popUpbox self) "A"))
         (:off (setf (state self) t) (set-box-title (popUpbox self) "D"))))

(defmethod patch-value ((self C-pw-midi-in) obj)
  (patchwork.midi:midi-reset)
  (if (state self)
      (set-output self :on))
  (let ((box (first (input-objects self))))
    (unless (eql (first (pw-controls self)) box)
      (start (apdfuncall 5 (priority) 10 'pw-schedule-midi-in
                         self box (second (input-objects self))
                         (if (third (input-objects self))
                             (patch-value (third (input-objects self)) obj)
                             1 ))))
    "active"))

(defmethod pw-schedule-midi-in ((self C-pw-midi-in) box patch delay)
  (let ((data (midi-read)))
    (if (and (not (patchwork.midi:null-event-p data)) (pw-midi-filtered self data))
        (progn
          (setf (value box) (format-midi self data))
          (patch-value patch self)
          (pw-schedule-midi-in self box patch delay))
        (and (not (state self)) (re-dfuncall delay self box patch delay)))))

(defmethod format-midi ((self C-pw-midi-in) data) data)

(defmethod pw-midi-filtered ((self C-pw-midi-in) data)
  (not (equal (patchwork.midi:evtype data) patchwork.midi:typeClock)))

(defunp pw-midi-in ((in-box (list (:value "()" :type-list (midi-in-obj))))
                    (patch (list (:type-list ())))
                    &optional (delay (fix>0 (:value 10)))) nil
    "
<pw-midi-in> & <m-data> are used to gather incoming MIDI data from the
Macintosh serial ports.  In order for the two to work, there must
always be a loop with pw-midi-in at the bottom, <m-data> at the top,
and some kind of <patch> dealing with the MIDI data in between.  The
optional input <delay> gives the delay time  evaluation (in 100ths of
a second) of the input <patch>.  In order to start the loop collecting
MIDI data, the pw-midi-in module must be evaluated.  When you are
finished collecting MIDI data, select deactivate in the pw-midi-in
menu.  WARNING!!!!!, if the module is not deactivated, patch
evaluation will be very slow.  Note that the incoming MIDI data will
come out of the above patch in a compressed form.  To decompress the
data, see the modules: midi-opcode, midi-channel, midi-data1,
midi-data2, and midi-status.
"
  (declare (ignore in-box patch delay)))

(defunp raw-in ((in-box (list (:value "()" :type-list (midi-in-obj))))
                (patch (list (:type-list ())))
                &optional (delay (fix>0 (:value 10)))) nil
    "
The modules raw-in and m-data together gather incoming MIDI data from
the Macintosh serial ports.  In order for them to work, there must
always be a loop with raw-in at the bottom, m-data ;at the top, and
some kind of patch dealing with the MIDI data in between.  The
optional input delay gives the delay time evaluation (in 100ths of a
second) of the input patch.  In order to start the loop collecting
MIDI data, the raw-in module must be evaluated.  When you are finished
collecting MIDI data, select 'Deactivate' in the raw-in menu.
Warning, Very Important!!:: If the module is not deactivated after
use, patch evaluation will be very slow.  It is very dangerous because
may cause Patchwork to report endlessly: 'late Task'.  Note that the
incoming MIDI data comes out of the patch in a compressed form.  To
decompress the data, see the modules: midi-opcode;, midi-chan;,
midi-data1;, midi- data2;, and midi-status;.
"
  (declare (ignore in-box patch delay)))

(defclass C-PW-midi-in-top (C-patch)
  ((value :initform nil :accessor value)))

(defmethod patch-value ((self C-PW-midi-in-top) obj)
  (declare (ignore obj))
  (value self))

(defunp m-data () nil
    "A box always connected to a pw-midi-in box. Gives successive midi events."
  )

;;================
;; Note-in module

(defclass C-pw-note-in (C-pw-midi-in) ())

(defmethod format-midi ((self C-pw-note-in) data)
  (list (patchwork.midi:pitch data) (patchwork.midi:vel data)
        (1+ (patchwork.midi:chan data))))


(defmethod pw-midi-filtered ((self C-pw-note-in) data)
  (member (patchwork.midi:evtype data) (list 0 1 2)))



(defclass C-pw-note-on-in (C-pw-note-in) ())

(defmethod pw-midi-filtered ((self C-pw-note-on-in) data)
  (and (call-next-method) (not (zerop (patchwork.midi:vel data)))))

(defunp note-on-in ((in-box (list (:value "()" :type-list (midi-in-obj))))
                    (patch (list (:type-list ())))
                    &optional (delay (fix>0 (:value 10)))) nil
    "<note-on-in> & <m-data> are used to gather incoming MIDI data from the Macintosh serial ports
 ( it filters out all events other than note-on).
  In order for the two to work, there must always be a loop with pw-midi-in at the bottom,
 <m-data> at the top, and some kind of <patch> dealing with the MIDI data in between.
The optional input <delay> gives the delay time  evaluation (in 100ths of a second) of the input <patch>.
In order to start the loop collecting MIDI data, the pw-midi-in module must be evaluated.
When you are finished collecting MIDI data, select deactivate in the pw-midi-in menu.
  WARNING!!!!!, if the module is not deactivated, patch evaluation will be very slow."
  (declare (ignore in-box patch delay)))

(defunp note-in ((in-box (list (:value "()" :type-list (midi-in-obj))))
                 (patch (list (:type-list ())))
                 &optional (delay (fix>0 (:value 10)))) nil
    "
The note-in and m-data modules are invoked simultaneously.  They gather
incoming MIDI data from the Macintosh serial ports.  note-in filters
out all events other than note-on messages.  In order for the two
modules to work, there must always be a loop with note-in at the
bottom, m-data at the top, and some kind of patch dealing with the
MIDI data in between.  The optional input delay gives the delay time
evaluation (in 100ths of a second) of the input patch.  In order to
start the loop that collects MIDI data, evaluate the note-in
module.  When you are finished collecting MIDI data, select
'Deactivate' in the note-in menu.  Warning, Very Important!!: If the
module is not deactivated after use, patch evaluation will be very
slow.  It is very dangerous because may cause Patchwork to report
endlessly: 'late Task'.
"
  (declare (ignore in-box patch delay)))

(defclass C-pw-chord-in (C-pw-note-on-in)
  ((the-chord :initform (make-instance 'pw::C-chord :notes ()) :accessor the-chord)))

(defmethod format-midi ((self C-pw-chord-in) data)
  (let* ((the-chord (the-chord self))
         (the-notes (pw::notes the-chord))
         (a-note (pw::make-C-note
                  (* 100 (patchwork.midi:pitch data)) nil nil 100
                  (patchwork.midi:vel data) (1+ (patchwork.midi:chan data))
                  nil 0 0)))
    (setf (pw::order a-note) (length the-notes))
    (setf (pw::notes the-chord)
          (push (pw::make-C-note
                 (* 100 (patchwork.midi:pitch data)) nil nil 100
                 (patchwork.midi:vel data) (1+ (patchwork.midi:chan data))
                 nil 0 0) the-notes))
    (pw::update-chord the-chord)
    the-chord))

(defmethod patch-value ((self C-pw-chord-in) obj)
  (declare (ignore obj))
                                        ;(patchwork.midi:midi-reset)
  (setf (the-chord self) (make-instance 'pw::C-chord :notes ()))
                                        ;(setf (pw::notes (the-chord self)) nil)
  (call-next-method))

(defmethod pw::disconnect-ctrl ((self C-pw-chord-in) ctrl)
  (declare (ignore ctrl))
  (set-output self :off)
  (call-next-method))

(defunp chord-in ((in-box (list (:value "()" :type-list (midi-in-obj))))
                  (patch (list (:type-list ())))
                  &optional (delay (fix>0 (:value 10)))) nil
    "The chord-in and   m-data ;modules work in a similar way as   note-in;. The
chord-in module filters   MIDI ;events other than note-on messages. The patch
connected to patch  is repeatedly evaluated for each new MIDI note-on event.
The output of the m-data box is a chord object with all accumulated notes
since the last box request. The optional input delay gives the delay time
evaluation (in 100ths of a second) of the input patch. . Warning,  Very
Important!:: If the module is not deactivated after use, patch evaluation will be
very slow.  It is very dangerous because may cause Patchwork to report
endlessly: 'late Task'."
  (declare (ignore in-box patch delay)))




(defunp delay ((delay (fix>0 (:value 10)))
               (patch (list (:value "()" :type-list ())))) nil
    "<delay> takes the evaluation of <patch>  after the <delay>"
  (declare (ignore delay patch)))

(defclass C-pw-delay-box (C-patch)
  ((state :initform t :accessor state)))


(defmethod patch-value ((self C-pw-delay-box) obj)
  (sleep (/ (patch-value (first (input-objects self)) obj) 100 ))
  (patch-value (second (input-objects self)) obj))


;;(defmethod patch-value ((self C-pw-delay-box) obj)
;;  (start (apdfuncall 0 (priority)
;;                     (round (patch-value (first (input-objects self)) obj))
;;                     'sleep-yourself self))
;;  (while (state self))
;;  (setf (state self) t)
;;  (patch-value (second (input-objects self)) obj))

(defmethod sleep-yourself ((self C-pw-delay-box))
  (setf (state self) nil))

(defun pw::pw-reset-for-midi()
  ;;(patchwork.scheduler:set-scheduler-state :oot)
  (patchwork.midi:midi-close)
  (patchwork.scheduler::init-scheduler)
  (patchwork.midi:midi-open)
  ;;(patchwork.scheduler:set-scheduler-state :rt)
  )



(defclass C-jouer/eteindre (C-pw-functional)
  ((etat :initform nil :accessor etat)))


(defmethod patch-value ((self C-jouer/eteindre) obj)
  (let* ((result nil)
         (accord (patch-value (first (pw::input-objects self)) obj))
         (approx (patch-value (second (pw::input-objects self)) obj))
         (canal (patch-value (third (pw::input-objects self)) obj))
         (hauteurs (pw::ask-all (pw::notes accord) 'pw::midic))
         (velo     (pw::ask-all (pw::notes accord) 'pw::vel))
         (dur (if (nth 3 (pw-controls self))
                  (patch-value (fourth (input-objects self)) obj)  0)))
    (while hauteurs (let ((hauteur (epw::approx-m (nextl hauteurs) approx)))
                      (push (+ 143 canal (/ (mod  hauteur 100) 25)) result)
                      (push (truncate hauteur 100) result)
                      (if  (etat self) (push 0 result) (push (nextl velo) result) ) ))
    (pw::midi-o (setq result (nreverse result)))
    (if (not (or (etat self) (zerop dur)))
        (start (apdfuncall 0 (priority) dur 'stop-the-notes self
                           (modify-velocity result))))
    (setf (etat self) (null (etat self)))
    nil))

(defunp play/stop ((midics list (:type-list () :value '(6000))) (vel fix>=0 (:value 100)) (channel approx)
                   (approx fix (:value 4))
                   &optional (dur fix>=0)) list
    "The play/stop module plays a chord object through the   MIDI ;channel given
in channel. The approx variable is the approximation value for midicents, which
can be set to whole tone (approx = 1), semitone (approx  = 2), quartertone
\(approx = 4, the default)  or eighth-tone (approx = 8).  If no duration is supplied
\(or if it is equal to zero)  the module keeps playing until you option-click again at
its output box. Otherwise  it plays for the given duration dur. Notes with
microtonal accidentals are sent to different output channels according to the
following mapping: channel + 1 (eighth-tones), channel + 2 (quartertones) or
channel + 3 (three-eighths tones). For example, if you set channel to 8,
semitones are sent out channel 8, eighth-tones are sent out channel 9, and so
on."
  (declare (ignore midics approx channel dur vel)))

(defclass C-play/stop-list (C-jouer/eteindre) ())

(in-package "C-PW-MIDI-IN")
(defmethod patch-value ((self C-play/stop-list) obj)
  (let ((object (patch-value (first  (input-objects self)) obj))
        midics vel channel dur approx pitch result def-chan def-vel)
    (if (subtypep (type-of object) 'pw::C-chord)
        (setq
         midics (ask-all (pw::notes object) 'pw::midic)
         vel (ask-all (pw::notes object) 'pw::vel)
         channel (ask-all (pw::notes object) 'pw::chan))
        (setq
         midics (pw::list! object)
         vel (pw::list! (patch-value (second (input-objects self)) obj))
         channel (pw::list! (patch-value (third (input-objects self)) obj))))
    (setq approx (patch-value (fourth (input-objects self)) obj)
          dur (if (nth 4 (pw-controls self))
                  (patch-value (fifth (input-objects self)) obj)  0)
          def-vel (first (last vel))
          def-chan (first (last channel)))
    (dolist (midic midics)
      (setq pitch (epw::approx-m midic approx))
      (push (+ 143 (or (pop channel) def-chan) (/ (mod  pitch 100) 25)) result)
      (push (truncate pitch 100) result)
      (if  (etat self) (push 0 result) (push (or (pop vel) def-vel) result) ) )
    (pw::midi-o (setq result (nreverse result)))
    (if (not (or (etat self) (zerop dur)))
                                        ;(start (apdfuncall 0 (priority) dur 'stop-the-notes self
                                        ;                   (modify-velocity result)))
        (print "Option dur is not available")
        )
    (setf (etat self) (null (etat self))) ))

(defmethod stop-the-notes ((self C-jouer/eteindre) notes)
  (pw::midi-o notes)
  (setf (etat self) nil))

(defun modify-velocity (list)
  (let ((length (length list))
        result)
    (for (i 1 1 length)
      (push (if (zerop (mod i 3)) 0 (nth (1- i) list)) result))
    (nreverse result)))

(in-package :pw)

(defun all-off ()
  (dotimes (c 15)
    (dotimes (n 127)
      (pw::midi-o (list (+ 144 c) n 0)))))
