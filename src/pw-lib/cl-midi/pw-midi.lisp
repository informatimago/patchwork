;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pw-midi.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;
;;;;    This package exports an interface to midi files using the
;;;;    CL-MIDI system:
;;;;
;;;;        This library for reading and writing MIDI files, and for
;;;;        representing MIDI events, was originally written by
;;;;        students of Robert Strandh at Université Bordeaux I.  It
;;;;        was then included in the Gsharp score editor[1], and was
;;;;        also posted to the alt.music.midi and comp.lang.lisp
;;;;        newsgroups[2].  Subsequently, workers at Goldsmiths
;;;;        College included it in their own projects, and took over
;;;;        the responsibility of distributing a copy usable by
;;;;        multiple projects.
;;;;
;;;;
;;;;    In addition real-time midi playing and recording is provided
;;;;    using the MacOSX CoreMIDI framework.
;;;;
;;;;    Original documentation: http://midishare.sourceforge.net/doc/
;;;;
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2017-07-29 <PJB> Connected to CoreMIDI for real-time MIDI I/O.
;;;;    2014-08-13 <PJB> Midishare API implemented over cl-midi.
;;;;    1986-??-?? Assayag, Agon -- Patchwork midi stuff.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;
;;;;    Copyright IRCAM 1986 - 1996
;;;;    Copyright Pascal J. Bourguignon 2014 - 2018
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
;;; Midishare stuff
;;;---------------------------------------------------------------------


;;;-----------------------------------------------------------------------
;;;-----------------------------------------------------------------------
;;;
;;;                     MidiShare Constant Definitions
;;;
;;;-----------------------------------------------------------------------
;;;-----------------------------------------------------------------------

;;; Constant definitions for every type of MidiShare event


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant typeNote          0 "note with pitch, velocity and duration")
  (defconstant typeKeyOn         1 "key on with pitch and velocity")
  (defconstant typeKeyOff        2 "key off with pitch and velocity")
  (defconstant typeKeyPress      3 "key pressure with pitch and pressure value")
  (defconstant typeCtrlChange    4 "control change with control number and control value")

  (defconstant typeProgChange    5 "program change with program number")
  (defconstant typeChanPress     6 "channel pressure with pressure value")
  (defconstant typePitchWheel    7 "pitch bend with lsb and msb of the 14-bit value")
  (defconstant typePitchBend     7 "pitch bender with lsb and msb of the 14-bit value")
  (defconstant typeSongPos       8 "song position with lsb and msb of the 14-bit position")
  (defconstant typeSongSel       9 "song selection with a song number")
  (defconstant typeClock        10 "clock request (no argument)")
  (defconstant typeStart        11 "start request (no argument)")
  (defconstant typeContinue     12 "continue request (no argument)")
  (defconstant typeStop         13 "stop request (no argument)")
  (defconstant typeTune         14 "tune request (no argument)")
  (defconstant typeActiveSens   15 "active sensing code (no argument)")
  (defconstant typeReset        16 "reset request (no argument)")
  (defconstant typeSysEx        17 "system exclusive with any number of data bytes. Leading $F0 and tailing $F7 are automatically supplied by MidiShare and MUST NOT be included by the user")
  (defconstant typeStream       18 "special event with any number of unprocessed data/status bytes")
  (defconstant typePrivate      19 "private event for internal use with 4 32-bits arguments")
  (defconstant typeProcess     128 "interrupt level task with a function adress and 3 32-bits args")
  (defconstant typeDProcess    129 "foreground task with a function address and 3 32-bits arguments")
  (defconstant typeQFrame      130 "quarter frame message with a type from 0 to 7 and a value")

  (defconstant typeCtrl14b     131)
  (defconstant typeNonRegParam 132)
  (defconstant typeRegParam    133)

  (defconstant typeSeqNum            134)
  (defconstant typeTextual     135)
  (defconstant typeCopyright   136)
  (defconstant typeSeqName     137)
  (defconstant typeInstrName   138)
  (defconstant typeLyric             139)
  (defconstant typeMarker            140)
  (defconstant typeCuePoint    141)
  (defconstant typeChanPrefix  142)
  (defconstant typeEndTrack    143)
  (defconstant typeTempo             144)
  (defconstant typeSMPTEOffset 145)

  (defconstant typeTimeSign    146)
  (defconstant typeKeySign     147)
  (defconstant typeSpecific    148)
  (defconstant typePortPrefix  149)

  (defconstant typeRcvAlarm    150)
  (defconstant typeApplAlarm   151)

  (defconstant typeReserved    152 "events reserved for futur use")
  (defconstant typedead        255 "dead task. Used by MidiShare to forget and inactivate typeProcess and typeDProcess tasks")

;;; Constant definition for every MidiShare error code

  (defconstant MIDIerrSpace   -1 "too many applications")
  (defconstant MIDIerrRefNu   -2 "bad reference number")
  (defconstant MIDIerrBadType -3 "bad event type")
  (defconstant MIDIerrIndex   -4 "bad index")

;;; Constant definition for the Macintosh serial ports

  (defconstant ModemPort   0 "Macintosh modem port")
  (defconstant PrinterPort 1 "Macintosh printer port")

;;; Constant definition for the synchronisation modes

  (defconstant MidiExternalSync #x8000
    "Bit-15 set for external synchronisation")
  (defconstant MidiSyncAnyPort  #x4000
    "Bit-14 set for synchronisation on any port")

;;; Constant definition for SMPTE frame format

  (defconstant smpte24fr 0 "24 frame/sec")
  (defconstant smpte25fr 1 "25 frame/sec")
  (defconstant smpte29fr 2 "29 frame/sec (30 drop frame)")
  (defconstant smpte30fr 3 "30 frame/sec")

;;; Constant definition for MidiShare world changes

  (defconstant MIDIOpenAppl     1 "application was opened")
  (defconstant MIDICloseAppl    2 "application was closed")
  (defconstant MIDIChgName      3 "application name was changed")
  (defconstant MIDIChgConnect   4 "connection was changed")
  (defconstant MIDIOpenModem    5 "Modem port was opened") ; obsolete
  (defconstant MIDICloseModem   6 "Modem port was closed") ; obsolete
  (defconstant MIDIOpenPrinter  7 "Printer port was opened")
  (defconstant MIDIClosePrinter 8 "Printer port was closed")
  (defconstant MIDISyncStart    9 "SMPTE synchronisation just start")
  (defconstant MIDISyncStop    10 "SMPTE synchronisation just stop")

  (defconstant MIDIChangeSync     10)
  (defconstant MIDIOpenDriver     11)
  (defconstant MIDICloseDriver    12)
  (defconstant MIDIAddSlot        13)
  (defconstant MIDIRemoveSlot     14)
  (defconstant MIDIChgSlotConnect 15)
  (defconstant MIDIChgSlotName    16)
  );;eval-when

;;;-----------------------------------------------------------------------
;;;                     Midishare implemented over CL-MIDI (midi file only).
;;;-----------------------------------------------------------------------

;;;
;;; Midishare entry point. (midishare-framework) must be called
;;; before Midishare can be used.
;;;

(defvar *midishare* t)

;;;
;;; Macros for accessing MidiShare Events data structures
;;;

(defun nullptrp (p) (null p))
(defun nullptr  ()  nil)

(defun ensure-boolean (value)
  (case value
    ((0 nil)    nil)
    (otherwise  t)))

;;;   Functions common to every type of event

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun get-option (key options &optional all-options)
    (let ((opt (remove-if (lambda (x) (not (eql key (if (symbolp x) x (car x)))))
                          options)))
      (cond
        (all-options opt)
        ((null opt) nil)
        ((null (cdr opt))
         (if (symbolp (car opt))
             t
             (cadar opt)))
        (t (error "Expected only one ~A option."
                  (if (symbolp (car opt))
                      (car opt)
                      (caar opt)))))))

  (defun make-name (option prefix name suffix)
    (cond
      ((or (null option) (and option (not (listp option))))
       (intern (with-standard-io-syntax (format nil "~A~A~A" prefix name suffix))))
      ((and option (listp option) (first option))
       (first option))
      (t nil)))

  (defun get-name (option)
    (if (and option (listp option))
        (first option)
        nil))

  (defun first-element (object)
    (if (listp object)
        (first object)
        object))

  );;eval-when



(defmacro defstruct* (&whole whole name-and-options &rest doc-and-fields)

  "Define a structure with the same NAME-AND-OPTIONS as DEFSTRUCT, an
optional docstring, then a list of usual defstruct slots, and an
optional list of variable fields.

This list of variable fields contains list of field names.  All the
fields named in the same list are stored in the same variable slot.

When there are variable fields, a normal info slot is added to the
structure, in addition to the slot used to store the variable fields.

If there is a :CONC-NAME option, then it is not passed down to
defstruct, but it is used to prefix the name of the specific
accessing functions.

For each field, compulsory or variable, a specific accessing function
is defined, with the structure as mandatory argument, and an optional
value argument.  Called with one argument, the value of the field is
returned; called with two arguments, the value of the field is set.
"

  (let (sname options include conc-name
        documentation
        fields compulsory-fields variable-fields)
    (if (symbolp name-and-options)
        (setf sname   name-and-options
              options nil)
        (setf sname   (car name-and-options)
              options (cdr name-and-options)))
    (if (stringp (car doc-and-fields))
        (setf documentation (car doc-and-fields)
              fields        (cdr doc-and-fields))
        (setf documentation nil
              fields        doc-and-fields))
    (setf include           (get-option :include options)
          conc-name         (or (get-option :conc-name options) "")
          options           (remove :conc-name options :key (function car))
          compulsory-fields (pop fields)
          variable-fields   (pop fields))
    (check-type compulsory-fields list)
    (check-type variable-fields   list)
    (assert (null fields) (fields) "~S: too many arguments in ~S" 'defstruct* whole)
    (when (cdr include)
      (setf compulsory-fields  (append (cddr include) compulsory-fields)))
    `(progn
       (defstruct (,sname ,@options)
         ,@(when documentation (list documentation))
         ,@compulsory-fields
         ,@(when variable-fields
             `((info (make-array ,(length variable-fields) :initial-element nil))
               additionnal-fields)))
       ,@(mapcar (lambda (field)
                   (let* ((fname (scat conc-name (first-element field)))
                          (aname (scat sname '-  (first-element field))))
                     `(defun ,fname (s &optional (v nil vp))
                        (if vp
                            (setf (,aname s) v)
                            (,aname s)))))
                 compulsory-fields)
       ,@(loop
           :with kname = (scat sname '-info)
           :for i :from 0
           :for fields :in variable-fields
           :append (loop :for field :in fields
                         :collect (let* ((fname (scat conc-name (first-element field))))
                                    `(defun ,fname (s &optional (v nil vp))
                                       (if vp
                                           (setf (aref (,kname s) ,i) v)
                                           (aref (,kname s) ,i))))))
       ',sname)))



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
            `(list 'let (list (list ,vvar ',initialization))
                   (list 'unwind-protect
                         (cons 'progn ,vbody)
                         ',finalization))
            `(list* 'let (list (list ,vvar ',initialization))
                    ,vbody)))))



(defstruct* midi-event
    (
     link    ; "read or set the link of an event"
      date    ; "read or set the date of an event"
      evtype  ; "read or set the type of an event. Be careful in modifying the type of an event"
      ref     ; "read or set the reference number of an event"
      port    ; "read or set the port number of an event"
      chan    ; "read or set the chan number of an event"
      )
  (
   (pitch
    clk bend
    pgm      fmsg    song ; clk.hi  bend.hi
    ctrl param num prefix tempo seconds tsnum alteration    ; 0
    )
   (kpress fcount         ; clk.lo  bend.lo
           vel
           subframes valint  tsdenom   minor-scale%         ; 1
           )
   (dur
    tsclick                                                 ; 2
    )
   (tsquarter)                                              ; 3
   ))

(defun null-event-p (event) (null event))




(defun minor-scale (e &optional v)
  (if v
      (minor-scale% e (if v 1 0))
      (= 1 (minor-scale% e 1))))


(defun field (e &optional f v)
  "give the number of fields or read or set a particular field of an event"
  (if f
      (if v
          (midiSetField e f v)
          (midiGetField e f))
      (midiCountFields e)))

(defun fieldsList (e &optional (n 4))
  "collect all the fields of an event into a list"
  (let (l)
    (dotimes (i (min n (midicountfields e)))
      (push (midigetfield e i) l))
    (nreverse l)))


(defun fields (e &optional v)
  (if v
      (mapc #'(lambda (f) (midiaddfield e f)) v)
      (let (l)
        (dotimes (i (midicountfields e))
          (push (midigetfield e i) l))
        (nreverse l))))

(defun text (e &optional s)
  (if s
      (fields e (map 'list #'char-code s))
      (map 'string #'character (fields e)) ))




(defstruct* (seq (:copier nil))
    (firstev lastev) ())

(defun seq-to-list (seq)
  (loop
    :for ev = (firstev seq) :then (link ev)
    :while ev
    :collect ev))

(defun seq-add-event (seq ev)
  (if (lastev seq)
      (link (lastev seq) ev)
      (firstev seq ev))
  (lastev seq ev))

#-(and) (
         (let ((seq (make-seq)))
           (seq-add-event seq (make-midi-event :evtype 1))
           (seq-add-event seq (make-midi-event :evtype 2))
           (seq-add-event seq (make-midi-event :evtype 3))
           (seq-to-list seq))
         )



;;----------------------------------------------------------------------

(defparameter *sources-refnums*      (make-hash-table :test (function equal)))
(defparameter *destinations-refnums* (make-hash-table :test (function equal)))

(defun list-source-ports ()
  (loop
    :with ports := '()
    :for device :in (append (coremidi:devices)
                            (coremidi:external-devices))
    :do (loop :for entity :in (coremidi:device-entities device)
              :do (loop
                    :for source :in  (coremidi:entity-sources entity)
                    :for key := (list  (coremidi:name device)
                                       (coremidi:name entity)
                                       (coremidi:name source))
                    :do (push key ports)))
    :finally (return (nreverse ports))))

(defun list-destination-ports ()
  (loop
    :with ports := '()
    :for device :in (append (coremidi:devices)
                            (coremidi:external-devices))
    :do (loop :for entity :in (coremidi:device-entities device)
              :do (loop
                    :for destination :in  (coremidi:entity-destinations entity)
                    :for key := (list  (coremidi:name device)
                                       (coremidi:name entity)
                                       (coremidi:name destination))
                    :do (push key ports)))
    :finally (return (nreverse ports))))

(defun update-port-refnums ()
  (let ((sources       (make-hash-table :test (function equal)))
        (destinations  (make-hash-table :test (function equal))))
    (loop
      :for device :in (append (coremidi:devices)
                              (coremidi:external-devices))
      :do (loop :for entity :in (coremidi:device-entities device)
                :do (loop
                      :for source :in  (coremidi:entity-sources entity)
                      :for key := (list  (coremidi:name device)
                                         (coremidi:name entity)
                                         (coremidi:name source))
                      :for refnum := (sxhash key)
                      :do (setf (gethash refnum sources) key
                                (gethash key    sources) refnum))
                    (loop
                      :for destination :in  (coremidi:entity-destinations entity)
                      :for key := (list  (coremidi:name device)
                                         (coremidi:name entity)
                                         (coremidi:name destination))
                      :for refnum := (sxhash key)
                      :do (setf (gethash refnum destinations) key
                                (gethash key    destinations) refnum))))
    (setf *sources-refnums* sources
          *destinations-refnums* destinations))
  (values))

(defun get-source-refnum (source-names)
  (values (gethash source-names *sources-refnums*)))

(defun get-destination-refnum (destination-names)
  (values (gethash destination-names *destinations-refnums*)))

(defun get-source-by-refnum (source-refnum)
  (let* ((names  (gethash source-refnum *sources-refnums*))
         (device (find (first names) (append (coremidi:devices)
                                             (coremidi:external-devices))
                       :test (function string=)
                       :key (function coremidi:name)))
         (entity (find (second names) (coremidi:device-entities device)
                       :test (function string=)
                       :key (function coremidi:name)))
         (port   (find (third names) (coremidi:entity-sources entity)
                       :test (function string=)
                       :key (function coremidi:name))))
    port))

(defun get-destination-by-refnum (destination-refnum)
  (let* ((names  (gethash destination-refnum *destinations-refnums*))
         (device (find (first names) (append (coremidi:devices)
                                             (coremidi:external-devices))
                       :test (function string=)
                       :key (function coremidi:name)))
         (entity (find (second names) (coremidi:device-entities device)
                       :test (function string=)
                       :key (function coremidi:name)))
         (port   (find (third names) (coremidi:entity-destinations entity)
                       :test (function string=)
                       :key (function coremidi:name))))
    port))





;; TODO: we must set a MidiShare refnum 0 application to shadow this:

(defvar *default-source-refnum*)
(defvar *default-destination-refnum*)

(defun set-default-refnums (source-refnum destination-refnum)
  (setf *default-source-refnum*      source-refnum
        *default-destination-refnum* destination-refnum))




;;----------------------------------------------------------------------



;;;
;;; To Know about MidiShare and Active Sessions
;;;

(defun MidiShare ()
  "returns true if MidiShare is installed"
  ;; hkt: The MCL version checked a macptr.
  (when *midiShare* 1))

(defun MidiGetVersion ()
  "Give MidiShare version as a fixnum. For example 131 as result, means : version 1.31"
  1200)

(defstruct midiapp
  refnum
  name
  (info 0)
  filter
  receive-alarm
  context-alarm
  receive-queue
  coremidi
  (port-state t)) ; when nil, Midi I/O is disabled.

(defstruct coremidi
  client
  output-port destination-endpoint
  input-port  source-endpoint)

(defun cm-client      (app) (coremidi-client      (midiapp-coremidi app)))
(defun cm-input-port  (app) (coremidi-input-port  (midiapp-coremidi app)))
(defun cm-output-port (app) (coremidi-output-port (midiapp-coremidi app)))


(defvar *midi-apps* '() "A list of midiapp.")

(defun MidiCountAppls ()
  "Give the number of MidiShare applications currently opened"
  (length *midi-apps*))

(defun MidiGetIndAppl (index)
  "Give the reference number of a MidiShare application from its index, a fixnum between 1 and (MidiCountAppls)"
  (when (<= 1 index (1- (length *midi-apps*)))
      (midiapp-refnum (elt *midi-apps*  (1- index)))
      (error "Invalid midi application index: ~A" index)))


(defun getapp (refnum)  (find refNum *midi-apps* :key (function midiapp-refnum)))

(defun MidiGetNamedAppl (name)
  "Give the reference number of a MidiShare application from its name"
  (find name *midi-apps* :key (function midiapp-name) :test (function string=)))


;;;
;;; To Open and Close a MidiShare session
;;;

(defun cm-client-notify (&rest args) (declare (ignore args)))
(defun cm-port-read     (&rest args) (declare (ignore args)))

(defun cm-close (cm)
  (when (coremidi-source-endpoint cm)
    (coremidi:port-disconnect-source (coremidi-input-port cm)
                                     (coremidi-source-endpoint cm)))
  (coremidi:port-dispose (coremidi-input-port  cm))
  (coremidi:port-dispose (coremidi-output-port cm))
  (coremidi:client-dispose (coremidi-client cm)))


#|

TODO: Upon first MidiOpen, we must create the midi application named
"MidiShare" refNum 0, corresponding to the physical Midi inputs and
outputs. Actually we'll configure by out of band means, one or two
midi entities of a midi devices as "MidiShare" application.  (If the
entity has both sources and destinations endpoints, then we can
connect to this single entity, otherwise we have to use two entities,
one for source, another for destination).

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defun find-endpoint-for-external-device (external-device &key (direction :source))
  "Finds a source or destination endpoint belonging to a device, that
is linked to some endpoint of this EXTERNAL-DEVICE."
  (check-type external-device coremidi:device)
  (check-type direction (member :source :input :destination :output))
  (flet ((not-connected (device)
           (error "Device named ~S doesn't seem to be connected" (coremidi:name device))))
    (let* ((inp (case direction
                  ((:source :input) t)
                  (otherwise        nil)))
           (entity-endpoints (if inp
                                 (function coremidi:entity-sources)
                                 (function coremidi:entity-destinations))))
      (with-functions (entity-endpoints)
        (first (mapcan (lambda (device)
                         (mapcan (lambda (entity)
                                   (let ((endpoints (find external-device (entity-endpoints entity)
                                                          :key (function coremidi:connected-devices)
                                                          :test (function member))))
                                     (when endpoints (list endpoints))))
                                 (coremidi:device-entities device)))
                       (delete-duplicates
                        (mapcan (lambda (x)
                                  (mapcan (function coremidi:connected-devices)
                                          (append (coremidi:entity-sources x)
                                                  (coremidi:entity-destinations x))))
                                (coremidi:device-entities external-device)))))))))

(defun find-endpoint-for-device (device &key (direction :source))
  "Finds a source or destination endpoint belonging to a device, that
is linked to some endpoint of this EXTERNAL-DEVICE."
  (check-type device coremidi:device)
  (check-type direction (member :source :input :destination :output))
  (flet ((not-connected (device)
           (error "Device named ~S doesn't seem to be connected" (name device))))
    (let* ((inp (case direction
                  ((:source :input) t)
                  (otherwise        nil)))
           (entity-endpoints (if inp
                                 (function coremidi:entity-sources)
                                 (function coremidi:entity-destinations))))
      (with-functions (entity-endpoints)
        (first (entity-endpoints
                (find-if (lambda (entity)
                           (and (coremidi:entity-sources      entity)
                                (coremidi:entity-destinations entity)))
                         (coremidi:device-entities device))))))))

(defun find-source-endpoint-for-device-named (name)
  (let ((device (coremidi:find-external-device-named name)))
    (if device
        (find-endpoint-for-external-device device :direction :source)
        (let ((device (coremidi:find-device-named name)))
          (when device
            (find-endpoint-for-device device :direction :source))))))

(defun find-destination-endpoint-for-device-named (name)
  (let ((device (coremidi:find-external-device-named name)))
    (if device
        (find-endpoint-for-external-device device :direction :destination)
        (let ((device (coremidi:find-device-named name)))
          (when device
            (find-endpoint-for-device device :direction :destination))))))
;;;
;;;;;;;;

(defun midishare-endpoints (&optional (device-name "Network"))
  (let ((source      (find-source-endpoint-for-device-named device-name))
        (destination (find-destination-endpoint-for-device-named device-name)))
    (coremidi:port-connect-source (cm-input-port (getapp 0)) source (coremidi:generate-refcon))
    (values destination
            source)))

(defvar *destination*)
(defvar *source*)

(defun MidiOpen (name)
  "Open a new MidiShare application, with name NAME. Give a unique reference number."
  (update-port-refnums)
  (let ((ref (1+ (reduce (function max) *midi-apps* :key (function midiapp-refnum) :initial-value 0))))
    (push (make-midiapp
           :refnum ref
           :name name
           :coremidi (let ((client (coremidi:client-create name 'cm-client-notify)))
                       (make-coremidi
                        :client  client
                        :output-port (coremidi:output-port-create client (format nil "~A-OUT" name))
                        :input-port  (coremidi:input-port-create  client (format nil "~A-IN"  name)
                                                                  'cm-port-read))))
          *midi-apps*)
    ref))

(defun MidiClose (refNum)
  "Close an opened MidiShare application from its reference number"
  (let ((app (getapp refNum)))
    (when app
      (cm-close (midiapp-coremidi app))))
  (deletef *midi-apps* refNum :key (function midiapp-refnum)))

;;;
;;; To Configure a MidiShare session
;;;
;;; HKT: check the f and p args for what these pointers are passing..

(defun MidiGetName (refNum)
  "Give the name of a MidiShare application from its reference number"
  (midiapp-name (getapp refNum)))

(defun MidiSetName (refNum name)
  "Change the name of a MidiShare application"
  ;; TODO: call Midi Alarms?
  (let ((app (getapp refNum)))
    (when app
      (setf (midiapp-name app) name)
      (let ((cm (midiapp-coremidi app)))
        (setf (coremidi:name (cm-client      cm)) name
              (coremidi:name (cm-input-port  cm)) (format nil "~A-IN"  name)
              (coremidi:name (cm-output-port cm)) (format nil "~A-OUT" name))))))

(defun MidiGetInfo (refNum)
  "Give the 32-bits user defined content of the info field of a MidiShare application. Analogous to window's refcon."
  (midiapp-info (getapp refNum)))

(defun MidiSetInfo (refNum p)
  "Set the 32-bits user defined content of the info field of a MidiShare application. Analogous to window's refcon."
  (setf (midiapp-info (getapp refNum)) p))


(defstruct filter
  (types (make-array 256 :element-type 'bit :initial-element 0))
  (ports (make-array 256 :element-type 'bit :initial-element 0))
  (chans (make-array  16 :element-type 'bit :initial-element 0)))

(defun MidiNewFilter ()
  "Returns a new filter"
  (make-filter))

(defun MidiFreeFilter (f)
  "Delete a filter"
  (declare (ignore f))
  (values))

(defun MidiAcceptChan (f c s)
  "Change the chan state of a filter"
  (setf (aref (filter-chans f) c) s))

(defun MidiAcceptType (f c s)
  "Change the type state of a filter"
  (setf (aref (filter-types f) c) s))

(defun MidiAcceptPort (f c s)
  "Change the port state of a filter"
  (setf (aref (filter-ports f) c) s))

(defun MidiIsAcceptedChan (f c)
  "Returns the chan state of a filter"
  (aref (filter-chans f) c))

(defun MidiIsAcceptedType (f c)
  "Returns the type state of a filter"
  (aref (filter-types f) c))

(defun MidiIsAcceptedPort (f c)
  "Returns the port state of a filter"
  (aref (filter-ports f) c))

(defun MidiGetFilter (refNum)
  "Give a pointer to the input filter record of a MidiShare application. Give NIL if no filter is installed"
  (midiapp-filter (getapp refNum)))

(defun MidiSetFilter (refNum p)
  "Install an input filter. The argument p is a pointer to a filter record."
  (setf (midiapp-filter (getapp refNum)) p))

(defun MidiGetRcvAlarm (refNum)
  "Get the adress of the receive alarm"
  (midiapp-receive-alarm (getapp refNum)))

(defun MidiSetRcvAlarm (refNum alarm)
  "Install a receive alarm"
  (setf (midiapp-receive-alarm (getapp refNum)) alarm))

(defun MidiGetApplAlarm (refNum)
  "Get the adress of the context alarm"
  (midiapp-context-alarm (getapp refNum)))

(defun MidiSetApplAlarm (refNum alarm)
  "Install a context alarm"
  (setf (midiapp-context-alarm (getapp refNum)) alarm))

;;;
;;; To Manage MidiShare IAC and Midi Ports
;;;


#|

MidiConnect
-----------

Connects two MidiApps.

An application has a coremidi:device -> coremidi:entities -> coremidi:sources and coremidi:destinations
MidiConnect connects a coremidi:source to a coremidi:destination of two applications.

|#

(defun MidiConnect (src dst s)
  (declare (ignorable s))
  "Connect or disconnect two MidiShare applications"
  ;;(#_MidiConnect src dst s)
  (warn "Not implemented yet: (~S ~S ~S ~S)" 'MidiConnect src dst s)
  ;; (if (ensure-boolean s)
  ;;     (midiapp-connect)
  ;;     (midiapp-disconnect))
  (if (zerop src)
      *default-source-refnum*
      src)
  (if (zerop dst)
      *default-destination-refnum*
      dst))

(defun MidiIsConnected (src dst)
  "Test if two MidiShare applications are connected"
  (declare (ignore src dst))
  ;;(#_MidiIsConnected src dst)
  )



#|

MidiPort
--------

We could map the MidiPort to the coremidi:devices, and the MidiShare
application would represent all the connected
MidiPort/coremidi:devices.

For now, we'll just hardwire a single port 0.

|#

(defun MidiGetPortState (port)
  "Give the state : open or closed, of a MidiPort"
  (when (zerop port)
    (midiapp-port-state (getapp 0))))

(defun MidiSetPortState (port state)
  "Open or close a MidiPort"
  (when (zerop port)
    (setf (midiapp-port-state (getapp 0)) (ensure-boolean state))))


;;;
;;; To Manage MidiShare events
;;;

(defun MidiFreeSpace ()
  "Amount of free MidiShare cells"
  most-positive-fixnum)

(defun MidiNewEv (typeNum)
  "Allocate a new MidiEvent"
  (make-midi-event :evtype typeNum))

(defun MidiCopyEv (ev)
  "Duplicate a MidiEvent"
  (let ((copy (copy-event ev)))
    (setf (midi-event-info copy) (copy-array (midi-event-info ev))
          (midi-event-additionnal-fields copy) (copy-array (midi-event-additionnal-fields ev)))
    copy))

(defun MidiFreeEv (ev)
  "Free a MidiEvent"
  (declare (ignore ev))
  (values))

(defun MidiSetField (ev f v)
  "Set a field of a MidiEvent"
  (let ((base (length (midi-event-info ev))))
    (cond
      ((< f base)
       (setf (aref (midi-event-info ev) f) v))
      ((null (midi-event-additionnal-fields ev))
       (setf (midi-event-additionnal-fields ev) (make-array (1+ f) :initial-element nil :adjustable t)
             (aref (midi-event-additionnal-fields ev) f) v))
      ((< (- f base) (length (midi-event-additionnal-fields ev)))
       (setf (aref (midi-event-additionnal-fields ev) (- f base)) v))
      (t
       (setf (midi-event-additionnal-fields ev) (adjust-array (midi-event-additionnal-fields ev) (1+ f))
             (aref (midi-event-additionnal-fields ev) (- f base)) v)))))

(defun MidiGetField (ev f)
  "Get a field of a MidiEvent"
  (let ((base (length (midi-event-info ev))))
    (cond
      ((< f base)
       (aref (midi-event-info ev) f))
      ((null (midi-event-additionnal-fields ev))
       0)
      ((< (- f base) (length (midi-event-additionnal-fields ev)))
       (aref (midi-event-additionnal-fields ev) (- f base)))
      (t
       0))))

(defun MidiAddField (ev v)
  "Append a field to a MidiEvent (only for sysex and stream)"
  (cond
    ((null (midi-event-additionnal-fields ev))
     (setf (midi-event-additionnal-fields ev) (make-array 1 :initial-element nil :adjustable t)
           (aref (midi-event-additionnal-fields ev) 0) v))
    (t
     (setf (midi-event-additionnal-fields ev) (adjust-array (midi-event-additionnal-fields ev) (1+ (length (midi-event-additionnal-fields ev))))
           (aref (midi-event-additionnal-fields ev) (1- (length (midi-event-additionnal-fields ev)))) v))))

(defun MidiCountFields (ev)
  "The number of fields of a MidiEvent"
  (cond
    ((null (midi-event-additionnal-fields ev))
     (length (midi-event-info ev)))
    (t
     (+ (length (midi-event-info ev)) (length (midi-event-additionnal-fields ev))))))


;;;
;;; To Manage MidiShare Sequences
;;;

(defun MidiNewSeq ()
  "Allocate an empty sequence"
  (make-seq))

(defun MidiAddSeq (seq ev)
  "Add an event to a sequence"
  (if (lastev seq)
      (setf (midi-event-link (lastev seq)) ev
            (seq-lastev seq) ev
            (midi-event-link ev) nil)
      (setf (seq-firstev seq) ev
            (seq-lastev seq) ev
            (midi-event-link ev) nil)))

(defun MidiFreeSeq (seq)
  "Free a sequence and its content"
  (declare (ignore seq))
  (values))

(defun MidiClearSeq (seq)
  "Free only the content of a sequence. The sequence become empty"
  (setf (seq-firstev seq) nil
        (seq-lastev seq) nil))

(defun MidiApplySeq (seq proc)
  "Call a function for every events of a sequence"
  (loop
    :for ev = (seq-firstev seq) :then (midi-event-link ev)
    :while ev
    :do (funcall proc ev)))

;;;
;;; MidiShare Time
;;;

(declaim (inline midi-get-time MidiGetTime))

(defun MidiGetTime ()
  "give the current time"
  (values (truncate (* 10 (ui::timestamp)) (/ ui::+tick-per-second+))))

(defun midi-get-time ()
  ;; TODO
  (midigettime))


;;;
;;; To Send MidiShare events
;;;

(defun MidiSendIm (refNum ev)
  ;; TODO
  "send an event now"
  (midisendat refnum ev (MidiGetTime)))

(defun MidiSend (refNum ev)
  "send an event using its own date"
  (midisendat refnum ev (midi-event-date ev)))

(defun MidiSendAt (refNum ev date)
  "send an event at date <date>"
  (declare (ignore refNum ev date))
  ;; TODO
  )

;;;
;;;  To Receive MidiShare Events
;;;

(defun MidiCountEvs (refNum)
  "Give the number of events waiting in the reception fifo"
  (length (midiapp-receive-queue (getapp refNum))))

(defun MidiGetEv (refNum)
  "Read an event from the reception fifo"
  (pop (midiapp-receive-queue (getapp refNum))))

(defun MidiAvailEv (refNum)
  "Get a pointer to the first event in the reception fifo without removing it"
  (first (midiapp-receive-queue (getapp refNum))))

(defun MidiFlushEvs (refNum)
  "Delete all the events waiting in the reception fifo"
  (setf (midiapp-receive-queue (getapp refNum)) nil))


;;;
;;; Realtime Tasks
;;;

#+not-yet(
          (defun MidiCall (proc date refNum arg1 arg2 arg3)
            "Call the routine <proc> at date <date> with arguments <arg1> <arg2> <arg3>"
            (#_MidiCall proc date refNum arg1 arg2 arg3))

          (defun MidiTask (proc date refNum arg1 arg2 arg3)
            "Call the routine <proc> at date <date> with arguments <arg1> <arg2> <arg3>. Return a pointer to the corresponding typeProcess event"
            (#_MidiTask proc date refNum arg1 arg2 arg3))

          (defun MidiDTask (proc date refNum arg1 arg2 arg3)
            "Call the routine <proc> at date <date> with arguments <arg1> <arg2> <arg3>. Return a pointer to the corresponding typeDProcess event"
            (#_MidiDTask proc date refNum arg1 arg2 arg3))

          (defun MidiForgetTaskHdl (thdl)
            "Forget a previously scheduled typeProcess or typeDProcess event created by MidiTask or MidiDTask"
            (#_MidiForgetTask thdl))

          (defun MidiForgetTask (ev)
            "Forget a previously scheduled typeProcess or typeDProcess event created by MidiTask or MidiDTask"
            (ccl:without-interrupts
              (ccl:%stack-block ((taskptr 4))
                (setf (ccl:%get-ptr taskptr) ev)
                (MidiForgetTask taskptr))))

          (defun MidiCountDTasks (refNum)
            "Give the number of typeDProcess events waiting"
            (#_MidiCountDTasks refNum))

          (defun MidiFlushDTasks (refNum)
            "Remove all the typeDProcess events waiting"
            (#_MidiFlushDTasks refNum))

          (defun MidiExec1DTask (refNum)
            "Call the next typeDProcess waiting"
            (#_MidiExec1DTask refNum))
          )

;;;
;;; Low Level MidiShare Memory Management
;;;

#+not-needed(
             (defun MidiNewCell ()
               "Allocate a basic Cell"
               (#_MidiNewCell))

             (defun MidiFreeCell (cell)
               "Delete a basic Cell"
               (#_MidiFreeCell cell))

             (defun MidiTotalSpace ()
               "Total amount of Cells"
               (#_MidiTotalSpace))

             (defun MidiGrowSpace (n)
               "Total amount of Cells"
               (#_MidiGrowSpace n))
             )

;;;
;;; SMPTE Synchronisation functions
;;;
#+not-needed(
             (defun MidiGetSyncInfo (syncInfo)
               "Fill syncInfo with current synchronisation informations"
               (#_MidiGetSyncInfo syncInfo))

             (defun MidiSetSyncMode (mode)
               "set the MidiShare synchroniation mode"
               (#_MidiSetSyncMode mode))

             (defun MidiGetExtTime ()
               "give the current external time"
               (#_MidiGetExtTime))

             (defun MidiInt2ExtTime (time)
               "convert internal time to external time"
               (#_MidiInt2ExtTime time))

             (defun MidiExt2IntTime (time)
               "convert internal time to external time"
               (#_MidiExt2IntTime time))

             (defun MidiTime2Smpte (time format smpteLocation)
               "convert time to Smpte location"
               (#_MidiTime2Smpte time format smpteLocation))

             (defun MidiSmpte2Time (smpteLocation)
               "convert time to Smpte location"
               (#_MidiSmpte2Time smpteLocation))
             )

;;;
;;; Drivers functions
;;;

#+not-needed(
             (defun MidiCountDrivers ()
               "number of opened drivers"
               (#_MidiCountDrivers))

             (defun MidiGetIndDriver (index)
               "Give the reference number of a MidiShare driver from its index, a fixnum"
               (#_MidiGetIndDriver index))

             (defun MidiGetDriverInfos (refNum info)
               "Give information about a driver"
               (#_MidiGetDriverInfos refNum info))

;;;
;;; BUG! For some reaon the MidiGetIndSLot decl in Midishare.h:
;;;   SlotRefNum MidiGetIndSlot (short refnum, short index);
;;; gets translated as this in the Midishare.{ffi,lisp} files:
;;;   (ccl::define-external-function (MIDIGETINDSLOT "MidiGetIndSlot")
;;;     (:<s>lot<r>ef<n>um (:signed 16) (:signed 16) )
;;;     :void )
;;; which is wrong. I'm not sure what to do about it yet... --hkt
;;;

             (defun MidiGetIndSlot (refNum index)
               "Give the reference number of a driver slot from its order number."
               (error "Fix me.")
               (#_MidiGetIndSlot :bogus-arg refNum index))

             (defun MidiGetSlotInfos (slotRefNum info)
               "Give information about a slot"
               (#_MidiGetSlotInfos slotRefNum info))

             (defun MidiConnectSlot (port slotRefNum state)
               "Make or remove a connection between a slot and a MidiShare logical port"
               (#_MidiConnectSlot port slotRefNum state))

             (defun MidiIsSlotConnected  (port slotRefNum)
               "Test a connection between a slot and a MidiShare logical port"
               (#_MidiIsSlotConnected port slotRefNum))

             (defun MidiNewSmpteLocation ()
               (ccl::make-record :<ts>mpte<l>ocation))

             (defun MidiFreeSmpteLocation (location)
               (ccl::free location))

             (defun MidiNewSyncInfo ()
               (ccl::make-record :<ts>ync<i>nfo))

             (defun MidiFreeSyncInfo (location)
               (ccl::free  location))

             )


;;;---------------------------------------------------------------------
;;; midi player
;;;---------------------------------------------------------------------

;;===============================
;; Date structures ond constants
;;===============================

;;--------------------------------------------------------------------------
;; Player state
;;--------------------------------------------------------------------------

(defconstant kIdle       0)
(defconstant kPause      1)
(defconstant kRecording  2)
(defconstant kPlaying    3)
(defconstant kWaiting    4)


;;--------------------------------------------------------------------------
;; Tracks state
;;--------------------------------------------------------------------------

(defconstant kMaxTrack  256)
(defconstant kMuteOn  1)
(defconstant kMuteOff 0)
(defconstant kSoloOn  1)
(defconstant kSoloOff 0)
(defconstant kMute  0)
(defconstant kSolo  1)

;;--------------------------------------------------------------------------
;; Recording management
;;--------------------------------------------------------------------------

(defconstant kNoTrack           -1)
(defconstant kEraseMode         1)
(defconstant kMixMode           0)

;;--------------------------------------------------------------------------
;; Loop  management
;;--------------------------------------------------------------------------

(defconstant kLoopOn    0)
(defconstant kLoopOff   1)

;;--------------------------------------------------------------------------
;; Step Playing
;;--------------------------------------------------------------------------

(defconstant kStepPlay  1)
(defconstant kStepMute  0)

;;--------------------------------------------------------------------------
;; Synchronisation
;;--------------------------------------------------------------------------

(defconstant kInternalSync      0)
(defconstant kClockSync         1)
(defconstant kSMPTESync         2)
(defconstant kExternalSync      3)

(defconstant kNoSyncOut 0)
(defconstant kClockSyncOut 1)

;;--------------------------------------------------------------------------
;; MIDIfile
;;--------------------------------------------------------------------------

(defconstant midifile0  0)
(defconstant midifile1  1)
(defconstant midifile2  2)

(defconstant TicksPerQuarterNote     0)
(defconstant Smpte24                 24)
(defconstant Smpte25                 25)
(defconstant Smpte29                 29)
(defconstant Smpte30                 30)


;;--------------------------------------------------------------------------
;; Errors  :  for MidiFile
;;--------------------------------------------------------------------------

(defconstant noErr               0)   ;; no error
(defconstant ErrOpen             1)   ;; file open error
(defconstant ErrRead             2)   ;; file read error
(defconstant ErrWrite            3)   ;; file write error
(defconstant ErrVol              4)   ;; Volume error
(defconstant ErrGetInfo          5)   ;; GetFInfo error
(defconstant ErrSetInfo          6)   ;; SetFInfo error
(defconstant ErrMidiFileFormat   7)   ;; bad MidiFile format

;;--------------------------------------------------------------------------
;; Errors  : for the player
;;--------------------------------------------------------------------------

(defconstant PLAYERnoErr           -1)  ;; No error
(defconstant PLAYERerrAppl         -2)  ;; Unable to open MidiShare app
(defconstant PLAYERerrEvent        -3)  ;; No more MidiShare Memory
(defconstant PLAYERerrMemory       -4)  ;; No more Mac Memory
(defconstant PLAYERerrSequencer    -5)  ;; Sequencer error



(defvar *player-framework* nil)

(defun player-framework ()
  (or *player-framework*
      (setf *player-framework* 'player-framework)))

(defstruct* (pos (:conc-name p-))
    (bar
     beat
     unit))

(defstruct* (player-state (:conc-name s-))
    ((bar 0)
     (beat 0)
     (unit 0)
     (date 0)
     (tempo 0)
     (num 0)
     (denom 0)
     (click 0)
     (quater 0)
     (state kIdle)
     (syncin 0)
     (syncout 0)))

(define-with-temporary-macro with-temporary-player-state    (make-player-state))

(defstruct* (midi-file-infos (:conc-name mf-))
    (format timedef clicks tracks))


(define-with-temporary-macro with-temporary-midi-file-infos (make-midi-file-infos))



(defstruct player
  refnum
  name
  info)

(defvar *players* '() "A list of players.")

(defun getplayer (refnum)  (find refNum *players* :key (function player-refnum)))
(defun getplayernamed (name)  (find name *players* :key (function player-name) :test (function string=)))

(defun OpenPlayer (name)
  (if (getplayernamed name)
      (error "There is already a player named ~S" name)
      (let ((refnum (1+ (reduce (function max) *players* :key (function player-refnum) :initial-value 0))))
        (push (make-player :refnum refnum :name name) *players*)
        refnum)))

(defun ClosePlayer (refNum)
  (deletef *players* (getplayer refnum)))

(defun open-player (name)
  (format t "~S~%" (list 'open-player name))
  ;; TODO
  (OpenPlayer name))

;;===================
;; Transport control
;;===================

(defun StartPlayer (refNum)
  ;; TODO
  (format t "~S~%" (list 'startplayer refnum)))

(defun ContPlayer (refNum)
  (format t "~S~%" (list 'contplayer refnum)))

(defun StopPlayer (refNum)
  ;; TODO
  (format t "~S~%" (list 'StopPlayer refnum)))

(defun PausePlayer (refNum)
  (format t "~S~%" (list 'pauseplayer refnum)))

;;===================
;; Record management
;;===================

(defun SetRecordModePlayer (refNum state)
  ;; TODO
  (format t "~S~%" (list 'SetRecordModePlayer refnum state)))

(defun RecordPlayer (refNum tracknum)
  ;; TODO
  (format t "~S~%" (list 'RecordPlayer refnum tracknum)))

(defun SetRecordFilterPlayer (refNum filter)
  ;; TODO
  (format t "~S~%" (list 'SetRecordFilterPlayer refnum filter)))

;;=====================
;; Position management
;;=====================

(defun SetPosBBUPlayer (refNum pos)
  ;; TODO
  (format t "~S~%" (list 'SetPosBBUPlayer refnum pos)))

(defun SetPosMsPlayer (refNum date-ms)
  ;; TODO
  (format t "~S~%" (list 'SetPosMsPlayer refnum date-ms)))

;;==================
;; Loop management
;;==================

(defun SetLoopPlayer (refNum state)
  ;; TODO
  (format t "~S~%" (list 'SetLoopPlayer refnum state)))

(defun SetLoopStartBBUPlayer (refNum pos)
  ;; TODO
  (format t "~S~%" (list 'SetLoopStartBBUPlayer refnum pos)))

(defun SetLoopEndBBUPlayer (refNum pos)
  ;; TODO
  (format t "~S~%" (list 'SetLoopEndMsPlayer refnum pos)))

(defun SetLoopStartMsPlayer (refNum date-ms)
  ;; TODO
  (format t "~S~%" (list 'SetLoopStartMsPlayer refnum date-ms)))

(defun SetLoopEndMsPlayer (refNum date-ms)
  ;; TODO
  (format t "~S~%" (list 'SetLoopEndMsPlayer refnum date-ms)))

;;============================
;; Synchronisation management
;;============================

(defun SetSynchroInPlayer (refNum state)
  ;; TODO
  (format t "~S~%" (list 'SetSynchroInPlayer refnum state)))

(defun SetSynchroOutPlayer (refNum state)
  ;; TODO
  (format t "~S~%" (list 'SetSynchroOutPlayer refnum state)))

(defun SetSMPTEOffsetPlayer (refNum smptepos)
  ;; TODO
  (format t "~S~%" (list 'SetSMPTEOffsetPlayer refnum smptepos)))

(defun SetTempoPlayer (refNum tempo)
  ;; TODO
  (format t "~S~%" (list 'SetTempoPlayer refnum tempo)))

;;===================
;; State management
;;===================

(defun GetStatePlayer (refNum playerstate)
  ;; TODO
  (format t "~S~%" (list 'GetStatePlayer refnum playerstate)))

(defun GetEndScorePlayer (refNum playerstate)
  ;; TODO
  (format t "~S~%" (list 'GetEndScorePlayer refnum playerstate)))

;;==============
;; Step playing
;;==============

(defun ForwardStepPlayer (refNum flag)
  ;; TODO
  (format t "~S~%" (list 'ForwardStepPlayer refnum flag)))

(defun BackwardStepPlayer (refNum flag)
  ;; TODO
  (format t "~S~%" (list 'BackwardStepPlayer refnum flag)))

;;====================
;; Tracks management
;;====================

(defun GetAllTrackPlayer (refNum)
  ;; TODO
  (format t "~S~%" (list 'GetAllTrackPlayer refnum)))

(defun GetTrackPlayer (refNum tracknum)
  ;; TODO
  (format t "~S~%" (list 'GetTrackPlayer refnum tracknum)))

(defun SetTrackPlayer (refNum tracknum seq)
  ;; TODO
  (format t "~S~%" (list 'SetTrackPlayer refnum tracknum seq)))

(defun SetAllTrackPlayer (refNum seq tpq)
  ;; TODO
  (format t "~S~%" (list 'SetAllTrackPlayer refnum seq tpq)))

(defun SetParamPlayer (refNum track param val)
  ;; TODO
  (format t "~S~%" (list 'SetParamPlayer refnum track param val)))

(defun InsertAllTrackPlayer (refNum seq)
  ;; TODO
  (format t "~S~%" (list 'InsertAllTrackPlayer refnum seq)))

(defun InsertTrackPlayer (refNum track seq)
  ;; TODO
  (format t "~S~%" (list 'InsertTrackPlayer refnum track seq)))


;;====================
;; Midifile management
;;====================


(defun make-note-messages (event)
  "Convert a midishare typeNote event into a list of two midi note-on-message and note-off-message."
  (assert (= typeNote (evtype event)))
  (let ((date (date event))
        (chan (chan event)))
    (list
     (make-instance 'midi:note-on-message
                    :time date
                    :status (+ #x90 chan)
                    :channel chan
                    :key (pitch event)
                    :velocity (vel event))
     (make-instance 'midi:note-off-message
                    :time (+ date (dur event))
                    :status (+ #x80 chan)
                    :channel chan
                    :key (pitch event)
                    :velocity (vel event)))))


(defun convert-midishare-event-to-midi-message (event)
  "Convert the midishare event into a list of midi messages."
  (let ((date (date event))
        (chan (chan event)))
    (case (evtype event)

      ((#.typeNote) #| note with pitch, velocity and duration |#
       (make-note-messages event))

      ((#.typeKeyOn) #| key on with pitch and velocity |#
       (list (let ((class 'midi::note-on-message))
               (make-instance class
                              :time date
                              :status (+ (midi:status-min class) chan)
                              :channel chan
                              :key (pitch event)
                              :velocity (vel event)))))

      ((#.typeKeyOff) #| key off with pitch and velocity |#
       (list (let ((class 'midi::note-off-message))
               (make-instance class
                              :time date
                              :status (+ (midi:status-min class) chan)
                              :channel chan
                              :key (pitch event)
                              :velocity (vel event)))))

      ((#.typeKeyPress) #| key pressure with pitch and pressure value |#
       (list (let ((class 'midi::polyphonic-key-pressure-message))
               (make-instance class
                              :time date
                              :status (+ (midi:status-min class) chan)
                              :channel chan
                              :key (pitch event)
                              :pressure (kpress event)))))

      ((#.typeCtrlChange) #| control change with control number and control value |#
       (list (let ((class 'midi:control-change-message))
               (make-instance class
                              :time date
                              :status (+ (midi:status-min class) chan)
                              :channel chan
                              :controller (ctrl event)
                              :value (valint event)))))

      ((#.typeProgChange) #| program change with program number |#
       (list (let ((class 'midi:program-change-message))
               (make-instance class
                              :time date
                              :status (+ (midi:status-min class) chan)
                              :channel chan
                              :program (pgm event)))))

      ((#.typeChanPress) #| channel pressure with pressure value |#
       (list (let ((class 'midi:channel-pressure-message))
               (make-instance class
                              :time date
                              :status (+ (midi:status-min class) chan)
                              :channel chan
                              :pressure (kpress event)))))

      ((#.typePitchBend) #| pitch bender with lsb and msb of the 14-bit value |#
       (list (let ((class 'midi:pitch-bend-message))
               (make-instance class
                              :time date
                              :status (+ (midi:status-min class) chan)
                              :channel chan
                              :value (valint event)))))

      ((#.typeSongPos) #| song position with lsb and msb of the 14-bit position |#)
      ((#.typeSongSel) #| song selection with a song number |#)

      ((#.typeClock) #| clock request (no argument) |#
       (list (make-instance 'midi:timing-clock-message :time date)))

      ((#.typeStart) #| start request (no argument) |#
       (list (make-instance 'midi:start-sequence-message :time date)))

      ((#.typeContinue) #| continue request (no argument) |#
       (list (make-instance 'midi:continue-sequence-message :time date)))

      ((#.typeStop) #| stop request (no argument) |#
       (list (make-instance 'midi:stop-sequence-message :time date)))

      ((#.typeTune) #| tune request (no argument) |#
       (list (make-instance 'midi:tune-request-message :time date)))

      ((#.typeActiveSens) #| active sensing code (no argument) |#
       (list (make-instance 'midi:active-sensing-message :time date)))

      ((#.typeReset) #| reset request (no argument) |#
       (list (let ((class 'midi:reset-all-controllers-message))
               (make-instance class
                              :time date
                              :status (+ (midi:status-min class) chan)
                              :channel chan))))

      ((#.typeSysEx) #| system exclusive with any number of data bytes. Leading $F0 and tailing $F7 are automatically supplied by MidiShare and MUST NOT be included by the user |#
       'midi:system-exclusive-message
       nil)

      ((#.typeStream) #| special event with any number of unprocessed data/status bytes |#)
      ((#.typePrivate) #| private event for internal use with 4 32-bits arguments |#)
      ((#.typeProcess) #| interrupt level task with a function adress and 3 32-bits args |#)
      ((#.typeDProcess) #| foreground task with a function address and 3 32-bits arguments |#)
      ((#.typeQFrame) #| quarter frame message with a type from 0 to 7 and a value |#)
      ((#.typeCtrl14b))
      ((#.typeNonRegParam))
      ((#.typeRegParam))

      ((#.typeSeqNum)
       (list (make-instance 'midi:sequence-number-message
                            :time date
                            :sequence (num event))))

      ((#.typeTextual)
       (list (make-instance 'midi:general-text-message
                            :time date
                            :text (text event))))

      ((#.typeCopyright)
       (list (make-instance 'midi:copyright-message
                            :time date
                            :text (text event))))

      ((#.typeSeqName)
       (list (make-instance 'midi:sequence/track-name-message
                            :time date
                            :text (text event))))

      ((#.typeInstrName)
       (list (make-instance 'midi:instrument-message
                            :time date
                            :text (text event))))

      ((#.typeLyric)
       (list (make-instance 'midi:lyric-message
                            :time date
                            :text (text event))))

      ((#.typeMarker)
       (list (make-instance 'midi:marker-message
                            :time date
                            :text (text event))))

      ((#.typeCuePoint)
       (list (make-instance 'midi:cue-point-message
                            :time date
                            :text (text event))))

      ((#.typeChanPrefix)
       (list (make-instance 'midi:channel-prefix-message
                            :time date
                            :channel chan)))

      ((#.typeEndTrack)
       (list (make-instance 'midi:end-of-track-message)))

      ((#.typeTempo)
       (list (let ((class 'midi:tempo-message))
               (make-instance class
                              :time date
                              :status (midi:status-min class)
                              :tempo (tempo event)))))

      ((#.typeSMPTEOffset)
       #-(and) (list (make-instance 'midi:smpte-offset-message
                                    :time date
                                    :hr
                                    :mn
                                    :se
                                    :fr
                                    :ff)))

      ((#.typeTimeSign)
       'midi:time-signature-message
       )
      ((#.typeKeySign)
       'midi:key-signature-message
       )

      ((#.typeSpecific))
      ((#.typePortPrefix))
      ((#.typeRcvAlarm))
      ((#.typeApplAlarm))
      (otherwise
       ))))


(defun convert-text (message eventType)
  (let ((ev (make-midi-event :evtype eventType
                             :date (midi:message-time message))))
    (text ev (midi:message-text message))
    ev))


(defgeneric convert-midi-message-to-midishare-event (message)

  (:method (message)
    (declare (ignorable message))
    nil)

  (:method ((message midi::note-on-message))
    (let ((ev (make-midi-event :evtype typeKeyOn
                               :date (midi:message-time message)
                               :port 0
                               :chan (midi:message-channel message))))
      (pitch ev (midi:message-key message))
      (vel   ev (midi:message-velocity message))
      ev))

  (:method ((message midi::note-off-message))
    (let ((ev (make-midi-event :evtype typeKeyOff
                               :date (midi:message-time message)
                               :port 0
                               :chan (midi:message-channel message))))
      (pitch ev (midi:message-key message))
      (vel   ev (midi:message-velocity message))
      ev))

  (:method ((message midi::polyphonic-key-pressure-message))
    (let ((ev (make-midi-event :evtype typeKeyPress
                               :date (midi:message-time message)
                               :port 0
                               :chan (midi:message-channel message))))
      (pitch  ev (midi:message-key message))
      (kpress ev (midi:message-pressure message))
      ev))

  (:method ((message midi:control-change-message))
    (let ((ev (make-midi-event :evtype typeCtrlChange
                               :date (midi:message-time message)
                               :port 0
                               :chan (midi:message-channel message))))
      (ctrl   ev (midi:message-controller message))
      (valint ev (midi:message-value message))
      ev))

  (:method ((message midi:program-change-message))
    (let ((ev (make-midi-event :evtype typeProgChange
                               :date (midi:message-time message)
                               :port 0
                               :chan (midi:message-channel message))))
      (pgm ev (midi:message-program message))
      ev))

  (:method ((message midi:channel-pressure-message))
    (let ((ev (make-midi-event :evtype typeChanPress
                               :date (midi:message-time message)
                               :port 0
                               :chan (midi:message-channel message))))
      (kpress ev (midi:message-pressure message))
      ev))

  (:method ((message midi:pitch-bend-message))
    (let ((ev (make-midi-event :evtype typePitchBend
                               :date (midi:message-time message)
                               :port 0
                               :chan (midi:message-channel message))))
      (valint ev (midi:message-value message))
      ev))

  (:method ((message midi:timing-clock-message))
    (make-midi-event :evtype typeClock :date (midi:message-time message) :port 0))

  (:method ((message midi:start-sequence-message))
    (make-midi-event :evtype typeStart :date (midi:message-time message) :port 0))

  (:method ((message midi:continue-sequence-message))
    (make-midi-event :evtype typeContinue :date (midi:message-time message) :port 0))

  (:method ((message midi:stop-sequence-message))
    (make-midi-event :evtype typeStop :date (midi:message-time message) :port 0))

  (:method ((message midi:tune-request-message))
    (make-midi-event :evtype typeTune :date (midi:message-time message) :port 0))

  (:method ((message midi:active-sensing-message))
    (make-midi-event :evtype typeActiveSens :date (midi:message-time message) :port 0))

  (:method ((message midi:reset-all-controllers-message))
    (make-midi-event :evtype typeReset
                     :date (midi:message-time message)
                     :port 0
                     :chan (midi:message-channel message)))

  (:method ((message midi:sequence-number-message))
    (let ((ev (make-midi-event :evtype typeSeqNum
                               :date (midi:message-time message)
                               :port 0
                               :chan (midi:message-channel message))))
      (num ev (midi:message-sequence message))
      ev))

  (:method ((message midi:general-text-message))        (convert-text message typeTextual))
  (:method ((message midi:copyright-message))           (convert-text message typeCopyright))
  (:method ((message midi:sequence/track-name-message)) (convert-text message typeSeqName))
  (:method ((message midi:instrument-message))          (convert-text message typeInstrName))
  (:method ((message midi:lyric-message))               (convert-text message typeLyric))
  (:method ((message midi:marker-message))              (convert-text message typeMarker))
  (:method ((message midi:cue-point-message))           (convert-text message typeCuePoint))

  (:method ((message midi:channel-prefix-message))
    (make-midi-event :evtype typeChanPrefix
                     :date (midi:message-time message)
                     :port 0
                     :chan (midi:message-channel message)))

  (:method ((message midi:end-of-track-message))
    (make-midi-event :evtype typeEndTrack :date (midi:message-time message)))

  (:method ((message midi:tempo-message))
    (let ((ev (make-midi-event :evtype typeTempo
                               :date (midi:message-time message))))
      (tempo ev (midi:message-tempo message))
      ev)))


(defun MidiFileSave (name seq info)
  (midi:write-midi-file
   (make-instance 'midi:midifile
                  :format   (mf-format info)
                  :division (mf-clicks info)
                  :tracks   (list (stable-sort (loop
                                                 :for ev = (firstev seq) :then (link ev)
                                                 :while ev
                                                 :append (convert-midishare-event-to-midi-message ev))
                                               (function <)
                                               :key (function midi:message-time))))
   name))

(defun MidiFileLoad (name seq info)
  (let* ((mf (midi:read-midi-file name))
         (track (first (if (zerop (midi:midifile-format mf))
                           (midi:midifile-tracks mf)
                           (midi::format1-tracks-to-format0-tracks (midi:midifile-tracks mf))))))
    (mf-format info (midi:midifile-format mf))
    ;; (mf-timedef info ?)
    (mf-clicks info (midi:midifile-division mf))
    (mf-tracks info 1)
    (format t "Read ~D MIDI events.~%" (length track))
    (dolist (mev track)
      (let ((ev (convert-midi-message-to-midishare-event mev)))
        (when ev
          (seq-add-event seq ev))))
    seq))


#-(and) (
         (let ((seq (make-seq))
               (info (make-midi-file-infos)))
           (midifileload #P"~/Documents/Patchwork/PW-user-patches/B/ afrik midi/167"
                         seq info)
           seq)

         (inspect (midi:read-midi-file #P"~/Documents/Patchwork/PW-user-patches/B/ afrik midi/167"))
         )

(defun midi-file-save (name seq info)
  ;; TODO
  (MidiFileSave  name seq info))

(defun midi-file-load (name seq info)
  ;; TODO
  (MidiFileLoad  name seq info))

(defun MidiNewMidiFileInfos ()
  (make-midi-file-infos))

(defun MidiFreeMidiFileInfos (info)
  (declare (ignore info))
  (values))

(defun MidiNewPlayerState ()
  (make-player-state))

(defun MidiFreePlayerState (state)
  (declare (ignore state))
  (values))

(defun MidiNewPos ()
  (make-pos))

(defun MidiFreePos (pos)
  (declare (ignore pos))
  (values))


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
    (cond ((eql chan t)    (dotimes (i 16) (midiacceptchan f i 1)))
          ((numberp chan) (midiacceptchan f chan 1))
          (t              (dolist (i chan) (midiacceptchan f i 1))))
    (cond ((eql type t)    (dotimes (i 256) (midiaccepttype f i 1)))
          ((numberp type) (midiaccepttype f type 1))
          (t              (dolist (i type) (midiaccepttype f i 1))))
    (cond ((eql port t)    (dotimes (i 256) (midiacceptport f i 1)))
          ((numberp port) (midiacceptport f port 1))
          (t              (dolist (i port) (midiacceptport f i 1))))
    f))


(defmethod midi-free-filter ((f t))
  (unless (nullptrp f)
    (midifreefilter f)))


(defmethod midi-modify-filter ((f t) &key accept chan port type)
  (unless (nullptrp f)
    (let ((accept (if accept 1 0)))
      (cond ((eql chan t)   (dotimes (i 16) (midiacceptchan f i accept)))
            ((numberp chan) (midiacceptchan f chan accept))
            (t              (dolist (i chan) (midiacceptchan f i accept))))
      (cond ((eql type t)   (dotimes (i 256) (midiaccepttype f i accept)))
            ((numberp type) (midiaccepttype f type accept))
            (t              (dolist (i type) (midiaccepttype f i accept))))
      (cond ((eql port t)   (dotimes (i 256) (midiacceptport f i accept)))
            ((numberp port) (midiacceptport f port accept))
            (t              (dolist (i port) (midiacceptport f i accept)))))))



;;;;Open MidiShare and connections
(defun midi-open ()
  ;; TODO
  (midi-close)
  (setf *pw-refnum* nil)
  (setf *player* nil)
  (if (setf *midi-share?* (midishare))
      (progn
        (setf *pw-refnum* (midiopen "PatchWork"))
        (setf *filter* (midi-new-filter :chan t :port t :type t))
        (midisetfilter *pw-refnum*  *filter*)
        (midi-modify-filter *filter* :accept nil :type t)
        (MidiConnect *pw-refnum* 0 1)
        (MidiConnect 0 *pw-refnum* 1)
        (setq *player* (open-player "PatchWorkPlayer"))
        (values *pw-refnum* *player*))
      (format *error-output* "~&MidiShare not present. PatchWork won't play Midi.~%")))


;;;;Close MidiShare and off the scheduler
(defun midi-close ()
  ;; TODO
  (when *pw-refnum*
    (when *player* (closeplayer *player*))
    (when *filter* (midi-free-filter *filter*))
    (when *pw-refnum* (MidiClose *pw-refnum*))))


;;;;MidiWrite
(defun midi-write-time (event time)
  ;; TODO
  (when *pw-refnum*
    (MidiSendAt *pw-refnum* event time)))

(defun midi-write (event)
  ;; TODO
  (midi-write-time event (MidiGetTime)))


;;;;Midi-read
(defun midi-read ()
  ;; TODO
  (ui::without-interrupts
    (let ((ev (MidiGetEv *pw-refnum*)))
      (loop :named eat-clock-events
            :while (and ev (not (nullptrp ev)) (= (evtype ev) typeClock))
            :do (setf ev (MidiGetEv *pw-refnum*)))
      (unless (nullptrp ev)
        ev))))


;;;;Midi-clear - Flush the MidiShare's events.
(defun midi-clear ()
  (MidiFlushEvs *pw-refnum*))


(defconstant tick (/ internal-time-units-per-second 60))

;;;;clock-time - return the current time of. The time is expressed in ticks.
(defun clock-time ()
  (values (round (MidiGetTime) 10)))


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
  ;; TODO
  (MidiFlushEvs *pw-refnum*)
  (midi-notes-off))


;;;---------------------------------------------------------------------
;;;
;;; To Install and Remove the MidiShare Interface
;;;

(defun install-midishare-interface ()
  (unless (coremidi:coreaudio-framework) (error "Missing CoreAudio"))
  (unless (coremidi:coremidi-framework)  (error "Missing CoreMIDI"))
  (setf *midiShare* t)
  (unless (midishare) (error "MidiShare not installed")))

(defun remove-midishare-interface ()
  (setf coremidi:*coremidi*  nil)
  (setf coremidi:*coreaudio* nil)
  (setf *midiShare*          nil))

(on-quit midi/quit
  (midi-close)
  (remove-midishare-interface))





;; (eval-when (:load-toplevel :execute)  (pushnew ':midishare *features*))


;;;; THE END ;;;;


(defvar *effects* '())
(defun call-effects (&rest arguments)
  (mapc (lambda (effect)
          (block effect
            (handler-bind
                ((error (lambda (err)
                          (terpri *error-output*)
                          (mclgui::print-backtrace *error-output*)
                          (format *error-output* "~&EE: effect ~A error: ~A~%" effect err)
                          (setf *effects* (delete effect *effects*))
                          (return-from effect))))
              (apply effect arguments))))
        (copy-list *effects*))
  *effects*)


(defvar *midi-log*      *error-output*)
(defvar *midi-verbose*  t)

(defun client-notify (message)
  (format *midi-log* "CN: ~A~%" message)
  (force-output *midi-log*))

(defvar *bad-source-connection-refcon* '())
(defun midi-port-read (packet-list source-connection-refcon)
  (handler-bind
      ((error (lambda (err)
                (terpri *error-output*)
                (mclgui::print-backtrace  *error-output*)
                (format *error-output* "~&EE: ~A: ~A~%"
                        (cffi:pointer-address source-connection-refcon)
                        err)
                (force-output *error-output*)
                (return-from midi-port-read))))

    (let ((*standard-output* *midi-log*)
          ;; (source-connection-refcon (cffi:pointer-address source-connection-refcon))
          (output-list '()))

      ;; (format *error-output* "PR: source-connection-refcon = ~S ~A ~A~%"
      ;;         source-connection-refcon
      ;;         (dw-8000-refcon-p *midi-application* source-connection-refcon)
      ;;         (controller-refcon-p *midi-application* source-connection-refcon))
      ;; (finish-output *error-output*)

      (cond
        ;; ((null *midi-application*) #| not initialized yet|#)
        ;;
        ;; ((dw-8000-refcon-p *midi-application* source-connection-refcon)
        ;;
        ;;  (let ((message-list (packet-list-to-messages packet-list)))
        ;;    (call-effects :start-packet-list message-list source-connection-refcon)
        ;;    (dolist (message message-list)
        ;;      (when *midi-verbose*
        ;;        (unless (typep message '(or midi:timing-clock-message midi:active-sensing-message))
        ;;          (format t "~&RD: ~A: ~A~%" source-connection-refcon message)))
        ;;      (call-effects :message message source-connection-refcon))
        ;;    (force-output)
        ;;    (call-effects :end-packet-list message-list source-connection-refcon)))

        (t ;; (controller-refcon-p *midi-application* source-connection-refcon)

         (let ((message-list (com.informatimago.macosx.coremidi.midi:packet-list-to-messages packet-list)))
           (dolist (message message-list)
             ;; (when (and (typep message 'channel-message)
             ;;            (= (midi:message-channel message) (controller-channel *midi-application*)))

             (typecase message
               (program-change-message
                (let ((program  (midi:message-program message)))
                  (format t "~&RC: ~S~%" message)
                  (format t "~&RC: ~A: PC ~A~%" source-connection-refcon program)
                  ;; (unless (= (message-channel message)
                  ;;            (dw-8000-channel *midi-application*))
                  ;;   (setf (message-channel message) (dw-8000-channel *midi-application*)))
                  ;; (push message output-list)
                  ))
               (control-change-message
                (let ((controller (midi:message-controller message))
                      (value      (midi:message-value      message)))
                  (when *midi-verbose*
                    (format t "~&RC: ~S~%" message)
                    (format t "~&RC: ~A: CC ~A ~A~%" source-connection-refcon controller value))
                  ;; (if (configuringp *midi-application*)
                  ;;     (configure *midi-application* controller value)
                  ;;     (map-controller-to-sysex-request *midi-application* controller value))
                  ))
               (t
                (when *midi-verbose*
                  (unless (typep message '(or midi:timing-clock-message midi:active-sensing-message))
                    (format t "~&RC: ~A: ~A~%" source-connection-refcon message)))
                ;; (unless (= (message-channel message)
                ;;            (dw-8000-channel *midi-application*))
                ;;   (setf (message-channel message) (dw-8000-channel *midi-application*)))
                ;; (push message output-list)
                ))
             ;; )
             )))

        ;; (t
        ;;  (unless (member source-connection-refcon *bad-source-connection-refcon*)
        ;;    (push source-connection-refcon *bad-source-connection-refcon*)
        ;;    (format *error-output* "~&RR: ~A: unexpected refcon.~%" source-connection-refcon)))

        )

      (when output-list
        (when *midi-verbose*
          (format t "~&RO: output-list ~S~%" output-list))
        ;; (send (midi-output-port *midi-application*)
        ;;       (dw-8000-destination *midi-application*)
        ;;       (packet-list-from-messages (nreverse output-list)))
        )
      (when *midi-verbose*
        (force-output)))))




(defun print-midi-devices ()
  (let ((*print-circle* nil))
    (flet ((endpoint-and-connected-device (endpoint)
             (list (coremidi:name endpoint)
                   (mapcar (function coremidi:name) (coremidi:connected-devices endpoint)))))
      (dolist (device (append (coremidi:devices)
                              (coremidi:external-devices)))
        (let ((entities      (coremidi:device-entities device)))
          (format t "~30A ~%"
                  (coremidi:name device))
          (dolist (entity entities)
            (format t "          - ~A~@[ <- ~{~S~^, ~}~]~@[ -> ~{~S~^, ~}~]~%"
                    (coremidi:name entity)
                    (mapcar (function endpoint-and-connected-device)
                            (coremidi:entity-sources entity))
                    (mapcar (function endpoint-and-connected-device)
                            (coremidi:entity-destinations entity))))
          (terpri))))))


(defun initialize-midi ()
  (let ((midishare-device-name "Network"))
    (install-midishare-interface)
    (update-port-refnums)
    (push (let ((name "MidiShare"))
            (make-midiapp
             :refnum 0
             :name name
             :coremidi (let ((client (coremidi:client-create name 'cm-client-notify)))
                         (make-coremidi
                          :client  client
                          :output-port (coremidi:output-port-create client (format nil "~A-OUT" name))
                          :input-port  (coremidi:input-port-create  client (format nil "~A-IN"  name)
                                                                    'cm-port-read)))))
          *midi-apps*)
    (set-default-refnums
     (get-source-refnum      (find midishare-device-name (list-source-ports)      :key (function first) :test (function string=)))
     (get-destination-refnum (find midishare-device-name (list-destination-ports) :key (function first) :test (function string=))))
    (multiple-value-setq (*destination* *source*) (midishare-endpoints midishare-device-name))
    (setf *filter* (midi-new-filter :chan t :port t :type t))
    (midi-open)))

#-(and)
(eval-when (:load-toplevel :execute)
  (initialize-midi))

;;;; THE END ;;;;

;; (mapcar 'coremidi:name(coremidi:devices))
;; ("Korg KRONOS" "Bluetooth" "IAC Driver" "Network" "MIDI4x4" "Moog Subsequent 37cv" "VI61" "SCHMIDT SYNTH" "VMini" "TOUCHE_BOOTLOADER" "TOUCHE" "EWI5000" "sao" "sao")
;;
;; (coremidi:output-port-create (first (CoreMIDI:clients)))
;; (getapp 1)
;; (loop for channel below 16
;;       do (sleep 5)
;;          (print channel)
;;          (test/send (cm-output-port (getapp 1)) *destination* :channel channel))
;;
;;
;;
;; (make-midiapp
;;  :refnum 0
;;  :name "MidiShare"
;;  :coremidi (let ((client (coremidi:client-create name 'cm-client-notify)))
;;              (make-coremidi
;;               :client  client
;;               :output-port (coremidi:output-port-create client (format nil "~A-OUT" name))
;;               :input-port  (coremidi:input-port-create  client (format nil "~A-IN"  name)
;;                                                         'cm-port-read))))
