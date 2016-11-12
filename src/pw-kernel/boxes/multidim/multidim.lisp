;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               multidim.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;
;;;;    Functions and objects for constructing a chord.
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
(pw:enable-patchwork-reader-macros)

;;;==========================================
;;;Class: C-patch-chord-setUp
;;;inherits from:   C-patch
;;;methods:
;;;  patch-value          ;builds a chord object
;;;============================================

(defunp Chbuild ((midics (fixs (:dialog-item-text "()" :type-list (fixnum list))))
                 (durs (fixs (:dialog-item-text "()" :type-list (fixnum list))))
                 (offs (fixs (:dialog-item-text "()" :type-list (fixnum list))))
                 (dyns (fixs (:dialog-item-text "()" :type-list (fixnum list))))
                 &optional
                   (channs (fixs (:dialog-item-text "()" :type-list (fixnum list))))
                   (ords (fixs (:dialog-item-text "()" :type-list (fixnum list) :value ())))
                   (comments (symbol (:value () :dialog-item-text "()")))
                   (objects (list (:value () :dialog-item-text "()" :type-list ())))
                   (ch-type (symbol (:value 'pw::c-chord))))
        ch-ob
"Constructs a chord object. All entries accept either single values or lists.
 The 'objects' entry takes either a note (or list of note) objects or a chord object whose
 fields are affected by the values at other entries (if present).
 If there is no input at the 'objects' entry, notes are constructed with as much
 information as can be extracted from the other entries. Missing information
 is filled in with default values as follows:
 midic=6000 dur=100 vel=100 chan=1 offs=0 order=sequential comments=nil"
   (let* ((midics (list! midics))
         (durs (epw::ll/round (list! durs) 1))
         (offs (epw::ll/round (list! offs) 1))
         (dyns (list! dyns))
         (channs (list! channs))
         (ords (list! ords))
         (objects (list! objects))
         (an-order (length ords))
         (comms (list! comments))
         (def-midic 6000) (def-vel 100) (def-dur 75) (def-offs 0) (def-chann 1)
         notes chord)
     (if (and objects (subtypep (type-of (car objects)) 'C-chord))
       (psetq objects (notes (car objects)) chord (car objects)))
      (while (or midics durs offs dyns channs ords objects comms)
        (if objects
          (let ((the-note (pop objects)))
            (setf (midic the-note) (or (pop midics) (midic the-note)))
            (setf (dur the-note) (or (pop durs) (dur the-note)))
            (setf (vel the-note) (or (pop dyns) (vel the-note)))
            (setf (chan the-note) (or (pop channs) (chan the-note)))
            (setf (offset-time the-note) (or (pop offs) (offset-time the-note)))
            (setf (order the-note) (or (pop ords) (incf an-order)))
            (setf (comm the-note) (pop comms))
            (update-note the-note)
            (push the-note notes))
          (push
           (make-instance 'C-note
                        :midic (if (car midics) (setq def-midic (pop midics)) def-midic)
                        :offset-time (if (car offs) (setq def-offs (pop offs)) def-offs)
                        :dur (if (car durs) (setq def-dur (pop durs)) def-dur)
                        :order (or (pop ords) (incf an-order))
                        :comm (pop comms)
                        :chan (if (car channs) (setq def-chann (pop channs)) def-chann)
                        :vel (if (car dyns) (setq def-vel (pop dyns)) def-vel))
           notes)))
      (if chord
        (progn (setf (notes chord) (sort notes '< :key #'midic)) (update-chord chord) chord)
        (make-instance ch-type :notes (sort notes '< :key #'midic) :t-time 0))))

(defunp mk-chord ((midic (fixs (:dialog-item-text "()" :type-list (fixnum list))))
                 (dur (fixs (:dialog-item-text "()" :type-list (fixnum list))))
                 (offset-time (fixs (:dialog-item-text "()" :type-list (fixnum list))))
                 (vel (fixs (:dialog-item-text "()" :type-list (fixnum list))))
                 &optional
                   (chan (fixs (:dialog-item-text "()" :type-list (fixnum list))))
                   (order (fixs (:dialog-item-text "()" :type-list (fixnum list) :value ())))
                   (comment (symbol (:value () :dialog-item-text "()")))
                   (objects (list (:value () :dialog-item-text "()" :type-list ())))
                   (ch-type (symbol (:value '()))))
        ch-ob
"<make-chrd> is a chord constructor object. Its input is either an element or a
list of elements. The number of notes in the constructed chord is equal to the
length of the longest list of values supplied for the inputs. Each note of the
chord is constructed by taking the next element of each list supplied to the
inputs and operating on them exactly as for the make-note object. If a list is
exhausted, the default value of the note object slot it names is taken for each
successive note until all lists are exhausted. If an input is supplied to the object
argument, it must be either a note object, a list of note objects, or a chord
object. In this case the behavior of the module is as described before, except
that the values of the slots of the supplied objects (either note or chord) have
precedence over the default values. That is, if one of the lists in the inputs is
exhausted but note objects remain in the list of note objects supplied in object,
then those remaining note objects will not be modified in the slot corresponding
to the input that was exhausted. Similarly, if a chord object is supplied in object,
then those note objects in the chord's note list (that is, the list linked to the
chord's notes slot) which have not yet been processed will not be modified in
the slot corresponding to an exhausted list."
  (chbuild midic dur offset-time vel chan order comment objects (or ch-type 'pw::C-chord)))

(defclass C-patch-MD-set-slot (C-patch) ())

(defun class-instance-slots (class)
  (remove-if-not (lambda (slot)
                   (eql :instance (slot-definition-allocation slot)))
                 (class-slots class)))

(defun class-slot-names (obj)
  ;;(mapcar #'car (class-slots (find-class (class-name (class-of obj)))))
  ;;modified 920818 [Camilo]
  (ask-all (class-instance-slots (class-of obj)) #'slot-definition-name))

(defunp set-slot ((object object (:value "()")) (slot (list (:value "slot" :type-list ()))) (value nilNum)) nil
 "An object slot modification. The first input must be an object-instance or list of
object-instances.  The second input is  a slot-name . Third input is the
corresponding new value or list of new values. Returns the modified object(s).
Warning: This operation is potentially dangerous. You should know what you
are doing when changing object slot's values"
  (if object
    (let ((default value) methods)
      (if (consp object)
        (dolist (my-obj object)
          (if (setq methods (member slot (class-slot-names my-obj) :test #'string=))
            (setf  (slot-value my-obj (car methods))
                   (if (consp value) (setq default (pop value)) default))
            (progn (format t "Valid Slots: ~S" (class-slot-names my-obj))
                   (ui:ed-beep)(return nil))))
        (if (setq methods (member slot (class-slot-names object) :test #'string=))
          (setf (slot-value object (car methods)) value)
          (progn (format t "Valid Slots: ~S" (class-slot-names object))
                 (ui:ed-beep) nil)))
      object)))

(defclass C-patch-MD-get-slot (C-patch) ())

(defunp get-slot ((object object (:value "()")) (slot (list (:value "slot" :type-list ())))) nil
 "An object slot inspection. The first input must be an object-instance
\(such as chord objects or breakpoint functions), or a list of object-instances.
The second input is a slot-name. Returns the corresponding value(s) of the
chosen slot.
Evaluating the get-slot module with the string 'slot in the <slot> input, the
module returns the list of the valid slots of the object in the first input <object>."
  (if object
    (let* (output methods)
      (if (consp object)
        (dolist (my-obj object (nreverse output))
          (if (setq methods (member slot (class-slot-names my-obj) :test #'string=))
            (push  (slot-value my-obj (car methods)) output)
            (progn (format t "Valid Slots: ~S" (class-slot-names my-obj))
                   (ui:ed-beep)(return nil))))
        (if (setq methods (member slot (class-slot-names object) :test #'string=))
          (slot-value object (car methods))
          (progn (format t "Valid Slots: ~S" (class-slot-names object))
                 (ui:ed-beep) nil)))
      )))

(defunp send ((box list (:value "chord" :type-list ()))
                 &optional (mssge list (:value '() :type-list ()))
                 &rest (arg list (:value '() :type-list nil))) nil
  "sends the message 'mssge' (default: 'patch-value') to a box whose name
 is  'box'. 'arg(s)' are the arguments of the message"
  (let ((target (find-boxes-named box))
        pw-message)
    (if target
      (if mssge
        (if (fboundp mssge)
          (apply mssge (cons target arg))
          (if (fboundp
               (setq pw-message (read-from-string (format () "pw::~S" mssge))))
            (apply pw-message (cons target arg))
            (error "message '~S' is not known by box '~S'" mssge box)))
        (patch-value target target))
      (format t "No box named ~S" box))))

(defun find-boxes-named (name)
  (let ((boxes (controls *active-patch-window*)))
    (dolist (box boxes nil)
      (if (string= (string-downcase (format () "~S" name)) (pw-function-string box))
        (return box)))))

;;a function for storing slot values from a patchwork box
(defun method-sf (obj field val) (setf (slot-value obj field) val))

(defclass  C-patch-make-note (C-pw-functional) ())

(defmethod patch-value ((self C-patch-make-note) obj)
    (let ((midic   (patch-value (nth 0 (input-objects self)) self))
          (dur     (patch-value (nth 1 (input-objects self)) self))
          (vel     (patch-value (nth 2 (input-objects self)) self))
          (chan    (patch-value (nth 3 (input-objects self)) self))
         instrument)
   (if
    ;;;;(not (eql (class-name (class-of obj)) 'C-patch-midi-Mod))
       (not (yourself-if-collecting obj))
      (make-instrument-note midic dur chan vel instrument ())
      (progn
         (when (nth-connected-p self 4)
           (setq instrument (patch-value (nth 4 (input-objects self)) obj)))
         (make-instrument-note midic dur chan vel instrument (application-object obj))))))

(defunp make-note ((midic midic) (dur fix>0 (:value 75))
                   (vel (fix>=0 (:max-val 127 :value 100))) (chan approx)
                   &optional (m-ins (list (:value () :dialog-item-text "()" :type-list ()))))
        note-ob
" see mk-note"
  (declare (ignore midic dur vel chan m-ins)))

(defunp mk-note ((midic midic) (dur fix>0 (:value 75))
                   (vel (fix>=0 (:max-val 127 :value 100))) (chan approx)
                   &optional (m-ins (list (:value () :dialog-item-text "()" :type-list ()))))
        note-ob
"<make-note> is the note object constructor module. Each entry corresponds to
the value of the named slot of the object. The <midic>  argument, for instance,
is a midicent value (with a default value of 6000) that is stored in the midic slot
of the constructed object, <dur> is the  duration of the note, in hundredths of a
second,  <vel> is the velocity or ‘dynamic’ of the note, between 1 and 127
(default is 100); <chan> is a MIDI channel number (default is 1). The optional
<m-ins>  input, if given, should be connected to a patch that outputs a
Patchwork instrument. This could be, for example, a structured abstraction, that
is, a window with a subpatch on it. Any argument not supplied to make-note
takes its default value."
  (declare (ignore midic dur vel chan m-ins)))
;;==================================

(defparameter *note-obj-pw-type*
  (make-instance 'C-pw-type
  :control-form
   `(make-instance 'C-ttybox  :view-size (make-point 36 14)
                    :type-list '(note-obj))))

(defparameter *fix-list-tty-pw-type*
  (make-instance 'C-pw-type
  :control-form
   `(make-instance 'C-ttybox  :view-size #@(36 14) :dialog-item-text "()"
      :type-list '(fixnum list))))

(defparameter *note-obj-field-pw-type*
  (make-instance 'C-pw-type :control-form `(make-instance 'C-menubox-val
                                             :view-size (make-point 36 14)
                                             :menu-box-list '(("midic" . midic) ("dur". dur)
                                                              ("vel". vel) ("chan". chan)
                                                              ("instrument". instrument))
                                             :type-list '(no-connection))))

;;==================================
