;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               md-note-slots.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;
;;;;    Multi-dimensional boxes for chord and chord-line objects.
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
(in-package "C-GET-NOTE-SLOTS")

(defvar *no-notes-error*
  "object ~A contains no notes or there is no slot: ~S in the note")

(defclass C-get-note-slots (C-patch) ())

(defun get-chosen-note-slots (notes the-slots)
  (if notes
    (let ((valid-slots (class-slot-names (car notes))) methods)
      (if (consp the-slots)
        (mapcar
         (lambda (note)
             (mapcar (lambda (slot)
                         (if (setq methods (member slot valid-slots :test #'string=))
                           (slot-value note (car methods))
                           (error "invalid slot ~A " slot)))
                     the-slots))  notes)
        (mapcar  (lambda (note)
                     (if (setq methods (member the-slots valid-slots :test #'string=))
                       (slot-value note (car methods))
                       (error *no-notes-error* note the-slots)))  notes)))
    (error *no-notes-error* notes the-slots)))

(defunp get-note-slots ((object object) (slots (list (:value "midic")))
                        &optional (include? menu (:menu-box-list (("atime" . 1) ("no". 2))
                                                  :type-list (no-connection)))) list
  "<get-note-slot> is similar to <get-slot>, except that it only returns information
concerning notes.  This data includes: midic, dur, vel, chan, offset-time, and
comm.  Note that when the module is used with a chord sequence object, the
requested field is returned for each note of each chord in the object, and the
slots of each chord is paired in a list with the chord's attack time. "
  (and object (get-note-dimensions object slots (= include? 1))))

(defmethod get-note-dimensions ((self C-chord) the-slots &optional include?)
  (declare (ignore include?))
  (get-chosen-note-slots (notes self) the-slots))

(defmethod get-note-dimensions ((self C-chord-line) the-slots &optional include?)
  (mapcar (lambda (chord)
              (if include?
                (list (t-time chord)
                      (get-chosen-note-slots (notes chord) the-slots))
                (get-chosen-note-slots (notes chord) the-slots)))
          (chords self)))

(defmethod get-note-dimensions ((self cons) the-slots &optional include?)
  (ask-all self #'get-note-dimensions the-slots include?))

(defmethod get-note-dimensions ((self pw::C-note) the-slots &optional include?)
  (declare (ignore include?))
  (car (get-chosen-note-slots (list self) the-slots)))

(defmethod get-note-dimensions ((self t) the-slots &optional include?)
  (declare (ignore include?))
  (error *no-notes-error* self the-slots))

(defunp set-note-slots ((object object) (slots (list (:value "midic")))
                        (values nilnum)) nil
   "<set-note-slot> is similar to <set-slot>, except that
 it only assigns information concerning notes.
The slots it assigns includes: midic, dur, vel, chan, offset-time, and comm. The
module changes the contents of the object for  the given <slot(s)> by assigning
them the given <value(s)>."
  (and object (not (symbolp object))
       (let ((default-val (car (last (list! values)))))
         (cond ((subtypep (type-of object) 'C-chord)
                (set-chosen-note-slots (notes object) slots values)
                (pw::update-chord object))
               ((subtypep (type-of object) 'C-chord-line)
                (mapc (lambda (chord)
                            (set-chosen-note-slots (notes chord) slots
                                                   (or (pop values) default-val)))
                        (chords object))
                (tell (pw::chords object) 'pw::update-chord))
               ((and object (member 'notes (class-slot-names object)))
                (set-chosen-note-slots (notes object) slots values))
               (t (error *no-notes-error* object slots)))))
  object)

(defun set-chosen-note-slots (notes the-slots values)
  (if notes
    (let* ((valid-slots (class-slot-names (car notes))) methods
           (values (list! values))
           (def-val (car (last values))))
      (if (consp the-slots)
        (mapc
         (lambda (note)
             (mapc (lambda (slot)
                       (if (setq methods (member slot valid-slots :test #'string=))
                         (progn
                           (setf  (slot-value note (car methods))
                                  (or (pop values) def-val))
                           (update-note note))
                         (error "invalid slot ~A " slot)))
                   the-slots))  notes)
        (mapc (lambda (note)
                  (if (setq methods (member the-slots valid-slots :test #'string=))
                    (progn
                      (setf (slot-value note (car methods)) (or (pop values) def-val))
                      (update-note note))
                    (error *no-notes-error* note)))  notes)))
    (error *no-notes-error* notes)))

;; (pw::PW-addmenu pw::*pw-Multidim-menu* '(set-note-slots ))

(in-package "C-GET-SELECTIONS")

(defclass C-get-selections (C-Patch) ())

(defmethod patch-value ((self C-get-selections) obj)
  (declare (ignore obj))
  (if (subtypep (type-of (car (input-objects self))) 'pw::C-patch-midi-mod)
    (let ((win (application-object (car (input-objects self)))))
      (if (and win (wptr win))
        (saved-selected (car (subviews win)))))))

(defunp get-selections ((coll list (:value "()" :type-list ()))) list
 "get-sel retrieves the list of chords previously selected in the collector editor.
The module's input must always be connected directly with the output of a
<chord-seqn> ."
  (declare (ignore coll)))


