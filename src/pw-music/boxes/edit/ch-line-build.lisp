;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               ch-line-build.lisp
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
;;;;    
;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;=========================================================
;;;;
;;;;  PATCH-WORK
;;;;  By Mikael Laurson, Jacques Duthen, Camilo Rueda.
;;;;  © 1986-1992 IRCAM 
;;;;
;;;;=========================================================

(defpackage "C-PATCH-CHORD-LINE"
  (:use "COMMON-LISP" "UI" "LELISP-MACROS" "PATCH-WORK")
  (:export "C-PATCH-CHORD-LINE" "CHORD-SEQN"))

(in-package "C-PATCH-CHORD-LINE")
(enable-patchwork-reader-macros)

(defclass C-patch-chord-line (C-patch-midi-Mod )())

(defmethod initialize-instance :after ((self C-patch-chord-line) &key controls)
  (declare (ignore controls))
  (-make-lock self (make-point (- (w self) 30) (- (h self) 10)))
  self)

(defmethod collect ((self C-patch-chord-line)) (declare (ignore self)) )
(defmethod pw::clock ((self C-patch-chord-line)) (pw::clock pw::*global-clock*))

(defmethod  begin-process ((self C-patch-chord-line)) (declare (ignore self)) )

(defmethod yourself-if-collecting ((self C-patch-chord-line)) nil)

(defmethod draw-patch-extra :after ((self C-patch-chord-line))
  (pw::draw-char (+ -16 (w self)) (- (h self) 4) #\E))

(defmethod set-dialog-item-text-from-dialog ((self C-patch-chord-line) str)
  (declare (ignore str))
  (call-next-method)
  (let ((editor (application-object self)))
    (if (and editor (wptr editor))
      (set-window-title editor (pw-function-string self)))))

(defmethod complete-box ((self C-patch-chord-line) args)
  (declare (ignore args))
  (call-next-method)
  (set-window-title (application-object self) (pw-function-string self)))

(defmethod  get-chord-type ((self C-chord-line)) (chords self))
(defmethod get-chord-type ((self t)) (list!  self))

(defmethod set-new-class ((self C-patch-chord-line) (obj C-chord-line))
  (let ((win (application-object self)))
    (setf (chord-seq self) obj)
    (if (and win (wptr win))
      (setf (pw::chord-line (car (pw::editor-objects (car (subviews win))))) (chord-seq self)))))

(defmethod set-new-class ((self C-patch-chord-line) (obj t))
  (declare (ignore obj)))

(defmethod patch-value ((self C-patch-chord-line) obj)
  (unless (value self)
    (let ((base-chords (patch-value (first (input-objects self)) obj)))
      (set-new-class self base-chords)
      (let* ((my-in (input-objects self))
             (my-ctrl (pw-controls self))
             (time-list (list! (epw::ll/round (patch-value (second my-in) obj) 1)))
             (default (or (car (last time-list)) 100))
             (chords (get-chord-type base-chords))
             (durs (list! (epw::ll/round (patch-value (third my-in) obj) 1)))
             (vels (list! (patch-value (fourth my-in) obj)))
             (chans (list! (patch-value (sixth my-in) obj)))
             (insts (list! (and (eighth my-ctrl) (patch-value (eighth my-in) obj))))
             (offs (list! (epw::ll/round (patch-value (fifth my-in) obj) 1)))
             (acum (if (seventh my-ctrl) (patch-value (seventh my-in) obj) 0))
             (win (application-object self))
             (def-durs (get-last-singleton durs))
             (def-midic (get-last-singleton chords))
             (def-vels (get-last-singleton vels))
             (def-chans (get-last-singleton chans))
             (def-offs (get-last-singleton offs))
             all-chords a-chord chord )
        (when (and (every #'(lambda (chord) (subtypep (type-of chord) 'C-chord)) chords)
                   (some  #'(lambda (chord) (not (zerop (pw::t-time chord)))) chords))
          (setq time-list
                (epw::x->dx (pw::get-slot chords 'pw::t-time))))
        (setf (chords (chord-seq self))
              ;(dolist (chord chords (nreverse all-chords))
              (progn 
                (while (or chords durs offs vels chans time-list)
                  (setq chord (pop chords))
                  (push
                   (setq a-chord
                         (cond ((subtypep (type-of chord) 'C-chord)
                                (pop durs) (pop offs) (pop vels) (pop chans) chord)
                               ((or (subtypep (type-of chord) 'C-note)
                                    (and (consp chord) (subtypep (type-of (car chord)) 'C-note)))
                                (chbuild () (or (pop durs) def-durs) (or (pop offs) def-offs)
                                         (or (pop vels) def-vels) (or (pop chans) def-chans) () () chord))
                               (t (chbuild (or chord def-midic) (or (pop durs) def-durs) (or (pop offs) def-offs)
                                           (or (pop vels) def-vels) (or (pop chans) def-chans)))))
                   all-chords)
                  (make-instrument-for-chord a-chord
                                             (round acum)
                                             (pop insts) win)
                  (incf acum (or (pop time-list) default)))
                (nreverse all-chords)))
        (if (wptr (application-object self))
          (update-editor (car (subviews win)))))))
  ;(chord-seq self)
  (make-instance 'pw::c-chord-line :chords (chords (chord-seq self))))


(defun get-last-singleton (list)
  (let ((elem (car (last list))))
    (if (consp elem) (car (last elem)) (get-default-val elem))))

(defmethod get-default-val ((self C-chord)) 6000)

(defmethod get-default-val ((self t)) self)

(defmethod correct-extension-box ((self C-patch-chord-line) new-box values)
  (declare (ignore values))
  (let* ((new-win (application-object new-box))
         (new-editor
          (car (pw::editor-objects (car (subviews new-win))))))
    (if (wptr (application-object self)) (window-close(application-object self)))
    (setf (chord-seq new-box) (chord-seq self))
    (setf (chord-line new-editor) (chord-seq new-box))
    (pw::put-window-state new-box new-win (pw::window-state self))))

(defmethod compile-me ((self C-patch-chord-line) obj)
  (if (value self)
    `',(decompile (chord-seq self))
    (let* ((my-in (input-objects self))
           (my-ctrl (pw-controls self))
           (time-list (if (eq (second my-in) (second my-ctrl))
                        `'(cons 0 (pw::list! 
                                  (epw::ll/round ',(patch-value (second my-in) obj) 1)))
                        `(list 'cons 0 (list 'pw::list! 
                                  (list 'epw::ll/round ,(compile-me (second my-in) obj) 1)))))
           (chords (if (eq (first my-in) (first my-ctrl))
                         `'(pw::list! ,(patch-value (first my-in) obj))
                         `(list 'list! ,(compile-me (first my-in) obj))))
           (durs (if (eq (third my-in) (third my-ctrl))
                   `'(pw::list! ',(epw::ll/round (patch-value (third my-in) obj) 1))
                   `(list 'pw::list! (list 'epw::ll/round ,(compile-me (third my-in) obj) 1))))
           (offs (if (eq (fifth my-in) (fifth my-ctrl))
                   `'(pw::list! ',(epw::ll/round (patch-value (fifth my-in) obj) 1))
                   `(list 'pw::list! (list 'epw::ll/round ,(compile-me (fifth my-in) obj) 1))))
           (vels (if (eq (fourth my-in) (fourth my-ctrl))
                   `'(pw::list! ',(patch-value (fourth my-in) obj))
                   `(list 'pw::list! ,(compile-me (fourth my-in) obj))))
           (chans (if (eq (sixth my-in) (sixth my-ctrl))
                    `'(pw::list! ',(patch-value (sixth my-in) obj))
                    `(list 'pw::list! ,(compile-me (sixth my-in) obj))))
           (acum (if (seventh my-ctrl)
                     (if (eq (seventh my-in) (seventh my-ctrl))
                       `',(patch-value (seventh my-in) obj)
                       `,(compile-me (seventh my-in) obj)) 0)))
      `(list 'let* (list 
                    (list 'chords ,chords)
                    'chord 'a-chord 'all-chords
                    (list 'the-ch-line 
                          (list 'make-instance ''C-chord-line ))
                    (list 'time-list ,time-list)
                    (list 'durs ,durs)
                    (list 'vels ,vels)
                    (list 'chans ,chans)
                    (list 'offs ,offs)
                    (list 'acum ,acum)
                    '(default (or (car  time-list) 100))
                    '(def-durs (car (last durs)))
                    '(def-vels (car (last vels)))
                    '(def-chans (car (last chans)))
                    '(def-offs (car (last offs))))
             '(progn 
                (while (or chords durs offs vels chans time-list)
                  (setq chord (pop chords))
                  (push
                   (progn
                     (setq a-chord
                           (if (or (subtypep (type-of chord) 'C-chord)
                                   (subtypep (type-of chord) 'C-note)
                                   (and (consp chord) (subtypep (type-of (car chord)) 'C-note)))
                             (chbuild () (or (pop durs) def-durs) (or (pop offs) def-offs)
                                      (or (pop vels) def-vels) (or (pop chans) def-chans) () () chord)
                             (chbuild (or chord 6000) (or (pop durs) def-durs) (or (pop offs) def-offs)
                                      (or (pop vels) def-vels) (or (pop chans) def-chans))))
                     (setf (t-time a-chord) acum)
                     (if (car time-list) (setq default (pop time-list)))
                     a-chord)    all-chords)
                  (incf acum default))
                (nreverse all-chords))
             '(setf (chords the-ch-line) all-chords)
             'the-ch-line))))

(in-package :pw)

(pw::defunp chord-seqn ((chords midic (:type-list (list fixnum float chord note-obj)))
                    (dels fix/fl/list (:value 100))
                    (durs fix/fl/list (:value 75))
                    (vels fix/fl/list (:value 100))
                    (offs fix/fl/list (:value 0))
                    (chans midic (:value 1))
                    &optional
                    (start fix>=0)
                    (insts list (:value '() :type-list ()))
                    ) collector
"<chord-seqn> is a sequence constructor object. Builds a chord-line (an object with a 'chords' slot containing a 
list of chords). <chords> can be either a list (or list of lists) of midics or note objects,
 a chord object or list of chord objects or a single midic or note object. The <dels> input controls the
spacing of the chords (in 100ths of a second). <durs> input controls the duration of the chord (in 100ths of a second) , <vels> input
 controls the dynamic of the chord, <chans> input controls the MIDI channel number  of the chord  , <offs> 
input controls the offset time of each note   of the chord, <start> is the start point of the sequence (in 100ths of a second),
<insts> input allow to connect an instrument to chord  .
A popUp menu is linked to the letter "
  (declare (ignore chords dels durs vels offs chans insts start)))

(pw::defunp chordseq ((chords midic (:type-list (list fixnum float chord note-obj pw::collector)))
                    (dels fix/fl/list (:value 100))
                    (durs fix/fl/list (:value 75))
                    (vels fix/fl/list (:value 100))
                    (offs fix/fl/list (:value 0))
                    (chans midic (:value 1))
                    &optional
                    (start fix>=0)
                    (insts list (:value '() :type-list ()))
                    ) collector
"<chord-seqn> is a sequence constructor object. It builds a chord-line (an 
object with a 'chords' slot containing a list of chords). <chords> can be either a 
list (or list of lists) of midics or note objects, a chord object (or list of chord 
objects), or a single midic or note object. The <dels> input controls the spacing 
of the chords (in 100ths of a second). <durs> input controls the duration of the 
chord (in 100ths of a second) , <vels> input  controls the dynamic of the chord, 
<chans> input controls the MIDI channel number of the chord, <offs>  input 
controls the offset time of each note of the chord, <start> is the start point of the 
sequence (in 100ths of a second), and the <insts> input lets one connect an 
instrument to chord. A popUp menu is linked to the letter 'A' just to the right of 
the output box.  This menu can save the module (and its collected chord 
sequence) into a file. An editor for the <chord-seqn> object is entered either by 
selecting the module and typing the letter ‘o’ or by double-clicking on the 
module's name. Click 'h' with the music-notation-editor opened for more 
informations. The module can be locked (to avoid new evaluations of the patch 
that is under 'chord') to keep its contents by clicking on the small ‘o’ in the lower 
right of the module The ‘o’ indicates that the module is open."
  (declare (ignore chords dels durs vels offs chans insts start)))

;;(pw::pw-addmenu-fun pw::*pw-menu-patch* 'chordseq 'C-patch-chord-line)

;;;this is only kept for compatibility and may be erased at any moment...
(setf pw::*chord-line-object-type*
  (make-instance 'C-pw-type
  :control-form
   `(make-instance 'C-ttybox  :view-size #@(36 14) :dialog-item-text "()" 
      :type-list '(pw::chord list))))
;;(pw::pw-addmenu-fun (pw::the-user-menu) 'chordseq 'C-patch-chord-line::C-patch-chord-line)
