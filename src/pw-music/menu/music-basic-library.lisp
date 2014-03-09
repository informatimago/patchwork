;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               music-basic-library.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;    
;;;;    XXX
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

;;===========================
;;Items for Music Edit Menu
(ui:add-menu-items *PWoper-menu* (new-leafmenu  "-" ()))

(ui:add-menu-items *PWoper-menu* 
                (new-leafmenu "MIDI Reset" (lambda () (pw-reset-for-midi)))
                (new-leafmenu "MIDI all-notes-off" (lambda () (all-off))))


(PW-addmenu-fun *pw-MN-Edit-menu* 'chord 'C-patch-chord-box-M)

;;(PW-addmenu-fun *pw-MN-Edit-menu* 'collector 'C-patch-midi-Mod)

;;(PW-addmenu-fun *pw-MN-Edit-menu* 'poly-coll 'C-patch-PolifMN-mod)

;;(PW-addmenu-fun *pw-MN-Edit-menu* 'stime 'C-pw-stop-time)

(PW-addmenu-fun *pw-MN-Edit-menu* 'mk-note 'C-patch-make-note)

(PW-addmenu *pw-MN-Edit-menu* '(mk-chord))

;;(PW-addmenu-fun *pw-MN-Edit-menu* 'ch-l-build 'C-patch-chord-line:C-patch-chord-line)

(pw::pw-addmenu-fun *pw-MN-Edit-menu* 'chordseq 'C-patch-chord-line:C-patch-chord-line)

;;(PW-addmenu-fun *pw-MN-Edit-menu* 'epw::ascii-chord 'EPW::C-pw-ascii-chord-box)

(PW-addmenu-fun *pw-MN-Edit-menu* 'multiseq 'C-patch-PolifMN-mod)

;;=================================
;;items for Conv-Approx menu

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadowing-import
   '(epw::F->MC epw::MC->F epw::MC->N epw::N->MC epw::INT->SYMB epw::SYMB->INT
     epw::CENTS->COEF epw::COEF->CENTS epw::NBCENTS-F epw::APPROX-M
     epw::LIN->DB)))

(PW-addmenu *pw-Conv-approx-menu*
 '(f->mc mc->f mc->n n->mc int->symb symb->int cents->coef coef->cents approx-m
   lin->db epw::db->lin))

;;;============================
;;; Extern Items

;;(pw-addmenu-fun *pw-Music-Extern-menu* 'pw-clock 'C-pw-gclock)

;;(pw-addmenu-fun *pw-Music-Extern-menu* 'str-dur 'C-structured-dur)

;;(pw-addmenu-fun *pw-Music-Extern-menu* 'strout 'C-structured-outbox)

;;(new-PW-sub-menu-item *pw-Music-Extern-menu* "struct-abstraction" 'C-make-structured-abstraction 'strabs 
;;  '(*string-pw-type* "strcoll") '(midi-ins) '("Strcoll"))

;;=========================
;;MIDI

;;(pw-addmenu *pw-Midi-menu* '(midi-o pgmout volume TXtune))

;;(pw-addmenu *pw-Midi-menu* '(bendout))

(pw-addmenu-fun *pw-Midi-menu* 'C-pw-send-midi-note::play-chords
                'C-pw-send-midi-note:C-pw-send-midi-note)

(pw-addmenu-fun *pw-Midi-menu* 'C-PW-MIDI-IN::play/stop
                'C-PW-MIDI-IN::C-play/stop-list)

(pw-addmenu *pw-Midi-menu* '(play-object))

;;(PW-addmenu-fun *pw-Midi-menu* 'c-pw-midi-in::play/stop 'c-pw-midi-in::C-jouer/eteindre)

#|(PW-addmenu-fun *pw-Midi-menu* 'c-pw-midi-in::play/st-list
                'c-pw-midi-in::C-play/stop-list)|#

(ui:add-menu-items *pw-Midi-menu*  (new-leafmenu "-" ()))

(pw-addmenu *pw-Midi-menu* '(midi-o pgmout bendout volume))

(ui:add-menu-items *pw-Midi-menu*  (new-leafmenu "-" ()))

(pw-addmenu-fun *pw-Midi-menu* 'C-pw-midi-in:delay 'C-pw-midi-in:C-pw-delay-box)

(pw-addmenu *pw-Midi-menu* '(epw::microtone))

(ui:add-menu-items *pw-Midi-menu*  (new-leafmenu "-" ()))

#|(ui:add-menu-items *pw-Midi-menu* 
  (new-leafmenu "raw-in" 
     (lambda () 
       (let ((enum (make-PW-standard-box 'C-pw-midi-in:C-PW-midi-in-top
                                           'C-pw-midi-in:m-data))
             (loop (make-PW-standard-box 'C-pw-midi-in:C-pw-midi-in
                                        'C-pw-midi-in::raw-in)))
       (add-subviews *active-patch-window* enum loop)
       (set-view-position loop (make-point (x loop) (+ (h loop) 24)))
       (connect-ctrl loop (car (pw-controls loop)) enum)
       (setf (open-state (car (pw-controls loop)) ) nil)
       (tell (controls *active-patch-window*) 'draw-connections))))
    (new-leafmenu "note-in" 
     (lambda () 
       (let ((enum (make-PW-standard-box 'C-pw-midi-in:C-PW-midi-in-top
                                           'C-pw-midi-in:m-data))
             (loop (make-PW-standard-box 'C-pw-midi-in:C-pw-note-in
                                        'C-pw-midi-in:note-in)))
       (add-subviews *active-patch-window* enum loop)
       (set-view-position loop (make-point (x loop) (+ (h loop) 24)))
       (connect-ctrl loop (car (pw-controls loop)) enum)
       (setf (open-state (car (pw-controls loop)) ) nil)
       (tell (controls *active-patch-window*) 'draw-connections))))
  (new-leafmenu "note-in" 
     (lambda () 
       (let ((enum (make-PW-standard-box 'C-pw-midi-in:C-PW-midi-in-top
                                           'C-pw-midi-in:m-data))
             (loop (make-PW-standard-box 'C-pw-midi-in:C-pw-note-on-in
                                        'C-pw-midi-in:note-in)))
       (add-subviews *active-patch-window* enum loop)
       (set-view-position loop (make-point (x loop) (+ (h loop) 24)))
       (connect-ctrl loop (car (pw-controls loop)) enum)
       (setf (open-state (car (pw-controls loop)) ) nil)
       (tell (controls *active-patch-window*) 'draw-connections))))
  (new-leafmenu "chord-in" 
     (lambda () 
       (let ((enum (make-PW-standard-box 'C-pw-midi-in:C-PW-midi-in-top
                                           'C-pw-midi-in:m-data))
             (loop (make-PW-standard-box 'C-pw-midi-in:C-pw-chord-in
                                        'C-pw-midi-in:chord-in)))
       (add-subviews *active-patch-window* enum loop)
       (set-view-position loop (make-point (x loop) (+ (h loop) 24)))
       (connect-ctrl loop (car (pw-controls loop)) enum)
       (setf (open-state (car (pw-controls loop)) ) nil)
       (tell (controls *active-patch-window*) 'draw-connections)))))|#

;;aaa (ui:add-menu-items *pw-Midi-menu*  (new-leafmenu "-" ()))

;;aaa  (PW-addmenu *pw-Midi-menu* '())

;;(ui:add-menu-items *pw-Midi-menu*  (new-leafmenu "-" ()))


;;(pw-addmenu *pw-Midi-menu* '(epw::microtone))

;;===========================
;;Multidim Objects

(PW-addmenu *pw-Multidim-Music-menu* '(c-get-note-slots:get-note-slots))

(PW-addmenu *pw-Multidim-Music-menu* '(c-get-note-slots::set-note-slots))

(PW-addmenu-fun *pw-Multidim-Music-menu* 'c-get-selections:get-selections
                'c-get-selections::C-get-selections)

;;;============================
;;Hardcopy printing

(defvar *MN-print-setUp*
  (new-leafmenu "Page Setup…" (lambda () (win-print-setUp *active-mn-window*))))

(defvar *print-MN-menu* 
  (new-leafmenu "Print…" (lambda () (window-hardcopy *active-mn-window*))))

(ui:add-menu-items *MN-menu-file* *MN-print-setUp* *print-MN-menu*)

;;;===================

