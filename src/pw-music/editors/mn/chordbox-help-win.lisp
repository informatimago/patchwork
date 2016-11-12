;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               chordbox-help-win.lisp
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

;;====================================================================================================
(defclass C-chbox-help-window (C-pw-help-window)())

(defmethod view-key-event-handler ((self C-chbox-help-window) char)
  (case char
    ((:Newline)
     (when *active-MN-window* (window-select *active-MN-window*))
     (window-hide self))
    ((:Enter)
     (when *active-MN-window* (window-select *active-MN-window*)))))

;;====================================================================================================

(defvar *chbox-help-window* ())


(defmethod open-application-help-window ((self C-chordbox-Win))
  (if *chbox-help-window*
      (unless (wptr  *chbox-help-window*) (make-chbox-help-window))
      (make-chbox-help-window))
  (window-select *chbox-help-window*))

(defun make-chbox-help-window ()
  (let (scroller)
    (setq *chbox-help-window*
          (make-instance 'C-chbox-help-window :window-title "Chordbox help" :GROW-ICON-P t
                         :view-position (make-point 30 25) :view-size (make-point 460 255) :close-box-p nil))
    (setq scroller (make-instance 'C-pw-help-window-view :view-size (make-point (- 460 8) (- 255 16))
                                  :view-container *chbox-help-window* :v-scrollp t :h-scrollp nil :track-thumb-p t))
    (add-subviews scroller
                  (make-instance 'static-text-dialog-item :view-position (make-point 5 5)
                                 :view-font *patchwork-font-spec*
                                 :dialog-item-text
                                 "    Chord box editor keyboard shortcuts and clicks:
   general:
    h            open help window
    Return       select MN window,hide chord box editor window
    Enter        select MN window
    R            rename chord box editor window

    p            play
    a            arpeggio view
    t            time view
    n            normal view

    up arrow     tranpose selected note up
    down arrow   tranpose selected note down
                 (normal 1/4 tone,shift 1/2 tone,control 1 octave)
    Back Space   delete selected note (or notes)

    tab          select next edit mode

    To edit a note field:
     1. Select the field (DURation, DYNamics, OFFSet-time, CHANNEL) by
        pushing the corresponding round button.

     2. Select the note by placing the cross-hair cursor on top of it (it should
        become black).

     3. Click and drag vertically. Upwards motion increments and dowmwards decrements
        the selected note field value. Dragging with the SHIFT key pressed augments
        the increasing or decreasding step.

     When in time view, OFFSet time can be edited by selecting the note and then
     Click and drag horizontally to the desired position. When in arpeggio view,
     ORDER can be edited similarly.

     Either in time view or in arpeggio view, several notes can be selected at
     the same time by selecting a region containing them. A region is selected
     by clicking and dragging horizontally. Field edition affects then all selected
     notes.
     Note fields can also be edited by selecting a zone containing the note and
     pressing 'e'. A text box is then opened where a value can be typed in.

     pour entrer une note, manuellement: positionner le curseur et faire 'option'-clique

     Pressing the mouse on the letter 'A', 'V' or 'T' appearing at the bottom right
     rolls down a menu where view mode and staff type can be selected.

     A chord module has a popUp menu linked to the letter just to the right
     of its output box. The output of the chord module depends on the menu item chosen.
     The items in this menu are as follows:

midics:	output is the list of midic slots of the notes.

Durations:	output is the list of dur  slots of the notes.

Velocity:	output is the list of vel  slots of the notes.

offsets: 	output is the list of offset-time  slots.

Reorder: 	output is the list of midic  slots, reordered according to their order slots.

Chord Object:  	output is the whole chord object.

Save Chord:  	output does not change. The module is saved in a file.

The letter to the right of the chord module's output box indicates the current output option.



The module can be locked (to avoid new evaluations of the patch under the chord module) to
protect its contents by clicking on the small 'v' found in the lower left of the module.
The 'v' indicates that the module is not locked."))))




