;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               rtm-help-window.lisp
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

;;====================================================================================================
(defclass C-rtm-help-window (C-pw-help-window)())

(defmethod view-key-event-handler ((self C-rtm-help-window) char)
  (case char
    ((:Newline)
     (when *active-rtm-window* (window-select *active-rtm-window*))
     (window-hide self)) 
    ((:Enter)
     (when *active-rtm-window* (window-select *active-rtm-window*))))) 

;;====================================================================================================

(defun make-rtm-help-window ()
  (let (scroller)
    (setq *rtm-help-window*
          (make-instance 'C-rtm-help-window :window-title "Rhythm help" :GROW-ICON-P t
                         :view-position (make-point 30 25) :view-size (make-point 560 455) :close-box-p nil))
    (setq scroller (make-instance 'C-pw-help-window-view :view-size (make-point (- 560 8) (- 455 16)) 
                                  :view-container *rtm-help-window* :v-scrollp t :h-scrollp nil :track-thumb-p t))
    (add-subviews scroller
                  (make-instance 'static-text-dialog-item :view-position (make-point 5 5) 
                                 :view-font '("monaco" 9 :srcor)
                                 :dialog-item-text 
                                 "    Rhythm editor keyboard shortcuts and clicks:

    h         open help window
    Return    select PW window,hide RTM window
    Enter     select PW window
    R         rename RTM window

    H         scroll to 1 measure
    L         scroll to last measure
    +         next measure
    -         previous measure
    ->        next page
    <-        previous page
    p         play selected measure in all selected staffs
    P         play only selected measure  

    e         toggle edit-mode on/off
    a         add measure/beat after selected measure-line/measure/beat
    b         add measure/beat before selected measure/beat
    Backspace remove selected measure/beat
    K         kill all measures in all measure-lines 

    r         record from midi to selected measure-line/measure/beat
    k         erase midi buffer
    S         select all selection buttons
    U         unselect all selection buttons

    D         redraw window

    Selection:

    to select (in edit mode) measure-line/measure/beat/chord click inside a gray rectangle
    to select a range shift-click 
      (in range selection only the first and the last items are black)  
      (range selection is used by the edit selection menu-items)  
    to open the rhythm-editor-dialog double-click inside a gray rectangle
    to open a chord-editor you have to double-click inside a leaf and the 
    chord-check-box should be selected

    Layout:

   to zoom edit scale numbox
   to scroll middle C click inside clef area (not inside selection button) and drag
   to scroll beam start shift-click inside clef area (not inside selection button) and drag
   to change staff look at the Choose Staff menu-item in RTM menu
   (to display only rhythm choose Empty) 
   to control the first staff (in polifonic editor) edit staff numbox
   to control the amount of visible staffs (in polifonic editor) edit stcnt numbox

    Editing velocity:
   if you press the option key you can draw an arbitary shape with the mouse inside 
   the editor and scale the range of that shape by editing the numboxes by selecting
   the velocity... menu-item

"))))




