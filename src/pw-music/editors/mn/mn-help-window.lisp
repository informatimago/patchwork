;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               mn-help-window.lisp
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
(defvar *active-MN-window* nil)

;;====================================================================================================
(defclass C-MN-help-window (C-pw-help-window)())

(defmethod view-key-event-handler ((self C-MN-help-window) char)
  (case char
    ((:Newline)
     (when *active-MN-window* (window-select *active-MN-window*))
     (window-hide self)) 
    ((:Enter)
     (when *active-MN-window* (window-select *active-MN-window*))))) 

;;====================================================================================================

(defvar *MN-help-window* ())

(defun make-MN-help-window ()
  (let (scroller)
    (setq *MN-help-window*
          (make-instance 'C-MN-help-window :window-title "MN help" :GROW-ICON-P t
                         :view-position (make-point 30 25) :view-size (make-point 560 455) :close-box-p nil))
    (setq scroller (make-instance 'C-pw-help-window-view :view-size (make-point (- 560 8) (- 455 16)) 
                                  :view-container *MN-help-window* :v-scrollp t :h-scrollp nil :track-thumb-p t))
    (add-subviews scroller
                  (make-instance 'static-text-dialog-item :view-position (make-point 5 5) 
                                 :view-font *patchwork-font-spec*
                                 :dialog-item-text 
                                 "    MN editor keyboard shortcuts and clicks:
   general:
    h         open help window
    Return    select PW window,hide MN window
    Enter     select PW window
    R         rename MN window
    H         scroll to 0 time
    K         kill all chords
    p         play all
    P         play selected or visible chords
    s         stop play 

    o         open active notes
    w         add window to active note 
    c         add window with collector to active note 
    r         remove instrument from active note

    Buttons:
      offs    display chords with note offset-time
      dur     show note durations
      dyn     show note dynamics
      ins     show instrument of notes
      tr      transpose selected chords by the amount (in midics)
              contained in the text box to its left
    Boxes
      stcnt   Sets the number of staffs for the editor
      zoom    zooms attack times of chords
"))))


;;;; THE END ;;;;
