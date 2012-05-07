;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               bpf-help-window.lisp
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

(in-package :pw)

(provide 'BPF-help-window)

;;====================================================================================================
(defclass C-bpf-help-window (C-pw-help-window)())

(defmethod view-key-event-handler ((self C-bpf-help-window) char)
  (declare (special *active-bpf-window*))
     (case char
        ((:Newline)
           (when *active-bpf-window* (window-select *active-bpf-window*))
           (window-hide self)) 
        ((:Enter)
           (when *active-bpf-window* (window-select *active-bpf-window*))))) 

;;====================================================================================================

(defvar *BPF-help-window* ())

(defun make-BPF-help-window ()
  (let (scroller)
    (setq *BPF-help-window*
          (make-instance 'C-bpf-help-window :window-title "BPF help" :GROW-ICON-P t
                         :view-position (make-point 100 50) :view-size (make-point 460 230) :close-box-p nil))
    (setq scroller (make-instance 'C-pw-help-window-view :view-size (make-point (- 460 8) (- 230 16)) 
                                  :view-container *BPF-help-window* :v-scrollp t :h-scrollp nil :track-thumb-p t))
    (add-subviews scroller
                  (make-instance 'static-text-dialog-item :view-position (make-point 5 5) 
                                 :view-font '("monaco" 9 :srcor)
                                 :dialog-item-text 
                                 "
    BPF Editor Keyboard Commands

H			opens the Window Help file, displaying commands
Return		selects the current PW window, hiding the BPF editor 
Enter			selects the current PW window, hiding the BPF editor  
R			renames the BPF window 
Backspace		deletes the selected point
f			rescales the BPF so that the function fills the editor window
K			removes all point except the first
+			zoom out
-			zoom in
g			show/hide the grid
->			time-stretch the selected points
<-			time-contract the selected points
up-arrow		stretch the values of the selected points
down-arrow		contract the values of the selected points
tab			change to another edit mode following the sequence 
			(edit - zoom - sel - drag ...)
a			add another BPF to the editor
s			select  one BPF 
d			deletes the selected BPF

"))))
