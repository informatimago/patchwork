;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               load-rtm.lisp
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
(in-package :pw)


#|
now defined in Music-Package.lisp GA 20/06/94

(defvar *all-rhythm-files*
  (list "PW:PW-Music;editors;rhythm;global-vars" 
  "PW:PW-Music;editors;rhythm;rtm-selection-button" 
  "PW:PW-Music;editors;rhythm;beat-measure-measure-line" 
  "PW:PW-Music;editors;rhythm;rtm-editor" 
  "PW:PW-Music;editors;rhythm;rtm-window" 
  "PW:PW-Music;Boxes;edit;rtm-patch" 
  "PW:PW-Music;editors;rhythm;rtm-midi-files" 
  "PW:PW-Music;editors;rhythm;rtm-help-window" 
  "PW:PW-Music;editors;rhythm;print-rtm" 
  "PW:PW-Music;Menu;rtm-menu" 
  "PW:PW-Music;editors;rhythm;rtm-dialog-win" 
  "CLENI:cleni" ; GAS 920811 
  "PW:PW-Music;Boxes;edit;quantizer" 
  "PW:PW-Music;editors;rhythm;rtm-cleni-interface"
  "PW:PW-Music;editors;rhythm;rtm-paging+kill"
  "PW:PW-Music;Boxes;edit;rhythm-formation"))

(defun update-all-rhythm-files ()
  (mapc  #'|CLPF-UTIL|:compile-file? *all-rhythm-files*))

(mapc #'(lambda (file) (load-once file)) *all-rhythm-files*)

|#

(in-package :pw)
(defclass  C-patch-application-rtm-editor (C-patch-application C-process-begin+end)  
  ((clock :initform 0 :accessor clock)
   (clock-obj :initform *global-clock* :allocation :class :accessor clock-obj)
   (chord-objects :initform nil :accessor chord-objects)
;;   (previous-t-time :initform nil :accessor previous-t-time)
   (play-flag :initform nil :accessor play-flag)
  (measure-line :initform (make-instance 'C-measure-line) :initarg :measure-line :accessor measure-line)
))
