;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               global-vars.lisp
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

;;================================================================
;;(defvar *selection-buttons-window* ())
(defvar *selection-buttons-x* ())
(defvar *selection-buttons-y* ())
;;(defvar  *global-pw-edit-control* ())
;;(defvar  *previous-global-pw-edit-control* ())
(defvar  *beat-leaf-objs* ())
;;================================================================
(defvar *selection-buttons-pool* ())
(defvar *rtm-struct-selection-scrap* ())
(defvar *measure-selection-scrap* ())
(defvar *measure-line-selection-scrap* ())
(defvar *beat-chord-scrap* ())
(defvar *rec-rtm-chs-list* ())

(defvar *current-rtm-editor* ())
(defvar *rtm-editor-velocity-list* ())
(defvar *active-RTM-window* ())

(defvar *rtm-last-chord-object* ())
(defvar *rtm-duration-scaler* 1.0)

(defvar *RTM-window-counter* 0)
(defvar *rtm-last-note-pixel* 0)
(defvar *measure-edit-mode* ()) ;???

(defvar *apps-RTM-menu-item* ())
(defvar *RTM-menu-root* () "menubar for RTM")
(defvar *rtm-help-window* ())

