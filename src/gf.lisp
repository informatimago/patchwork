;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               gf.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This file defines patchwork generic functions.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-03-08 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2014 - 2014
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

(defgeneric init-patch (self))
(defgeneric update-size (self size))
(defgeneric set-array (self the-array))
(defgeneric table-layout (self option))
(defgeneric fill-table-disp (self maxlength data increm))
(defgeneric collect (self))
(defgeneric update-all-chord-lines (self chord-lines))

(defgeneric key-pressed-extra (self char))
(defgeneric open-application-help-window (self))
(defgeneric make-application-object (self))

(defgeneric save (self))
(defgeneric duplicate (self))
(defgeneric mn-cut (self))
(defgeneric mn-copy (self))
(defgeneric mn-paste (self))
(defgeneric draw-appl-label (self label))
(defgeneric draw-patch-extra (self))
(defgeneric patch-work-type-of (self ctrl-index)) 

(defgeneric draw-instrument (self x y argument))
(defgeneric play-instrument (self note))
(defgeneric make-super-note-connections (self super-note super-win))
(defgeneric set-output (self o-type))
(defgeneric erase-my-connections (self))

(defgeneric remove-yourself-control (self))
(defgeneric complete-box (box args))

;;;; THE END ;;;;
