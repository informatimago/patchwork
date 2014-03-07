;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pw-scripting-gf.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Extracted generic functions from pw-scripting.lisp.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-08-17 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2013 - 2013
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

(defvar *list-pw-script-objects* nil)
(defvar *contador-pw-script-object* 0)
(defvar *si-record* t)
(defvar *position-new-box* nil)
(defvar *charge?* nil)


(defgeneric even-open     (application theAppleEvent reply handlerRefcon))
(defgeneric even-close    (application theAppleEvent reply handlerRefcon))
(defgeneric even-new      (application theAppleEvent reply handlerRefcon))
(defgeneric even-load     (application theAppleEvent reply handlerRefcon))
(defgeneric even-options  (application theAppleEvent reply handlerRefcon))
(defgeneric even-set      (application theAppleEvent reply handlerRefcon))
(defgeneric even-get      (application theAppleEvent reply handlerRefcon))
(defgeneric even-connect  (application theAppleEvent reply handlerRefcon))
(defgeneric even-unco     (application theAppleEvent reply handlerRefcon))
(defgeneric even-move     (application theAppleEvent reply handlerRefcon))
(defgeneric even-save     (application theAppleEvent reply handlerRefcon))
(defgeneric even-eval     (application theAppleEvent reply handlerRefcon))
(defgeneric even-sele     (application theAppleEvent reply handlerRefcon))
(defgeneric even-rena     (application theAppleEvent reply handlerRefcon))
(defgeneric even-lock     (application theAppleEvent reply handlerRefcon))
(defgeneric even-unlo     (application theAppleEvent reply handlerRefcon))
(defgeneric even-actv     (application theAppleEvent reply handlerRefcon))
(defgeneric even-dupli    (application theAppleEvent reply handlerRefcon))
(defgeneric even-delete   (application theAppleEvent reply handlerRefcon))
(defgeneric even-cut      (application theAppleEvent reply handlerRefcon))
(defgeneric even-copy     (application theAppleEvent reply handlerRefcon))
(defgeneric even-paste    (application theAppleEvent reply handlerRefcon))
(defgeneric even-print    (application theAppleEvent reply handlerRefcon))
(defgeneric even-extend   (application theAppleEvent reply handlerRefcon))
(defgeneric even-play     (application theAppleEvent reply handlerRefcon))
(defgeneric even-stop     (application theAppleEvent reply handlerRefcon))
(defgeneric even-record   (application theAppleEvent reply handlerRefcon))
(defgeneric even-unrecord (application theAppleEvent reply handlerRefcon))

(defgeneric salve-as      (window as?))
(defgeneric get-chords-from-beat (beat))
(defgeneric popUpbox      (buffer))
(defgeneric even-command  (application theAppleEvent reply handlerRefcon))
(defgeneric script-get-selected-file (file-buffer para))

;;;; THE END ;;;;
