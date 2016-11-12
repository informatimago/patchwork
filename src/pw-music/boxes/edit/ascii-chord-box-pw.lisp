;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               ascii-chord-box-pw.lisp
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

;; ===========================================================================
;; From "CAO:From-Mika-910218:PW-CL:PatchWork:PW-Library:basic-library-boxes.Lisp" 
(in-package :pw)

(defvar *no-sharp-read-table* (let ((rt (copy-readtable nil)))
                                (set-syntax-from-char #\# #\A rt)
                                rt))


(defclass C-pw-ascii-chord-box (C-pw-resize-x) ())

(defmethod patch-value ((self C-pw-ascii-chord-box) obj)
  (epw::n->mc (let ((*readtable* *no-sharp-read-table*))
                (read-from-string (patch-value (car (input-objects self)) obj)))))

(defmethod compile-me ((self C-pw-ascii-chord-box) obj)
  (let ((ctrl (car (pw-controls self)))
        (in (car (input-objects self))))
    (if (eql ctrl in)
        `',(epw::n->mc (let ((*readtable* *no-sharp-read-table*))
                         (read-from-string (patch-value ctrl obj))))
        `(epw::n->mc (let ((*readtable* *no-sharp-read-table*))
                       (read-from-string ,(compile-me in obj)))))))

(defunp ascii-chord ((chord (absin (:dialog-item-text "()")))) midic
    "Double click the input and type in any list of ascii symbolic notes, for example:
\"(C3 C3+25 C+3 C#3-25 C#3 Db3 C#3+25)\"
The pw-ascii-chord-box will return the corresponding midic notes, in this case:
\(6000 6025 6050 6075 6100 6100 6125). "
  (declare (ignore chord)))
