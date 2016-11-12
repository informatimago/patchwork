;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               reader-macros.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;  
;;;;    Defines the #I reader macro for clpf-util expressions.
;;;;  
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-03-01 <PJB> Pruned commented out code, moved into pw-kernel/environment/.
;;;;    2012-05-07 <PJB> Changed license to GPL3; Added this header.
;;;;    2012-04-09 <PJB> Created
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;  
;;;;    Copyright Pascal J. Bourguignon 2012 - 2014
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

(in-package "PW")


(defun sharp-i-dispatch-reader-macro (stream char count)
  (declare (ignore char count))
  (if *read-suppress*
      (progn (read stream t nil t)
             (values))
      (values (clpf-util:prefix-expr (read stream t nil t))))) ; maybe (CLtLII p548)


(defun set-patchwork-reader-macros ()
  (set-dispatch-macro-character #\# #\i (function sharp-i-dispatch-reader-macro))
  (ui:enable-sharp-at-reader-macro)
  (values))

(defun reset-patchwork-reader-macros ()
  (set-dispatch-macro-character #\# #\i nil)
  (ui:disable-sharp-at-reader-macro)
  (values))

(defparameter *readtable-patchwork*
  (let ((*readtable* (copy-readtable))) ; we want ccl reader macros for cocoa.
    (set-patchwork-reader-macros)
    *readtable*))

(defmacro enable-patchwork-reader-macros ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (set-patchwork-reader-macros)
     (values)))

(defmacro disable-patchwork-reader-macros ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (reset-patchwork-reader-macros)
     (values)))


;;;; THE END ;;;;


