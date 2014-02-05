;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               fred-window.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    XXX
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-06-11 <PJB> 
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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

(in-package "MCLGUI")

(defclass fred-window (window)
  ((buffer :initform nil :accessor fred-buffer)))


(defgeneric ed-kill-selection (window)
  (:method (window)
    (niy ed-kill-selection window)
    (values)))


(defgeneric buffer-insert-file (buffer file-pathname)
  (:method (buffer file-pathname)
    (niy buffer-insert-file buffer file-pathname)))

(defgeneric buffer-write-file (buffer file-pathname &key if-exists if-does-not-exists)
  (:method (buffer file-pathname &key if-exists if-does-not-exists)
    (niy buffer-write-file buffer file-pathname  if-exists if-does-not-exists)))

(defgeneric buffer-size (buffer)
  (:method (buffer)
    (declare (ignorable buffer))
    0))

(defgeneric buffer-current-sexp (buffer &optional position)
  (:method (buffer &optional position)
    (declare (ignorable buffer position))
    'nil))

(defgeneric buffer-char (buffer position)
  (:method  (buffer position)
    (declare (ignorable buffer position))
    nil))

(defgeneric set-fred-display-start-mark (dialog-item position)
  (:method  (dialog-item position)
    (declare (ignorable dialog-item position))
    (niy set-fred-display-start-mark dialog-item position)
    (values)))

(defgeneric fred-update (dialog-item)
  (:method  (dialog-item)
    (declare (ignorable dialog-item))
    (niy fred-update dialog-item)
    (values)))

;;;; THE END ;;;;
