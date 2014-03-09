;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               record.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Recording API.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-03-09 <PJB> Created.
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

(defvar *recorder* nil)

(defgeneric record-event* (recorder class keyword lis)
  (:method (recorder class keyword lis)
    (declare (ignore recorder class keyword lis))
    (values)))

(defgeneric record-select-list* (recorder list)
  (:method (recorder list)
    (declare (ignore recorder list))
    (values)))

(defgeneric record-patch* (recorder class posi name)
  (:method (recorder class posi name)
    (declare (ignore recorder class posi name))
    (values)))

(defgeneric record-menu* (recorder title para self)
  (:method (recorder title para self)
    (declare (ignore recorder title para self))
    (values)))




(defun record-event (class keyword lis)
  (record-event* *recorder* class keyword lis))

(defun record-select-list (list)
  (record-select-list* *recorder* list))

(defun record-patch (class posi name)
  (record-patch* *recorder* class posi name))

(defun record-menu (title para self)
  (record-menu* *recorder* title para self))



(defgeneric delete-extra-inputs (self)
  (:method (self)
    (declare (ignore self))
    (values)))

;;;; THE END ;;;;
