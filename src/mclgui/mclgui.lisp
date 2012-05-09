;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               mclgui.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines general MCLGUI functions.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-09 <PJB> Created.
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

(defun ensure-list (object)
  (if (listp object)
      object
      (list object)))


(defun initialize-patterns ()
  (niy initialize-patterns))

(defun initialize-screen ()
  (niy initialize-screen))


(defun initialize ()
  (initialize-font)
  (initialize-pattern)
  (initialize-screen)
  (values))





(defmethod view-font (view)
  (niy view-font))

(defmethod set-view-font (view font-spec)
  (niy set-view-font))


(defmethod view-font-codes (view)
  (niy view-font-codes))

(defmethod set-view-font-codes (view ff ms &optional ff-mask ms-mask)
  (niy set-view-font-codes))

;;;; THE END ;;;;
