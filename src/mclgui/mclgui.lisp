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


(defmacro def-load-pointers (name lambda-list &body body)
  ;; NOTE: we won't implement this, the sources will have to change.
  (niy def-load-pointer name lambda-list body)
  `(niy def-load-pointer ',name ',lambda-list ',body))




(defun initialize/pattern ()
  (niy initialize/pattern))

(defun initialize/screen ()
  (niy initialize/screen))



(defun initialize ()
  "Initialize the MCL GUI."
  (initialize/color)
  (initialize/pattern)
  (initialize/cursor)
  (initialize/scrap)
  (initialize/font)
  (initialize/screen)
  (initialize/menu)
  (initialize/view)
  (initialize/window)
  (initialize/table-dialog-item)
  (initialize/file)
  (initialize/event)
  (initialize/eval)
  (initialize/application)
  (values))


;; (eval-when (:load-toplevel :execute)
;;   ;; It should be better done at application launch…
;;   (initialize))

;;;; THE END ;;;;
