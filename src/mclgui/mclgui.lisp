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



(defun initialize/screen ()
  (multiple-value-bind (pos siz) (main-screen-frame)
    (declare (ignore pos))
    (setf *screen-width*       (point-h siz)
          *screen-height*      (point-v siz)))
  (values))


(defvar *initialized* nil)

(defun initialize ()
  "Initialize the MCL GUI."
  (unless *initialized*
    (initialize/application)
    (initialize/screen)
    (initialize/region)
    (initialize/color)
    (initialize/pattern)
    (initialize/pen)
    (initialize/cursor)
    (initialize/scrap)
    (initialize/font)
    (initialize/menu)
    (initialize/view)
    (initialize/window)
    (initialize/table-dialog-item)
    (initialize/file)
    (initialize/event)
    (initialize/eval)
    #+has-appleevent (initialize/apple-event)
    (setf *initialized* t))
  (values))


(initialize) ; It should be better done at application launch…

;;;; THE END ;;;;
