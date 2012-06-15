;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               system.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    System functions
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-16 <PJB> Created.
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


(defun fixnump (object)
  (typep object 'fixnum))


(defun ed-beep (&optional (duration 3) &rest args)
  (declare (ignorable duration args))
  #+ccl (#_NSBeep)
  #-ccl (niy ed-beep duration args))

(defun get-sys-font ()
  0)

(defun get-sys-just ()
  0)



(defun ensure-list (object)
  "
RETURN:         If OBJECT is a list then OBJECT, otherwise a fresh
                list containing OBJECT.
"
  (if (listp object)
      object
      (list object)))


(defun list-designator (object)
  "
RETURN:         If the OBJECT is a list containing a single non-NIL
                atom, then this first element is returned, else OBJECT.
"
  (if (and (listp object)
           object
           (endp (rest object))
           (first object)
           (atom (first object)))
      (first object)
      object))


;;;; THE END ;;;;
