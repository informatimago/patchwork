;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               color.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Colors.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-15 <PJB> Extracted from menu.
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


(defclass colored ()
  ((color-list :initform '()
               :documentation "The property-list of key and colors for all the parts of the thing."
               :reader part-color-list))
  (:documentation "A mix-in for colored things."))



(defgeneric part-color (thing key)
  (:documentation "
RETURN:         The color of the part KEY of the THING.
")
  (:method ((thing t) key)
    (declare (ignore key))
    nil)
  (:method ((thing colored) key)
    (getf (slot-value thing 'color-list) key nil)))


(defgeneric set-part-color (thing key new-color)
  (:documentation "
DO:             Sets the color of the part KEY of the THING to NEW-COLOR,
                or resets it if NEW-COLOR is NIL.
")
  (:method ((thing t) key new-color)
    (declare (ignore key))
    new-color)
  (:method ((thing colored) key new-color)
    (if new-color
        (setf (getf (slot-value thing 'color-list) key) new-color)
        (remf (slot-value thing 'color-list) key))
    new-color))


(defgeneric color-parts (thing)
  (:documentation "
RETURN:         A list of key parts that can be colored in the THING.
")
  (:method ((thing t))
    '()))


;;;; THE END ;;;;
