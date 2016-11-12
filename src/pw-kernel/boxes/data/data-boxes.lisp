;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               data-boxes.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;  
;;;;    PW Data Boxes
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
(in-package :pw)

(defunp const ((const list (:type-list ()))) nil
  "This module controls numerical values or lists  (on many levels). It accepts 
either numerical values, symbols, or mixed values. It returns a list without 
evaluating it, and it can also be useful for control of many inputs that must have 
the same list."
  const)

(defunp evconst ((const (list (:value "()")))) nil
  "This module controls numerical values or lists (on many levels). It accepts 
either numerical values, symbols, or mixed values. It returns the evaluation of 
its input. This module behaves like the Lisp expression (eval const), where 
const is the input value of the module. For example, if one writes (+ 1 2) the 
output is 3. "
  (eval const))

(defunp numbox ((val fix/float)) number
"The numbox module accepts integers and floating-point values.
 It returns the value it receives on its input. 
This module is useful for controlling many inputs that must 
have the same value. 
"
 val)
