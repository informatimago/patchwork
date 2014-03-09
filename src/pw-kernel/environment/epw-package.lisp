;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               epw-package.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;    
;;;;    This file (and no other one) contains also the definition of the package "EPW".
;;;;    
;;;;AUTHORS
;;;;    Mikael Laurson, Jacques Duthen, Camilo Rueda.
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-07 <PJB> Changed license to GPL3; Added this header.
;;;;    1991-09-13 [jack] EPW-Package.Lisp
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
(defpackage "EPW" 
  (:use "COMMON-LISP" "LELISP-MACROS" "CLPF-UTIL" "PATCHWORK" "PW-STYPE")
  (:shadow "MAKE-NUM-FUN")
  (:export "*ASCII-NOTE-C-SCALE*" "*ASCII-NOTE-DO-SCALE*"
           "*ASCII-NOTE-SCALES*" "ARITHM-SER"
           "AVERAGE" "DEFUNE" "DISTOR" "DISTOR-EXT" "F-BINARY-SEARCH"
           "FLAT" "FLAT-ONCE" "FUN-MINMAX" "G-MAX" "G-MIN" "G-SCALING"
           "INCLUDED?" "INTERPOLATION" "L-LAST" "L-MAX" "L-MIN"
           "L-NTH" "L-SCALER/MAX" "L-SCALER/SUM" "L-SUPPRESS"
           "LIST-EXPLODE" "LIST-FILL" "LIST-PART" "LO-FLAT"
           "MAKE-LIST2" "MAT-TRANS" "NTH-RANDOM" "PERMUT-CIRC"
           "PERMUT-RANDOM" "SORT-LIST" "X-APPEND" "X-DIFF"
           "X-INTERSECT" "X-UNION" "X-XOR" "MAKE-NUM-FUN" "LAGRANGE"
           "LINEAR" "POWER-FUN" "POWER/2" "POWER/3" "PARABOLE/2"
           "PARABOLE/3"))
(in-package "EPW")


(defmacro defune (name args outtype documentation &body body)
  `(defunp ,name ,args ,outtype ,documentation ,@body))

(defvar *EPW-files*
  '(
    "EPW:Combinatorial"
    "EPW:Freq-Harmony"
    "EPW:Chord-Filter"
    "EPW:Harmonicity"
    "EPW:crime-fm"
    ;"EPW:EPW-Menus"
    ))

;;;; THE END ;;;;
