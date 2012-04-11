;;;; -*- mode:lisp; coding:utf-8 -*-
;; ==========================================================
;; [jack] 910913               EPW-Package.Lisp
;;;;=========================================================
;;;;
;;;;  PATCH-WORK
;;;;  By Mikael Laurson, Jacques Duthen, Camilo Rueda.
;;;;  © 1986-1992 IRCAM 
;;;;
;;;;=========================================================
;; This file (and no other one) contains also the definition of the package "EPW"


(defpackage "EPW" 
  (:use "COMMON-LISP" "LELISP-MACROS" "CLPF-UTIL" "PATCH-WORK" "PW-STYPE")

  (:export "*ASCII-NOTE-C-SCALE*" "*ASCII-NOTE-DO-SCALE*"
           "*ASCII-NOTE-SCALES*" "*NO-SHARP-READ-TABLE*" "ARITHM-SER"
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


;; =============================================================================-======

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

;; =============================================================================-======
