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

(eval-when (eval compile load)
  (load-once "PW:PW-Kernel;Environment;PW-symbolic-types"))

(defpackage "EPW" 
  (:use "COMMON-LISP" "PW-STYPE")
  (:import-from "PATCH-WORK" "DEFUNP" "DEFUNT" "DEFMETHODP" "SELF"))

;; =============================================================================-======

(in-package "EPW")

(export '(defune))

;;(import '(pw::defunp pw::defunt pw::defmethodp pw::self))

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
