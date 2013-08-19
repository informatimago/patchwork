;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               lelisp-macros.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;    
;;;;    XXX
;;;;    
;;;;AUTHORS
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
;;;;    
;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;=========================================================
;;;;
;;;;  PATCH-WORK
;;;;  By Mikael Laurson, Jacques Duthen, Camilo Rueda.
;;;;  © 1986-1992 IRCAM 
;;;;
;;;;=========================================================

(defpackage "LELISP-MACROS"
  (:use "COMMON-LISP")
  #+:ccl
  (:import-from "CCL" "MEMQ" "ASSQ" "DELQ" "NEQ" "TRUE" "FALSE" "WHILE" "UNTIL")
  #+:loop
  (:import-from "LOOP" "FOR" "WHILE") ;; seront aussi fonctions
  (:export
   "MEMQ" "ASSQ" "DELQ" "NEQ" "TRUE" "FALSE"
   "RASSQ" "CASSQ"
   "WHILE" "UNTIL" "IFNOT" "REPEAT" "FOR"
   "NEWL" "NEXTL" "VREF" "VSET"
   "TELL" "ASK" "ASK-ALL" "WITH"))
(in-package "LELISP-MACROS")


;; =============================================================================-======
;; This file "LELISP-MACROS.Lisp" exports lisp macros.
;; Macros have a special status for the compiler:
;; Any file exporting macros must be LOADED
;; before "compile-file" can compile any source file using these macros,
;; otherwise you get strange error messages ("undefined variable", "cant compile ...").
;; So any file saying :
;;	(load-once "CLPF:LELISP-MACROS")
;; MUST embed it in a good "eval-when":
;; (eval-when (eval compile)
;;	(load-once "CLPF:LELISP-MACROS"))
;; It is not needed to import the macros since they are exported to common lisp
;;	(use-package "LELISP-MACROS") ;is not needed

;; =============================================================================-======

#-:ccl
(progn
  ; From "MACL1.3.2:PCL:macros.lisp"
  (defun memq (item list) (member item list :test #'eq))
  (defun assq (item list) (assoc  item list :test #'eq))
  (defun delq (item list) (delete item list :test #'eq))
  (defun neq (x y) (not (eq x y)))
  (declaim (inline memq assq delq neq))
  (defmacro true  (&body body) `(progn ,@body t))
  (defmacro false (&body body) `(progn ,@body nil)))

(defun rassq (item list) (rassoc item list :test #'eq))
(defun cassq (item list) (cdr (assq item list)))
(declaim (inline rassq cassq))
;; =============================================================================-======

;; [jack] 910821 ui::while is more efficient than Lee's with loop

#-:ccl
(defmacro while (condition &body body)
  (let ((begin (gensym "WHILE-")) (end (gensym "WHILE-")))
    `(tagbody
       ,begin
       (when ,condition (go ,end))
       ,@body
       (go ,begin)
       ,end
       nil)))

#-:ccl
(defmacro until (condition &body body)
  `(while (not ,condition) ,@body))

(defmacro ifnot (testform elseform &body body)
  `(if (not ,testform) ,elseform (progn ,@body)))

(defmacro repeat (count &body body)
  `(dotimes (,(gensym) ,count)
     ,@body))

(defmacro for ((var begin step end) &body body)
  (let ((s2 (gensym)) (s3 (gensym)))
    `(let ((,var ,begin) (,s2 ,step) (,s3 ,end))
       (if (> ,s2 0)
         (loop
           (when (> ,var ,s3) (return))
           (progn ,@body)
           (incf ,var ,s2))
         (loop
           (when (< ,var ,s3) (return))
           (progn ,@body)
           (incf ,var ,s2))))))

;; =============================================================================-======

(defmacro newl (lst elem) `(push ,elem ,lst))

(defmacro nextl (lst &optional symb)
  (if symb
    `(setq ,symb (pop ,lst))
    `(pop ,lst) ))

(defun vref (vect index)     (svref vect index))
(defun (setf vref) (new-value vect index)  (setf (svref vect index) new-value))
(defun vset (vect index val) (setf (svref vect index) val))
(declaim (inline vref vset))

;; =============================================================================-======

(defmacro tell (outlet fun &rest args)
  (let ((args-var (gensym "ARGS-")) (fun-var (gensym "FUN-")))
    (if args
      `(let ((,args-var (list ,@args)) (,fun-var ,fun))
         (mapc (lambda (x) (apply ,fun-var x ,args-var)) ,outlet))
      `(mapc ,fun ,outlet))))

(defmacro ask (outlet fun &rest args)
  (let ((args-var (gensym "ARGS-")) (fun-var (gensym "FUN-"))
        (out-var (gensym "OUT-")) (result-var (gensym "RESULT-")))
    `(let ((,args-var (list ,@args)) (,fun-var ,fun) (,result-var nil))
       (dolist (,out-var ,outlet)
         (when (setq ,result-var (apply ,fun-var ,out-var ,args-var))
           (return)))
       ,result-var)))

(defmacro ask-all (outlet fun &rest args)
  (let ((args-var (gensym "ARGS-")) (fun-var (gensym "FUN-")))
    (if args
      `(let ((,args-var (list ,@args)) (,fun-var ,fun))
         (mapcar (lambda (x) (apply ,fun-var x ,args-var)) ,outlet))
      `(mapcar ,fun ,outlet))))

;; =============================================================================-======
;; The syntax is different from the Le_Lisp "with"

(defmacro with (l-place-value &body body)
  "Changes locally the value of \"setf-able\" places (like a \"let\" where places
would not be restricted to variables)."
  (let ((places (mapcar #'first l-place-value))
        (values (mapcar #'second l-place-value))
        (vars (mapcar (lambda (pv) (declare (ignore pv)) (gensym "WITH-"))
                      l-place-value)))
    `(let ,(mapcar #'list vars places)
       (unwind-protect
         (progn
           ,@(mapcar (lambda (place value) `(setf ,place ,value)) places values)
           ,@body)
         ,@(mapcar (lambda (place var) `(setf ,place ,var)) places vars)))))

;;(let ((l '(a . b))) (with (((car l) 1) ((cdr l) 2)) (print l)))

;; =============================================================================-======
