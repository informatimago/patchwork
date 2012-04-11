;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;=========================================================
;;;;
;;;;  PATCH-WORK
;;;;  By Mikael Laurson, Jacques Duthen, Camilo Rueda.
;;;;  © 1986-1992 IRCAM 
;;;;
;;;;=========================================================

(defpackage "LeLisp-macros" (:use "COMMON-LISP")
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

(in-package "LeLisp-macros")

(do-external-symbols (s "LeLisp-macros")
  (import s "COMMON-LISP")
  (export s "COMMON-LISP"))

;; =============================================================================-======
;; This file "LeLisp-macros.Lisp" exports lisp macros.
;; Macros have a special status for the compiler:
;; Any file exporting macros must be LOADED
;; before "compile-file" can compile any source file using these macros,
;; otherwise you get strange error messages ("undefined variable", "cant compile ...").
;; So any file saying :
;;	(load-once "CLPF:LeLisp-macros")
;; MUST embed it in a good "eval-when":
;; (eval-when (eval compile)
;;	(load-once "CLPF:LeLisp-macros"))
;; It is not needed to import the macros since they are exported to common lisp
;;	(use-package "LeLisp-macros") ;is not needed

;; =============================================================================-======

#-:ccl
(progn
  ; From "MACL1.3.2:PCL:macros.lisp"
  (defmacro memq (item list) `(member ,item ,list :test #'eq))
  (defmacro assq (item list) `(assoc ,item ,list :test #'eq))
  (defmacro delq (item list) `(delete ,item ,list :test #'eq))
  (defmacro neq (x y) `(not (eq ,x ,y)))
  (defmacro true (&body body) `(progn ,@body t))
  (defmacro false (&body body) `(progn ,@body nil)))

(defmacro rassq (item list) `(rassoc ,item ,list :test #'eq))
(defmacro cassq (item list) `(cdr (assq ,item ,list)))

;; =============================================================================-======

;; [jack] 910821 ccl::while is more efficient than Lee's with loop

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
  `(while (not ,condition) ,.body))

(defmacro ifnot (testform elseform &body body)
  `(if (not ,testform) ,elseform (progn ,.body)))

(defmacro repeat (count &body body)
  `(dotimes (,(gensym) ,count)
     ,.body))

(defmacro for ((var begin step end) &body body)
  (let ((s2 (gensym)) (s3 (gensym)))
    `(let ((,var ,begin) (,s2 ,step) (,s3 ,end))
       (if (> ,s2 0)
         (loop
           (when (> ,var ,s3) (return))
           (progn ,.body)
           (incf ,var ,s2))
         (loop
           (when (< ,var ,s3) (return))
           (progn ,.body)
           (incf ,var ,s2))))))

;; =============================================================================-======

(defmacro newl (lst elem) `(push ,elem ,lst))

(defmacro nextl (lst &optional symb)
  (if symb
    `(setq ,symb (pop ,lst))
    `(pop ,lst) ))

(defmacro vref (vect index) `(svref ,vect ,index))
(defmacro vset (vect index val) `(setf (svref ,vect ,index) ,val))

;; =============================================================================-======

(defmacro tell (outlet fun &rest args)
  (let ((args-var (gensym "ARGS-")) (fun-var (gensym "FUN-")))
    (if args
      `(let ((,args-var (list ,@args)) (,fun-var ,fun))
         (mapc #'(lambda (x) (apply ,fun-var x ,args-var)) ,outlet))
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
         (mapcar #'(lambda (x) (apply ,fun-var x ,args-var)) ,outlet))
      `(mapcar ,fun ,outlet))))

;; =============================================================================-======
;; The syntaxe is different from the Le_Lisp "with"

(defmacro with (l-place-value &body body)
  "Changes locally the value of \"setf-able\" places (like a \"let\" where places
would not be restricted to variables)."
  (let ((places (mapcar #'first l-place-value))
        (values (mapcar #'second l-place-value))
        (vars (mapcar #'(lambda (pv) (declare (ignore pv)) (gensym "WITH-"))
                      l-place-value)))
    `(let ,(mapcar #'list vars places)
       (unwind-protect
         (progn
           ,.(mapcar #'(lambda (place value) `(setf ,place ,value)) places values)
           ,.body)
         ,.(mapcar #'(lambda (place var) `(setf ,place ,var)) places vars)))))

;(let ((l '(a . b))) (with (((car l) 1) ((cdr l) 2)) (print l)))

;; =============================================================================-======
