;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;=========================================================
;;;;
;;;;  PATCH-WORK
;;;;  By Mikael Laurson, Jacques Duthen, Camilo Rueda.
;;;;  © 1986-1992 IRCAM 
;;;;
;;;;=========================================================

;; ==========================================================================
;; [jack] 30.06.90               PW-Symbolic-Types.Lisp
;; ==========================================================================

(defpackage "PW-STYPE"
  (:use "COMMON-LISP")
  (:import-from "PATCH-WORK"
   "C-PW-TYPE" "C-PATCH"
   "C-NUMBOX" "REPEAT" "WHILE")
  (:export
   "CAR!" "LIST!" "DEEP-MAPCAR" "DOUBLE-MAPCAR"
))

(in-package "PW-STYPE")

;; =============================================================================-======

;; ==== utilities functions ====

(defun car! (thing)
  "Returns (caa...ar <thing>).  Applies #'car as many times as possible (maybe 0)."
  (ifnot (consp thing) thing (car! (car thing))))

(defun list! (thing)
  "Returns a list containing <thing> or <thing> if it's already a list."
  (if (listp thing) thing (list thing)))

(defun deep-mapcar (fun fun1 list? &rest args)
  "Mapcars <fun> or applies <fun1> to <list?> <args> whether <list?> is a list or not."
   (cond
    ((null list?) ())
    ((not (consp list?)) (apply fun1 list? args))
    (t (cons (apply #'deep-mapcar fun fun1 (car list?) args)
             (apply #'deep-mapcar fun fun1 (cdr list?) args)))))

(defun double-mapcar (fun1 list1? list2? &rest args)
  "Mapcars <fun> or applies <fun1> to <list1?> <list2?> <args>
whether each of <list1?> <list2?> is a list or not."
   (cond
    ((consp list1?)
     (if (consp list2?)
       ;(error "cannot double-mapcar 2 lists: ~S and ~S~%." list1? list2?)
       (mapcar #'(lambda (x1 x2) (apply fun1 x1 x2 args))
               list1? list2?)
       (mapcar #'(lambda (x) (apply fun1 x list2? args))
               list1?)))
    ((consp list2?)
     (mapcar #'(lambda (x) (apply fun1 list1? x args))
             list2?))
    (t (apply fun1 list1? list2? args))))

#|
(defmacro mapcar! (fun fun1 list? . args)
  "Mapcars <fun> or applies <fun1> to <list?> <args> whether <list?> is a list or not.
All the arguments <fun> <fun1> <list?> <args> must not side-effect.
<list?> must not contain any dotted pair."
  `(ifnot (listp ,list?)
     (,fun1 ,list? ,.args)
     (mapcar ,(ifnot args `#',fun `#'(lambda (arg1) (,fun arg1 ,.args)))
       ,list?)))

(defun do-+ (freqs)
  (mapcar! do-+ one-+ freqs))

(defun do-rec-+ (freqs)
  (deep-mapcar 'do-rec-+ 'one-+ freqs))

(defun one-+ (x) (1+ x))

(time (repeat 100 (do-+ '((4 8) 3 (6) ()))))
;     (repeat 100 (do-+ '((4 8) 3 (6) nil))) took 96 ticks (1.600 seconds) to run.
;= ((5 9) 4 (7) nil)

(time (repeat 100 (do-rec-+ '((4 8) 3 (6) ()))))
;     (repeat 100 (do-rec-+ '((4 8) 3 (6) nil))) took 95 ticks (1.583 seconds) to run.
;= ((5 9) 4 (7) nil)

(defun do-++ (freqs val)
  (mapcar! do-++ one-++ freqs val))

(defun do-rec-++ (freqs val)
  (deep-mapcar 'do-++ 'one-++ freqs val))

(defun one-++ (x val) (+ x val))

(time (repeat 100 (do-++ '((4 8) 3 (6) ()) 2)))
;     (repeat 100 (do-++ '((4 8) 3 (6) nil) 2)) took 113 ticks (1.883 seconds) to run.
;= ((6 10) 5 (8) nil)

(time (repeat 100 (do-rec-++ '((4 8) 3 (6) ()) 2)))
;     (repeat 100 (do-rec-++ '((4 8) 3 (6) nil) 2)) took 112 ticks (1.867 seconds) to run.
;= ((6 10) 5 (8) nil)
|#

;; ============================================================================`

