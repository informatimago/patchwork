;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               gestalt.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Determine environment and push *features*.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-03-15 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2014 - 2014
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
(in-package "PATCHWORK.BUILDER")


(defun version (major minor &rest rest)
  (list* major minor rest))


(defun version-split (version-string)
  "We handle n{.n}[-n{.n}]"
  (let ((dash (position #\- version-string)))
    (if dash
        (append (version-split (subseq version-string 0 dash))
                (mapcar (function -)
                        (version-split (subseq version-string (1+ dash)))))
        (mapcar (function parse-integer)
                (split-string version-string ".")))))

(defun version-join (version)
  (let ((neg (position-if (function minusp) version)))
    (if neg
        (format nil "~{~A~^.~A~}-~{~A~^.~A~}"
                (subseq version 0 (1- neg))
                (mapcar (function -) (subseq version (1- neg))))
        (format nil "~{~A~^.~A~}" version))))

(defun maptree (func tree)
  "Calls func on each node of the tree (conses and atoms).
If the function returns the node itself, proceeds recursively,
otherwise uses the result to build the resuling tree."
  (let ((new (funcall func tree)))
    (if (and (consp tree) (eql new tree))
        (cons (maptree func (car tree))
              (maptree func (cdr tree)))
        new)))

(defgeneric version= (a b)
  (:method ((a string) b)      (version= (version-split a) b))
  (:method (a (b string))      (version= a (version-split b)))
  (:method ((a null) b)        (every (function zerop) b))
  (:method (a (b null))        (every (function zerop) a))
  (:method (a b)               (and (equal (first a) (first b))
                                    (version= (rest a) (rest b)))))

(defgeneric version< (a b)
  (:method ((a string) b)      (version< (version-split a) b))
  (:method (a (b string))      (version< a (version-split b)))
  (:method ((a null) (b null)) t)

  (:method ((a null) b)
    (loop :while b
          :do (let ((item (pop b)))
                (cond
                  ((plusp  item) (return t))
                  ((minusp item) (return nil))))
          :finally (return t)))

  (:method (a (b null))
    (loop :while a
          :do (let ((item (pop a)))
                (cond
                  ((plusp  item) (return nil))
                  ((minusp item) (return t))))
          :finally (return nil)))

  (:method (a b)
    (cond
      ((< (car a) (car b)) t)
      ((= (car a) (car b)) (version< (cdr a) (cdr b)))
      (t                   nil))))

(defgeneric version<= (a b)
  (:method (a b)
    (or (version= a b) (version< a b))))

#-(and)
(assert (every (function identity)
               (list
                (not (version< '(10 1) '(10 1 0 -722)))
                (version< '(10 1 0 -722) '(10 1))
                (version< '(10 1) '(10 1 0))
                (not (version< '(10 1 0)   '(10 1)))
                (not (version< '(10 1 0 0) '(10 1)))
                (version< '(10 1 0 -12) '(10 1))
                (not (version< '(10 1 0 33) '(10 1)))
                (version< '(10 0) '(10 1))
                (not (version< '(10 1) '(10 0)))
                (version< '(10 1 0) '(10 2 1))
                (not (version< '(10 2 1) '(10 2 0)))
                (version< '(10 1 0 0) '(10 2 1 0))
                (not (version< '(10 2 1 0) '(10 2 0 0)))
                (not (version< "10.1" "10.1-0.722"))
                (version< "10.1-0.722" "10.1")
                (version< "10.1" "10.10")
                (not (version< "10.10" "10.1"))
                (not (version< "10.10.0" "10.1"))
                (version< "10.1-0.12" "10.1")
                (not (version< "10.10.33" "10.1"))
                (version< "10.0" "10.1")
                (not (version< "10.1" "10.0"))
                (version< "10.10" "10.21")
                (not (version< "10.21" "10.20"))
                (version< "10.10.0" "10.21.0")
                (not (version< "10.21.0" "10.20.0")))))


(defun rt-version=  (a b) (if (version=  a b) '(:and) '(:or)))
(defun rt-version<  (a b) (if (version<  a b) '(:and) '(:or)))
(defun rt-version<= (a b) (if (version<= a b) '(:and) '(:or)))




(defun featurep (key) (member key *features*))

(when (or (and (featurep :ccl)   (featurep :darwin))
          (and (featurep :clisp) (featurep :macosx))
          (and (featurep :sbcl)  (featurep :darwin)))
  (pushnew :cocoa *features*))


(defun system-version ()
  "Run time system version number, as a list (major minor)."
  #+cocoa
  (cffi:with-foreign-pointer (major 4)
    (cffi:with-foreign-pointer (minor 4)
      (when (and (zerop (#_Gestalt #$gestaltSystemVersionMajor major))
                 (zerop (#_Gestalt #$gestaltSystemVersionMinor minor)))
        (list (cffi:mem-ref major :int) (cffi:mem-ref minor :int)))))
  #-cocoa '())

(defun add-cocoa-version-features ()
  "Adds :cocoa-10.4 to :cocoa-10.9, according to the system version."
  (when (featurep :cocoa)
    (loop
      :for minor :from 4 :to 9
      :when (version<= (version 10 minor) (system-version))
        :do (pushnew (intern (format nil "COCOA-~A.~A" 10 minor) "KEYWORD") *features*))))


;;;; THE END ;;;;

