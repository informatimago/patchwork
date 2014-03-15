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
(in-package "COMMON-LISP-USER")
(ql:quickload :cffi)


(defun version (major minor)
  (list major minor))

(defun version= (a b)
  (equal a b))

(defun version< (a b)
  (cond
    ((null a)            (not (null b)))
    ((null b)            nil)
    ((< (car a) (car b)) t)
    ((= (car a) (car b)) (version< (cdr a) (cdr b)))
    (t                   nil)))

(defun version<= (a b)
  (or (version= a b) (version< a b)))


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

(when (featurep :cocoa)
  (loop
    :for minor :from 4 :to 9
    :when (version<= (version 10 minor) (system-version))
    :do (pushnew (intern (format nil "COCOA-~A.~A" 10 minor) "KEYWORD") *features*)))


;;;; THE END ;;;;

