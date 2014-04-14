;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               headers.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines logical pathname translations for the additionnal
;;;;    framework header and interface directories.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-04-14 <PJB> Created.
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
(in-package :cl-user)

(defparameter *additionnal-headers-directory*
  (make-pathname :name nil :type nil :version nil
                 :defaults (truename #.(or *compile-file-pathname* *load-pathname* #P"./"))))


(defun headers-wild-pathname (name bits)
  (merge-pathnames (make-pathname
                    :case :local
                    :directory (list :relative
                                     (format nil "headers~A" bits)
                                     name
                                     :wild-inferiors)
                    :name :wild :type :wild
                    :defaults *additionnal-headers-directory*)
                   *additionnal-headers-directory*
                   nil))

  
(defun add-headers-logical-pathname-translations (name)
  (setf (logical-pathname-translations "CCL")
        (list* (list (make-pathname :host "CCL"
                                    :case :local
                                    :name :wild
                                    :type :wild
                                    :directory (list :absolute
                                                     "darwin-x86-headers"
                                                     name
                                                     :wild-inferiors))
                     (headers-wild-pathname name 32))
               (list (make-pathname :host "CCL"
                                    :case :local
                                    :name :wild
                                    :type :wild
                                    :directory (list :absolute
                                                     "darwin-x86-headers64"
                                                     name
                                                     :wild-inferiors))
                     (headers-wild-pathname name 64))
               (logical-pathname-translations "CCL"))))

;;;; THE END ;;;;
