;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               generate-interfaces.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Build the CCL Interface Databases.
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
(require :parse-ffi)

(defparameter *additionnal-headers-directory*
  (make-pathname :name nil :type nil :version nil
                 :defaults (truename #.(or *compile-file-pathname* *load-pathname* #P"./"))))

(load (merge-pathnames "headers.lisp" *additionnal-headers-directory*))

(add-headers-logical-pathname-translations "midishare")
(ensure-directories-exist #P"CCL:darwin-x86-headers;midishare;test.file")
(ensure-directories-exist #P"CCL:darwin-x86-headers64;midishare;test.file")
(ccl::parse-standard-ffi-files :midishare)

(add-headers-logical-pathname-translations "player")
(ensure-directories-exist #P"CCL:darwin-x86-headers;player;test.file")
(ensure-directories-exist #P"CCL:darwin-x86-headers64;player;test.file")
(ccl::parse-standard-ffi-files :player)

(add-headers-logical-pathname-translations "coreservices")
(ensure-directories-exist #P"CCL:darwin-x86-headers;coreservices;test.file")
(ensure-directories-exist #P"CCL:darwin-x86-headers64;coreservices;test.file")
(ccl::parse-standard-ffi-files :coreservices)

;;;; THE END ;;;;
