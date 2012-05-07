;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               generate-application.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;    
;;;;    This script generates the Patchwork application on CCL on MacOSX.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-07 <PJB> Changed license to GPL3; Added this header.
;;;;    2012-04-12 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
;;;;    Copyright IRCAM 2012 - 2012
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

#+ccl (setf ccl:*default-external-format*           :unix
            ccl:*default-file-character-encoding*   :utf-8
            ccl:*default-line-termination*          :unix
            ccl:*default-socket-character-encoding* :utf-8)

(load #P"~/quicklisp/setup.lisp")

#+ccl       (ccl::cd #P"/home/pjb/works/patchwork/patchwork/")
#+lispworks (ccl::cd #P"/home/pjb/works/patchwork/patchwork/")

(pushnew #P"/home/pjb/works/patchwork//patchwork/src/"
         asdf:*central-registry* :test (function equalp))
(ql:quickload :patchwork)

;; (pushnew 'cl-user::no-cocoa *features*)

#+(and ccl (not cl-user::no-cocoa)) (require :cocoa)
#+(and ccl (not cl-user::no-cocoa)) (require :build-application)
#+(and ccl (not cl-user::no-cocoa))
(defmethod  ccl:application-init-file :around (app)
  (declare (ignorable app))
  (make-pathname :name  "patchwork-init" :type "lisp"
                 :defaults (user-homedir-pathname)))

#+(and ccl (not cl-user::no-cocoa))
(ccl::build-application
 :name "PatchWork"
 :directory #P"~/Desktop/"
 :copy-ide-resources t
 ;; :init-file "HOME:patchwork-init.lisp"
 ;; '(pathname "~/application-init.lisp")
 ;;  (lambda ()
 ;;              (make-pathname :name  "patchwork-init" :type "lisp"
 ;;                             :defaults (user-homedir-pathname)))
 )

#+(and ccl  cl-user::no-cocoa)
(ccl::save-application
 #P"~/Desktop/PatchWork"
 :init-file "HOME:patchwork-init.lisp"
 ;; :native t
 :prepend-kernel t
 ;; '(pathname "~/patchwork-init.lisp")
 ;;  (lambda ()
 ;;              (make-pathname :name  "patchwork-init" :type "lisp"
 ;;                             :defaults (user-homedir-pathname)))
 )




#+lispworks
(hcl:save-image-with-bundle #P"~/Desktop/PatchWork.app"
                            :console :always)

;;;; the END ;;;;
