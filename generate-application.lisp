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

(setf *load-verbose* t)

#+ccl (setf ccl:*default-external-format*           :unix
            ccl:*default-file-character-encoding*   :utf-8
            ccl:*default-line-termination*          :unix
            ccl:*default-socket-character-encoding* :utf-8)

(defun say (fmt &rest args)
  (format *trace-output* "~%;;; ~?~%" fmt args)
  (finish-output *trace-output*))

(say "Loading quicklisp.")
(load #P"~/quicklisp/setup.lisp")
(setf quicklisp-client:*quickload-verbose* t)


;;;------------------------------------------------------------
;;; Same as loader.lisp:
;;;

;; The logical host PATCHWORK should be set so that the  _FOSSIL_ file
;; should be  at its root:
;; #+ccl (probe-file #P"PATCHWORK:_FOSSIL_") --> true

;; We use load-logical-pathname-translations to load the logical host PATCHWORK.
;; You must configure it for each implementation.

;; Note: we only only use the PATCHWORK logical host in this file, the
;;       rest of the sources are loaded with ql:quickload/asdf.

#+ccl (setf (logical-pathname-translations "CCL")
            (cons  (list "CCL:*.pathname-translations.*"
                         (merge-pathnames
                          (make-pathname :defaults (user-homedir-pathname)
                                         :directory '(:relative "LOGHOSTS")
                                         :name :wild
                                         :type :unspecific
                                         ;; :type "HOST"
                                         :version :wild)
                          (user-homedir-pathname)
                          nil))
                   (logical-pathname-translations "CCL")))

;; #+ccl (translate-logical-pathname #P"ccl:PATCHWORK.pathname-translations.newest")
;; --> #P"/Users/pjb/LOGHOSTS/PATCHWORK"

(load-logical-pathname-translations "PATCHWORK")

;; An example ~/LOGHOST/PATCHWORK.  I use #P"/home/pjb/" instead of
;; (user-homedir-pathname) because my sources are on a NFS mount, not on
;; the real homedir which is /Users/pjb.
;; 
;; ------------------------------------------------------------------------
;; ;;;; -*- mode:lisp; coding:utf-8; -*-
;; 
;; #.(list
;;    (list "PATCHWORK:**;*.*.*"
;;          (merge-pathnames #P"works/patchwork/patchwork/**/*.*"
;;                           #P"/home/pjb/" nil))
;;    (list "PATCHWORK:**;*.*"
;;          (merge-pathnames #P"works/patchwork/patchwork/**/*.*"
;;                           #P"/home/pjb/" nil))
;;    (list "PATCHWORK:**;*"
;;          (merge-pathnames #P"works/patchwork/patchwork/**/*"
;;                           #P"/home/pjb/" nil)))
;; ------------------------------------------------------------------------



#+ccl       (ccl::cd (truename #P"PATCHWORK:"))
#+lispworks (cd      (truename #P"PATCHWORK:"))
#+clisp     (ext:cd  (truename #P"PATCHWORK:"))

(pushnew #+(or ccl allegro) (truename #P"PATCHWORK:src;")
         #-(or ccl allegro) (truename #P"PATCHWORK:SRC;")
         asdf:*central-registry* :test (function equalp))

(pushnew #+(or ccl allegro) (truename #P"PATCHWORK:src;mclgui;")
         #-(or ccl allegro) (truename #P"PATCHWORK:SRC;MCLGUI;")
         asdf:*central-registry* :test (function equalp))

;; (pushnew 'cl-user::no-cocoa *features*)
#+(and ccl (not cl-user::no-cocoa)) (say "Loading :cocoa (takes some time to start…)")
#+(and ccl (not cl-user::no-cocoa)) (require :cocoa)
#+(and ccl (not cl-user::no-cocoa)) (defparameter *cocoa-readtable* (copy-readtable *readtable*))
#+(and ccl (not cl-user::no-cocoa)) (say "Loading MacOSX Libraries")
#+(and ccl (not cl-user::no-cocoa)) (load #P"PATCHWORK:src;macosx;load-libraries.lisp")


;; DEBUG ;;
(ql:quickload :com.informatimago.common-lisp.lisp.stepper)
;; (print *features*)
(ql:quickload :com.informatimago.clext) ; closer-weak

(ql:quickload :mclgui)
(ui:initialize)

(ql:quickload :patchwork)

;;;
;;;------------------------------------------------------------

;; Let's reset the readtable to the implementation defined one.
#+(and ccl (not cl-user::no-cocoa)) (setf *readtable* (copy-readtable *cocoa-readtable*))
#+(and ccl (not cl-user::no-cocoa)) (require :build-application)

#+(and ccl (not cl-user::no-cocoa))
(defmethod  ccl:application-init-file :around (app)
  (declare (ignorable app))
  (make-pathname :name  "patchwork-init" :type "lisp"
                 :defaults (user-homedir-pathname)))



#+ccl (dolist (lib ccl::*shared-libraries*)
        (say "Shared library: ~A" lib))
(say "Generating ~A" "~/Desktop/Patchwork.app")

#|


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
(progn (princ "ccl:save-application will exit.") (terpri) (finish-output))

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

|#
;;;; the END ;;;;
