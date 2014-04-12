;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               builder.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Patchwork building script common to generate-application and loader.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-03-26 <PJB> Created.
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
(defpackage "PATCHWORK.BUILDER"
  (:use "COMMON-LISP")
  (:shadowing-import-from "COM.INFORMATIMAGO.TOOLS.PATHNAME"
                          "MAKE-PATHNAME"
                          "USER-HOMEDIR-PATHNAME"
                          "TRANSLATE-LOGICAL-PATHNAME")
  (:use "COM.INFORMATIMAGO.TOOLS.MANIFEST")
  (:use "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE")
  (:export "*PATCHWORK-VERSION*"
           "VERSION" "VERSION=" "VERSION<" "RT-VERSION=" "RT-VERSION<" "RT-VERSION<="
           "FEATUREP" "SYSTEM-VERSION" "ADD-COCOA-VERSION-FEATURES"))
(in-package "PATCHWORK.BUILDER")
(declaim (optimize (safety 3) (debug 3) (space 0) (speed 0)))

#+ccl (setf ccl:*default-external-format*           :unix
            ccl:*default-file-character-encoding*   :utf-8
            ccl:*default-line-termination*          :unix
            ccl:*default-socket-character-encoding* :utf-8)

(setf *load-verbose* t
      *print-right-margin* 110)

(defun say (fmt &rest args)
  (format *trace-output* "~&;;; ~?~%" fmt args)
  (finish-output *trace-output*))





;; The logical host PATCHWORK should be set so that the .git/ subdirectory
;; should be  at its root:
;; #+ccl (probe-file #P"PATCHWORK:.git;") --> true

;; We use load-logical-pathname-translations to load the logical host PATCHWORK.
;; You must configure it for each implementation.

;; Note: we only only use the PATCHWORK logical host in this file, the
;;       rest of the sources are loaded with ql:quickload/asdf.


;; Other logical hosts used by patchwork or its dependencies include:
;;   PW-USER  -- See src/application.lisp  initialize-directories
;;   CLENI


;; Map ccl pathname translations to ~/LOGHOSTS/${logical_host}
#+ccl (setf (logical-pathname-translations "CCL")
            (cons  (list "CCL:*.pathname-translations.*"
                         (merge-pathnames
                          (make-pathname :defaults (user-homedir-pathname)
                                         :directory '(:relative "LOGHOSTS")
                                         :name :wild
                                         :type :unspecific
                                         :version :wild)
                          (user-homedir-pathname)
                          nil))
                   (logical-pathname-translations "CCL")))

;; (translate-logical-pathname #P"ccl:PATCHWORK.pathname-translations.newest")
;; (translate-logical-pathname #P"ccl:PW-USER.pathname-translations.newest")
;; (translate-logical-pathname #P"PW-USER:A;B;C.D")#P"/home/pjb/works/patchwork/pw-user/A/B/C.D"


(defun make-logical-pathname-translations (logical-host base-pathname)
  (list
   (list (format nil "~A:**;*.*.*"   logical-host)
         (merge-pathnames #P"**/*.*" base-pathname nil))
   (list (format nil "~A:**;*.*"     logical-host)
         (merge-pathnames #P"**/*.*" base-pathname nil))
   (list (format nil "~A:**;*"       logical-host)
         (merge-pathnames #P"**/*"   base-pathname nil))))

(defun generate-logical-pathname-translation-file (logical-host base-pathname &key (force nil))
  "Generate a default logical pathname translation mapping the logical-host to a given base directory."
  (let ((translation-file (merge-pathnames (make-pathname :directory `(:relative "LOGHOSTS")
                                                          :name (string-upcase logical-host)
                                                          :type nil)
                                           (user-homedir-pathname))))
    (with-open-file (trans translation-file
                           :if-does-not-exist :create
                           :if-exists (if force :supersede nil)
                           :direction :output
                           :external-format :default)
      (if trans
        (let ((*print-pretty*       t)
              (*print-right-margin* 60)
              (*print-circle*       nil))
          (format trans ";;;; -*- mode:lisp;coding:us-ascii; -*-~2%")
          (format trans "~S~%" (make-logical-pathname-translations logical-host base-pathname)))
        (cerror "~A already exists; aborting." translation-file)))
    translation-file))


(defun install-patchwork (&key (force nil))
  (generate-logical-pathname-translation-file "PATCHWORK" #P"/home/pjb/works/patchwork/patchwork/"                  :force force)
  (generate-logical-pathname-translation-file "PW-USER"   #P"/home/pjb/works/patchwork/pw-user/"                    :force force)
  (generate-logical-pathname-translation-file "CLENI"     #P"/home/pjb/works/patchwork/patchwork/src/pw-lib/cleni/" :force force))
;; (install-patchwork :force t)


(load-logical-pathname-translations "PW-USER")
(load-logical-pathname-translations "CLENI")

;; (load-logical-pathname-translations "PATCHWORK")

(setf (logical-pathname-translations "PATCHWORK") nil
      (logical-pathname-translations "PATCHWORK")
      (make-logical-pathname-translations "PATCHWORK"
                                          (make-pathname :name nil :type nil :version nil
                                                         :defaults #.(or *compile-file-truename* *load-truename*
                                                                         #P"~/works/patchwork/patchwork/"))))

(defun cd (path)
  #+ccl       (ccl::cd      path)
  #+lispworks (cl-user::cd  path)
  #+clisp     (ext:cd       path))

(cd (translate-logical-pathname #P"PATCHWORK:"))

(pushnew (translate-logical-pathname #P"PATCHWORK:SRC;")
         asdf:*central-registry* :test (function equalp))

(pushnew (translate-logical-pathname #P"PATCHWORK:SRC;MCLGUI;")
         asdf:*central-registry* :test (function equalp))



(defparameter *patchwork-version*
  #.(destructuring-bind (major minor compile)
        (sexp-file-contents (translate-logical-pathname #P"PATCHWORK:VERSION.DATA"))
      (decf compile 0.001)
      (setf (sexp-file-contents (translate-logical-pathname #P"PATCHWORK:VERSION.DATA"))
            (list major minor compile))
      (format nil "~A.~A-~,3F" major minor compile))
  "Patchwork version")


;;;; THE END ;;;;
