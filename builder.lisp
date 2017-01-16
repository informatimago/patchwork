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
  (:use "PATCHWORK.LOGICAL-HOSTS")
  (:use "COM.INFORMATIMAGO.TOOLS.MANIFEST")
  (:use "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE")
  (:export "*PATCHWORK-VERSION*"
           "VERSION" "VERSION=" "VERSION<"
           "RT-VERSION=" "RT-VERSION<" "RT-VERSION<="
           "FEATUREP" "SYSTEM-VERSION" "ADD-COCOA-VERSION-FEATURES"
           "SAY"))
(in-package "PATCHWORK.BUILDER")
(declaim (optimize (safety 3) (debug 3) (space 0) (speed 0)))

#+ccl (setf ccl:*default-external-format*           :unix
            ccl:*default-file-character-encoding*   :utf-8
            ccl:*default-line-termination*          :unix
            ccl:*default-socket-character-encoding* :utf-8)

(setf *load-verbose* t
      *print-right-margin* 110)


(defun say (fmt &rest args)
  (format t "~&;;; ~?~%" fmt args)
  (finish-output))


(defun cd (path)
  #+ccl       (ccl::cd      path)
  #+lispworks (cl-user::cd  path)
  #+clisp     (ext:cd       path))

(cd (translate-logical-pathname #P"PATCHWORK:"))

(pushnew (translate-logical-pathname #P"PATCHWORK:SRC;")
         asdf:*central-registry* :test (function equalp))

(pushnew (translate-logical-pathname #P"MCLGUI:")
         asdf:*central-registry* :test (function equalp))

(pushnew (translate-logical-pathname #P"MIDI:")
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
