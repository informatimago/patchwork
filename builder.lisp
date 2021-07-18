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
(in-package "COMMON-LISP-USER")

(setf *readtable*
      (copy-readtable
       #+#.(cl:if (cl:find-package "COM.INFORMATIMAGO.PJB.UTILITY")
                  '(:and) '(:or))
       com.informatimago.pjb.utility:*original-readtable*
       #-#.(cl:if (cl:find-package "COM.INFORMATIMAGO.PJB.UTILITY")
                  '(:and) '(:or))
       nil))

#+ccl (setf ccl:*default-external-format*           :utf-8
            ccl:*default-file-character-encoding*   :utf-8
            ccl:*default-socket-character-encoding* :utf-8
            ccl:*default-line-termination*          :unix)

(declaim (optimize (safety 3) (debug 3) (space 0) (speed 0)))

(defvar *verbose* nil)

(setf *LOAD-VERBOSE*    *verbose*
      *COMPILE-VERBOSE* *verbose*
      *LOAD-PRINT*      *verbose*
      *COMPILE-PRINT*   *verbose*)


(pushnew :patchwork-use-cl-midi *features*)


(load "src/loghosts.lisp")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadowing-import
   '(patchwork.logical-hosts:*logical-hosts*
     patchwork.logical-hosts:make-pathname
     patchwork.logical-hosts:user-homedir-pathname
     patchwork.logical-hosts:make-translations
     patchwork.logical-hosts:translate-logical-pathname
     patchwork.logical-hosts:set-logical-pathname-translations
     patchwork.logical-hosts:define-runtime-loghosts)))

(load "ct-loghosts.lisp")


(uiop:chdir (translate-logical-pathname #P"PATCHWORK:"))

#+quicklisp
(pushnew (translate-logical-pathname #P"PATCHWORK:SRC;")
         quicklisp:*local-project-directories*
         :test (function equalp))

#+quicklisp
(pushnew (translate-logical-pathname #P"SRC:INFORMATIMAGO;")
         quicklisp:*local-project-directories*
         :test (function equalp))

(pushnew (translate-logical-pathname #P"PATCHWORK:SRC;")
         asdf:*central-registry*
         :test (function equalp))

(pushnew (translate-logical-pathname #P"MCLGUI:")
         asdf:*central-registry*
         :test (function equalp))

(pushnew (translate-logical-pathname #P"MIDI:")
         asdf:*central-registry*
         :test (function equalp))

(pushnew (translate-logical-pathname #P"COREMIDI:")
         asdf:*central-registry*
         :test (function equalp))

(pushnew (translate-logical-pathname #P"SRC:INFORMATIMAGO;")
         asdf:*central-registry*
         :test (function equalp))


(defparameter *patchwork-version*
  (destructuring-bind (major minor compile)
      (with-open-file (version "version.data") (read version))
    (format nil "~A.~A-~,3F" major minor compile))
  "Patchwork version")

(defun increment-patchwork-version ()
  (destructuring-bind (major minor compile)
      (with-open-file (version "version.data") (read version))
    (decf compile 0.001d0)
    (with-open-file (version "version.data" :direction :output :if-exists :supersede)
      (prin1 (list major minor compile) version))
    (setf *patchwork-version* (format nil "~A.~A-~,3F" major minor compile))))

(defun build-patchwork ()
  (increment-patchwork-version)
  (asdf:oos 'asdf:monolithic-compile-bundle-op "patchwork-main")
  (let* ((fasl   (asdf:output-file (asdf:make-operation 'asdf:monolithic-compile-bundle-op) (asdf:find-system "patchwork-main")))
         (target (translate-logical-pathname (merge-pathnames  #P"PATCHWORK:PATCHWORK" fasl))))
    (ignore-errors (delete-file target))
    (rename-file fasl target)))

;;;; THE END ;;;;
