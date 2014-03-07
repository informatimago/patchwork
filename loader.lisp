;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               loader.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This file contains the expressions used to load the patchwork
;;;;    program during development.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-06-24 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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

(defpackage "PATCHWORK.LOADER"
  (:use "COMMON-LISP"))
(in-package "PATCHWORK.LOADER")

;; (pushnew 'cl-user::no-cocoa *features*)
(pushnew 'cl-user::cocoa-midi-player *features*)

#-cl-user::no-cocoa (require :cocoa)


#||

(progn (ql:quickload :swank) (eval (read-from-string
 "(let ((swank::*loopback-interface* \"192.168.7.4\")) (swank:create-server))")))

(ql:quickload :swank)
(let ((swank::*loopback-interface* "192.168.7.4")) (swank:create-server))

||#

#+ccl (setf ccl:*default-external-format*           :unix
            ccl:*default-file-character-encoding*   :utf-8
            ccl:*default-line-termination*          :unix
            ccl:*default-socket-character-encoding* :utf-8)

(setf *print-right-margin* 110)

;; The logical host PATCHWORK should be set so that the .git/ subdirectory
;; should be  at its root:
;; #+ccl (probe-file #P"PATCHWORK:.git;") --> true

;; We use load-logical-pathname-translations to load the logical host PATCHWORK.
;; You must configure it for each implementation.

;; Note: we only only use the PATCHWORK logical host in this file, the
;;       rest of the sources are loaded with ql:quickload/asdf.


;; Other logical hosts used by patchwork or its dependencies include:
;;   PW-USER
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
          (format trans "~S~%" (list
                                   (list (format nil "~A:**;*.*.*"   logical-host)
                                         (merge-pathnames #P"**/*.*" base-pathname nil))
                                   (list (format nil "~A:**;*.*"     logical-host)
                                         (merge-pathnames #P"**/*.*" base-pathname nil))
                                   (list (format nil "~A:**;*"       logical-host)
                                         (merge-pathnames #P"**/*"   base-pathname nil)))))
        (cerror "~A already exists; aborting." translation-file)))
    translation-file))

(defun install-patchwork (&key (force nil))
  (generate-logical-pathname-translation-file "PATCHWORK" #P"/home/pjb/works/patchwork/"                            :force force)
  (generate-logical-pathname-translation-file "PW-USER"   #P"/home/pjb/works/patchwork/pw-user/"                    :force force)
  (generate-logical-pathname-translation-file "CLENI"     #P"/home/pjb/works/patchwork/patchwork/src/pw-lib/cleni/" :force force))

(load-logical-pathname-translations "PATCHWORK")
(load-logical-pathname-translations "PW-USER")
(load-logical-pathname-translations "CLENI")


#+ccl       (ccl::cd (truename #P"PATCHWORK:"))
#+lispworks (cd      (truename #P"PATCHWORK:"))
#+clisp     (ext:cd  (truename #P"PATCHWORK:"))

(pushnew #+(or ccl allegro) (truename #P"PATCHWORK:src;")
         #-(or ccl allegro) (truename #P"PATCHWORK:SRC;")
         asdf:*central-registry* :test (function equalp))

(pushnew #+(or ccl allegro) (truename #P"PATCHWORK:src;mclgui;")
         #-(or ccl allegro) (truename #P"PATCHWORK:SRC;MCLGUI;")
         asdf:*central-registry* :test (function equalp))


;; AppleEvents are not used for now.
#+(and use-apple-events ccl darwin (not cl-user::no-cocoa))
(load #P"PATCHWORK:src;macosx;load-libraries.lisp")

(ql:quickload :com.informatimago.common-lisp.lisp.stepper :verbose t :explain t)
(ql:quickload :com.informatimago.objcl                    :verbose t :explain t)
(ql:quickload :com.informatimago.clext                    :verbose t :explain t)
(ql:quickload :mclgui                                     :verbose t :explain t)
(mclgui:initialize)
(ql:quickload :patchwork                                  :verbose t :explain t)


;;;; THE END ;;;;
