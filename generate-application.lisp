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

(declaim (optimize (safety 3) (debug 3) (space 0) (speed 0)))

(defparameter *patchwork-version* "10.0-0.998")

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
(load-logical-pathname-translations "PW-USER")
(load-logical-pathname-translations "CLENI")

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
(pushnew 'cocoa-midi-player *features*)

#+(and ccl (not cl-user::no-cocoa))
(progn
  (say "Loading :cocoa (takes some time to start…)")
  (require :cocoa))

(load #+(or ccl allegro) #P"PATCHWORK:gestalt"
      #-(or ccl allegro) #P"PATCHWORK:GESTALT")

#+(and ccl (not cl-user::no-cocoa))
(defparameter *cocoa-readtable* (copy-readtable *readtable*))

;; AppleEvents are not used for now.
#+(and use-apple-events ccl darwin (not cl-user::no-cocoa))
(progn
  (say "Loading MacOSX Libraries")
  (load #P"PATCHWORK:src;macosx;load-libraries.lisp"))


(ql:quickload :com.informatimago.common-lisp.cesarum)
(ql:quickload :com.informatimago.common-lisp.lisp.stepper) ;; DEBUG ;;
(ql:quickload :com.informatimago.objcl)
(ql:quickload :com.informatimago.clext) ; closer-weak
(ql:quickload :mclgui)
(ui:initialize)

(ql:quickload :trivial-gray-streams)
(load #+(or ccl allegro) #P"PATCHWORK:src;stream;redirecting-stream"
      #-(or ccl allegro) #P"PATCHWORK:SRC;STREAM;REDIRECTING-STREAM")

(ui:on-main-thread/sync
  (ql:quickload :patchwork))

(ui:on-main-thread/sync
  (mclgui:on-main-thread (patchwork::initialize-menus)))

;;;
;;;------------------------------------------------------------

(in-package :pw)

(defparameter *patchwork-io*
  (make-two-way-stream
   (make-instance 'redirecting-stream:redirecting-character-input-stream
                  :input-stream-function
                  (let ((default-stream
                          (com.informatimago.common-lisp.cesarum.stream:stream-input-stream *terminal-io*)))
                    (lambda ()
                      (or (hemlock-ext:top-listener-input-stream)
                          default-stream))))
   (make-instance 'redirecting-stream:redirecting-character-output-stream
                  :output-stream-function
                  (let ((default-stream
                          (com.informatimago.common-lisp.cesarum.stream:stream-output-stream *terminal-io*)))
                    (lambda ()
                      (or (hemlock-ext:top-listener-output-stream)
                          default-stream))))))

;; (on-restore pw-initialization
;;   (eval-enqueue '(let ((stream (make-synonym-stream '*terminal-io*)))
;;                   (setf
;;                    *terminal-io*       *patchwork-io*
;;                    *standard-input*    stream
;;                    *standard-output*   stream
;;                    *error-output*      stream
;;                    ;; *trace-output*      stream
;;                    *query-io*          stream
;;                    *debug-io*          stream
;;                    *package* (find-package "PATCHWORK")))))

(in-package :cl-user)

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



(objcl:enable-objcl-reader-macros)

(defun dictionary (&rest keys-and-values)
  (assert (evenp (length keys-and-values)))
  (let ((table (make-hash-table :test (function equal))))
    (loop
      :for (key value) :on keys-and-values :by (function cddr)
      :do (setf (gethash key table) value))
    table))



(defparameter *exported-type-utis* ; :|UTExportedTypeDeclarations|
  (vector
   (dictionary
    :|UTTypeIdentifier| "com.informatimago.patckwork.macos.patchwork-file"
    :|UTTypeDescription| "MacOS Patchwork file"
    ;; :|UTTypeIconFile| "public.text.icns"
    ;; :|UTTypeReferenceURL| ""
    :|UTTypeConformsTo| #("org.lisp.lisp-source")
    :|UTTypeTagSpecification| (dictionary
                               :|com.apple.ostype| "PTCH" ;; ???
                               :|public.filename-extension| #("pwpatch9")))
   (dictionary
    :|UTTypeIdentifier| "com.informatimago.patckwork.macosx.patchwork-file"
    :|UTTypeDescription| "MacOSX Patchwork file"
    ;; :|UTTypeIconFile| "public.text.icns"
    ;; :|UTTypeReferenceURL| ""
    ;; :|UTTypeVersion| "1.0"
    :|UTTypeConformsTo| #("org.lisp.lisp-source")
    :|UTTypeTagSpecification| (dictionary
                               ;; :|public.mime-type| "text/lisp"
                               :|public.filename-extension| #("pwpatch")))))



#+(and ccl (not cl-user::no-cocoa))
(ccl::build-application
 :name "Patchwork"
 :type-string "APPL"
 :creator-string "SOSP"
 :directory #P"~/Desktop/"
 :copy-ide-resources t   ; whether to copy the IDE's resources
 ;; :info-plist nil         ; optional user-defined info-plist
 :info-plist (ccl::make-info-dict
              ;; (development-region $default-info-plist-development-region)
              ;; (executable $default-info-plist-executable)
              :getinfo-string (format nil "\"~A Copyright © 2014\"" *patchwork-version*)
              ;; (help-book-folder $default-info-plist-help-book-folder)
              ;; (help-book-name $default-info-plist-help-book-name)
              ;; (icon-file $default-info-plist-icon-file)
              :bundle-identifier "com.informatimago.patchwork"
              ;; (dictionary-version $default-info-dictionary-version)
              ;; overriden by write-info-plist (bundle-name $default-info-plist-bundle-name)
              ;; overriden by write-info-plist (bundle-package-type $default-info-plist-bundle-package-type)
              ;; overriden by write-info-plist (bundle-signature $default-info-plist-bundle-signature)
              :short-version-string  (format nil "\"~A\"" *patchwork-version*)
              :version (format nil "\"~A\"" *patchwork-version*)
              ;; (has-localized-display-name $default-info-plist-has-localized-display-name)
              ;; (minimum-system-version $default-info-plist-minimum-system-version)
              ;; (main-nib-file $default-info-plist-main-nib-file)
              ;; (principal-class $default-info-plist-principal-class)
              )
 :nibfiles '()           ; a list of user-specified nibfiles
                                        ; to be copied into the app bundle
 :main-nib-name nil     ; the name of the nib that is to be loaded
                                        ; as the app's main. this name gets written
                                        ; into the Info.plist on the "NSMainNibFile" key
 ;; :application-class #-ccl-1.9 'gui::cocoa-application #+ccl-1.9 'gui::lisp-application
 :private-frameworks '()
 :toplevel-function nil
 :altconsole t)





;;; ccl::build-application --> ccl::save-application --> ccl::%save-application-interal --> ccl::save-image
;;
;; #+(and ccl  cl-user::no-cocoa)
;; (progn (princ "ccl:save-application will exit.") (terpri) (finish-output))
;; 
;; #+(and ccl  cl-user::no-cocoa)
;; (ccl::save-application 
;;  #P"~/Desktop/PatchWork"
;;  :init-file "HOME:patchwork-init.lisp"
;;  ;; :native t
;;  :prepend-kernel t
;;  ;; '(pathname "~/patchwork-init.lisp")
;;  ;;  (lambda ()
;;  ;;              (make-pathname :name  "patchwork-init" :type "lisp"
;;  ;;                             :defaults (user-homedir-pathname)))
;;  )




#+lispworks
(hcl:save-image-with-bundle #P"~/Desktop/PatchWork.app"
                            :console :always)


;;;; the END ;;;;
