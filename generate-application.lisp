;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               generate-application.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
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
;;;;    Copyright Pascal J. Bourguignon 2012 - 2014
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

(defun say (fmt &rest args)
  (format t "~&;;; ~?~%" fmt args)
  (finish-output))

(defun local-file (name &optional type)
  (make-pathname :name name
                 :type type
                 :version nil
                 :defaults #.(or *compile-file-truename* *load-truename*)))

;;; --------------------------------------------------------------------
;;; Load quicklisp
(say "Loading quicklisp.")
(load #P"~/quicklisp/setup.lisp")
(setf quicklisp-client:*quickload-verbose* t)


;;; --------------------------------------------------------------------
;; Logical Hosts used at compilation time
;; --------------------------------------
;;
;;   PATCHWORK
;;
;;     The logical host PATCHWORK should be set so that the .git/
;;     subdirectory should be  at its root:
;;
;;         #+ccl (probe-file #P"PATCHWORK:.git;") --> true
;;
;;   MCLGUI
;;
;;   MIDI
;;
;; Logical Hosts used at run-time
;; ------------------------------
;;
;; Those logical hosts are used by patchwork or its dependencies include:
;;
;;   PW-USER  -- See src/application.lisp  initialize-directories
;;
;;   CLENI

(say "Defining logical hosts.")
(load (local-file "loghosts"))

;;; --------------------------------------------------------------------
;;; Configure quicklisp.
(say "Configure quicklisp.")
;; On ccl-1.6/MacOSX 10.5.8, quicklisp doesn't deal properly with symbolic links in local-projects.
(push (translate-logical-pathname #P"SRC:INFORMATIMAGO;") ql:*local-project-directories*)

;;; --------------------------------------------------------------------
;;; Load builder stuff.
(say "Load builder stuff from quicklisp.")
(ql:quickload :cffi                                       :verbose t :explain t)
(ql:quickload :com.informatimago.tools.pathname           :verbose t :explain t)
(ql:quickload :com.informatimago.tools.manifest           :verbose t :explain t)
(ql:quickload :com.informatimago.common-lisp.cesarum      :verbose t :explain t)
(ql:quickload :com.informatimago.common-lisp.interactive  :verbose t :explain t)

(say "Load builder.")
(load (local-file "builder"))

(in-package "PATCHWORK.BUILDER")
(load (translate-logical-pathname #P"PATCHWORK:PROPERTY-LIST-KEYS"))


;;; --------------------------------------------------------------------
;;; configure *features*
;; (pushnew 'patchwork.builder::no-cocoa           *features*)
;; (pushnew 'patchwork.builder::use-apple-events   *features*)
;; (pushnew 'patchwork.builder::cocoa-midi-player  *features*)
;; (pushnew 'patchwork.builder::use-midishare      *features*)
(pushnew 'patchwork.builder::use-cl-midi        *features*)
(pushnew :debug-streams                         *features*)

;;; --------------------------------------------------------------------
;;; Load cocoa
#+(and ccl (not patchwork.builder::no-cocoa))
(progn
  (say "Loading :cocoa (takes some time to start…)")
  (require :cocoa))
(defparameter *cocoa-readtable* (copy-readtable *readtable*))

(load (translate-logical-pathname #P"PATCHWORK:GESTALT"))
(add-cocoa-version-features)

;;; --------------------------------------------------------------------
;; #+(and ccl (not patchwork.builder::no-cocoa))
;; (load (translate-logical-pathname #P"PATCHWORK:SRC;MACOSX;CCL-1-9-PATCH.LISP"))

;;; --------------------------------------------------------------------
;;; AppleEvents are not used for now.
#+(and patchwork.builder::use-apple-events ccl darwin (not patchwork.builder::no-cocoa))
(progn
  (say "Loading MacOSX Libraries")
  (load (translate-logical-pathname #P"PATCHWORK:SRC;MACOSX;LOAD-LIBRARIES")))


;;; --------------------------------------------------------------------
;;; Loading dependencies
(ql:quickload :com.informatimago.common-lisp.lisp.stepper :verbose t :explain t) ; debug
(ql:quickload :com.informatimago.objcl                    :verbose t :explain t)
(ql:quickload :com.informatimago.clext                    :verbose t :explain t) ; closer-weak
(ql:quickload :trivial-gray-streams                       :verbose t :explain t)
(ql:quickload :mclgui                                     :verbose t :explain t)

;;; --------------------------------------------------------------------
;;; Loading patchwork
(ql:quickload :patchwork                                  :verbose t :explain t)
;; (defpackage "PATCHWORK" (:use "CL") (:nicknames "PW"))

(mapc (lambda (package) (unuse-package package "COMMON-LISP-USER"))
      (remove (find-package "COMMON-LISP") (package-use-list "COMMON-LISP-USER")))
(use-package '("MCLGUI" "PATCHWORK") "COMMON-LISP-USER")


;;; --------------------------------------------------------------------
;;; Program Parameters

(defparameter *program-name*                     "Patchwork")
(defparameter *program-system*                   :patchwork)
(defparameter *name-and-version*                 (format nil "~A-~A" *program-name* *patchwork-version*))
(defparameter *release-directory*                (merge-pathnames
                                                  (make-pathname :directory (list :relative "Desktop"
                                                                                  (executable-name *name-and-version*)))
                                                  (user-homedir-pathname)))
(defparameter *application-class-name*           'pw::patchwork-application)   ; mclgui:application subclass
(defparameter *application-delegate-class-name*  "MclguiApplicationDelegate")  ; IdeApplicationDelegate subclass
(defparameter *principal-class-name*             "CCLApplication")             ; NSApplication subclass.


;;; --------------------------------------------------------------------
;;; Save the manifest.

(ensure-directories-exist (merge-pathnames "TEST" *release-directory*))
(setf (logical-pathname-translations "RELEASE")
      (make-translations "RELEASE" '() *release-directory*))

(say "Generating manifest.")
(let ((*default-pathname-defaults* *release-directory*)
      (*print-circle* nil))
  (let ((manifest-pathname (write-manifest *name-and-version* *program-system*))
        (git-source-sandboxes '(#P"SRC:PATCHWORK;"
                                #P"SRC:MCLGUI;"
                                #P"SRC:MIDI;"
                                #P"SRC:MIDISHARE;"
                                #P"SRC:INFORMATIMAGO;"))
        ;; (svn-source-sandboxes '())
        (pwd (ccl:current-directory)))
    (with-open-file (manifest manifest-pathname :direction :output
                                                :if-exists :append
                                                :if-does-not-exist :create)
      (unwind-protect
           (dolist (git-source-log git-source-sandboxes)
             (let ((git-source (translate-logical-pathname git-source-log)))
               (format manifest "~2%~A~%~V@{-~}~2%~A~2%"
                       (namestring git-source-log) (length (namestring git-source-log))
                       (namestring git-source))
               (setf (ccl:current-directory) git-source)
               (format manifest "~A~%" (com.informatimago.tools.manifest::shell-command-to-string "git-info"))))
        (setf (ccl:current-directory) pwd)))))

(say "Copying release notes.")
(copy-file (translate-logical-pathname #P"PATCHWORK:RELEASE-NOTES.TXT")
           (translate-logical-pathname #P"RELEASE:RELEASE-NOTES.TXT"))

(say "Copying reference documentation.")
(copy-file (translate-logical-pathname #P"PATCHWORK:DOC/PW-REFERENCE.PDF")
           (translate-logical-pathname #P"RELEASE:PW-REFERENCE.PDF"))

(say "Copying tutorials.")
(copy-directory (translate-logical-pathname #P"PATCHWORK:TUTORIALS;")
                (translate-logical-pathname #P"RELEASE:TUTORIALS;")
                :if-exists :supersede
                :on-error :continue)

(say "Copying MidiShare.framework.")
(copy-directory #P"/Library/Frameworks/MidiShare.framework/"
                #P"RELEASE:MidiShare.framework;"
                :if-exists :supersede
                :on-error :continue)

;;; --------------------------------------------------------------------
;;; Save the application package.

;; Let's reset the readtable to the implementation defined one.
(setf *readtable* (copy-readtable *cocoa-readtable*))

#+(and ccl (not patchwork.builder::no-cocoa))
(require :build-application)


#+ccl
(dolist (lib ccl::*shared-libraries*)
  (say "Shared library: ~A" lib))

(say "Generating ~A" (merge-pathnames (make-pathname :name *program-name*
                                                     :type "app")
                                      *release-directory*))

(defun exported-type-utis ()
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


(defun cf-bundle-document-types ()
  (vector

   (dictionary
    :|CFBundleTypeExtensions| #("lisp")
    :|CFBundleTypeIconFile| "openmcl-icon.icns"
    :|CFBundleTypeName| "Lisp source code"
    :|CFBundleTypeRole| "Editor"
    :|LSIsAppleDefaultForType| t
    :|NSDocumentClass| "HemlockEditorDocument")

   (dictionary
    :|CFBundleTypeIconFile| "openmcl-icon.icns"
    :|CFBundleTypeName| "Listener"
    :|CFBundleTypeRole| "Editor"
    :|NSDocumentClass| "HemlockListenerDocument")

   (dictionary
    :|CFBundleTypeExtensions| #("txt" "text" "*")
    :|CFBundleTypeIconFile| "openmcl-icon.icns"
    :|CFBundleTypeName| "NSStringPboardType"
    :|CFBundleTypeOSTypes| #("****")
    :|CFBundleTypeRole| "Editor"
    :|NSDocumentClass| "HemlockEditorDocument")

   (dictionary
    :|CFBundleTypeName| "html"
    :|CFBundleTypeRole| "Editor"
    :|NSDocumentClass| "DisplayDocument")))


(defun save-patchwork-application (&key
                                     (name "Untitled Application")
                                     (directory #P"~/Desktop/")
                                     (application-class-name
                                      #+(and ccl      (not ccl-1.9))  'gui::cocoa-application
                                      #+(and ccl-1.9  (not ccl-1.10)) 'gui::lisp-application
                                      #+(and ccl-1.10 (not ccl-1.11)) 'ccl::lisp-development-system
                                      #+(and ccl-1.11)                'gui::cocoa-ide)
                                     (principal-class-name "NSApplication")
                                     (application-delegate-class-name nil)
                                     (main-nib-file nil)
                                     (nib-files '()))

  #+(and ccl (not patchwork.builder::no-cocoa))
  (when application-delegate-class-name
    (setf gui::*delegate-class-name* application-delegate-class-name))

  ;; ccl::build-application
  ;;  calls ccl::save-application
  ;;  calls ccl::%save-application-interal
  ;;  calls ccl::save-image
  #+(and ccl (not patchwork.builder::no-cocoa))
  (ccl::build-application ;; This doesn't return.
   :name name
   :directory directory
   :type-string "APPL"
   :creator-string "SOSP"
   :copy-ide-resources t         ; whether to copy the IDE's resources
   ;; :info-plist nil         ; optional user-defined info-plist
   :info-plist (ui::unwrap
                (dictionary
                 :|LSApplicationCategoryType| "public.app-category.music"
                 ;; (development-region $default-info-plist-development-region)
                 ;; (executable $default-info-plist-executable)
                 ;; (help-book-folder $default-info-plist-help-book-folder)
                 ;; (help-book-name $default-info-plist-help-book-name)
                 ;; (icon-file $default-info-plist-icon-file)
                 :|CFBundleIconFile| (file-namestring (translate-logical-pathname
                                                       #P"PATCHWORK:SRC;MACOSX;PATCHWORK-ICON.ICNS"))
                 :|CFBundleIdentifier| "com.informatimago.patchwork"
                 ;; (dictionary-version $default-info-dictionary-version)
                 ;; overriden by write-info-plist (bundle-name $default-info-plist-bundle-name)
                 ;; overriden by write-info-plist (bundle-package-type $default-info-plist-bundle-package-type)
                 ;; overriden by write-info-plist (bundle-signature $default-info-plist-bundle-signature)
                 :|CFBundleShortVersionString|  (format nil "~A" *patchwork-version*)
                 :|CFBundleVersion| (format nil "Patchwork ~A, ~A ~A" *patchwork-version*
                                            (lisp-implementation-type)
                                            (lisp-implementation-version))
                 ;; (has-localized-display-name $default-info-plist-has-localized-display-name)
                 ;; (minimum-system-version $default-info-plist-minimum-system-version)
                 ;; (main-nib-file $default-info-plist-main-nib-file)
                 ;; (principal-class $default-info-plist-principal-class)
                 :|CFBundleDocumentTypes| (cf-bundle-document-types)
                 :|NSAppleScriptEnabled| nil ; not yet.
                 :|LSMinimumSystemVersion| (if (featurep :cocoa-10.6) "10.6" "10.3")
                 :|CFBundleDevelopmentRegion| "English"
                 :|UTExportedTypeDeclarations| (exported-type-utis)
                 :|NSHumanReadableCopyright| (format nil "Copyright 1992 - 2012 IRCAM~%Copyright 2012 - 2017 Pascal Bourguignon~%License: GPL3")

                 ;; :|NSMainNibFile| main-nib-file ; overriden by write-info-plist, I assume.
                 :|NSPrincipalClass| principal-class-name))
   :nibfiles nib-files
                                        ; a list of user-specified nibfiles
                                        ; to be copied into the app bundle
   :main-nib-name main-nib-file
                                        ; the name of the nib that is to be loaded
                                        ; as the app's main. this name gets written
                                        ; into the Info.plist on the "NSMainNibFile" key
   :private-frameworks '()
   :application-class application-class-name
   :toplevel-function nil ; implies (gui::toplevel-function *application*)
   :altconsole nil))





(say "*release-directory*      = ~S" *release-directory*)
(say "MCLGUI application class = ~S" *application-class-name*)
(say "Cocoa application class  = ~S" *principal-class-name*)
(say "Cocoa delegate class     = ~S" *application-delegate-class-name*)
(say "save image and quit      = ~S" #+save-image-and-quit t #-save-image-and-quit nil)

(dolist (var
         '(mclgui.system:*lisp-cleanup-functions*
           mclgui.system:*save-exit-functions*
           mclgui.system:*restore-lisp-functions*
           mclgui.system:*lisp-user-pointer-functions*
           mclgui.system:*lisp-startup-functions*
           mclgui.system:*application-did-finish-launching-functions*
           mclgui.system:*application-should-terminate-functions*
           ccl:*application*
           ui:*application*))
  (say "~50A = ~S" var (symbol-value var)))


#+save-image-and-quit
(progn
  (let ((destination (merge-pathnames #P"Patchwork.app/Contents/Resources/patchwork-icon.icns"
                                      *release-directory*)))
    (ensure-directories-exist destination)
    (copy-file (translate-logical-pathname #P"PATCHWORK:SRC;MACOSX;PATCHWORK-ICON.ICNS")
               destination :element-type '(unsigned-byte 8) :if-exists :supersede))

  (progn ; print path of generated executable
    (format t  "~%~A~%" (merge-pathnames (make-pathname :directory (list :relative
                                                                         (format nil "~A.app" *program-name*)
                                                                         "Contents" "MacOS")
                                                        :name "Patchwork"
                                                        :type nil)
                                         *release-directory*))
    (finish-output))

  ;; this quits ccl:
  (save-patchwork-application :name *program-name*
                              :directory *release-directory*
                              :application-class-name *application-class-name*
                              :principal-class-name *principal-class-name*
                              :application-delegate-class-name *application-delegate-class-name*)

  #+lispworks
  (hcl:save-image-with-bundle #P"~/Desktop/PatchWork.app"
                              :console :always)

  ) ;;unless

;;;; THE END ;;;;
