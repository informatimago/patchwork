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

;;; --------------------------------------------------------------------
;;; Load quicklisp
(format *trace-output* "~%;;; Loading quicklisp.~%")
(finish-output *trace-output*)
(load #P"~/quicklisp/setup.lisp")
(setf quicklisp-client:*quickload-verbose* t)

;;; --------------------------------------------------------------------
;;; Configure quicklisp.
;; On ccl-1.6/MacOSX 10.5.8, quicklisp doesn't deal properly with symbolic links in local-projects.
#+(and ccl-1.6 (not ccl-1.7)) (push #P"/Users/pjb/src/public/lisp/" ql:*local-project-directories*)
(push #P"/Users/pjb/src/public/lisp/" ql:*local-project-directories*)

;;; --------------------------------------------------------------------
;;; Load builder stuff.
(ql:quickload :cffi                                       :verbose t :explain t)
(ql:quickload :com.informatimago.tools.pathname           :verbose t :explain t)
(ql:quickload :com.informatimago.tools.manifest           :verbose t :explain t)
(ql:quickload :com.informatimago.common-lisp.cesarum      :verbose t :explain t)
(load (merge-pathnames "builder.lisp" (or *load-pathname* #P"./")))
(in-package "PATCHWORK.BUILDER")
(load (translate-logical-pathname #P"PATCHWORK:PROPERTY-LIST-KEYS"))


;;; --------------------------------------------------------------------
;;; configure *features*
;; (pushnew 'patchwork.builder::no-cocoa *features*)
;; (pushnew 'patchwork.builder::use-apple-events *features*)
(pushnew 'patchwork.builder::cocoa-midi-player *features*)


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


(mapc (lambda (package) (unuse-package package "COMMON-LISP-USER"))
      (remove (find-package "COMMON-LISP") (package-use-list "COMMON-LISP-USER")))
(use-package '("MCLGUI" "PATCHWORK") "COMMON-LISP-USER")

;;; --------------------------------------------------------------------
;;; Save the manifest.
(defparameter *program-name*      "Patchwork")
(defparameter *program-system*    :patchwork)
(defparameter *name-and-version*  (format nil "~A-~A" *program-name* *patchwork-version*))
(defparameter *release-directory* (merge-pathnames
                                   (make-pathname :directory (list :relative "Desktop"
                                                                   (executable-name *name-and-version*)))
                                   (user-homedir-pathname)))

(ensure-directories-exist (merge-pathnames "TEST" *release-directory*))
(setf (logical-pathname-translations "RELEASE") nil
      (logical-pathname-translations "RELEASE") 
      (make-logical-pathname-translations "RELEASE" *release-directory*))


(say "Generating manifest.")
(let ((*default-pathname-defaults* *release-directory*))
  (write-manifest *name-and-version* *program-system*))

(say "Copying release notes.")
(copy-file (translate-logical-pathname #P"PATCHWORK:RELEASE-NOTES.TXT")
           (translate-logical-pathname #P"RELEASE:RELEASE-NOTES.TXT"))


;;; --------------------------------------------------------------------
;;; Save the application package.

;; Let's reset the readtable to the implementation defined one.
(setf *readtable* (copy-readtable *cocoa-readtable*))

#+(and ccl (not patchwork.builder::no-cocoa))
(require :build-application)

#+(and ccl (not patchwork.builder::no-cocoa))
(defmethod  ccl:application-init-file :around (app)
  (declare (ignorable app))
  #-(and) (make-pathname :name  "patchwork-init" :type "lisp"
                         :defaults (user-homedir-pathname))
  #P"PW-USER:PW-inits;init.lisp")


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


(defun save-patchwork-application (&key (name "Patchwork") (directory #P"~/Desktop/"))
  ;; ccl::build-application
  ;;  calls ccl::save-application
  ;;  calls ccl::%save-application-interal
  ;;  calls ccl::save-image
  #+ (and ccl (not patchwork.builder::no-cocoa))
  (ccl::build-application ;; This doesn't return.
   :name name
   :directory directory
   :type-string "APPL"
   :creator-string "SOSP"
   :copy-ide-resources t   ; whether to copy the IDE's resources
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
                 :|NSHumanReadableCopyright| (format nil "Copyright 1992 - 2012 IRCAM~%Copyright 2012 - 2014 Pascal Bourguignon~%License: GPL3")
                 ;; overriden by write-info-plist, I assume. :|NSMainNibFile| "MainMenu"
                 :|NSPrincipalClass| "LispApplication"))
   :nibfiles '()
                                        ; a list of user-specified nibfiles
                                        ; to be copied into the app bundle
   :main-nib-name "MainMenu"
                                        ; the name of the nib that is to be loaded
                                        ; as the app's main. this name gets written
                                        ; into the Info.plist on the "NSMainNibFile" key
   ;; :application-class #-ccl-1.9 'gui::cocoa-application #+ccl-1.9 'gui::lisp-application
   :private-frameworks '()
   :toplevel-function nil
   :altconsole nil))

(unless *load-pathname*
  (let ((destination (merge-pathnames #P"Patchwork.app/Contents/Resources/patchwork-icon.icns"
                                      *release-directory*)))
    (ensure-directories-exist destination)
    (copy-file (translate-logical-pathname #P"PATCHWORK:SRC;MACOSX;PATCHWORK-ICON.ICNS")
               destination :element-type '(unsigned-byte 8) :if-exists :supersede))
  ;; this quits ccl:
  (save-patchwork-application :name *program-name* :directory *release-directory*))


#+lispworks
(hcl:save-image-with-bundle #P"~/Desktop/PatchWork.app"
                            :console :always)


;;;; THE END ;;;;
