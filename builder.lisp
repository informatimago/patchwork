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


;;;---------------------------------------------------------------------
;;;
;;;

(defgeneric dependencies (system)
  (:method ((system asdf:system)) (asdf:component-load-dependencies system))
  (:method ((system string))      (asdf:component-load-dependencies (asdf:find-system system)))
  (:method ((system symbol))      (asdf:component-load-dependencies (asdf:find-system system)))
  (:method ((system list))
    (destructuring-bind (op system version) system
      (declare (ignore version))
      (assert (eq op :version))
      (asdf:component-load-dependencies (asdf:find-system system)))))

(defgeneric name (system)
  (:method ((system asdf:system)) (asdf:component-name system))
  (:method ((system string)) system)
  (:method ((system symbol)) (name (asdf:find-system system)))
  (:method ((system list))
    (destructuring-bind (op system version) system
      (declare (ignore version))
      (assert (eq op :version))
      (name (asdf:find-system system)))))

(defgeneric lessp (a b)
  (:method ((a asdf:system) (b asdf:system))
    (find b (mapcar (function asdf:find-system) (dependencies a))))
  (:method ((a string) b) (lessp (asdf:find-system a) b))
  (:method ((a symbol) b) (lessp (asdf:find-system a) b))
  (:method (a (b string)) (lessp a (asdf:find-system b)))
  (:method (a (b symbol)) (lessp a (asdf:find-system b))))

(defun all-dependencies (system)
 (let ((system (asdf:find-system system)))
    (delete-duplicates
     (mapcar (function name)
             (com.informatimago.common-lisp.cesarum.utility:transitive-closure
              (function dependencies)
              (list system)
              :use 'hash-table))
     :test (function string=))))

;; (all-dependencies "patchwork-main")





#|


("com.informatimago.macosx.coremidi.midi" "patchwork-main" "com.informatimago.clext.redirecting-stream" "com.informatimago.common-lisp.lisp-sexp" "patchwork" "com.informatimago.clext.character-sets" "com.informatimago.clext.filter-stream" "cffi-grovel" "com.informatimago.macosx.coremidi" "com.informatimago.clext.association" "trivial-gray-streams" "cffi-libffi" "com.informatimago.clext" "babel" "com.informatimago.clext.pipe" "com.informatimago.clext.shell" "split-sequence" "uiop" "cffi-toolchain" "com.informatimago.clext.queue" "midi" "com.informatimago.clext.closer-weak" "com.informatimago.objcl" "mclgui" "bordeaux-threads" "com.informatimago.common-lisp.cesarum" "cffi" "closer-mop" "alexandria" "asdf" "trivial-features")


 P F

(let ((system (asdf:find-system "patchwork-main")))
  (asdf:component-load-dependencies system))
("mclgui" "patchwork")


(lspack :asdf t)

ASDF/INTERFACE
   Symbols:        231 exported, 1862 total.
   Nicknames:     ASDF ASDF-UTILITIES
   Uses:          ASDF/ACTION ASDF/BACKWARD-INTERFACE ASDF/BACKWARD-INTERNALS
                  ASDF/BUNDLE ASDF/COMPONENT ASDF/CONCATENATE-SOURCE
                  ASDF/FIND-COMPONENT ASDF/FIND-SYSTEM ASDF/FORCING
                  ASDF/LISP-ACTION ASDF/OPERATE ASDF/OPERATION
                  ASDF/OUTPUT-TRANSLATIONS ASDF/PACKAGE-INFERRED-SYSTEM
                  ASDF/PARSE-DEFSYSTEM ASDF/PLAN ASDF/SESSION
                  ASDF/SOURCE-REGISTRY ASDF/SYSTEM ASDF/SYSTEM-REGISTRY
                  ASDF/UPGRADE UIOP/COMMON-LISP UIOP/DRIVER
   Used by:       ASDF/USER CFFI-GROVEL CFFI-TOOLCHAIN CL-PPCRE-ASD
                  COM.INFORMATIMAGO.TOOLS.ASDF
                  COM.INFORMATIMAGO.TOOLS.QUICKLISP
   Exported:      *ASDF-VERBOSE* *CENTRAL-REGISTRY*
                  *COMPILE-FILE-FAILURE-BEHAVIOUR*
                  *COMPILE-FILE-WARNINGS-BEHAVIOUR* *DEFAULT-ENCODING*
                  *DEFAULT-SOURCE-REGISTRIES* *ENCODING-DETECTION-HOOK*
                  *ENCODING-EXTERNAL-FORMAT-HOOK*
                  *OUTPUT-TRANSLATIONS-PARAMETER* *RESOLVE-SYMLINKS*
                  *SOURCE-REGISTRY-PARAMETER*
                  *SYSTEM-DEFINITION-SEARCH-FUNCTIONS* *USER-CACHE*
                  *UTF-8-EXTERNAL-FORMAT* *VERBOSE-OUT* *WARNINGS-FILE-TYPE*
                  ACCEPT ACTION-DESCRIPTION ADDITIONAL-INPUT-FILES
                  ALREADY-LOADED-SYSTEMS APPLY-OUTPUT-TRANSLATIONS ASDF-MESSAGE
                  ASDF-VERSION BAD-SYSTEM-NAME BASIC-COMPILE-BUNDLE-OP BUILD-OP
                  BUNDLE-OP BUNDLE-SYSTEM C-SOURCE-FILE CHILD-COMPONENT
                  CIRCULAR-DEPENDENCY CL-SOURCE-FILE CL-SOURCE-FILE.CL
                  CL-SOURCE-FILE.LSP CLEAR-CONFIGURATION
                  CLEAR-CONFIGURATION-AND-RETRY CLEAR-OUTPUT-TRANSLATIONS
                  CLEAR-SOURCE-REGISTRY CLEAR-SYSTEM COERCE-ENTRY-TO-DIRECTORY
                  COERCE-NAME COMPILE-BUNDLE-OP COMPILE-CONCATENATED-SOURCE-OP
                  COMPILE-CONDITION COMPILE-ERROR COMPILE-FAILED
                  COMPILE-FAILED-ERROR COMPILE-FAILED-WARNING COMPILE-FILE*
                  COMPILE-FILE-ERROR COMPILE-FILE-PATHNAME* COMPILE-OP
                  COMPILE-SYSTEM COMPILE-WARNED COMPILE-WARNED-ERROR
                  COMPILE-WARNED-WARNING COMPILED-FILE COMPONENT
                  COMPONENT-CHILDREN COMPONENT-CHILDREN-BY-NAME
                  COMPONENT-DEPENDS-ON COMPONENT-ENCODING
                  COMPONENT-EXTERNAL-FORMAT COMPONENT-FIND-PATH
                  COMPONENT-LOAD-DEPENDENCIES COMPONENT-LOADED-P COMPONENT-NAME
                  COMPONENT-PARENT COMPONENT-PATHNAME COMPONENT-PROPERTY
                  COMPONENT-RELATIVE-PATHNAME COMPONENT-SIDEWAY-DEPENDENCIES
                  COMPONENT-SYSTEM COMPONENT-VERSION COMPUTE-SOURCE-REGISTRY
                  CONCATENATE-SOURCE-OP DEFINE-OP DEFSYSTEM DELIVER-ASD-OP
                  DISABLE-DEFERRED-WARNINGS-CHECK DISABLE-OUTPUT-TRANSLATIONS
                  DLL-OP DOC-FILE DOWNWARD-OPERATION DUPLICATE-NAMES
                  ENABLE-ASDF-BINARY-LOCATIONS-COMPATIBILITY
                  ENABLE-DEFERRED-WARNINGS-CHECK ENSURE-OUTPUT-TRANSLATIONS
                  ENSURE-SOURCE-REGISTRY ERROR-COMPONENT ERROR-NAME
                  ERROR-OPERATION ERROR-PATHNAME EXPLAIN FEATURE FILE-COMPONENT
                  FILE-TYPE FIND-COMPONENT FIND-OPERATION FIND-SYSTEM HOSTNAME
                  HTML-FILE IMAGE-OP IMPLEMENTATION-IDENTIFIER
                  IMPLEMENTATION-TYPE INITIALIZE-OUTPUT-TRANSLATIONS
                  INITIALIZE-SOURCE-REGISTRY INPUT-FILES JAVA-SOURCE-FILE
                  LIB-OP LOAD-ASD LOAD-BUNDLE-OP
                  LOAD-COMPILED-CONCATENATED-SOURCE-OP
                  LOAD-CONCATENATED-SOURCE-OP LOAD-OP LOAD-SOURCE-OP
                  LOAD-SYSTEM LOAD-SYSTEM-DEFINITION-ERROR LOAD-SYSTEMS
                  LOAD-SYSTEMS* LOCATE-SYSTEM MAKE MAKE-OPERATION MAKE-PLAN
                  MAP-SYSTEMS MISSING-COMPONENT MISSING-COMPONENT-OF-VERSION
                  MISSING-DEPENDENCY MISSING-DEPENDENCY-OF-VERSION MODULE
                  MODULE-COMPONENTS MONOLITHIC-BUNDLE-OP
                  MONOLITHIC-COMPILE-BUNDLE-OP
                  MONOLITHIC-COMPILE-CONCATENATED-SOURCE-OP
                  MONOLITHIC-CONCATENATE-SOURCE-OP MONOLITHIC-DELIVER-ASD-OP
                  MONOLITHIC-DLL-OP MONOLITHIC-LIB-OP MONOLITHIC-LOAD-BUNDLE-OP
                  MONOLITHIC-LOAD-COMPILED-CONCATENATED-SOURCE-OP
                  MONOLITHIC-LOAD-CONCATENATED-SOURCE-OP NEEDED-IN-IMAGE-P
                  NON-PROPAGATING-OPERATION NON-SYSTEM-SYSTEM
                  NON-TOPLEVEL-SYSTEM OOS OPERATE OPERATION
                  OPERATION-DEFINITION-ERROR OPERATION-DEFINITION-WARNING
                  OPERATION-DONE-P OPERATION-ERROR OPERATION-MONOLITHIC-P
                  OPERATION-ON-FAILURE OPERATION-ON-WARNINGS OUTPUT-FILE
                  OUTPUT-FILES PACKAGE-INFERRED-SYSTEM
                  PACKAGE-INFERRED-SYSTEM-MISSING-PACKAGE-ERROR PACKAGE-SYSTEM
                  PARENT-COMPONENT PERFORM PERFORM-PLAN PERFORM-WITH-RESTARTS
                  PRECOMPILED-SYSTEM PREPARE-BUNDLE-OP PREPARE-OP
                  PREPARE-SOURCE-OP PRIMARY-SYSTEM-NAME PROCESS-SOURCE-REGISTRY
                  PROGRAM-OP PROGRAM-SYSTEM REGISTER-IMMUTABLE-SYSTEM
                  REGISTER-PRELOADED-SYSTEM REGISTER-SYSTEM-PACKAGES
                  REGISTERED-SYSTEM REGISTERED-SYSTEMS
                  REMOVE-ENTRY-FROM-REGISTRY REQUIRE-SYSTEM REQUIRED-COMPONENTS
                  RESOLVE-LOCATION RETRY RUN-SHELL-COMMAND
                  SEARCH-FOR-SYSTEM-DEFINITION SELFWARD-OPERATION
                  SEQUENTIAL-PLAN SIDEWAY-OPERATION SOURCE-FILE
                  SOURCE-FILE-TYPE STATIC-FILE SYSDEF-IMMUTABLE-SYSTEM-SEARCH
                  SYSDEF-PRELOADED-SYSTEM-SEARCH SYSTEM SYSTEM-AUTHOR
                  SYSTEM-BUG-TRACKER SYSTEM-DEFINITION-ERROR
                  SYSTEM-DEFINITION-PATHNAME SYSTEM-DEFSYSTEM-DEPENDS-ON
                  SYSTEM-DEPENDS-ON SYSTEM-DESCRIPTION SYSTEM-HOMEPAGE
                  SYSTEM-LICENCE SYSTEM-LICENSE SYSTEM-LONG-DESCRIPTION
                  SYSTEM-LONG-NAME SYSTEM-MAILTO SYSTEM-MAINTAINER
                  SYSTEM-OUT-OF-DATE
                  SYSTEM-OUTPUT-TRANSLATIONS-DIRECTORY-PATHNAME
                  SYSTEM-OUTPUT-TRANSLATIONS-PATHNAME SYSTEM-REGISTERED-P
                  SYSTEM-RELATIVE-PATHNAME SYSTEM-SOURCE-CONTROL
                  SYSTEM-SOURCE-DIRECTORY SYSTEM-SOURCE-FILE
                  SYSTEM-SOURCE-REGISTRY SYSTEM-SOURCE-REGISTRY-DIRECTORY
                  SYSTEM-VERSION SYSTEM-WEAKLY-DEPENDS-ON TEST-OP TEST-SYSTEM
                  TRAVERSE TRY-RECOMPILING UPGRADE-ASDF UPWARD-OPERATION
                  USER-OUTPUT-TRANSLATIONS-DIRECTORY-PATHNAME
                  USER-OUTPUT-TRANSLATIONS-PATHNAME USER-SOURCE-REGISTRY
                  USER-SOURCE-REGISTRY-DIRECTORY VERSION VERSION-SATISFIES

|#
;;;; THE END ;;;;
