;; -*- mode:lisp;coding:utf-8 -*-
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
;;; Redirect swank streams.
;;; This is for the case where swank:*globally-redirect-io* is true.
#+swank
(progn

  (defun new-swank (&optional (port 4010) (interface #+ccl (ccl::primary-ip-interface-address)
                                                     #-ccl "localhost"))
    (let ((swank::*loopback-interface* interface))
      (swank:create-server :port port)))

  (defvar swank::*current-standard-input*)
  (defvar swank::*current-standard-output*)
  (defvar swank::*current-error-output*)
  (defvar swank::*current-trace-output*)
  (defvar swank::*current-terminal-io*)
  (defvar swank::*current-query-io*)
  (defvar swank::*current-debug-io*))

#+swank
(let ((stream (make-synonym-stream '*terminal-io*)))
  (setf swank::*current-standard-input*  stream
        swank::*current-standard-output* stream
        swank::*current-error-output*    stream
        ;; swank::*current-trace-output*    stream
        ;; swank::*current-terminal-io*     stream
        swank::*current-query-io*        stream
        swank::*current-debug-io*        stream))

;;; --------------------------------------------------------------------
;;; remote debugging
#-(and)
(progn
  (ql:quickload :swank                                     :verbose t :explain t)
  (eval (read-from-string
         "(let ((swank::*loopback-interface* \"192.168.7.4\")) (swank:create-server))")))


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
;; On ccl-1.6/MacOSX 10.5.8, quicklisp doesn't deal properly with symbolic links in local-projects.
#+(and ccl-1.6 (not ccl-1.7)) (push #P"/Users/pjb/src/public/lisp/" ql:*local-project-directories*)
(say "Configure quicklisp.")
(push (translate-logical-pathname #P"SRC:INFORMATIMAGO;") ql:*local-project-directories*)


;;; --------------------------------------------------------------------
;;; Load builder stuff.
(ql:quickload :cffi                                       :verbose t :explain t)
(ql:quickload :com.informatimago.tools.pathname           :verbose t :explain t)
(ql:quickload :com.informatimago.tools.manifest           :verbose t :explain t)
(ql:quickload :com.informatimago.common-lisp.cesarum      :verbose t :explain t)
(load (merge-pathnames "builder.lisp" (or *load-pathname* #P"./")))
(in-package "PATCHWORK.BUILDER")


;;; --------------------------------------------------------------------
;;; Load patches
;; #+ccl-1.9
;; (handler-bind ((simple-error (lambda (c)
;;                                (princ c) (terpri)
;;                                (invoke-restart 'continue))))
;;   (load (translate-logical-pathname #P"PATCHWORK:SRC;MACOSX;CCL-1-9-PATCH")))


;;; --------------------------------------------------------------------
;;; configure *features*
(setf (uiop::getenv "CG_CONTEXT_SHOW_BACKTRACE") "YES")

(setf *features* (set-difference *features*
                                 '(:debug-application
                                   :debug-event
                                   :debug-focused-view
                                   :debug-graphics
                                   :debug-objc
                                   :debug-streams
                                   :debug-text
                                   :debug-trace
                                   :debug-views
                                   :debug-views-colors
                                   :debug-views-instance
                                   :debug-wrapper
                                   patchwork.builder::no-cocoa
                                   patchwork.builder::use-apple-events
                                   patchwork.builder::cocoa-midi-player
                                   patchwork.builder::use-midishare
                                   patchwork.builder::use-cl-midi)))

;; (pushnew :debug-application    *features*)
;; (pushnew :debug-event          *features*)
;; (pushnew :debug-focused-view   *features*)
;; (pushnew :debug-graphics       *features*)
;; (pushnew :debug-objc           *features*)
;; (pushnew :debug-streams        *features*)
;; (pushnew :debug-text           *features*)
;; (pushnew :debug-trace          *features*)
;; (pushnew :debug-views          *features*)
;; (pushnew :debug-views-colors   *features*)
;; (pushnew :debug-views-instance *features*)
;; (pushnew :debug-wrapper        *features*)

;; (pushnew 'patchwork.builder::no-cocoa          *features*)
;; (pushnew 'patchwork.builder::use-apple-events  *features*)
;; (pushnew 'patchwork.builder::cocoa-midi-player *features*)
;; (pushnew 'patchwork.builder::use-midishare     *features*)
   (pushnew 'patchwork.builder::use-cl-midi       *features*)


;;; --------------------------------------------------------------------
;;; Load cocoa
#+(and ccl (not patchwork.builder::no-cocoa))
(progn
  (say "Loading :cocoa (takes some time to start…)")
  (require :cocoa))




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
#+(and ccl cocoa) (load "src/macosx/headers.lisp")
#+(and ccl cocoa) (load "src/macosx/load-libraries.lisp")
(ql:quickload :patchwork                                  :verbose t :explain t)

;;; --------------------------------------------------------------------
;;; Initialization of patchwork


(defmacro with-streams (&body body)
  `(let ((*terminal-io*     *terminal-io*)
         (*standard-input*  *standard-input*)
         (*standard-output* *standard-output*)
         (*error-output*    *error-output*)
         (*trace-output*    *trace-output*)
         (*query-io*        *query-io*)
         (*debug-io*        *debug-io*))
     ,@body))


(defmacro show (&body expressions)
  "
DO:         Prints each expression and their values.
"
  (let ((width (reduce (function max)
                       (mapcar (lambda (expr) (length (format nil "~S" expr)))
                               expressions)
                       :initial-value 0)))
    `(progn
       ,@(mapcar
          (lambda (expr)
            `(let ((vals  (multiple-value-list ,expr)))
               (format *trace-output*
                 ,(format nil "~~~DS = ~~{~~S~~^ ; ~~%~:*~VA   ~~}~~%" width "")
                 (quote ,expr) vals)
               (values-list vals)))
          expressions))))

(defun print-streams ()
  (show
    *terminal-io*
    *standard-input*
    *standard-output*
    *error-output*
    *trace-output*
    *query-io*
    *debug-io*)
  (values))

#+swank (progn
          (defvar *slime-input*  *standard-input*)
          (defvar *slime-output* *standard-output*))

(defun start-patchwork ()
  (ui:on-main-thread/sync
    (handler-case
        (progn
          #+swank (setf *trace-output* *slime-output*)
          (unless (typep ui:*application* 'pw::patchwork-application)
            (ui:format-trace 'start-patchwork "change-class application")
            (change-class ui:*application* 'pw::patchwork-application))
          (ui:format-trace 'start-patchwork 'ui:initialize)
          (ui:initialize)
          (ui:format-trace 'start-patchwork 'pw::initialize-patchwork)
          (pw::initialize-patchwork ui:*application*)
          (pw::show-welcome ui:*application*))
      (error (err)
        (format *error-output* "~A~%" err)))))

(import 'start-patchwork "COMMON-LISP-USER")
(import 'with-streams    "COMMON-LISP-USER")

(in-package "COMMON-LISP-USER")
(let ((*print-circle* nil))
  (format t "~&;; Use ~S to start Patchwork in the main thread.~%~:*~S~%"
          '(start-patchwork))
  (format t  "~&~S~%"
          '(in-package :pw)))



;;; --------------------------------------------------------------------
;;; Done
;;; --------------------------------------------------------------------


;; (ql:quickload :com.informatimago.tools)
;; (use-package :com.informatimago.tools.symbol)
;; (use-package :com.informatimago.common-lisp.cesarum.package)
;;
;; (defun duplicate-symbols (&key (packages (list-all-packages)) (exported nil))
;;   "Return: a list of list of symbols that have the same name."
;;   (let ((symbols (make-hash-table :test (function equal))) ; maps names to list of unique symbols
;;         (duplicates '()))
;;     (dolist (p packages)
;;       (dolist (s (list-all-symbols p))
;;         (pushnew s (gethash (symbol-name s) symbols '()))))
;;     (maphash (lambda (name symbols)
;;                (declare (ignore name))
;;                (when (cdr symbols)
;;                  (push symbols duplicates)))
;;              symbols)
;;     (if exported
;;         (remove-if-not (lambda (symbols)
;;                          (some (lambda (symbol)
;;                                  (eq :external
;;                                      (nth-value 1 (find-symbol (symbol-name symbol)
;;                                                                (symbol-package symbol)))))
;;                                symbols))
;;                        duplicates)
;;         duplicates)))
;;
;; (defparameter *pw-packages* '("PATCHWORK.LOADER" "MCLGUI" "LELISP-MACROS"
;;                               "PATCHWORK.MIDI" "PATCHWORK.SCHEDULER" "PATCHWORK" "C-PATCH-BUFFER"
;;                               "C-PATCH-ACCUM" "C-PATCH-FILE-BUFFER" "C-PW-MIDI-IN"
;;                               "CLOS-APPLE-EVENT" "USER-SUPPLIED-IN-OUTS" "CLPF-UTIL" "PW-STYPE"
;;                               "EPW" "C-LIST-ITEM-H" "C-LIST-ITEM" "C-TABLE-WINDOW-H"
;;                               "C-TABLE-WINDOW" "C-PATCH-LIST-EDITOR" "C-GET-NOTE-SLOTS"
;;                               "C-GET-SELECTIONS" "USER-ABSTRACTION" "C-PATCH-CHORD-LINE"
;;                               "C-PW-SEND-MIDI-NOTE" "CLENI" "COMBINATORIAL-INTERV"
;;                               "C-PW-TEXT-INPUT" "C-PW-TEXT-BOX" "USER-COMP-ABSTR" "QUANTIZING"))



;;;; THE END ;;;;
