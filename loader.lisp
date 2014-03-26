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

;; #+ccl-1.9
;; (handler-bind ((simple-error (lambda (c)
;;                                (princ c) (terpri)
;;                                (invoke-restart 'continue))))
;;   (load #P"~/works/patchwork/ccl-1.9-patch.lisp"))


(defpackage "PATCHWORK.LOADER"
  (:use "COMMON-LISP"))
(in-package "PATCHWORK.LOADER")

(declaim (optimize (safety 3) (debug 3) (space 0) (speed 0)))

(defun say (fmt &rest args)
  (format *trace-output* "~%;;; ~?~%" fmt args)
  (finish-output *trace-output*))

#+swank
(let ((stream (make-synonym-stream '*terminal-io*)))
  (setf swank::*current-standard-input*  stream
        swank::*current-standard-output* stream
        swank::*current-error-output*    stream
        ;; swank::*current-trace-output*    stream
        ;; swank::*current-terminal-io*     stream
        swank::*current-query-io*        stream
        swank::*current-debug-io*        stream))


;; (pushnew 'cl-user::no-cocoa *features*)
;; (pushnew 'cl-user::cocoa-midi-player *features*)

#+(and ccl (not cl-user::no-cocoa))
(progn
  (say "Loading :cocoa (takes some time to start…)")
  (require :cocoa))

(defparameter *cocoa-readtable* (copy-readtable *readtable*))


#+(and ccl-1.6 (not ccl-1.7)) (push #P"/Users/pjb/src/public/lisp/" ql:*local-project-directories*)

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
  (generate-logical-pathname-translation-file "PATCHWORK" #P"/home/pjb/works/patchwork/patchwork/"                  :force force)
  (generate-logical-pathname-translation-file "PW-USER"   #P"/home/pjb/works/patchwork/pw-user/"                    :force force)
  (generate-logical-pathname-translation-file "CLENI"     #P"/home/pjb/works/patchwork/patchwork/src/pw-lib/cleni/" :force force))
;; (install-patchwork :force t)

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

(load #+(or ccl allegro) #P"PATCHWORK:gestalt"
      #-(or ccl allegro) #P"PATCHWORK:GESTALT")

;; AppleEvents are not used for now.
#+(and use-apple-events ccl darwin (not cl-user::no-cocoa))
(load #P"PATCHWORK:src;macosx;load-libraries.lisp")

(ql:quickload :com.informatimago.common-lisp.cesarum      :verbose t :explain t)
(ql:quickload :com.informatimago.common-lisp.lisp.stepper :verbose t :explain t)
(ql:quickload :com.informatimago.objcl                    :verbose t :explain t)
(ql:quickload :com.informatimago.clext                    :verbose t :explain t)
(ql:quickload :mclgui                                     :verbose t :explain t)
(ui:initialize)

;;;
;;; Redirection of streams to the listener window.
;;;

(ql:quickload :trivial-gray-streams                       :verbose t :explain t)
(load #+(or ccl allegro) #P"PATCHWORK:src;stream;redirecting-stream"
      #-(or ccl allegro) #P"PATCHWORK:SRC;STREAM;REDIRECTING-STREAM")

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

(terpri *patchwork-io*)
(write-line "Welcome to Patchwork Hello" *patchwork-io*)

;;;
;;; Load patchwork.
;;;

(let ((*terminal-io* *patchwork-io*))
 (ui:on-main-thread/sync
   (ql:quickload :patchwork                                  :verbose t :explain t)))

(let ((*terminal-io* *patchwork-io*))
 (ui:on-main-thread/sync
   (mclgui:on-main-thread (patchwork::initialize-menus))))



;;;
;;;
;;;

#+(and ccl cocoa ignore)
(let* ((*standard-input*
         (make-instance 'redirecting-stream:redirecting-character-input-stream
                        :input-stream-function (function hemlock-ext:top-listener-input-stream)))
       (*standard-output*
         (make-instance 'redirecting-stream:redirecting-character-output-stream
                        :output-stream-function (function hemlock-ext:top-listener-output-stream)))
       (*error-output* *standard-output*)
       (*trace-output* *standard-output*)
       (*terminal-io*  (make-two-way-stream
                        *standard-input*
                        *standard-output*))
       (*query-io*     *terminal-io*)
       (*debug-io*     *terminal-io*))

  (setf *listener-io* *terminal-io*)
  #+swank
  (setf swank::*current-error-output*    *error-output*
        swank::*current-standard-output* *standard-output*
        swank::*current-trace-output*    *trace-output*

        swank::*current-standard-input*  *standard-input*

        swank::*current-terminal-io*     *terminal-io*
        swank::*current-debug-io*        *debug-io*
        swank::*current-query-io*        *query-io*))


#-(and)
(let ((*standard-input*  *patchwork-io*)
      (*standard-output* *patchwork-io*)
      (*error-output*    *patchwork-io*)
      (*trace-output*    *patchwork-io*)
      ;; (*terminal-io*     *patchwork-io*)
      (*query-io*        *patchwork-io*)
      (*debug-io*        *patchwork-io*))
  (ui:on-main-thread/sync
    (ql:quickload :patchwork                                  :verbose t :explain t)))

#-(and)
(let ((*standard-input*  *patchwork-io*)
      (*standard-output* *patchwork-io*)
      (*error-output*    *patchwork-io*)
      (*trace-output*    *patchwork-io*)
      ;; (*terminal-io*     *patchwork-io*)
      (*query-io*        *patchwork-io*)
      (*debug-io*        *patchwork-io*))
  (ui:on-main-thread/sync
    (mclgui:on-main-thread (patchwork::initialize-menus))))





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
;; (defparameter *pw-packages* '("PATCHWORK.LOADER" "MCLGUI" "LELISP-MACROS" "MIDI-PLAYER" "MIDISHARE"
;;                               "MIDI" "PATCHWORK.SCHEDULER" "PATCHWORK" "C-PATCH-BUFFER"
;;                               "C-PATCH-ACCUM" "C-PATCH-FILE-BUFFER" "C-PW-MIDI-IN"
;;                               "CLOS-APPLE-EVENT" "USER-SUPPLIED-IN-OUTS" "CLPF-UTIL" "PW-STYPE"
;;                               "EPW" "C-LIST-ITEM-H" "C-LIST-ITEM" "C-TABLE-WINDOW-H"
;;                               "C-TABLE-WINDOW" "C-PATCH-LIST-EDITOR" "C-GET-NOTE-SLOTS"
;;                               "C-GET-SELECTIONS" "USER-ABSTRACTION" "C-PATCH-CHORD-LINE"
;;                               "C-PW-SEND-MIDI-NOTE" "CLENI" "COMBINATORIAL-INTERV"
;;                               "C-PW-TEXT-INPUT" "C-PW-TEXT-BOX" "USER-COMP-ABSTR" "QUANTIZING"))

;; (map nil 'print (DUPLICATE-SYMBOLS :packages *pw-packages* :exported t))


;; (print (list swank::*current-standard-input*  
;;              swank::*current-standard-output* 
;;              swank::*current-error-output*    
;;              swank::*current-trace-output*    
;;              swank::*current-terminal-io*  
;;              swank::*current-query-io*        
;;              swank::*current-debug-io*        ))
;; (print (list *patchwork-io*))

(in-package :pw)
(eval-enqueue '(setf swank::*current-terminal-io* patchwork.loader::*patchwork-io*
                *package* (find-package "PATCHWORK")))

;; (eval-enqueue '(progn (setf *print-circle* nil
;;                        *print-right-margin* 80)
;;                 (pprint (list
;;                          'swank::*current-standard-input*   swank::*current-standard-input*  
;;                          'swank::*current-standard-output*  swank::*current-standard-output* 
;;                          'swank::*current-error-output*     swank::*current-error-output*    
;;                          'swank::*current-trace-output*     swank::*current-trace-output*    
;;                          'swank::*current-terminal-io*      swank::*current-terminal-io*  
;;                          'swank::*current-query-io*         swank::*current-query-io*        
;;                          'swank::*current-debug-io*         swank::*current-debug-io*)
;;                  patchwork.loader::*patchwork-io*)
;;                 (pprint (list
;;                          *standard-input*  
;;                          *standard-output* 
;;                          *error-output*    
;;                          *trace-output*    
;;                          *terminal-io*  
;;                          *query-io*        
;;                           *debug-io*)
;;                  patchwork.loader::*patchwork-io*)
;;                 (print patchwork.loader::*patchwork-io* patchwork.loader::*patchwork-io*)
;;                 (print *terminal-io* patchwork.loader::*patchwork-io*)))



;;;; THE END ;;;;
