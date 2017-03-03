;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               application.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Implements the Patchwork application initialization.
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
(in-package :pw)



(defun make-patchwork-io ()
  #-cocoa
  *terminal-io*
  #+cocoa
  (make-two-way-stream
   (make-instance 'redirecting-stream:redirecting-character-input-stream
                  :input-stream-function
                  (let ((default-stream
                          (com.informatimago.common-lisp.cesarum.stream:stream-input-stream
                           *terminal-io*)))
                    (lambda ()
                      (or (hemlock-ext:top-listener-input-stream)
                          default-stream))))
   (make-instance 'redirecting-stream:redirecting-character-output-stream
                  :output-stream-function
                  (let ((default-stream
                          (com.informatimago.common-lisp.cesarum.stream:stream-output-stream
                           *terminal-io*)))
                    (lambda ()
                      (or (hemlock-ext:top-listener-output-stream)
                          default-stream))))))


(defvar *patchwork-io* (make-synonym-stream '*terminal-io*))
#+swank (defvar swank::*current-terminal-io*)

(defun initialize-streams ()
  (setf *patchwork-io* (make-patchwork-io))
  #+swank (setf swank::*current-terminal-io* *patchwork-io*)
  (let ((stream (make-synonym-stream '*terminal-io*)))
    (setf *terminal-io*       *patchwork-io*
          *standard-input*    stream
          *standard-output*   stream
          *error-output*      stream
          ;; *trace-output*      stream ;; TODO: redirect to stderr (NSLog) or a trace file in production.
          *query-io*          stream
          *debug-io*          stream
          *package*           (find-package "PATCHWORK"))))



#+(and ccl (not patchwork.builder::no-cocoa))
(defmethod  ccl:application-init-file (app)
  (declare (ignorable app))
  #P"PW-USER:PW-inits;init.lisp")


(defun logical-pathname-translations-directory ()
  (merge-pathnames #P"LOGHOSTS/" (user-homedir-pathname)))

(defun canonicalize-logical-host (host)
  (check-type host string)
  (assert (every (lambda (ch) (find ch "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-"))
                 host) (host) "Invalid logical host name ~S" host)
  (string-upcase host))

(defun really-load-logical-pathname-translations (host)
  (let ((host      (canonicalize-logical-host host)))
    (setf (logical-pathname-translations host)
          (with-open-file (trans
                           (make-pathname :name host :defaults (logical-pathname-translations-directory))
                           :direction :input
                           :if-does-not-exist :error)
            (read trans)))))

(defun initialize-directories ()
  (handler-case
      (really-load-logical-pathname-translations "PW-USER")
    (error ()
      (setf (logical-pathname-translations "PW-USER")
            `(("**;*.*.*" ,(merge-pathnames #P"Documents/Patchwork/**/*.*" (user-homedir-pathname)))
              ("**;*.*"   ,(merge-pathnames #P"Documents/Patchwork/**/*.*" (user-homedir-pathname)))
              ("**;*"     ,(merge-pathnames #P"Documents/Patchwork/**/*"   (user-homedir-pathname)))))))
  (dolist (path (list *PW-user-abstract-pathName* *PW-user-library-pathName*
                      *config-default-libr-path* *config-default-abst-path*
                      *config-init-file*))
    (ensure-directories-exist
     (make-pathname :name "TEST" :type "TEST" :version nil
                    :directory (remove-if (lambda (item) (member item '(:wild-inferiors :wild)))
                                          (pathname-directory path))
                    :defaults path))))


(defun initialize-patchwork ()
  "Initialize the Patchwork application.
Must be called on the main thread."
  ;; (ui::reporting-errors (ui:initialize))
  (ui::reporting-errors (setf (application-name *application*) "Patchwork"))
  (ui::reporting-errors (initialize-streams))
  (ui::reporting-errors (initialize-mn-editor))
  (ui::reporting-errors (initialize-menus))
  (ui::reporting-errors (initialize-beat-measure-line))
  (ui::reporting-errors (initialize-directories))
  ;;#-(and)(ui::reporting-errors (installapple-event-handlers)
  ;; ---
  (format *patchwork-io* "~2%Welcome to ~A Version ~A!~%~?"
          (application-name *application*)
          patchwork.builder:*patchwork-version*
          #+ccl ccl:*listener-prompt-format* #+ccl '(0)
          #-ccl "? "
          )
  (finish-output *patchwork-io*)
  (values))


(defun short-package-name (package)
  (first (sort (copy-list (cons (package-name package) (package-nicknames package)))
               (function <) :key (function length))))


(defclass patchwork-application (ui:lisp-development-system)
  ())

(defmethod ui::application-did-finish-launching :after ((application patchwork-application))
  (initialize-patchwork))

;;;; THE END ;;;;
