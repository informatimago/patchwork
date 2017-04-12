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
                      (let ((hemlock-stream (hemlock-ext:top-listener-output-stream)))
                        (if (and hemlock-stream
                                 #+ccl (gui::dob-output-data (slot-value hemlock-stream 'gui::buffer)))
                            hemlock-stream
                            default-stream)))))))


(defvar *old-terminal-io* (make-synonym-stream '*terminal-io*))
(defvar *patchwork-io*    (make-synonym-stream '*terminal-io*))
#+swank (defvar swank::*current-terminal-io*)

(defun initialize-streams ()
  (setf *old-terminal-io* *terminal-io*)
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
  (format *error-output* "~&initialize-patchwork before~%")
  (format-trace 'initialize-patchwork *error-output*)
  (ui::reporting-errors (initialize-streams))
  (format *error-output* "~&initialize-patchwork after~%")
  (format-trace 'initialize-patchwork *error-output* *terminal-io*)
  (ui::reporting-errors (initialize-mn-editor))
  (ui::reporting-errors (initialize-menus))
  (ui::reporting-errors (setf (application-name *application*) "Patchwork"))
  (ui::reporting-errors (initialize-beat-measure-line))
  (ui::reporting-errors (initialize-directories))
  ;;#-(and)(ui::reporting-errors (installapple-event-handlers)
  ;; We cannot flush yet, since the listener window is not open yet.
  (values))


(defun short-package-name (package)
  (first (sort (copy-list (cons (package-name package) (package-nicknames package)))
               (function <) :key (function length))))


(defclass patchwork-application (cocoa-ide-application)
  ((%flags :initform 0)))

(defgeneric %did-show-welcome (application)
  (:method ((self patchwork-application)) (logbitp (slot-value self '%flags) 0)))
(defgeneric %initialized (application)
  (:method ((self patchwork-application)) (logbitp (slot-value self '%flags) 1)))
(defgeneric set-%did-show-welcome (application)
  (:method ((self patchwork-application)) (dpb (slot-value self '%flags) (byte 1 0) 1)))
(defgeneric set-%initialized (application)
  (:method ((self patchwork-application)) (dpb (slot-value self '%flags) (byte 1 1) 1)))


(defgeneric show-welcome (application)
  (:method ((application patchwork-application))
    (unless (%did-show-welcome application)
      (dolist (stream-var '(*terminal-io*
                            *standard-input*
                            *standard-output*
                            *error-output*
                            *trace-output*
                            *query-io*
                            *debug-io*))
        (format t "~&~40A = ~S~%" stream-var (symbol-value stream-var)))
      (format t "~% ~S~% ~S~%"
              (application-name *application*)
              (application-name application))
      (format t
              "~2%Welcome to ~A Version ~A!~%~?"
              (application-name application)
              patchwork.builder:*patchwork-version*
              #+ccl ccl:*listener-prompt-format* #+ccl '(0)
              #-ccl "? ")
      (finish-output)
      (set-%did-show-welcome application))
    (values)))


(defmethod application-init-file ((application patchwork-application))
  (directory #P"PW-USER:PW-inits;*.lisp"))


(defmethod application-will-finish-launching ((application patchwork-application))
  (format *error-output* "~S ~S :name ~:[nil~;~A~]  (eq self *application*) = ~A~%"
          'application-will-finish-launching
          *application*
          (and *application* (application-name *application*))
          (eq application *application*))
  (call-next-method))

(defmethod application-did-finish-launching :before ((application patchwork-application))
  (format-trace 'application-did-finish-launching  :before (application-name application) application)
  (values))

(defmethod application-did-finish-launching ((application patchwork-application))
  (format-trace 'application-did-finish-launching (eq application *application*))
  (call-next-method)
  (format-trace 'application-did-finish-launching "after call-next-method" :%initialized  (%initialized application))
  ;; (application-eval-enqueue application '(show-welcome *application*))
  (format-trace 'application-did-finish-launching  :after :before (application-name application) application)
  (initialize-patchwork)
  (set-%initialized application)
  (format-trace 'application-did-finish-launching  :after :after (application-name application) application)
  (show-welcome application)
  (values))

(defmethod application-did-finish-launching :after ((application patchwork-application))
  (format-trace 'application-did-finish-launching :after application (application-name application))
  (format-trace 'application-did-finish-launching :after *application*
                (and *application* (application-name *application*))
                '(eq application *application*) (eq application *application*))
  (values))

(defmethod application-will-become-active ((application patchwork-application))
  (format-trace 'application-will-become-active application)
  (call-next-method))

(defmethod application-will-become-active :after ((application patchwork-application))
  (format-trace 'application-will-become-active :after application))

(defmethod application-did-become-active ((application patchwork-application))
  (format-trace 'application-did-become-active application)
  (call-next-method))

(defmethod application-will-resign-active ((application patchwork-application))
  (format-trace 'application-will-resign-active application)
  (call-next-method))

(defmethod application-did-resign-active ((application patchwork-application))
  (format-trace 'application-did-resign-active application)
  (call-next-method))

;;;; THE END ;;;;
