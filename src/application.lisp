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

(defun shortest-package-nickname (package)
  "Return the shortest nickname of PACKAGE."
  (loop :for name :in (cons (package-name package) (package-nicknames package))
        :for shortest := name :then (if (< (length name) (length shortest))
                                        name
                                        shortest)
        :finally (return shortest)))

(defun fmt-package (stream arg colon at &rest parameters)
  (declare (ignore arg colon at parameters))
  (write-string (shortest-package-nickname *package*) stream))

(defun initialize-patchwork (application)
  "Initialize the Patchwork application.
Must be called on the main thread."
  (unless (%initialized application)
    (setf *package*       (find-package "PATCHWORK")
          #+ccl ccl::*listener-prompt-format* #+ccl "~[?~:;~:*~/pw::fmt-package/ ~:*~d >~] ")
   (ui::reporting-errors (initialize-mn-editor))
   (ui::reporting-errors (initialize-menus))
   (ui::reporting-errors (setf (application-name *application*) "Patchwork"))
   (ui::reporting-errors (initialize-beat-measure-line))
   (ui::reporting-errors (initialize-directories))
   ;;#-(and)(ui::reporting-errors (installapple-event-handlers)
   (set-%initialized application))
  (values))

(defgeneric show-welcome (application)
  (:method ((application patchwork-application))
    (unless (%did-show-welcome application)

      #+debug-streams
      (progn
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
                (application-name application)))

      (format t "~2%Welcome to ~A Version ~A!~%~?"
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
  (initialize-patchwork application)
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
