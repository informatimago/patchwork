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


(defun initialize-directories ()
  (handler-case
      (load-logical-pathname-translations "PW-USER")
    (error ()
      (setf (logical-pathname-translations "PW-USER")
            '(("**;*.*.*" #.(merge-pathnames #P"Desktop/pw-user/**/*.*" (user-homedir-pathname)))
              ("**;*.*"   #.(merge-pathnames #P"Desktop/pw-user/**/*.*" (user-homedir-pathname)))
              ("**;*"     #.(merge-pathnames #P"Desktop/pw-user/**/*"   (user-homedir-pathname)))))))
  (dolist (path (list *PW-user-abstract-pathName* *PW-user-library-pathName*
                      *config-default-libr-path* *config-default-abst-path*
                      *config-init-file*))
    (ensure-directories-exist
     (make-pathname :name "TEST" :type "TEST" :version nil
                    :directory (remove-if (lambda (item) (member item '(:wild-inferiors :wild)))
                                          (pathname-directory path))
                    :defaults path))))




(defun repl (&key
               ((:input-stream  *standard-input*)  *standard-input*)
               ((:output-stream *standard-output*) *standard-output*)
               (break-level #+ccl ccl::*break-level* #-ccl 0)
		       (prompt-function #+ccl (lambda (stream)
                                        (when (and ccl::*show-available-restarts* ccl::*break-condition*)
                                          (ccl::list-restarts)
                                          (setf ccl::*show-available-restarts* nil))
                                        (ccl::print-listener-prompt stream t))
                                #-ccl (lambda (stream)
                                        (declare (special *hist*))
                                        (format t "~%~A[~D~[~;/~:*~A~]]> " (package-name *package*) *hist* break-level))))
  "
DO:        Implements a minimalist CL REPL.
"
  (declare (special *hist*))
  (format *standard-output* "~&Patchwork REPL ~S~%" break-level)
  (catch 'repl
    (do ((+eof+ (gensym))
         (*hist* 1 (1+ *hist*)))
        (nil)
      (restart-case
          (progn
            (funcall prompt-function *standard-output*)
            (finish-output *standard-output*)
            (handler-bind ((error #'invoke-debugger))
              (setf - (read *standard-input* nil +eof+))
              (when (or (eq - +eof+)
                        (and (listp -)
                             (null (rest -))
                             (member (first -) '(quit  exit continue)
                                     :test (function string-equal))))
                (return-from repl))
              (let ((results (multiple-value-list (eval -))))
                (setf +++ ++   ++ +   + -
                      /// //   // /   / results
                      *** **   ** *   * (first /)))
              (format t "~& --> ~{~S~^ ;~%     ~}~%" /)
              (finish-output)))          
        (abort ()
          :report (lambda (stream)
                    (if (= break-level 0)
                        (format stream "Return to REPL toplevel.")
                        (format stream "Return to REPL break level ~D." break-level))))
        ;; (abort-break () 
        ;;   (unless (= break-level 0)
        ;;     (abort)))
        ))))


;; (defmethod ccl:repl-function-name ((self ui:application))
;;   'repl)


(defun initialize-patchwork ()
  "Initialize the Patchwork application.
Must be called on the main thread."
  (ui:initialize)
  (initialize-streams)
  (initialize-mn-editor)
  (initialize-menus)
  (initialize-beat-measure-line)
  (initialize-directories)
  #-(and)(installapple-event-handlers)
  ;; ---
  (terpri *patchwork-io*)
  (write-line "Welcome to Patchwork" *patchwork-io*)
  (values))


(defun date (&optional (date (get-universal-time)))
  (format nil "~{~5*~4,'0D-~2:*~2,'0D-~2:*~2,'0D ~2:*~2,'0D:~2:*~2,'0D:~2:*~2,'0D~%~8*~}"
          (multiple-value-list (decode-universal-time date))))

(defun safe-repl (&rest arguments &key &allow-other-keys)
  (loop
    (handler-bind ((error (function invoke-debugger)))
      (apply (function ccl::read-loop) arguments))))

;;; --------------------------------------------------------------------
;;; Initialization of patchwork
(on-restore patchwork-trace
  #+ccl (setf ccl::*read-loop-function* 'safe-repl)
  (setf *trace-output* (open (merge-pathnames #P"Desktop/patchwork-trace.txt"
                                              (user-homedir-pathname))
                             :direction :output
                             :if-does-not-exist :create
                             :if-exists :append
                             #+ccl :sharing #+ccl :lock))
  (format *trace-output* "~%~A~%" (date)))

(on-startup patchwork-initialization
  (eval-enqueue '(initialize-patchwork)))

;;;; THE END ;;;;
