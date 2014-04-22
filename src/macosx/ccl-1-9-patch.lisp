;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               ccl-1-9-patch.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Patches to ccl.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-04-12 <PJB> Created.
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


(in-package :ccl)

#-(and)
(defun make-mcl-listener-process (procname
                                  input-stream
                                  output-stream
                                  cleanup-function
                                  &key
                                    (initial-function #'listener-function)
                                    (close-streams t)
                                    (class 'process)
                                    (control-stack-size *default-control-stack-size*)
                                    (auto-flush t)
                                    (value-stack-size *default-value-stack-size*)
                                    (temp-stack-size *default-temp-stack-size*)
                                    (echoing t)
                                    (process)
                                    (initargs nil))
  (let ((p (if (typep process class)
               (progn
                 (setf (process-thread process)
                       (new-thread procname control-stack-size value-stack-size  temp-stack-size))
                 process)
               (make-process procname
                             :class class :initargs initargs
                             :stack-size control-stack-size
                             :vstack-size value-stack-size
                             :tstack-size temp-stack-size))))
    (process-preset p #'(lambda ()
                          (let* ((*standard-input*  input-stream)
                                 (*standard-output* output-stream)
                                 (*error-output*    output-stream)
                                 (*trace-output*    output-stream)
                                 (*terminal-io*
                                   (if echoing
                                       (make-echoing-two-way-stream
                                        input-stream output-stream)
                                       (make-two-way-stream
                                        input-stream output-stream)))
                                 (*query-io* *terminal-io*)
                                 (*debug-io* *terminal-io*))
                            (unwind-protect
                                 (progn
                                   (when auto-flush
                                     (add-auto-flush-stream output-stream))
                                   (let* ((shared-input
                                            (input-stream-shared-resource
                                             input-stream)))
                                     (when shared-input
                                       (setf (shared-resource-primary-owner
                                              shared-input)
                                             *current-process*)))
                                   (application-ui-operation
                                    *application*
                                    :note-current-package *package*)
                                   (funcall initial-function))
                              (remove-auto-flush-stream output-stream)
                              (funcall cleanup-function)
                              (when close-streams
                                (close input-stream)
                                (close output-stream))))))
    (process-enable p)
    p))


;;; --------------------------------------------------------------------
(in-package :gui)

#-(and)
(defun new-cocoa-listener-process (procname window &key (class 'cocoa-listener-process)
                                                     (initial-function (repl-function-name *application*))
                                                     initargs)
  (declare (special *standalone-cocoa-ide*))
  (let* ((input-stream (make-instance 'cocoa-listener-input-stream))
         (output-stream (make-instance 'cocoa-listener-output-stream
                                       :hemlock-view (hemlock-view window))))
    (ccl::make-mcl-listener-process 
     procname
     input-stream
     output-stream
     ;; cleanup function
     #'(lambda ()
         (mapcar #'(lambda (buf)
                     (when (eq (buffer-process buf) *current-process*)
                       (let ((doc (hi::buffer-document buf)))
                         (when doc
                           (setf (hemlock-document-process doc) nil) ;; so #/close doesn't kill it.
                           (cocoa-close doc nil)))))
                 hi:*buffer-list*))
     :initial-function
     #'(lambda ()
         (setq ccl::*listener-autorelease-pool* (create-autorelease-pool))
         (when (and *standalone-cocoa-ide*
                    (prog1 *first-listener* (setq *first-listener* nil)))
           (ccl::startup-ccl (ccl::application-init-file ccl::*application*))
           (ui-object-note-package *nsapp* *package*))
         (funcall initial-function))
     :echoing nil
     :class class
     :initargs `(:listener-input-stream ,input-stream
                 :listener-output-stream ,output-stream
                 :listener-window ,window
                 ,@initargs))))




;;;; THE END ;;;;
