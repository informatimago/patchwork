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
;;;;    2012-04-12 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    Reserved
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
;;;;    
;;;;    All Rights Reserved.
;;;;    
;;;;    This program may not be included in any commercial product
;;;;    without the author written permission. It may be used freely
;;;;    for any non-commercial purpose, provided that this header is
;;;;    always included.
;;;;**************************************************************************

(cl:in-package :cl-user)
(setf ccl:*default-external-format*           :unix
      ccl:*default-file-character-encoding*   :utf-8
      ccl:*default-line-termination*          :unix
      ccl:*default-socket-character-encoding* :utf-8)

(load #P"~/quicklisp/setup.lisp")

(ccl::cd #P"/home/pjb/works/patchwork/patchwork/")
(pushnew #P"/home/pjb/works/patchwork//patchwork/src/"
         asdf:*central-registry* :test (function equalp))
(ql:quickload :patchwork)

(require :cocoa)
(require :build-application)

(defmethod  ccl:application-init-file :around (app)
  (declare (ignorable app))
  (make-pathname :name  "patchwork-init" :type "lisp"
                 :defaults (user-homedir-pathname)))

(ccl::build-application
 :name "PatchWork"
 :directory #P"~/Desktop/"
 :copy-ide-resources t
 ;; :init-file "HOME:patchwork-init.lisp"
 ;; '(pathname "~/application-init.lisp")
 ;;  (lambda ()
 ;;              (make-pathname :name  "patchwork-init" :type "lisp"
 ;;                             :defaults (user-homedir-pathname)))
 )



;;;; the END ;;;;
