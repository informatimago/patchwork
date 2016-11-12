;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               initialize.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;  
;;;;    Initialize the objc-bridge.
;;;;  
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-06-02 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    LLGPL
;;;;  
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
;;;;  
;;;;    This library is licenced under the Lisp Lesser General Public
;;;;    License.
;;;;  
;;;;    This library is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later
;;;;    version.
;;;;  
;;;;    This library is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU Lesser General Public License for more
;;;;    details.
;;;;  
;;;;    You should have received a copy of the GNU Lesser General
;;;;    Public License along with this library; if not, write to the
;;;;    Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************

(in-package "OBJC-BRIDGE")


;;; Not a perfect mechanism.
(defclass objc-dispatch-function (closer-mop:funcallable-standard-object)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defmethod print-object ((o objc-dispatch-function) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (let* ((name (function-name o)))
      (when name
        (format stream "~s" name)))))





;;; Return the ObjC class named CNAME

(defun find-objc-class (cname)
  (%objc-class-classptr (if (symbolp cname) 
                            (find-class cname)
                            (load-objc-class-descriptor cname))))


#+ccl (ccl::def-standard-initial-binding *listener-autorelease-pool* nil)
(defvar lisp-objc-error-handler nil)

(defun initialize ()

  (register-objc-class-decls)
  (reset-objc-class-count)
  (maybe-map-objc-classes t)
  (register-objc-init-messages)
  (register-objc-set-messages)

  #+apple-objc-2.0 (setup-objc-exception-globals)
  #+ccl (pushnew 'recognize-objc-exception ccl::*foreign-error-condition-recognizers*)
  #+ccl (pushnew #'%clear-objc-class-maps ccl:*save-exit-functions*
                 :test #'eq :key #'ccl:function-name)
  #+ccl (pushnew #'revive-objc-classes ccl::*lisp-system-pointer-functions*
                 :test #'eq :key #'ccl:function-name)

  (setf *listener-autorelease-pool* (create-autorelease-pool))
  (setf *appkit-library-version-number*     (get-appkit-version)
        *foundation-library-version-number* (get-foundation-version)
        *extension-framework-paths*         '())

  ;; An instance of NSConstantString (which is a subclass of NSString)
  ;; consists of a pointer to the NSConstantString class (which the
  ;; global "_NSConstantStringClassReference" conveniently refers to), a
  ;; pointer to an array of 8-bit characters (doesn't have to be #\Nul
  ;; terminated, but doesn't hurt) and the length of that string (not
  ;; counting any #\Nul.)
  ;; The global reference to the "NSConstantString" class allows us to
  ;; make instances of NSConstantString, ala the @"foo" construct in
  ;; ObjC.  Sure it's ugly, but it seems to be exactly what the ObjC
  ;; compiler does.

  #+ccl
  (ccl:defloadvar *NSConstantString-class*
      (ccl:with-cstrs ((name "NSConstantString"))
        #+(or apple-objc cocotron-objc) (#_objc_lookUpClass name)
        #+gnu-objc                      (#_objc_lookup_class name)))

  #+(and ccl gnu-objc)
  (ccl:defloadvar *gnu-objc-runtime-mutex*
      (ccl:%get-ptr (ccl:foreign-symbol-address "__objc_runtime_mutex")))

  #+gnu-objc
  (ccl:defcallback lisp-objc-error-handler (:id receiver :int errcode (:* :char) format :address argptr :<BOOL>)
    (let* ((message (get-c-format-string format argptr)))
      (error "ObjC runtime error ~d, receiver ~s :~& ~a"
             errcode receiver message))
    *YES*)

  #+(and ccl gnu-objc)
  (ccl::def-ccl-pointers install-lisp-objc-error-handler ()
    (#_objc_set_error_handler lisp-objc-error-handler))

  (values))



(eval-when (:load-toplevel :execute)
  (initialize))

;;;; THE END ;;;;
