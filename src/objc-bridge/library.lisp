;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               library.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;  
;;;;    Load an objc library.
;;;;  
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-06-04 <PJB> Created.
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



(defun getenv (var)
  #+ccl           (ccl::getenv var)
  #+clisp         (ext:getenv var)
  #+CMU           (cdr (assoc var ext:*environment-list* :test #'string=))
  #+ecl           (ext:getenv var)
  #+SBCL          (sb-ext:posix-getenv var)
  #+Allegro       (sys:getenv var)
  #+Lispworks     (lispworks:environment-variable var)
  #-(or ccl
        clisp
        cmu
        ecl
        sbcl
        allegro
        lispworks) (error "Please implement getenv for ~A" (lisp-implementation-type)))






;;; Open shared libs.
#+(or darwin-target cocotron-objc)
(progn
  #+ccl
  (ccl:defloadvar *cocoa-event-process* *initial-process*)


  (defun current-ns-thread ()
    (ccl:with-cstrs ((class-name "NSThread")
                 (message-selector-name "currentThread"))
      (let* ((nsthread-class (#_objc_lookUpClass class-name))
             (message-selector (#_sel_getUid message-selector-name)))
        (#_objc_msgSend nsthread-class message-selector)
        nil)))


  (defun create-void-nsthread ()
    ;; Create an NSThread which does nothing but exit.
    ;; This'll help to convince the AppKit that we're
    ;; multitheaded.  (A lot of other things, including
    ;; the ObjC runtime, seem to have already noticed.)
    (ccl:with-cstrs ((thread-class-name "NSThread")
                 (pool-class-name "NSAutoreleasePool")
                 (thread-message-selector-name "detachNewThreadSelector:toTarget:withObject:")
                 (exit-selector-name "class")
                 (alloc-selector-name "alloc")
                 (init-selector-name "init")
                 (release-selector-name "release"))
      (let* ((nsthread-class (#_objc_lookUpClass thread-class-name))
             (pool-class (#_objc_lookUpClass pool-class-name))
             (thread-message-selector (#_sel_getUid thread-message-selector-name))
             (exit-selector (#_sel_getUid exit-selector-name))
             (alloc-selector (#_sel_getUid alloc-selector-name))
             (init-selector (#_sel_getUid init-selector-name))
             (release-selector (#_sel_getUid release-selector-name))
             (pool (#_objc_msgSend
                    (#_objc_msgSend pool-class
                                    alloc-selector)
                    init-selector)))
        (unwind-protect
             (#_objc_msgSend nsthread-class thread-message-selector
                             :address exit-selector
                             :address nsthread-class
                             :address (ccl:%null-ptr))
          (#_objc_msgSend pool release-selector))
        nil)))


  (defun load-cocoa-framework ()
    #-ccl (niy load-cocoa-framework)
    #+ccl
    (call-in-initial-process
     (lambda ()
         ;; We need to load and "initialize" the CoreFoundation library
         ;; in the thread that's going to process events.  Looking up a
         ;; symbol in the library should cause it to be initialized
         #+apple-objc
         (ccl:open-shared-library "/System/Library/Frameworks/Cocoa.framework/Cocoa")
         #+cocotron-objc
         (progn
           (ccl:open-shared-library "Foundation.1.0.dll")
           (ccl:open-shared-library "AppKit.1.0.dll")
           (ccl:open-shared-library "CoreData.1.0.dll")
           ;; If the BOOL variable NSDebugEnabled can be found, ensure
           ;; that it's set to *NO*.  (When set to non-zero values, it
           ;; can cause attempts to raise NSExceptions to do nothing.
           ;; It's not clear how it gets set to a non-zero value.)
           (let* ((addr (ccl:foreign-symbol-address "NSDebugEnabled")))
             (when addr (setf (ccl:%get-unsigned-byte addr) *NO*)))
           ;; We may need to call #_NSInitializeProcess
           ;; under Cocotron.  If so, we'd need to do
           ;; so on standalone startup, too, and would
           ;; have to have heap-allocated the string vector
           ;; and its strings.
           #+not-yet
           (with-string-vector (argv (list (kernel-path)))
             (#_NSInitializeProcess 1 argv)))
       
         ;;(#_GetCurrentEventQueue)
         (current-ns-thread)
         (create-void-nsthread))))

  (pushnew #'load-cocoa-framework ccl::*lisp-system-pointer-functions* :key #'function-name)

  #-cocotron
  (load-cocoa-framework)

  #+cocotron
  (let* ((path (getenv "PATH")))
    (unwind-protect
         (progn
           (setenv "PATH"
                   (format nil "~a;~a"
                           (native-translated-namestring
                            (truename "ccl:cocotron;"))
                           path))
           (load-cocoa-framework))
      (setenv "PATH" path)))


  (defun find-cfstring-sections ()
    (ui:uiwarn "~s is obsolete" 'find-cfstring-sections)))


#+gnu-objc
(progn

  (defparameter *gnustep-system-root* #P"/usr/GNUstep/"
    "The root of GNUstep installation.")

  (defparameter *gnustep-libraries-pathname*
    (merge-pathnames "System/Library/Libraries/" *gnustep-system-root*))


  #+ccl
  (ccl:defloadvar *pending-loaded-classes* '())

  #+ccl
  (ccl:defcallback register-class-callback (:address class :address category :void)
    (let* ((id (map-objc-class class)))
      (unless (ccl:%null-ptr-p category)
        (let* ((cell (or (assoc id *pending-loaded-classes*)
                         (let* ((c (list id)))
                           (push c *pending-loaded-classes*)
                           c))))
          (push (ccl:%inc-ptr category 0) (cdr cell))))))


  ;; Shouldn't really be GNU-objc-specific.
  (defun get-c-format-string (c-format-ptr c-arg-ptr)
    (do* ((n 128))
         ()
      (declare (fixnum n))
      #+ccl
      (ccl:%stack-block ((buf n))
                        (let* ((m (#_vsnprintf buf n c-format-ptr c-arg-ptr)))
                          (declare (fixnum m))
                          (cond ((< m 0) (return nil))
                                ((< m n) (return (ccl:%get-cstring buf)))
                                (t (setf n m)))))
      #-ccl (niy get-c-format-string c-format-ptr c-arg-ptr)))


  (defun init-gnustep-framework ()
    (or (getenv "GNUSTEP_SYSTEM_ROOT")
        (setenv "GNUSTEP_SYSTEM_ROOT" *gnustep-system-root*))
    (ccl:open-shared-library "/usr/lib/gcc/x86_64-pc-linux-gnu/4.5.3/libobjc.so")
    #-ccl (niy init-gnustep-framework)
    #+ccl (setf (ccl:%get-ptr (ccl:foreign-symbol-address "_objc_load_callback"))
                register-class-callback)
    (ccl:open-shared-library "/usr/lib64/libgnustep-base.so")
    (ccl:open-shared-library "/usr/lib64/libgnustep-gui.so")
    ;; (ccl:open-shared-library (namestring (merge-pathnames "libgnustep-base.so"
    ;;                                                       *gnustep-libraries-pathname*)))
    ;; (ccl:open-shared-library (namestring (merge-pathnames "libgnustep-gui.so"
    ;;                                                       *gnustep-libraries-pathname*)))
    ))




(defun load-foreign-libraries ()

  #+ccl (setf (ccl::pkg.intern-hook (find-package "NSFUN")) 'get-objc-message-info)

  #+(and gnu-objc ccl)       (ccl::def-ccl-pointers gnustep-framework ()
                               (init-gnustep-framework))
  #+(and gnu-objc (not ccl)) (init-gnustep-framework)

  ;; An <NSA>ffine<T>ransform<S>truct is identical to a
  ;; (:struct :<GGA>ffine<T>ransform), except for the names of its fields.
  (setf (ccl::foreign-type-ordinal (ccl::parse-foreign-type '(:struct :<GGA>ffine<T>ransform)))
        (ccl::foreign-type-ordinal (ccl::parse-foreign-type :<NSA>ffine<T>ransform<S>truct)))


  (setf *cls-meta*   #+apple-objc #$CLS_META   #+gnu-objc #$_CLS_META)
  (setf *cls-class*  #+apple-objc #$CLS_CLASS  #+gnu-objc #$_CLS_CLASS)
  (setf *cls-resolv* #+apple-objc #$CLS_RESOLV #+gnu-objc #$_CLS_RESOLV)
  (setf *yes* #$YES)
  (setf *no*  #$NO)
  (setf *NSUTF8StringEncoding* #$NSUTF8StringEncoding)

  #+ccl
  (setf *objc-id-type*   (ccl::parse-foreign-type :id)
        *objc-sel-type*  (ccl::parse-foreign-type :<SEL>)
        *objc-char-type* (ccl::parse-foreign-type :char))
  #-ccl (niy initialize 'initializing 'foreign 'types))



(eval-when (:load-toplevel :execute)
  (load-foreign-libraries))

;;;; THE END ;;;;
