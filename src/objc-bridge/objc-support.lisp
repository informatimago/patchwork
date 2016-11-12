;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               objc-support.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;  
;;;;    ObjC support routines.
;;;;  
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-06-02 <PJB> Forked
;;;;BUGS
;;;;LEGAL
;;;;    LLGPL
;;;;  
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
;;;;    Copyright (c) 2003 Randall D. Beer
;;;;  
;;;;    This software is licensed under the terms of the Lisp Lesser
;;;;    GNU Public License, known as the LLGPL.  The LLGPL consists of
;;;;    a preamble and  the LGPL. Where these conflict, the preamble
;;;;    takes precedence.  The  LLGPL is available online at
;;;;    http://opensource.franz.com/preamble.html.
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
(enable-objc-reader-macros)


#+darwin-target
(unless (>= (parse-integer (software-version) :junk-allowed t) 10)
  (error "the Objective-C bridge needs at least Mac OS X 10.6"))





(defun all-init-keywords-for-class (c)
  (let* ((keyinfo ()))
    (dolist (class (class-precedence-list c))
      (when (eq class ns::ns-object)
        (return keyinfo))
      (dolist (class-keys (gethash class *class-init-keywords*))
        (pushnew class-keys keyinfo :test #'equal)))))


(defun send-init-message-for-class (class initargs)
  (let* ((all-keywords-for-class (all-init-keywords-for-class class)))
    (multiple-value-bind (initfunction args)
        (if all-keywords-for-class
            (let* ((candidate-functions ())
                   (candidate-arglists ())
                   (missing-keyword (cons nil nil)))
              (declare (dynamic-extent missing-keyword))
              (dolist (keys-and-function all-keywords-for-class)
                (collect ((arglist))
                         (destructuring-bind (keys . function) keys-and-function
                           (dolist (key keys (progn (push function candidate-functions)
                                                    (push (arglist) candidate-arglists)))
                             (let* ((val (getf initargs key missing-keyword)))
                               (if (eq missing-keyword val)
                                   (return)
                                   (arglist val)))))))
              (if candidate-functions
                  (if (null (cdr candidate-functions))
                      (values (car candidate-functions) (car candidate-arglists))
                      ;; Pick the longest match, if that's unique.  If there's
                      ;; no unique longest match, complain.
                      (let* ((maxlen 0)
                             (maxfun ())
                             (maxargs ())
                             (duplicate-match nil))
                        (declare (fixnum maxlen))
                        (do* ((functions candidate-functions (cdr functions))
                              (arglists candidate-arglists (cdr arglists)))
                             ((null functions)
                              (if duplicate-match
                                  (values nil nil)
                                  (values maxfun maxargs)))
                          (let* ((arglist (car arglists))
                                 (n (length arglist)))
                            (declare (fixnum n))
                            (if (> n maxlen)
                                (setf maxlen n
                                      duplicate-match nil
                                      maxargs arglist
                                      maxfun (car functions))
                                (if (= n maxlen)
                                    (setf duplicate-match t)))))))
                  (values '#/init nil)))
            (values '#/init nil))
      (if initfunction
          (let* ((instance (apply initfunction (#/alloc class) args)))
            (ensure-lisp-slots instance class)
            instance)
          (error "Can't determine ObjC init function for class ~s and initargs ~s." class initargs)))))


#+gnu-objc
(defun iterate-over-class-methods (class method-function)
  (do* ((mlist (ccl:pref class :objc_class.methods)
               (ccl:pref mlist :objc_method_list.method_next)))
       ((ccl:%null-ptr-p mlist))
    (do* ((n (ccl:pref mlist :objc_method_list.method_count))
          (i 0 (1+ i))
          (method (ccl:pref mlist :objc_method_list.method_list)
                  (ccl:%incf-ptr method (record-length :objc_method))))
         ((= i n))
      (declare (fixnum i n))
      (funcall method-function method class))))


#+gnu-objc
(progn
  ;; Er, um ... this needs lots-o-work.
  (let* ((objc-class-count 0))
    (defun reset-objc-class-count () (setf objc-class-count 0))
    (defun note-all-library-methods (method-function)
      (do* ((i objc-class-count (1+ i))
            (class (id->objc-class i) (id->objc-class i)))
           ((eq class 0))
        (iterate-over-class-methods class method-function)
        (iterate-over-class-methods (id->objc-metaclass i) method-function))))
  #+ccl
  (ccl::def-ccl-pointers revive-objc-classes ()
    (reset-objc-class-count)))


#+apple-objc-2.0
(progn
  (defun setup-objc-exception-globals ()
    (flet ((set-global (offset name)
             #-ccl (niy setup-objc-exception-globals)
             #+ccl
             (setf (ccl:%get-ptr (ccl:%int-to-ptr (+ (target-nil-value) (ccl:%kernel-global-offset offset))))
                   (ccl:foreign-symbol-address name))))
      (set-global 'objc-2-personality "___objc_personality_v0")
      (set-global 'objc-2-begin-catch "objc_begin_catch")
      (set-global 'objc-2-end-catch "objc_end_catch")
      (set-global 'unwind-resume "__Unwind_Resume")))

  #+ccl
  (ccl::def-ccl-pointers setup-objc-exception-handling ()
    (setup-objc-exception-globals)))


(defvar *condition-id-map* (make-id-map)
  "Map lisp conditions to small integers")


;;; Encapsulate an NSException in a lisp condition.
(define-condition ns-exception (error)
  ((ns-exception :initarg :ns-exception :accessor ns-exception))
  (:report (lambda (c s)
             (format s "Objective-C runtime exception: ~&~a"
                     (nsobject-description (ns-exception c))))))


(defun ensure-dealloc-method-for-class (class)
  (let* ((direct-slots (class-direct-slots class))
         (effective-slots (class-slots class)))
    (when (and (dolist (d direct-slots)
                 (when (and (typep d 'standard-direct-slot-definition)
                            (eq :instance (slot-definition-allocation d)))
                   (return t)))
               (dolist (e effective-slots t)
                 (when (and (typep e 'closer-mop:standard-effective-slot-definition)
                            (eq :instance (slot-definition-allocation e))
                            (not (find (slot-definition-name e)
                                       direct-slots
                                       :key #'slot-definition-name
                                       :test #'eq)))
                   (return))))
      (eval `(objc:defmethod (#/dealloc :void) ((self ,(class-name class)))
               (objc:remove-lisp-slots self)
               (call-next-method))))))


(eval-when (:compile-toplevel :execute)
  (declaim (ftype (function (&rest t) t) objc-callback-error-return)))


(defclass ns-lisp-exception (ns::ns-exception)
  ((condition :initarg :condition :initform nil :reader ns-lisp-exception-condition))
  (:metaclass ns::+ns-object))


(objc:defmethod #/init ((self ns-lisp-exception))
  (#/initWithName:reason:userInfo: self #@"lisp exception" #@"lisp exception" +null-ptr+))


(defun recognize-objc-exception (x)
  (if (typep x 'ns::ns-exception)
      (ns-exception->lisp-condition x)))


(defun objc:make-nsstring (string)
  #+ccl
  (with-encoded-cstrs :utf-8 ((s string))
                      (#/initWithUTF8String: (#/alloc ns::ns-string) s))
  #-ccl (niy objc:make-nsstring string))


(defun %make-nsstring (string)
  (objc:make-nsstring string))


(defmacro with-autoreleased-nsstring ((nsstring lisp-string) &body body)
  `(let* ((,nsstring (%make-nsstring ,lisp-string)))
     (#/autorelease ,nsstring)
     ,@body))


(defmacro objc:with-autoreleased-nsstrings (speclist &body body)
  (with-specs-aux 'with-autoreleased-nsstring speclist body))


(defun retain-objc-instance (instance)
  (#/retain instance))


;;; May have to create/release autorelease pools before the bridge
;;; is fully reinitialized, so use low-level OBJC-MESSAGE-SEND
;;; and @class.
(defun create-autorelease-pool ()
  (objc-message-send
   (objc-message-send (@class "NSAutoreleasePool") "alloc") "init"))


(defun release-autorelease-pool (p)
  (objc-message-send p "release" :void))


(defun lisp-string-from-nsstring (nsstring)
  (with-autorelease-pool
      ;; It's not clear that it's even possible to lose information
      ;; when converting to UTF-8, but allow lossage to occur, just in
      ;; case.
      (let* ((data (#/dataUsingEncoding:allowLossyConversion:
                    nsstring *NSUTF8StringEncoding* t))
             (len (#/length data)))
        (if (= len 0)
            ""
            (let* ((bytes (#/bytes data))
                   (nchars (utf-8-length-of-memory-encoding bytes len 0))
                   (string (make-string nchars)))
              (utf-8-memory-decode bytes len 0 string)
              string)))))


(objc:defmethod #/reason ((self ns-lisp-exception))
  (with-slots (condition) self
    (if condition
        (#/autorelease (%make-nsstring (format nil "~A" condition)))
        (call-next-method))))


(objc:defmethod #/description ((self ns-lisp-exception))
  (#/stringWithFormat: ns::ns-string #@"Lisp exception: %@" (#/reason self)))


(defun ns-exception->lisp-condition (nsexception)
  (if (typep nsexception 'ns-lisp-exception)
      (ns-lisp-exception-condition nsexception)
      (make-condition 'ns-exception :ns-exception nsexception)))


(defmethod ns-exception ((c condition))
  "Map a lisp condition object to an NSException.  Note that instances
of the NS-EXCEPTION condition class implement this by accessing an
instance variable."
  ;;; Create an NSLispException with a lispid that encapsulates
  ;;; this condition.

  ;; (dbg (format nil "~a" c))
  ;;(#_NSLog #@"Lisp exception: %@" :id (%make-nsstring (format nil "~a" c)))
  (make-instance 'ns-lisp-exception :condition c))



#+(or apple-objc cocotron-objc)         ; not really
(progn

  #+ppc-target
  (defun objc-callback-error-return (condition return-value-pointer return-address-pointer)
    ;; On PPC, the "address" of an external entry point is always
    ;; aligned on a 32-bit word boundary.  On PPC32, it can always
    ;; be represented as a fixnum; on PPC64, it might be a pointer
    ;; instead.
    ;; Note that this clobbers the actual (foreign) return address,
    ;; replacing it with the address of #__NSRaiseError.  Note also
    ;; that storing the NSException object as the return value has
    ;; the desired effect of causing #__NSRaiseError to be called
    ;; with that NSException as its argument (because r3 is used both
    ;; as the canonical return value register and used to pass the
    ;; first argument on PPC.)
    (process-debug-condition *current-process* condition (ccl:%get-frame-ptr))
    (let* ((addr (ccl:%reference-external-entry-point (load-time-value (external "__NSRaiseError")))))
      (if (typep addr 'fixnum)
          (ccl:%set-object return-address-pointer 0 addr)
          (setf (ccl:%get-ptr return-address-pointer 0) addr)))
    (setf (ccl:%get-ptr return-value-pointer 0) (ns-exception condition))
    nil)

  #+x8664-target
  (progn
    #+ccl
    (ccl:defloadvar *x8664-objc-callback-error-return-trampoline*
        (let* ((code-bytes '(#x48 #x89 #xc7      ; movq %rax %rdi
                             #x66 #x48 #x0f #x7e #xc0 ; movd %xmm0,%rax
                             #x52                ; pushq %rdx
                             #xff #xe0))         ; jmp *rax
               (nbytes (length code-bytes))
               (ptr (ccl:%allocate-callback-pointer 16)))
          (dotimes (i nbytes ptr)
            (setf (ccl:%get-unsigned-byte ptr i) (pop code-bytes)))))

    (defun objc-callback-error-return (condition return-value-pointer return-address-pointer) 
      ;; The callback glue reserves space for %rax at return-value-pointer-8,
      ;; for %rdx at -16, for %xmm0 at -24.  Store NS-EXCEPTION in the
      ;; %rax slot, the address of #_objc_exception_throw in the %rdx slot, the
      ;; original return address in the %xmm0 slot, and force a return to
      ;; the trampoline code above.
      (process-debug-condition *current-process* condition (ccl:%get-frame-ptr))
      (setf (ccl:%get-ptr return-value-pointer -8) (ns-exception condition)
            (ccl:%get-ptr return-value-pointer -16) (ccl:%get-ptr return-address-pointer 0)
            (ccl:%get-ptr return-address-pointer 0) *x8664-objc-callback-error-return-trampoline*)
      ;; A foreign entry point is always an integer on x8664.
      (let* ((addr (ccl:%reference-external-entry-point (load-time-value (external "_objc_exception_throw")))))
        (if (< addr 0)                      ;unlikely
            (setf (ccl:%%get-signed-longlong return-value-pointer -24) addr)
            (setf (ccl:%%get-unsigned-longlong return-value-pointer -24) addr)))
      nil))

  #+x8632-target
  (progn

    #+ccl
    (ccl:defloadvar *x8632-objc-callback-error-return-trampoline*
        (let* ((code-bytes '(#x83 #xec #x10      ; subl $16,%esp
                             #x89 #x04 #x24      ; movl %eax,(ccl:%esp)
                             #x52                ; pushl %edx
                             #xff #xe1))         ; jmp *ecx
               (nbytes (length code-bytes))
               (ptr (ccl:%allocate-callback-pointer 16)))
          (dotimes (i nbytes ptr)
            (setf (ccl:%get-unsigned-byte ptr i) (pop code-bytes)))))

    (defun objc-callback-error-return (condition return-value-pointer return-address-pointer)
      (process-debug-condition *current-process* condition (ccl:%get-frame-ptr))
      (let* ((addr (ccl:%reference-external-entry-point (load-time-value (external #+cocotron-objc "_NSRaiseException" #-cocotron-objc "__NSRaiseError")))))
        (setf (ccl:%get-unsigned-long return-value-pointer -12 ) addr))
      (setf (ccl:%get-ptr return-value-pointer -8) (ns-exception condition)
            (ccl:%get-ptr return-value-pointer -4) (ccl:%get-ptr return-address-pointer)
            (ccl:%get-ptr return-address-pointer) *x8632-objc-callback-error-return-trampoline*)
      nil)))



(defun open-main-bundle ()
  (#/mainBundle ns::ns-bundle))


;;; Create a new immutable dictionary just like src, replacing the
;;; value of each key in key-value-pairs with the corresponding value.
(defun copy-dictionary (src &rest key-value-pairs)
  (declare (dynamic-extent key-value-pairs))
  ;; (#_NSLog #@"src = %@" :id src)
  (let* ((count  (#/count src))
         (enum   (#/keyEnumerator src))
         (keys   (#/arrayWithCapacity: ns::ns-mutable-array count))
         (values (#/arrayWithCapacity: ns::ns-mutable-array count)))
    (loop
      (let* ((nextkey (#/nextObject enum)))
        (when (ccl:%null-ptr-p nextkey)
          (return))
        (do* ((kvps key-value-pairs (cddr kvps))
              (newkey (car kvps) (car kvps))
              (newval (cadr kvps) (cadr kvps)))
             ((null kvps)
              ;; Copy the key, value pair from the src dict
              (#/addObject: keys nextkey)
              (#/addObject: values (#/objectForKey: src nextkey)))
          (when (#/isEqualToString: nextkey newkey)
            (#/addObject: keys nextkey)
            (#/addObject: values newval)
            (return)))))
    (make-instance 'ns::ns-dictionary
        :with-objects values
        :for-keys keys)))


(defparameter *objc-description-max-length* 1024
  "Limit on the length of NSObject description strings if non-NIL.")


(defun %cf-instance-p (instance)
  #-apple-objc (declare (ignore instance))
  #+apple-objc (< 1 (objc-message-send instance "_cfTypeID" #>CFTypeID)))


(defun initialized-nsobject-p (nsobject)
  (or (objc-class-p nsobject)
      (objc-metaclass-p nsobject)
      (has-lisp-slot-vector nsobject)
      (let* ((cf-p  (%cf-instance-p nsobject)) 
             (isize (if cf-p
                        (ccl:external-call "malloc_size" :address nsobject :size_t)
                        (%objc-class-instance-size (#/class nsobject))))
             (skip  (if cf-p
                        (+ (record-length :id) 4 #+64-bit-target 4)
                        (record-length :id))))
        (declare (fixnum isize skip))
        (or (> skip isize)
            (do* ((i skip (1+ i)))
                 ((>= i isize))
              (declare (fixnum i))
              (unless (zerop (the (unsigned-byte 8) (ccl:%get-unsigned-byte nsobject i)))
                (return t)))))))


(defun nsobject-description (nsobject)
  "Returns a lisp string that describes nsobject.  Note that some
NSObjects describe themselves in more detail than others."
  (if (initialized-nsobject-p nsobject)
      (with-autorelease-pool
          (let* ((desc (#/description nsobject)))
            (if (or (null *objc-description-max-length*)
                    (< (#/length desc) *objc-description-max-length*))
                (lisp-string-from-nsstring desc)
                (ns:with-ns-range (r 0 *objc-description-max-length*)
                  (format nil "~a[...]"
                          (lisp-string-from-nsstring (#/substringWithRange: desc r)))))))
      "[uninitialized]"))



(defun lisp-string-from-nsstring-substring (nsstring start length)
  (let* ((substring (#/substringWithRange: nsstring (ns:make-ns-range start length))))
    (lisp-string-from-nsstring substring)))



(defun reset-listener-autorelease-pool ()
  "Release and reestablish *LISTENER-AUTORELEASE-POOL*"
  (when (eql *break-level* 0)
    (without-interrupts
        (when (boundp '*listener-autorelease-pool*)
          (let ((old *listener-autorelease-pool*))
            (setf *listener-autorelease-pool* (create-autorelease-pool))
            (when old
              (release-autorelease-pool old)))))))


(defun release-listener-autorelease-pool ()
  "Release (but don't reestablish) *LISTENER-AUTORELEASE-POOL*"
  (when (eql *break-level* 0)
    (without-interrupts
        (when (boundp '*listener-autorelease-pool*)
          (let ((old *listener-autorelease-pool*))
            (setf *listener-autorelease-pool* nil)
            (when old
              (release-autorelease-pool old)))))))


#+ccl
(ccl::define-toplevel-command :global rap ()
                              "Release and reestablish *LISTENER-AUTORELEASE-POOL*"
                              (reset-listener-autorelease-pool))

#+ccl
(ccl::define-toplevel-command :global kap ()
                              "Release (but don't reestablish) *LISTENER-AUTORELEASE-POOL*"
                              (release-listener-autorelease-pool))


#+apple-objc
(defun show-autorelease-pools ()
  (objc-message-send (@class ns-autorelease-pool) "showPools" :void))


#+gnu-objc
(defun show-autorelease-pools ()
  (do* ((current (objc-message-send (@class ns-autorelease-pool) "currentPool")
                 (objc-message-send current "_parentAutoreleasePool"))
        (i 0 (1+ i)))
       ((ccl:%null-ptr-p current) (values))
    (format t "~& ~d : ~a [~d]"
            i
            (nsobject-description current)
            (ccl:pref current :<NSA>utorelease<P>ool._released_count))))


#+cocotron-objc
(defun show-autorelease-pools ()
  (ccl:%string-to-stderr  "No info about current thread's autorelease pools is available"))


#+ccl
(ccl::define-toplevel-command :global sap ()
                              "Log information about current thread's autorelease-pool(s) to C's standard error stream"
                              (show-autorelease-pools))



;;; Use the interfaces for an add-on ObjC framework.  We need to
;;; tell the bridge to reconsider what it knows about the type
;;; signatures of ObjC messages, since the new headers may define
;;; a method whose type signature differs from the message's existing
;;; methods.  (This probably doesn't happen too often, but it's
;;; possible that some SENDs that have already been compiled would
;;; need to be recompiled with that augmented method type info, e.g.,
;;; because ambiguity was introduced.)

(defun augment-objc-interfaces (dirname)
  #+ccl (ccl:use-interface-dir dirname)
  (register-objc-class-decls)
  (update-objc-method-info))


;;; A list of "standard" locations which are known to contain
;;; framework bundles.  We should look in ~/Library/Frameworks/" first,
;;; if it exists.
(defparameter *standard-framework-directories*
  (list #P"/Library/Frameworks/"
        #P"/System/Library/Frameworks/"
        #+gnu-objc #P"/usr/lib/GNUstep/Frameworks/")



;;; This has to run during application (re-)initializtion, so it
;;; uses lower-level bridge features.
  (defun %reload-objc-framework (path)
    (when (probe-file path)
      (let* ((namestring (native-translated-namestring path)))
        (ccl:with-cstrs ((cnamestring namestring))
          (with-nsstr (nsnamestring cnamestring (length namestring))
            (with-autorelease-pool
                (let* ((bundle (objc-message-send (@class "NSBundle")
                                                  "bundleWithPath:"
                                                  :id nsnamestring :id)))
                  (unless (ccl:%null-ptr-p bundle)
                    (objc-message-send bundle "load" :<BOOL>)))))))))


  (defun load-objc-extension-framework (name)
    (let* ((dirs *standard-framework-directories*)
           (home-frameworks (make-pathname :defaults nil
                                           :directory
                                           (append (pathname-directory
                                                    (user-homedir-pathname))
                                                   '("Library" "Frameworks"))))
           (fname (list (format nil "~a.framework" name))))
      (when (probe-file home-frameworks)
        (pushnew home-frameworks dirs :test #'equalp))
      (dolist (d dirs)
        (let* ((path (probe-file (make-pathname :defaults nil
                                                :directory (append (pathname-directory d)
                                                                   fname)))))
          (when path
            (let* ((namestring (native-translated-namestring path)))
              (ccl:with-cstrs ((cnamestring namestring))
                (with-nsstr (nsnamestring cnamestring (length namestring))
                  (with-autorelease-pool
                      (let* ((bundle (#/bundleWithPath: ns::ns-bundle nsnamestring))
                             (winning (unless (ccl:%null-ptr-p bundle)
                                        t)))
                        (when winning
                          (let* ((libpath (#/executablePath bundle)))
                            (unless (ccl:%null-ptr-p libpath)
                              (ccl:open-shared-library (lisp-string-from-nsstring
                                                    libpath))))
                          (#/load bundle)
                          (pushnew path *extension-framework-paths*
                                   :test #'equalp)
                          (map-objc-classes)
                          ;; Update info about init messages.
                          (register-objc-init-messages)
                          (register-objc-set-messages))
                        (return winning)))))))))))


  (defun objc:load-framework (framework-name interfaces-name)
    #+ccl (ccl:use-interface-dir interfaces-name)
    (or (load-objc-extension-framework framework-name)
        (error "Can't load ObjC framework ~s" framework-name))
    (augment-objc-interfaces interfaces-name))


  (defmethod print-object ((p ns:protocol) stream)
    (print-unreadable-object (p stream :type t)
      (format stream "~a (#x~x)"
              (ccl:%get-cstring (#/name p))
              (ccl:%ptr-to-int p))))


  (defmethod terminate ((instance objc:objc-object))
    (objc-message-send instance "release"))


  #+ccl
  (ccl:defloadvar *tagged-instance-class-indices*
      (let* ((alist ()))
        ;; There should be a better way of doing this.  (A much better way.)
        (let* ((instance (#/initWithInt: (#/alloc ns::ns-number) 0))
               (tag (tagged-objc-instance-p instance)))
          (if tag
              (let* ((class (objc-message-send instance "class")))
                (unless (ccl:%null-ptr-p class)
                  (install-foreign-objc-class class nil)
                  (push (cons tag (objc-class-or-private-class-id class)) alist)))
              (#/release instance)))
        alist))


  (defun objc-tagged-instance-class-index (tag)
    (cdr (assoc tag *tagged-instance-class-indices* :test #'eq)))


;;;; THE END ;;;;
