;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               objc-classes.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;  
;;;;    objc-class stuff. 
;;;;  
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-06-05 <PJB> Extracted from objc-support.
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+darwin-target                     (pushnew :apple-objc      *features*)
  #+(and darwin-target 64-bit-target) (pushnew :apple-objc-2.0  *features*)
  #+win32-target                      (pushnew :cocotron-objc   *features*)
  #-(or darwin-target win32-target)   (pushnew :gnu-objc        *features*))

#+ccl
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(or apple-objc cocotron-objc) (ccl:use-interface-dir :cocoa)
  #+gnu-objc                      (ccl:use-interface-dir :gnustep))

(enable-objc-reader-macros)


(defun allocate-objc-object (class)
  (#/alloc class))


(defun conforms-to-protocol (thing protocol)
  (#/conformsToProtocol: thing (objc-protocol-address protocol)))



#+(or apple-objc cocotron-objc)
(defun iterate-over-objc-classes (fn)
  (let* ((n (#_objc_getClassList (ccl:%null-ptr) 0)))
    (declare (fixnum n))
    (ccl:%stack-block ((buffer (the fixnum (ash n target::word-shift))))
                  (#_objc_getClassList buffer n)
                  (do* ((i 0 (1+ i)))
                       ((= i n) (values))
                    (declare (fixnum i))
                    (funcall fn (ccl:paref buffer (:* :id) i))))))


#+(or apple-objc cocotron-objc)
(defun count-objc-classes ()
  (#_objc_getClassList (ccl:%null-ptr) 0))


#+gnu-objc
(defun iterate-over-objc-classes (fn)
  #-ccl (niy iterate-over-objc-classes fn)
  #+ccl (ccl:rletZ ((enum-state :address))
                   (loop
                     (let* ((class (#_objc_next_class enum-state)))
                       (if (ccl:%null-ptr-p class)
                           (return)
                           (funcall fn class))))))


#+gnu-objc
(defun count-objc-classes ()
  #-ccl (niy count-objc-classes)
  #+ccl (let* ((n 0))
          (declare (fixnum n))
          (ccl:rletZ ((enum-state :address))
                     (loop
                       (if (ccl:%null-ptr-p (#_objc_next_class enum-state))
                           (return n)
                           (incf n))))))








;;;--- Selectors

;;; Likewise, we want to cache the selectors ("SEL"s) which identify
;;; method names.  They can vary from session to session, but within
;;; a session, all methods with a given name (e.g, "init") will be
;;; represented by the same SEL.
(defun get-selector-for (method-name &optional error)
  (ccl:with-cstrs ((cmethod-name method-name))
    (let* ((p (#+(or apple-objc cocotron-objc) #_sel_getUid
                 #+gnu-objc #_sel_get_uid
                 cmethod-name)))
      (if (ccl:%null-ptr-p p)
          (if error
              (error "Can't find ObjC selector for ~a" method-name))
          p))))

(defvar *objc-selectors* (make-hash-table :test #'equal))

(defstruct objc-selector
  name
  %sel)

(defun %get-selector (selector &optional (error-p t))
  (or (objc-selector-%sel selector)
      (setf (objc-selector-%sel selector)
            (get-selector-for (objc-selector-name selector) error-p))))

(defun clear-objc-selectors ()
  (maphash (lambda (name sel)
               (declare (ignore name))
               (setf (objc-selector-%sel sel) nil))
           *objc-selectors*))

;;; Find or create a SELECTOR; don't bother resolving it.
(defun ensure-objc-selector (name)
  (setf name (string name))
  (or (gethash name *objc-selectors*)
      (setf (gethash name *objc-selectors*)
            (make-objc-selector :name name))))

(defun load-objc-selector (name)
  (let* ((selector (ensure-objc-selector name)))
    (%get-selector selector nil)
    selector))

(defmacro @SELECTOR (name)
  `(%get-selector ,(load-objc-selector name)))

(defmethod make-load-form ((s objc-selector) &optional env)
  (declare (ignore env))
  `(load-objc-selector ,(objc-selector-name s)))

;;;---




;;; #_objc_msgSend takes two required arguments (the receiving object
;;; and the method selector) and 0 or more additional arguments;
;;; there'd have to be some macrology to handle common cases, since we
;;; want the compiler to see all of the args in a foreign call.

;;; I don't remmber what the second half of the above comment might
;;; have been talking about.

(defmacro objc-message-send (receiver selector-name &rest argspecs)
  (when (evenp (length argspecs))
    (setf argspecs (append argspecs '(:id))))
  #+(or apple-objc cocotron-objc)
  (funcall (ccl::ftd-ff-call-expand-function ccl::*target-ftd*)
           `(ccl:%ff-call (ccl:%reference-external-entry-point (load-time-value (external "objc_msgSend"))))
           `(:address ,receiver :<SEL> (@selector ,selector-name) ,@argspecs)
           :arg-coerce 'objc-arg-coerce
           :result-coerce 'objc-result-coerce)
  #+gnu-objc
  (let* ((r (gensym))
         (s (gensym))
         (imp (gensym)))
    `(ccl:with-macptrs ((,r ,receiver)
                        (,s (@selector ,selector-name))
                        (,imp (ccl:external-call "objc_msg_lookup"
                                                 :id ,r
                                                 :<SEL> ,s
                                                 :<IMP>)))
       ,(funcall (ccl::ftd-ff-call-expand-function ccl::*target-ftd*)
                 `(ccl:%ff-call ,imp)
                 `(:address ,receiver :<SEL> (@selector ,selector-name) ,@argspecs)
                 :arg-coerce 'objc-arg-coerce
                 :result-coerce 'objc-result-coerce))))







(defun %note-protocol (p)
  ;; In Cocotron (which is ultimately based on the GNU ObjC runtime),
  ;; it may be the case that some Protocol objects aren't fully initialized
  ;; when this code runs, hence the sleazy use of PREF here.
  (ccl:with-macptrs ((cname #+cocotron-objc (ccl:pref p #>Protocol.nameCString)
                            #-cocotron-objc (objc-message-send p "name" :address)))
    (let* ((namelen (ccl::%cstrlen cname))
           (name (make-string namelen)))
      (declare (dynamic-extent name))
      (ccl:%str-from-ptr cname namelen name)
      (let* ((proto (or (gethash name *objc-protocols*)
                        (progn
                          (setf name (subseq name 0))
                          (setf (gethash name *objc-protocols*)
                                (make-objc-protocol :name name))))))
        (unless (objc-protocol-address proto)
          (setf (objc-protocol-address proto) (ccl:%inc-ptr p 0)))
        proto))))


(defun note-class-protocols (class)
  #-(or apple-objc-2.0)
  (do* ((protocols (ccl:pref class :objc_class.protocols)
                   (ccl:pref protocols :objc_protocol_list.next)))
       ((ccl:%null-ptr-p protocols))
    (let* ((count (ccl:pref protocols :objc_protocol_list.count)))
      (ccl:with-macptrs ((list (ccl:pref protocols :objc_protocol_list.list)))
        (dotimes (i count)
          (ccl:with-macptrs ((p (ccl:paref list (:* (:* (:struct :<P>rotocol))) i)))
            (%note-protocol p))))))
  #+(and (or apple-objc-2.0) ccl)
  (ccl:rlet ((p-out-count :int 0))
        (ccl:with-macptrs ((protocols (#_class_copyProtocolList class p-out-count)))
          (let* ((n (ccl:pref p-out-count :int)))
            (dotimes (i n)
              (ccl:with-macptrs ((p (ccl:paref protocols (:* (:* (:struct :<P>rotocol))) i)))
                (%note-protocol p))))
          (unless (ccl:%null-ptr-p protocols) (#_free protocols))))
  #+(and (or apple-objc-2.0) (not ccl))
  (niy note-class-protocols class))


(defun map-objc-classes (&optional (lookup-in-database-p t))
  (iterate-over-objc-classes
   (lambda (class)
       (note-class-protocols class)
       (install-foreign-objc-class class lookup-in-database-p))))


(let* ((nclasses 0)
       (lock (bordeaux-threads:make-lock)))
  (declare (fixnum nclasses))
  (defun maybe-map-objc-classes (&optional use-db)
    (bordeaux-threads:with-lock-held (lock)
      (let* ((new (count-objc-classes)))
        (declare (fixnum new))
        (unless (= nclasses new)
          (setf nclasses new)
          (map-objc-classes use-db))
        t)))
  (defun reset-objc-class-count ()
    (bordeaux-threads:with-lock-held (lock)
      (setf nclasses 0))))


;;;; THE END ;;;;
