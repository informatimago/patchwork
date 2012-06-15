;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               wrapper.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines the wrapper mixin class.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-06-11 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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

(in-package "MCLGUI")
(objcl:enable-objcl-reader-macros)


(defclass wrapper ()
  ((handle :initform nil
           :initarg :handle
           :reader handle
           :documentation "The NSObject instance wrapped over."))
  (:documentation "This mixin adds a wrapped-over NSObject instance handle to a wrapper object."))


(defmethod initialize-instance :after ((self wrapper) &key &allow-other-keys)
  (when (handle self)
    [(handle self) retain])
  self)


(defmethod (setf handle) (new-handle (wrapper wrapper))
  (let ((old-handle (handle wrapper)))
    (if new-handle
        (unless (eq old-handle new-handle)
          (when old-handle
            [old-handle release])
          [new-handle retain]
          (setf (slot-value wrapper 'handle) new-handle))
        (when old-handle
          [old-handle release]
          (setf (slot-value wrapper 'handle) nil))))
  new-handle)


(defgeneric unwrap (wrapper)
  (:documentation "
DO:             Create and initialize the underlying object and bind
                it to the HANDLE of the WRAPPER, unless it's already
                there.

POST:           (not (null (handle wrapper)))

RETURN:         (handle wrapper)

NOTE:           There are functions such as WRAP-NSMENU to build
                subclass-of-WRAPPER instances from
                subclass-of-NSObject instances, hence the name of
                UNWRAP.

NOTE:           Subclasses should define a method, calling
                (unwrapping object …).

SEE ALSO:       UNWRAPPING, WRAPPING.
"))


(defgeneric release (wrapper)
  (:documentation "

DO:             Release the NSObject retained by this WRAPPER and all
                its components.

POST:           (null (handle wrapper))

RETURN:         WRAPPER

")
  (:method ((wrapper wrapper))
    (setf (handle wrapper) nil)
    wrapper))



(defvar *wrapping* nil)

(defmacro wrapping (&body body)
  "
Wrapping functions should use this macro so that calls to UNWRAP are
detected and inhibited.
"
  `(let ((*wrapping* t))
     ,@body))


(defmacro unwrapping (object &body body)
  "
DO:             Execute BODY, unless a wrapping is occuring, in which
                case it just check that OBJECT already has a handle.
"
  (let ((vobject (gensym "object")))
    `(let ((,vobject ,object))
       (if *wrapping*
           (let ((handle (handle ,vobject)))
             (unless handle
               (cerror "Continue" "Called (UNWRAP ~S) while wrapping." ,vobject))
             handle)
           (progn
             ,@body)))))

;;;; THE END ;;;;
