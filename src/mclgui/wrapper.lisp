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


(defmacro with-handle ((handle-var wrapper) &body body)
  "
DO:  Binds HANDLE-VAR to (handle WRAPPER) and then executes BODY only
     when the handle of the WRAPPER is not NULL.
"
  `(let ((,handle-var (handle ,wrapper)))
     (when ,handle-var
       ,@body)))


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




(defun wrap (nsobject)
  ;; Note: circular ns structures not implemented yet.
  (cond
    ;; Atoms:
    ((nullp nsobject)
     nil)
    ([nsobject isKindOfClass:(oclo:@class "NSString")]
     (objcl:lisp-string nsobject))
    ([nsobject isKindOfClass:(oclo:@class "NSNumber")]
     (let ((objctype (aref (objcl:lisp-string [nsobject objCType]) 0)))
       (cond
         ((find objctype #.(vector #$_C_FLT #$_C_DBL))
          [nsobject doubleValue])
         ((find objctype #.(vector #$_C_UCHR #$_C_USHT #$_C_UINT #$_C_ULNG #$_C_ULNG_LNG))
          [nsobject unsignedLongLongValue])
         ((find objctype #.(vector #$_C_CHR #$_C_SHT #$_C_INT #$_C_LNG #$_C_LNG_LNG #$_C_BOOL))
          [nsobject unsignedLongLongValue])
         (t
          nsobject))))
    (t
     ;; Compound objects:
     ;; (let ((*wrap-objects* (or *wrap-objects* (make-hash-table))))
     (cond
       ([nsobject isKindOfClass:(oclo:@class "NSArray")]
        (wrap-nsarray nsobject))
       ([nsobject isKindOfClass:(oclo:@class "NSMenu")]
        (wrap-nsmenu nsobject))
       ([nsobject isKindOfClass:(oclo:@class "NSMenuItem")]
        (wrap-nsmenuitem nsobject))
       ([nsobject isKindOfClass:(oclo:@class "NSWindow")]
        (wrap-nswindow nsobject))
       ([nsobject isKindOfClass:(oclo:@class "NSDictionary")]
        (wrap-nsdictionary nsobject))
       ([nsobject isKindOfClass:(oclo:@class "NSNotification")]
        (wrap-nsnotification nsobject))
       (t
        nsobject))
     ;;)
     )))



(defun wrap-nsdictionary (nsdictionary)
   "
RETURN:         A fresh P-list containing the NSDICTIONARY entries.

DO:             Keys are converted to keywords; the values are
                converted to lisp type if possible, or else left as
                foreign types.

NOTE:           It's expected the dictionary is small, hence the P-list.
"
   (wrapping
    (if (nullp nsdictionary)
        nil
        (loop
          :with keyword = (find-package "KEYWORD")
          :with enum =  [nsdictionary keyEnumerator]
          :for key = [enum nextObject]
          :until (nullp key)
          :collect (intern (objcl:lisp-string key) keyword)
          :collect (wrap [nsdictionary objectForKey:key])))))


(defun wrap-nsarray (nsarray)
  (wrapping
   (let ((result '()))
     (dotimes (i [nsarray count] (nreverse result))
       (push (wrap [nsarray objectAtIndex:i]) result)))))



(defmethod unwrap ((item symbol))
  (unwrapping
   (objcl:objcl-string (symbol-name item))))

(defmethod unwrap ((item string))
  (unwrapping
   (objcl:objcl-string item)))

(defmethod unwrap ((item integer))
  (unwrapping
   [NSNumber numberWithLongLong:item]))

(defmethod unwrap ((item real))
  (unwrapping
   [NSNumber numberWithDouble:(coerce item 'double-float)]))

(defmethod unwrap ((list cons))
  (unwrapping
   (let ((nsarray [NSMutableArray arrayWithCapacity:(length list)]))
     (dolist (element list nsarray)
       [nsarray addObject:(unwrap element)]))))




(defun nsarray-to-list (nsarray)
  (wrap-nsarray nsarray))

(defun list-to-nsarray (list)
  (if list
      (unwrap list)
      [NSMutableArray arrayWithCapacity:0]))


;;;; THE END ;;;;
