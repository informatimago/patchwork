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
;;;;    2014-03-10 <PJB> Corrected WRAPPING macro.
;;;;    2012-06-11 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2014
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


(defun map-nsarray (result-type mapper nsarray &rest other-sequences)
  (let ((min-length (reduce (function min)
                            (cons nsarray other-sequences)
                            :key (lambda (sequence)
                                     (etypecase sequence
                                       (ns:ns-array [sequence count])
                                       (vector (length sequence))
                                       (cons   (length sequence))
                                       (null   0)))))
        (enumerators  (mapcar (lambda (sequence)
                                  (etypecase sequence
                                    (ns:ns-array   (lambda (index) [sequence objectAtIndex:index]))
                                    (vector        (lambda (index) (aref sequence index)))
                                    (cons          (lambda (index) (declare (ignore index)) (pop sequence)))
                                    (null          (constantly nil))))
                              (cons nsarray other-sequences)))
        result setter postprocessing)
    (cond
      ((null result-type)
       (setf result nil
             setter (lambda (value index) (declare (ignore index)) value)
             postprocessing (function identity)))
      ((subtypep result-type 'ns:ns-array)
       (setf result [NSMutableArray arrayWithCapacity:min-length]
             setter (lambda (value index) (declare (ignore index)) [result addObject:value])
             postprocessing (function identity)))
      ((subtypep result-type 'string)
       (setf result (make-string min-length)
             setter (lambda (value index) (setf (aref result index) value))
             postprocessing (function identity)))
      ((subtypep result-type 'vector)
       (setf result (make-array  min-length)
             setter (lambda (value index) (setf (aref result index) value))
             postprocessing (function identity)))
      ((subtypep result-type 'list)
       (setf result '()
             setter (lambda (value index) (declare (ignore index)) (push value result))
             postprocessing (function nreverse)))
      (t
       (error "Unexpected result-type ~S" result-type)))
    (loop
      :for i :below min-length
      :do (funcall setter (apply mapper (mapcar (lambda (enumerator) (funcall enumerator i)) enumerators)) i))
    (funcall postprocessing result)))

(defmacro do-nsarray ((element-variable nsarray-expression &optional result-form) &body body)
  (let ((varray (gensym))
        (vindex (gensym)))
    `(let* ((,varray ,nsarray-expression))
       (dotimes (,vindex [,varray count] ,result-form)
         (let ((,element-variable [,varray objectAtIndex:,vindex]))
           ,@body)))))

(defmacro do-nsdictionary ((key-variable value-variable nsdictionary-expression &optional result-form) &body body)
  (let ((vdictionary (gensym))
        (vkeys (gensym))
        (vindex (gensym)))
    `(let* ((,vdictionary ,nsdictionary-expression)
            (,vkeys [,vdictionary allKeys]))
       (dotimes (,vindex [,vkeys count] ,result-form)
         (let* ((,key-variable   [,vkeys objectAtIndex:,vindex])
                (,value-variable [,vdictionary objectForKey:,key-variable]))
           ,@body)))))



(defgeneric handle (object)
  (:documentation "The NSObject instance wrapped over.")
  (:method ((none null)) nil))


(defgeneric update-handle (wrapper)
  (:documentation "
Some subclasses need to compute a new NS instance when the lisp
instance state changes.  This method compute and sets, the new (handle
wrapper), or updates an old (handle wrapper) object from the wrapper.

UNWRAP of a subclass could be implemented as:

    (defmethod unwrap ((self some-class))
       (unwrapping self
          (or (handle self) (update-handle self))))

"))



(defclass wrapper ()
  ((handle :initform nil
           :initarg :handle
           :reader handle))
  (:documentation "
This mixin adds a wrapped-over NSObject instance handle to a wrapper object.

Subclasses should implement a method for UPDATE-HANDLE to initialize
the Objective-C object.
"))

(defmethod print-object ((self wrapper) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (prin1 (handle self) stream))
  self)

(defmacro with-handle ((handle-var wrapper) &body body)
  "
DO:             Binds HANDLE-VAR to (handle WRAPPER) and then executes
                BODY only when the handle of the WRAPPER is not NULL.

RETURN:         The result of BODY if the WRAPPER has a handle, NIL
                otherwise.
"
  `(let ((,handle-var (handle ,wrapper)))
     (when ,handle-var
       ,@body)))



;;;------------------------------------------------------------
;;; wrapper
;;;------------------------------------------------------------

(defgeneric structure (object element-function)
  (:documentation "The object should call element-function on each of its elements."))

(defgeneric wrap (object)
  (:documentation "
Return a lisp object, usually an instance of a subclass of WRAPPER,
that represents or wraps over the NS object.  Simple classes such as
NSString or NSNumber can be 'wrapped' as lisp objects of types such as
STRING or NUMBER
"))




(defun recursive-circular-register (key object)
  (declare (ignore key))
  (when (circular-register object)
    (structure object (function recursive-circular-register))))

(defvar *wrapping* nil)

(defmethod wrap :around (object)
  (let ((*wrapping* t))
   (if *circular-references*
       (call-next-method)
       (with-circular-references ()
         (recursive-circular-register :root object)
         (call-next-method)))))

(defun wrap-resolving-circular-references (object)
  (resolve-circular-reference object (wrap object)))


(defmacro wrapping (object &body body)
  "
DO:             Wrapping functions should use this macro so that calls
                to UNWRAP are detected and inhibited.

NOTE:           WRAPPING updates the instance from a NS object.
"
  (let ((fbody (gensym)))
    `(let ((*wrapping* t))
       (flet ((,fbody () ,@body))
         (declare (inline ,fbody))
         (if *circular-references*
             (,fbody)
             (with-circular-references ()
               (recursive-circular-register :root ,object)
               (,fbody)))))))


(defmacro unwrapping (object &body body)
  "
DO:             Execute BODY, unless a wrapping is occuring, in which
                case it just check that OBJECT already has a handle.

NOTE:           UNWRAPPING returns the handle or compute a new NS
                object from the instance (and sets the handle with it).
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



(defparameter *wrapper-instances* (make-weak-list '()))

(on-save clear-handles
  (mapcar (lambda (wrapper)
              (format *trace-output* "~&clearing a ~A~%" (class-name (class-of wrapper)))
            (setf (handle wrapper) nil))
          (weak-list-list *wrapper-instances*))
  (force-output *trace-output*))

#|on-restore|#
(on-application-did-finish-launching reset-handles
  (dolist (wrapper (weak-list-list *wrapper-instances*))
    (format *trace-output* "~&unwrapping a ~A~%" (class-name (class-of wrapper)))
    (unwrap wrapper))
  (force-output *trace-output*))


#+ccl (defmethod ccl:terminate ((self wrapper))
        (setf (handle self) nil))


(defmethod initialize-instance :after ((self wrapper) &key &allow-other-keys)
  #+ccl (ccl:terminate-when-unreachable self)
  (push self (weak-list-list *wrapper-instances*))
  (if (handle self)
    [(handle self) retain]
    (update-handle self))
  self)


(defgeneric unwrap (wrapper)
  (:documentation "
DO:             Create and initialize the underlying object and bind
                it to the HANDLE of the WRAPPER, unless it's already
                there.

POST:           (not (null (handle wrapper)))

RETURN:         (handle wrapper)

NOTE:           The generic function WRAP builds subclass-of-WRAPPER
                instances from subclass-of-NSObject instances, hence
                the name of UNWRAP.

NOTE:           Subclasses should define a method, calling
                (unwrapping object …).


SEE ALSO:       UNWRAPPING, WRAPPING.
")
  (:method ((wrapper wrapper))
    (unwrapping wrapper
      (or (handle wrapper)
          (progn ;;(cerror "Continue" "Unwrapping an empty wrapper ~S." wrapper)
            (warn "Unwrapping an empty wrapper ~S." wrapper)
            *null*)))))


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


(defgeneric (setf handle) (new-handle wrapper)
  (:documentation "
DO:             Sets the handle of the wrapper.
                If NEW-HANDLE is the same as the old handle, then nothing is done.
                If NEW-HANDLE is nil, the release the old handler if any.
                If NEW-HANDLE is not nil, then it's retained.

RETURN:         NEW-HANDLE.
")
  (:method (new-handle (wrapper wrapper))
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
    new-handle))





(defclass anonymous-wrapper (wrapper)
  ((thunk         :initarg :thunk        :reader anonymous-wrapper-thunk)
   (thunk-source  :initarg :thunk-source :reader anonymous-wrapper-thunk-source)))

(defmethod print-object ((self anonymous-wrapper) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (prin1 (list :handle (handle self)
                 :thunk (anonymous-wrapper-thunk-source self)) stream))
  self)

(defmethod update-handle ((wrapper anonymous-wrapper))
  (let ((thunk (anonymous-wrapper-thunk wrapper)))
    (if (or (functionp thunk)
            (and (symbolp thunk)
                 (fboundp thunk)))
        (setf (handle wrapper) (funcall thunk))   
        (warn "Unwrapping an invalid anonymous wrapper ~S" wrapper))))

(defmethod unwrap ((wrapper anonymous-wrapper))
  (unwrapping wrapper
    (let ((thunk (anonymous-wrapper-thunk wrapper)))
      (if (or (functionp thunk)
              (and (symbolp thunk)
                   (fboundp thunk)))
       (funcall thunk)   
       (warn "Unwrapping an invalid anonymous wrapper ~S" wrapper)))))

(defmacro awrap (&body body)
  (let ((thunk (gensym)))
    `(let ((,thunk (lambda () ,@body)))
       (make-instance 'anonymous-wrapper
           :handle (funcall ,thunk)
           :thunk ,thunk
           :thunk-source (list* 'lambda '() ',body)))))



;;;------------------------------------------------------------
;;; circular structures wrapping.
;;;------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  
 @[NSObject subClass:MclguiReference
            slots:((index  :initform nil
                           :initarg :index
                           :accessor reference-index))]

 (defmethod print-object ((self mclgui-reference) stream)
   (print-unreadable-object (self stream :identity t :type t)
     (format stream "#~D#" (reference-index self)))
   self)

 @[NSObject subClass:MclguiReferenced
            slots:((index  :initform nil
                           :initarg :index
                           :accessor referenced-index
                           :accessor reference-index)
                   (object :initform nil
                           :initarg :object
                           :accessor referenced-object))]

 (defmethod print-object ((self mclgui-referenced) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (format stream "#~D=~S" (reference-index self) (referenced-object self)))
  self)

 );;eval-when


(defun make-reference (&key index)
  (let ((reference [[MclguiReference alloc] init]))
    (setf (reference-index reference) index)
    [reference autorelease]))

(defun make-referenced (&key index object)
  (let ((referenced [[MclguiReferenced alloc] init]))
    (setf (referenced-index referenced) index
          (referenced-object referenced) object)
    [referenced autorelease]))

(defun flatten-circular-reference (object)
  (let ((index (circular-reference object)))
    (if index
      (if (cdr index)
        (make-reference :index (car index))
        (let ((referenced (make-referenced :index (car index) :object object)))
          (setf (cdr index) referenced)
          referenced))
      object)))



;;------------------------------------------------------------
;; wrapping NSObject and objects.
;;------------------------------------------------------------

(defmethod structure ((object t) element-function)
  (declare (ignorable element-function))
  (values))


(defmethod wrap ((object t))
  (cond
    ((nullp object)
     nil)
    (t
     object)))


(defmethod wrap ((object ns:ns-object))
  (make-instance 'wrapper :handle object))


;;------------------------------------------------------------
;; wrapping NSString
;;------------------------------------------------------------

(defmethod wrap ((object ns:ns-string))
  ;; TODO: resolve-circular-reference for long strings.
  (objcl:lisp-string object))


;;------------------------------------------------------------
;; wrapping NSNumber
;;------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +C-ID+            #\@)
  (defconstant +C-CLASS+         #\#)
  (defconstant +C-SEL+           #\:)
  (defconstant +C-CHR+           #\c)
  (defconstant +C-UCHR+          #\C)
  (defconstant +C-SHT+           #\s)
  (defconstant +C-USHT+          #\S)
  (defconstant +C-INT+           #\i)
  (defconstant +C-UINT+          #\I)
  (defconstant +C-LNG+           #\l)
  (defconstant +C-ULNG+          #\L)
  (defconstant +C-LNG-LNG+       #\q)
  (defconstant +C-ULNG-LNG+      #\Q)
  (defconstant +C-FLT+           #\f)
  (defconstant +C-DBL+           #\d)
  (defconstant +C-BFLD+          #\b) ; bitfield.
  (defconstant +C-BOOL+          #\B)
  (defconstant +C-VOID+          #\v)
  (defconstant +C-UNDEF+         #\?)
  (defconstant +C-PTR+           #\^)
  (defconstant +C-CHARPTR+       #\*)
  (defconstant +C-ATOM+          #\%)
  (defconstant +C-ARY-B+         #\[)
  (defconstant +C-ARY-E+         #\])
  (defconstant +C-UNION-B+       #\()
  (defconstant +C-UNION-E+       #\))
  (defconstant +C-STRUCT-B+      #\{)
  (defconstant +C-STRUCT-E+      #\})
  (defconstant +C-VECTOR+        #\!)
  (defconstant +C-CONST+         #\r))


(defmethod wrap ((object ns:ns-number))
  (let ((objctype (aref #+ccl (ccl:%get-cstring [object objCType])
                        #-ccl (error "Decoding [object objCType] is not implemented in ~S"
                                     (lisp-implementation-type))
                        0)))
    (cond
      ((find objctype #.(vector +C-FLT+ +C-DBL+))
       [object doubleValue])
      ((find objctype #.(vector +C-UCHR+ +C-USHT+ +C-UINT+ +C-ULNG+ +C-ULNG-LNG+))
       [object unsignedLongLongValue])
      ((find objctype #.(vector +C-CHR+ +C-SHT+ +C-INT+ +C-LNG+ +C-LNG-LNG+ +C-BOOL+))
       [object longLongValue])
      (t
       (call-next-method)))))

;;------------------------------------------------------------
;; wrapping NSArray
;;------------------------------------------------------------

(defmethod structure ((nsarray ns:ns-array) element)
  (dotimes (i [nsarray count])
    (funcall element i [nsarray objectAtIndex:i])))

(defmethod wrap ((object ns:ns-array))
  (let ((result (make-array [object count])))
    (dotimes (i [object count] result)
      (setf (aref result i) (wrap-resolving-circular-references [object objectAtIndex:i])))))


;;------------------------------------------------------------
;; wrapping NSDictionary
;;------------------------------------------------------------

(defmethod structure ((nsdictionary ns:ns-dictionary) element)
  (do-nsdictionary (key value nsdictionary)
    (funcall element :key key)
    (funcall element :value value)))


(defmethod wrap ((object ns:ns-dictionary))
  "
RETURN:         A fresh hash-table containing the NSDICTIONARY entries.

DO:             Keys that are NSString are converted to keywords,
                other keys are wrapped; the values are converted to
                lisp type if possible, or else left as foreign types.

"
  (let ((result (make-hash-table)) ; !!!!
        ;; Since we map all the Objective-C key object to a lisp object,
        ;; it doesn't matter what test function is used in the lisp hash-table.
        ;; Hash-table test functions cannot be customized on lisp
        ;; object to match isEqual: on the wrapped nsobjects.
        (keyword (load-time-value (find-package "KEYWORD"))))
    (do-nsdictionary (key value object result)
      (let ((lisp-key   (if [key isKindOfClass:(oclo:@class "NSString")]
                            (intern (objcl:lisp-string key) keyword)
                            (wrap-resolving-circular-references key)))
            (lisp-value (wrap-resolving-circular-references value)))
        (setf (gethash lisp-key result) lisp-value)))))



;;------------------------------------------------------------
;;------------------------------------------------------------

(defmethod unwrap ((item symbol))
  (unwrapping item
    (objcl:objcl-string (symbol-name item))))

(defmethod unwrap ((item string))
  (unwrapping item
    (objcl:objcl-string item)))

(defmethod unwrap ((item real))
  (unwrapping item
    [NSNumber numberWithDouble:(coerce item 'double-float)]))

(defmethod unwrap ((item single-float))
  (unwrapping item
    [NSNumber numberWithFloat:(coerce item 'single-float)]))

(defmethod unwrap ((item integer))
  (unwrapping item
    [NSNumber numberWithLongLong:item]))

(defmethod unwrap ((seq cons))
  (unwrapping seq
    (loop
      :with nsarray = [NSMutableArray arrayWithCapacity:(length seq)]
      :for element :in seq
      :do [nsarray addObject:(unwrap element)]
      :finally (return nsarray))))

(defmethod unwrap ((seq vector))
  (unwrapping seq
    (loop
      :with nsarray = [NSMutableArray arrayWithCapacity:(length seq)]
      :for element :across seq
      :do [nsarray addObject:(unwrap element)]
      :finally (return nsarray))))

(defmethod unwrap ((dict hash-table))
  #-(and)
  (let ((objects '())
        (keys    '()))
    (maphash (lambda (k v) (push k keys) (push v objects)) dict)
    (unwrapping dict
      [NSDictionary
       dictionaryWithObjects: (unwrap objects)
       forKeys: (unwrap keys)]))
  (unwrapping dict
    (let ((nsdict [NSMutableDictionary dictionaryWithCapacity:(hash-table-count dict)]))
      (maphash (lambda (k v) [nsdict setObject:(unwrap v) forKey:(unwrap k)]) dict)
      nsdict)))

(defmethod unwrap ((self ns:ns-object))
  self)

(defun unwrap-plist (plist)
  (loop
    :for (k v) :on plist :by (function cddr)
    :collect k :into keys
    :collect v :into objects
    :finally (return (unwrapping plist
                       [NSDictionary
                        dictionaryWithObjects: (unwrap objects)
                        forKeys: (unwrap keys)]))))


(defun nsarray-to-list (nsarray)
  (wrapping nsarray
   (let ((result '()))
     (do-nsarray (element nsarray (nreverse result))
       (push (wrap element) result)))))

(defun list-to-nsarray (list)
  (if list
    (unwrap list)
    [NSMutableArray arrayWithCapacity:0]))


;;;; THE END ;;;;
