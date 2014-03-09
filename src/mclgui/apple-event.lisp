;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               apple-event.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    XXX
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-12-09 <PJB> Extracted from application.lisp for specific
;;;;                     dependency on CoreServices / Apple Event.
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



;;----------------------------------------------------------------------
;; Standard Apple Events
;;----------------------------------------------------------------------

(defgeneric open-application-handler (application appleevent reply refcon)
  (:documentation "
The generic function open-application-handler handles the Open
Application Apple event. The default method does nothing.

APPLICATION:    The application.  MCL standard event handling always
                uses the value of *APPLICATION*.

APPLEEVENT:     The Apple event, which is an MCL object of type macptr
                and a record of type AEDesc—a record with only two
                fields, a type and a handle.  MCL users generally do
                not have to look at the record structure directly. 

REPLY:          Another Apple event record, provided by the Apple
                Event Manager.  If a reply is required, information
                should be copied into this record using Apple Event
                Manager calls. 

REFCON:         The handler reference constant, which is any Lisp
                object.  When the handler is installed, you have the
                option of specifying some Lisp object that serves to
                distinguish (for instance) two different installations
                of the same handler.  The reference constant is often
                ignored. 
")
  (:method ((application application) appleevent reply refcon)
    (declare (ignore appleevent reply refcon))
    (values)))


(defgeneric quit-application-handler (application appleevent reply refcon)
  (:documentation "
The generic function QUIT-APPLICATION-HANDLER handles the Quit
Application Apple event. The default method quits Macintosh Common
Lisp.

APPLICATION:    The application.  MCL standard event handling always
                uses the value of *APPLICATION*.

APPLEEVENT:     The Apple event, which is an MCL object of type macptr
                and a record of type AEDesc—a record with only two
                fields, a type and a handle.  MCL users generally do
                not have to look at the record structure directly. 

REPLY:          Another Apple event record, provided by the Apple
                Event Manager.  If a reply is required, information
                should be copied into this record using Apple Event
                Manager calls. 

REFCON:         The handler reference constant, which is any Lisp
                object.  When the handler is installed, you have the
                option of specifying some Lisp object that serves to
                distinguish (for instance) two different installations
                of the same handler.  The reference constant is often
                ignored. 
")
  (:method ((application application) appleevent reply refcon)
    (declare (ignore appleevent reply refcon))
    (niy quit-application-handler)
    #+ccl (ccl:quit)))


(defgeneric open-application-document (application filename startup)
  (:documentation "

The generic function OPEN-APPLICATION-DOCUMENT is called by the
OPEN-DOCUMENTS-HANDLER method.  The method for LISP-DEVELOPMENT-SYSTEM
loads files of type :FASL and opens files of type :TEXT for editing;
the method for application does nothing.  You can customize an Apple
event by defining a subclass of application and setting *APPLICATION*
to an instance of your class.

APPLICATION:    The application.  MCL standard event handling always
                uses the value of *APPLICATION*.

FILENAME:       The file to load and open for editing.

STARTUP:        A boolean value that indicates whether the event is
                occurring on startup. If the value is true, this
                function was called upon startup.
")
  (:method ((application application) filename startup)
    (declare (ignore filename startup))
    (values))
  (:method ((application lisp-development-system) filename startup)
    (niy open-application-document application filename startup)))



(defgeneric open-documents-handler (application appleevent reply refcon)
  (:documentation "
The generic function OPEN-DOCUMENTS-HANDLER handles the Open Documents
Apple event. The method for application calls
OPEN-APPLICATION-DOCUMENT on application for each document.

APPLICATION:    The application.  MCL standard event handling always
                uses the value of *APPLICATION*.

APPLEEVENT:     The Apple event, which is an MCL object of type macptr
                and a record of type AEDesc—a record with only two
                fields, a type and a handle.  MCL users generally do
                not have to look at the record structure directly. 

REPLY:          Another Apple event record, provided by the Apple
                Event Manager.  If a reply is required, information
                should be copied into this record using Apple Event
                Manager calls. 

REFCON:         The handler reference constant, which is any Lisp
                object.  When the handler is installed, you have the
                option of specifying some Lisp object that serves to
                distinguish (for instance) two different installations
                of the same handler.  The reference constant is often
                ignored. 
")
  (:method ((application application) appleevent reply refcon)
    (declare (ignore appleevent reply refcon))
    '(for each document do
      (open-application-document application filename startup))
    (niy open-documents-handler)))


(defgeneric print-application-document (application filename startup)
  (:documentation "
The generic function PRINT-APPLICATION-DOCUMENT is called by the
PRINT-DOCUMENT-HANDLER method for application.  The
PRINT-APPLICATION-DOCUMENT method for LISP-DEVELOPMENT-SYSTEM prints
filename; the PRINT-APPLICATION-DOCUMENT method for application does
nothing.  You can customize an Apple event by defining a subclass of
application and setting *APPLICATION* to an instance of your class.

APPLICATION:    The application.  MCL standard event handling always
                uses the value of *APPLICATION*.

FILENAME:       The file to load and open for editing.

STARTUP:        A boolean value that indicates whether the event is
                occurring on startup. If the value is true, this
                function was called upon startup.
")
  (:method ((application application) filename startup)
    (declare (ignore filename startup))
    (values))
  (:method ((application lisp-development-system) filename startup)
    (niy print-application-document application filename startup)))



(defgeneric print-documents-handler (application appleevent reply refcon)
  (:documentation "
The generic function PRINT-DOCUMENTS-HANDLER handles the Print
Documents Apple event.  The method for application calls
PRINT-APPLICATION-DOCUMENT on application for each document.

APPLICATION:    The application.  MCL standard event handling always
                uses the value of *APPLICATION*.

APPLEEVENT:     The Apple event, which is an MCL object of type macptr
                and a record of type AEDesc—a record with only two
                fields, a type and a handle.  MCL users generally do
                not have to look at the record structure directly. 

REPLY:          Another Apple event record, provided by the Apple
                Event Manager.  If a reply is required, information
                should be copied into this record using Apple Event
                Manager calls. 

REFCON:         The handler reference constant, which is any Lisp
                object.  When the handler is installed, you have the
                option of specifying some Lisp object that serves to
                distinguish (for instance) two different installations
                of the same handler.  The reference constant is often
                ignored. 
")
  (:method ((application application) appleevent reply refcon)
    (declare (ignore appleevent reply refcon))
    '(for each document do
      (print-application-document application filename startup))
    (niy print-documents-handler)))




;;----------------------------------------------------------------------
;; Apple Events
;;----------------------------------------------------------------------

(define-condition appleevent-error (error)
  ((oserr                         :initarg :oserr        :reader oserr)
   (error-string :initform nil    :initarg :error-string :reader error-string))
  (:report
   (lambda (c s)
     (format s "(oserr ~d)~@[ - ~a~]" (oserr c) (error-string c))))
  (:documentation "
If an Apple event handler finds an error, it should signal this
condition.  Any MCL errors that occur while handling the Apple event
are automatically caught by Macintosh Common Lisp and handled
appropriately.
"))


(defmacro ae-error-str (error-string &body forms)
  "
This macro simplify calls to the Apple Event Manager by signaling the
APPLEEVENT-ERROR condition if the error code returned by the Apple
Event Manager is not 0 (NoErr).  The value of the call should be the value
of the body of the macro.

The AE-ERROR-STR macro lets you specify an error string.

All calls to the Apple Event Manager should be wrapped in a call to either the
AE-ERROR macro or the AE-ERROR-STR macro.
"
  (let ((errsym (gensym)))
    `(let ((,errsym (progn ,@forms)))
       (unless (zerop ,errsym)
         (error 'appleevent-error :oserr ,errsym :error-string ,error-string)))))


(defmacro ae-error (&body forms)
  "
This macro simplify calls to the Apple Event Manager by signaling the
APPLEEVENT-ERROR condition if the error code returned by the Apple
Event Manager is not 0 (NoErr).  The value of the call should be the value
of the body of the macro.

All calls to the Apple Event Manager should be wrapped in a call to either the
AE-ERROR macro or the AE-ERROR-STR macro.
"
  (let ((errsym (gensym)))
    `(let ((,errsym (progn ,@forms)))
       (unless (zerop ,errsym)
         (error 'appleevent-error :oserr ,errsym)))))


(defmacro with-aedescs (vars &body body)
  "
The WITH-AEDESCS macro creates a temporary record of type AEDesc for
the extent of the macro.  It is similar to the macro rlet.  It wraps
body within an UNWIND-PROTECT, so that no matter how body is exited,
WITH-AEDESCS disposes of all its temporary records in the correct way.
If the data handle has anything in it, WITH-AEDESCS calls
#_|AEDisposeDesc|. 

Thus any memory allocated by the Apple Event Manager is properly
disposed of. 

If you have a need for an AEDesc record with indefinite extent, you must
use MAKE-RECORD.  When you want to dispose of the record, you must
explicitly call #_|AEDisposeDesc|, then dispose-record. 
"
  `(ccl:rlet (,@(mapcar (lambda (var) `(,var :<AED>esc)) vars))
     ,@(mapcar (lambda (var) `(#_|AEInitializeDesc| ,var)) vars)
     (unwind-protect
         (progn ,@body)
       ,@(mapcar (lambda (var) `(#_|AEDisposeDesc| ,var)) vars))))


(defun check-required-params (error-string theAppleEvent)
  "
The CHECK-REQUIRED-PARAMS function uses the Apple Event Manager
to check whether all required parameters of the Apple event appleevent
have been extracted.  If a parameter has been missed, the APPLEEVENT-ERROR
condition is signaled with :oserr #$|AEParamMissed| and
:error-string error-string.
"
  (ccl:rlet ((missed-keyword :<D>esc<T>ype)
             (actual-type    :<D>esc<T>ype)
             (actual-size    :<S>ize))
    (let ((myerr (#_|AEGetAttributePtr| theAppleEvent
                    #$|keyMissedKeywordAttr|
                    #$|typeWildCard|
                    actual-type
                    missed-keyword
                    4
                    actual-size)))
      (when (eq myerr #$|noErr|)           ; missed a parameter!
        (error 'appleevent-error
               :oserr #$|errAEParamMissed|
               :error-string error-string)))))



(defun appleevent-idle (event sleeptime mousergn)
  "
The appleevent-idle Pascal function should be specified whenever
the Apple Event Manager asks for a function to call while it is waiting (for
example, in calls to #_|AEInteractWithUser|). It should never be called
directly, only passed.
"
  (niy appleevent-idle event sleeptime mousergn))


(defvar %appleevent-handlers% (make-hash-table :test #'eq :size 4))


(defun install-appleevent-handler (class id function &optional (refcon nil))
  "
DO:             Install an Apple event handler.

CLASS:          A four-letter keyword denoting the class of the event,
                for example, :|aevt|.

ID:             A four-letter keyword denoting the ID of the event,
                for example, :|odoc|.

REFCON:         An optional reference identifier, which can be any MCL
                object; it identifies the specific installation of a
                handler.
"
  (let ((id-table (gethash class %appleevent-handlers%)))
    (unless id-table
      (setq id-table (make-hash-table :test #'eq :size 1))
      (setf (gethash class %appleevent-handlers%) id-table))
    (setf (gethash id id-table) (cons function refcon))))


(defun deinstall-appleevent-handler (class id)
  "
DO:             Deinstall an Apple event handler.

CLASS:          A four-letter keyword denoting the class of the event,
                for example, :|aevt|.

ID:             A four-letter keyword denoting the ID of the event,
                for example, :|odoc|.
"
  (let ((id-table (gethash class %appleevent-handlers%)))
    (when id-table
      (setf (gethash id id-table) nil))))




(defun ae-get-attribute-longinteger (the-desc keyword &optional (errorp t))
  (ccl:rlet ((buffer     :signed-long)
             (typecode   :<D>esc<T>ype)
             (actualsize :<S>ize))
    (ae-errorp-handler
     errorp
     (ae-error (#_|AEGetAttributePtr| the-desc keyword
                  #$|typeSInt32| typecode buffer
                  (record-length :unsigned-long) actualsize))
     (ccl:%get-signed-long buffer))))



(defvar %queued-reply-handlers% (make-hash-table :test #'eql))

(defun install-queued-reply-handler (appleevent-or-id function &optional (refcon nil))
  "
DO:             installs a handler for a queued reply.

APPLEEVENT-OR-ID:
                Either a return ID number or the originating Apple event
                from which a return ID number can be extracted.

FUNCTION:       A function to be called when the reply comes back.
                This Should be a normal Apple event handler as
                described in “Defining New Apple Events”.

REFCON:         An optional reference identifier, which can be any MCL
                object.  It identifies the specific installation of a handler.
"
  ;; (when (ccl:macptrp appleevent-or-id)
  ;;   (setq appleevent-or-id
  ;;         (ae-get-attribute-longinteger appleevent-or-id #$|keyReturnIDAttr|)))
  (setf (gethash appleevent-or-id %queued-reply-handlers%)
        (cons function refcon)))


(defgeneric no-queued-reply-handler (application theAppleEvent reply handlerRefcon)
  (:documentation "

The default method of the generic function NO-QUEUED-REPLY-HANDLER
signals the appleevent-error condition with :oserr
#$|AEEventNotHandled|.

APPLICATION:    The application, always the value of *application*.

THEAPPLEEVENT:  The Apple event, which is an MCL object of type macptr
                and a record of type AEDesc—a record with only two
                fields, a type and a handle.  MCL users generally do
                not have to look at the record structure directly.

REPLY:          Another Apple event record, provided by the Apple
                Event Manager.  If a reply is required, information
                should be copied into this record using Apple Event
                Manager calls.

HANDLERREFCON:  The handler reference constant, which is any Lisp object.
                When the handler is installed, you have the option of
                specifying some Lisp object that serves to distinguish (for
                instance) two different installations of the same handler.
                The reference constant is often ignored.
")
  (:method ((application application) theAppleEvent reply handlerRefcon)
    (declare (ignore reply handlerRefcon))
    (let ((return-id (ae-get-attribute-longinteger theAppleEvent '#$|keyReturnIDAttr|)))
      (error 'appleevent-error :oserr '#$|errAEEventNotHandled|
             :error-string (format nil "No queued reply handler for id: ~d"
                                   return-id)))))


(defgeneric queued-reply-handler (application theAppleEvent reply handlerRefcon)
  (:documentation "
The generic function QUEUED-REPLY-HANDLER calls the installed reply
handler for the return ID of appleevent.  If there is no applicable reply
handler, it calls NO-QUEUED-REPLY-HANDLER.

APPLICATION:    The application, always the value of *application*.

THEAPPLEEVENT:  The Apple event, which is an MCL object of type macptr
                and a record of type AEDesc—a record with only two
                fields, a type and a handle. MCL users generally do not
                have to look at the record structure directly.
                reply Another Apple event record, provided by the Apple
                Event Manager. If a reply is required, information should
                be copied into this record using Apple Event Manager
                calls.

HANDLERREFCON:  The handler reference constant, which is any Lisp object.
                When the handler is installed, you have the option of
                specifying some Lisp object that serves to distinguish (for
                instance) two different installations of the same handler.
                The reference constant is often ignored.
")
  (:method ((application application) theAppleEvent reply handlerRefcon)
    (let* ((return-id (ae-get-attribute-longinteger theAppleEvent '#$|keyReturnIDAttr|))
           (handler   (gethash return-id %queued-reply-handlers%)))
      (if handler
        (progn
          (let ((fun    (car handler))
                (refcon (cdr handler)))
            (remhash return-id %queued-reply-handlers%)
            (funcall fun application theAppleEvent reply refcon)))
        (no-queued-reply-handler application theAppleEvent reply handlerRefcon)))))



(defun initialize/apple-event ()
  (install-appleevent-handler :|aevt| :|ansr| #'queued-reply-handler)
  (install-appleevent-handler :|aevt| :|oapp| #'open-application-handler)
  (install-appleevent-handler :|aevt| :|quit| #'quit-application-handler)
  (install-appleevent-handler :|aevt| :|odoc| #'open-documents-handler)
  (install-appleevent-handler :|aevt| :|pdoc| #'print-documents-handler)
  (values))

;;;; THE END ;;;;
