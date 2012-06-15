;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               application.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Application classes.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-18 <PJB> Created.
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


(defclass application (wrapper)
  ())

(defclass lisp-development-system (application)
  ())


(defgeneric application-error (application condition error-pointer)
  (:documentation "
The generic function APPLICATION-ERROR is called whenever a condition
is signaled that has no handler.  The method for APPLICATION quits the
application. The method for LISP-DEVELOPMENT-SYSTEM enters a
BREAK-LOOP.

You can customize your error handling by defining a subclass of
application and setting *application* to an instance of your
class. User APPLICATION-ERROR methods should have a non-local exit,
because  if APPLICATION-ERROR returns, MCL calls it again with a
condition so that it may not return.  However, if it returns from that
call, MCL throws to the toplevel.

APPLICATION:    The application. MCL standard event handling always
                uses the value of *APPLICATION*.

CONDITION:      The error condition.

ERROR-POINTER:  An integer representing the address of the stack frame
                of the function that signaled the error.  The method
                specialized on lisp-development-system uses this
                address to determine the name of the function and uses
                this address as an input to the stack backtrace
                facility.
")
  (:method ((application application) condition error-pointer)
    (declare (ignore condition error-pointer))
    (niy application-error))
  (:method ((application lisp-development-system) condition error-pointer)
    (declare (ignore error-pointer))
    (invoke-debugger condition)))


(defgeneric application-overwrite-dialog (application filename prompt)
  (:documentation "
The generic function APPLICATION-OVERWRITE-DIALOG displays a
dialog when there is an attempt to overwrite an existing file.  The dialog
asks whether to replace the file or choose a new filename.

APPLICATION:    The application.  MCL standard event handling always
                uses the value of *APPLICATION*.

FILENAME:       A pathname or string that specifies an existing file.

PROMPT:         The prompt message.
")
  (:method ((application application) filename prompt)
    (declare (ignore filename prompt))
    (niy application-overwrite-dialog)))


(defgeneric find-edit-menu (application)
  (:documentation "
RETURN:         the first menu in the menu bar containing the Command-X.

APPLICATION:    The application.  MCL standard event handling always
                uses the value of *APPLICATION*.
")
  (:method ((application application))
    (niy find-edit-menu)))


(defgeneric view-key-event-handler (application key)
  (:documentation "
The generic function VIEW-KEY-EVENT-HANDLER is called with
*APPLICATION* as the first argument when there are no active windows
and the user presses a key on the keyboard.  The method for
application sounds a beeps.

APPLICATION:    The application.  MCL standard event handling always
                uses the value of *APPLICATION*.

KEY:            The current keystroke character.
")
  (:method ((application application) key)
    (declare (ignore key))
    (ed-beep)))


(defgeneric application-name (application)
  (:documentation "
RETURN:         The name of the application (a string). The default
                value is NIL.

APPLICATION:    The application.  MCL standard event handling always
                uses the value of *APPLICATION*.
")
  (:method ((application application))
    nil))


(defgeneric application-file-creator (application)
  (:documentation "
RETURN:         a four-character string or symbol for Finder file
                creator type.  The default value is :|????| (the value
                of the constant DEFAULT-APPLICATION-CREATOR).

APPLICATION:    The application.  MCL standard event handling always
                uses the value of *APPLICATION*.
")
  (:method ((application application))
    default-application-creator))


(defgeneric application-about-view (application)
  (:documentation "
RETURN:         A view instance containing dialog items to display in
                the About dialog; the mandatory MCL redistribution
                notice is placed below this view to make the About
                dialog.  The default value is a static text item with
                the application's name.

APPLICATION:    The application.  MCL standard event handling always
                uses the value of *APPLICATION*.
")
  (:method ((application application))
    (niy application-about-view)))


(defgeneric application-about-dialog (application)
  (:documentation "
RETURN:         A view instance containing dialog items to display in
                the About dialog; the mandatory MCL redistribution
                notice is placed below this view to make the About
                dialog.  The default value is a static text item with
                the application's name.

APPLICATION:    The application.  MCL standard event handling always
                uses the value of *APPLICATION*.
")
  (:method ((application application))
    (niy application-about-view)))


(defgeneric application-suspend-event-handler (application)
  (:documentation "
This function is called with the value of *APPLICATION* when MCL is
suspended.  The application method converts the scrap, deactivates
windows, and hides windoids if *HIDE-WINDOIDS-ON-SUSPEND* is true.

APPLICATION:    The application.  MCL standard event handling always
                uses the value of *APPLICATION*.
")
  (:method ((application application))
    (niy application-suspend-event-handler)))


(defgeneric application-resume-event-handler (application)
  (:documentation "
This function is called with the value of *APPLICATION* when MCL is
resumed. The application method converts the scrap, reactivates the
front window, and shows hidden windoids if *HIDE-WINDOIDS-ON-SUSPEND*
is true.

APPLICATION:    The application.  MCL standard event handling always
                uses the value of *APPLICATION*.
")
  (:method ((application application))
    (niy application-resume-event-handler)))


(defgeneric application-eval-enqueue (application form)
  (:documentation "
This function is called with the value of *APPLICATION* by the
EVAL-ENQUEUE function.  The application method calls funcall (for a
function or symbol) or eval (for a list) on form.  The
LISP-DEVELOPMENT-SYSTEM method adds form to the eval queue of the
frontmost active listener if one exists, otherwise invokes
CALL-NEXT-METHOD.

APPLICATION:    The application.  MCL standard event handling always
                uses the value of *APPLICATION*.

FORM:           A symbol, function or lisp form.
")
  (:method ((application application) form)
    (typecase form
      ((or symbol function) (funcall form))
      (otherwise (eval form))))
  (:method ((application lisp-development-system) form)
    (niy application-eval-enqueue application form)
    (call-next-method)))



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
         (error (make-condition 'appleevent-error :oserr ,errsym 
                                :error-string ,error-string))))))


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
         (error (make-condition 'appleevent-error :oserr ,errsym))))))


(defmacro with-aedescs (vars &body body)
  "
The WITH-AEDESCS macro creates a temporary record of type AEDesc for
the extent of the macro.  It is similar to the macro rlet.  It wraps
body within an UNWIND-PROTECT, so that no matter how body is exited,
WITH-AEDESCS disposes of all its temporary records in the correct way.
If the data handle has anything in it, WITH-AEDESCS calls
#_AEDisposeDesc. 

Thus any memory allocated by the Apple Event Manager is properly
disposed of. 

If you have a need for an AEDesc record with indefinite extent, you must
use MAKE-RECORD.  When you want to dispose of the record, you must
explicitly call #_AEDisposeDesc, then dispose-record. 
"
  (niy with-aedescs vars body)
  `(let ,vars
     ,@body))


(defun check-required-params (error-string theAppleEvent)
  "
The CHECK-REQUIRED-PARAMS function uses the Apple Event Manager
to check whether all required parameters of the Apple event appleevent
have been extracted.  If a parameter has been missed, the APPLEEVENT-ERROR
condition is signaled with :oserr #$AEParamMissed and
:error-string error-string.
"
  (niy check-required-params error-string theAppleEvent)
  #-(and)
  (rlet ((missed-keyword :ostype)
         (actual-type :ostype)
         (actual-size :signed-long))
        (let ((myerr (#_AEGetAttributePtr theAppleEvent #$keyMissedKeywordAttr
                                          #$typeWildCard actual-type missed-keyword 4 actual-size)))
          (when (eq myerr $noErr)           ; missed a parameter!
            (error (make-condition 'appleevent-error :oserr #$errAEParamMissed 
                                   :error-string error-string))))))



(defun appleevent-idle (event sleeptime mousergn)
  "
The appleevent-idle Pascal function should be specified whenever
the Apple Event Manager asks for a function to call while it is waiting (for
example, in calls to #_AEInteractWithUser). It should never be called
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
  (niy ae-get-attribute-longinteger the-desc keyword errorp)
  ;; (rlet ((buffer :signed-long)
  ;;        (typecode :desctype)
  ;;        (actualsize :size))
  ;;       (ae-errorp-handler
  ;;        errorp
  ;;        (ae-error (#_aegetattributeptr the-desc keyword
  ;;                                       #$typeLongInteger typecode buffer
  ;;                                       (record-length :unsigned-long) actualsize))
  ;;        (%get-signed-long buffer)))
  )


(defvar %queued-reply-handlers% (make-hash-table :test #'eql))

(defun install-queued-reply-handler (appleevent-or-id function &optional (refcon nil))
  "
DO:             installs a handler for a queued reply.

APPLEEVENT-OR-ID:
                Either a return ID number or the originating Apple event
                from which a return ID number can be extracted.
                function A function to be called when the reply comes back. This

FUNCTION:       Should be a normal Apple event handler as
                described in “Defining new Apple events” on page 404.

REFCON:         An optional reference identifier, which can be any MCL
                object. It identifies the specific installation of a handler.
"
  ;; (when (macptrp appleevent-or-id)
  ;;   (setq appleevent-or-id
  ;;         (ae-get-attribute-longinteger appleevent-or-id #$keyReturnIDAttr)))
  (setf (gethash appleevent-or-id %queued-reply-handlers%)
        (cons function refcon)))


(defmethod no-queued-reply-handler ((application application) theAppleEvent reply handlerRefcon)
  "

The default method of the generic function NO-QUEUED-REPLY-HANDLER
signals the appleevent-error condition with :oserr
#$AEEventNotHandled.

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
"
  (declare (ignore reply handlerRefcon))
  (let ((return-id (ae-get-attribute-longinteger theAppleEvent '\#$keyReturnIDAttr)))
    (error (make-condition 'appleevent-error :oserr '\#$errAEEventNotHandled
                           :error-string (format nil "No queued reply handler for id: ~d"
                                                 return-id)))))


(defmethod queued-reply-handler ((application application) theAppleEvent reply handlerRefcon)
  "
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
"
  (let* ((return-id (ae-get-attribute-longinteger theAppleEvent '\#$keyReturnIDAttr))
         (handler   (gethash return-id %queued-reply-handlers%)))
    (if handler
      (progn
        (let ((fun    (car handler))
              (refcon (cdr handler)))
          (remhash return-id %queued-reply-handlers%)
          (funcall fun application theAppleEvent reply refcon)))
      (no-queued-reply-handler application theAppleEvent reply handlerRefcon))))





(defun initialize/application ()
  (niy initialize/application)
  (setf *application* (make-instance 'application))
  (install-appleevent-handler :|aevt| :|ansr| #'queued-reply-handler)
  (install-appleevent-handler :|aevt| :|oapp| #'open-application-handler)
  (install-appleevent-handler :|aevt| :|quit| #'quit-application-handler)
  (install-appleevent-handler :|aevt| :|odoc| #'open-documents-handler)
  (install-appleevent-handler :|aevt| :|pdoc| #'print-documents-handler)
  (values))

;;;; THE END ;;;;
