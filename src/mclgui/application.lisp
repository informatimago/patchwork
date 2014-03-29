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
(objcl:enable-objcl-reader-macros)


(defclass application (wrapper #+ccl ccl::application)
  ())

(defclass lisp-development-system (application #+ccl ccl::lisp-development-system)
  ())


(defmethod update-handle ((self application))
  (setf (handle self) [NSApplication sharedApplication]))


(defmethod unwrap ((self application))
  (unwrapping self
    (or (handle self) (update-handle self))))


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


(defmethod view-key-event-handler ((application application) key)
  "
The generic function VIEW-KEY-EVENT-HANDLER is called with
*APPLICATION* as the first argument when there are no active windows
and the user presses a key on the keyboard.  The method for
application sounds a beeps.

APPLICATION:    The application.  MCL standard event handling always
                uses the value of *APPLICATION*.

KEY:            The current keystroke character.
"
  (declare (ignore key))
  (ed-beep))


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
                of the constant DEFAULT-APPL-CREATOR).

APPLICATION:    The application.  MCL standard event handling always
                uses the value of *APPLICATION*.
")
  (:method ((application application))
    default-appl-creator))


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
  (:method ((application t) form)
    (let ((evaluator [[MclguiEvaluator alloc] init]))
      (setf (evaluator-thunk evaluator)
            (typecase form
              ((or symbol cl:function) form)
              (otherwise               (lambda () (eval form)))))
      (on-main-thread [evaluator evaluate])
      [evaluator autorelease]))
  (:method ((application lisp-development-system) form)
    (declare (ignorable form))
    ;; TODO: see how to integrate with ccl::lisp-development-system
    (call-next-method)))



(defun initialize/application ()
  (setf *application*
        #+ccl (setf ccl:*application*
                    (if ccl:*application*
                        (change-class ccl:*application*
                                      (if (typep ccl:*application* 'ccl::lisp-development-system)
                                          'lisp-development-system
                                          'application))
                        (make-instance 'application)))
        #-ccl (make-instance 'application))
  (values))

;;;; THE END ;;;;
