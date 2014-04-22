;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               key-handler-mixin.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Key Handler Mixin
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-24 <PJB> Created.
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


(defgeneric allow-tabs-p    (key-handler)
  (:documentation "
The allow-tabs-p generic function returns true if tabs are allowed in
the editable-text dialog item. Otherwise, it returns NIL.
"))


(defgeneric allow-returns-p (key-handler)
  (:documentation "
The generic function allow-returns-p returns true if carriage returns
are allowed in the editable-text dialog item. Otherwise, it returns NIL.
"))



(defgeneric key-handler-list (view)
  (:documentation "
RETURN:         The list of key handlers associated with view.
VIEW:           A simple view or dialog item.
"))


(defgeneric current-key-handler (window)
  (:documentation "
RETURN:        The current key handler of window.
"))


(defgeneric set-current-key-handler (window item &optional select-all)
  (:documentation "
DO:            Set the current key handler of window to item.  If item
               is not already the current key handler and SELECT-ALL
               is true, SET-CURRENT-KEY-HANDLER selects all of the
               WINDOW.

WINDOW:        A window.

ITEM:          A key handler.  If item is not a key handler, the
               function signals an error.

SELECT-ALL:    This parameter determines whether the entire text of
               the key handler is highlighted when it is first
               selected.  The default is T; that is, all the text is
               highlighted and can be manipulated at once.
"))


(defgeneric add-key-handler (view &optional window)
  (:documentation "
The generic function add-key-handler adds a key handler to view. It is
called by install-view-in-window when the view installed is a
subclass of key-handler-mixin. If window has no current key handler,
view becomes the current key handler.

VIEW:           A simple view or dialog item.

WINDOW:         A window to which to add the key handler. The default
                value is (VIEW-WINDOW VIEW).
"))


(defgeneric remove-key-handler (view &optional window)
  (:documentation "
The generic function remove-key-handler removes a key handler from
a window. It is called by the method of remove-view-from-window for
key-handler-mixin.

VIEW:           A simple view or dialog item.

WINDOW:         A window to which to add the key handler. The default
                value is (VIEW-WINDOW VIEW).
"))



(defgeneric change-key-handler (view)
  (:documentation "
The generic function change-key-handler changes the key handler of
view to the next key handler on key-handler-list of view.

VIEW:           A simple view or dialog item.
"))





(defgeneric key-handler-idle (view &optional dialog)
  (:documentation "
The KEY-HANDLER-IDLE generic function is called periodically via the
default WINDOW-NULL-EVENT-HANDLER function to allow a key handler to
blink a cursor or perform other periodic activities.  The method for
FRED-DIALOG-ITEM blinks the insertion point and matches
parentheses.  The method for SIMPLE-VIEW does nothing.

VIEW:           A simple view.

DIALOG:         An argument allowing a dialog to be specified. In
                system-supplied methods, this argument is ignored.

")
  (:method ((view simple-view) &optional dialog)
    (declare (ignore dialog))
    view))



(defclass key-handler-mixin ()
  ((allow-returns :initarg :allow-returns :initform nil :accessor allow-returns-p)
   (allow-tabs    :initarg :allow-tabs    :initform nil :accessor allow-tabs-p))
  (:documentation "
The class key-handler-mixin should be mixed into any class that
handles key events. The class fred-dialog-item includes keyhandler-
mixin.
"))


(defgeneric key-handler-p (object)
  (:documentation "
The KEY-HANDLER-P generic function checks to see whether item is a key
handler.  When KEY-HANDLER-P is called on an instance of a class one
of whose superclasses is KEY-HANDLER-MIXIN, the function returns T
unless the key handler is disabled.  The method for dialog-item
returns NIL.
")
  (:method ((object t))                 nil)
  (:method ((object key-handler-mixin)) t))


(defgeneric exit-key-handler (item next-key-handler)
  (:documentation "
The generic function EXIT-KEY-HANDLER is called when an
EDITABLE-TEXT-DIALOG-ITEM that is the current key handler is about to
be exited.  At this point, it is still the current key handler, but
soon it won’t be.  If the function returns T (as the method for
KEY-HANDLER-MIXIN does), new-KEY-HANDLER is made the new key handler.
If it returns NIL, item remains the current key-handler.

ITEM:            An editable-text dialog item.

NEW-KEY-HANDLER: The editable-text-dialog-item about to be made current.
")
  (:method ((item key-handler-mixin) next-key-handler)
    (declare (ignore next-key-handler))
    t))


(defgeneric enter-key-handler (item last-key-handler)
  (:documentation "
The generic function ENTER-KEY-HANDLER is called when a key handler
such as an editable-text dialog item has just been made current.  The
method for KEY-HANDLER-MIXIN doesn’t do anything; it is a hook on
which you can specialize behavior.  For example, you can set another
dialog item as the current key handler, as in the example.

ITEM:           An editable-text dialog item.

LAST-KEY-HANDLER:
                The previously current editable-text item in the
                dialog.  This is NIL the first time an editable-text
                item is added to a dialog.

")
  (:method ((item key-handler-mixin) last-key-handler)
    (declare (ignore last-key-handler))
    nil))





(defgeneric set-allow-returns (item value)
  (:documentation "
The generic function SET-ALLOW-RETURNS sets whether carriage returns
are allowed in the editable-text dialog item.
")
  (:method ((item key-handler-mixin) value)
    (setf (allow-returns-p item) value)))


(defgeneric set-allow-tabs (item value)
  (:documentation "
The set-allow-tabs generic function sets whether tabs are allowed in
the editable-text dialog item.
")
  (:method ((item key-handler-mixin) value)
    (setf (allow-tabs-p item) value)))


(defgeneric selection-range (item)
  (:method ((item key-handler-mixin))
    (values 0 0))
  (:method ((item null))
    (values 0 0)))


(defgeneric set-selection-range (item &optional pos curpos)
  (:method ((item key-handler-mixin) &optional pos curpos)
    (declare (ignore pos curpos))
    0))



(defmethod dialog-item-disable :before ((item key-handler-mixin))
  (let ((my-dialog (view-window item)))
    (when my-dialog
      (when (eq item (current-key-handler my-dialog))
        (change-key-handler my-dialog))
      (when (eq item (current-key-handler my-dialog)) ;still current, so only one
        (niy dialog-item-disable :before item)#-(and)
        (setf (%get-current-key-handler my-dialog) nil)))))


(defmethod dialog-item-enable :after ((item key-handler-mixin)) ; was basic-editable-text-dialog-item
  (let ((my-dialog (view-window item)))
    (when my-dialog
      (unless (current-key-handler my-dialog)
        (niy dialog-item-enable :after item)#-(and)
        (when (and (member item (%get-key-handler-list my-dialog))
                   (key-handler-p item))
          (set-current-key-handler my-dialog item))))))



(defmethod view-cursor ((item key-handler-mixin) point)
  (declare (ignore point))
  (let ((w (view-window item)))
    (if (and w (eq item (current-key-handler w))) 
      *i-beam-cursor*
      *arrow-cursor*)))

	  

(defmethod install-view-in-window :after ((item key-handler-mixin) dialog)
  (add-key-handler item dialog))


(defmethod remove-view-from-window :before ((item key-handler-mixin))
  (remove-key-handler item))





;;;; THE END ;;;;
