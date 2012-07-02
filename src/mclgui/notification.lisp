;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               notification.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Converts a NSNotifications into a CLOS object.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-06-16 <PJB> Created.
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


(defclass notification ()
  ((name      :initarg :name      :initform nil :reader notification-name
              :type string :documentation "The name of the notification.")
   (nsobject  :initarg :nsobject  :initform nil :reader notification-nsobject
              :documentation "The NSObject of the notification (can be nullp).")
   (user-info :initarg :user-info :initform nil :reader notification-user-info
              :documentation "A P-list converted from the userInfo NSDictionary.
Keys are converted to keywords; the values are converted to lisp type if possible,
or else left as foreign types.")))



;; (defun ascii-char (code)
;;   "
;; RETURN: The character with the given ASCII code.
;; "
;;   (if (<= 32 code 126)
;;       (aref " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~" (- code 32))
;;       (code-char code)))

;; (defvar *wrap-objects* nil)


(defun wrap-nsnotification (nsnotification)
  "
RETURN:         A new NOTIFICATION instance filled with data from the
                NSNOTIFICATION.
"
  (make-instance 'notification
      :name (objcl:lisp-string [nsnotification name])
      :nsobject [nsnotification object]
      :user-info (wrap-nsdictionary [nsnotification userInfo])))




;;;; END ;;;;
