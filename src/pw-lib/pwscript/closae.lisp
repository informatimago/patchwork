;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               closae.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Simulated AppleEvent CLOS layer.
;;;;
;;;;    We implement a minimalistic AppleEvent CLOS API, used by the
;;;;    rest of the Patchwork application
;;;;
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-03-02 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2014 - 2014
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

(in-package "CLOSAE")

(defclass appleevent ()
  ((data :accessor appleevent-data)))

(defmethod initialize-instance (self &key descrecptr &allow-other-keys)
  (or descrecptr (call-next-method)))

(defmethod getparam ((self appleevent) keyword &optional (type :|****|))
  (aedesc-object (assoc keyword (appleevent-data self))))

(defmethod putparam ((self appleevent) keyword data)
  (let ((entry (assoc keyword (appleevent-data self))))
    (if entry
        (setf (cdr entry) (asaedesc data))
        (push (cons keyword (asaedesc data)) (appleevent-data self)))))


(defclass aedesc ()
  ((object :initarg :object :accessor aedesc-object)
   (type   :initarg :type   :accessor aedesc-type)))

(defmethod getdescrecptr ((desc aedesc))
  desc)

(defmethod asaedesc (object &optional type)
  (make-instance 'aedesc :object object :type type))


(defclass objectspecifier (standard-object)
  ((class     :reader getclass
              :writer setclass)
   (container :reader getcontainer
              :writer setcontainer)
   (form      :reader getform
              :writer setform)
   (data      :reader getdata
              :writer setdata)))


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
  `(let (,@(mapcar (lambda (var) `(,var (make-instance 'aedesc))) vars))
     ,@body))





(defun create-self-target (&optional the-desc)
  "Create an appleevent target to the current application.

the-desc      An AEDesc record.

Return value: the-desc
"
  the-desc)


(defun create-appleevent (the-desc class id target
                          &key (return-id #$kAutoGenerateReturnID)
                            (transaction-id #$kAnyTransactionID))
  "Create an apple event

the-desc         An AEDesc record
class            An OSType. e.g. :|aevt|
id               An OSType. e.g. :|odoc|
target           An AEDesc record as initialized by e.g. choose-appleevent-target
return-id        an integer.  You'll ususally want the default.
transaction-id   an integer.  IBID.

Returned value:  the-desc
"

  (let ((event (make-instance 'appleevent)))
    (putparam event #$keyEventClassAttr class)
    (putparam event #$keyEventIDAttr id)
    (putparam event #$keyAddressAttr target)
    (putparam event #$keyReturnIDAttr return-id)
    (putparam event #$TransactionIDAttr transaction-id))
  (setf (aedesc-object the-desc) event)
  the-desc)



