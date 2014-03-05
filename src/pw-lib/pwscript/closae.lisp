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
(objcl:enable-objcl-reader-macros)


(defclass appleevent ()
  ((data :accessor appleevent-data)))

(defmethod initialize-instance ((self appleevent) &key descrecptr &allow-other-keys)
  (or descrecptr (call-next-method)))

(defmethod getparam ((self appleevent) keyword &optional (type :|****|))
  (declare (ignore type))
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


(defclass objectspecifier ()
  ((class     :initarg :class     :reader getclass     :writer setclass)
   (container :initarg :container :reader getcontainer :writer setcontainer)
   (form      :initarg :form      :reader getform      :writer setform)
   (data      :initarg :data      :reader getdata      :writer setdata)))


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
    (putparam event #$keyTransactionIDAttr transaction-id)
    (setf (aedesc-object the-desc) event)
    the-desc))



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


(defmacro ae-error (&body body)
  (let ((verrno (gensym)))
    `(let ((,verrno (progn ,@body)))
       (if (zerop ,verrno)
           ,verrno
           (error 'appleevent-error :oserr ,verrno :error-string "Apple Event Error")))))



(defstruct (ae-event
            (:include event))
  appleevent
  reply
  mode
  priority
  timeout
  idleproc
  filterproc)


(defun aesend (the-appleevent the-reply mode priority timeout idleproc filterproc)
  (ui::post-event (make-ae-event
                   :what ui::app1-evt
                   :message 0
                   :when (truncate (ui::timestamp) (/ ui::+tick-per-second+))
                   :where (ui::nspoint-to-point (ui::get-nspoint [NSEvent mouseLocation]))
                   :modifiers (ui::nsmodifier-to-macmodifier [NSEvent modifierFlags])
                   ;; ---
                   :appleevent the-appleevent
                   :reply the-reply
                   :mode mode
                   :priority priority
                   :timeout timeout
                   :idleproc idleproc
                   :filterproc filterproc))
  0)




(defvar *appleevent-handlers* (make-hash-table :test #'eq :size 41))


(defgeneric symbolicate (thing)
  (:method ((thing integer))
    (intern (map 'string (function code-char)
              (loop :for i :from 24 :downto 0 :by 8
                    :collect (ldb (byte 8 i) thing)))
            "KEYWORD"))
  (:method ((thing string))
    (intern thing "KEYWORD"))
  (:method ((thing keyword))
    thing)
  (:method ((thing symbol))
    (intern (symbol-name thing) "KEYWORD")))


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
  (let* ((class (symbolicate class))
         (id-table (gethash class *appleevent-handlers*)))
    (unless id-table
      (setq id-table (make-hash-table :test #'eq :size 1))
      (setf (gethash class *appleevent-handlers*) id-table))
    (setf (gethash (symbolicate id) id-table) (cons function refcon))))


(defun get-appleevent-handler (class id)
  (let* ((class (symbolicate class))
         (id    (symbolicate id))
         (entry (gethash class *appleevent-handlers*)))
    (if entry
        (let ((entry (gethash id entry)))
          (if entry
              (values (car entry) (cdr entry))
              (get-appleevent-handler class :|****|)))
        (get-appleevent-handler :|****| id))))


(defun process-ae-event (event)
  (typecase event
    (ae-event
     (let* ((ae    (ae-event-appleevent event))
            (class (getparam ae #$keyEventClassAttr))
            (id    (getparam ae #$keyEventIDAttr)))
       (print (list 'apple-event :class (symbolicate class) :id (symbolicate  id)))
       (multiple-value-bind (fun refcon) (get-appleevent-handler class id)
         (when fun
           (print fun)
           (funcall fun *application* ae (ae-event-reply event) refcon)
           (when (ae-event-reply event)
             
             )))
       t))
    (t nil)))


;;;; THE END ;;;;
