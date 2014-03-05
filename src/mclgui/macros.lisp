;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               macro.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    MCLGUI internal macros.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-09 <PJB> Created.
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

(defmacro niy (item &rest vars)
  `(format *trace-output* "~&(~40A (~S~:{ (~S ~S)~}))~%"
           "not implemented yet:"
           ',item (mapcar (lambda (var) (list var (type-of var)))
                          (list ,@vars))))

(defmacro uiwarn (control-string &rest args)
  `(format *trace-output* "~&(~?)~%" ',control-string' (list ,@args)))



(define-modify-macro appendf (&rest args) 
  append "Append onto list")

(define-modify-macro nconcf (&rest args) 
  nconc "Nconc onto list")

(declaim (inline delete/swapped-arguments))
(defun delete/swapped-arguments (sequence item &rest keyword-arguments)
  (apply #'delete item sequence keyword-arguments))
(define-modify-macro deletef (item &rest remove-keywords)
  delete/swapped-arguments
  "Modify-macro for DELETE. Sets place designated by the first argument to
the result of calling DELETE with ITEM, place, and the REMOVE-KEYWORDS.")


(defmacro add-to-list (list-place element)
    "
DO:             Destructively add the ELEMENT to the LIST-PLACE in the
                last position.
"
  `(appendf ,list-place (list ,element)))


(defmacro delete-from-list (list-place element)
  "
DO:             Destructuvely delete from the list stored in place
                LIST-PLACE the ELEMENT.
"
  `(deletef ,list-place ,element))

(defmacro insert-into-list (&whole whole &environment env
                            list-place position element)
  "
DO:             Destructively insert into the LIST-PLACE the ELEMENT
                in the given position.

POSITION:       0 means insert in front of the list.
                n means after the n-th element.
"
  (multiple-value-bind (dummies vals new setter getter) (get-setf-expansion list-place env)
    (when (cdr new) (error "Can't expand ~S" whole))
    (let ((vposition (gensym))
          (velement  (gensym))
          (vplace    (car new)))
      `(let* (,@(mapcar #'list dummies vals) (,vplace ,getter)
                (,vposition ,position)
                (,velement  ,element))
         (if (zerop ,vposition)
             (push ,velement ,vplace)
             (push ,velement (cdr (or (nthcdr (1- ,vposition) ,vplace)
                                      (last ,vplace)))))
         ,setter))))



(defmacro dovector ((var vector &optional result) &body body)
  (let ((vvector (gensym "vector"))
        (vindex  (gensym "index"))
        (vlength (gensym "length")))
    `(block nil
       (let* ((,vvector ,vector)
              (,vlength (length ,vvector))
              (,vindex  -1))
         (tagbody
            (go :test)
          :loop
            (let ((,var (aref ,vvector ,vindex)))
              ,@body)
          :test
            (incf ,vindex)
            (if (< ,vindex ,vlength)
                (go :loop))
            (return ,result))))))




(declaim (declaration stepper))
(defvar *step-mode* :run)

(defun object-identity (object)
  "
RETURN:         A string containing the object identity as printed by
                PRINT-UNREADABLE-OBJECT.
"
  (declare (stepper disable))
  (let ((*step-mode* :run)
        (*print-readably* nil))
    (let ((ident
           (with-output-to-string (stream)
             (print-unreadable-object (object stream :type nil :identity t)))))
      (subseq ident 3 (1- (length ident))))))


(defun call-print-parseable-object (object stream type identity thunk)
  "
SEE:            PRINT-PARSEABLE-OBJECT
"
  (declare (stepper disable))
  (let ((*step-mode* :run))
    (if *print-readably*
        (error 'print-not-readable :object object)
        (progn
          (format stream "~S"
                  (append (when type
                            (list (class-name (class-of object))))
                          (funcall thunk object)
                          (when identity
                            (list (object-identity object))))) 
          object))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun extract-slots (ovar slots)
    "
SEE:            PRINT-PARSEABLE-OBJECT
RETURN:         A form building a plist of slot values.
"
    (cons 'list
          (loop
            :for slot :in slots
            :collect  (if (symbolp slot)
                          (intern (symbol-name slot) "KEYWORD")
                          `(quote ,(first slot)))
            :collect  (if (symbolp slot)
                        `(ignore-errors (slot-value ,ovar ',slot))
                        `(ignore-errors ,(second slot)))))))


(defmacro print-parseable-object ((object stream &key (type t) identity) &rest slots)
  "

DO:             Prints on the STREAM the object as a list.  If all the
                objects printed inside it are printed readably or with
                PRINT-PARSEABLE-OBJECT, then that list should be
                readable, at least with *READ-SUPPRESS* set to T.

OBJECT:         Either a variable bound to the object to be printed,
                or a binding list (VARNAME OBJECT-EXPRESSION), in
                which case the VARNAME is bound to the
                OBJECT-EXPRESSION during the evaluation of the SLOTS.

STREAM:         The output stream where the object is printed to.

TYPE:           If true, the class-name of the OBJECT is printed as
                first element of the list.

IDENTITY:       If true, the object identity is printed as a string in
                the last position of the list.

SLOTS:          A list of either a symbol naming the slot, or a list
                (name expression), name being included quoted in the
                list, and the expression being evalauted to obtain the
                value.

RETURN:         The object that bas been printed (so that you can use
                it in tail position in PRINT-OBJECT conformingly).

"
  `(locally (declare (stepper disable))
     ,(if (symbolp object)
         `(call-print-parseable-object ,object ,stream ,type ,identity
                                       (lambda (,object)
                                         (declare (ignorable ,object) (stepper disable))
                                         ,(extract-slots object slots)))
         (destructuring-bind (ovar oval) object
           `(let ((,ovar ,oval))
              (call-print-parseable-object ,ovar ,stream ,type ,identity
                                           (lambda (,ovar)
                                             (declare (ignorable ,ovar) (stepper disable))
                                             ,(extract-slots object slots))))))))





(define-condition simple-program-error (simple-error program-error)
  ())

(defmacro on-main-thread (&whole whole form &key (wait t))
  "
FORM:   Should be an Objective-C message send with zero or one argument.
        Examples: [view drawRect:rect]
                 [super drawRect:rect]
                 (objc:send view :draw-rect rect)
                 (objc:objc-message-send-super :draw-rect rect)

WAIT:   Whether we must wait for the message to return from the main
        thread.  Default is T, to wait.

RETURN: A form sending
        performSelectorOnMainThread:withObject:waitUntilDone: message
        to the recipient in the FORM, with the selector of the message
        in the FORM, and the argument object in the FORM. 

"
  (let ((varg (gensym)))
    (flet ((objcmsg (message)
             (cond
               ((keywordp message)
                (oclo:lisp-to-objc-message (list message)))
               ((and (listp message)
                     (eq 'quote (first message))
                     (symbolp (second message)))
                (oclo:lisp-to-objc-message (list (second message))))
               (t
                (check-type message (or keyword
                                        (cons symbol null) ; ???
                                        (cons symbol (cons symbol null)))))))
           (objarg (argument)
             (if (null argument)
                 '*null*
                 `(let ((,varg ,argument))
                   (if (numberp ,varg)
                       (ccl:%int-to-ptr ,varg)
                       ,varg)))))
      (cond
        ((and (listp form)
              (<= 3 (length form) 4)
              (eq 'objc:send (first form)))
         (destructuring-bind (send recipient message &optional argument) form
           (declare (ignore send))
           ;; TODO: eval once arguments!
           `(progn (format-trace "performSelectorOnMainThread" ',recipient ,message ,argument ,wait)
                   [,recipient performSelectorOnMainThread: (oclo:selector ,(objcmsg message))
                                withObject: ,(objarg argument)
                                waitUntilDone: ,wait])))
        ((and (listp form)
              (<= 2 (length form) 3)
              (eq 'objc:objc-message-send-super (first form)))
         (destructuring-bind (send message &optional argument) form
           (declare (ignore send))
           ;; TODO: eval once arguments!
           `(progn (format-trace "performSelectorOnMainThread" 'super ,message ,argument ,wait)
                   [super performSelectorOnMainThread: (oclo:selector ,(objcmsg message))
                           withObject: ,(objarg argument)
                           waitUntilDone: ,wait])))
        (t
         (error 'simple-program-error
                :format-control "The ~S form in ~S must be an Objective-C message send with a single argument."
                :format-arguments (list form whole)))))))



(defgeneric copy-object-from (dst src)
  (:documentation "
DO:             Perform a deep copy of the slots of the SRC object to
                the DST object.  Methods are usually specialized  only
                for DST and SRC of the same class.

DST:            An instance.

SRC:            An instance.

RETURN:         DST
"))


;;;; THE END ;;;;
