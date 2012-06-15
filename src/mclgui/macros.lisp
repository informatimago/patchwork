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

(defmacro niy (item &rest vars)
  `(locally
     (warn "(~S ~{~S~^ ~}) is not implemented yet" ',item (list ,@vars))))

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





(defun object-identity (object)
  "
RETURN:         A string containing the object identity as printed by
                PRINT-UNREADABLE-OBJECT.
"
  (let ((*print-readably* nil))
    (string-trim "#< >"
     (with-output-to-string (stream)
       (print-unreadable-object (object stream :type nil :identity t))))))


(defun call-print-parseable-object (object stream type identity thunk)
  "
SEE:            PRINT-PARSEABLE-OBJECT
"
  (if *print-readably*
      (error 'print-not-readable :object object)
      (progn
        (format stream "~S"
                (append (when type
                          (list (class-name (class-of object))))
                        (funcall thunk object)
                        (when identity
                          (list (object-identity object))))) 
        object)))


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
                          `(slot-value ,ovar ',slot)
                          (second slot))))))


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
  
  (if (symbolp object)
      `(call-print-parseable-object ,object ,stream ,type ,identity
                                    (lambda (,object)
                                      (declare (ignorable ,object))
                                      ,(extract-slots object slots)))
      (destructuring-bind (ovar oval) object
        `(let ((,ovar ,oval))
           (call-print-parseable-object ,ovar ,stream ,type ,identity
                                        (lambda (,ovar)
                                          (declare (ignorable ,ovar))
                                          ,(extract-slots object slots)))))))



;;;; THE END ;;;;
