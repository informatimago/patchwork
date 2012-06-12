;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               objc-readtable.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Reader macros.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-06-02 <PJB> Forked.
;;;;BUGS
;;;;LEGAL
;;;;    LLGPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
;;;;    Copyright (C) 2002-2009 Clozure Associates
;;;;    Parts of this file were part of Clozure CL.  
;;;;
;;;;    Clozure CL is licensed under the terms of the Lisp Lesser GNU
;;;;    Public License , known as the LLGPL and distributed with
;;;;    Clozure CL as the file "LICENSE".  The LLGPL consists of a
;;;;    preamble and the LGPL, which is distributed with Clozure CL as
;;;;    the file "LGPL".  Where these conflict, the preamble takes
;;;;    precedence.  
;;;;
;;;;    Clozure CL is referenced in the preamble as the "LIBRARY."
;;;;
;;;;    The LLGPL is also available online at
;;;;    http://opensource.franz.com/preamble.html
;;;;    
;;;;    This library is licenced under the Lisp Lesser General Public
;;;;    License.
;;;;    
;;;;    This library is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later
;;;;    version.
;;;;    
;;;;    This library is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU Lesser General Public License for more
;;;;    details.
;;;;    
;;;;    You should have received a copy of the GNU Lesser General
;;;;    Public License along with this library; if not, write to the
;;;;    Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************

(in-package "OBJC-BRIDGE")


(defun left-bracket-reader-macro (stream char)
  "
We use the convention that [:super ....] denotes a send to the
defining object's superclass's method, and that a return value
specification of the form (:-> ... x) indicates a message send
that returns a structure (by reference) via the pointer x.
"
  (declare (ignore char))
  (let* ((tail (read-delimited-list #\] stream))
         (structptr nil))
    (unless *read-suppress*
      (let* ((return (car (last tail))))
        (when (and (consp return) (eq (car return) :->))
          (rplaca (last tail) :void)
          (setq structptr (car (last return)))))
      (if (eq (car tail) :super)
          (if structptr
              `(objc-message-send-super-stret ,structptr (super) ,@(cdr tail))
              `(objc-message-send-super (super) ,@(cdr tail)))
          (if structptr
              `(objc-message-send-stret ,structptr ,@tail)
              `(objc-message-send ,@tail))))))


;;; String might be stack allocated; make a copy before complaining
;;; about it.
(defun check-objc-message-name (string)
  (let* ((initial-p t)
         (colon nil)
         (lastch nil))
    (dotimes (i (length string))
      (let* ((ch (char string i)))
        (if (eql ch #\:)
            (setf initial-p t colon t)
            (progn
              (if (and initial-p (digit-char-p ch 10))
                  (error "Digit ~d not allowed at position ~d of ObjC message name ~s."
                         ch i (copy-seq string)))
              (setf initial-p nil)))
        (setf lastch ch)))
    (when (and colon
               (not (eql lastch #\:)))
      (error "ObjC message name ~s contains colons, but last character is not a colon"
             (copy-seq string)))))


(defun sharp-slash-reader-macro (stream subchar numarg)
  (declare (ignorable subchar numarg))
  (let* ((token (make-array 16 :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop
      (let* ((char (read-char stream nil nil)))
        (if (or (eql char #\:)
                (eql char #\_)
                (and char (digit-char-p char 36)))
            (vector-push-extend char token)
            (progn
              (when char
                (unread-char char stream))
              (return)))))
    (unless *read-suppress*
      (unless (> (length token) 0)
        (signal-reader-error stream "Invalid token after #/."))
      (check-objc-message-name token)
      (intern token "NSFUN"))))


(defun sharp-at-reader-macro (stream subchar numarg)
  (declare (ignore subchar numarg))
  (let* ((string (read stream)))
    (unless *read-suppress*
      (check-type string string)
      `(@ ,string))))


(defun set-objc-reader-macros (*readtable*)
  (set-syntax-from-char #\] #\))
  (set-macro-character #\[ 'left-bracket-reader-macro nil)
  (set-dispatch-macro-character #\# #\/ 'sharp-slash-reader-macro)
  (set-dispatch-macro-character #\# #\@ 'sharp-at-reader-macro))


(defun reset-objc-reader-macros (*readtable*)
  (set-syntax-from-char #\] #\a)
  (set-macro-character #\[ nil nil)
  (set-dispatch-macro-character #\# #\/ nil)
  (set-dispatch-macro-character #\# #\@ nil))


(defmacro enable-objc-reader-macros ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (set-objc-reader-macros *readtable*)))


(defmacro disable-objc-reader-macros ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (reset-objc-reader-macros *readtable*)))


;;;; THE END ;;;;
