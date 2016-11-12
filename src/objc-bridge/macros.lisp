;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               macros.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Macros (and functions/constants used at macroexpand-time) ONLY.
;;;;
;;;;AUTHORS
;;;;MODIFICATIONS
;;;;    2012-06-02 <PJB> Extracted from ccl lib/macros.lisp.
;;;;BUGS
;;;;LEGAL
;;;;    LLGPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
;;;;    Copyright (C) 2009 Clozure Associates
;;;;    Copyright (C) 1994-2001 Digitool, Inc
;;;;
;;;;    Parts of this file were part of Clozure CL.
;;;;
;;;;    Clozure CL is licensed under the terms of the Lisp Lesser GNU Public
;;;;    License , known as the LLGPL and distributed with Clozure CL as the
;;;;    file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;;    which is distributed with Clozure CL as the file "LGPL".  Where these
;;;;    conflict, the preamble takes precedence.
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

;;;; The Collect macro:

;;; Collect-Normal-Expander  --  Internal
;;;
;;;    This function does the real work of macroexpansion for normal collection
;;; macros.  N-Value is the name of the variable which holds the current
;;; value.  Fun is the function which does collection.  Forms is the list of
;;; forms whose values we are supposed to collect.
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun collect-normal-expander (n-value fun forms)
    `(progn
       ,@(mapcar (lambda (form) `(setf ,n-value (,fun ,form ,n-value))) forms)
       ,n-value))

  (defun form-symbol (first &rest others)
    (intern (apply #'concatenate 'simple-base-string (string first) (mapcar #'string others))))

  (define-condition simple-program-error (simple-condition program-error)
    ())



  ;;Some of these macros were stolen from CMUCL.  Sort of ...
  (defmacro iterate (name binds &body body)
    "Iterate Name ({(Var Initial-Value)}*) Declaration* Form*
  This is syntactic sugar for Labels.  It creates a local function Name with
  the specified Vars as its arguments and the Declarations and Forms as its
  body.  This function is then called with the Initial-Values, and the result
  of the call is return from the macro."
    (dolist (x binds)
      (unless (and (listp x)
                   (= (length x) 2))
        (error 'simple-program-error
               :format-control "Malformed iterate variable spec: ~S."
               :format-arguments (list x))))
    `(labels ((,name ,(mapcar #'first binds) ,@body))
       (,name ,@(mapcar #'second binds))))

  );;eval-when



(defmacro once-only (specs &body body)
  "Once-Only ({(Var Value-Expression)}*) Form*
  Create a Let* which evaluates each Value-Expression, binding a temporary
  variable to the result, and wrapping the Let* around the result of the
  evaluation of Body.  Within the body, each Var is bound to the corresponding
  temporary variable."
  (iterate frob
           ((specs specs)
            (body body))
           (if (null specs)
               `(progn ,@body)
               (let ((spec (first specs)))
                 (when (/= (length spec) 2)
                   (error 'simple-program-error
                          :format-control "Malformed ~s binding spec: ~S."
                          :format-arguments (list 'once-only spec)))
                 (let ((name (first spec))
                       (exp-temp (gensym)))
                   `(let ((,exp-temp ,(second spec))
                          (,name (gensym)))
                      `(let ((,,name ,,exp-temp))
                         ,,(frob (rest specs) body))))))))




;;; Collect-List-Expander  --  Internal
;;;
;;;    This function deals with the list collection case.  N-Tail is the pointer
;;; to the current tail of the list, which is NIL if the list is empty.
;;;
(defun collect-list-expander (n-value n-tail forms)
  (let ((n-res (gensym)))
    `(progn
       ,@(mapcar (lambda (form)
                     `(let ((,n-res (cons ,form nil)))
                        (cond (,n-tail
                               (setf (cdr ,n-tail) ,n-res)
                               (setf ,n-tail ,n-res))
                              (t
                               (setf ,n-tail ,n-res  ,n-value ,n-res)))))
                 forms)
       ,n-value)))


;;;
;;;    The ultimate collection macro...
;;;

(defmacro collect (collections &body body)
  "Collect ({(Name [Initial-Value] [Function])}*) {Form}*
  Collect some values somehow.  Each of the collections specifies a bunch of
  things which collected during the evaluation of the body of the form.  The
  name of the collection is used to define a local macro, a la MACROLET.
  Within the body, this macro will evaluate each of its arguments and collect
  the result, returning the current value after the collection is done.  The
  body is evaluated as a PROGN; to get the final values when you are done, just
  call the collection macro with no arguments.

  Initial-Value is the value that the collection starts out with, which
  defaults to NIL.  Function is the function which does the collection.  It is
  a function which will accept two arguments: the value to be collected and the
  current collection.  The result of the function is made the new value for the
  collection.  As a totally magical special-case, the Function may be Collect,
  which tells us to build a list in forward order; this is the default.  If an
  Initial-Value is supplied for Collect, the stuff will be rplacd'd onto the
  end.  Note that Function may be anything that can appear in the functional
  position, including macros and lambdas."


  (let ((macros ())
        (binds ()))
    (dolist (spec collections)
      (unless (<= 1 (length spec) 3)
        (error 'simple-program-error
               :format-control "Malformed collection specifier: ~S."
               :format-arguments (list spec)))
      (let ((n-value (gensym))
            (name (first spec))
            (default (second spec))
            (kind (or (third spec) 'collect)))

        (push `(,n-value ,default) binds)
        (if (eq kind 'collect)
          (let ((n-tail (gensym)))
            (if default
              (push `(,n-tail (last ,n-value)) binds)
              (push n-tail binds))
            (push `(,name (&rest args)
                          (collect-list-expander ',n-value ',n-tail args))
                  macros))
          (push `(,name (&rest args)
                        (collect-normal-expander ',n-value ',kind args))
                macros))))
    `(macrolet ,macros (let* ,(nreverse binds) (declare (ignorable ,@binds)) ,@body))))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun quoted-form-p (form)
    (and (consp form)
         (eq (car form) 'quote)
         (consp (cdr form))
         (null (cddr form)))))


(defmacro report-bad-arg (&whole w thing typespec &environment env)
  (when (quoted-form-p typespec)
    (unless (ignore-errors (specifier-type-if-known (cadr typespec) env))
      (warn "Unknown type specifier ~s in ~s." (cadr typespec) w)))
  `(values (error 'type-error
                  :datum ,thing
                  :expected-type ,typespec)))


(defmacro niy (item &rest vars)
  `(warn "(~S ~{~S~^ ~}) is not implemented yet" ',item ',vars))

;;;; THE END ;;;;
