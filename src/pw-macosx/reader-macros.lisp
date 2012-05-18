;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               reader-macros.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;    
;;;;    Defines the reader macros.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-07 <PJB> Changed license to GPL3; Added this header.
;;;;    2012-04-09 <PJB> Created
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
;;;;    Copyright IRCAM 1986 - 2012
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

(in-package "PW")


(defvar *readtable-patchwork*)
(defvar *readtable-preserve*)

(defun sharp-underline-dispatch-reader-macro (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((*readtable* *readtable-preserve*)
        (*package*   (find-package "FFI")))
    (if *read-suppress*
        (progn (read stream)
               (values))
        ;; actual would return the name of a FFI function, variable or #define,
        ;; we just return the preserved symbol, read in the FFI package..
        (values (read stream)))))

#-(and)
(defun sharp-at-dispatch-reader-macro (stream subchar arg)
  "#@(x y) reads a Point."
  (declare (ignore subchar arg))
  (let ((coord  (read stream)))
    (if *read-suppress*
        (values)
        (values (apply (function ui:make-point) coord)))))

#-(and)
(defun sharp-dot-dispatch-reader-macro (stream subchar arg)
  "#. we read the expression in the host CL."
  (declare (ignore subchar arg))
  (let ((form (cl:read stream)))
    (if *read-eval*
        (values (eval form))
        (error "Cannot evaluate #.~S when ~S is ~S"
               form '*read-eval* *read-eval*))))

(defun sharp-i-dispatch-reader-macro (stream char count)
  (declare (ignore char count))
  (if *read-suppress*
      (progn (read stream t nil t)
             (values))
      (values (clpf-util:prefix-expr (read stream t nil t))))) ; maybe (CLtLII p548)


(defun setup ()
  "
Configure a com.informatimago.common-lisp.lisp-reader.reader:readtable
for reading lisp sources interning packages and symbols in
com.informatimago.common-lisp.lisp-reader.package:package and
com.informatimago.common-lisp.lisp-reader.package:symbol instead of
common-lisp:package and common-lisp:symbol.
"
  (setf *read-eval* t)
  (let ((rt (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\_ (function sharp-underline-dispatch-reader-macro) rt)
    (set-dispatch-macro-character #\# #\$ (function sharp-underline-dispatch-reader-macro) rt)
    ;; (set-dispatch-macro-character #\# #\. (function sharp-dot-dispatch-reader-macro)       rt)
    ;; (set-dispatch-macro-character #\# #\@ (function sharp-at-dispatch-reader-macro)        rt)
    (let ((*readtable* rt)) (ui:enable-sharp-at-reader-macro))
    (set-dispatch-macro-character #\# #\i (function sharp-i-dispatch-reader-macro)         rt)
    (setf *readtable-preserve* (copy-readtable rt))
    (setf (readtable-case *readtable-preserve*) :preserve)
    (setf *readtable-patchwork* rt))
  ;; (eval-when (:compile-toplevel) (print '(setup called at compilation-time)))
  ;; (eval-when (:load-toplevel)    (print '(setup called at load-time)))
  ;; (eval-when (:execute)          (print '(setup called at execution-time)))
  ;; (terpri)
  *readtable-patchwork*)



(defmacro enable-patchwork-readtable ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setup)
     (setf *readtable* (copy-readtable *readtable-patchwork*))
     ;; (terpri)
     ;; (print (or *load-pathname* *compile-file-pathname*))
     ;; (print (get-dispatch-macro-character #\# #\@ *readtable*))
     ;; (print (let ((pt (read-from-string "#@(1 2)")))
     ;;          (list (typep pt 'ui:point)
     ;;                pt)))
     ;; (terpri)
     (values)))

(defmacro disable-patchwork-readtable ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *readtable* (copy-readtable nil))
     (values)))


(eval-when (:load-toplevel :execute)
  (setup))

;;;; THE END ;;;;


