;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               circular.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Utility to deal with circular structures.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-04-14 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2013 - 2013
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

(defun process-referenced-nodes (root walker processor &key (test (function eql)))
  (let ((table (make-hash-table :test test)))
    (loop
      :with nodes = (list root)
      :while nodes
      :do (let ((node (pop nodes)))
            (if (gethash node table)
                (incf (gethash node table))
                (progn
                  (setf (gethash node table) 1)
                  (setf nodes (nconc nodes (funcall walker node)))))))
    (maphash (let ((index 0))
               (lambda (k v)
                 (if (= 1 v)
                     (remhash k table)
                     (setf (gethash k table) (cons (incf index) nil)))))
             table)
    (funcall processor root (lambda (node) (gethash node table)))))



(defun print-identified-conses/1 (tree  &optional (stream *standard-output*))
  "
DO:      Print the TREE with all cons cells identified with a #n= notation.
TREE:    A sexp.
STREAM:  The output stream (default: *STANDARD-OUTPUT*)
NOTE:    Handles circles in the cons structure, but not thru the other
         atoms (vectors, structures, objects).
EXAMPLE: (print-identified-conses '((a . b) #1=(c . d) (e . #1#)))
         prints:
         ((a . b) . (#1=(c . d) . ((e . #1# ) . ())))
"
  (process-referenced-nodes
   tree
   (lambda (node) (when (consp node) (list (car node) (cdr node))))
   (lambda (node reference)
     (labels ((print-node (node)
                (if (null node)
                    (princ "()" stream)
                    (let ((index (funcall reference node)))
                      (if (and index (cdr index))
                          (format stream "#~A#" (car index))
                          (progn
                            (when index
                              (format stream "#~A=" (car index))
                              (setf (cdr index) t))
                            (if (atom node)
                                (princ node stream)
                                (progn
                                  (princ "(" stream) 
                                  (print-node (car node))
                                  (princ " . " stream)
                                  (print-node (cdr node))
                                  (princ ")" stream)))))))))
       (print-node node))))
  tree)
(defun test/print-identified-conses/1 ()
  (let ((result (substitute-if #\9 (function digit-char-p) ; reference numbers may be different.
                               (with-output-to-string (stream)
                                 (let ((*print-case* :upcase))
                                   (print-identified-conses/1 '((a . #2=(b . (c . d))) #1=(c d . #2#) (e . #1#)) stream)))))
        (expected "((A . #9=(B . (#9=C . #9=D))) . (#9=(#9# . (#9# . #9#)) . ((E . #9#) . ())))"))
    (assert (string= result expected) ()
            "expected = ~S~%  result = ~S~%" expected result))
  :success)


;;; --------------------------------------------------------------------

(defvar *circular-references* nil)

(defmacro with-circular-references ((&key (test ''eql)) &body body)
  `(let ((*circular-references* (cons (make-hash-table :test ,test) 0)))
     ,@body))

(defun circular-register (object)
  (let ((count (gethash object (car *circular-references*) 0)))
    (if count
      (= 1 (incf (gethash object (car *circular-references*) 0)))
      (progn
        ;; (invoke-debugger (make-condition 'simple-error :format-control "circular-register: Re-registering."))
        (warn "BAD: re-registering ~S" object)
        nil))))

(defun circular-reference (object)
  (let ((index (gethash object (car *circular-references*))))
    (typecase index
      (null    nil)
      (integer (setf (gethash object (car *circular-references*))
                     (if (= 1 index)
                         nil
                         (cons (incf (cdr *circular-references*))
                               nil))))
      (t       index))))

;;; --------------------------------------------------------------------

(defun print-identified-conses/2 (tree  &optional (stream *standard-output*))
  "
DO:      Print the TREE with all cons cells identified with a #n= notation.
TREE:    A sexp.
STREAM:  The output stream (default: *STANDARD-OUTPUT*)
NOTE:    Handles circles in the cons structure, but not thru the other
         atoms (vectors, structures, objects).
EXAMPLE: (print-identified-conses '((a . b) #1=(c . d) (e . #1#)))
         prints:
         ((a . b) . (#1=(c . d) . ((e . #1# ) . ())))
"
  (with-circular-references ()
    (labels ((walk-cons-tree (object)
               (when (circular-register object)
                 (when (consp object)
                   (walk-cons-tree (car object))
                   (walk-cons-tree (cdr object))))))
      (walk-cons-tree tree))
    (labels ((print-node (node)
               (if (null node)
                   (princ "()" stream)
                   (let ((index (circular-reference node)))
                     (if (and index (cdr index))
                         (format stream "#~A#" (car index))
                         (progn
                           (when index
                             (format stream "#~A=" (car index))
                             (setf (cdr index) t))
                           (if (atom node)
                               (princ node stream)
                               (progn
                                 (princ "(" stream) 
                                 (print-node (car node))
                                 (princ " . " stream)
                                 (print-node (cdr node))
                                 (princ ")" stream)))))))))
      (print-node tree)))
  tree)


(defun test/print-identified-conses/2 ()
  (let ((result (with-output-to-string (stream)
                  (let ((*print-case* :upcase))
                    (print-identified-conses/2 '((a . #2=(b . (c . d))) #1=(c d . #2#) (e . #1#)) stream))))
        (expected "((A . #1=(B . (#2=C . #3=D))) . (#4=(#2# . (#3# . #1#)) . ((E . #4#) . ())))"))
    (assert (string= result expected) ()
            "expected = ~S~%  result = ~S~%" expected result))
  :success)



;; (with-output-to-string (stream)
;;   (print-identified-conses/1 '((a . #2=(b . (c . d))) #1=(c d . #2#) (e . #1#)) stream))
;; "((a . #2=(b . (#3=c . #1=d))) . (#4=(#3# . (#1# . #2#)) . ((e . #4#) . ())))"
;; 
;; (with-output-to-string (stream)
;;   (print-identified-conses/2 '((a . #2=(b . (c . d))) #1=(c d . #2#) (e . #1#)) stream))
;; "((a . #1=(b . (#2=c . #3=d))) . (#4=(#2# . (#3# . #1#)) . ((e . #4#) . ())))"



;; (defgeneric archive-object-circularly (object archive)
;;   (:method ((object atom))
;;     (write-archive archive object))
;;   (:method ((object array))
;;     (let ((reference (circular-register object)))
;;       (if reference
;;           (write-archive archive reference))
;;         )
;;     (let (())
;;      (write-archive archive (list 'array
;;                                   (array-dimensions object)
;;                                   (array-element-type object)
;;                                   )))
;;     ()))
;; 
;; (defun archive-circularly (object)
;;   (let ((*circularly-archived-objects* (make-hash-table)))
;;     (archive-object-circularly object)))



(test/print-identified-conses/1)
(test/print-identified-conses/2)

;;;; THE END ;;;;

