;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               system.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    System functions
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-16 <PJB> Created.
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


(defun fixnump (object)
  (typep object 'fixnum))


(defun ed-beep (&optional (duration 3) &rest args)
  (declare (ignorable duration args))
  #+ccl (#_NSBeep)
  #-ccl (niy ed-beep duration args))


(defun get-sys-just ()
  0)



(defun ensure-list (object)
  "
RETURN:         If OBJECT is a list then OBJECT, otherwise a fresh
                list containing OBJECT.
"
  (if (listp object)
      object
      (list object)))


(defun list-designator (object)
  "
RETURN:         If the OBJECT is a list containing a single non-NIL
                atom, then this first element is returned, else OBJECT.
"
  (if (and (listp object)
           object
           (endp (rest object))
           (first object)
           (atom (first object)))
      (first object)
      object))


;; Note: taken from com.informatimago.common-lisp.cesarum.utility
(defun nsubseq (sequence start &optional (end nil))
  "
RETURN:  When the SEQUENCE is a vector, the SEQUENCE itself, or a dispaced
         array to the SEQUENCE.
         When the SEQUENCE is a list, it may destroy the list and reuse the
         cons cells to make the subsequence.
"
  (if (vectorp sequence)
      (if (and (zerop start) (or (null end) (= end (length sequence))))
          sequence
          (make-array (- (if end
                             (min end (length sequence))
                             (length sequence))
                         start)
                      :element-type (array-element-type sequence)
                      :displaced-to sequence
                      :displaced-index-offset start))
      (let ((result (nthcdr start sequence)))
        (when end
          (setf (cdr (nthcdr (- end start -1) sequence)) nil))
        result)))


(defun standard-generic-function-p (object)
  (typep object 'common-lisp:standard-generic-function))


(defun method-exists-p (op object)
  #+ccl (ccl:method-exists-p op object)
  #-ccl (error "method-exists-p not implemented on ~S" (lisp-implementation-type)))

;;;; THE END ;;;;
