;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               sequence-dialog-item.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Sequence Dialog Item
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-29 <PJB> Created.
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



(defclass sequence-dialog-item (table-dialog-item)
  ((default-table-sequence
    :allocation :class
    :initform '(0 1 2))
   (table-sequence
    :initarg :table-sequence
    :documentation "the sequence associated with the dialog item.")
   (sequence-order
    :initarg :sequence-order
    :documentation "
Whether the sequence will fill the table dialog item row by row or
column by column.  The value of this keyword should be either
:vertical or :horizontal. The default is :vertical.
")
   (sequence-wrap-length
    :initarg :sequence-wrap-length
    :documentation "
The number of elements allowed in a row or column before the table
dialog item wraps to the next row or column. This number overrides the
:tabledimensions argument.
"))
  (:default-initargs :sequence-order :vertical :sequence-wrap-length most-positive-fixnum))



(defmethod initialize-instance ((item sequence-dialog-item) &rest rest
                                &key
                                (table-sequence nil sequencep) table-dimensions 
                                sequence-order sequence-wrap-length)
  (declare (dynamic-extent rest))
  (let ((sequence-length))
   (when (null sequencep)
     (setf table-sequence (slot-value item 'default-table-sequence)))
   (setf sequence-length (length table-sequence))
   (when (null table-dimensions)
     (setf table-dimensions
           (let* ((dimen-prime 1)
                  (dimen-aux 1))
             (if (<= sequence-length sequence-wrap-length)
                 (setf dimen-prime sequence-length)
                 (setf dimen-prime sequence-wrap-length
                       dimen-aux (ceiling sequence-length sequence-wrap-length)))
             (case sequence-order
               (:vertical
                (make-big-point dimen-aux dimen-prime))
               (:horizontal
                (make-big-point dimen-prime dimen-aux))
               (otherwise
                (report-bad-arg sequence-order '(member :horizontal :vertical))))))))
  (apply #'call-next-method
         item 
         :table-sequence table-sequence
         :sequence-order sequence-order
         :sequence-wrap-length sequence-wrap-length
         :table-dimensions table-dimensions
         rest)
  (set-table-sequence item table-sequence))



(defgeneric table-sequence (item)
  (:documentation "

The TABLE-SEQUENCE generic function returns the sequence associated
with the dialog item.

ITEM:           A sequence dialog item.

")
  (:method ((item sequence-dialog-item))
    (slot-value item 'table-sequence)))


(defmethod (setf table-sequence) (value (item sequence-dialog-item))
  (set-table-sequence item value))


(defgeneric set-table-sequence (item new-seq)
  (:documentation "

The SET-TABLE-SEQUENCE generic function sets the sequence associated
with the dialog item to new-sequence, resets the dimensions of the table
dialog item and the scroll bars, and redisplays the dialog item.

ITEM:           A sequence dialog item.

NEW-SEQUENCE:   The sequence to be associated with the sequence dialog
                item.  The elements of this sequence are displayed in
                the cells of the sequence dialog item.

")
  (:method ((item sequence-dialog-item) new-seq)
    (let ((sequence-wrap-length (slot-value item 'sequence-wrap-length))
          (sequence-order (slot-value item 'sequence-order)))
      (let* ((old-seq (table-sequence item))
             (old-dims (table-dimensions item))
             (length (length new-seq))
             (prim (min length sequence-wrap-length))
             (sec (if (= prim 0) 0 (ceiling length prim)))
             new-dims)
        (let* (                    ;(handle (dialog-item-handle item))
                                        ;(active-p (and handle (href handle :ListRec.lactive)))
               (f #'(lambda (item h v)
                      (let ((index (cell-to-index item h v)))
                        (when (and index (>= index length))
                          (cell-deselect item h v))))))
          (new-map-selected-cells item f))
        (without-interrupts
            (setf (slot-value item 'table-sequence) new-seq)
          (set-table-dimensions 
           item
           (setf new-dims
                 (if (eq sequence-order :horizontal)
                     (make-big-point prim sec)
                     (make-big-point sec prim)))))
        (unless (and (equal old-dims new-dims)
                     (or (eq old-seq new-seq)
                         (every #'eq old-seq new-seq)))
          (when (installed-item-p item)
            (maybe-need-scroll-bars item)
            (invalidate-corners item #@(0 0) (table-inner-size item) nil)))
        new-seq))))


(defgeneric cell-to-index (item h &optional v)
  (:documentation "

The CELL-TO-INDEX generic function returns an index into the sequence
associated with the dialog item, corresponding to the cell whose indices in
the table are H and V.  If there is no such cell, it returns NIL.
This index is suitable for passing to the Common Lisp function elt.

ITEM:           A sequence dialog item.

H:              Horizontal index.

V:              Vertical index.  If the value of V is NIL, H is
                assumed to represent a point.

")
  (:method ((item sequence-dialog-item) h &optional v)
      (normalize-h&v h v)
      (let* ((table-dimensions (table-dimensions item))
             (table-sequence (table-sequence item))
             (sequence-order (slot-value item 'sequence-order))
             (index (if (eq sequence-order :horizontal)
                        (+ (* (point-h table-dimensions) v) h)
                        (+ (* (point-v table-dimensions) h) v))))
        (if (< index (length table-sequence))
            index
            nil))))


(defgeneric index-to-cell (item index)
  (:documentation "

The INDEX-TO-cell generic function returns a cell in the dialog item.
The cell corresponds to the indexth element of the table’s sequence.

ITEM:           A sequence dialog item.

INDEX:          An index to the sequence (zero based, as would be
                passed to ELT).

")
  (:method ((item sequence-dialog-item) index)
    (let ((sequence-order (slot-value item 'sequence-order))
          (table-dimensions (table-dimensions item)))
      (if (eq sequence-order :horizontal)
          (multiple-value-bind (ind-1 ind-2)
              (floor index (point-h table-dimensions))
            (make-big-point ind-2 ind-1))
          (multiple-value-call #'make-big-point
            (floor index (point-v table-dimensions)))))))






(defmethod cell-contents ((item sequence-dialog-item) h &optional v
                          &aux index)
  (if (setf index (cell-to-index item h v))
      (elt (slot-value item 'table-sequence) index)
      nil))

(defmethod window-can-do-operation ((table sequence-dialog-item) op &optional item)
  (declare (ignore item))
  (and (eq op 'copy) (first-selected-cell table)))


;; put-scrap a lisp object and a textual representation thereof
(defun put-scraps (value text)
  (put-scrap :lisp value)
  (let ((mactext text))
    (niy put-scraps value text)
    #-(and)
    (when (not (7bit-ascii-p text))
      (setf mactext (convert-string text #$kcfstringencodingunicode #$kcfstringencodingmacroman)))
    (put-scrap-flavor :text mactext)
    (unless (eq mactext text)  ;; unicode text preferred
      (put-scrap-flavor :|utxt| text))))


(defmethod copy ((table sequence-dialog-item))
  (let ((cells (selected-cells table)))
    (cond
      ((null cells))
      ((null (cdr cells))
       (let ((cell (car cells)))
         (put-scraps (cell-contents table cell) (cell-contents-string table cell))))
      (t (let* ((vals nil)
                (string (with-output-to-string (stream)
                          (setf vals (mapcar (lambda (cell)
                                               (format stream "~A~%" (cell-contents-string table cell))
                                               (cell-contents table cell))
                                             cells)))))
           (put-scraps vals string))))))


;;;; THE END ;;;;
