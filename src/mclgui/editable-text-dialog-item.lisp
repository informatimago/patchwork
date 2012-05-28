;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               editable-text-dialog-item.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Editable Text Dialog Item
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-23 <PJB> Created.
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

;;;---------------------------------------------------------------------
;;;

(defclass basic-editable-text-dialog-item (key-handler-mixin dialog-item)
  ((width-correction   :allocation :class :initform 4)
   (text-justification :allocation :class :initform 0)))


(defmethod view-default-size ((item basic-editable-text-dialog-item))
  (let ((pt    (call-next-method))
        (width (point-h pt)))
    (make-point (- (ash width 1) (ash width -1)) (point-v pt))))


(defmethod dialog-item-disable :before ((item basic-editable-text-dialog-item))
  (let ((window (view-window item)))
    (when window
      (when (eq item (current-key-handler window))
        (change-key-handler window))
      (when (eq item (current-key-handler window)) ;still current, so only one
        (set-selection-range item 0 0)
        (setf (%get-current-key-handler window) nil)))))


;;;---------------------------------------------------------------------
;;;

(defclass editable-text-dialog-item (basic-editable-text-dialog-item)
  ())


(defmethod view-clip-region ((view editable-text-dialog-item))
  (niy view-clip-region view)
  ;; don't include frame
  #-(and)
  (let* ((pos           (view-position view))
         (size          (view-size view))
         (rgn           *simple-view-clip-region*)
         (container     (view-container view))
         (container-rgn (view-clip-region container)))
    (if (or (null pos) (null size))
        (#_EmptyRgn rgn)
        (let* ((pos-h  (point-h pos))
               (pos-v  (point-v pos))
               (size   (view-size view))
               (size-h (point-h size))
               (size-v (point-v size)))
          (#_setrectrgn rgn pos-h pos-v (+ pos-h size-h) (+ pos-v size-v))
          (#_SectRgn rgn container-rgn rgn)
          (#_OffsetRgn rgn (- pos-h) (- pos-v))))
    rgn))


(defmethod view-default-font ((view editable-text-dialog-item))
  (sys-font-spec))





;;;; THE END ;;;;
