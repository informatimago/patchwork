;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               mini-scroller.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;    
;;;;    XXX
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-07 <PJB> Changed license to GPL3; Added this header.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
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
;;;;    
;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;=========================================================
;;;;
;;;;  PATCH-WORK
;;;;  By Mikael Laurson, Jacques Duthen, Camilo Rueda.
;;;;  © 1986-1992 IRCAM 
;;;;
;;;;=========================================================

(in-package :ui)

;;======================================
;; from library:scrollers.lisp to allow smaller scrollbars
(defvar *default-scroll-size* 8) ; normally 16

(defmethod initialize-instance ((self scroller-mixin) &rest initargs &key
                                view-container (v-scrollp t) (h-scrollp t)
                                (draw-scroller-outline t) (bottom-boarder 1)
                                track-thumb-p
                                (scroll-bar-class 'scroll-bar-dialog-item)
                                h-scroll-class v-scroll-class)
  (declare (dynamic-extent initargs))
  (setf (v-scroller self) nil)          ; fix start-up transient.
  (setf (h-scroller self) nil)
  (apply #'call-next-method self :view-container nil initargs)   ; delay the set-view-container
  (let* ((v-scroll (if v-scrollp
                     (make-instance (or v-scroll-class scroll-bar-class)
                                    :scrollee self
                                    :direction :vertical  :width *default-scroll-size*
                                    :track-thumb-p track-thumb-p)))
         (h-scroll (if h-scrollp
                     (make-instance (or h-scroll-class scroll-bar-class)
                                    :scrollee self
                                    :direction :horizontal :width *default-scroll-size*
                                    :track-thumb-p track-thumb-p)))
         (outline (if draw-scroller-outline
                    (make-instance 'box-dialog-item))))
    (setf (scroll-bar-correction self)
          (make-point (if v-scroll (1+ *default-scroll-size*) 2)
                      (if h-scroll (+ bottom-boarder *default-scroll-size*) 2)))
    (setf (v-scroller self) v-scroll)
    (setf (h-scroller self) h-scroll)
    (setf (scroller-outline self) outline)
    (if (and (view-position self) (view-size self))
      (update-scroll-bars self :length t :position t))
    (when view-container
      (set-view-container self view-container))))

;;;; THE END ;;;;
