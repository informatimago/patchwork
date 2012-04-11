;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;=========================================================
;;;;
;;;;  PATCH-WORK
;;;;  By Mikael Laurson, Jacques Duthen, Camilo Rueda.
;;;;  © 1986-1992 IRCAM 
;;;;
;;;;=========================================================

(in-package :ccl)

;======================================
; from library:scrollers.lisp to allow smaller scrollbars
(defvar *default-scroll-size* 8) ; normally 16

(defmethod initialize-instance ((self ccl::scroller-mixin) &rest initargs &key
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
    (setf (scroll-bar-correction self) (make-point (if v-scroll (1+ *default-scroll-size*) 2)
                                             (if h-scroll (+ bottom-boarder *default-scroll-size*) 2)))
    (setf (v-scroller self) v-scroll)
    (setf (h-scroller self) h-scroll)
    (setf (scroller-outline self) outline)
    (if (and (view-position self) (view-size self))
      (update-scroll-bars self :length t :position t))
    (when view-container
      (set-view-container self view-container))))

