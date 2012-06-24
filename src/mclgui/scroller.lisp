;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               scroller.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Views with scroll bars.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-31 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
;;;;    Copyright 1989-1994 Apple Computer, Inc.
;;;;    Copyright 1995 Digitool, Inc.
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


(defclass box-dialog-item (simple-view)
  ())


(defmethod point-in-click-region-p ((self box-dialog-item) point)
  (declare (ignore point))
  nil)


(defmethod view-contains-point-p ((dialog-item box-dialog-item) point)
  "Hide the box dialog item from attention"
  (declare (ignore  point))
  nil)


(defmethod view-draw-contents ((self box-dialog-item))
  (let* ((pos (view-position self))
        (end (add-points pos (view-size self))))
    (niy view-draw-contents self)
    #-(and)
    (rlet ((r :rect
              :topleft pos
              :bottomright end))
      (#_FrameRect r))))


(defclass scroller-mixin ()
  ((v-scroller            :accessor v-scroller)
   (h-scroller            :accessor h-scroller)
   ;; ## GROW-ICON-P ADDED:
   (grow-icon-p           :reader   grow-icon-p
                          :initarg  :grow-icon-p
                          :initform NIL)
   (field-size            :initarg  :field-size
                          :initform nil
                          :reader   field-size)
   (scroller-outline      :accessor scroller-outline)
   (scroll-bar-correction :accessor scroll-bar-correction)))


(defclass scroller (scroller-mixin view)
  ())

(defmethod initialize-instance ((self scroller-mixin) &rest initargs &key
                                view-container (v-scrollp t) (h-scrollp t)
                                (draw-scroller-outline t)
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
                                    :direction :vertical
                                    :track-thumb-p track-thumb-p)))
         (h-scroll (if h-scrollp
                     (make-instance (or h-scroll-class scroll-bar-class)
                                    :scrollee self
                                    :direction :horizontal
                                    :track-thumb-p track-thumb-p)))
         (outline (if draw-scroller-outline
                    (make-instance 'box-dialog-item))))
    (setf (scroll-bar-correction self) (make-point (if v-scroll 17 2)
                                             (if h-scroll 17 2)))
    (setf (v-scroller self) v-scroll)
    (setf (h-scroller self) h-scroll)
    (setf (scroller-outline self) outline)
    (when view-container
      (set-view-container self view-container))
    (if (and (view-position self) (view-size self))
      (update-scroll-bars self :length t :position t))))


;; This is how a view communicates it's scroll bar limits to a scroller.
;; Returns two points, the limits for the horizontal & vertical scroll bars
;; This is the coordinate system passed to set-view-scroll-position

(defmethod scroll-bar-limits ((view scroller-mixin))
  (let ((field-size (field-size view))
        (size (view-size view))
        (h-scroller (h-scroller view))
        (v-scroller (v-scroller view)))
    (if field-size
      (values (make-point 0 (max (if h-scroller (scroll-bar-setting h-scroller) 0)
                                 (- (point-h field-size) (point-h size))))
              (make-point 0 (max (if v-scroller (scroll-bar-setting v-scroller) 0)
                                 (- (point-v field-size) (point-v size)))))
      (let ((size (view-size view)))
        (normal-scroll-bar-limits view (add-points size size))))))


(defmethod normal-scroll-bar-limits ((view scroller-mixin) max-h &optional max-v)
  (let ((size (view-size view))
        (max (make-point max-h max-v)))
    (values (make-point 0 (max 0 (- (point-h max) (point-h size))))
            (make-point 0 (max 0 (- (point-v max) (point-v size)))))))


;; And here's how a view communicates it's page size
;; Returns a point.
(defmethod scroll-bar-page-size ((view scroller-mixin))
  (view-size view))


(defmethod scroll-bar-scroll-size ((self scroller-mixin))
  (let ((h-scroller (h-scroller self))
        (v-scroller (v-scroller self)))
    (make-point (if h-scroller (scroll-bar-scroll-size h-scroller) 0)
                (if v-scroller (scroll-bar-scroll-size v-scroller) 0))))


(defmethod set-view-container ((self scroller-mixin) new-container)
  (let ((need-to-update? (not (and (view-position self) (view-size self)))))
    (call-next-method)
    (when (v-scroller self)  (set-view-container (v-scroller self) new-container))
    (when (h-scroller self)  (set-view-container (h-scroller self) new-container))
    (when (scroller-outline self)  
      (set-view-container (scroller-outline self) new-container))
    (when need-to-update?
      (update-scroll-bars self :length t :position t))
    new-container))


(defmethod set-view-position ((self scroller-mixin) h &optional v)
  (declare (ignore h v))
  (without-interrupts
   (prog1
     (call-next-method)
     (update-scroll-bars self :position t))))


(defmethod set-view-size ((self scroller-mixin) h &optional v)
  (declare (ignore h v))  
  (without-interrupts
   (prog1
     (call-next-method)
     (update-scroll-bars self :length t :position t))))


(defmethod update-scroll-bars ((self scroller-mixin) &key length position)
  (let* ((pos (view-position self))
         (size (view-size self))
         (h-scroller (h-scroller self))
         (v-scroller (v-scroller self))
         (outline (scroller-outline self)))
    (when (and pos size)                ; auto-sizing may not have happenned yet 
      (without-interrupts
       (reposition-scroll-bars self h-scroller v-scroller :length length :position position)
       (when length
         (update-scroll-bar-limits self h-scroller v-scroller))
       (when outline
         (setq pos (subtract-points pos #@(1 1))
               size (add-points size (scroll-bar-correction self)))
         (set-view-position outline pos)
         (set-view-size outline size))))))


(defmethod update-scroll-bar-limits ((self scroller-mixin) &optional
                                     (h-scroller (h-scroller self))
                                     (v-scroller (v-scroller self)))
  (multiple-value-bind (h-limits v-limits) (scroll-bar-limits self)
    (let ((page-size (scroll-bar-page-size self))
          (scroll-size (scroll-bar-scroll-size self)))
      (when  h-scroller
        (set-scroll-bar-min h-scroller (point-h h-limits))
        (set-scroll-bar-max h-scroller (point-v h-limits))
        (setf (scroll-bar-page-size h-scroller) (point-h page-size))
        (setf (scroll-bar-scroll-size h-scroller) (point-h scroll-size)))
      (when v-scroller
        (set-scroll-bar-min v-scroller (point-h v-limits))
        (set-scroll-bar-max v-scroller (point-v v-limits))
        (setf (scroll-bar-page-size v-scroller) (point-v page-size))
        (setf (scroll-bar-scroll-size v-scroller) (point-v scroll-size))))))


;; Call this whenever the thumb position changes.
(defmethod update-thumbs ((self scroller-mixin))
  (let ((pos (view-scroll-position self))
        (h-scroller (h-scroller self))
        (v-scroller (v-scroller self))
        (update-limits? nil))
    (when (and h-scroller
               (not (eql (scroll-bar-setting h-scroller) (point-h pos))))
      (when (eql (scroll-bar-min h-scroller)
                 (set-scroll-bar-setting h-scroller (point-h pos)))
        (setq update-limits? t)))
    (when (and v-scroller
               (not (eql (scroll-bar-setting v-scroller) (point-v pos))))
      (when (eql (scroll-bar-min v-scroller)
                 (set-scroll-bar-setting v-scroller (point-v pos)))
        (setq update-limits? t)))
    (when update-limits? (update-scroll-bar-limits self))))


; Seperate from update-scroll-bars so that users can specialize it.
(defmethod reposition-scroll-bars ((self scroller-mixin) h-scroller v-scroller &key length position)
  (let* ((pos (view-position self))
         (size (view-size self))
         (width (point-h size))
         (height (point-v size))
         (grow-icon-p (grow-icon-p self)))
    (when (and pos size)                ; auto-sizing may not have happenned yet 
      (without-interrupts
       (when h-scroller
         (when position
           (set-view-position h-scroller (add-points pos (make-point -1 height))))
         (when length
           (set-scroll-bar-length h-scroller
            ;; ## WIDTH ADJUSTED IF A GROW ICON:
             (+ 2 width (if (and grow-icon-p (not v-scroller)) -14 0)))))
       (when v-scroller
         (when position
           (set-view-position v-scroller (add-points pos (make-point width -1))))
         (when length
           (set-scroll-bar-length v-scroller
                                  ;; height ADJUSTED IF A GROW ICON:
              (+ 2 height (if (and grow-icon-p (not h-scroller)) -14 0)))))))))

 
(defmethod scroll-bar-changed ((view scroller-mixin) scroll-bar)
  (let* ((new-value (scroll-bar-setting scroll-bar))
         (horizontal-p (eq (scroll-bar-direction scroll-bar) :horizontal))
         (old-pos (view-scroll-position view)))
    (set-view-scroll-position 
     view
     (if horizontal-p
       (make-point new-value (point-v old-pos))
       (make-point (point-h old-pos) new-value)))
    (when (eql new-value (scroll-bar-min scroll-bar))
      (update-scroll-bar-limits view)))
  (window-update-event-handler (view-window view)))


(defmethod set-view-scroll-position ((view scroller-mixin) h &optional v scroll-visibly)
  (declare (ignore h v scroll-visibly))
  (prog1
    (call-next-method)
    (update-thumbs view)))


;;;;;;;;;;;;;;;;;;;;;
;;
;; scroller-pane is the easiest way to use a scroller
;; It packages up a scroller and two scroll bars into a single
;; view. The scroller should have scroll-bar-mixin mixed in.
;;

(defclass scroller-pane (view)
  ((scroller              :accessor scroller)
   (scroller-outline      :accessor scroller-outline
                          :initarg :scroller-outline)
   (draw-scroller-outline :accessor draw-scroller-outline
                          :initarg :draw-scroller-outline)))



(defmethod v-scroller ((pane scroller-pane))
  (v-scroller (scroller pane)))


(defmethod h-scroller ((pane scroller-pane))
  (h-scroller (scroller pane)))


(defmethod field-size ((pane scroller-pane))
  (field-size (scroller pane)))


(defmethod initialize-instance ((self scroller-pane) &rest initargs &key
                                ; These args are used locally
                                view-position view-size view-container
                                view-nick-name
                                (draw-scroller-outline t)
                                (scroller-class 'scroller)
                                ; The rest of the args are passed through
                                ; to the scroller
                                (v-scrollp t) (h-scrollp t)
                                track-thumb-p
                                scroll-bar-class h-scroll-class v-scroll-class)
  (declare (dynamic-extent initargs))
  (declare (ignore track-thumb-p scroll-bar-class h-scroll-class v-scroll-class))
  (call-next-method
   self
   :view-container nil                  ; set container later
   :draw-scroller-outline draw-scroller-outline
   (if view-position :view-position :ignore) view-position
   (if view-size :view-size :ignore) view-size
   (if view-nick-name :view-nick-name :ignore) view-nick-name)
  (multiple-value-bind (scroller-size scroller-position)
                       (scroller-size-and-position self h-scrollp v-scrollp)
    (remf initargs :scroller-class)
    (setf (scroller self)
          (apply #'make-instance
                 scroller-class
                 :view-position scroller-position
                 :view-size scroller-size
                 :draw-scroller-outline nil
                 :view-container self
                 :view-nick-name nil
                 initargs)))
  (when view-container
    (set-view-container self view-container))
  self)

    
(defmethod scroller-size-and-position ((self scroller-pane) h-scrollp v-scrollp)
  (let ((scroller-size (view-size self))
        (scroller-position #@(0 0)))
    (when scroller-size
      (when h-scrollp
        (setq scroller-size (subtract-points scroller-size #@(15 0))))
      (when v-scrollp
        (setq scroller-size (subtract-points scroller-size #@(0 15)))))
    (when (draw-scroller-outline self)
      (setq scroller-position #@(1 1)
            scroller-size (and scroller-size
                               (subtract-points scroller-size #@(2 2)))))
    (values scroller-size scroller-position)))


(defmethod scroll-bar-limits ((self scroller-pane))
  (scroll-bar-limits (scroller self)))


(defmethod scroll-bar-page-size ((self scroller-pane))
  (scroll-bar-page-size (scroller self)))


(defmethod scroll-bar-scroll-size ((self scroller-pane))
  (scroll-bar-scroll-size self))


(defmethod view-draw-contents :before ((self scroller-pane))
  (when (draw-scroller-outline self)
    (niy view-draw-contents self)
    #-(and)
    (rlet ((rect :rect
                 :topleft #@(0 0)
                 :botright (view-size self)))
      (#_FrameRect rect))))


(defmethod set-view-size ((self scroller-pane) h &optional v)
  (declare (ignore h v))  
  (when (draw-scroller-outline self)
    (let* ((old-size (view-size self))
           (h (point-h old-size))
           (v (point-v old-size)))
      (invalidate-corners self (make-point 0 (1- v)) (make-point h v) t)
      (invalidate-corners self (make-point (1- h) 0) (make-point h v) t)))
  (prog1
    (call-next-method)
    (set-view-size (scroller self)
                   (scroller-size-and-position
                    self (h-scroller self) (v-scroller self)))))


(defmethod update-scroll-bars ((self scroller-pane) &key length position)
  (update-scroll-bars (scroller self) :length length :position position))


(defmethod update-scroll-bar-limits ((self scroller-pane) &optional
                                     (h-scroller (h-scroller self))
                                     (v-scroller (v-scroller self)))
  (update-scroll-bar-limits (scroller self) h-scroller v-scroller))


(defmethod update-thumbs ((self scroller-pane))
  (update-thumbs (scroller self)))


(defmethod scroll-bar-changed ((self scroller-pane) scroll-bar)
  (scroll-bar-changed (scroller self) scroll-bar))


(defmethod set-view-scroll-position ((view scroller-pane) h &optional v scroll-visibly)
  (set-view-scroll-position (scroller view) h v scroll-visibly))

;;;; THE END ;;;;
