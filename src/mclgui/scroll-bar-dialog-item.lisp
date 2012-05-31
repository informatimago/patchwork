;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               scroll-bar-dialog-item.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Scroll Bar Dialog Item.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-28 <PJB> Created.
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




(defclass scroll-bar-dialog-item (control-dialog-item)
  ((procid                 :allocation :class
                           :initform 0 ; #$scrollBarProc
                           )
   (direction              :initarg    :direction
                           :reader     scroll-bar-direction)  
   (min                    :initarg    :min
                           :reader     scroll-bar-min
                           :documentation "The minimum setting of item.")
   (max                    :initarg    :max
                           :reader     scroll-bar-max
                           :documentation "The maximum setting of item.")
   (setting                :initarg    :setting
                           :reader     scroll-bar-setting
                           :documentation "The current setting of item.")
   (track-thumb-p          :initarg    :track-thumb-p
                           :initform   t
                           :accessor   scroll-bar-track-thumb-p
                           :documentation "
The SCROLL-BAR-TRACK-THUMB-P generic function returns a value
indicating the behavior of item when the scroll box is dragged.  If true, the
scroll box moves and the function SCROLL-BAR-CHANGED is called as the
user drags the scroll box.  If NIL, only an outline of the scroll box moves
and scrolling does not occur until the user releases the mouse button. The
default value is NIL.
")
   (page-size              :initarg    :page-size
                           :initform   5
                           :accessor   scroll-bar-page-size
                           :documentation "The page size of item.")
   (scroll-size            :initarg    :scroll-size
                           :initform   1
                           :accessor   scroll-bar-scroll-size
                           :documentation "The scroll size of item.")
   (scrollee               :initarg    :scrollee
                           :initform   nil
                           :reader     scroll-bar-scrollee
                           :documentation "The scrollee of item (that is, what item is scrolling).")
   (pane-splitter          :initform   nil
                           :accessor   pane-splitter)
   (pane-splitter-position :initform   nil
                           :initarg    :pane-splitter 
                           :reader     pane-splitter-position)))


(defgeneric set-scroll-bar-track-thumb-p (item value)
  (:documentation "

The SET-SCROLL-BAR-TRACK-THUMB-P generic function sets the value
controlling the behavior of item when the scroll box is dragged.  If
true, the scroll box moves and the function SCROLL-BAR-CHANGED is
called as the user drags the scroll box.  If NIL, only an outline of
the scroll box moves and scrolling does not occur until the user
releases the mouse button.

ITEM:           A scroll-bar dialog item.

VALUE:          A Boolean value.  If item does not have a scroll box,
                the value is NIL.

")
  (:method ((item scroll-bar-dialog-item) value)
    (setf (scroll-bar-track-thumb-p item) value)))


(defgeneric set-scroll-bar-scrollee (item new-scrollee)
  (:documentation "

The SET-SCROLL-BAR-SCROLLEE generic function sets the scrollee of
ITEM (that is, what item is scrolling) to NEW-SCROLLEE.

ITEM:          A scroll-bar dialog item.

NEW-SCROLLEE: The new scrollee of item.

"))



(defclass pane-splitter (simple-view)
  ((scrollee   :initarg :scrollee 
               :reader   scroll-bar-scrollee)
   (direction  :initarg :direction
               :reader   scroll-bar-direction)
   (cursor     :initarg :cursor
               :initform *arrow-cursor*
               :accessor pane-splitter-cursor)
   (scroll-bar :initarg :scroll-bar
               :initform nil
               :reader   scroll-bar)))



(defmethod set-scroll-bar-scrollee ((item pane-splitter) new-scrollee)
  (setf (slot-value item 'scrollee) new-scrollee))


;; These are initialized for real by a def-ccl-pointers in l1-edfrec
(defparameter *top-ps-cursor*        *arrow-cursor*)
(defparameter *bottom-ps-cursor*     *arrow-cursor*)
(defparameter *left-ps-cursor*       *arrow-cursor*)
(defparameter *right-ps-cursor*      *arrow-cursor*)
(defparameter *vertical-ps-cursor*   *arrow-cursor*)
(defparameter *horizontal-ps-cursor* *arrow-cursor*)


(defmethod view-cursor ((p pane-splitter) where)
  (declare (ignore where))
  (pane-splitter-cursor p))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;initialize-instance
;;
;;initargs:
;;   length
;;   width
;;   direction
;;   setting
;;   min
;;   max
;;   page-size
;;   track-thumb-p
;;
;;in addition, the standard dialog-item initargs can be used
;;Size can be specified by either the view-size initarg or
;;the length & width initargs, but not both.
;;

(defmethod initialize-instance ((item scroll-bar-dialog-item) &rest initargs
                                &key (min 0) 
                                (max (if t 10000 100))  ;; was + $scroll-bar-max $scroll-bar-max
                                (setting 0)
                                width
                                (direction :vertical) length scrollee
                                pane-splitter-cursor pane-splitter-class
                                pane-splitter (pane-splitter-length 7) view-size
                                view-position view-container)
  (declare (dynamic-extent initargs))
  (setf max (max min max)
        setting (min (max setting min) max))
  (if (and view-size (or length width))
      (error "Both ~s and ~s were specified."
             ':view-size (if length :length :width)))
  (unless length
    (setf length
          (if view-size
              (ecase direction
                (:vertical (point-v view-size))
                (:horizontal (point-h view-size)))
              100)))
  (unless width
    (setf width
          (if view-size
              (ecase direction
                (:vertical (point-h view-size))
                (:horizontal (point-v view-size)))
              16)))
  (when pane-splitter
    (when (not pane-splitter-cursor)
      (setf pane-splitter-cursor
            (case direction
              (:vertical
               (case pane-splitter
                 (:top *top-ps-cursor*)
                 (t *bottom-ps-cursor*)))
              (t
               (case pane-splitter
                 (:left *left-ps-cursor*)
                 (t *right-ps-cursor*))))))
    (let* ((splitter (make-instance (or pane-splitter-class 'pane-splitter) 
                         :direction direction
                         :width width
                         :cursor pane-splitter-cursor
                         :length (or pane-splitter-length 7)
                         :scroll-bar item
                         :scrollee scrollee))
           (size (view-size splitter))
           (h (point-h size))
           (v (point-v size)))
      (setf (pane-splitter item) splitter)
      (if (eq direction :vertical)
          (progn
            (decf length v)
            (when view-position
              (let ((p-h (point-h view-position))
                    (p-v (point-v view-position)))
                (if (eq pane-splitter :top)
                    (progn
                      (set-view-position splitter view-position)
                      (setf view-position (make-point p-h (+ p-v v))))
                    (progn
                      (set-view-position splitter p-h (+ p-v length)))))))
          (progn
            (decf length h)
            (when view-position
              (let ((p-h (point-h view-position))
                    (p-v (point-v view-position)))
                (if (eq pane-splitter :left)
                    (progn
                      (set-view-position splitter view-position)
                      (setf view-position (make-point (+ p-h h) p-v)))
                    (progn
                      (set-view-position splitter (+ p-h length) p-v)))))))))  
  (apply #'call-next-method
         item
         :min min
         :max max
         :setting setting
         :direction direction
         :length length
         :view-container nil
         :view-position view-position
         :view-size
         (case direction
           (:vertical (make-point width length))
           (:horizontal (make-point length width))
           (t (error "illegal :direction ~a (must be :vertical or :horizontal)."
                     direction)))
         initargs)
  (when (and pane-splitter view-container (not view-position))
    (set-default-size-and-position item view-container))
  (when view-container
    (set-view-container item view-container))
  (when scrollee
    (add-view-scroll-bar scrollee item))
  item)


(defun view-scroll-bars (view)
  (view-get view 'scroll-bars))


(defun add-view-scroll-bar (view scroll-bar)
  (pushnew scroll-bar (view-get view 'scroll-bars)))


(defun delete-view-scroll-bar (view scroll-bar)
  (setf (view-get view 'scroll-bars)
        (delete scroll-bar (view-get view 'scroll-bars))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;install-view-in-window
;;
;;  this is when we actually create the control (when the item
;;  is added to a window)


(defconstant $scroll-bar-max 16384)


(defun mac-scroll-bar-min-max (min max &aux dif)
  (unless (>= max min) (setf max min))
  (cond ((and (>= min (- $scroll-bar-max)) (<= max $scroll-bar-max))
         (values min max))
        ((< (setf dif (- max min)) (+ $scroll-bar-max $scroll-bar-max))
         (let ((min-return
                (max (- $scroll-bar-max)
                     (min min (- $scroll-bar-max dif)))))
           (values min-return (+ min-return dif))))
        (t (values (- $scroll-bar-max) $scroll-bar-max))))


(defun mac-scroll-bar-setting (setting min max &optional mac-min mac-max)
  (if (<= max min)
      min
      (progn
        (unless (and mac-min mac-max)
          (multiple-value-setf (mac-min mac-max) (mac-scroll-bar-min-max min max)))
        (min mac-max
             (+ mac-min
                (round (* (- setting min) (- mac-max mac-min)) (- max min)))))))


(defun outside-scroll-bar-setting (scroll-bar handle)
  (niy outside-scroll-bar-setting scroll-bar handle)
  #-(and)
  (let ((mac-setting (#_GetControlValue handle))
        (mac-min (#_GetControlMinimum handle))
        (mac-max (#_GetControlMaximum handle))
        (min (scroll-bar-min scroll-bar))
        (max (scroll-bar-max scroll-bar)))    
    (declare (fixnum mac-min mac-max))
    (when (and #|(osx-p)|# (not (scroll-bar-track-thumb-p scroll-bar))) ;(> (- mac-max mac-min) 3000)) ;; total kludge because osx sucks
      (if (and (neq 0 mac-setting)(> (truncate (- mac-max mac-min) mac-setting) 100))(progn (setf mac-setting mac-min))))
    (if (eql mac-min mac-max)
        mac-min
        (+ min (round (* (- mac-setting mac-min) (- max min)) (- mac-max mac-min))))))


(defmethod install-view-in-window :after ((item scroll-bar-dialog-item) view)
  (declare (ignore view))
  (let* ((window (view-window item))
         (my-size (view-size item))
         (my-position (view-position item))
         (setting (scroll-bar-setting item))
         (min (scroll-bar-min item))
         (max (scroll-bar-max item))
         (mac-setting (mac-scroll-bar-setting setting min max)))
    (multiple-value-bind (mac-min mac-max) (mac-scroll-bar-min-max min max)
      (niy install-view-in-window item view)
      #-(and)
      (when window
        (rlet ((scroll-rect :rect :topleft my-position :bottomright (add-points my-position my-size)))
              (let ((handle (dialog-item-handle item)))
                (setf (dialog-item-handle item) nil)          ; I'm paranoid
                (when handle
                  (#_DisposeControl handle)))
              (rlet ((res :ptr))
                    (errchk
                     (#_CreateScrollBarControl
                      (wptr item)            ;window
                      scroll-rect            ;item rectangle
                      mac-setting            ;initial value
                      mac-min                ;min value
                      mac-max                ;max value
                      0                     ; view size  ?? what is that for
                      T                      ; live tracking
                      scroll-bar-proc                     ; live trackingproc
                      res                    ; out control
                      ))
                    (setf (dialog-item-handle item) (%get-ptr res))))
        (unless (dialog-item-enabled-p item)
          (#_deactivatecontrol (dialog-item-handle item)))
        ))))




(defmethod remove-view-from-window :before ((item scroll-bar-dialog-item))
  (niy remove-view-from-window item)
  #-(and)
  (let ((handle (dialog-item-handle item)))
    (when handle
      (with-focused-view (view-container item)
        (#_DisposeControl handle)
        (setf (dialog-item-handle item) nil)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;view-draw-contents
;;
;;this function is called whenever the item needs to be drawn
;;
;;to draw the dialog-item, we just call _Draw1Control
;;unless we just created it and it's still invisible.
;;


(defmethod view-draw-contents ((item scroll-bar-dialog-item))
  (niy view-draw-contents item)
  #-(and)
  (let* ((handle (dialog-item-handle item))
         (window (view-window item)))
    (when handle
      (if (#_iscontrolvisible handle)
          (#_Draw1Control handle)
          (#_ShowControl handle))
      (if (window-active-p window)
          (progn          
            (if (dialog-item-enabled-p item)
                (#_activatecontrol handle)
                (#_deactivatecontrol handle)))        
          (progn
            #+ignore
            (multiple-value-bind (tl br) (scroll-bar-and-splitter-corners item)          
              (rlet ((rect :rect :topLeft tl :botRight br))            
                    (#_FrameRect rect)))          
            (#_deactivatecontrol handle))))))


(defun scroll-bar-and-splitter-corners (scroll-bar)
  (multiple-value-bind (tl br) (view-corners scroll-bar)
    (let ((splitter (pane-splitter scroll-bar)))
      (if splitter
          (multiple-value-bind (stl sbr) (view-corners splitter)
            (values (make-point (min (point-h tl) (point-h stl))
                                (min (point-v tl) (point-v stl)))
                    (make-point (max (point-h br) (point-h sbr))
                                (max (point-v br) (point-v sbr)))))
          (values tl br)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;view-deactivate-event-handler
;;
;;this function is called whenever the scrollbar needs to be deactivated
;;

(defmethod view-deactivate-event-handler ((item scroll-bar-dialog-item))
  (niy view-deactivate-event-handler item)
  #-(and)
  (let ((handle (dialog-item-handle item))
        (container (view-container item)))
    (when handle
      (with-focused-view container
        (unless (window-active-p (view-window item))
          (let ((splitter (pane-splitter item)))
            (when splitter
              (view-draw-contents splitter))))            
        (#_deactivatecontrol handle)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;view-activate-event-handler
;;
;;this function is called whenever the scrollbar needs to be activated - gee I never would have guessed
;;


(defmethod view-activate-event-handler ((item scroll-bar-dialog-item))
  (niy view-activate-event-handler item)
  #-(and)
  (let ((w (view-window item)))
    (when (and w (window-active-p w))
      (let ((handle (dialog-item-handle item))
            (container (view-container item)))
        (with-focused-view container
          (if (not (#_iscontrolvisible handle))
                                        ; #_ShowControl is similarly naughty - why needed  when opening a bunch of fred windows quickly ?
              (progn 
                (#_showcontrol handle)
                #+ignore
                (multiple-value-bind (tl br) (scroll-bar-and-splitter-corners item)          
                  (rlet ((rect :rect :topLeft tl :botRight br))            
                        (#_FrameRect rect))))
                                        ;(#_draw1control handle)
              ) 
          (if (dialog-item-enabled-p item)
              (#_activatecontrol handle)
              (#_deactivatecontrol handle))
          (let ((splitter (pane-splitter item)))
            (when splitter (view-draw-contents splitter))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;dialog-item-enable
;;
;; Need to patch the system-supplied method for control-dialog-item
;; scroll bars are not visibly enabled unless the window they're on
;; is the top window.

(defmethod dialog-item-enable ((item scroll-bar-dialog-item))
  (unless (dialog-item-enabled-p item)
    (setf (dialog-item-enabled-p item) t)
    (view-activate-event-handler item)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;dialog-item-disable
;;
;; Patch the control-dialog-item method to delay
;; the actual disable during scrolling.
;; This gets around a bug in the Mac ROM where the scroll
;; a control is enabled just before #_TrackControl returns.

                                        ; This is bound to the scroll bar that is currently being tracked.
(defvar *scroll-bar-item* nil)

(defmethod dialog-item-disable ((item scroll-bar-dialog-item))
  (if (eq item *scroll-bar-item*)
      (setf (dialog-item-enabled-p item) nil)
      (call-next-method)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;scroll-bar-proc
;;
;;this is the hook function which is passed to _TrackControl.  The toolbox
;;  will call this function periodically as the control is clicked.
;;
;; It calls track-scroll-bar every time the ROM calls it.
;; The default version of track-scroll-bat updates the
;; scroll bar position according to the scroll-bar-scroll-size or
;; scroll-bar-page-size and calls dialog-item-action.
;; User's may shadow the default method if they need custom behavior.

#-(and)
(defpascal scroll-bar-proc (:ptr sb-handle :word part)
  "This procedure adjusts the control value, and calls dialog-item-action."
  (let ((item *scroll-bar-item*))
    (track-scroll-bar
     item
     (if (eq part #.#$kcontrolIndicatorPart)  ;; thumb
         (outside-scroll-bar-setting item sb-handle)
         (scroll-bar-setting item))
     (case part
       (#.#$kControlUpButtonPart :in-up-button)
       (#.#$kControlDownButtonPart :in-down-button)
       (#.#$kControlPageUpPart :in-page-up)
       (#.#$kControlPageDownPart  :in-page-down)
       (#.#$kcontrolIndicatorPart :in-thumb)
       (t nil)))))



(defun track-scroll-bar-thumb (item)
  (let* ((old-setting (scroll-bar-setting item))
         (min (scroll-bar-min item))
         (max (scroll-bar-max item))
         (horizontal? (eq (scroll-bar-direction item) :horizontal))
         (position (view-position item))
         (last-mouse (rref *current-event* :eventRecord.where))
         (size (view-size item))
         (real-time-tracking (scroll-bar-track-thumb-p item))
         width length old-mouse left right mouse setting)
                                        ; disable periodic tasks that draw
    (progn
      (setf last-mouse
            ;; global-to-local
            (add-points (view-origin item)
                        (subtract-points last-mouse (view-position (view-window item)))))
      (if horizontal?
          (setf width (point-v size)
                length (- (point-h size) width width width)
                left (+ (round (* width 3) 2) (point-h position))
                old-mouse (point-h last-mouse))
          (setf width (point-h size)
                length (- (point-v size) width width width)
                left (+ (round (* width 3) 2) (point-v position))
                old-mouse (point-v last-mouse)))
      (setf right (+ left length))
      (niy track-scroll-bar-thumb item)
      #-(and)
      (loop
        (when (not (#_stilldown))          
          (return))
        (setf mouse (view-mouse-position item))
        (when (eql mouse last-mouse)
          (when (not (wait-mouse-up-or-moved))            
            (return))
          (setf mouse (view-mouse-position item)))
        (unless (eql mouse last-mouse)
          (setf last-mouse mouse)
          (setf mouse (if horizontal? (point-h mouse) (point-v mouse)))
          (setf setting (min max
                             (max min
                                  (+ old-setting
                                     (round (* (- mouse old-mouse) (- max min))
                                            (- right left))))))
          (if real-time-tracking
              (track-scroll-bar item setting :in-thumb)
              (set-scroll-bar-setting item setting))))
      (unless (or real-time-tracking (not setting))
        (track-scroll-bar item setting :in-thumb)))))

                                        
(defmethod track-scroll-bar ((item scroll-bar-dialog-item) value part)
  ;; Returns the new value for the scroll bar
  (set-scroll-bar-setting 
   item
   (case part
     (:in-up-button   (- value (scroll-bar-scroll-size item)))
     (:in-down-button (+ value (scroll-bar-scroll-size item)))
     (:in-page-up     (- value (scroll-bar-page-size item)))
     (:in-page-down   (+ value (scroll-bar-page-size item)))
     (t               value)))
  (dialog-item-action item))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;view-click-event-handler
;;
;;this is the function which is called when the user clicks in the scroll-bar
;;
;;It checks the scroll-bar part, and calls _TrackControl
;;  If appropriate, it passes a hook function to _TrackControl
;;
;;During tracking, dialog-item-action is repeatedly called.
;;


(defmethod view-click-event-handler ((item scroll-bar-dialog-item) where)
  (niy view-click-event-handler item where)
  #-(and)
  (let* ((sb-handle (dialog-item-handle item))
         (part (#_TestControl sb-handle where)))
    (with-timer      
        (cond ((eq part #.#$kcontrolIndicatorPart)  ;; thumb
               (if (scroll-bar-track-thumb-p item)
                   (track-scroll-bar-thumb item)
                   (progn
                     (let ((*scroll-bar-item* item))
                       (#_TrackControl sb-handle where (%null-ptr)))
                     (track-scroll-bar
                      item (outside-scroll-bar-setting item sb-handle) :in-thumb))))
              ((member part '(#.#$kControlUpButtonPart #.#$kControlDownButtonPart
                            #.#$kControlPageUpPart #.#$kControlPageDownPart))
               (let ((was-enabled (dialog-item-enabled-p item)))
                 (let ((*scroll-bar-item* item))
                   (#_TrackControl sb-handle where scroll-bar-proc))
                                        ; The ROM enables on its way out
                 (when (and was-enabled (not (dialog-item-enabled-p item)))
                   (#_deactivatecontrol sb-handle))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;dialog-item-action
;;
;;The default dialog-item-action for a scroll bar calls
;;scroll-bar-changed on the scrollee
;;

(defmethod dialog-item-action ((item scroll-bar-dialog-item))
  (let ((f (dialog-item-action-function item)))
    (if f
        (funcall f item)
        (let ((scrollee (scroll-bar-scrollee item)))
          (when scrollee
            (scroll-bar-changed scrollee item))))
    (niy dialog-item-action item)
    #-(and)
    (with-port-macptr port
      (#_QDFlushPortBuffer port (%null-ptr)))))


(defgeneric scroll-bar-changed (scrollee scroll-bar)
  (:documentation "

The SCROLL-BAR-CHANGED generic function is called by the
DIALOG-ITEM-ACTION method for SCROLL-BAR-DIALOG-ITEM if the
DIALOG-ITEM-ACTION-FUNCTION specified by the :dialog-item-action
initialization argument is nil.  The scrollee argument is the value of
(scroll-bar-scrollee scroll-bar), as set by SET-SCROLL-BAR-SCROLLEE or
the :scrollee initialization argument for SCROLL-BAR.  The default
method does nothing. 

Writing a scroll-bar-changed method is an easy way to cause user mouse
clicks on a scroll-bar dialog item to update another view. 


SCROLLEE:       A scroll-bar scrollee; what is scrolled by the dialog item.

SCROLL-BAR:     A scroll bar.

")
  (:method ((scrollee t) (scroll-bar t))
    (values)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(setf scroll-bar-setting)
;;
;; A nice safe Lisp-level function for changing the value of the scroll-bar
;; The accessor is defined by the DEFCLASS
;;

(defmethod (setf scroll-bar-setting) (new-value (item scroll-bar-dialog-item))
  (set-scroll-bar-setting item new-value))


(defun %set-scroll-bar-setting (item new-value only-if-new-value)
  (setf new-value (max (scroll-bar-min item) (min (scroll-bar-max item) new-value)))
  (niy %set-scroll-bar-setting item new-value only-if-new-value)
  #-(and)
  (unless (and only-if-new-value (eql new-value (scroll-bar-setting item)))
    (let ((handle (dialog-item-handle item)))
      (when handle
        (with-focused-view (view-container item)
          (#_SetControlValue 
           handle 
           (mac-scroll-bar-setting 
            new-value 
            (scroll-bar-min item) 
            (scroll-bar-max item))))))
    (setf (slot-value item 'setting) new-value))
  new-value)


(defgeneric set-scroll-bar-setting (item new-setting)
  (:documentation "

The SET-SCROLL-BAR-SETTING generic function sets the setting of item
to NEW-SETTING.  It does not call DIALOG-ITEM-ACTION.

ITEM:           A scroll-bar dialog item.

NEW-SETTING:    The new setting of item.

")
  (:method ((item scroll-bar-dialog-item) new-setting)
    (check-type new-setting fixnum)
    (%set-scroll-bar-setting item new-setting t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;scroll-bar-min is a :reader for the class
;;here's the setter
;;

(defmethod (setf scroll-bar-min) (new-value (item scroll-bar-dialog-item))
  (set-scroll-bar-min item new-value))


(defgeneric set-scroll-bar-min (item new-value)
  (:documentation "

The SET-SCROLL-BAR-MIN generic function sets the minimum setting of
ITEM to NEW-VALUE.

ITEM:          A scroll-bar dialog item.

NEW-VALUE:     The new minimum setting of item.

")
  (:method ((item scroll-bar-dialog-item) new-value)
    (setf new-value (require-type new-value 'fixnum))
    (unless (eql new-value (scroll-bar-min item))
      (setf (slot-value item 'min) new-value)
      (update-scroll-bar-max-min-setting item))
    new-value))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;scroll-bar-max is a :reader for the class
;;here's the setter
;;
(defmethod (setf scroll-bar-max) (new-value (item scroll-bar-dialog-item))
  (set-scroll-bar-max item new-value))


(defgeneric set-scroll-bar-max (item new-value)
  (:documentation "

The SET-SCROLL-BAR-MAX generic function sets the maximum setting of
ITEM to NEW-VALUE.

ITEM:          A scroll-bar dialog item.

NEW-VALUE:     The new maximum setting of item.

")
  (:method ((item scroll-bar-dialog-item) new-value)
    (setf new-value (require-type new-value 'fixnum))
    (unless (eql new-value (scroll-bar-max item))
      (setf (slot-value item 'max) new-value)
      (update-scroll-bar-max-min-setting item))
    new-value))


(defun update-scroll-bar-max-min-setting (item)
  (niy update-scroll-bar-max-min-setting item)
  #-(and)
  (let ((handle (dialog-item-handle item)))
    (when handle
      (with-focused-view (view-container item)
        (let ((max (scroll-bar-max item))
              (min (scroll-bar-min item))
              (setting (scroll-bar-setting item)))
          (multiple-value-bind (mac-min mac-max) (mac-scroll-bar-min-max min max)
            (let ((mac-setting (mac-scroll-bar-setting setting min max mac-min mac-max)))
              #-carbon-compat
              (setf (href handle :controlrecord.contrlmin) mac-min
                    (href handle :controlrecord.contrlmax) mac-max
                    (href handle :controlrecord.contrlvalue) mac-setting)
              #+carbon-compat
              (progn
                (#_Setcontrolminimum handle mac-min)
                (#_setcontrolmaximum handle mac-max)
                (#_Setcontrolvalue handle mac-setting))
              (invalidate-view item))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;scroll-bar-length
;;
;;this is a variation of view-size
;;
;;It only used one dimension, since scroll-bars almost always have a width
;;  of 16 pixels.
;;

(defgeneric scroll-bar-length (item)
  (:documentation "
The SCROLL-BAR-LENGTH generic function returns the length of item.
")
  (:method ((item scroll-bar-dialog-item))
    (let* ((size (view-size item))
           (splitter (pane-splitter item))
           (splitter-size (and splitter (view-size splitter))))
      (if (eq (scroll-bar-direction item) :horizontal)
          (+ (point-h size) (if splitter (point-h splitter-size) 0))
          (+ (point-v size) (if splitter (point-v splitter-size) 0))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;set-scroll-bar-length
;;
;;sets the length of the scroll-bar
;;
;;Note that because of the way this is implemented, you MUST
;;change the length of a scroll bar with a splitter with
;;set-scroll-bar-length, not by calling set-view-size directly
;;

(defun (setf scroll-bar-length) (new-length scroll-bar-dialog-item)
  (set-scroll-bar-length scroll-bar-dialog-item new-length))

(defgeneric set-scroll-bar-length (item new-length)
  (:documentation "

The SET-SCROLL-BAR-LENGTH generic function sets the length of ITEM to
NEW-LENGTH.

ITEM:           A scroll-bar dialog item.

NEW-LENGTH:     The new length of item.

")
  (:method ((item scroll-bar-dialog-item) new-length)
    (let ((splitter (pane-splitter item))
          (direction (scroll-bar-direction item))
          (inner-length new-length))
      (when splitter
        (let ((size (view-size splitter)))
          (decf inner-length
                (min inner-length
                     (if (eq direction :horizontal) (point-h size) (point-v size))))))
      (set-view-size item (if (eq direction :horizontal)
                              (make-point inner-length (scroll-bar-width item))
                              (make-point (scroll-bar-width item) inner-length)))
      (when splitter
        (let ((dir (scroll-bar-direction splitter))
              (pos (pane-splitter-position item))
              (bar-pos (view-position item)))
          (cond ((and (eq dir :vertical) (member pos '(:bottom  t)))
                 (set-view-position splitter  (make-point (point-h bar-pos)
                                                          (+ (point-v bar-pos)
                                                             inner-length))))
                ((and (eq dir :horizontal) (member pos '(:right t)))
                 (set-view-position splitter (make-point (+ (point-h bar-pos)
                                                            inner-length)
                                                         (point-v bar-pos))))))))              
    new-length))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;scroll-bar-width
;;
;; Sometimes you want a different width
;;

(defgeneric scroll-bar-width (item)
  (:documentation "
The scroll-bar-width generic function returns the width of item.
")
  (:method ((item scroll-bar-dialog-item))
    (let ((size (view-size item)))
      (if (eq (scroll-bar-direction item) :horizontal)
          (point-v size)
          (point-h size)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;set-scroll-bar-width
;;
;;sets the width of the scroll-bar
;;
;;Note that because of the way this is implemented, you MUST
;;change the width of a scroll bar with a splitter with
;;set-scroll-bar-width, not by calling set-view-size directly
;;

(defmethod (setf scroll-bar-width) (new-length (item scroll-bar-dialog-item))
  (set-scroll-bar-width item new-length))


(defgeneric set-scroll-bar-width (item new-width)
  (:documentation "

The SET-SCROLL-BAR-WIDTH generic function sets the width of ITEM to
NEW-WIDTH.

ITEM:           A scroll-bar dialog item.

NEW-VALUE:      The new width of item.

")
  (:method ((item scroll-bar-dialog-item) new-width)
    (let ((size (view-size item)))
      (set-view-size item (if (eq (scroll-bar-direction item) :horizontal)
                              (make-point (point-h size) new-width)
                              (make-point new-width (point-v size)))))
    (let ((splitter (pane-splitter item)))
      (if splitter
          (let ((size (view-size splitter)))
            (set-view-size splitter (if (eq (scroll-bar-direction splitter) :horizontal)
                                        (make-point (point-h size) new-width)
                                        (make-point new-width (point-v size)))))))
    new-width))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(setf scroll-bar-scrollee)
;;
;;Change the scrollee of a scroll-bar
;;

(defun (setf scroll-bar-scrollee) (new-scrollee scroll-bar-dialog-item)
  (set-scroll-bar-scrollee scroll-bar-dialog-item new-scrollee))


(defmethod set-scroll-bar-scrollee ((item scroll-bar-dialog-item) new-scrollee)
  (let ((old-scrollee (scroll-bar-scrollee item)))
    (when old-scrollee
      (delete-view-scroll-bar old-scrollee item)))
  (add-view-scroll-bar new-scrollee item)
  (let ((splitter (pane-splitter item)))
    (if splitter (set-scroll-bar-scrollee splitter new-scrollee)))
  (setf (slot-value item 'scrollee) new-scrollee))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pass set-view-container and set-view-position
;; to the pane-splitter
;;
(defmethod set-view-container ((item scroll-bar-dialog-item) new-container)
  (let ((splitter (pane-splitter item))
        (old-container (view-container item)))
    (when splitter
      (set-view-container splitter new-container))
    (call-next-method)
    (when (and new-container (neq old-container new-container)) 
      (multiple-value-bind (tl br) (scroll-bar-and-splitter-corners item)
        (invalidate-corners new-container tl br)))))


(defmethod set-view-position ((item scroll-bar-dialog-item) h &optional v)  
  (let ((pos (make-point h v))
        (splitter (pane-splitter item))
        (splitter-position (pane-splitter-position item)))
    (setf h (point-h pos) v (point-v pos))
    (when splitter
      (let ((size (view-size item))
            (s-size (view-size splitter)))
        (if (eq (scroll-bar-direction item) :horizontal)
            (if (eq splitter-position :left)
                (progn (set-view-position splitter pos)
                       (incf h (point-h s-size)))
                (set-view-position splitter (+ h (point-h size)) v))
            (if (eq splitter-position :top)
                (progn (set-view-position splitter pos)
                       (incf v (point-v s-size)))
                (set-view-position splitter h (+ v (point-v size)))))))
    (call-next-method item h v)))


(defmethod corrected-view-position ((item scroll-bar-dialog-item))
  (let ((splitter (pane-splitter item)))
    (if (and splitter (member (pane-splitter-position item) '(:top :left)))
        (view-position splitter)
        (view-position item))))

                                        ; Change the relative position of a scroll bar's pane splitter.
                                        ;  :top <-> :bottom
                                        ; :left <-> :right
(defmethod set-pane-splitter-position ((item scroll-bar-dialog-item) pos)
  (let ((position (corrected-view-position item)))
    (setf (slot-value item 'pane-splitter-position) pos)
    (set-view-position item position))
  pos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; set-view-size needs to invalidate the entire scroll bar
;; if it is inactive.
;;
(defmethod set-view-size ((view scroll-bar-dialog-item) h &optional v)
  (declare (ignore h v))
  (without-interrupts
      (prog1
          (call-next-method)
        (let ((w (view-window view)))
          (when w
            (unless (window-active-p w)
              (multiple-value-bind (tl br) (scroll-bar-and-splitter-corners view)
                (invalidate-corners 
                 (view-container view) (add-points tl #@(1 1)) br t))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Methods for pane-splitter
;;
(defmethod initialize-instance ((item pane-splitter) &rest initargs
                                &key (width 16) (length 5) (direction :vertical))
  (declare (dynamic-extent initargs))
  (let ((size (if (eq direction :vertical)
                  (make-point width length)
                  (make-point length width))))
    (apply #'call-next-method
           item
           :view-size size
           :direction direction
           initargs)))

(defparameter *gray-color*      #x808080) ;; from color.lisp - boot prob
(defparameter *dark-gray-color* #x404040) ;; ditto

(defmethod view-draw-contents ((item pane-splitter))
  (let ((active-p (window-active-p (view-window item))))
    (let* ((tl (view-position item))
           (br (add-points tl (view-size item))))
      (niy view-draw-contents item)
      #-(and)
      (rlet ((r :rect :topleft tl :botright br))
            (with-fore-color (if active-p *dark-gray-color* *gray-color*)
              (#_paintrect r))))))


(defmethod pane-splitter-limiting-container ((scrollee simple-view))
  (view-window scrollee))


(defmethod view-click-event-handler ((item pane-splitter) where)
                                        ;(declare (ignore where))
  (let* ((scrollee (or (scroll-bar-scrollee item) (view-window item)))
         (container (pane-splitter-limiting-container scrollee))
         (scroll-bar (scroll-bar item)))
    (when container
      (when (double-click-p)
        (when (pane-splitter-handle-double-click scrollee item where)
          (return-from view-click-event-handler)))
      (multiple-value-bind (s-tl s-br)
          (pane-splitter-corners scrollee scroll-bar)
        (let* ((direction (scroll-bar-direction item))
               (mouse-pos (convert-coordinates where (view-container item) container)) ;(view-mouse-position container))
               min max min-pos max-pos drawn pos-accessor line-direction delta pos)
          (if (eq direction :vertical)
              (setf min (1+ (point-h s-tl))
                    max (- (point-h s-br) 2)
                    min-pos (1+ (point-v s-tl))
                    max-pos (- (point-v s-br) 2) 
                    pos-accessor #'point-v
                    line-direction :horizontal)
              (setf min (1+ (point-v s-tl))
                    max (- (point-v s-br) 2)
                    min-pos (1+ (point-h s-tl))
                    max-pos (- (point-h s-br) 2)
                    pos-accessor #'point-h
                    line-direction :vertical))
                                        ; Compute the initial position for the outline.
                                        ; All this rigamarole is to convert from the window's coordinate system
                                        ; to the scrollee's and back again.
          (setf pos
                (let ((pos (pane-splitter-outline-position 
                            scrollee scroll-bar
                            (convert-coordinates mouse-pos container scrollee))))
                  (funcall pos-accessor
                           (convert-coordinates
                            pos
                            scrollee
                            container)))
                delta (- pos (funcall pos-accessor mouse-pos)))
                                        ; Now loop until mouse up.
          (flet ((draw-line (pos)
                   (draw-pane-splitter-outline
                    scrollee scroll-bar pos min max line-direction)))
            (declare (dynamic-extent #'draw-line))
            (multiple-value-setf (pos drawn)
              (track-and-draw container #'draw-line pos direction delta min-pos max-pos)))
                                        ; Convert back to scrollee's coordinate system
          (setf pos (funcall pos-accessor (convert-coordinates 
                                           (if (eq direction :horizontal)
                                               (make-point pos 0)
                                               (make-point 0 pos))
                                           container 
                                           scrollee)))
                                        ; And call the user method to actually do something.
          (split-pane scrollee scroll-bar pos direction drawn))))))


                                        ; This controls the position of the outline when the mouse is first clicked.
                                        ; mouse-position is the position of the mouse in the coordinate system of
                                        ; the scrollee.
                                        ; The default method draws the outline right where the mouse is.
(defmethod pane-splitter-outline-position (scrollee scroll-bar mouse-position)
  (declare (ignore scrollee scroll-bar))
  mouse-position)


(defmethod draw-pane-splitter-outline (scrollee scroll-bar pos min max direction)
  (declare (ignore scrollee scroll-bar))
  (niy draw-pane-splitter-outline scrollee scroll-bar pos min max direction)
  #-(and)
  (if (eq direction :horizontal)
      (progn (#_MoveTo min pos)
             (#_LineTo max pos))
      (progn (#_MoveTo pos min)
             (#_LineTo pos max))))

                                        ; Some users may want to specialize on this
(defmethod pane-splitter-corners ((scrollee simple-view) scroll-bar)
  (declare (ignore scroll-bar))
  (let* ((window (view-window scrollee))
         (container (view-container scrollee)))
    (multiple-value-bind (tl br) (view-corners scrollee)
      (when (and container (neq container window))
        (setf tl (convert-coordinates tl container window)
              br (convert-coordinates br container window)))
      (values tl br))))

                                        ; This is the method that all users will specialize on if they
                                        ; want a pane-splitter to do anything but draw a line.
(defmethod split-pane ((scrollee simple-view) scroll-bar pos direction inside-limits)
  (declare (ignore scroll-bar pos direction inside-limits)))

                                        ; Users need to specialize this if they want double clicks to do anything
                                        ; It should return true to denote that it handled the double click.
                                        ; Otherwise, the double click will be treated just like a single click.
(defmethod pane-splitter-handle-double-click ((scrollee simple-view) pane-splitter where)
  (declare (ignore pane-splitter where))
  nil)

;;;;;;;;;;;;;;;
;; support for mouse wheel maybe?


(defvar *wheel-scroll-factor* nil "Integer multiplier for both horizontal and vertical scrolling in case
  you need additional speed beyond that provided by the 'Scrolling Speed' item in the Mouse preference
  pane, or in cases where that option is not provided. Set to nil or 1 for no effect.")

(defvar *horizontal-wheel-scroll-factor* 10 "Quantum for horizontal wheel scrolling. Essentially
   the minimum number of pixels to shift when horizontal scrolling. This value is multiplied
   by *wheel-scroll-factor* (if non-nil) for horizontal speed adjustment, thus this value is
   allowed to be a float <1 so you can slow down horizontal scrolling if necessary. Set to nil
   or 1 for no effect. It is meaningful to have a non-nil value here even if *wheel-scroll-factor*
   is nil.")

                                        ; If you have a 'Scrolling Speed' item in your Mouse preference pane, use it. The defaults above should suffice.
                                        ; If you don't have this preference, the defaults may feel too slow. Try setting both to 4 and go from there.
                                        ; Set *wheel-scroll-factor* to 0 to globally disable mouse wheel scrolling.

(defmethod adjust-horizontal-wheel-speed ((w t) delta)
  "Specialize if needed for different types of windows."
  (if (numberp *horizontal-wheel-scroll-factor*)
      (round (* delta *horizontal-wheel-scroll-factor*))
      delta))

(defmethod scroll-wheel-handler ((w t) delta direction wherep)
                                        ; belt and suspenders
  (declare (ignore delta direction wherep))
                                        ;(format t "Attempting to wheel scroll ~S" w)
  0 ; #$noerr
  )


(defmethod scroll-wheel-handler ((w simple-view) delta direction wherep)
  "Default method for any viewlike thing that doesn't have its own specialized handler. Just
   punt back out to the handler for the enclosing window."
                                        ;(format t "Attempting to wheel scroll ~S" w)
  (setf w (view-window w))
  (if w
      (scroll-wheel-handler w delta direction wherep)
      0 ; #$noerr
      ))



(defmethod scroll-wheel-handler ((w window) delta direction wherep)
  "Default handler for Fred windows and most everything else. Now makes
   both horizontal (shift-wheel) and vertical scrolling in Fred windows instantaneous."
  (declare (ignore-if-unused wherep))
  (niy scroll-wheel-handler w delta direction wherep)
  #-(and)
  (let ((res #$eventNotHandledErr))
    (progn ;with-port (wptr w)
      (let ((window-pos))
                                        ;(#_globaltolocal wherep)
                                        ;(setf window-pos (%get-point wherep))
        (setf window-pos (view-mouse-position w))
        (let ((scroll-bar (find-scroll-bar-controlling-point w direction window-pos)))
          (when scroll-bar
            (cond ((and (typep scroll-bar 'fred-v-scroll-bar) ; this is essential
                        (eq direction :vertical))             ; but this may be redundant?
                                        ; Don't use a loop on the commonest and most speed-sensitive cases
                   (let* ((view (scroll-bar-scrollee scroll-bar))
                          (frec (frec view)) ; meaningless for non fred-v-scroll-bars
                          (mark (fred-display-start-mark view))
                          )
                     (set-fred-display-start-mark
                      view
                      (frec-screen-line-start frec mark (- delta))
                      )))
                  ((and (typep scroll-bar 'fred-h-scroll-bar)
                        (eq direction :horizontal))            ; ditto
                   (let* ((view (scroll-bar-scrollee scroll-bar))
                          (hscroll (fred-hscroll view)))
                     (declare (fixnum hscroll))
                     (set-fred-hscroll view
                                       (if (and (= 0 hscroll)
                                                (< delta 0))
                                           (fred-margin view)
                                           (- hscroll (adjust-horizontal-wheel-speed w delta))))
                     (fred-update view)
                     #+ignore
                     (when (osx-p)
                       ;; work around crock - wish I knew why needed
                                        ;(view-draw-contents scroll-bar) ; doesn't seem needed here
                       )
                     ))
                  (t ; do it the old way on all other cases
                   (when delta
                     (dotimes (count (abs delta)) 
                       (track-scroll-bar scroll-bar (scroll-bar-setting scroll-bar)
                                         (if (minusp delta) :in-down-button :in-up-button))))))
            (setf res #$noerr)))))
    res))

#-(and)
(defpascal scroll-wheel-handler-proc (:ptr targetref :ptr eventref :word)
  (declare (ignore targetref))
  (let ((res #$eventNotHandledErr) direction delta)
    (rlet ((axis :unsigned-integer)
                                        ;(out-type :ptr)
           (wherep :point))
          (#_GetEventParameter eventref #$keventParamMouseWheelAxis #$typeMouseWheelAxis (%null-ptr) 2 (%null-ptr) axis)
          (let* ((the-axis (%get-word axis)))
            (setf direction
                  (if (eq the-axis #$kEventMouseWheelAxisX)
                      :horizontal
                      (if (eq the-axis #$kEventMouseWheelAxisY)
                          :vertical))))
          (when direction
                                        ;#+ignore ;; wherep not used?
            (#_GetEventParameter eventref #$kEventParamMouseLocation #$typeQDPoint (%null-ptr)
                                 (record-length :point) (%null-ptr) wherep)
            (rlet ((deltap :signed-long))
                  (#_GetEventParameter eventref #$kEventParamMouseWheelDelta #$typeLongInteger (%null-ptr)
                                       (record-length :signed-long)  (%null-ptr) deltap)
                  (setf delta (%get-signed-long deltap)))
            (when (integerp *wheel-scroll-factor*)
              (setf delta (* delta *wheel-scroll-factor*)))
                                        ; NB: delta is affected both by how fast you spin the wheel and by the Scrolling Speed setting
                                        ;     in the Mouse preference pane.
            (let ((w (find-view-containing-point nil (%get-point wherep) ))) ; Look for deepest view first.
                                        ; If it doesn't have its own handler, bounce back up to the window handler.
              (when w
                (setf res (scroll-wheel-handler w delta direction wherep))
                ))))
    res     
    ))


;;;; THE END ;;;;
