;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pop-up-menu-dialog-item.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Pop Up Menu Dialog Item.
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


(defclass pop-up-menu (menu simple-view)
  ((width-correction
    :allocation :class
    :initform 0
    :accessor pop-up-menu-width-correction)
   (menu-rect
    :initform nil
    :accessor pop-up-menu-rect)
   (title-rect
    :initform nil
    :accessor pop-up-menu-title-rect)
   (default-item
    :initform 1
    :initarg  :default-item
    :accessor pop-up-menu-default-item
    :documentation "
An integer identifying the default item that will be selected from the
menu.  The default is 1.  The first item is 1, not 0.
")
   (auto-update-default
    :initarg  :auto-update-default
    :initform t
    :accessor pop-up-menu-auto-update-default
    :documentation "
An argument specifying how defaults are handled. If true (the
default), each time an item is selected from the popup menu, it
becomes the default.  Otherwise, the default item remains fixed.
")
   (item-display
    :initarg  :item-display
    :initform :selection
    :accessor pop-up-menu-item-display
    :documentation "
An argument specifying whether the menu item or its value is
displayed.  If the value is :selection (the default), displays the
default menu item.  Otherwise the value itself is displayed as if by
\(format t \"~a\" value).
")   
   (control-handle
    :initform nil
    :accessor control-handle)
   (cached-title
    :initform nil
    :accessor pop-up-menu-cached-title))
  (:default-initargs :view-position nil :view-size nil :menu-title ""))


(defparameter *use-pop-up-control* t)


(defgeneric use-pop-up-control (menu)
  (:method ((menu pop-up-menu))  
    *use-pop-up-control*))


(defun make-new-pop-up-control (menu &optional (window (view-window menu)))
  (niy make-new-pop-up-control menu window) #-(and)
  (with-pstrs ((sp ""))  ;; we draw the title if any for highlighting
    (rlet ((fake-rect :rect))
      (setf (pref fake-rect :rect.topleft) #@(-3000 -3000))
      (setf (pref fake-rect :rect.botright) (add-points #@(-3000 -3000) (view-size menu)))
      (let ((handle (#_NewControl 
                     (wptr window) 
                     fake-rect ;(pop-up-menu-rect menu)
                     sp
                     nil                             ;; visible
                     #$popupTitleLeftJust 
                     (slot-value menu 'menu-id) 
                     0                                ;; max <- pixel width of menu title
                     (logior #$kControlPopupButtonProc #$kControlPopupFixedWidthVariant #$kControlPopupUseWFontVariant)                                   
                     0)))
        (setf (control-handle menu) handle)
        (let ((default-item (pop-up-menu-default-item menu)))
          (when default-item
            (#_setcontrolvalue handle default-item)))  ;; shouldn't this do the gag song and dance too.
        (set-view-position menu (view-position menu))
        ;;(#_setcontrolpopupmenuhandle handle (menu-handle menu)) ;; does nada
        ;;(#_setcontrolpopupmenuid handle (menu-id menu))
        )
      (maybe-add-menu-height menu)
      (unless (slot-value menu 'enabledp)
        ;;sync up what we believe with what the mac believes.
        (#_deactivatecontrol (control-handle menu))))))


(defmethod install-view-in-window :after ((menu pop-up-menu) window)
  (when (use-pop-up-control menu)
    (let ((item-disp (pop-up-menu-item-display menu)))
      (when (not (or (eq  item-disp :selection)
                     (equal item-disp "")))
        (warn "*use-pop-up-control* and item display ~s are incompatible." item-disp)))
    (make-new-pop-up-control menu window)))





(defun maybe-add-menu-height (menu)
  (let ((vh (point-v (view-size menu)))
        (dh (point-v (view-default-size menu))))
    (when (< dh vh)
      (add-menu-height menu (- vh dh)))))


(defun add-menu-height (pop-up-menu height)
  ;; does make it bigger for all the good that does
  ;; seems to add to default height rather than current
  (niy add-menu-height pop-up-menu height)
  #-(and)
  (let ((handle (control-handle pop-up-menu)))
    (when handle
      (rlet ((hgt :word height))
        (errchk (#_SetControlData handle 0  #$kControlPopupButtonExtraHeightTag 2 hgt))))))


(defmethod view-default-font ((menu pop-up-menu))
  (sys-font-spec))


(defgeneric the-empty-menu-string (menu)
  (:method ((menu pop-up-menu))
    "<No items>"))


(defmethod print-object ((thing pop-up-menu) stream)
  (declare (stepper disable))
  (print-parseable-object (thing stream :type t :identity t)
                          (:title (let ((title (menu-title  thing)))
                                    (if (and title (not (equal title "")))
                                        title
                                        (get-menu-body-text thing)))))
  ;; (print-unreadable-object (thing stream)
  ;;   (format stream "~S ~S"
  ;;           (class-name (class-of thing))
  ;;           (let ((title (menu-title  thing)))
  ;;             (if (and title (not (equal title "")))
  ;;                 title
  ;;                 (get-menu-body-text thing)))))
  )

                                        ;------

(defgeneric set-pop-up-menu-default-item (menu num &optional force)
  (:method ((menu pop-up-menu) num &optional force)
    (let* ((old (pop-up-menu-default-item menu))
           (items (menu-items menu)))    
      (prog1 num  
        (when (or force (/= old num))
          (when (and (/= old 0) items)
            (set-pop-up-item-check-mark (nth (1- old) items) nil))
          (when (and num (/= num 0) items)
            (set-pop-up-item-check-mark (nth (1- num) items) t))
          (setf (pop-up-menu-default-item menu) num)
          (when (eq (pop-up-menu-item-display menu) :selection)
            (niy set-pop-up-menu-default-item menu num force)
            #-(and)

            (let ((window (view-window menu))
                  (handle (control-handle menu)))
              (with-focused-dialog-item (menu window)
                (when handle
                  (cond 
                    ((not (eq (view-container menu) window))
                     (let ((old-rect (pop-up-menu-rect menu)))
                       (rlet ((gag-rect :rect))
                         (setf (rref gag-rect :rect.topleft) (convert-coordinates (rref old-rect :rect.topleft) (view-container menu) window))
                         (setf (rref gag-rect :rect.botright) (convert-coordinates (rref old-rect :rect.botright) (view-container menu) window))
                         (without-interrupts
                           (#_setcontrolbounds handle gag-rect)
                           (#_setcontrolvalue handle num)
                           (#_setcontrolbounds handle old-rect)))))                 
                    (t (#_setcontrolvalue handle num))))))
            (invalidate-view menu)))))))


(defmethod install-view-in-window ((menu pop-up-menu) dialog &aux ok)  
  (declare (ignore dialog))  
  (menu-install menu)
  (without-interrupts ; this is what dialog-item buys us
    (unwind-protect
         (let ((container (view-container menu)))
           (set-default-size-and-position menu container)
           (setf ok t))
      (unless ok
        (set-view-container menu nil))))
  (size-rectangles menu)
  (invalidate-view menu))




;;;;;;;;;;;;;;;;;;;;
;;
;;  definitions for pop-up menus
;;


(defmethod initialize-instance :after ((menu pop-up-menu) &rest initargs &key highlight-title)
  (declare (ignore initargs))
  (when highlight-title (view-put menu :highlight-title t))
  (let ((default (pop-up-menu-default-item menu)))    
    (when (and default (/= default 0))
      (setf default (nth (1- default) (menu-items menu)))
      (when default (set-pop-up-item-check-mark default t)))))


;;------------
;; Geometry


(defmethod set-view-size ((menu pop-up-menu) h &optional v)
  (with-focused-dialog-item (menu)        
    (invalidate-view menu t)       
    (call-next-method)
    (size-rectangles menu)
    (niy set-view-size menu h v)
    #-(and)
    (when (control-handle menu)
      (#_setcontrolbounds (control-handle menu)(pop-up-menu-rect menu))
      (maybe-add-menu-height menu))
    (setf (pop-up-menu-cached-title menu) nil)
    (invalidate-view menu t)))


(defmethod set-view-position ((menu pop-up-menu) h &optional v)
  (with-focused-dialog-item (menu)        
    (invalidate-view menu t)        
    (call-next-method)
    (size-rectangles menu)
    (niy set-view-position menu h v)
    #-(and)
    (when (control-handle menu)
      (#_setcontrolbounds (control-handle menu)(pop-up-menu-rect menu))
      (maybe-add-menu-height menu))
    (invalidate-view menu t)))


(defgeneric size-rectangles (menu)
  (:documentation "does a lot of tweaking to get the thing to draw right")
  (:method ((menu pop-up-menu))
    (let* ((my-pos (view-position menu))
           (my-size (view-size menu)))
      (when (and my-pos my-size)
        (setf my-size (add-points my-size (cond
                                            ((pull-down-menu-p menu) #@(0 -1))
                                            ((control-handle menu)   #@(0 0))
                                            (t                       #@(-1 -1))))) 
        (niy size-rectangles menu)
        #-(and)
        (let* ((text (menu-title menu))
               (title-offset (make-point (if (zerop (length text))
                                             0
                                             (+ 8 (string-width
                                                   text
                                                   (or (menu-font menu)
                                                       (view-font (view-window menu))))))
                                         0))
               (menu-rect (or (pop-up-menu-rect menu)
                              (setf (pop-up-menu-rect menu) (make-record :rect))))
               (title-rect (or (pop-up-menu-title-rect menu)
                               (setf (pop-up-menu-title-rect menu)
                                     (make-record :rect)))))
          (rset menu-rect :rect.topleft (add-points my-pos title-offset))
          (rset menu-rect :rect.bottomright (add-points my-pos my-size))
          (rset title-rect :rect.topleft (add-points my-pos #@(0 1)))
          (rset title-rect :rect.bottomright (make-point (+ (point-h my-pos)
                                                            title-offset)
                                                         (+ (point-v my-pos)
                                                            (point-v my-size)
                                                            -2))))))))

(defmethod view-corners ((item pop-up-menu))
  (niy view-corners item)
  #-(and)
  (if (and #|(osx-p)|# (control-handle item)) ;(ccl::editing-dialogs-p (view-window item)))
      (let ((rgn ccl::*temp-rgn-3*))
        (#_getcontrolregion (ccl::control-handle item) #$kControlStructureMetaPart rgn)
        (multiple-value-bind (tl br) (ccl::get-region-bounds-tlbr rgn)        
          (let ((btl (call-next-method)))  ;; maybe it has a title
            (setf tl (make-point (min (point-h tl)(point-h btl)) (point-v tl)))
            (values tl br))))
                                        ;(Multiple-value-bind (tl br) (call-next-method)
                                        ;  (values (subtract-points tl #@(2 2)) (add-points br #@(10 4))))
      (call-next-method)))


(defmethod view-default-size ((menu pop-up-menu))
  (multiple-value-bind (ff ms) (font-codes (menu-font menu))
    (let* ((item-display   (slot-value menu 'item-display))
           (max-menu-width (max 10 (font-codes-string-width (get-menu-body-text menu) ff ms)))
           (title          (menu-title menu))
           (title-width    (if (zerop (length title))
                               0
                               (font-codes-string-width title ff ms)))
           (fudge          0))
      (when (use-pop-up-control menu) ;; the arrows thing is bigger in this case
        (setf fudge 2))
      (setf max-menu-width
            (+ (if (zerop title-width) 9 18)
               fudge
               ;; we used to dolist always
               (if (eq item-display :selection)
                   (let ((item-max (max-menu-width menu)))
                     (if (> item-max max-menu-width)
                         item-max
                         max-menu-width))
                   max-menu-width)))
      (multiple-value-bind (a d w l) (font-codes-info ff ms)
        (let ((height (+ a d l 4)))
          (make-point (+  title-width  max-menu-width (if (pull-down-menu-p menu) 5 (+ 12 w))) ; was 15
                      (if (use-pop-up-control menu)
                          (max 20 height)
                          height)))))))



(defmethod view-font-codes-info ((menu pop-up-menu))
  (if (slot-value menu 'menu-font)
      (multiple-value-bind (ff ms) (font-codes (slot-value menu 'menu-font))  ;; ugh
        (font-codes-info ff ms))
      (multiple-value-call (function font-codes-info) (view-font-codes menu))))


(defmethod point-in-click-region-p ((menu pop-up-menu) where)
  ;; prevent changing selection when mouse doesn't move.
  (when (view-contains-point-p menu where)
    (let* ((vh (point-v (view-size menu)))
           (lh vh) ;(view-font-line-height menu))  ;; << odd behavior with use-pop-up-control because the menu may be bigger than it needs to be
           (rect (pop-up-menu-rect menu)))
      (declare (ignore lh rect))
      (niy point-in-click-region-p menu where)
      #-(and)
      (and (< (rref rect :rect.left) (point-h where)
              (- (rref rect :rect.right) (menu-display-h-offset menu)))
           (or (< vh (+ 2 lh))
               (let* ((v-where (point-v where))
                      (v-pos (point-v (view-position menu)))
                      (offset (menu-display-v-offset menu)))
                 (declare (fixnum v-where v-pos offset))
                 (and (> v-where (+ offset v-pos))
                      (< v-where
                         (+ v-pos lh offset)))))))))



(defgeneric menu-display-v-offset (menu)
  (:method ((menu pop-up-menu))
    1))


(defgeneric menu-display-h-offset (menu)
  (:method ((menu pop-up-menu))
    1))


;;-------------
;; Appearance

(defmethod part-color ((menu pop-up-menu) key)
  (or (getf (slot-value menu 'color-list) key nil)
      (case key (:menu-body (if (control-handle menu) nil *white-color*)))))



(defgeneric draw-menu-title (menu)
  (:method ((menu pop-up-menu))
    ;; for both pull-down and pop-up
    (let* ((text (menu-title menu))
           (no-title (or (null  text)(equal text ""))))
      (unless no-title
        (let* ((ti-rect (pop-up-menu-title-rect menu))
               (enabled (menu-enabled-p menu))
               (colorp (and (color-or-gray-p menu) (window-color-p (view-window menu))))
               (disabled-color (if (and (not enabled) colorp)
                                   *gray-color*))
               (title-color (or disabled-color
                                (part-color menu :menu-title))))
          (declare (ignore ti-rect title-color))
          (multiple-value-bind (ff ms)(view-font-codes menu)
            (let ((lineheight (font-codes-line-height ff ms)))
              (declare (ignore lineheight))
              (progn ;with-fore-color title-color ; 21-Jun-91 -wkf  ; draw-string does it
                (niy draw-menu-title menu)
                #-(and)
                (with-back-color (part-color menu :title-background)
                  (#_EraseRect  ti-rect)
                  (rlet ((my-rect :rect))
                    (copy-record ti-rect :rect my-rect)
                    (let* ((rect-height (- (pref my-rect :rect.bottom)(pref my-rect :rect.top)))
                           (delta (max 0 (ash (- rect-height lineheight) -1))))
                      (setf (pref my-rect :rect.top) (+ (pref my-rect :rect.top) delta))) 
                    (draw-string-in-rect text  my-rect :ff ff :ms ms :color title-color)))))))))))


(defmethod view-draw-contents ((menu pop-up-menu))
  (with-focused-dialog-item (menu)
    (draw-menu-title menu)
    (niy view-draw-contents menu)
    #-(and)
    (let ((handle (control-handle menu)))
      (if handle
          (progn ;with-focused-view menu
            (if (#_iscontrolvisible handle)
                (#_Draw1Control handle)
                (#_ShowControl handle)))))))




(defun paint-menu-gray (menu)
  (let* ((ti-rect (pop-up-menu-title-rect menu))
         (no-title (equal (menu-item-title menu) "")))
    (declare (ignore ti-rect no-title))
    (niy paint-menu-gray menu)
    #-(and)
    (rlet ((ps :penstate))
      (with-item-rect (rect menu)
        (#_InsetRect rect 0 -1)
        (#_GetPenState ps)
        (#_PenPat *gray-pattern*)
        (#_PenMode 11)
        (#_PaintRect rect)
        (unless no-title (#_PaintRect ti-rect)) ; ??
        (#_SetPenState ps)))))


(defun get-menu-body-text (menu)
  (let ((item-display (pop-up-menu-item-display menu)))
    (cond ((eq item-display :selection)
           (let* ((selection (pop-up-menu-default-item menu))
                  (items (menu-items menu)))
             (cond ((null items) (the-empty-menu-string menu))
                   ((zerop selection) "<No selection>")
                   (t (menu-item-title (nth (1- selection) items))))))
          ((stringp item-display)
           item-display)
          (t 
           (format nil "~a" item-display)))))

;;-------------
;; Click event handling - sez who

(defmethod view-activate-event-handler ((menu pop-up-menu))  
  (when (and (menu-enabled-p menu)(control-handle menu))
    (with-focused-dialog-item (menu)
      (niy view-activate-event-handler menu)
      #-(and)
      (#_activatecontrol (control-handle menu)))))

(defmethod view-deactivate-event-handler ((menu pop-up-menu))  
  (when (and (menu-enabled-p menu)(control-handle menu))
    (with-focused-dialog-item (menu)
      (niy view-deactivate-event-handler menu)
      #-(and)
      (#_deactivatecontrol (control-handle menu)))))


(defmethod menu-disable ((menu pop-up-menu))
  (when (menu-enabled-p menu)
    (invalidate-view menu)
    (when (control-handle menu)
      (with-focused-dialog-item (menu)
        (niy menu-disable menu)
        #-(and)
        (#_deactivatecontrol (control-handle menu)))) ;(#_HiliteControl (control-handle menu) 255)))
    (call-next-method)))

(defmethod menu-enable ((menu pop-up-menu))
  (when (not (menu-enabled-p menu))
    (invalidate-view menu)
    (when (control-handle menu)
      (with-focused-dialog-item (menu)
        (niy menu-enable menu)
        #-(and)
        (#_activatecontrol (control-handle menu)))) ;(#_HiliteControl (control-handle menu) 0)))
    (call-next-method)))


(defmethod dialog-item-enable ((menu pop-up-menu))
  (menu-enable menu))


(defmethod dialog-item-disable ((menu pop-up-menu))
  (menu-disable menu))


(defmethod view-click-event-handler ((menu pop-up-menu) where)
  (declare (ignore where))
  (when (menu-enabled-p menu)
    (let ((no-text (eq 0  (length (menu-title menu)))))    
      (unless no-text ; want to hilite not invert
        (title-hilite menu))
      (allow-view-draw-contents (view-window menu))
      (with-focused-view nil
        (menu-select menu 0))        
      (unless no-text
        (title-hilite menu t)))))

(defgeneric title-hilite (menu &optional un)
  (:method ((menu pop-up-menu) &optional un)
    (when (view-get menu :highlight-title)
      (let ((rect (pop-up-menu-title-rect menu)))
        (when rect
          (if un
              (invalidate-view menu)
              (niy title-hilite menu un)
              #-(and)
              (progn
                (#_LMSetHiliteMode (%ilogand2 #x7f (#_LMGetHiliteMode)))
                (with-focused-view (view-container menu)
                  (#_InvertRect rect)))))))))

(defmethod set-part-color ((menu pop-up-menu) part color)
  (declare (ignore part))
  (call-next-method)
  (invalidate-view menu)
  color)


;;-------------
;; Adding/removing items

(defmethod add-menu-items ((menu pop-up-menu) &rest items)
  ;; this is stupidly inefficient when control-handle
  (call-next-method)
  (when (pop-up-menu-auto-update-default menu)
    (let* ((default (pop-up-menu-default-item menu))
           (items (menu-items menu)))
      (when items       
        (when nil
          (dolist (item items)
            (when (menu-item-check-mark item)
              (set-menu-item-check-mark item nil))))
        (cond ((and default (/= default 0))
               ;; (set-pop-up-menu-default-item menu 0)
               (set-pop-up-menu-default-item menu default))
              (t (set-pop-up-menu-default-item menu 1))))))
  (niy add-menu-items menu items)
  #-(and)
  (let ((handle (control-handle menu)))
    (when handle
      (let ((items (menu-items menu)))
        (with-focused-dialog-item (menu)
          (#_SetControl32bitMaximum handle (length items))
          (let ((default (pop-up-menu-default-item menu)))
            (when default
                                        ;(set-pop-up-menu-default-item menu 0)
              (set-pop-up-menu-default-item menu default t))))))))


(defun inverse-style-arg (arg)  
  (if (zerop arg)
      (dolist (e *style-alist* nil)
        (when (zerop (cdr e))
          (return (car e))))
      (let ((val))
        (dotimes (i 7)
          (when (logbitp i arg)
            (let* ((n (ash 1 i))
                   (it (dolist (e *style-alist* nil)
                         (when (= n (cdr e))
                           (return (car e))))))
              (when it
                (if (consp val)
                    (push it val)
                    (if val
                        (setq val (list it val))
                        (setq val it)))))))
        (or val (error "~S: Invalid style ~S" 'inverse-style-arg  arg)))))

(defmethod add-menu-items :after ((menu pop-up-menu) &rest items)
  (declare (dynamic-extent items))
  ;; fix this mess or nuke it and do in main method
  ;; nb changing the font of a menu on the fly wont do item style today
  (let ((style-num (ash (logand (view-font-codes menu) #xffff) -8)))
    (unless (eq style-num (cdr (assoc :plain *style-alist*)))
      (let ((style (inverse-style-arg style-num)))
        (dolist (i items)
          (when (not (menu-item-style i))
            (set-menu-item-style i style))))))
  (setf (pop-up-menu-cached-title menu) nil))



(defmethod remove-menu-items :after ((menu pop-up-menu) &rest ignore)
  (declare (ignore ignore))
  (setf (pop-up-menu-cached-title menu) nil))


(defmethod remove-menu-items :around ((menu pop-up-menu) &rest items)
  (let* ((default-item-number (pop-up-menu-default-item menu))
         (default-item (unless (eql default-item-number 0)
                         (nth (1- default-item-number) (menu-items menu)))))
    (when default-item (set-pop-up-menu-default-item menu 0))
    (unwind-protect
         (call-next-method)
      (when (and default-item (menu-items menu))
        (let ((index (position default-item (menu-items menu))))
          (set-pop-up-menu-default-item menu (if index (1+ index) 1))))
      (niy remove-menu-items menu items)
      #-(and)
      (let ((handle (control-handle menu)))
        (when handle
          (let ((items (menu-items menu)))
            (with-focused-dialog-item (menu)
              (#_SetControl32bitMaximum handle (length items))
              (let ((default (pop-up-menu-default-item menu)))
                (when default
                                        ;(set-pop-up-menu-default-item menu 0)
                  (set-pop-up-menu-default-item menu default t))))))))))


;;---------------
;; Selection

(defgeneric selected-item (menu)
  (:method ((menu pop-up-menu))
    (nth (1- (pop-up-menu-default-item menu)) (menu-items menu))))

(defmethod menu-select ((menu pop-up-menu) num)
  ;; Update the menu's items then displays the pop-menu.  Default-item is the
  ;; item which will come up selected  when the menu is displayed.
  (declare (ignore num))
  (let (selection
        selected-menu
        selected-menu-item
        (a-rect (pop-up-menu-rect menu))
        (pos (with-focused-view (view-container menu)
               #-(and)
               (local-to-global
                (wptr menu)
                (rref a-rect :rect.topleft)))))
    (declare (ignore selection a-rect pos))
    (menu-update menu)
    (multiple-value-bind (ff ms) (view-font-codes menu)
      (declare (ignore ff ms))
      (let* (;; (handle (menu-handle menu))
             (cached-title (pop-up-menu-cached-title menu))
             (items (menu-items menu))
             (orig (if items (menu-item-title (car items)))))
        (declare (ignore orig))
        (when (and (not cached-title) items)
          (setf cached-title (adjusted-menu-item-title menu)))
        (niy menu-select)
        #-(and)
        (unwind-protect
             (progn ;with-focused-view nil
               (when cached-title (set-menu-item-title (car items) cached-title))
               (when t ;(not (menu-font menu)) ;unless same-p            
                 ;; may not be needed?
                 (#_setmenufont handle (ash ff -16) (logand ms #xffff)))
               (setf selection (#_PopUpMenuSelect
                                handle
                                (+ (point-v pos) (menu-select-v-offset menu)(menu-display-v-offset menu))
                                (+ (point-h pos) (menu-display-h-offset menu))
                                (or (pop-up-menu-default-item menu) 0))
                                        ;we get the selected menu in case you want to break the rules
                                        ;and use heirarchical menus in a pop-up menu
                     selected-menu (menu-object (ash (logand #xFFFF0000 selection) -16))
                     selected-menu-item (logand #x0000FFFF selection)))                  
          (when cached-title
            (set-menu-item-title (car items) orig)
            (setf (pop-up-menu-cached-title menu) cached-title)))
        #-(and)
        (when (typep menu 'pull-down-menu) ; gag-puke
          (inval-window-rect (wptr menu) (pop-up-menu-rect menu)))
        (unless (eq selected-menu-item 0)
          (let* ((items (menu-items selected-menu))
                 (update (pop-up-menu-auto-update-default menu)))
            (when update
              (set-pop-up-menu-default-item menu
                                            (if (eq selected-menu menu)
                                                selected-menu-item
                                                (let ((1st-level-submenu selected-menu))
                                                  (loop
                                                    (let ((owner (menu-owner 1st-level-submenu)))
                                                      (if (eq owner menu)
                                                          (return (1+ (position 1st-level-submenu (menu-items menu)))))
                                                      (if (null owner)
                                                          (return (pop-up-menu-default-item menu)))
                                                      (setf 1st-level-submenu owner)))))))
            (with-event-processing-enabled 
                (when update (event-dispatch))  ; let it be drawn          
              (menu-item-action
               (nth (- selected-menu-item 1) items)))))))))


(defgeneric menu-select-v-offset (menu)
  (:method ((menu pop-up-menu))
    (if (control-handle menu)
        (let ((lh (view-font-line-height menu))
              (vh (point-v (view-size menu)))
              (mouse-v (point-v (view-mouse-position menu))))      
          (cond ((< mouse-v lh) 0)            
                ((and (> vh (+ lh 2)))
                 (max 0 (- mouse-v lh)))             
                (t 0)))
        0)))



;;---------
;; Install/deinstall menu


(defmethod menu-font ((menu pop-up-menu))
  (or (slot-value menu 'menu-font)
      (view-font menu)))

(defmethod view-font-codes ((menu pop-up-menu))
  (let ((font-slot (slot-value menu 'menu-font)))
    (if font-slot
        (font-codes font-slot)
        (call-next-method))))



(defmethod menu-install ((menu pop-up-menu))
  "Creates the actual Macintosh menu with all of the menu's current items."
  (let* ((menu-items (menu-items menu))
         (default (pop-up-menu-default-item menu)))
    (apply (function remove-menu-items) menu menu-items)
    #-(and) (init-menu-id menu)
    (niy menu-install menu)
    #-(and)
    (rlet ((foo :ptr))
      (let* ((the-title (menu-title menu))
             (menu-handle))
        (errchk (#_CreateNewMenu (slot-value menu 'menu-id) 0 foo))
        (setf menu-handle (%get-ptr foo))
        (if (7bit-ascii-p the-title)
            (with-pstrs ((menu-title the-title))
              (#_SetMenuTitle menu-handle menu-title))
            (set-menu-title-cfstring menu-handle the-title))
        (#_InsertMenu menu-handle -1)
        (setf (slot-value menu 'menu-handle) menu-handle)
        (when t ;(menu-font menu)
          (multiple-value-bind (ff ms)(view-font-codes menu)
            (when ff
              (#_SetMenuFont menu-handle (ash ff -16)(logand ms #xffff)))))))
    (let* ((colors (part-color-list menu)))
      (loop
        (unless colors (return))
        (set-part-color menu (pop colors) (pop colors))))
    (apply (function add-menu-items) menu menu-items)
    (when (and default (not (zerop default)))
      (set-pop-up-menu-default-item menu default))))



(defmethod menu-deinstall ((menu pop-up-menu))
  (let* ((*menubar-frozen* t))
    (call-next-method)))


(defmethod remove-view-from-window ((menu pop-up-menu))
  (menu-deinstall menu)
  (call-next-method)
  (niy remove-view-from-window menu)
  #-(and)
  (without-interrupts
    (dispose-record (pop-up-menu-rect menu) :rect)
    (setf (pop-up-menu-rect menu) nil)
    (dispose-record (pop-up-menu-title-rect menu) :rect)
    (setf (pop-up-menu-title-rect menu) nil)
    (when (control-handle menu)
      (with-focused-dialog-item (menu) ;
        (niy remove-view-from-window menu)
        #-(and)
        (#_disposecontrol (control-handle menu)))
      (setf (control-handle menu) nil))))


;;;;;;;;;;;;;;;;;
;;
;; action-pop-up-menu
;;
;; An action-pop-up-menu should have a separator as the second item;
;; the menu will pop up over the separator, so a quick click doesn't
;; cause anything to happen.

(defclass action-pop-up-menu (pop-up-menu)
  ())

(defmethod view-click-event-handler :before ((menu action-pop-up-menu) where)
  (declare (ignore where))
  (set-pop-up-menu-default-item menu 2))

(defmethod view-click-event-handler :after ((menu action-pop-up-menu) where)
  (declare (ignore where))
  (set-pop-up-menu-default-item menu 1))


;;;;;;;;;;;;;;;;;
;;
;; pull-down-menu
;;
;; omits the triangle and outline. No default


(defclass pull-down-menu (pop-up-menu)
  ((:crescent :initarg :crescent :initform nil :reader crescent))
  (:default-initargs
   :auto-update-default nil
   :default-item 0))

(defmethod part-color ((menu pull-down-menu) key)
  ;; don't default body to white
  (getf (slot-value menu 'color-list) key nil))

(defmethod menu-display-v-offset ((menu pull-down-menu))
  (if t                               ;(osx-p)
      (+ 3 (point-v (view-size menu)))
      (1- (point-v (view-size menu)))))

(defun pull-down-menu-p (menu)
  (typep menu 'pull-down-menu))


(defmethod view-draw-contents ((menu pull-down-menu))
  (let* ((enabled (menu-enabled-p menu))
         (colorp (and (color-or-gray-p menu)(window-color-p (view-window menu))))
         (disabled-color (if (and (not enabled) colorp)
                             *gray-color*))
         (title-color (or disabled-color
                          (part-color menu :menu-title))))
    (declare (ignore title-color))
    (with-focused-dialog-item (menu)  ; take font from item, draw in containers coords - this is the other thing that dialog item gives us
      (multiple-value-bind (ff ms)(view-font-codes menu)
        (declare (ignore ff ms))
        (draw-menu-title menu)  ;; very unlikely that there is one
        (niy view-draw-contents menu)
        #-(and)
        (rlet ((a-rect :rect))
          (copy-record (pop-up-menu-rect menu) :rect a-rect)
          (let ((mi-title (get-menu-body-text menu)))
            (with-back-color (part-color menu :menu-body) ; 10-Nov-92 -straz
              (#_EraseRect a-rect)
              (cond ((crescent menu)
                     (let ((tl (rref a-rect rect.topleft)))
                       (#_moveto (point-h tl)(point-v tl))
                       (dolist (length '(5 3 2 1 0 0))
                         (#_line length 0)
                         (#_move (- length) 1)))))
              (progn ;with-fore-color title-color ;; draw-string does it
                (let* ((left (+ (rref a-rect rect.left) 7)))
                  (progn ;with-clip-rect-intersect a-rect  ;;draw-string does it
                    (setf (pref a-rect :rect.left) left)
                    (incf (pref a-rect :rect.top) 2)
                    (draw-string-in-rect mi-title  a-rect :ff ff :ms ms :color title-color))
                  )))))               
        (unless (or enabled colorp)
          (paint-menu-gray menu))))))



(defmethod view-click-event-handler ((menu pull-down-menu) where)
  (when (menu-enabled-p menu)
    (multiple-value-bind (ff ms) (view-font-codes menu)
      (declare (ignore ff ms))
      (with-focused-dialog-item (menu)  ; << focus she said
        ;; redraw with back-color black and fore-color white, or fore and back switched?
        (niy view-click-event-handler menu where)
        #-(and)
        (let ((orig-back (or (part-color menu :menu-body) *white-color*))
              (orig-fore  (or (part-color menu :menu-title) *black-color*)))
          (rlet ((rect :rect))
            (copy-record (pop-up-menu-rect menu) :rect rect)
            (with-back-color orig-fore
              (let* ((mi-title (get-menu-body-text menu))
                     (left (+ (rref rect rect.left) 7))  ;(if pull-down-p 6 (max 6 w))))
                     )
                (#_Eraserect rect)
                (progn ;with-clip-rect-intersect rect ;; draw-string does it
                  (setf (pref rect :rect.left) left) 
                  (incf (pref rect :rect.top) 2)
                  (draw-string-in-rect mi-title  rect :ff ff :ms ms :color orig-back)
                  )))))
        (menu-select menu 0)))))


(defmethod point-in-click-region-p ((menu pull-down-menu) where)
  ;; in this case it can be anyplace
  (view-contains-point-p menu where))

(defmethod use-pop-up-control ((menu pull-down-menu))
  nil)



;;;;;;;;;;;;;;;;;
;;
;; typein-menu 
;;
;; its items should be of type typein-menu-item


(defclass typein-menu (view)
  ((menu :initform nil :accessor typein-menu-menu)
   (menu-position :initarg :menu-position :initform :right :reader typein-menu-menu-position) 
   (editable-text :initform nil
                  :accessor typein-editable-text))
  (:default-initargs
   :menu-class 'typein-menu-menu
   :view-subviews nil
   :editable-text-class 'editable-text-dialog-item))

(defmethod menu-disable ((menu typein-menu))
  (let ((mm (typein-menu-menu menu)))
    (when (menu-enabled-p mm)
      (menu-disable mm)
      (dialog-item-disable (typein-editable-text menu))
      (invalidate-view menu))))  ; get the title if any

(defmethod menu-enable ((menu typein-menu))
  (let ((mm (typein-menu-menu menu)))
    (unless (menu-enabled-p mm)
      (menu-enable mm)
      (dialog-item-enable (typein-editable-text menu))
      (invalidate-view menu))))

(defclass typein-menu-menu (pop-up-menu)()
  (:default-initargs
   ;;:auto-update-default nil
   :item-display ""
   :default-item 0))


(defmethod view-corners ((tmm typein-menu-menu))
  (call-next-method))


(defmethod initialize-instance :after ((view typein-menu-menu) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs)))



(defmethod add-menu-items ((menu typein-menu-menu) &rest items)
  (let* ((second (second items))
         (typein-p (and second (string= (menu-item-title second) "-"))))
    (if (not typein-p)
        (apply #'call-next-method
               menu
               (make-instance 'typein-menu-item :menu-item-title "None")
               (make-instance 'menu-item :menu-item-title "-")
               items)
        (call-next-method))))

(defmethod menu-display-v-offset ((menu typein-menu-menu))
  (case (typein-menu-menu-position (view-container menu))
    (:left 2) ; line up with title & text, doesn't cover the control (could erase it oneself)
    (t 2)))   ; maybe we will


(defmethod size-rectangles ((menu typein-menu-menu))
  (let ((title (menu-title menu)))
    (if (eq (length title) 0)
        (call-next-method)
        (let* ((my-pos (view-position menu))
               (my-size (view-size menu)))
          (when (and my-pos my-size)
            (multiple-value-bind (ff ms) (view-font-codes menu)
              (declare (ignore ff ms))
              (setf my-size (add-points my-size #@(-1 -1)))
              (if (use-pop-up-control menu)
                  ;; tweak else lose top edge
                  (setf my-pos (add-points my-pos #@(0 1)))) 
              (niy size-rectangles menu)
              #-(and)
              (let* ((menu-rect (or (pop-up-menu-rect menu)
                                    (setf (pop-up-menu-rect menu) (make-record :rect))))
                     (title-rect (or (pop-up-menu-title-rect menu)
                                     (setf (pop-up-menu-title-rect menu) (make-record :rect))))
                     (title-width (+ 8 (font-codes-string-width title ff ms))))
                (rset menu-rect :rect.topleft my-pos)
                (rset menu-rect :rect.bottomright (add-points my-pos my-size))
                (rset title-rect :rect.topleft (make-point 0 (+ (point-v my-pos) 2)))
                (rset title-rect :rect.bottomright (make-point title-width
                                                               (+ (point-v my-size)
                                                                  (point-v my-pos)
                                                                  -1))))))))))

(defmethod menu-select ((menu typein-menu-menu) num)
  (declare (ignore num))
  (let ((num (pop-up-menu-default-item menu)))
    (if (or (not (menu-enabled-p menu))(and num (> num 1)))
        (call-next-method)      
        (let* ((c (view-container menu))
               (pos (typein-menu-menu-position c))
               (view-pos (view-position menu))
                                        ;(w (- (point-h (view-size menu)) 2))
               (tl view-pos)
               (br (make-point (point-h (view-size c)) 1))
               t-pos t-w)
          (declare (ignore br) (ignorable t-pos t-w tl))
          (when (eq pos :left)
            (let ((text (typein-editable-text c)))
              (setf t-pos (subtract-points (view-position text) #@(2 2)))
              (setf t-w (+  (point-h (view-size text)) 3))
              (setf tl #@(0 0))))
          (niy menu-select menu num)
          #-(and)
          (rlet ((rect :rect :topleft tl :bottomright br))
                                        ; erase top edge which is not obscured by the menu contents
            (#_eraserect rect))
          (call-next-method)
                                        ; restore top edge
          #-(and)
          (progn
            ;; change for platinum-pop-up compatibility
            (invalidate-corners menu #@(0 0) (make-point (point-h (view-size menu)) 1))
                                        ;(#_moveto :long view-pos)
                                        ;(#_line :word w :word 0)
            (when (eq pos :left)
              (#_moveto (point-h t-pos)(point-v t-pos))
              (#_line t-w 0)))))))



(defmethod initialize-instance ((view typein-menu) &rest initargs
                                &key view-size
                                  view-position
                                  menu-class
                                  menu-position
                                  menu-items
                                  menu-title
                                  view-font
                                  item-display
                                  (draw-outline -2)
                                  (dialog-item-text "")
                                  editable-text-class)
  (declare (dynamic-extent initargs)(ignore menu-position menu-items menu-title item-display))
  (apply (function call-next-method) view 
         :view-size view-size    ; make default size&pos work right
         :view-position view-position
         initargs)
  (let ((menu (apply (function make-instance) menu-class               
                     :view-container view
                     initargs)))
    (setf (typein-menu-menu view) menu)    
    (let* ((edit (apply (function make-instance) editable-text-class
                        :view-container view                  
                        :draw-outline draw-outline
                        :dialog-item-text dialog-item-text
                        :allow-returns nil
                        :allow-tabs nil
                        (when view-font (list :view-font view-font)))))
      (setf (typein-editable-text view) edit))
    (when view-size (view-size-parts view))))


(defmethod view-default-size ((view typein-menu))
  (multiple-value-bind (ff ms) (view-font-codes view)
    (let ((text (typein-editable-text view))
          (menu (typein-menu-menu view)))
      (if (and text menu)
          (let* ((size (or (view-size text) (view-default-size text)))
                 (h (max 100 (point-h size)))
                 (v (point-v size))
                 (title (menu-title menu))
                 (title-width (if (zerop (length title))
                                  0
                                  (font-codes-string-width title ff ms))))
            (make-point (+ h title-width 6 (if (> v 16)
                                               22
                                               20))
                        (max 22 (+ 4 v))))))))


(defgeneric view-size-parts (view)
  (:method ((view typein-menu))  
    (let* ((size (view-size view))
           (size-v (point-v size))
           (size-h (point-h size))
           (menu (typein-menu-menu view))
           (text (typein-editable-text view))
           (title (menu-item-title menu))
           (title-width 0)
           (v-delta 2)) ;;  ugly on os9 but the focus rect does show
      (multiple-value-bind (ff ms)(view-font-codes view)
        (unless (zerop (length title))        
          (setf title-width (+ 8 (font-codes-string-width title ff ms))))
        (let* ((menu-h (if (> size-v 16) 22 20)))      
          (set-view-size menu (make-point menu-h  size-v))
          (set-view-size text (make-point (- size-h (+ menu-h 6 title-width)) (- size-v (+ v-delta v-delta))))
          (case (typein-menu-menu-position view)
            (:left (set-view-position menu (make-point title-width 0))
             (set-view-position text (make-point (+ menu-h 4 title-width ) (+ 1 v-delta))))
            (t 
             (set-view-position menu (make-point (+  size-h (- menu-h)) 0))
             (set-view-position text (make-point (+ 0 title-width)  (1+ v-delta)))))))))) ;; 0 was 2



(defmethod view-corners ((view typein-menu)) ;; so the focus rect will show up    
  (multiple-value-bind (tl br)
      (multiple-value-call  (function inset-corners) #@(-4 -4) (call-next-method))
    (if (eq (typein-menu-menu-position view) :left)
        (setf br (add-points br #@(2 0)))
        (setf tl (subtract-points tl #@(2 0))))
    (values tl br)))


(defmethod set-default-size-and-position ((view typein-menu) &optional container)
  (declare (ignore container))
  (call-next-method)
  (view-size-parts view))



(defmethod install-view-in-window ((menu typein-menu) dialog &aux ok)  
  (declare (ignore dialog))
  (without-interrupts
    (unwind-protect
         (let ((container (view-container menu)))
           (set-default-size-and-position menu container)
           (setf ok t))
      (unless ok
        (set-view-container menu nil))))
  (call-next-method))


(defmethod set-view-size ((view typein-menu) h &optional v)
  (declare (ignore h v))
  (let ((menu (typein-menu-menu view)))
    (setf (pop-up-menu-cached-title menu) nil)
    (call-next-method)
    (view-size-parts view))
  (view-size view))



(defmethod menu-display-h-offset ((menu typein-menu-menu))
  (case (typein-menu-menu-position (view-container menu))
    ((:right :left) 
     1)
    (t                        ; this we probably won't ever use     
     (let* ((text-size (point-h (view-size (typein-editable-text 
                                            (view-container menu))))))       
       (- -5 text-size)))))


(defmethod menu-item-icon-handle ((item menu))
  nil)


(defmethod menu-item-icon-num ((item menu))
  nil)


(defun max-menu-width (menu)
  (multiple-value-bind (ff ms)(view-font-codes menu)
    (let* ((max 0)
           (fudge (if (and (not (typep menu 'pull-down-menu))
                           (or (fboundp 'new-draw-pop-up-menu) (use-pop-up-control menu)))
                      (dolist (m (menu-items menu) 2)   ;(if (osx-p) 2 0))
                        (if (or (menu-item-icon-handle m)(menu-item-icon-num m))
                            (return 24))) ;; constant 16 may be wrong in some cases
                      0)))
      (dolist (m (menu-items menu) max)
        (when (> (setf m (+ fudge (font-codes-string-width (menu-item-title m)
                                                           ff ms)))
                 max)
          (setf max m))))))


;; vanilla menu-item action functions do not take an argument!

(defclass typein-menu-item (menu-item)
  ())

(defmethod menu-item-action ((item typein-menu-item))
  (let ((action (menu-item-action-function item)))    
    (let* ((menu     (menu-item-owner item))
           (ti       (typein-editable-text (view-container menu)))
           (new-text (if (and (eq 1 (pop-up-menu-default-item menu))
                              (string= "None" (menu-item-title item)))
                         "" 
                         (menu-item-title item))))
      (when (string/= (dialog-item-text ti) new-text) ; prevents flashies
        (set-dialog-item-text ti new-text))
      (let ((items (menu-items menu)))
        (dolist (i items)
          (when (menu-item-check-mark i)
            (set-pop-up-item-check-mark i nil)
            (return))))
      (set-pop-up-item-check-mark item t))
    (when action (funcall action item))))



(defparameter *chicago-font* 0)  ;; avoid some consing below


(defun set-pop-up-item-check-mark (item mark)  
  (let ((menu (menu-item-owner item)))
    (when (and menu (eq mark t)
               (let ((font (ash (view-font-codes menu) -16)))
                 (not (or (eql font (ash (sys-font-codes) -16))
                          (eql font *chicago-font*)))))
      (niy set-pop-up-item-check-mark item mark)
      (setf mark (code-char 195)))  ;; weird = macroman #\altCheckMark
    (set-menu-item-check-mark item mark)))



(defmethod view-click-event-handler ((menu typein-menu-menu) where)
  (declare (ignore where))  
  (let* ((ti     (typein-editable-text (view-container menu)))
         (items  (menu-items menu))
         (text-p (not (zerop (dialog-item-text-length ti)))))
    (unless items
      (add-menu-items menu)
      (setf items (menu-items menu)))
    (if text-p
        (let ((str (dialog-item-text ti)))
          (unless (let ((n 1))
                    (dolist (item (menu-items menu) nil)
                      (when (string-equal str (menu-item-title item))
                        (set-pop-up-menu-default-item menu n)
                        (return t))
                      (setf n (1+ n))))
            (setf (pop-up-menu-cached-title menu) nil)
            (set-menu-item-title (car items) str)
            (set-pop-up-menu-default-item menu 1)))
        (progn
          (when  (string/= (menu-item-title (car items)) "None")
            (setf (pop-up-menu-cached-title menu) nil)          
            (set-menu-item-title (car items) "None"))
          (set-pop-up-menu-default-item menu 1)))
    (with-focused-view (view-container menu)
      (call-next-method))        
    (when (dialog-item-enabled-p ti) ; let it be typeable always - gratuitous change - forget it
      (set-current-key-handler (view-window menu) ti))))

;;----------
;; Menu body width

(defgeneric menu-body-width (menu)
  (:method ((menu pop-up-menu))
    (let* ((ti-rect  (pop-up-menu-title-rect menu))
           (ti-width (- (rect-right ti-rect) (rect-left ti-rect))))
      (- (point-h (view-size menu)) ti-width)))
  (:method ((menu typein-menu-menu))
    (let* ((ti-rect  (pop-up-menu-title-rect menu))
           (ti-width (- (rect-right ti-rect) (rect-left ti-rect))))
      (- (point-h (view-size (view-container menu))) ti-width))))



;;--------
;; Adjusted menu-item title

(defgeneric adjusted-menu-item-title (menu)
  (:method ((menu pull-down-menu))
    (declare (ignore menu)))
  (:method ((menu pop-up-menu))
    (let ((items (menu-items menu)))
      (when  items
        (multiple-value-bind (ff ms) (view-font-codes menu)
          (let* ((w (nth-value 2 (font-codes-info ff ms)))
                 (max (+ w 12 (max-menu-width menu)))
                 (width (menu-body-width menu)))
            (when (< max width)
              (setf max (+ w 13 (font-codes-string-width (menu-item-title (car items)) ff ms)))
              (let* ((ss (font-codes-string-width " " ff ms))
                     (first (car items)))           
                (concatenate 'string (menu-item-title first)
                             (make-array (ceiling (- width max) ss)
                                         :element-type 'base-char
                                         :initial-element #\space)))))))))
  (:method ((menu typein-menu-menu))
    (case (typein-menu-menu-position (view-container menu))
      (:right nil)
      (t (call-next-method)))))


(defun initialize/pop-up-menu-dialog-item ()
  (setf *chicago-font* (ash (font-codes '("chicago")) -16)))

;;;; THE END ;;;;
