;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               button-dialog-item.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Button Dialog Item.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-19 <PJB> Created.
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


(defclass button-dialog-item (default-button-mixin control-dialog-item)
  ((procid           :allocation :class
                     :initarg  :procid
                     :initform 0 ; #$PushButProc
                     )
   (width-correction :allocation :class
                     :initform 10))
  (:documentation "
This is the class used to  make buttons. Clicking a button usually has
an  immediate  result. Buttons  are  generally  given  a function  for
DIALOG-ITEM-ACTION-FUNCTION via the :dialog-item-action initialization
argument.
"))


(defmethod initialize-instance ((item button-dialog-item) &key (border-p t))
  (call-next-method)
  (unless border-p
    (setf (view-get item 'no-border) t)))



(defclass default-button-dialog-item (button-dialog-item)
  ()
  (:default-initargs
   :dialog-item-text "OK"
    :default-button  t
    :cancel-button   nil)
  (:documentation "
Default buttons are a convenient subclass of button dialog items; they
serve as the default button. A dialog may have one default button. This
button has a bold border and usually may be selected by one of the
keystrokes Return or Enter.
"))


(defgeneric set-default-button (window new-button)
  (:documentation "

The SET-DEFAULT-BUTTON generic function changes the default button
according to the value of new-button and returns new-button.  If
carriage returns are allowed in the current editable-text item, they
are sent to that item rather than to the default button.

WINDOW:         A window.

NEW-BUTTON:     The button that should be made the default button, or
                NIL, indicating that there should be no default button.
")
  (:method ((dialog window) new-button)
    (let ((default-button (%get-default-button dialog)))
      (unless (eq default-button new-button)
        (without-interrupts
            (when default-button
              (invalidate-view-border default-button t)
              (niy set-default-button dialog new-button)
              #-(and)
              (#_setwindowdefaultbutton (wptr dialog) (%null-ptr)))
          (setf (%get-default-button dialog) new-button)
          (when new-button
            (when (dialog-item-handle new-button)
              (if (dont-throb new-button) ;(and (osx-p) (neq (view-container new-button)(view-window new-button)))
                  nil  ;; so we act like a default button but dont look like one
                  (niy set-default-button dialog new-button)
                  #-(and)
                  (#_setwindowdefaultbutton (wptr dialog) (dialog-item-handle new-button))))
            (invalidate-view-border new-button)))))
    new-button))


(defgeneric default-button-p (item)
  (:documentation "
The DEFAULT-BUTTON-P generic function returns true if item is the
default button in the view-window of ITEM.  Otherwise it returns NIL.
")
  (:method ((item default-button-mixin))
    (let ((window (view-window item)))
      (and window (eq item (default-button window))))))



(defmethod view-corners ((item button-dialog-item))
  (multiple-value-call (function inset-corners) #@(-1 -1) (call-next-method)))


 
(defmethod view-default-size ((button button-dialog-item))
  (let ((size (call-next-method)))
    (make-point (max 60 (+ 12 (point-h size)))
                (+ 4 (point-v size)))))


(defmethod view-default-font ((button button-dialog-item))
  (sys-font-spec))



(defmethod dont-throb ((item button-dialog-item))
  (or
   ;; (not (eq (view-window item)(view-container item)))
   (part-color item :text)
   (part-color item :body)        
   #+ignore ;; font seems to work - oops no it doesn't
   (and (default-button-p item)
        (or (not (eq (view-window item)(view-container item)))
            #+ignore
            (multiple-value-bind (ff ms)(view-font-codes item)
              (declare (ignore-if-unused ms))
              (multiple-value-bind (sys-ff sys-ms)(sys-font-codes)
                (declare (ignore-if-unused sys-ms))
                (or (not (eql ff sys-ff))
                    #+ignore
                    (not (eql ms sys-ms)))))))
   ))



(defmethod call-with-focused-dialog-item ((item button-dialog-item) fn &optional container)
  (call-next-method item fn (if (default-button-p item)
                                (view-window (or container item))
                                container)))


(defmethod dialog-item-disable ((item button-dialog-item))
  (view-put item 'was-enabled nil)
  (call-next-method))


(defgeneric press-button (button)
  (:documentation "
The press-button generic function highlights button, then calls the
dialog-item-action method for button.
")
  (:method ((button button-dialog-item))
    (with-focused-dialog-item (button)
      (niy  press-button button)
      ;; (let ((handle (dialog-item-handle button)))
      ;;   (#_deactivatecontrol handle) ;(#_HiliteControl handle 1)
      ;;   (let ((time (%tick-sum 3 (get-tick-count))))
      ;;     (while (< (%tick-difference (get-tick-count) time) 0)))
      ;;   (when (setq handle (dialog-item-handle button))   ; window may have closed
      ;;     (#_activatecontrol handle)) ;(#_HiliteControl handle 0))
      ;;   (dialog-item-action button))
      (dialog-item-action button))))


(defmethod validate-control-dialog-item ((item button-dialog-item))
  ;; ROM redraw doesn't erase between the round-rect and the view borders
  (niy validate-control-dialog-item item)
  ;; (let ((wptr (wptr item))
  ;;       (container (view-container item))
  ;;       (handle (dialog-item-handle item)))
  ;;   (when (and wptr container)
  ;;     (with-focused-view (view-container item)
  ;;       (with-macptrs ((rgn (#_NewRgn))
  ;;                      (rgnSave (rref wptr :windowrecord.rgnSave)))
  ;;         (rset wptr :windowrecord.rgnSave (%null-ptr))
  ;;         (#_OpenRgn)
  ;;         (#_Draw1Control handle)
  ;;         (#_CloseRgn rgn)
  ;;         (rset wptr :windowrecord.rgnSave rgnSave)
  ;;         (validate-region container rgn)
  ;;         (#_DisposeRgn rgn)))))
  )



(defmethod invalidate-view ((item button-dialog-item) &optional erase-p)
  (if erase-p
    (call-next-method)
    (without-interrupts
      (call-next-method item t)
      (validate-control-dialog-item item)
      (call-next-method))))
            

(defmethod (setf dialog-item-enabled-p) (p (item button-dialog-item))
  (when (if (prog1 (dialog-item-enabled-p item)
              (call-next-method))
            (not p)
            p)
    (maybe-draw-default-button-outline item))
  p)





(defmethod view-draw-contents :before ((item button-dialog-item))
  (when (and (default-button-p item)
             (not (eq (view-window item) (view-container item))))
    (niy view-draw-contents item)
    ;; (let ((topLeft (convert-coordinates (view-position item) (view-container item) (view-window item)))
    ;;       (control-rect-position (get-control-bounds item)))
    ;;   (unless (eql control-rect-position topleft)         
    ;;     (#_MoveControl handle (point-h topLeft) (point-v topLeft))))
    ))



;; (defun get-control-bounds (x)
;;   (rlet ((rect :rect))
;;     (#_getcontrolbounds (dialog-item-handle x) rect)
;;     (values (pref rect :rect.topleft)
;;             (pref rect :rect.botright))))


(defmethod view-draw-contents ((item button-dialog-item))
  (let ((no-border (view-get item 'no-border)))
    (if no-border
      (with-item-rect (rect item)
        (niy view-draw-contents item)
        ;; (#_eraserect rect)
        (clip-inside-view item 2 2)
        (call-next-method))
      (progn
        (call-next-method)
        (maybe-draw-default-button-outline item)))))


(defmethod draw-default-button-outline ((item button-dialog-item))
  (when (installed-item-p item)
    (with-focused-dialog-item (item)
      (let ((grayp (not (dialog-item-enabled-p item))))
        (with-slots (color-list) item
          (with-fore-color (or (getf color-list :frame nil)  *light-blue-color*) ;; how do we tell if user changed to "graphite"?
            (without-interrupts
                (niy draw-default-button-outline item)
                ;; (with-item-rect (rect item)
                ;;   (#_insetRect rect -4 -4)
                ;;   (rlet ((ps :penstate))
                ;;         (#_GetPenState ps)
                ;;         (#_PenSize 3 3)
                ;;         (when grayp
                ;;           (#_PenPat *gray-pattern*))
                ;;         (#_FrameRoundRect rect 16 16)
                ;;         (#_SetPenState ps)))
              )))))))


(defun maybe-draw-default-button-outline (button)
  (let ((my-dialog (view-window button)))
    (when (and my-dialog
               (eq button (default-button my-dialog))
               (not (view-get button 'no-border)))
      (draw-default-button-outline button))))



;; (defun theme-color ()  ;; here is how - what a lousy interface
;;   (rlet ((what :ptr))
;;     (errchk (#_copythemeidentifier what))
;;     (let* ((cfstr-ptr (%get-ptr what))
;;            (str (get-cfstr cfstr-ptr)))
;;       (#_cfrelease cfstr-ptr) ; ??
;;       (if (or (string= str "com.apple.theme.appearance.platinum") ;; os-9
;;               (string= str "com.apple.theme.appearance.aqua.graphite")) ;; os-x
;;         :platinum
;;         :blue))))



;;;; THE END ;;;;

