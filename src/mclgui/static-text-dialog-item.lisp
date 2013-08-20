;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               static-text-dialog-item.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Static Text Dialog Item
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


(defclass static-text-dialog-item (dialog-item)
  ((width-correction   :allocation :class
                       :initform    4)
   (text-justification :initform   nil
                       :initarg    :text-justification)
   (text-truncation    :initform   nil
                       :initarg    :text-truncation)
   (compress-text      :initform   nil
                       :initarg    :compress-text
                       :reader     compress-text))
  (:default-initargs :dialog-item-text "Untitled"))


(defmethod update-handle ((item static-text-dialog-item))
  (let* ((pos (or (slot-value item 'view-position) #@(0   0)))
         (siz (or (slot-value item 'view-size)     #@(10 10)))
         (texth [[MclguiTextField alloc]
                 initWithFrame:(ns:make-ns-rect (point-h pos) (point-v pos)
                                                (point-h siz) (point-v siz))])) 
    ;; -- NSControl attributes:
    [texth setTarget:texth]
    [texth setAction:(objc:@selector "mclguiAction:")]
    [texth setStringValue:(objcl:objcl-string (dialog-item-text item))]
    [texth setEnabled:(if (dialog-item-enabled-p item)
                          YES
                          NO)]
    (multiple-value-bind (ff ms) (view-font-codes item)
      (multiple-value-bind (font mode color other) (nsfont-from-codes ff ms)
        (declare (ignore mode other))
        (declare (ignore color))
        ;;[texth setTextColor:color]
        (format-trace "static-text" font)
        [texth setFont:font]))
    [texth setAlignment:(case (slot-value item 'text-justification)
                          (:left      #$NSLeftTextAlignment)
                          (:right     #$NSRightTextAlignment)
                          (:center    #$NSCenterTextAlignment)
                          (:justified #$NSJustifiedTextAlignment)
                          (:natural   #$NSNaturalTextAlignment)
                          (otherwise  #$NSNaturalTextAlignment))]
    ;; -- NSTextField attributes:
    (let ((editable (typep item 'editable-text-dialog-item)))
      [texth setEditable:editable]
      [texth setBordered:editable])
    [texth setSelectable:YES]
    ;; [texth setTextColor:] ;; set above
    ;; [texth setBackgroundColor:]
    ;; [texth setDrawBackground:]
    [texth setBezeled:NO]
    [texth setBezelStyle:#$NSTextFieldSquareBezel]
    ;; #$NSTextFieldSquareBezel  = 0
    ;; #$NSTextFieldRoundedBezel = 1
    ;; --
    (setf (handle item) texth)))


(defmethod unwrap ((item static-text-dialog-item))
  (unwrapping item
              (or (handle item) (update-handle item))))


(defmethod view-default-font ((view static-text-dialog-item))
  (sys-font-spec))



(defmethod view-default-size ((dialog-item static-text-dialog-item))
  (multiple-value-bind (ff ms)(view-font-codes dialog-item)
    (let* ((text (dialog-item-text dialog-item)))
      (multiple-value-bind (string-width nlines)
          (font-codes-string-width-with-eol text ff ms)
        (make-point (+ (dialog-item-width-correction dialog-item) string-width)
                    (* nlines (font-codes-line-height ff ms)))))))


(defmethod set-view-font-codes ((item static-text-dialog-item) ff ms &optional ff-mask ms-mask)
  (declare (ignore ff ms ff-mask ms-mask))
  (multiple-value-prog1 (call-next-method)
    (with-handle (texth item)
      (multiple-value-bind (ff ms) (view-font-codes item)
        (multiple-value-bind (font mode color other) (nsfont-from-codes ff ms)
          (declare (ignore mode other))
          (declare (ignore color))
          ;;[texth setTextColor:color]
          (format-trace "set-view-font-codes" font)
          [texth setFont:font])))))

(defmethod set-dialog-item-text ((item static-text-dialog-item) text)
  (setf (slot-value item 'dialog-item-text) text)
  (with-handle (texth item)
    [texth setStringValue:(objcl:objcl-string (dialog-item-text item))])
  (invalidate-view item t)  
  text)


(defmethod dialog-item-text ((item static-text-dialog-item))
  (let ((text (slot-value item 'dialog-item-text)))    
    text))


(defmethod view-activate-event-handler ((item static-text-dialog-item))
  (setf (dialog-item-enabled-p item) t)
  (invalidate-view item))


(defmethod view-deactivate-event-handler ((item static-text-dialog-item))
  (setf (dialog-item-enabled-p item) nil)
  (invalidate-view item))



(defmethod view-draw-contents ((item static-text-dialog-item))
  (with-handle (texth item)
    [texth drawRect: (unwrap (make-nsrect :origin (view-origin item) :size (view-size item)))])
  ;; We shouldn't have to do anything really
  #-(and)
  (when (installed-item-p item)
    (without-interrupts
     (with-focused-view (view-container item)
       (let ((position           (view-position item))
             (size               (view-size item))
             (text-justification (slot-value item 'text-justification))
             (truncation         (slot-value item 'text-truncation))
             (enabled-p          (dialog-item-enabled-p item))
             (compress-p         (compress-text item))
             (old-state          nil))
         (declare (ignorable position size text-justification truncation enabled-p compress-p old-state))
         (let* ((rect (make-rect position (add-points position size)))
                (theme-back nil ;; (theme-background-p item)
                            )
                (back (or (part-color item :body)
                          (when (not theme-back)
                            (slot-value (view-window item) 'back-color))))                          
                (fore (if enabled-p
                        (part-color item :text)
                        *gray-color*)))
           ;; (when (and (not back) theme-back) ; (not (dialog-item-enabled-p item)))  ;; sometimes background goes white??
           ;; (rlet ((old-statep :ptr))
           ;;   (#_getthemedrawingstate old-statep)
           ;;   (setq old-state (%get-ptr old-statep)))
           ;; (let* ((wptr (wptr item))
           ;;        (depth (current-pixel-depth)))
           ;;   (#_setthemebackground  #$kThemeBrushModelessDialogBackgroundActive depth (wptr-color-p wptr)))
           ;; )
           (with-back-color back
             (multiple-value-bind (ff ms)(view-font-codes item)
               (when t ;; or when back?
                 (erase-rect* item
                             (point-h position) (point-v position)
                             (point-h size) (point-v size)))  
               (draw-string-in-rect (dialog-item-text item) rect 
                                    :justification text-justification
                                    :compress-p compress-p
                                    :truncation truncation
                                    :ff ff :ms ms :color fore)))
           ;; (if old-state (#_setthemedrawingstate old-state t))
           ))))))



;;;; THE END ;;;;
