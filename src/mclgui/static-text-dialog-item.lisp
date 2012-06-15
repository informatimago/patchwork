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


(defmethod view-default-font ((view static-text-dialog-item))
  (sys-font-spec))



(defmethod view-default-size ((dialog-item static-text-dialog-item))
  (multiple-value-bind (ff ms)(view-font-codes dialog-item)
    (let* ((text (dialog-item-text dialog-item)))
      (multiple-value-bind (string-width nlines)
          (font-codes-string-width-with-eol text ff ms)
        (make-point (+ (dialog-item-width-correction dialog-item) string-width)
                    (* nlines (font-codes-line-height ff ms)))))))




(defmethod set-dialog-item-text ((item static-text-dialog-item) text)
  (setf (slot-value item 'dialog-item-text) text)
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
  (when (installed-item-p item)
    (without-interrupts
     (with-focused-view (view-container item)
       (let ((position (view-position item))
             (size (view-size item))
             (text-justification (slot-value item 'text-justification))
             (truncation (slot-value item 'text-truncation))
             (enabled-p (dialog-item-enabled-p item))
             (compress-p (compress-text item))
             (old-state nil))
         (niy view-draw-contents item)
         ;; (rlet ((rect :rect :topleft position :botright (add-points position size) ))
         ;;   (let* ((theme-back (theme-background-p item))
         ;;          (back (or (part-color item :body)
         ;;                    (if (not theme-back) (slot-value (view-window item) 'back-color))))                          
         ;;          (fore (if enabled-p (part-color item :text) *gray-color*)))
         ;;     (when (and (not back) theme-back) ; (not (dialog-item-enabled-p item)))  ;; sometimes background goes white??
         ;;       (rlet ((old-statep :ptr))
         ;;         (#_getthemedrawingstate old-statep)
         ;;         (setq old-state (%get-ptr old-statep)))
         ;;       (let* ((wptr (wptr item))
         ;;              (depth (current-pixel-depth)))
         ;;         (#_setthemebackground  #$kThemeBrushModelessDialogBackgroundActive depth (wptr-color-p wptr))))
         ;;     (with-back-color back
         ;;       (multiple-value-bind (ff ms)(view-font-codes item)
         ;;         (when t (#_eraserect rect))  ;; or when back?
         ;;         (draw-string-in-rect (dialog-item-text item) rect 
         ;;                              :justification text-justification
         ;;                              :compress-p compress-p
         ;;                              :truncation truncation
         ;;                              :ff ff :ms ms :color fore)))
         ;;     (if old-state (#_setthemedrawingstate old-state t))
         ;;     ))
         )))))



;;;; THE END ;;;;
