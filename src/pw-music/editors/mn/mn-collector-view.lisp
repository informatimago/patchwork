;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               mn-collector-view.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;    
;;;;    XXX
;;;;    
;;;;AUTHORS
;;;;    Mikael Laurson, Jacques Duthen, Camilo Rueda.
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
(in-package :pw)
(enable-patchwork-reader-macros)

;;;==================================================
;;; The Class of a view controller for a MN editor. 
;;;
;;; Contains buttons for changing view, chord field selection, scrolling and transposing. Contains
;;; a PopUp Menu (see code above) for additional functionalities
;;;
;;; Class Name:  C-MN-view-mod
;;; Inherits from: C-editor-view
;;; Methods:
;;;   set-up-dialog:            ;instanciates a dialog template for chord transformations
;;;   pretty-visible-layout:    ;computes the necessary hight for drawing the chords without clipping
;;;   use-staff                 ;changes staff
;;;   flip-selections           ;flip button color according to mouse clicking
;;;   play-the-chords           ; plays selected chords
;;;   transpose-selection       ;transposes selected chords according to value in button
;;;   retro-selection           ;retrogrades selected chords
;;;   accell-selection          ;affects chord fields of selected chords
;;;   

(defun make-chord-collector-pops ()
  (new-menu 
   " "
   (new-menu "Play" 
             (new-leafmenu "All" (lambda () (play-all-staffs *target-action-object*)))
                                        ;(new-leafmenu "Visible" (lambda ()(play-the-chords *target-action-object* t)))
                                        ;(new-leafmenu "selected" (lambda ()(play-the-chords *target-action-object* ())))
             (new-leafmenu "Stop Playing" (lambda () (stop-all-staffs *target-action-object*))))
   (new-menu "Approximation"
             (prog1 (setf a-leaf-menu
                          (new-leafmenu "SemiTone" 
                                        (lambda () (use-all-approx-scale  *target-action-object* *c-major-scale*))))
               (set-command-key a-leaf-menu #\2))
             (prog1 (setf a-leaf-menu
                          (new-leafmenu "Quarter tone" 
                                        (lambda () (use-all-approx-scale  *target-action-object* *1/4-tone-chromatic-scale*))))
               (set-command-key a-leaf-menu #\4))
             (prog1 (setf a-leaf-menu
                          (new-leafmenu "Eigth tone" 
                                        (lambda () (use-all-approx-scale  *target-action-object* *1/8-tone-chromatic-scale*))))
               (set-command-key a-leaf-menu #\8)))
   (new-menu "Scale"
             (new-leafmenu "C-major" 
                           (lambda () (use-all-scale  *target-action-object* *c-major-scale*)))
             (new-leafmenu "Chromatic" 
                           (lambda () (use-all-scale  *target-action-object* *chromatic-scale*))))
   (new-menu "Choose Staff"   ;;;;;;;;"Lay out"
             ;;(new-leafmenu "Add Staff" (lambda () (add-new-staff-to-MN-editor *target-action-object*)))
             ;;(new-menu "Choose Staff"
             (new-leafmenu "G2-G" (lambda () 
                                    (use-staff  *target-action-object* 1 *g2-g-staffs*)))
             (new-leafmenu "G" (lambda () 
                                 (use-staff  *target-action-object* 2 *g-plain-staffs*)))
             (new-leafmenu "G F" (lambda () 
                                   (use-staff  *target-action-object* 3 *g-f-staffs*)))
             (new-leafmenu "F" (lambda () 
                                 (use-staff  *target-action-object* 4 *f-plain-staffs*)))
             (new-leafmenu "G F F2" (lambda () 
                                      (use-staff  *target-action-object* 5 *g-f-f2-staffs*)))
             (new-leafmenu "G2 G F F2" (lambda () 
                                         (use-staff  *target-action-object* 6 *g2-g-f-f2-staffs*)))
             (new-leafmenu "Empty" (lambda ()
                                     (use-staff  *target-action-object* 7 *empty-staffs*))))
   (new-menu "Select" 
             (new-leafmenu "visible" (lambda () (Do-selections *target-action-object* nil)))
             (new-leafmenu "All" (lambda () (Do-selections *target-action-object* t))))
   (new-leafmenu "-" ())
   (new-menu "Transforms"
             (new-leafmenu "Chord Notes" (lambda ()(Cresc-selection *target-action-object*)))
             (new-leafmenu "Retro" (lambda ()(Retro-selection *target-action-object*)))
             (new-leafmenu "Chord Time" (lambda ()(Accell-selection *target-action-object*))))))

(defvar *MN-common-popUpMenu* nil)

(defclass C-numbox-staffcnt (C-numbox) ())

(defmethod view-double-click-event-handler ((self C-numbox-staffcnt) where)
  (declare (ignore where)))

(defmethod item-action-after-drag ((self C-numbox-staffcnt))
  (set-staff-count (car (subviews (view-container self))) (value self)))

(defclass C-MN-view-mod (C-mus-not-view) 
  ((selections :initform nil)
   (popUpBox :initform nil :accessor popUpBox)
   (dial-stf :initform nil)
   (dial-stT :initform nil)
   (vel-button :initform nil)
   (vel-% :initform nil)
   (vel-range :initform nil)
   (vel-val1 :initform nil)
   (vel-val2 :initform nil)
   (dur-button :initform nil)
   (dur-% :initform nil)
   (dur-range :initform nil)
   (dur-val1 :initform nil)
   (dur-val2 :initform nil)
   (off-button :initform nil)
   (off-% :initform nil)
   (off-range :initform nil)
   (off-val1 :initform nil)
   (off-val2 :initform nil)
   (OK-button :initform nil)
   (cancel-button :initform nil)))

(defmethod make-extra-controls :after ((self C-MN-view-mod))
  (let* ((x-pos (+ 86 (point-h (view-position self))))
         (y-pos (+  (point-v (view-position self)) (point-v (view-size self)) -35))
         (ctrls 
          (list
           (add-to-radio-cluster self (+ 86 (point-h (view-position self)))
                                 (+  (point-v (view-position self)) (point-v (view-size self)) -35)
                                 "channel" :channel)
           (setf (popUpBox self)
                 (make-popUpbox "M" self
                                (or *MN-common-popUpMenu*
                                    (setf *MN-common-popUpMenu* (make-chord-collector-pops)))
                                :view-position (make-point (+ x-pos 204) 
                                                           (+ y-pos 18))
                                :view-container (view-window self)))
           (make-instance 'button-dialog-item
                          :view-size (make-point 14 14)
                          :view-container (view-window self)
                          :view-position (make-point (+ x-pos 182) (+ y-pos 16));;(make-point (+ x-pos 142) (+ y-pos 16))
                          :dialog-item-text "tr"
                          :view-font *patchwork-font-spec*
                          :dialog-item-action
                          (lambda (item)
                              (declare (ignore item))
                              (transpose-selection self)))
           (make-instance 'C-numbox  
                          :view-size (make-point 36 14)
                          :value 100 :min-val -12700 :max-val 12700
                          :type-list '(fixnum)
                          :view-container (view-window self)
                          :view-position (make-point (+ x-pos 144) (+ y-pos 16))) ;;(make-point (+ x-pos 104) (+ y-pos 16)))
           (make-instance 'C-numbox-staffcnt  
                          :view-size (make-point 36 14)
                          :value 1 :min-val 1 :max-val 10
                          :type-list '(fixnum)
                          :view-container (view-window self)
                          :view-position (make-point (+ x-pos 104) (+ y-pos 16)))
           (make-instance 'static-text-dialog-item
                          :view-container (view-window self)
                          :view-position (make-point (+ x-pos 104) (+ y-pos 2))
                          :dialog-item-text "stcnt"
                          :view-font *patchwork-font-spec*
                          :view-size (make-point 36 12))
           (make-instance 'static-text-dialog-item
                          :view-container (view-window self)
                          :view-position (make-point (+ x-pos 44) (+ y-pos 16))
                          :dialog-item-text " "
                          :view-font *patchwork-font-spec*
                          :view-size (make-point 50 12)))))
    (setf (external-controls self) (append (external-controls self) ctrls))
    (setf (ctrl-settings self) (append (ctrl-settings self) (list (list :channel))))
    (set-up-dialog self)))

(defgeneric set-display-value (self value))
(defmethod set-display-value ((self C-MN-view-mod) value)
  (set-dialog-item-text (car (last (external-controls self)))
                        (if value (format () "~S" value) " ")))

(defgeneric set-up-dialog (self))
(defmethod set-up-dialog ((self C-MN-view-mod))
  (setf (slot-value self 'dial-stF)
        (make-instance 'static-text-dialog-item
                       :dialog-item-text "From"
                       :view-position (make-point 196 40)
                       :view-size (make-point 40 16)
                       :view-font *patchwork-font-spec*))
  (setf (slot-value self 'dial-stT)   
        (make-instance 'static-text-dialog-item
                       :dialog-item-text "To"
                       :view-position (make-point 240 40)
                       :view-size (make-point 40 16)
                       :view-font *patchwork-font-spec*))
  (setf (slot-value self 'vel-button) 
        (make-instance 'check-box-dialog-item
                       :dialog-item-text "velocity"
                       :view-position (make-point 20 60)
                       :view-size (make-point 80 16)
                       :view-font *patchwork-font-spec*))
  (setf (slot-value self 'vel-% )
        (make-instance 'check-box-dialog-item
                       :dialog-item-text "%"
                       :view-position (make-point 106 60)
                       :view-size (make-point 30 16)
                       :view-font *patchwork-font-spec*))
  (setf (slot-value self 'vel-range )
        (make-instance 'check-box-dialog-item
                       :dialog-item-text "range"
                       :view-position (make-point 134 60)
                       :view-size (make-point 50 16)
                       :view-font *patchwork-font-spec*))
  (setf (slot-value self 'vel-val1)
        (make-instance 'editable-text-dialog-item
                       :dialog-item-text "0"
                       :view-position (make-point 190 60)
                       :view-size (make-point 40 16)
                       :view-font *patchwork-font-spec*))
  (setf (slot-value self 'vel-val2 )
        (make-instance 'editable-text-dialog-item
                       :dialog-item-text "0"
                       :view-position (make-point 235 60)
                       :view-size (make-point 40 16)
                       :view-font *patchwork-font-spec*))
  (setf (slot-value self 'dur-button ) 
        (make-instance 'check-box-dialog-item
                       :dialog-item-text "duration"
                       :view-position (make-point 20 90)
                       :view-size (make-point 80 16)
                       :view-font *patchwork-font-spec*))      
  (setf (slot-value self 'dur-% )
        (make-instance 'check-box-dialog-item
                       :dialog-item-text "%"
                       :view-position (make-point 106 90)
                       :view-size (make-point 30 16)
                       :view-font *patchwork-font-spec*))
  (setf (slot-value self 'dur-range )
        (make-instance 'check-box-dialog-item
                       :dialog-item-text "range"
                       :view-position (make-point 134 90)
                       :view-size (make-point 50 16)
                       :view-font *patchwork-font-spec*))
  (setf (slot-value self 'dur-val1 )
        (make-instance 'editable-text-dialog-item
                       :dialog-item-text "0"
                       :view-position (make-point 190 90)
                       :view-size (make-point 40 16)
                       :view-font *patchwork-font-spec*))
  (setf (slot-value self 'dur-val2 ) 
        (make-instance 'editable-text-dialog-item
                       :dialog-item-text "0"
                       :view-position (make-point 235 90)
                       :view-size (make-point 40 16)
                       :view-font *patchwork-font-spec*))
  (setf (slot-value self 'off-button )
        (make-instance 'check-box-dialog-item
                       :dialog-item-text "offset"
                       :view-position (make-point 20 120)
                       :view-size (make-point 80 16)
                       :view-font *patchwork-font-spec*))
  (setf (slot-value self 'off-% )
        (make-instance 'check-box-dialog-item
                       :dialog-item-text "%"
                       :view-position (make-point 106 120)
                       :view-size (make-point 30 16)
                       :view-font *patchwork-font-spec*))
  (setf (slot-value self 'off-range)
        (make-instance 'check-box-dialog-item
                       :dialog-item-text "range"
                       :view-position (make-point 134 120)
                       :view-size (make-point 50 16)
                       :view-font *patchwork-font-spec*))
  (setf (slot-value self 'off-val1)
        (make-instance 'editable-text-dialog-item
                       :dialog-item-text "0"
                       :view-position (make-point 190 120)
                       :view-size (make-point 40 16)
                       :view-font *patchwork-font-spec*))
  (setf (slot-value self 'off-val2 )
        (make-instance 'editable-text-dialog-item
                       :dialog-item-text "0"
                       :view-position (make-point 235 120)
                       :view-size (make-point 40 16)
                       :view-font *patchwork-font-spec*))
  (setf (slot-value self 'OK-button)
        (make-instance 'button-dialog-item
                       :default-button t
                       :dialog-item-text "OK"
                       :view-position (make-point 80 150)
                       :view-size (make-point 40 23)
                       :dialog-item-action
                       (lambda (item) (declare (ignore item)) (do-chosen-action self))))
  (setf (slot-value self 'Cancel-button) 
        (make-instance 'button-dialog-item
                       :default-button ()
                       :dialog-item-text "cancel"
                       :view-position (make-point 160 150)
                       :view-size (make-point 60 25)
                       :dialog-item-action
                       (lambda (item) (declare (ignore item)) (return-from-modal-dialog nil))))  )

(defgeneric use-staff (self num staff))
(defmethod use-staff ((self C-MN-view-mod) num staff)
  (dolist (panel (editor-objects self))
    (setf (staff-list panel) staff)
    (setf (staff-num panel) num)
    (update-editor panel)))

(defgeneric Do-selections (self sel))
(defmethod Do-selections ((self C-MN-view-mod) sel)
  (let ((selections))
    (unselect-all-chords (car (editor-objects self)) 0 0)
    (dolist (panel (editor-objects self))      
      (setq selections (if sel (chords (chord-line panel))
                           (visible-chords panel)))
      (dolist (chord selections)
        (hilite-if-not-selected panel chord 0 0)
        (setf (selected-chords panel)
              (insert-in-time panel (selected-chords panel) chord))))))

(defmethod Do-selections-all ((self C-MN-view-mod)) 
  (let ((selections))
    (dolist (panel (if (active-editor self) (list (active-editor self))
                       (editor-objects self)))      
      (setq selections (chords (chord-line panel)))
      (dolist (chord selections)
        (hilite-if-not-selected panel chord 0 0)
        (setf (selected-chords panel)
              (insert-in-time panel (selected-chords panel) chord))))))

(defmethod play-all-staffs ((self C-MN-view-mod))
    (if (get-ctrl-setting self :offs)
      (play-the-chords self nil 'all)
      (call-next-method)))

(defgeneric play-the-chords (self sel &optional all))
(defmethod play-the-chords ((self C-MN-view-mod) sel &optional all)
  (declare (special *MN-great-advance*))
  (let* ((*current-music-notation-scale* 
           (or (local-scale self) *current-music-notation-scale*))
         (*current-approx-scale* (or (local-approx self) *current-approx-scale*))
         (the-panels (editor-objects self))
         (a-panel (car the-panels))
         (x-origo (truncate 
                   (scaled-mouse-h  a-panel (point-h (view-scroll-position a-panel)))))
         all-the-chords notes beginT)
    (declare (ignorable beginT))
    (dolist (panel the-panels)
      (let ((chords 
              (cond (all (chords (chord-line panel)))
                    (sel (visible-chords panel))
                    (t (selected-chords panel)))))
        (setq all-the-chords (append chords all-the-chords))))
    (when all-the-chords
      (setq notes (order-the-notes (chord-line a-panel) all-the-chords x-origo))
      (setq beginT (abs (cdr (car notes))))
      (setf patchwork.scheduler::*print-on-late?* t)
      (start (apdfuncall  10 2 15       ;beginT 2 (+ beginT 10)
                          'play-chosen-chords (chord-line a-panel) notes (cdr (car notes)))))))

(defgeneric transpose-selection (self))
(defmethod transpose-selection ((self C-MN-view-mod))
  (let* ((numbox (nth (- (length (external-controls self)) 4) (external-controls self)))
         (cents (patch-value numbox self)))
    (if (numberp cents)
        (dolist (panel (editor-objects self))
          (dolist (chord (selected-chords panel))
            (transpose-chord chord cents))
          (update-editor panel))
        (print (format nil "~A is not a midic" cents)))))

(defgeneric transpose-chord (self cents))
(defmethod transpose-chord ((self C-chord) cents)
  (tell (notes self) 'transpose-note cents)
  (update-chord self))

(defgeneric retro-selection (self))
(defmethod retro-selection ((self C-MN-view-mod))
  (let ((selections (apply #'append (ask-all (editor-objects self) 'selected-chords))))
    (when selections
      (let ((max-time (t-time (car (last selections))))
            (min-time (t-time (car selections))))
        (dolist (chord selections)
          (setf (t-time chord) (- max-time  (- (t-time chord) min-time)))
          (dolist (note (notes chord))
            (setf (offset-time note) (- 0 (offset-time note))))
          (update-the-chord-line self chord)))
      (tell (editor-objects self) 'update-editor))))

(defgeneric update-the-chord-line (self chord &optional no-selection))
(defmethod update-the-chord-line ((self C-MN-view-mod) chord &optional no-selection)
  (dolist (panel (editor-objects self))
    (when (member chord (chords (chord-line panel)))
      (remove-chord (chord-line panel) chord t)
      (add-new-chord (chord-line panel) chord)
      (unless no-selection
        (setf (selected-chords panel)
              (insert-in-time panel (remove chord (selected-chords panel)) chord))))))

(defvar *Cresc-action* ())
(defgeneric accell-selection (self))
(defmethod accell-selection ((self C-MN-view-mod))
  (declare (special *Cresc-action*))
  (let ((dialog (make-instance 'dialog :window-show nil :view-size (make-point 285 200)
                                       :window-type :double-edge-box 
                                       :view-position (make-point 100 200))))
    (add-subviews  dialog
                   (slot-value self 'vel-%)
                   (slot-value self 'vel-range)
                   (slot-value self 'vel-val1)
                   (slot-value self 'vel-val2)
                   (slot-value self 'cancel-button)
                   (slot-value self 'OK-button))
    (setf (slot-value self 'selections) 
          (apply #'append (ask-all (editor-objects self) 'selected-chords)))
    (setf *cresc-action* ())
    (modal-dialog dialog)))

(defgeneric do-chosen-action (self))
(defmethod do-chosen-action ((self C-MN-view-mod))
  (if *Cresc-action* (do-interpols self) (do-accell self)))

(defgeneric do-accell (self))
(defmethod do-accell ((self C-MN-view-mod) )
  (let* ((selections (slot-value self 'selections))
         (count (length selections))
         (percent (check-box-checked-p (slot-value self 'vel-%)))
         (range (check-box-checked-p (slot-value self 'vel-range)))
         (valmin (read-from-string (dialog-item-text (slot-value self 'vel-val1))))
         (valmax (read-from-string (dialog-item-text (slot-value self 'vel-val2))))
         (vals (if range (interpol count valmin valmax))))
    (dolist (chord selections)
      (setf (t-time chord)
            (max 0
                 (+ (t-time chord)
                    (if percent 
                        (truncate (* (t-time chord) (or (pop vals) valmin)) 100)
                        (or (pop vals) valmin)))))
      (update-the-chord-line self chord t))
    (tell (editor-objects self) 'update-editor)
    (return-from-modal-dialog ())))
    

(defgeneric update-velocity (self vel))
(defmethod update-velocity ((self C-chord) vel)
  (dolist (note (notes self))
    (setf (vel note) vel)))

(defgeneric Cresc-selection (self))
(defmethod Cresc-selection ((self C-MN-view-mod))
  (let ((dialog (make-instance 'dialog :window-show nil :view-size #@(285 200) 
                                       :window-type :double-edge-box 
                                       :view-position (ui:make-point 100 200))))
    (add-subviews dialog
                  (slot-value self 'dial-stF) (slot-value self 'dial-stT)
                  (slot-value self 'vel-button) (slot-value self 'vel-%)
                  (slot-value self 'vel-range) (slot-value self 'vel-val1)
                  (slot-value self 'vel-val2)
                  (slot-value self 'dur-button) (slot-value self 'dur-%)
                  (slot-value self 'dur-range) (slot-value self 'dur-val1)
                  (slot-value self 'dur-val2)
                  (slot-value self 'off-button) (slot-value self 'off-%)
                  (slot-value self 'off-range) (slot-value self 'off-val1)
                  (slot-value self 'off-val2)
                  (slot-value self 'cancel-button) (slot-value self 'OK-button))
    (setf (slot-value self 'selections) 
          (apply #'append (ask-all (editor-objects self) 'selected-chords)))
    (setf *Cresc-action* t)
    (modal-dialog dialog)))

(defgeneric do-interpols (self))
(defmethod do-interpols ((self C-MN-view-mod))
  (let* ((selections (slot-value self 'selections))
         (count (length selections)))
    (when selections
      (if (check-box-checked-p (slot-value self 'vel-button))
          (interpol-velocity self selections count))
      (if (check-box-checked-p (slot-value self 'dur-button))
          (interpol-duration self selections count))
      (if (check-box-checked-p (slot-value self 'off-button))
          (interpol-offset self selections count))
      (tell (editor-objects self) 'update-editor))
    (return-from-modal-dialog ())))

(defgeneric interpol-velocity (self selections count))
(defmethod interpol-velocity ((self C-MN-view-mod) selections count)
  (let ((percent (check-box-checked-p (slot-value self 'vel-%)))
        (range (check-box-checked-p (slot-value self 'vel-range)))
        (valmin (read-from-string (dialog-item-text (slot-value self 'vel-val1))))
        (valmax (read-from-string (dialog-item-text (slot-value self 'vel-val2)))))
    (transform-chord-notes self 'vel  selections count percent range valmin valmax)))

(defgeneric interpol-duration (self selections count))
(defmethod interpol-duration ((self C-MN-view-mod) selections count)
  (let ((percent (check-box-checked-p (slot-value self 'dur-%)))
        (range (check-box-checked-p (slot-value self 'dur-range)))
        (valmin (read-from-string (dialog-item-text (slot-value self 'dur-val1))))
        (valmax (read-from-string (dialog-item-text (slot-value self 'dur-val2)))))
    (transform-chord-notes self 'dur  selections count percent range valmin valmax)))

(defgeneric interpol-offset (self selections count))
(defmethod interpol-offset ((self C-MN-view-mod) selections count)
  (let ((percent (check-box-checked-p (slot-value self 'off-%)))
        (range (check-box-checked-p (slot-value self 'off-range)))
        (valmin (read-from-string (dialog-item-text (slot-value self 'off-val1))))
        (valmax (read-from-string (dialog-item-text (slot-value self 'off-val2)))))
    (transform-chord-notes self 'offset  selections count percent range valmin valmax)
    (update-editor self)))
    
(defgeneric transform-chord-notes (self field selections count perc range valmin valmax))
(defmethod transform-chord-notes ((self C-MN-view-mod) field selections count perc range valmin valmax) 
  (let ((vals (if range (interpol count valmin valmax))) (value))
    (dolist (chord selections)
      (setq value (if vals (pop vals) valmin))
      (dolist (note (notes chord))
        (transform-note-field note field perc value)))))

(defgeneric transform-note-field (self field perc value))
(defmethod transform-note-field ((self C-note) field perc value)
  (let* ((field-value (slot-value self field))
         (value (if (< field-value 0)
                    (- field-value
                       (if perc (- 0 (truncate (* field-value value) 100)) value))
                    (+ field-value
                       (if perc (truncate (* field-value value) 100) value)))))
    (setf (slot-value self field)
          (cond ((eql field 'vel)
                 (if (> value 127) 127 (max 0 value)))
                ((eql field 'dur) (max 1 value))
                (t value)))))

(defgeneric update-editor (self))
(defmethod update-editor ((self C-MN-view-mod)) 
  (update-view-controler self))

(defgeneric set-staff-count (self value))
(defmethod set-staff-count ((self C-MN-view-mod) value)
  (if (not (monofonic-mn? self))
      (progn (format t "This is a poly-collector. Please do staff additions by box extension")
             (ui:ed-beep))
      (let* ((all-panels (editor-objects self))
             (current-length (length all-panels)))
        (cond ((< value current-length)
               (unselect-all-chords (car all-panels) 0 0)
               (apply #'remove-subviews (cons self (nthcdr value all-panels)))
               (setf (editor-objects self) (butlast (editor-objects self) (- current-length value)))
                                        ;(view-window-grown self)
               (update-editor self))
              ((> value current-length)
               (unselect-all-chords (car all-panels) 0 0)
               (let ((size (view-size (car all-panels)))
                     (max-y
                       (apply 'max  (mapcar (lambda (panel) (+ (point-v (view-size panel))
                                                               (point-v (view-position panel))))
                                            all-panels)))
                     (last-panel-home (origin (car (last all-panels))))
                     new-panels editor-now)
                 (dotimes (i (- value current-length))
                   (push
                    (setq editor-now
                          (make-instance 'C-MN-panel-Mod
                                         :view-container self
                                         :view-position (make-point 0 (+ 2 max-y))
                                         :view-size size
                                         :h-scrollp nil
                                         :view-font *music-font-spec*
                                         :origin 
                                         (setq last-panel-home (+ last-panel-home (- *MN-draw-offset*) (point-h size)))))
                    new-panels)
                   (setq max-y (+ 2 max-y (point-v size)))
                   (setf (chord-line editor-now) (chord-line (car all-panels))))
                 (setf (editor-objects self) (nconc all-panels (nreverse new-panels)))
                 (update-editor self)))
              (t nil)))))

(defgeneric monofonic-mn? (self))
(defmethod monofonic-mn? ((self C-MN-view-mod)) 
  (not (polifonic? (pw-object (view-container self)))))

(defgeneric update-editor-objects (self))
(defmethod update-editor-objects ((self C-MN-view-mod))
  (tell (editor-objects self) 'update-editor))

(defmethod update-all-chord-lines ((self C-MN-view-mod) chord-lines)
  (let ((editors (editor-objects self)))
    (while (and chord-lines editors)
      (setf (chord-line (pop editors)) (pop chord-lines)))))

(defgeneric scroll-editor-to-home (self))
(defmethod scroll-editor-to-home ((self C-MN-view-mod))
  (update-editor self))

;;========================
(defgeneric order-the-notes (self the-chords t-offset))
(defmethod order-the-notes ((self C-chord-line) the-chords t-offset)
  (declare (ignore t-offset))
  (when the-chords
    (let ((all-notes) (beg-t))
      (dolist (chord the-chords)
        (setq beg-t (t-time chord))
        (dolist (note (notes chord))
          (push (cons note (+ beg-t (offset-time note))) all-notes)))
      (setq all-notes (sort all-notes #'< :key #'cdr)))))

(defgeneric play-your-chords (self))
(defmethod play-your-chords ((self C-chord-line))
  (let* ((the-notes (order-the-notes self (chords self) 0)))
    (when the-notes
      (setf patchwork.scheduler::*print-on-late?* t)
      (start (apdfuncall 10 2 15
                         'play-chosen-chords self the-notes (cdr (car the-notes)))))))

(defparameter *MN-mod-play-advance* 20)    ; time advance for chord scheduling

(defgeneric play-chosen-chords (self the-notes t-offset))
(defmethod play-chosen-chords ((self C-chord-line) the-notes t-offset)
  (when the-notes
    (let ((start-time)
          (approx (compute-approx)))
      (setf *MN-play-flag* t)
      (setq start-time (cdr (car the-notes)))
      (apdfuncall 10 (priority) (-  start-time t-offset)
                  'keep-playing-notes self the-notes start-time approx))))

(defun micro-channel (midic)
  (+ 1 (/ (mod midic 100) 25)))

(defgeneric keep-playing-notes (self notes start-time &optional approx))
(defmethod keep-playing-notes ((self C-chord-line) notes start-time &optional approx)
  (when *MN-play-flag*
    (cond ((eql *playing-option* :pb)
           (play-note (car (pop notes)) approx)
           (while (and notes (= start-time (cdr (car notes))))
             (play-note (car (pop notes)) approx)))
          ((eql *playing-option* :mc)
           (let ((approx-m (approx-for-playing (midic (caar notes)) approx)))
             (write-midi-note (dur (caar notes)) (+ (chan (caar notes)) (micro-channel approx-m) -1) 
                              (truncate approx-m 100) (vel (car (pop notes))))
             (while (and notes (= start-time (cdr (car notes))))
               (setq approx-m (approx-for-playing (midic (caar notes)) approx))
               (write-midi-note (dur (caar notes)) (+ (chan (caar notes)) (micro-channel approx-m) -1) 
                                (truncate approx-m 100) (vel (car (pop notes))))))))
    (if notes
        (re-dfuncall (- (cdr (car notes)) start-time)
                     self notes (cdr (car notes)) approx))))

;;;=================
;;; an MN window sub-class for dealing with selected chords
;;;
;;;======

(defclass C-mn-window-mod (C-mn-window) ())

(defmethod view-deactivate-event-handler ((self C-mn-window-mod))
  (menu-item-disable *undo-MN-menu*)
  (let* ((mus-view (car (subviews self)))
         (panels (editor-objects mus-view)))
    (setf (saved-selected mus-view)
          (apply #'append (ask-all panels 'selected-chords)))
    ;(unselect-all-chords (car panels) 0 0)
    (call-next-method)))

(defvar *the-chord-editors* ())

(defmethod window-close ((self C-mn-window-mod))
  (dolist (the-ed *the-chord-editors*)
    (if (wptr the-ed) (window-close the-ed)))
  (call-next-method))

;;;; THE END ;;;;
