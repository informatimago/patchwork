;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               rtm-window.lisp
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
(in-package :pw)

;;========================================================

(defclass C-rtm-editor-window (C-application-window) ())

(defmethod decompile ((self C-rtm-editor-window))
  `(let ((rtm-win (make-instance 'C-rtm-editor-window  :window-show nil :close-box-p nil
                    :window-title ,(window-title self) 
                    :view-position ,(view-position self) :view-size ,(view-size self))))  
       (add-subviews rtm-win ,(decompile (editor-collection-object self)))
       rtm-win))
 
;;========================
;; for printing ...
;;*screen-width* 
;; (progn 
;;   (set-view-size pw::*active-rtm-window* *screen-width* 1650) 
;;   (pw::resize-new-rtm-w (pw::editor-collection-object pw::*active-rtm-window*) (view-size pw::*active-rtm-window*)))

;;========================
(defmethod window-close ((self C-rtm-editor-window))
    (if (wptr *the-chord-rtm-editor*)
        (window-close *the-chord-rtm-editor*))
    (call-next-method))

(defmethod open-application-help-window ((self C-rtm-editor-window))
  (if *rtm-help-window*
      (unless (wptr *rtm-help-window*)
        (make-rtm-help-window))
      (make-rtm-help-window))
  (window-select *rtm-help-window*))


;; (defmethod window-grow-event-handler ((self C-rtm-editor-window) where)
;;   (declare (ignore where))
;;   (call-next-method)
;;   (resize-new-rtm-w (editor-collection-object self)(view-size self))
;;   (erase+view-draw-contents self))

(defmethod window-grow-event-handler ((self C-rtm-editor-window) where)
  (declare (ignore where))
  (call-next-method)
  (invalidate-corners self (make-point 0 0) (view-size self))
  (resize-new-rtm-w (editor-collection-object self)(view-size self))
  (event-dispatch)
  (window-update-event-handler self)
)

#|
(defmethod window-zoom-event-handler ((self C-rtm-editor-window) where)
  (declare (ignore where))
  (call-next-method)
  (resize-new-rtm-w (editor-collection-object self)(view-size self))
  (erase+view-draw-contents self))
|#

(defmethod window-zoom-event-handler ((self C-rtm-editor-window) where)
  (declare (ignore where))
  (call-next-method)
  (invalidate-corners self (make-point 0 0) (view-size self))
  (resize-new-rtm-w (editor-collection-object self)(view-size self))
  (event-dispatch)
  (window-update-event-handler self))

(defgeneric editor-collection-object (self)
  (:method ((self C-rtm-editor-window))
    (car (subviews self))))

(defun record-rtm-chords-from-midi (beat-objs)
  (let ( midics t-time dur vel ins chord 
        (list (quantize-chords (read-from-midi))))
     (while (and beat-objs list)
        (setq t-time (t-time (beat-chord (car beat-objs))))
        (setq dur (dur (car (notes (beat-chord (car beat-objs))))))
        (setq vel (vel (car (notes (beat-chord (car beat-objs))))))
        (setq ins (instrument (car (notes (beat-chord (car beat-objs))))))
        (setq midics (mapcar #'* (mapcar #'second (cdar list)) (cirlist 100))) 
        (setf (beat-chord (pop beat-objs)) (setq chord (make-chord-object midics t-time)))
        (set-all-durs chord dur)
        (set-all-vels chord vel)
        (set-all-instruments chord ins)
        (pop list))))



(defmethod key-pressed-extra ((self C-rtm-editor-window) char)
  (case char
    ((:ForwardArrow)  (calc-next-rtm-page+scroll self ()))
    ((:BackArrow)  (calc-prev-rtm-page+scroll self ()))
    (#\+  (set-dialog-item-text-from-dialog
           (beat-number-ctrl (editor-collection-object self))
           (format nil "~5D" (1+ (value (beat-number-ctrl (editor-collection-object self))))))
          (scroll-beat (editor-collection-object self)(beat-number-ctrl (editor-collection-object self)))) 
    (#\-  (set-dialog-item-text-from-dialog
           (beat-number-ctrl (editor-collection-object self))
           (format nil "~5D" (max 1 (1- (value (beat-number-ctrl (editor-collection-object self)))))))
          (scroll-beat (editor-collection-object self)(beat-number-ctrl (editor-collection-object self)))) 
    (#\H  (set-dialog-item-text-from-dialog
           (beat-number-ctrl (editor-collection-object self)) (format nil "~5D" 1))
          (scroll-beat (editor-collection-object self)(beat-number-ctrl (editor-collection-object self)))) 
    (#\L  (set-dialog-item-text-from-dialog
           (beat-number-ctrl (editor-collection-object self)) 
           (format nil "~5D" (max 1 (length (measures (measure-line (car (beat-editors (editor-collection-object self)))))))))
          (scroll-beat (editor-collection-object self)(beat-number-ctrl (editor-collection-object self)))) 
    (#\p (when (rtm-selection-1 (editor-collection-object self)) 
           (let* ((nth-measure (position (rtm-selection-1 (editor-collection-object self))
                                         (measures (measure-line (current-rtm-editor (editor-collection-object self))))))
                  measures)
             (when nth-measure 
               (setq measures 
                     (mapcar #'nth (cirlist nth-measure)
                             (ask-all (ask-all (give-selected-editors (editor-collection-object self)) #'measure-line) #'measures)))
               (setq measures (remove nil measures))
               (setf *mn-view-offset-flag* (check-box-checked-p (third (rtm-radio-ctrls (editor-collection-object self)))))
               (start 
                (apdfuncall  100 (priority) 15 
                             (lambda ()
                                 (tell measures #'play-measure (get-play-speed (editor-collection-object self))))))))))
    (#\P (when (eql 'C-measure (class-name (class-of (rtm-selection-1 (editor-collection-object self)))))
           (start
            (setf *mn-view-offset-flag* (check-box-checked-p (third (rtm-radio-ctrls (editor-collection-object self)))))
            (apdfuncall 100 (priority) 15 
                        (lambda ()
                            (play-measure (rtm-selection-1 (editor-collection-object self)) (get-play-speed (editor-collection-object self))))))))
    (#\a (when (rtm-selection-1 (editor-collection-object self))
           (if (eql 'C-measure-line (class-name (class-of (rtm-selection-1 (editor-collection-object self)))))
               (if (measures (measure-line (current-rtm-editor (editor-collection-object self))))
                   (add-beat-after (car (last (measures (measure-line (current-rtm-editor (editor-collection-object self)))))))
                   (add-beat-after-myself (rtm-selection-1 (editor-collection-object self)) ()))
               (add-beat-after (rtm-selection-1 (editor-collection-object self))))
           (update-all-beat-groupings)
           (erase+view-draw-contents (current-rtm-editor (editor-collection-object self)))))
    (#\b (when (rtm-selection-1 (editor-collection-object self)) 
           (unless (eql 'C-measure-line (class-name (class-of (rtm-selection-1 (editor-collection-object self)))))
             (add-beat-before (rtm-selection-1 (editor-collection-object self)))
             (update-all-beat-groupings)
             (erase+view-draw-contents (current-rtm-editor (editor-collection-object self))))))
    (#\r (when (rtm-selection-1 (editor-collection-object self)) 
           (record-rtm-chords-from-midi (give-rtm-range-chords (editor-collection-object self) nil))
           (erase+view-draw-contents (current-rtm-editor (editor-collection-object self)))))
    (#\k (read-from-midi)) 
    (#\K  
     (tell (ask-all (beat-editors (editor-collection-object self)) 'measure-line) 'kill-all-measures)
     (erase+view-draw-contents self))
    ((:Backspace) 
     (let (m-line?)
       (if (eql 'C-measure-line
               (class-name
                (class-of
                 (setq m-line? (rtm-selection-1
                                (editor-collection-object self))))))
           (progn
             (kill-chords m-line?)(setf (measures m-line?) ())
             (erase+view-draw-contents (current-rtm-editor (editor-collection-object self))))
           (when (rtm-selection-1 (editor-collection-object self)) 
             (remove-beat-from-measure (rtm-selection-1 (editor-collection-object self)))
             (update-all-beat-groupings)
             (erase+view-draw-contents (current-rtm-editor (editor-collection-object self)))))))
    (#\S  (tell (beat-editors (editor-collection-object self)) 'set-selection-button t))
    (#\U  (tell (beat-editors (editor-collection-object self)) 'set-selection-button nil))
    (#\D  (erase+view-draw-contents self))
    (#\e  (let ((e-ctrl (beat-edit-ctrl (editor-collection-object self))))
            (if (check-box-checked-p e-ctrl)
                (check-box-uncheck  e-ctrl) (check-box-check  e-ctrl))
            (set-edit-mode (editor-collection-object self) e-ctrl)))
    (t (ui:ed-beep))))
  
;;=================

(defmethod view-activate-event-handler :after ((self C-rtm-editor-window))
  (when (pw-object self)
    (draw-appl-label (pw-object self) #\*))
  (setq *active-RTM-window* self) 
  (ui:set-menubar *RTM-menu-root*)
  (enable-all-apps-menu-items)
  (menu-item-disable *apps-RTM-menu-item*))

(defmethod view-deactivate-event-handler :after ((self C-rtm-editor-window))
  (when (pw-object self)
    (draw-appl-label (pw-object self) #\A))
  (when (eql *active-RTM-window* self)  ; no RTM window selected
    (menu-item-enable *apps-RTM-menu-item*)
    (enable-Lisp-apps-menu-item)))

;;;; THE END ;;;;
