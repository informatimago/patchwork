;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               rtm-paging+kill.lisp
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

;;;;;;; paging

(defun calc-next-rtm-page (self)
  (let* ((beat-number (1- (value (beat-number-ctrl (editor-collection-object self)))))
         (all-measures (measures (measure-line (car (beat-editors (editor-collection-object self))))))
         (measures (nthcdr beat-number all-measures))
         (beg-x 28)(new-beg-x beg-x) (end-x (w (car (beat-editors (editor-collection-object self)))))
         (beat-zoom (beat-zoom (car (beat-editors (editor-collection-object self)))))
         (monof? (= (length (beat-editors (editor-collection-object self))) 1))
         (beat-number-temp (1+ beat-number))
         (staff-count (visible-staffs-count (editor-collection-object self))))
    (incf beat-number)
    (if (not monof?)
      (while (and measures (< new-beg-x end-x))
        (setq new-beg-x (+ *rtm-editor-measure-x-offset* (calc-measure-pixel-x (car measures) beat-zoom new-beg-x)))
        (when (< new-beg-x end-x) (incf beat-number))
        (pop measures))
      (for (i 0 1 (1- staff-count))
        (setq new-beg-x beg-x)
        (while (and measures (< new-beg-x end-x))
          (setq new-beg-x (+ *rtm-editor-measure-x-offset* (calc-measure-pixel-x (car measures) beat-zoom new-beg-x)))
          (when (< new-beg-x end-x)
            (incf beat-number)
            (pop measures)))))
    (if (> beat-number (length all-measures))
      beat-number-temp beat-number)))

(defun calc-prev-rtm-page (self)
  (let* ((beat-number (1- (value (beat-number-ctrl (editor-collection-object self)))))
         (measures (reverse (subseq  (measures (measure-line (car (beat-editors (editor-collection-object self))))) 0 beat-number)))
         (beg-x 28)(new-beg-x beg-x) (end-x (w (car (beat-editors (editor-collection-object self)))))
         (monof? (= (length (beat-editors (editor-collection-object self))) 1))
         (beat-zoom (beat-zoom (car (beat-editors (editor-collection-object self)))))
         (staff-count (visible-staffs-count (editor-collection-object self))))
    (incf beat-number)
    (if (not monof?)
      (while (and measures (< new-beg-x end-x))
        (setq new-beg-x (+ *rtm-editor-measure-x-offset* (calc-measure-pixel-x (car measures) beat-zoom new-beg-x)))
        (when (< new-beg-x end-x) (decf beat-number))
        (pop measures))
      (for (i 0 1 (1- staff-count))
        (setq new-beg-x beg-x)
        (while (and measures (< new-beg-x end-x))
          (setq new-beg-x (+ *rtm-editor-measure-x-offset* (calc-measure-pixel-x (car measures) beat-zoom new-beg-x)))
          (when (< new-beg-x end-x)
            (decf beat-number)
            (pop measures)))))
    (max 1 beat-number)))

(defun calc-next-rtm-page+scroll (self beat-number)
  (unless beat-number (setq beat-number (calc-next-rtm-page self)))
  (unless (= beat-number (value (beat-number-ctrl (editor-collection-object self))))
    (set-dialog-item-text-from-dialog
     (beat-number-ctrl (editor-collection-object self)) (format nil "~5D" beat-number))
    (scroll-beat (editor-collection-object self)(beat-number-ctrl (editor-collection-object self))))
  beat-number)

(defun calc-prev-rtm-page+scroll (self beat-number)
  (unless beat-number (setq beat-number (calc-prev-rtm-page self)))
  (unless (= beat-number (value (beat-number-ctrl (editor-collection-object self))))
    (set-dialog-item-text-from-dialog
     (beat-number-ctrl (editor-collection-object self)) (format nil "~5D" beat-number))
    (scroll-beat (editor-collection-object self)(beat-number-ctrl (editor-collection-object self))))
  beat-number)

;;changed by aaa 28-08-95 from pw-modif
(defun play-rtms+scroll (self)
  (let ((editors (give-selected-editors self)))
    (calc-next-rtm-page+scroll (view-window self) 1)
    (setf *mn-view-offset-flag* (check-box-checked-p (third (rtm-radio-ctrls self))))
    (start
      (apdfuncall 100 (priority) 15
                  (lambda ()
                      (play-measure-line+scroll (measure-line (car editors)) (view-window self) (get-play-speed self) 0 (calc-next-rtm-page (view-window self)))
                      (tell (ask-all (cdr editors) 'measure-line) 'play-measure-line (get-play-speed self)))))))

;; (defun play-rtms+scroll (self)
;;   (let ((editors (give-selected-editors self)))
;;     (calc-next-rtm-page+scroll (view-window self) 1)
;;     (setf *mn-view-offset-flag* (check-box-checked-p (third (rtm-radio-ctrls self))))
;;     (start
;;       (apdfuncall 100 (priority) 15
;;                   (lambda ()
;;                       (play-measure-line+scroll (measure-line (car editors)) (view-window self) (get-play-speed self) 0 (calc-next-rtm-page (view-window self)))
;;                       (tell (ask-all (cdr editors) 'measure-line) 'play-measure-line (get-play-speed self)))))))


(defgeneric play-measure-line+scroll (self win t-scfactor beat-number next-page-beat-num)
  (:method ((self C-measure-line) win t-scfactor beat-number next-page-beat-num)
    (setf (stop-flag self) nil)
    (play-measure-line-continue+scroll  self win (measures self) t-scfactor beat-number next-page-beat-num)))

(defgeneric play-measure-line-continue+scroll (self win measures t-scfactor beat-number next-page-beat-num)
  (:method ((self C-measure-line) win measures t-scfactor beat-number next-page-beat-num)
    (unless (stop-flag self)
      (when measures
        (when (= (1+ beat-number) next-page-beat-num)
          (calc-next-rtm-page+scroll win (1+ beat-number))
          (setq next-page-beat-num (calc-next-rtm-page win)))
        (incf beat-number)
        (let ((delay (play-measure (pop measures) t-scfactor)))
          (dfuncall (truncate (abs delay))
                    'play-measure-line-continue+scroll self win measures t-scfactor beat-number next-page-beat-num))))))


;;(ui:add-menu-items *RTM-menu* (new-leafmenu "Play-rtms+scroll" (lambda () (play-rtms+scroll (editor-collection-object *active-rtm-window*)))))
;;; only polif
;; (defun calc-next-rtm-page (self)
;;   (let* ((beat-number (1- (value (beat-number-ctrl (editor-collection-object self)))))
;;          (all-measures (measures (measure-line (car (beat-editors (editor-collection-object self))))))
;;          (measures (nthcdr beat-number all-measures))
;;          (beg-x 28)(new-beg-x beg-x) (end-x (w (car (beat-editors (editor-collection-object self)))))
;;          (beat-zoom (beat-zoom (car (beat-editors (editor-collection-object self)))))
;;          (beat-number-temp (1+ beat-number)))
;;     (incf beat-number)
;;     (while (and measures (< new-beg-x end-x))
;;       (setq new-beg-x (+ *rtm-editor-measure-x-offset* (calc-measure-pixel-x (car measures) beat-zoom new-beg-x)))
;;       (when (< new-beg-x end-x) (incf beat-number))
;;       (pop measures))
;;     (if (> beat-number (length all-measures))
;;          beat-number-temp beat-number)))
;;
;;
;; (defun calc-prev-rtm-page (self)
;;   (let* ((beat-number (1- (value (beat-number-ctrl (editor-collection-object self)))))
;;          (measures (reverse (subseq  (measures (measure-line (car (beat-editors (editor-collection-object self))))) 0 beat-number)))
;;          (beg-x 28)(new-beg-x beg-x) (end-x (w (car (beat-editors (editor-collection-object self)))))
;;          (beat-zoom (beat-zoom (car (beat-editors (editor-collection-object self))))))
;;     (incf beat-number)
;;     (while (and measures (< new-beg-x end-x))
;;       (setq new-beg-x (+ *rtm-editor-measure-x-offset* (calc-measure-pixel-x (car measures) beat-zoom new-beg-x)))
;;       (when (< new-beg-x end-x) (decf beat-number))
;;       (pop measures))
;;     (max 1 beat-number)))


;;;KILL
(defmethod remove-yourself-control ((self C-patch-PolifRTM))
  (when (application-object self)
    (remove-yourself-control (application-object self))))

(defmethod remove-yourself-control ((self C-patch-application-rtm-editor))
  (when (application-object self)
    (remove-yourself-control (application-object self))))

(defmethod remove-yourself-control ((self C-rtm-editor-window))
  (tell (ask-all (beat-editors (editor-collection-object self)) #'measure-line) #'kill-chords)
  (window-close  self))

(defmethod kill-chords ((self C-beat))
  (setf *beat-leaf-objs* ())
  (tell (ask-all (collect-all-chord-beat-leafs self) #'beat-chord) #'kill-notes))

(defmethod kill-chords ((self C-measure))
  (setf *beat-leaf-objs* ())
  (tell (ask-all (collect-all-chord-beat-leafs self) #'beat-chord) #'kill-notes))

(defmethod kill-chords ((self C-measure-line))
  (tell (ask-all (collect-all-chord-beat-leafs self) #'beat-chord) #'kill-notes))

