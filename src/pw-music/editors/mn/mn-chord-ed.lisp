;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               mn-chord-ed.lisp
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

(defvar *mn-view-order-flag* ())
(defvar *mn-view-time-flag* ())
(defvar *mn-view-channel-flag* ())

(defclass C-MN-panel-ChordBox (C-music-notation-panel)
  ((undo-MN-edit :initform nil :accessor undo-MN-edit)
   (sel-start :initform 0 :accessor sel-start)
   (sel-end :initform 0 :accessor sel-end)
   (selected-notes :initform nil :accessor selected-notes)))

(defgeneric give-mn-selection-chords (self))
(defmethod give-mn-selection-chords ((self C-MN-panel-ChordBox))
  (chord-line self))


(defmethod set-visible-chords ((self C-MN-panel-ChordBox))
  (setf (visible-chords self) (chords (chord-line self) )))

(defvar *mn-drag-order-cursor* 202)   ; the cursor for the "change order" edit function

(defmethod view-draw-specific  ((self C-MN-panel-ChordBox)
                                zoom-scale scroll-pos MN-offset MN-C5)
  (let ((*mn-view-order-flag* (get-ctrl-setting (view-container self) :order))
        (*mn-view-channel-flag* (get-ctrl-setting (view-container self) :channel))
        (*mn-view-arp-flag* (setting-of (view-container self) :arp))
        (*mn-view-time-flag* (setting-of (view-container self) :time)))
    (declare (special *mn-view-order-flag* *mn-view-arp-flag* *mn-view-time-flag* *mn-view-channel-flag*) )
    (set-visible-chords self)
    (cond (*mn-view-arp-flag*
           (draw-arpeggiated-chord self (car (visible-chords self))
                                   zoom-scale scroll-pos MN-offset MN-C5))
          (*mn-view-time-flag*
           (draw-offset-chord self (car (visible-chords self))
                              zoom-scale scroll-pos MN-offset MN-C5))
          ((not (visible-chords self)))
          (t
           (draw-chord (car (visible-chords self)) 1 
                       MN-offset (t-time (car (visible-chords self))) MN-C5)
           (if *mn-view-channel-flag*
               (draw-all-channels self (car (visible-chords self)) MN-offset
                                  (t-time (car (visible-chords self))) MN-C5)))) ))

(defparameter *arp-note-offset* 18)

;; (defmethod draw-arpeggiated-chord ((self C-MN-panel-ChordBox) chord 
;;                                    zoom-scale scroll-pos MN-offset MN-C5)
;;   (declare (ignore scroll-pos zoom-scale))
;;   (let ((notes (sort (notes chord) #'< :key #'order))
;;         (offset-x MN-offset)
;;         (y-min) (alt) (dx))
;;     (dolist (one-note notes)
;;       (setq y-min (1- (give-pixel-y one-note MN-C5)))
;;       (draw-ledger-lines-arp self one-note  offset-x y-min y-min MN-C5)
;;       (setq alt (alt-delta-x one-note) dx (delta-x one-note))
;;       (setf (alt-delta-x one-note) -12  (delta-x one-note) -6)
;;       (draw-note-4 one-note
;;                    (setf (order one-note) 
;;                          (calc-chord-pixel-x chord 1
;;                                              (setf (arp-view-x one-note) offset-x)
;;                                              (t-time chord)))
;;                    MN-C5 1)
;;       (draw-comment self (comm one-note) (order one-note))
;;       (if *mn-view-channel-flag*
;;           (draw-channel self one-note (- (order one-note) 6) (+ (give-pixel-y one-note MN-C5) 8)))
;;       (setf (alt-delta-x one-note) alt (delta-x one-note) dx)
;;       (incf offset-x *arp-note-offset*))
;;     (setf (notes chord) notes)
;;     (sort-notes chord))
;;   (if (zone-selection? self) (draw-zone-selection self)))

(defgeneric draw-arpeggiated-chord (self chord zoom-scale scroll-pos MN-offset MN-C5))
(defmethod draw-arpeggiated-chord ((self C-MN-panel-ChordBox) chord 
                                   zoom-scale scroll-pos MN-offset MN-C5)
  (declare (ignore scroll-pos zoom-scale))
  (let ((notes (sort (notes chord) #'< :key #'order))
        (offset-x MN-offset)
        (y-min) (alt) (dx))
    (dolist (one-note notes)
      (setq y-min (1- (give-pixel-y one-note MN-C5)))
      (draw-ledger-lines-arp self one-note
                             (truncate offset-x
                                       (MN-zoom-scaler (view-container self))) y-min y-min MN-C5)
      (setq alt (alt-delta-x one-note) dx (delta-x one-note))
      (setf (alt-delta-x one-note) -12  (delta-x one-note) -6)
      (draw-note-4 one-note
                   (setf (order one-note) 
                         (calc-chord-pixel-x chord 1
                                             (progn (setf (arp-view-x one-note) offset-x)
                                                    (truncate offset-x
                                                              (MN-zoom-scaler (view-container self))) )
                                             (t-time chord)))
                   MN-C5 1 )            ;zoom-scale)
      (draw-comment self (comm one-note) (order one-note))
      (if *mn-view-channel-flag*
          (draw-channel self one-note (- (order one-note) 6)
                        (+ (give-pixel-y one-note MN-C5) 8)))
      (setf (alt-delta-x one-note) alt (delta-x one-note) dx)
      (incf offset-x *arp-note-offset*))
    (setf (notes chord) notes)
    (sort-notes chord))
  (if (zone-selection? self) (draw-zone-selection self)))

(defvar *mn-comment-offset* 10)

;; (defmethod draw-comment ((self C-MN-panel-ChordBox) comment x)
;;   (if comment
;;       (let ((font (view-font (view-container (view-container self)))))
;;         (set-view-font (view-container (view-container  self))
;;                        '("Monaco"  8  :srcor))
;;         (draw-string (- x 5) (+ *mn-comment-offset* (point-v (view-position self)))
;;                      (if (stringp comment) comment (prin1-to-string comment)))
;;         (set-view-font (view-container (view-container  self)) font))))

(defgeneric draw-comment (self comment x))
(defmethod draw-comment ((self C-MN-panel-ChordBox) comment x)
  (if comment
      (with-font-focused-view (view-container (view-container self))
        (with-focused-view self
          (draw-string (- x 5) (+ *mn-comment-offset* (point-v (view-position self)))
                       (if (stringp comment) comment (prin1-to-string comment))) ))))

(defgeneric draw-channel (self note x y))
(defmethod draw-channel ((self C-MN-panel-ChordBox) note x y)
  (draw-string x y (format () "~D" (chan note))))

(defgeneric draw-all-channels (self chord MN-offset beg-t MN-C5))
(defmethod draw-all-channels ((self C-MN-panel-ChordBox) chord MN-offset beg-t MN-C5)
  (dolist (note (notes chord))
    (draw-channel self note 
                  (+ 10 (calc-chord-pixel-x chord 0 MN-offset beg-t))
                  (give-pixel-y note MN-C5))))


;;==================================================
;;view according to offset
;;===================================================
(defconstant *MN-left-offset* 100)

(defgeneric scaled-offset-time-x (self one-note))
(defmethod scaled-offset-time-x ((self C-MN-panel-ChordBox) one-note)
  (truncate (offset-time one-note) (MN-zoom-scaler (view-container self)) ))

(defgeneric draw-offset-chord (self chord zoom-scale scroll-pos MN-offset C5))
(defmethod draw-offset-chord ((self C-MN-panel-ChordBox) chord 
                              zoom-scale scroll-pos MN-offset C5)
  (declare (ignore scroll-pos))
  (let ((notes (notes chord))
        (y-min) (alt) (dx)
        (my-offset (+ MN-offset *MN-left-offset*)))
    (dolist (one-note notes)
      (setq y-min (1- (give-pixel-y one-note C5)))
      (draw-ledger-lines-arp self one-note 
                             (+ my-offset (scaled-offset-time-x self one-note))
                             y-min y-min C5)
      (setq alt (alt-delta-x one-note) dx (delta-x one-note))
      (setf (alt-delta-x one-note) -12  (delta-x one-note) -6)
      (draw-note-4 one-note 
                   (calc-chord-pixel-x 
                    chord  0
                    (+ my-offset (scaled-offset-time-x self one-note))
                    (t-time chord))
                   C5 zoom-scale)
      (setf (alt-delta-x one-note) alt (delta-x one-note) dx) )
    (draw-extra-info chord (calc-chord-pixel-x 
                            chord  0
                            my-offset
                            (t-time chord)) C5 ()))
  (if (zone-selection? self) (draw-zone-selection self)))

;;;==========================================================

(defmethod draw-single-dur-line-2  ((self C-MN-panel-ChordBox) chord note)
  (cond ((setting-of (view-container self) :arp)
         (draw-single-dur-line chord note
                               0 (arp-view-x note)
                               (t-time chord) *MN-C5*))
        ((setting-of (view-container self) :time) 
         (draw-single-dur-line chord note
                               0
                               (+ *MN-draw-offset* *MN-left-offset*
                                  (scaled-offset-time-x self note))
                               (t-time chord) *MN-C5*))
        (t (draw-single-dur-line chord note
                                 0 *MN-draw-offset* (t-time chord)
                                 *MN-C5*))))

(defgeneric draw-single-symbolic-dynamic-2 (self chord note))
(defmethod draw-single-symbolic-dynamic-2  ((self C-MN-panel-ChordBox) chord note)
  (set-view-font  (view-container (view-container  self)) *music-font-spec*);  TODO: SRCXOR -> INSTANCE
  (cond ((setting-of (view-container self) :arp) 
         (draw-single-symbolic-dynamic chord note
                                       0 *MN-draw-offset* 
                                       (t-time chord) *MN-C5*))
        ((setting-of (view-container self) :time) 
         (draw-single-symbolic-dynamic chord note
                                       0 
                                       (+ *MN-draw-offset* *MN-left-offset*
                                          (scaled-offset-time-x self note))
                                       (t-time chord) *MN-C5*))
        (t (draw-single-symbolic-dynamic chord note
                                         0 *MN-draw-offset* 
                                         (t-time chord)  *MN-C5*))))

(defgeneric draw-single-channel (self chord note))
(defmethod draw-single-channel  ((self C-MN-panel-ChordBox) chord note)
  (set-view-font  (view-container (view-container  self)) *music-font-spec*); TODO: SRCXOR -> INSTANCE
  (cond ((setting-of (view-container self) :arp) 
         (draw-channel self note (- (order note) 6) (+ (give-pixel-y note *MN-C5*) 8)))
        ((setting-of (view-container self) :time))
        (t (draw-channel self note 
                         (+ 10 (calc-chord-pixel-x chord 0 *MN-draw-offset* (t-time chord)))
                         (give-pixel-y note *MN-C5*)))))

(defgeneric draw-single-offset-line-2 (self chord note))
(defmethod draw-single-offset-line-2  ((self C-MN-panel-ChordBox) chord note)
  (unless (setting-of (view-container self) :time)
    (if (setting-of (view-container self) :arp) 
        (draw-single-offset-line chord note
                                 0 *MN-draw-offset*
                                 (t-time chord) *MN-C5*)
        (draw-single-offset-line chord note
                                 0 *MN-draw-offset*
                                 (t-time chord) *MN-C5*))))

(defmethod view-mouse-moved ((self C-MN-panel-ChordBox) mouse)
  (declare (ignore mouse))
  (let* ((mouse (view-mouse-position self))
         (mouse-h (point-h mouse))
         (mouse-v (point-v mouse))
         (the-view (view-container self)))
    (setf (active-chord self) (car (chords (chord-line self))))
    (cond ((setting-of (view-container self) :arp)
           (setf (active-note self) (find-arp-active-note self mouse-h mouse-v)))
          ((setting-of (view-container self) :time)
           (setf (active-note self) (find-time-active-note self mouse-h mouse-v)))
          (t (setf (active-note self) (find-active-note self mouse-h mouse-v))))
    (cond ((active-note self) 
           (set-pitch&value self (active-note self)))
          ((and (setting-of the-view :time) (get-ctrl-setting the-view :offs))
           (set-display-value the-view 
                              (with-time-scale self 
                                (- mouse-h (+ *MN-draw-offset* *MN-left-offset*)) )))
          (t (reset-pitch&value self (active-note self))))))

(defgeneric set-pitch&value (self note))
(defmethod set-pitch&value ((self C-MN-panel-ChordBox) note)
  (let ((the-view (view-container self)))
    (cond ((get-ctrl-setting the-view :dur)
           (set-display-value the-view (dur note)))
          ((get-ctrl-setting the-view :dyn)
           (set-display-value the-view (vel note)))
          ((get-ctrl-setting the-view :channel)
           (set-display-value the-view (chan note)))
          ((get-ctrl-setting the-view :offs)
           (set-display-value the-view (offset-time note)))
          ((not (get-ctrl-setting the-view :order))
           (set-display-value the-view (midic note)))
          (t (reset-pitch&value self note)))))

(defgeneric reset-pitch&value (self note))
(defmethod reset-pitch&value ((self C-MN-panel-ChordBox) note)
  (declare (ignore note))
  (set-display-value (view-container self) nil))

(defgeneric find-active-note (self x y))
(defmethod find-active-note ((self C-MN-panel-chordBox) x y)
  (ask (notes (active-chord self)) 
       'inside-note?-3  x
       (calc-chord-pixel-x (active-chord self)
                           0 *MN-draw-offset* 
                           (t-time (active-chord self)))
       (give-y-diatone self y)))

;; (defmethod find-arp-active-note ((self C-MN-panel-chordBox) x y &optional time-offset)
;;   (let ((alt) (dx) (active-tmp) (chords (notes (active-chord self)))  pixel-x)
;;     (dolist (one-note chords)
;;       (setq alt (alt-delta-x one-note) dx (delta-x one-note))
;;       (setf (alt-delta-x one-note) -12  (delta-x one-note) -6)
;;       (setq pixel-x (calc-chord-pixel-x (active-chord self) 0
;;                                         (if time-offset 
;;                                             (+ time-offset (scaled-offset-time-x self one-note))
;;                                             (arp-view-x one-note))
;;                                         (t-time (active-chord self))))
;;       (setq active-tmp 
;;             (or (inside-note?-3  one-note x pixel-x (give-y-diatone self y))
;;                 (and (not time-offset)
;;                      (inside-note?-3  one-note (- x 6) pixel-x (give-y-diatone self y)))))
;;       (setf (alt-delta-x one-note) alt (delta-x one-note) dx)
;;       (if active-tmp (return active-tmp)))))

(defgeneric find-arp-active-note (self x y &optional time-offset))
(defmethod find-arp-active-note ((self C-MN-panel-chordBox) x y &optional time-offset)
  (let ((alt) (dx) (active-tmp) (chords (notes (active-chord self)))  pixel-x)
    (dolist (one-note chords)
      (setq alt (alt-delta-x one-note) dx (delta-x one-note))
      (setf (alt-delta-x one-note) -12  (delta-x one-note) -6)
      (setq pixel-x (calc-chord-pixel-x (active-chord self) 0
                                        (if time-offset 
                                            (+ time-offset (scaled-offset-time-x self one-note))
                                            ;;(arp-view-x one-note))
                                            (truncate (arp-view-x one-note)
                                                      (MN-zoom-scaler (view-container self))))
                                        (t-time (active-chord self))))
      (setq active-tmp 
            (or (inside-note?-3  one-note x pixel-x (give-y-diatone self y))
                (and (not time-offset)
                     (inside-note?-3  one-note (- x 6) pixel-x (give-y-diatone self y)))))
      (setf (alt-delta-x one-note) alt (delta-x one-note) dx)
      (if active-tmp (return active-tmp)))))

(defgeneric find-time-active-note (self x y))
(defmethod find-time-active-note ((self C-MN-panel-chordBox) x y)
  (find-arp-active-note self x y  (+ *MN-draw-offset* *MN-left-offset*)))


;; mouse-pressed 

(defmethod view-click-event-handler ((self C-MN-panel-ChordBox) where)
  (call-next-method)
  (remove-param-editor (view-container self))
  (cond ((active-note self)
         (set-old-drag-value self))
        ((and (option-key-p) (save-to-add-p self))
         (add-new-note-to-mn self where))
        ((and (shift-key-p) (zone-selection? self))
         (extend-selection self 0
                           (- (point-h (view-mouse-position self)) (sel-start self))))
        (t (new-selection self))))

(defmethod view-mouse-up ((self C-MN-panel-ChordBox)) 
  (cond ((active-note self)
         (when (and (selected-notes self) 
                    (or (get-ctrl-setting (view-container self) :order)
                        (and (get-ctrl-setting (view-container self) :offs)
                             (setting-of (view-container self) :time))))
           (setf (selected-notes self) nil)
           (undo-selection self))
         (call-next-method))
        ((and (zone-selection? self) (or (not (selected-notes self)) (shift-key-p)))
         (notes-in-selection self))
        (t ())))

(defgeneric set-old-drag-value (self))
(defmethod set-old-drag-value ((self C-MN-panel-ChordBox))
  (cond ((get-ctrl-setting (view-container self) :dur)
         (setf *old-drag-value* (dur (active-note self))))
        ((get-ctrl-setting (view-container self) :dyn)
         (setf *old-drag-value* (vel (active-note self))))
        ((get-ctrl-setting (view-container self) :channel)
         (setf *old-drag-value* (chan (active-note self))))
        ((get-ctrl-setting (view-container self) :offs)
         (setf *old-drag-value* (offset-time (active-note self))))
        ((get-ctrl-setting (view-container self) :order)
         (setf *old-drag-value* (order (active-note self))))
        (t (setf *old-drag-value* (midic (active-note self))))))

(defgeneric save-to-add-p (self))
(defmethod save-to-add-p ((self C-MN-panel-chordbox))
  (not (or (get-ctrl-setting (view-container self) :dur)
           (get-ctrl-setting (view-container self) :dyn)
           (get-ctrl-setting (view-container self) :offs)
           (get-ctrl-setting (view-container self) :order) )))

(defgeneric add-new-note-to-mn (self mouse))
(defmethod add-new-note-to-mn ((self C-MN-panel-ChordBox) mouse)
  (declare (ignore mouse))
  (let* ((mouse (view-mouse-position self))
         (mouse-h (point-h mouse))
         (mouse-v (point-v mouse))
         (active-note (make-C-note (give-y-value self mouse-v) nil nil 100 100 1 nil 0 0))
         (chord (car (chords (chord-line self))))
         (last-order 0) notes)
    (with-focused-view self
      (add-new-note chord (setf (active-note self) active-note))
      (if (setting-of (view-container self) :arp)
          (setf (order active-note) (+ mouse-h  6))
          (progn 
            (setq notes (notes chord))
            (setf (order active-note)
                  (dolist (note notes last-order) 
                    (if (> (order note) last-order) (setq last-order (order note)))))
            (if (setting-of (view-container self) :time)
                (setf (offset-time active-note)
                      (with-time-scale self 
                        (- mouse-h (+ *MN-draw-offset* *MN-left-offset*))))) )) )
    (setf (selected-notes self) nil)
    (if (zone-selection? self) (undo-selection self))))

;;___________
;; edit note information

(defgeneric edit-note-duration (self x y mouse-diff))
(defmethod edit-note-duration ((self C-MN-panel-ChordBox)  x y mouse-diff)
  (declare (ignore x y))
  (if (active-note self)
      (let* ((active-note (active-note self))
             (the-val (max 0 (+ *old-drag-value*
                                (* (if (shift-key-p) 10 1) mouse-diff))))
             (current-dur (dur active-note))
             (selections (remove active-note (selected-notes self))))
        (set-cursor (setf *default-MN-cursor* 605))
        (setf (dur active-note) the-val)
        (dolist (note selections)
          (setf (dur note) (+ (dur note) (- the-val current-dur))))
        (set-pitch&value self active-note))))

(defgeneric edit-note-velocity (self x y mouse-diff))
(defmethod edit-note-velocity ((self C-MN-panel-ChordBox) x y mouse-diff)
  (declare (ignore x y))
  (if (active-note self)
      (let* ((active-note (active-note self))
             (new-vel (min 127 (max 0 (+ *old-drag-value* mouse-diff))))
             (diff (- new-vel (vel active-note)))
             (selections (remove active-note (selected-notes self))))
        (set-cursor (setf *default-MN-cursor* 605))
        (setf (vel active-note) new-vel)
        (dolist (note selections)
          (setf (vel note) (+ (vel note) diff)))
        (set-pitch&value self active-note))))

(defgeneric edit-note-channel (self x y mouse-diff))
(defmethod edit-note-channel ((self C-MN-panel-ChordBox) x y mouse-diff)
  (declare (ignore x y))
  (if (active-note self)
      (let* ((active-note (active-note self))
             (new-chan (min 16 (max 1 (+ *old-drag-value* mouse-diff))))
             (diff (- new-chan (chan active-note)))
             (selections (remove active-note (selected-notes self))))
        (set-cursor (setf *default-MN-cursor* 605))
        (setf (chan active-note) new-chan)
        (dolist (note selections)
          (setf (chan note) (+ (chan note) diff)))
        (set-pitch&value self (active-note self)))) )

(defgeneric edit-note-offset (self x y mouse-diff))
(defmethod edit-note-offset ((self C-MN-panel-ChordBox) x y mouse-diff)
  (declare (ignore x y))
  (if (active-note self)
      (let* ((the-val (+ *old-drag-value*
                         (* (if (shift-key-p) 10 1) mouse-diff)))
             (active-note (active-note self))
             (diff (- the-val (offset-time active-note)))
             (selections (remove active-note (selected-notes self))))
        (setf (offset-time (active-note self)) the-val)
        (dolist (note selections)
          (setf (offset-time note) (+ (offset-time note) diff)))
        (set-pitch&value self (active-note self)))))

(defgeneric with-time-scale (self dx))
(defmethod with-time-scale ((self C-MN-panel-ChordBox) dx)
  (truncate (* dx (MN-zoom-scaler (view-container self)))))

(defgeneric note-travel (self x y mouse-diff))
(defmethod note-travel ((self C-MN-panel-ChordBox) x y mouse-diff)
  (declare (ignore x y))
  (if (active-note self)
      (let* ((dx (+ *old-drag-value* (with-time-scale self mouse-diff)))
             (active-note (active-note self))
             (diff (- dx (offset-time active-note)))
             (selections (remove active-note (selected-notes self))))
        (set-cursor (setf *default-MN-cursor* 202))
        (setf (offset-time active-note) dx)
        (dolist (note selections)
          (setf (offset-time note) (+ (offset-time note) diff)))
        (set-pitch&value self (active-note self)))))

(defgeneric edit-note-midic (self x y mouse-diff))
(defmethod edit-note-midic ((self C-MN-panel-ChordBox) x y mouse-diff)
  (declare (ignore mouse-diff x))
  (with-focused-view self
    (let ((the-note (active-note self))
          (y-val (give-y-value self y)))
      (if (and the-note (not (= (midic the-note) y-val)))
          (let ((selections (remove the-note (selected-notes self)))
                (diff (- y-val (midic the-note))))
            (setf (midic the-note) y-val)
            (set-pitch&value self the-note)
            (update-note the-note)
            (set-cursor (setf *default-MN-cursor*
                              (cond ((eql (alteration the-note) #\Y) 107)
                                    ((eql (alteration the-note) #\I) 103)
                                    (t 110))))
            (dolist (note selections)
              (setf (midic note) (+ (midic note) diff))
              (update-note note))
            (update-chord (or (active-chord self) (car (chords (chord-line self)))))
            )))))

(defgeneric edit-note-order (self x y mouse-diff))
(defmethod edit-note-order((self C-MN-panel-ChordBox) x y mouse-diff)
  (declare (ignore mouse-diff y))
  (if (active-note self)
      (let* ((one-note (active-note self))
             (new-order (- x (delta-x one-note)))
             (diff (- new-order (order one-note)))
             (selections (remove one-note (selected-notes self))))
        (set-cursor (setf *default-MN-cursor* 202))
        (setf (order one-note) new-order)
        (dolist (note selections)
          (setf (order note) (+ (order note) diff))))))

(defmethod view-mouse-dragged ((self C-MN-panel-ChordBox) mouse)
  (declare (ignore mouse))
  (unless *MN-first-click-mouse*
    (setf *MN-first-click-mouse* (view-mouse-position self)))
  (let* ((mouse (view-mouse-position self))
         (mouse-h (point-h mouse))
         (mouse-v (point-v mouse))
         (mouse-diff (- (point-v *MN-first-click-mouse*)
                        (point-v (view-mouse-position (view-window self)))))
         (m-diff-h (- (point-h (view-mouse-position self))
                      (point-h *MN-first-click-mouse*))))
    (if (active-note self)
        (cond 
          ((get-ctrl-setting (view-container self) :dur)
           (edit-note-duration self mouse-h mouse-v mouse-diff))
          ((get-ctrl-setting (view-container self) :dyn)
           (edit-note-velocity self mouse-h mouse-v mouse-diff))
          ((get-ctrl-setting (view-container self) :channel)
           (edit-note-channel self mouse-h mouse-v mouse-diff))
          ((get-ctrl-setting (view-container self) :offs)
           (if (setting-of (view-container self) :time)
               (note-travel self mouse-h mouse-v m-diff-h)
               (edit-note-offset self mouse-h mouse-v mouse-diff)))
          ((and (get-ctrl-setting (view-container self) :order) 
                (setting-of (view-container self) :arp))
           (edit-note-order self mouse-h mouse-v mouse-diff))
          (t (edit-note-midic self mouse-h mouse-v mouse-diff)))
        (extend-selection self mouse-h m-diff-h)
        )))

(defgeneric extend-selection (self mouse-h m-diff-h))
(defmethod extend-selection ((self C-MN-panel-ChordBox) mouse-h m-diff-h)
  (declare (ignore mouse-h))
  (when (or (setting-of (view-container self) :arp)
            (setting-of (view-container self) :time))
    (if (zone-selection? self) (draw-zone-selection self))
    (setf (sel-end self) (+ (sel-start self) m-diff-h))
    (draw-zone-selection self)))

(defgeneric new-selection (self))
(defmethod new-selection ((self C-MN-panel-ChordBox))
  (when (or (setting-of (view-container self) :arp)
            (setting-of (view-container self) :time))
    (unless *MN-first-click-mouse*
      (setf *MN-first-click-mouse* (view-mouse-position self)))
    (if (zone-selection? self) (undo-selection self))
    (setf (selected-notes self) nil)
    (setf (sel-start self) (point-h *MN-first-click-mouse*))
    (setf (sel-end self) (point-h *MN-first-click-mouse*))))

(defgeneric zone-selection? (self))
(defmethod zone-selection? ((self C-MN-panel-ChordBox)) 
  (not (= (sel-start self) (sel-end self))))

(defgeneric undo-selection (self))
(defmethod undo-selection ((self C-MN-panel-ChordBox))
  (draw-zone-selection self)
  (setf (sel-start self) 0 (sel-end self) 0))

(defgeneric draw-zone-selection (self))
(defmethod draw-zone-selection ((self C-MN-panel-ChordBox))
  (with-focused-view self
    (with-pen-state (:mode :patxor)
      (let* ((x (min (sel-start self) (sel-end self)))
             (w (- (max (sel-end self) (sel-start self)) x)))
        (fill-rect* x (+ 2 (point-v (view-scroll-position self))) w (- (point-v (view-size self)) 4))))))

(defgeneric notes-in-selection (self))
(defmethod notes-in-selection ((self C-MN-panel-ChordBox))
  (let* ((pLeft (sel-start self)) (pRight (sel-end self))
         (chord (car (chords (chord-line self))))
         (notes (notes chord))
         selection time-offset)
    (if (>  pLeft  pRight) (psetq pLeft pRight pRight pLeft))
    (if (setting-of (view-container self) :time)
        (setq time-offset (+ *MN-draw-offset* *MN-left-offset*)))
    (dolist (note notes)
      (if (within-rectangle? self pLeft pRight chord note time-offset) (push note selection) ))
    (setf (selected-notes self) (nreverse selection))))

(defgeneric within-rectangle? (self left right chord one-note &optional time-offset))
(defmethod within-rectangle? ((self C-MN-panel-ChordBox) left right chord one-note &optional time-offset)
  (let ((pix-x (calc-chord-pixel-x chord 
                                   0
                                   (if time-offset 
                                       (+ time-offset (scaled-offset-time-x self one-note))
                                       (arp-view-x one-note))
                                   (t-time chord))))
    (and (>= pix-x left) (<= pix-x right))))

(defmethod remove-chord-or-note ((self C-MN-panel-ChordBox))
  (let* ((selections (selected-notes self))
         (length-sel (length selections))
         (chord (car (chords (chord-line self))))
         (length-ch (length (notes chord))))
    (cond  ((and (selected-notes self) (> length-ch length-sel))
            (dolist (note selections)
              (remove-note  chord note))
            (setf (active-note self) ())
            (erase+view-draw-contents self))
           ((and (active-note self) (> (length (notes chord)) 1))
            (remove-note  chord (active-note self)) 
            (setf (active-note self) ())
            (erase+view-draw-contents self))
           ((ui:ed-beep)))))
;;___________
;;key-pressed

(defmethod handle-key-event ((self C-MN-panel-ChordBox) char)
  (declare (special *global-music-notation-panel*))
  (let ((active-note (active-note self)))
    (cond ((case char
             (#\a (change-arp-view (view-container self) 'arp) t)
             (#\t (change-arp-view (view-container self) 'time) t)
             (#\n (change-arp-view (view-container self) 'chord) t)
             ((:Tab) (circ-edit-dim self) t)))
          ((and (or active-note (selected-notes self))
                (or (eql char :UpArrow)
                    (eql char :DownArrow)))
           (transpose-selections self char)
           (if active-note (set-pitch&value self active-note))
           (update-chord (active-chord self))
           (erase+view-draw-contents self))
          ((and (or active-note (selected-notes self)) (eql char #\Backspace))
           (remove-chord-or-note self))
          ((eql char #\r) (when active-note 
                           (setf *global-music-notation-panel* self)
                           (remove-instrument-item active-note 0 0)))
          ((eql char #\B) (when active-note 
                           (setf *global-music-notation-panel* self)
                           ;;(add-bpf-to-note (active-note self) 0 0)
                           ))
          ((eql char #\F) (when active-note 
                           (setf *global-music-notation-panel* self)
                           ;;(add-fix-to-note (active-note self) 0 0)
                           ))
          ((eql char #\M) (when active-note 
                           (setf *global-music-notation-panel* self)
                           (add-MN-to-note active-note (view-window self) 0 0)))
          ((and (eql char #\e) (or active-note (selected-notes self)))
           (open-param-ctrl (view-container self)))
          ((eql char #\S) (reset-order self))
          (t (ui:ed-beep)))))

(defgeneric reset-order (self))
(defmethod reset-order ((self C-MN-panel-ChordBox))
  (let ((notes (notes (car (chords (chord-line self))))))
    (dolist (note notes) (setf (order note) 0))
    (if (setting-of (view-container self) :arp) (erase+view-draw-contents self))))

(defgeneric transpose-selections (self char))
(defmethod transpose-selections ((self C-MN-panel-ChordBox) char)
  (let ((selections (selected-notes self)))
    (if selections
        (dolist (note selections)
          (transpose-note note 
                          (* (cond ((shift-key-p) 2)
                                   ((control-key-p) 24)
                                   (t 1))
                             (if (eql char :UpArrow) 
                                 (approx-factor *current-approx-scale*)
                                 (- 0 (approx-factor *current-approx-scale*))))))
        (transpose-note (active-note self)
                        (* (cond ((shift-key-p) 2)
                                 ((control-key-p) 24)
                                 (t 1))
                           (if (eql char :UpArrow) 
                               (approx-factor *current-approx-scale*)
                               (- 0 (approx-factor *current-approx-scale*))))))))

(defmethod mn-copy ((self C-MN-panel-ChordBox))
  (when (selected-notes self)
    (menu-item-disable *undo-MN-menu*)
    (setf *MN-editor-scrap*
          `(list ,@(ask-all (selected-notes self) 'decompile)))))
        ;;`(list ,(decompile (car (chords (chord-line self)))))

(defmethod mn-cut ((self C-MN-panel-ChordBox))
  (when (selected-notes self)
    (save-undo self)
    (mn-copy self)
    (menu-item-enable *undo-MN-menu*)
    (remove-chord-or-note self)
    (setf (selected-notes self) nil)
    (undo-selection self)))

(defmethod mn-paste ((self C-MN-panel-ChordBox))
  (save-undo self)
  (if *MN-editor-scrap*
      (let ((new-notes (eval *MN-editor-scrap*))
            (chord (car (chords (chord-line self)))))
        (unless (list-of-notes new-notes)
          (setq new-notes (notes (car new-notes))))
        (cond ((setting-of (view-container self) :time)
               (if (selected-notes self)
                   (let* ((selection (sort (selected-notes self) '< :key #'offset-time))
                          (to (offset-time (car (last selection)))))
                     (dolist (note new-notes) 
                       (setf (offset-time note)
                             (if selection (offset-time (pop selection)) to))
                       (add-new-note chord note))
                     (remove-chord-or-note self)
                     (setf (selected-notes self) nil)
                     (undo-selection self))
                   (let* ((to (with-time-scale self 
                                (- (point-h (view-mouse-position self))
                                   (+ *MN-draw-offset* *MN-left-offset*)) )))
                     (dolist (note new-notes) 
                       (setf (offset-time note) to)
                       (add-new-note chord note))
                     (update-view-controler (view-container self)))))
              ((setting-of (view-container self) :arp)
               (if (selected-notes self)
                   (let* ((selection (selected-notes self))
                          (to (order (car (last selection))))
                          (notes (notes chord))
                          (all-higher (remove to notes :test #'>= :key #'order)))
                     (dolist (note new-notes) 
                       (setf (order note) (incf to))
                       (add-new-note chord note))
                     (dolist (note all-higher) (setf (order note) (incf to)))
                     (remove-chord-or-note self)
                     (setf (selected-notes self) nil)
                     (undo-selection self))
                   (let* ((notes (notes chord))
                          (to (point-h (view-mouse-position self)))
                          (all-higher 
                            (remove to notes :test #'>= 
                                             :key (lambda (note) 
                                                    (calc-chord-pixel-x chord 0 (arp-view-x note) (t-time chord))))))
                     (if all-higher
                         (setq to (- (order (car all-higher)) 2)))
                     (dolist (note new-notes) 
                       (setf (order note) (incf to))
                       (add-new-note chord note))
                     (dolist (note all-higher) (setf (order note) (incf to)))
                     (update-view-controler (view-container self)))))
              (t (dolist (note new-notes)
                   (add-new-note chord note))
                 (update-view-controler (view-container self))))    )))

(defgeneric save-undo (self))
(defmethod save-undo ((self C-MN-panel-ChordBox))
  (menu-item-enable *undo-MN-menu*)
  (setf (undo-MN-edit self) 
        (notes (car (chords (chord-line self))))))

(defgeneric restore-MN-edition (self))
(defmethod restore-MN-edition ((self C-MN-panel-ChordBox))
  (setf (notes (car (chords (chord-line self)))) (undo-MN-edit self))
  (update-view-controler (view-container self)))

(defgeneric circ-edit-dim (self))
(defmethod circ-edit-dim ((self C-MN-panel-ChordBox))
  (let* ((ctrls (external-controls (view-container self)))
         (radio-b (list (fourth ctrls) (fifth ctrls) (sixth ctrls)))
         (radio-names (list :dur :dyn :offs)))
    (if (no-button-pushed-p self radio-b)
        (progn (set-value-ctrl (view-container self) (car radio-b) (car radio-names))
               (radio-button-push (car radio-b)))
        (do ((item radio-b (cdr item)) 
             (names radio-names (cdr names))) ((null item) (car radio-b))
          (when (radio-button-pushed-p (car item))
            (set-value-ctrl (view-container self) (car item) (car names))
            (if (cdr item)
                (progn (set-value-ctrl (view-container self) (cadr item) (cadr names))
                       (radio-button-push (cadr item))
                       (return nil))))))
    (update-view-controler (view-container self))))

(defgeneric no-button-pushed-p (self button-list))
(defmethod no-button-pushed-p ((self C-MN-panel-ChordBox) button-list)
  (dolist (button button-list t)
    (if (radio-button-pushed-p button)
        (return nil))))


;;=========================================================

(defclass C-chord-mus-not-view (C-mus-not-view) 
  ((arp-ctrl :initform nil  :accessor arp-ctrl)
   (time-ctrl :initform nil :accessor time-ctrl)
   (param-ctrl :initform nil :accessor param-ctrl)
   (popUpbox :initform nil :accessor popUpbox)))


(defun make-chord-ed-pops ()
  (new-menu 
   " "
   (new-menu "Chord view" 
             (new-leafmenu "arpeggio" 
                           (lambda () (change-arp-view *target-action-object* 'arp)))
             (new-leafmenu "chord" 
                           (lambda () (change-arp-view *target-action-object* 'chord)))
             (new-leafmenu "time offset"
                           (lambda () (change-arp-view *target-action-object* 'time))))
   (new-menu "Approximation"
             (prog1 (setf a-leaf-menu
                          (new-leafmenu "SemiTone" 
                                        (lambda () (use-all-approx-scale
                                                    *target-action-object* *c-major-scale*))))
               (set-command-key a-leaf-menu #\2))
             (prog1 (setf a-leaf-menu
                          (new-leafmenu "Quarter tone" 
                                        (lambda () (use-all-approx-scale
                                                    *target-action-object* *1/4-tone-chromatic-scale*))))
               (set-command-key a-leaf-menu #\4))
             (prog1 (setf a-leaf-menu
                          (new-leafmenu "Eigth tone" 
                                        (lambda () (use-all-approx-scale
                                                    *target-action-object* *1/8-tone-chromatic-scale*))))
               (set-command-key a-leaf-menu #\8)))
   (new-menu "Scale"
             (new-leafmenu "C-major" 
                           (lambda () (use-all-scale
                                       *target-action-object* *c-major-scale*)))
             (new-leafmenu "Chromatic" 
                           (lambda () (use-all-scale
                                       *target-action-object* *chromatic-scale*))))
   (new-menu "Staff"
             (new-leafmenu "G2-G"
                           (lambda () (use-staff  *target-action-object* 1 *g2-g-staffs*)))
             (new-leafmenu "G"
                           (lambda () (use-staff  *target-action-object* 2 *g-plain-staffs*)))
             (new-leafmenu "G F" 
                           (lambda () (use-staff  *target-action-object* 3 *g-f-staffs*)))
             (new-leafmenu "F" 
                           (lambda () (use-staff  *target-action-object* 4 *f-plain-staffs*)))
             (new-leafmenu "G F F2" 
                           (lambda () (use-staff  *target-action-object* 5 *g-f-f2-staffs*)))
             (new-leafmenu "G2 G F F2" 
                           (lambda () (use-staff  *target-action-object* 6 *g2-g-f-f2-staffs*)))
             (new-leafmenu "Empty" 
                           (lambda () (use-staff  *target-action-object* 7 *empty-staffs*))))))

(defvar *MN-popUpMenu* nil)

(defmethod make-extra-controls :after ((self C-chord-mus-not-view))
  (let* ((x-pos (+ 86 (point-h (view-position self))))
         (y-pos (+  (point-v (view-position self)) (point-v (view-size self)) -35))
         (ctrls
           (list (add-to-radio-cluster self x-pos y-pos "order" :order)
                 (add-to-radio-cluster self (+ x-pos 48) y-pos "channel" :channel)
                 (make-instance 
                  'static-text-dialog-item
                  :view-container (view-window self)
                  :view-position 
                  (make-point (+ x-pos 44) (+ y-pos 16))
                  :dialog-item-text " "
                  :view-font *patchwork-font-spec*
                  :view-size (make-point 50 12))
                 (setf (popUpbox self)
                       (make-popUpbox "v" self (or *MN-popUpMenu*
                                                   (setf *MN-popUpMenu* (make-chord-ed-pops)))
                                      :view-position (make-point (+ x-pos 110) (+ y-pos 18))
                                      :view-container (view-window self)))) ))
    (setf (external-controls self) (append (external-controls self) ctrls))
    (setf (ctrl-settings self) (append (ctrl-settings self) (list (list :order) (list :channel))))
    (setf (param-ctrl self) (make-param-controller self))  ))

(defgeneric setting-of (self ctrl))
(defmethod setting-of ((self C-chord-mus-not-view) ctrl)
  (if (eql ctrl :arp)
      (arp-ctrl self)
      (time-ctrl self)))

(defmethod set-display-value ((self C-chord-mus-not-view) value)
  (unless (member (param-ctrl self) (subviews self))
    (let ((my-controls (external-controls self)))
      (set-dialog-item-text (nth (- (length my-controls) 2) my-controls)
                            (if value (format () "~S" value) " ")))))

(defclass C-par-ctrl (editable-text-dialog-item)
  ((a-note :initarg nil :accessor a-note)))

(defgeneric exit-from-param-ctrl (self))
(defmethod exit-from-param-ctrl ((self C-par-ctrl))
  (let* ((edit-view (view-container self))
         (panel (car (editor-objects edit-view)))
         (selections (selected-notes panel))
         (value (read-from-string (dialog-item-text self)))
         param)
    (when (numberp value)
      (setq param 
            (cond ((and (get-ctrl-setting edit-view :dur) (>= value 0)) 'dur)
                  ((and (get-ctrl-setting edit-view :dyn) (>= value 0) (< value 128)) 'vel)
                  ((and (get-ctrl-setting edit-view :channel) (>= value 0) (< value 33)) 'chan)
                  ((get-ctrl-setting edit-view :offs) 'offset)
                  ((and (not (get-ctrl-setting edit-view :order))
                        (> value 0) (< value 12800)) 'midic)))
      (when  param
        (cond (selections
               (dolist (note selections) 
                 (setf (slot-value note param) value)
                 (update-note note))
               (update-chord (car (chords (chord-line panel)))))
              ((a-note self)
               (setf (slot-value (a-note self) param) value)
               (update-note (a-note self))
               (update-chord (car (chords (chord-line panel))))
               (setf (a-note self) nil)))
        (erase+view-draw-contents panel))
      (undo-selection panel)
      (remove-subviews edit-view self))))

(defgeneric remove-param-editor (self))
(defmethod remove-param-editor ((self C-chord-mus-not-view))
  (if (member (param-ctrl self) (subviews self))
      (remove-subviews self (param-ctrl self))))

(defgeneric make-param-controller (self))
(defmethod make-param-controller ((self C-chord-mus-not-view ))
  (make-instance 'C-par-ctrl :view-font *patchwork-font-spec*
                             :view-size (make-point 50 12)
                             :view-position (make-point 130 (- (point-v (view-size self)) 19))))

(defgeneric open-param-ctrl (self))
(defmethod open-param-ctrl ((self C-chord-mus-not-view))
  (let ((ctrl (param-ctrl self))
        (panel (car (editor-objects self))))
    (setf (a-note ctrl) (active-note (car (editor-objects self))))
    (set-view-position ctrl (make-point 130 (- (point-v (view-size self)) 19)))
    (set-dialog-item-text ctrl
                          (if (a-note ctrl) 
                              (set-pitch&value panel (a-note ctrl))
                              (set-pitch&value panel (car (selected-notes panel)))))
    (add-subviews self ctrl)))

(defgeneric change-arp-view (self view-type))
(defmethod change-arp-view ((self C-chord-mus-not-view ) view-type)
  (setf (arp-ctrl self) ())
  (setf (time-ctrl self) ())
  (cond ((eql view-type 'arp)
         (setf (arp-ctrl self) t)
         (set-box-title (popUpBox self) "A"))
        ((eql view-type 'time)
         (setf (time-ctrl self) t)
         (set-box-title (popUpBox self) "T"))
        (t  (set-box-title (popUpBox self) "V"))) ;for now.......
  (update-view-controler self))

(defmethod set-value-ctrl ((self C-chord-mus-not-view) item kind)
  (declare (ignore item))
  (call-next-method)
  (if (and (eql :order kind) (not (arp-ctrl self)) (get-ctrl-setting self :order))
      (change-arp-view self 'arp)))

(defmethod use-staff ((self C-chord-mus-not-view ) num staff)
  (let ((panel (car (editor-objects self))))
    (setf (staff-list panel) staff)
    (setf (staff-num panel) num)
    (update-view-controler self)))

(defmethod update-all-chord-lines ((self C-chord-mus-not-view ) chord-lines)
  (let ((editors (editor-objects self)))
    (while (and chord-lines editors)
      (setf (chord-line (pop editors)) (pop chord-lines)))))

;;aaa from pw-modifs le 10-9-95

(defmethod play-all-staffs ((self C-chord-mus-not-view))
  (cond ((setting-of self :time)
         (play-your-chords (chord-line (car (editor-objects self)))))
        ((setting-of self :arp)
         (play-arpeggiated self))
        ((get-ctrl-setting self :dur) 
         (let ((panels (editor-objects self)))
           (progn (setf patchwork.scheduler::*print-on-late?* t)
                  (start
                    (apdfuncall 10  2 20 
                                'play-chords (chord-line (car panels)) 0)))))
        (t (play-your-chords (chord-line (car (editor-objects self)))))))

;; (defmethod play-all-staffs ((self C-chord-mus-not-view))
;;   (cond ((setting-of self :time)
;;          (play-your-chords (chord-line (car (editor-objects self)))))
;;         ((setting-of self :arp)
;;          (play-arpeggiated self))
;;         (t (let ((panels (editor-objects self)))
;;              (progn (setf patchwork.scheduler::*print-on-late?* t)
;;                     (start
;;                       (apdfuncall 10  2 20 
;;                                   'play-chords (chord-line (car panels)) 0)))))))
;;         ;; (t (call-next-method))

(defgeneric play-arpeggiated (self))
(defmethod play-arpeggiated ((self C-chord-mus-not-view))
  (let* ((chord (car (chords (chord-line (car (editor-objects self))))))
         (pitch-notes (copy-list (notes chord)))
         (notes (sort pitch-notes #'< :key #'order)))
    (setf *MN-play-flag* t)
    (cond ((eql *playing-option* :mc)
           (setf patchwork.scheduler::*print-on-late?* t)
           (start (apdfuncall 10 (priority) 10
                              'keep-playing-arps-mc self notes)))
          ((eql *playing-option* :pb)
           (setf patchwork.scheduler::*print-on-late?* t)
           (start (apdfuncall 10 (priority) 10
                              'keep-playing-arps self notes))))))

(defgeneric keep-playing-arps (self notes))
(defmethod keep-playing-arps ((self C-chord-mus-not-view) notes)
  (when *MN-play-flag*
    (play-note (pop notes))
    (if notes
        (re-dfuncall 30
                     self notes))))

(defgeneric keep-playing-arps-mc (self notes))
(defmethod keep-playing-arps-mc ((self C-chord-mus-not-view) notes)
  (when *MN-play-flag*
    (let ((approx-m (approx-for-playing (midic (car notes)))))
      (write-midi-note (dur (car notes)) (micro-channel approx-m)
                       (truncate approx-m 100) (vel (pop notes))))
    (if notes
        (re-dfuncall 30
                     self notes))))

;;______________________________________
;; 
(defmethod monofonic-mn? ((self C-chord-mus-not-view)) 
  t)


;;just for cleanness: this method is missing

(defmethod monofonic-mn? ((self C-mus-not-view))
  (not (and (pw-object (view-container self)) (polifonic? (pw-object (view-container self))))))


;;;; THE END ;;;;
