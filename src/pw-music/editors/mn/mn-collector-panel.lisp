;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               mn-collector-panel.lisp
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

;;;=======================================================
;;;
;;; The class Music Notation Editors, both mono and Poly.
;;;
;;; Class name: C-MN-panel-mod
;;; inherits from: C-music-notation-panel
;;; methods: 
;;;   update-editor:                 ;forces redesigning of the editing window
;;;   draw-rest-control:             ;redesigns the chords, according to view, selections, etc.
;;;   draw-with-offset-view          ;draws notes taking account of offset time
;;;   draw-ledger-lines-arp          ;draws ledger lines for offset view
;;;   draw-offset-chord              ;draws a chord with time offsets
;;;   continue-mouse-moved           ;changes cursor according to mouse position in editor
;;;   find-active-note               ;sees if mouse on a note (no offset view)
;;;   find-time-active-note          ;sees if mouse on a note (offset view)
;;;   hilite-chord                   ;highlites the stem (or the notes) for a selected chord
;;;   unhilite-chord
;;;   mouse-pressed-no-active        ;selects, un-selects, opens a chord, according to mouse pos'n
;;;   select-chord
;;;   unselect-chords
;;;   add-to-selection
;;;   open-chord-win                 ;opens the chord editor in response to a double-click on a chord
;;;   add-new-chord-to-MN            ;adds the output of the chord editor to the chord line
;;;   mouse-dragged-control-rest     ;sees if the mouse is dragged on a selected chord's stem
;;;   drag-selection-in-time         ;drags a chord or group of chords in time
;;;   key-pressed                    ;CUT, COPY, PASTE, Open note?
;;;   Cut, copy, paste               ;the standard stuff for selected chords
;;;   update-cursor-state            ;changes cursor when out of editor bounds
;;;========================================================


(defvar *current-editing-chord* ())

(defclass C-MN-panel-Mod (C-music-notation-panel)
  ((selected-chords :initform nil :accessor selected-chords)
   (drag-function :initform nil :allocation :class :accessor drag-function)
   (undo-MN-edit :initform nil :accessor undo-MN-edit)))

;;A horrible kludge so that inter-class editing works (because CLOS allocates in the
;;class of the instance rather than in the class where the slot is defined)
(defgeneric editor-scrap (self)
  (:method ((self C-music-notation-panel))
    (declare (special *MN-editor-scrap*))
    *MN-editor-scrap*))

(defgeneric (setf editor-scrap) (thing self)
  (:method (thing (self C-music-notation-panel))
    (declare (special *MN-editor-scrap*))
    (setf *MN-editor-scrap* thing)))

(defmethod update-editor ((self C-MN-panel-Mod))
  (let ((selections (selected-chords self)))
    (setf (selected-chords self) nil)
    (dolist (chord  selections) (unhilite-if-not-selected self chord 0 0))          
    (erase+view-draw-contents self)
    (dolist (chord  selections) (hilite-if-not-selected self chord 0 0))
    (setf (selected-chords self) selections)
    ))

(defmethod view-draw-contents ((self C-MN-panel-Mod))
  (if (selected-chords self) 
      (update-editor self)
      (call-next-method)))

(defvar *mn-view-offtime-flag* ())
(defmethod view-draw-specific  ((self C-MN-panel-Mod)
                                zoom-scale scroll-pos MN-offset MN-C5)
  (declare (ignore scroll-pos))
  (let ((*mn-view-offset-flag* )
        (*mn-view-offtime-flag* (get-ctrl-setting (view-container self) :offs))
        (*mn-view-channel-flag* (get-ctrl-setting (view-container self) :channel)))
    (declare (special *mn-view-offset-flag* *mn-view-offtime-flag*))
    (when (chord-line self)
      (set-visible-chords self) 
      (if *mn-view-offtime-flag*
          (draw-with-offset-view self (visible-chords self) zoom-scale MN-offset MN-C5)
          (progn 
            (tell (visible-chords self)  'draw-chord 
                  zoom-scale MN-offset (scaled-origin self) MN-C5)
            (when *mn-view-channel-flag*
              (draw-all-channels self (visible-chords self) MN-offset
                                 zoom-scale MN-C5)))
          ))))

(defmethod draw-all-channels ((self C-MN-panel-Mod) chords MN-offset zoom-scale MN-C5)
  (let ((org (scaled-origin self)))
    (dolist (chord chords)
      (dolist (note (notes chord))
        (draw-note-channel note 
                           (+ 10 (calc-chord-pixel-x chord zoom-scale MN-offset org))
                           (give-pixel-y note MN-C5))))))

(defgeneric draw-with-offset-view (self the-chords zoom-scale MN-offset MN-C5))
(defmethod draw-with-offset-view ((self C-MN-panel-Mod) the-chords
                                  zoom-scale MN-offset MN-C5)
  (mapc (lambda (chord) (draw-chord-with-offs self chord zoom-scale MN-offset MN-C5))
        the-chords))

(defgeneric draw-chord-with-offs (self chord zoom-scale MN-offset MN-C5 &optional mode))
(defmethod draw-chord-with-offs  ((self C-MN-panel-Mod) 
                                  chord zoom-scale MN-offset MN-C5 &optional mode)
  (declare (ignore mode))
  (let ((notes (notes chord))
        (y-min) (alt) (dx))
    (dolist (one-note notes)
      (setq y-min (1- (give-pixel-y one-note MN-C5)))
      (draw-ledger-lines-arp self one-note 
                             (calc-chord-pixel-x chord zoom-scale  MN-offset
                                                 (- (scaled-origin self) (offset-time one-note)))
                             y-min y-min MN-C5)
      (setq alt (alt-delta-x one-note) dx (delta-x one-note))
      (setf (alt-delta-x one-note) -12  (delta-x one-note) -6)
      (draw-note-4 one-note
                   (calc-chord-pixel-x chord zoom-scale MN-offset
                                       (- (scaled-origin self) (offset-time one-note)))
                   MN-C5 zoom-scale)
      (setf (alt-delta-x one-note) alt (delta-x one-note) dx))
    (draw-stem chord (calc-chord-pixel-x chord zoom-scale MN-offset (scaled-origin self))
               MN-C5)
    (draw-extra-info chord 
                     (calc-chord-pixel-x chord zoom-scale  MN-offset
                                         (- (scaled-origin self))) MN-C5 nil)))

(defmethod view-mouse-moved ((self C-MN-panel-Mod) mouse)
  (declare (ignore mouse))
  (let* ((mouse (view-mouse-position self))
         (x (point-h mouse))
         (y (point-v mouse)))
    (setf (active-chord self) 
          (find-mouse-point-in-chords self (- x *MN-draw-offset*)))
    (if (not (active-chord self))
        (setf (active-note self) nil)
        (setf (active-note self) 
              (if *mn-view-offtime-flag*
                  (find-time-active-note self x y)
                  (find-active-note self x y))))
    (set-display-value (view-container self) 
                       (truncate 
                        (scaled-mouse-h self 
                                        (+ (origin self) (- x *MN-draw-offset*)))))))

(defmethod find-active-note ((self C-MN-panel-Mod) x y)
  (ask (notes (active-chord self)) 
       'inside-note?-3  x
       (calc-chord-pixel-x (active-chord self) (MN-zoom-scaler (view-container  self))
                           *MN-draw-offset* (scaled-origin self))
       (give-y-diatone self y)))

(defmethod find-time-active-note ((self C-MN-panel-Mod) x y)
  (let ((active-tmp))
    (dolist (one-note (notes (active-chord self)))
      (setq active-tmp 
            (inside-note?-3
             one-note x
             (calc-chord-pixel-x (active-chord self)
                                 (MN-zoom-scaler (view-container  self))
                                 (+ *MN-draw-offset*
                                    (- (offset-time one-note) (delta-x one-note) 6))
                                 (scaled-origin self))
             (give-y-diatone self y)))
      (if active-tmp (return active-tmp)))))

(defgeneric unhilite-if-not-selected (self chord x y))
(defmethod unhilite-if-not-selected ((self C-MN-panel-Mod) chord x y)
  (declare (ignore x y))
  (if (and chord (not (member chord (selected-chords self))))
      (unhilite-chord chord self (MN-zoom-scaler (view-container  self)) 
                      *MN-draw-offset* (scaled-origin self) *MN-C5*)))

(defgeneric hilite-if-not-selected (self chord x y))
(defmethod hilite-if-not-selected ((self C-MN-panel-Mod) chord x y)
  (declare (ignore x y))
  (if (and chord (not (member chord (selected-chords self))))
      (hilite-chord chord self (MN-zoom-scaler (view-container  self))
                    *MN-draw-offset* (scaled-origin self) *MN-C5*)))

(defgeneric hilite-chord (self view t-scfactor beg-x time1 C5))
(defmethod hilite-chord ((self C-chord) view t-scfactor beg-x time1 C5)
  (let ((x-now (calc-chord-pixel-x self t-scfactor beg-x time1)))
    (if (get-ctrl-setting (view-container view) :offs)
        (draw-offset-hilite-chord self view t-scfactor beg-x time1 C5))
    (hilite-rect self view x-now C5 )))

(defgeneric unhilite-chord (self win t-scfactor beg-x time1 C5))
(defmethod unhilite-chord ((self C-chord) win t-scfactor beg-x time1 C5)
  (hilite-chord self win t-scfactor beg-x time1 C5))

(defgeneric draw-offset-hilite-chord (self view t-scfactor beg-x time1 C5))
(defmethod draw-offset-hilite-chord ((self C-chord) view t-scfactor beg-x time1 C5)
  (let ( (notes (notes self))
         (y-min) (alt) (dx))
    (with-focused-view view
      (set-view-font   (view-container (view-container view)) *music-font-spec*); TODO: PATXOR -> INSTANCE
      (dolist (one-note notes)
        (setq y-min (1- (give-pixel-y one-note C5)))
        (setq alt (alt-delta-x one-note) dx (delta-x one-note))
        (setf (alt-delta-x one-note) -12  (delta-x one-note) -6)
        (hilite-note one-note
                     (calc-chord-pixel-x self t-scfactor beg-x
                                         (- time1 (offset-time one-note)))
                     C5)
        (setf (alt-delta-x one-note) alt (delta-x one-note) dx)))))

(defgeneric hilite-note (self x C5))
(defmethod hilite-note ((self C-note) x C5)
  (let ((y-now (give-pixel-y self C5))
        (x-now (+ x (delta-x self))))
    (draw-char x-now y-now #\w)
    (draw-char x-now y-now #\266)))

(defgeneric hilite-rect (self view x C5))
(defmethod hilite-rect ((self C-chord) view x C5 )
  (let ((y-min (1- (give-pixel-y (car (notes self)) C5)))
        (y-max (give-pixel-y (car (last (notes self))) C5)))
    (with-focused-view view
      (with-pen-state (:pattern *black-pattern* :mode :patXor)
        (fill-rect*  x (- y-max 18) 3 (- y-min y-max -18 ))))))


(defgeneric inside-hilit-rect (self x y))
(defmethod inside-hilit-rect ((self C-MN-panel-Mod) x y)
  (let* ((chord (active-chord self))
         (x-now (calc-chord-pixel-x chord (MN-zoom-scaler (view-container  self))
                                    *MN-draw-offset* (scaled-origin self)))
         (y-min (1- (give-pixel-y (car (notes chord)) *MN-C5*)))
         (y-max (give-pixel-y (car (last (notes chord))) *MN-C5*)))
    (inside-rectangle?  x y (- x-now 2) (- y-max 18) 4 (- y-min y-max -18 ))))

(defmethod view-click-event-handler ((self C-MN-panel-Mod) where)
  (declare (ignore where))
  (call-next-method)
  (let ((x (point-h (view-mouse-position self)))
        (y (point-v (view-mouse-position self))))
    (cond 
      ((double-click-p)
       (if (active-chord self)
           (open-selected-chord self x y)
           (add-new-chord-to-mn self x y)))
      ((and (active-note self) (get-ctrl-setting (view-container self) :dur))
       (setf *old-MN-dur* (dur (active-note self))))
      ((and (active-chord self) (or (active-note self) (inside-hilit-rect self x y))
            (in-selection self (active-chord self)))
       (setf *old-drag-value* (t-time (active-chord self)))
       (begin-dragged-mn-control self x y))
      ((active-chord self)
       (select-chord self x y))
      (t (unselect-all-chords self 0 0)))))

(defgeneric in-selection (self active))
(defmethod in-selection ((self C-MN-panel-Mod) active)
  (member active (selected-chords self)))

(defgeneric open-selected-chord (self x y))
(defmethod open-selected-chord ((self C-MN-panel-Mod) x y)
  (unselect-all-chords self x y)
  (setf *current-editing-chord* (active-chord self))
  (open-chord-win self x y))

(defgeneric select-chord (self x y))
(defmethod select-chord ((self C-MN-panel-Mod) x y)
  (if (selected-already-p self (active-chord self))
      (unselect-chords self (active-chord self) x y)
      (add-to-selection self (active-chord self) x y)))

(defgeneric selected-already-p (self chord))
(defmethod selected-already-p ((self C-MN-panel-Mod) chord)
  (member chord (selected-chords self)))

(defgeneric unselect-all-chords (self x y))
(defmethod unselect-all-chords ((self C-MN-panel-Mod) x y)
  (let ((ed-objs (editor-objects (view-container self)))
        (selections))
    (dolist (panel ed-objs)
      (setq selections (selected-chords panel))
      (setf (selected-chords panel) ())
      (dolist (sel-chord selections)
        (unhilite-if-not-selected panel sel-chord x y)))))

(defgeneric unselect-chords (self chord x y))
(defmethod unselect-chords ((self C-MN-panel-Mod) chord x y)
  (cond ((shift-key-p)
         (setf (selected-chords self) (remove chord (selected-chords self)))
         (unhilite-if-not-selected self chord x y))
        (t (unselect-all-chords self x y))))

(defgeneric add-to-selection (self chord x y))
(defmethod add-to-selection ((self C-MN-panel-Mod) chord x y)
  (unless (shift-key-p)
    (unselect-all-chords self x y))
  (hilite-if-not-selected self chord x y)
  (setf (selected-chords self)
        (insert-in-time self (selected-chords self) chord)))

(defmethod update-all-selections ((self C-MN-panel-mod))
  (let ((selection (selected-chords self)))
    (setf (selected-chords self) nil)
    (dolist (chord selection)
      (hilite-if-not-selected self chord 0 0))
    (setf (selected-chords self) selection)))

(defgeneric insert-in-time (self the-list chord))
(defmethod insert-in-time ((self C-MN-panel-Mod) the-list chord)
  (merge 'list the-list (list chord) #'< :key #'t-time))

(defclass C-chord-boxMN-window (C-mn-window) ())

(defmethod view-key-event-handler ((self C-chord-boxMN-window) char)
  (let* ((editor (car (subviews self)))
         (ctrl (param-ctrl editor)))
    (if (member ctrl (subviews editor))
        (if (eql char #\Newline) 
            (exit-from-param-ctrl ctrl)
            (view-key-event-handler ctrl char))
        (call-next-method))))

(defmethod view-deactivate-event-handler :after ((self C-chord-boxMN-window))
  (menu-item-disable *undo-MN-menu*)
  (if (pw-object self)
      (update-editor (pw-object self))))

(defun make-MN-editor-chordMN (staff)
  (make-music-notation-editor 'C-chord-boxMN-window
                              'C-chord-mus-not-view 'C-MN-panel-ChordBox
                              (make-point 230 200) staff))

(defvar *current-chord-ed* 0)

(defgeneric open-chord-win (self x y))
(defmethod open-chord-win ((self C-MN-panel-Mod) x y)
  (declare (special *active-MN-window* *the-chord-editors*))
  (let ((the-chord-ed (get-chord-window *current-editing-chord*)))
    (let((middle-x (+ (point-h (view-position *active-MN-window*))
                      (- x (truncate (point-h (view-size the-chord-ed)) 2)
                         (point-h (view-scroll-position self)))))
         (middle-y  (+ (point-v (view-position *active-MN-window*))
                       (point-v (view-position self))
                       (- y (truncate (point-v (view-size the-chord-ed)) 2)
                          (point-v (view-scroll-position self))))))
      (open-chord-ed the-chord-ed 
                     *active-MN-window* self *current-editing-chord*  middle-x middle-y))))

(defun get-chord-window (chord)
  (let ((dead-win-index ())
        (win-index 0))
    (or (dolist (win *the-chord-editors*)
          (if (window-killed-p win)
              (setq dead-win-index win-index)
              (if (eql (car (chords (chord-line 
                                    (car (editor-objects (car (subviews win)))))))
                      chord)
                  (return win)))
          (incf win-index))
        (if dead-win-index
            (setf (nth dead-win-index *the-chord-editors*)
                  (make-MN-editor-chordMN *g2-g-f-f2-staffs*))
            (car (push (make-MN-editor-chordMN *g2-g-f-f2-staffs*) *the-chord-editors*))
            ))))

(defgeneric add-new-chord-to-mn (self x y))
(defmethod add-new-chord-to-mn ((self C-MN-panel-Mod) x y)
  (declare (special *current-editing-chord* *MN-draw-offset*))
  (unselect-all-chords self x y)
  (let ((dur 100) (vel 100) (chan 1))
    (setf *current-editing-chord*
          (make-instance 'C-chord 
                         :t-time 
                         (max 0 (truncate 
                                 (scaled-mouse-h self 
                                                 (+ (origin self)(- x *MN-draw-offset*)))))
                         :notes (list 
                                 (make-instance 'C-note :midic (give-y-value self y)
                                                        :dur dur :vel vel :chan chan))))
    (open-chord-win self  x y)
    (add-new-chord (chord-line self) *current-editing-chord*)))

(defmethod view-mouse-dragged ((self C-MN-panel-Mod) mouse)
  (declare (ignore mouse))
  (let* ((mouse (view-mouse-position self))
         (mouse-h (point-h mouse))
         (mouse-v (point-v mouse))
         (m-diff-h 
           (if *MN-first-click-mouse* 
               (- mouse-h (point-h  *MN-first-click-mouse*))
               (progn (setf *MN-first-click-mouse* mouse) 0))))
    (if (get-ctrl-setting (view-container self) :dur)
        (and (active-note self) 
             (progn (draw-dragged-duration self)
                    (set-display-value (view-container self) 
                                       (dur (active-note self)))))
        (drag-selection-in-time self  mouse-h mouse-v m-diff-h))))

(defgeneric begin-dragged-mn-control (self x y))
(defmethod begin-dragged-mn-control ((self C-MN-panel-Mod) x y)
  (unless (get-ctrl-setting (view-container self) :dur)
    (when (selected-chords self)
      (dolist (panel (editor-objects (view-container self)))
        (let ((selections (selected-chords panel)))
          (when selections
            (setf (selected-chords panel) nil)
            (dolist (sel-chord selections)
              (unhilite-if-not-selected panel sel-chord x y))
            (setf (selected-chords panel) selections))))
      (begin-rectangle-dragging  self  (active-chord self)
                                 (MN-zoom-scaler (view-container self))
                                 *MN-draw-offset* (scaled-origin self) *MN-C5*))))

(defvar *top-rect* 0)
(defvar *bottom-rect* 0)
(defvar *time-drag* nil)
(defgeneric begin-rectangle-dragging (self chord t-scfactor beg-x time1 C5))
(defmethod begin-rectangle-dragging ((self C-MN-panel-Mod)
                                     chord t-scfactor beg-x time1 C5 )
  (declare (special *top-rect* *bottom-rect* *time-drag*))
  (let ((my-x (calc-chord-pixel-x chord t-scfactor beg-x time1))
        (y-min (1- (give-pixel-y (car (notes chord)) C5)))
        (y-max (give-pixel-y (car (last (notes chord))) C5)))
    (setf *top-rect* (make-point (- my-x 5) (- y-max 18)))
    (setf *bottom-rect* (- y-min y-max -18))
    (setf *time-drag* t)
    (draw-dragging-rectangle self  *top-rect* *bottom-rect*)))

(defgeneric draw-dragging-rectangle (self top bottom))
(defmethod draw-dragging-rectangle ((self C-MN-panel-Mod) top bottom)
  (with-focused-view self
    (with-pen-state (:pattern *black-pattern* :mode :patXor)
      (draw-rect* (point-h top) (point-v top) 11 bottom))))

(defgeneric update-rectangle-dragging (self mouse-h mouse-v mouse-diff))
(defmethod  update-rectangle-dragging ((self C-MN-panel-Mod) mouse-h mouse-v mouse-diff)
  (declare (ignore mouse-v mouse-diff))
  (declare (special *top-rect* *bottom-rect*))
  (draw-dragging-rectangle self *top-rect* *bottom-rect*)
  (setf *top-rect* (make-point (- mouse-h 5) (point-v *top-rect*)))
  (draw-dragging-rectangle self *top-rect* *bottom-rect*))

(defgeneric drag-selection-in-time (self mouse-h mouse-v mouse-diff))
(defmethod drag-selection-in-time ((self C-MN-panel-Mod)  mouse-h mouse-v mouse-diff)
  (declare (special *time-drag*))
  (when *time-drag*
    (set-display-value (view-container self) 
                       (truncate 
                        (scaled-mouse-h self (+ (origin self) (- mouse-h *MN-draw-offset*)))))
    (update-rectangle-dragging self mouse-h mouse-v mouse-diff)))

(defmethod view-mouse-up ((self  C-MN-panel-Mod))
  (declare (special *time-drag* *top-rect* *bottom-rect*))
  (set-display-value (view-container self) nil)
  (if  *time-drag*  
       (let* ((mouse (point-h (view-mouse-position self)))
              (x-val (truncate (scaled-mouse-h self 
                                               (- mouse (point-h (get-old-click self))))))
              (panels (editor-objects (view-container self)))
              chords)
         (setf (active-note self) ())
         (draw-dragging-rectangle self *top-rect* *bottom-rect*)
         (dolist (panel panels)
           (setq chords (selected-chords panel))
           (dolist (chord chords)
             (setf (t-time chord) (max 0 (+ x-val (t-time chord))))
             (remove-chord  (chord-line panel) chord t)
             (add-new-chord (chord-line panel) chord))
           (set-visible-chords panel)
           (erase+view-draw-contents panel))
         (setf *time-drag* nil)
                                        ;(hilite-all-selected self)
         )))

(defgeneric move-all-the-selections (self))
(defmethod move-all-the-selections ((self C-MN-panel-Mod))
  (dolist (panel (editor-objects (view-container self)))
    (let ((selection (selected-chords panel)))
      (dolist (chord selection)
        (remove-chord  (chord-line panel) chord t)
        (add-new-chord (chord-line panel) chord)))))

(defgeneric hilite-all-selected (self))
(defmethod hilite-all-selected ((self C-MN-panel-Mod))
  (dolist (panel (editor-objects (view-container self)))
    (let ((selection (selected-chords panel)))
      (setf (selected-chords panel) nil)
      (dolist (chord selection)
        (hilite-if-not-selected panel chord 0 0))
      (setf (selected-chords panel) selection))))

(defmethod handle-key-event ((self C-MN-panel-Mod) char)
  (declare (special *global-music-notation-panel*))
  (cond
    ((eql char #\K) (remove-all-chords-from-chord-line self))
    ((eql char #\p) (play-chords (chord-line self)))
    ((eql char #\P) 
     (if (selected-chords self)
         (play-selected-chords (chord-line self) 
                               (selected-chords self)
                               0) ; TODO: ?
         (play-visible-chords (chord-line self) (visible-chords self)
                              (truncate
                               (scaled-mouse-h self 
                                               (point-h (view-scroll-position self)))))))
    ((eql char #\s) (stop-play (chord-line self)))
    ((eql char #\r) 
     (when (and (active-note self) (instrument (active-note self))) 
       (setf *global-music-notation-panel* self)
       (remove-instrument-item (active-note self) 0 0)
       (erase+view-draw-contents self))) 
    (t (ui:ed-beep))))

;;;CUT COPY PASTE

(defmethod mn-copy ((self C-MN-panel-Mod))
  (reset-undo self)
  (let ((chords))
    (dolist (panel (editor-objects (view-container self)))
      (if (selected-chords panel)
          (setq chords (append chords (ask-all (selected-chords panel) 'decompile)))))
    (setf (editor-scrap self) 
          (if chords `(list ,@chords) nil))))

(defmethod mn-paste ((self C-MN-panel-Mod)) 
  (save-all-undo self)
  (let ((new-chords (eval (editor-scrap self)))
        (first-time) (selections))
    (unless (list-of-chords new-chords)
      (setq new-chords (list (make-instance 'C-chord :notes new-chords))))
    (dolist (panel (editor-objects (view-container self)))
      (dolist (chord (selected-chords panel))
        (remove-chord (chord-line panel) chord)
        (push chord selections))
      (setf (selected-chords panel) nil))
    (setf selections (nreverse selections))
    (if new-chords
        (cond 
          ((listp new-chords)
           (setq first-time (t-time (car new-chords))) 
           (dolist (one-chord new-chords)
             (setf (t-time one-chord)
                   (+ (if selections
                          (t-time (pop selections))
                          (round (scaled-mouse-h self
                                                 (+ (origin self)
                                                    (- (point-h (view-mouse-position self))
                                                       *MN-draw-offset* )))))
                      (- (t-time one-chord) first-time)))
             (add-new-chord (chord-line self) one-chord))
           (dolist (panel (editor-objects (view-container self)))
             (update-editor panel)))
          (t (setf (t-time new-chords) 
                   (round (scaled-mouse-h self (- (point-h (view-mouse-position self))
                                                  *MN-draw-offset* ))))
             (add-new-chord (chord-line self) new-chords)
             (setf (active-chord self) new-chords)
             (update-editor self))))))

(defmethod mn-cut ((self C-MN-panel-Mod))
  (mn-copy self)
  (save-all-undo self)
  (dolist (panel (editor-objects (view-container self)))
    (dolist (chord (selected-chords panel))
      (remove-chord (chord-line panel) chord))
    (setf (selected-chords panel) nil)
    (setf (active-chord panel) nil)
    (update-editor panel)))

(defun redo-MN-edit ()
  (tell (editor-objects (editor-view-object *active-MN-window*)) 'restore-MN-edition)
  (menu-item-disable *undo-MN-menu*))

(defmethod restore-MN-edition ((self C-MN-panel-mod))
  (setf (chords (chord-line self)) (first (undo-MN-edit self)))
  (setf (selected-chords self) (second (undo-MN-edit self)))
  (update-editor self))

(defgeneric save-all-undo (self))
(defmethod save-all-undo ((self C-MN-panel-mod))
  (menu-item-enable *undo-MN-menu*)
  (tell (editor-objects (view-container self)) 'save-undo))

(defmethod save-undo ((self C-MN-panel-Mod))
  (setf (undo-MN-edit self) 
        (list (chords (chord-line self)) (selected-chords self))))

(defgeneric reset-undo (self))
(defmethod reset-undo ((self C-MN-panel-Mod))
  (menu-item-disable *undo-MN-menu*))

(defmethod draw-appl-label ((self  C-MN-panel-Mod) label)
  (declare (ignore label)))


(defmethod save-window-state ((self  C-MN-panel-Mod) win)
  (declare (ignore win)))

(defun open-chord-ed (win-obj win obj chord  x y)
  (let ((editor (car (editor-objects (car (subviews win-obj)))))
        (chord-l (make-instance 'C-chord-line 
                                :chords (list chord) )))
    (setf (chord-line editor) chord-l)             
    (set-view-position win-obj x (max 35 y))
    (set-pw-win+pw-obj win-obj win obj)
    (window-select win-obj)
                                        ;(update-view-controler (car (subviews win-obj)))
    (unless (zone-selection? editor)
      (update-view-controler (car (subviews win-obj))))))

