;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               mn-editor.lisp
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

(defgeneric editor-objects (self))
(defgeneric draw-clef (self x y))
(defgeneric draw-staff (self x C5))

;;=================================================================================================
(defvar *current-mn-window*    nil)
(defvar *global-selected-note* nil)
(defvar *mn-global-ins-y*      0)
(defvar *mn-note-ins-y*        0)
(defvar *mn-play-flag*         nil)
(defvar *mn-view-arp-flag*     nil)
(defvar *mn-view-dur-flag*     nil)
(defvar *mn-view-dyn-flag*     nil)
(defvar *mn-view-ins-flag*     nil)
(defvar *mn-view-offset-flag*  nil)
(defvar *staff-num*            3)

;;=================================================================================================
;; delta-y -> staffin alareuna

(defclass C-clef (ui:simple-view)
  ((clef :initform #\& :initarg :clef :accessor clef)
   (delta-y :initform 0 :initarg :delta-y :accessor delta-y)))

(defmethod draw-clef ((self C-clef) x y)
  (draw-char (+ 5 x) (- y (delta-y self)) (clef self)))

;;==============================

;;; GA 17/5/94
;;(defvar *staff-lines* (make-string 255 :initial-element #\= ))
(defvar *staff-lines* (make-string 65 :initial-element #\= ))

(defclass C-staff ()
  ((clef-obj :initform nil :initarg :clef-obj :accessor clef-obj)
   (delta-y :initform 0 :initarg :delta-y :accessor delta-y)))

(defmethod draw-staff ((self C-staff) x C5)
  (draw-clef (clef-obj self) x (- C5 (delta-y self)))
  (draw-string x (- C5 (delta-y self)) *staff-lines*))

;;__________________________

(defclass C-staff-empty (C-staff) ())

(defmethod draw-staff ((self C-staff-empty) x C5) (declare (ignore x C5)))

;;__________________________
;; global staffs and clefs

(defvar *mn-staff-line-width* 4)


(defvar *g-clef*)
(defvar *g-staff*)
(defvar *g2-staff*)
(defvar *f-clef*)
(defvar *f-staff*)
(defvar *f2-staff*)
(defvar *empty-staff*)
(defvar *g2-g-staffs*)
(defvar *g-plain-staffs*)
(defvar *g-f-staffs*)
(defvar *f-plain-staffs*)
(defvar *g-f-f2-staffs*)
(defvar *g2-g-f-f2-staffs*)
(defvar *empty-staffs*)

(defvar *global-staff-list*  '(*g2-g-staffs* *g-plain-staffs* *g-f-staffs*
                               *f-plain-staffs* *g-f-f2-staffs* *g2-g-f-f2-staffs* *empty-staffs*))

(defun initialize-mn-editor ()
  (setf *g-clef*   (make-instance 'C-clef   :delta-y (- (* *mn-staff-line-width* 1) 4)))
  (setf *g-staff*  (make-instance 'C-staff  :clef-obj *g-clef* :delta-y (- (* *mn-staff-line-width* 1) 1)))
  (setf *g2-staff* (make-instance 'C-staff  :clef-obj *g-clef* :delta-y (- (* *mn-staff-line-width* 8) 1)))
  (setf *f-clef*   (make-instance 'C-clef   :clef #\? :delta-y -1))
  (setf *f-staff*  (make-instance 'C-staff  :clef-obj *f-clef* :delta-y (- (* *mn-staff-line-width* -5) 1)))
  (setf *f2-staff* (make-instance 'C-staff  :clef-obj *f-clef* :delta-y (- (* *mn-staff-line-width* -12) 1)))

  (setf *empty-staff* (make-instance 'C-staff-empty))

  (setf *g2-g-staffs*      (list *g2-staff* *g-staff*))          ;1
  (setf *g-plain-staffs*   (list *g-staff*))                     ;2
  (setf *g-f-staffs*       (list *g-staff* *f-staff*))           ;3
  (setf *f-plain-staffs*   (list *f-staff*))                     ;4
  (setf *g-f-f2-staffs*    (list *g-staff* *f-staff* *f2-staff*)) ;5
  (setf *g2-g-f-f2-staffs* (list *g2-staff* *g-staff* *f-staff* *f2-staff*)) ;6
  (setf *empty-staffs*     (list *empty-staff*)) ;7
  (values))


(defun get-staff-offsets (staff-num)
  (case staff-num
    (1 (values 32 4)) (2 (values 4 4)) (3 (values 4 -20))
    (4 (values -20 -20)) (5 (values 4 -48)) (6 (values 32 -48))
    (7 (values 0 0))))

;;=====================================================
;;=====================================================
;;editor
(defvar *note-head-cursor* 110)
(defvar *MN-editor-scrap* ())
(defvar *CURRENT-MN-EDITOR* ())

(defclass C-mus-not-view (ui:scroller)
  ((editor-objects :initform nil :initarg :editor-objects :accessor editor-objects)
   (active-editor :initform nil :accessor active-editor)
   (external-controls :initarg nil :accessor external-controls)
   (ctrl-settings :accessor ctrl-settings)
   (saved-selected :initform nil :accessor saved-selected)
   (MN-zoom-scaler :initform 1.0 :initarg :MN-zoom-scaler :accessor MN-zoom-scaler)
   (local-scale :initform nil :accessor local-scale)
   (local-approx :initform nil :accessor local-approx)))

(defmethod editor-objects (self)
  (declare (ignorable self))
  '())

(defmethod decompile ((self C-mus-not-view))
  (let ((editor-objects (ask-all (editor-objects self) 'decompile)))
    `(let (objs)
       (make-instance ',(class-name (class-of self))
                      :view-position ,(view-position self)
                      :view-size ,(view-size self)
                      :bottom-boarder 20
                      :v-scrollp nil
                      :track-thumb-p t
                      :view-subviews (setq objs (list ,@editor-objects))
                      :editor-objects objs))))

(defmethod initialize-instance :after ((self C-mus-not-view) &key controls)
  (declare (ignore controls))
  (make-extra-controls self))

;;=============================
;;scales + approximations

(defgeneric use-all-approx-scale (self scale))
(defmethod use-all-approx-scale ((self C-mus-not-view) scale)
  (unless (eql (local-approx self) scale)
    (setf (local-approx self) scale)
    (setf (local-scale self) scale)
    (let ((*current-music-notation-scale* scale)
          (*current-approx-scale* scale))
      (mapc #'update-all-notes (editor-objects (car (subviews *active-mn-window*))))) ))

(defgeneric use-all-scale (self scale))
(defmethod use-all-scale ((self C-mus-not-view) scale)
  (unless (eql (local-scale self) scale)
    (setf (local-approx self) scale)
    (setf (local-scale self) scale)
    (let ((*current-music-notation-scale* scale)
          (*current-approx-scale* scale))
      (mapc #'update-all-notes (editor-objects (car (subviews *active-mn-window*)))))) )

(defun erase-all-scale-marks ()
  (set-menu-item-check-mark *g-option-c-major* nil)
  (set-menu-item-check-mark *g-option-chromatic* nil)
  (set-menu-item-check-mark *semitone-menu* nil)
  (set-menu-item-check-mark *quartertone-menu* nil)
  (set-menu-item-check-mark *eighthtone-menu* nil))

(defun set-scale-mark (scale)
  (cond
    ((eql scale *c-major-scale*)
     (set-menu-item-check-mark *g-option-c-major* t)
     (set-menu-item-check-mark *semitone-menu* t))
    ((eql scale *chromatic-scale*)
     (set-menu-item-check-mark *g-option-chromatic* t)
     (set-menu-item-check-mark *semitone-menu* t))
    ((eql scale *1/4-tone-chromatic-scale*)
     (set-menu-item-check-mark *g-option-chromatic* t)
     (set-menu-item-check-mark *quartertone-menu* t))
    ((eql scale *1/8-tone-chromatic-scale*)
     (set-menu-item-check-mark *g-option-chromatic* t)
     (set-menu-item-check-mark *eighthtone-menu* t))))

(defun get-global-options-marks ()
  (list (and (menu-item-check-mark *g-option-c-major*) t)
        (and (menu-item-check-mark *g-option-chromatic*) t)
        (and (menu-item-check-mark *semitone-menu*) t)
        (and (menu-item-check-mark *quartertone-menu*) t)
        (and (menu-item-check-mark *eighthtone-menu*) t)
        (if (menu-item-check-mark *play-Pbend-menu*) :pc :mc)))

(defun restore-global-options (mark-list &optional eval-option)
  (erase-all-scale-marks)
  (case (position t mark-list :from-end t)
    (0 (set-globally-scale *c-major-scale*))
    (1 (set-globally-scale *chromatic-scale*))
    (2 (set-globally-scale *chromatic-scale*))
    (3 (set-globally-scale *1/4-tone-chromatic-scale*))
    (4 (set-globally-scale *1/8-tone-chromatic-scale*)))
  (set-playing-option (if (eql (car (last mark-list)) :mc) :mc :pb))
  (set-eval-click eval-option))

;;(restore-global-options '(nil t nil nil t :mc))

(defun set-globally-scale (scale)
  (erase-all-scale-marks)
  (setf *current-approx-scale* scale *current-music-notation-scale* scale)
  (set-scale-mark scale)
  (record-event :|PWst| :|gloo| `((,(if (or (equal scale *chromatic-scale*) (equal scale *c-major-scale*))
                                        :|opsc| :|opap|)
                                   ,(cond ((equal scale *chromatic-scale*) "Chromatic")
                                          ((equal scale *1/4-tone-chromatic-scale*) "Quarter tone")
                                          ((equal scale *1/8-tone-chromatic-scale*) "Eigth tone")
                                          ((equal scale *c-major-scale*) "C-major")
                                          )))))

(defclass C-zoomer (C-numbox)
  ( (the-view :initarg :the-view :reader the-view)))

(defmethod initialize-instance :after ((self C-zoomer) &key args)
  (declare (ignore args))
  (setf (min-val self) 1)
  (setf (max-val self) 10000)
  (setf (value self) 1000))

(defvar *max-zoom* 10000)

(defmethod item-action-while-drag ((self C-zoomer))
  (let ((the-view (the-view self)))
    (setf (MN-zoom-scaler the-view) ;;(+ 0.01  (* 10 (value self) (/ *max-zoom*))))
          (/ *max-zoom* (+ 0.01 (value self)) 10))
    (update-view-controler the-view)))

(defmethod view-double-click-event-handler ((self C-zoomer) where)
  (declare (ignore where)))

(defgeneric erase-the-view (self))
(defmethod erase-the-view ((self C-mus-not-view))
  (with-focused-view self
    (with-pen-state (:mode :srccopy :pattern *white-pattern*)
      (fill-rect* 0 0 (- (w self) 9)(- (h self) 17)))) )

(defgeneric make-extra-controls (self))
(defmethod make-extra-controls ((self C-mus-not-view))
  (let ((v-extreme (+  (point-v (view-position self)) (point-v (view-size self)) 10))
        (h-initial (+ 2 (point-h (view-position self)))))
                                        ;(running-length  185))    ;(- (point-h (view-size self)) 5)))
    (setf (external-controls self)
          (list
           (make-instance
            'C-zoomer
            :the-view self
            :view-container (view-window self)
            :view-position (make-point h-initial v-extreme)
            :min-val -1000)
           (make-instance
            'static-text-dialog-item
            :view-container (view-window self)
            :view-position
            (make-point (+ h-initial 60) (- v-extreme 2))
            :dialog-item-text "zoom"
            :view-font *patchwork-font-spec*)
           (make-instance
            'check-box-dialog-item
            :view-container (view-window self)
            :view-position (make-point h-initial (- v-extreme 27))
            :dialog-item-text "ins"
            :view-font *patchwork-font-spec*
            :dialog-item-action
            (lambda (item)
              (rplacd (assoc :ins (ctrl-settings self))
                      (check-box-checked-p item))
              (update-view-controler self)))
           (add-to-radio-cluster self (+ h-initial 40) (- v-extreme 27) "dur" :dur)
           (add-to-radio-cluster self (+ h-initial 82) (- v-extreme 27) "dyn" :dyn)
           (add-to-radio-cluster self (+ h-initial 40) (- v-extreme 45) "offs" :offs)))
    (setf (ctrl-settings self)
          (list (cons :ins nil) (cons :dur nil) (cons :dyn nil) (cons :offs nil))) ) )

(defgeneric add-to-radio-cluster (self x y txt type))
(defmethod add-to-radio-cluster ((self C-mus-not-view) x y txt type)
  (make-instance
   'radio-button-dialog-item
   :view-container (view-window self)
   :view-position (make-point x y)
   :dialog-item-text txt
   :view-font *patchwork-font-spec*
   :dialog-item-action
   (lambda (item)
     (set-value-ctrl self item type)
     (update-view-controler self))))

(defgeneric set-value-ctrl (self item kind))
(defmethod set-value-ctrl ((self C-mus-not-view) item kind)
  (if (cdr (assoc kind (ctrl-settings self)))
      (progn (radio-button-unpush item)
             (rplacd (assoc kind (ctrl-settings self)) nil))
      (progn (reset-view-ctrls self)
             (rplacd (assoc kind (ctrl-settings self)) t))))


(defparameter *MN-view-ctrls-space* (make-point 9 36))

(defgeneric update-view-controler (self))
(defmethod update-view-controler ((self C-mus-not-view))
  (with-focused-view self
    (with-pen-state (:mode :srccopy :pattern *white-pattern*)
      (fill-rect* 0 0 (- (w self) (point-h *MN-view-ctrls-space*))
                  (- (h self) (point-v *MN-view-ctrls-space*)))))
  (view-draw-contents self))

(defgeneric reset-view-ctrls (self))
(defmethod reset-view-ctrls ((self C-mus-not-view))
  (let ((ctrl-a-list (cdr (ctrl-settings self))))
    (dolist (item ctrl-a-list)
      (rplacd item nil))))

(defgeneric get-ctrl-setting (self ctrl))
(defmethod get-ctrl-setting ((self C-mus-not-view) ctrl)
  (cdr (assoc ctrl (ctrl-settings self))))

(defmethod set-view-position ((self C-mus-not-view) h &optional v)
  (declare (ignorable h v))
  (call-next-method))

(defmethod set-view-size ((self C-mus-not-view) h &optional v)
  (declare (ignorable h v))
  (set-extCtrl-view-size self h v)
  (call-next-method))

;;This method should really send a "view-size-changed" to the external controls, which
;;should be sub-classes of dialog-items... [Camilo]
(defgeneric set-extCtrl-view-size (self h &optional v))
(defmethod set-extCtrl-view-size ((self C-mus-not-view)  h &optional v)
  (let ((ctrls (external-controls self))
        (old-size (view-size self)))
    (dolist (ctrl ctrls)
      (set-view-position ctrl
                         (point-h (view-position ctrl))
                         (+  (point-v (view-position ctrl))
                             (- (or  v (point-v h)) (point-v old-size)))))))

(defmethod view-window-grown ((self C-mus-not-view))
  (declare (special *MN-view-ctrls-space* *mn-draw-offset*))
  (set-view-size self
                 (subtract-points (view-size (view-window self)) (make-point 15 25)))
  (let* ((new-view-size
           (subtract-points (view-size  self) *MN-view-ctrls-space*))
         (objects (editor-objects self))
         (v-size (truncate (point-v new-view-size) (length objects)))
         (pos 2) (origin-count 0) (adjusted-size (- (point-h new-view-size) *MN-draw-offset*)))
    (dolist (panel objects)
      (set-view-position panel  (make-point (point-h (view-position panel)) pos))
      (set-view-size panel (make-point (point-h new-view-size) (- v-size 6)))
                                        ;(if (monofonic-mn? self) (setf (origin panel) origin-count))
      (setf (origin panel) origin-count)
      (if (monofonic-mn? self) (incf origin-count adjusted-size))
      (incf pos v-size))))

(defmethod scroll-bar-changed ((view C-mus-not-view) scroll-bar)
  (let ((new-value (point-h (scroll-bar-setting scroll-bar)))
        (panels (editor-objects view)))
    (dolist (panel panels)
      (erase-yourself panel)
      (set-origin panel (make-point new-value (point-v (view-scroll-position panel))))
      (view-draw-contents panel)
      ;;(update-all-selections panel)
      )))

(defmethod ui::normal-scroll-bar-limits ((view C-mus-not-view) max-h &optional max-v)
  (declare (ignore max-h max-v))
  (values (make-point 0 30000)
          (make-point 0 30000)))

(defmethod ui::scroll-bar-page-size ((view C-mus-not-view))
  (round (point-h (view-size view)) 4))

;;==============
(defgeneric pretty-visible-layout (self))
(defmethod pretty-visible-layout ((self C-mus-not-view))
  (tell (editor-objects self) 'pretty-visible-layout)
  (set-view-size (view-window self) (make-point (w self)(+ (h self) 2))))

(defmethod update-all-chord-lines ((self C-mus-not-view) chord-lines)
  (let ((editors (editor-objects self)))
    (while (and chord-lines editors)
      (setf (chord-line (pop editors)) (pop chord-lines)))))
;;==============
;;events

(defmethod view-mouse-moved ((self C-mus-not-view) mouse)
  (setf (active-editor self) (ask (editor-objects self) #'view-contains-point-p+self mouse))
  (if (active-editor self)
      (view-mouse-moved (active-editor self) mouse)
      (tell (subviews self) #'reset-active-chord)))

(defmethod view-mouse-dragged ((self C-mus-not-view) mouse)
  (setf (active-editor self) (ask (editor-objects self) #'view-contains-point-p+self mouse))
  (if (active-editor self)
      (view-mouse-dragged (active-editor self) mouse)))

(defgeneric key-pressed-MN-editor (self char))
(defmethod key-pressed-MN-editor ((self C-mus-not-view) char)
  (cond ((eql char #\p) (play-all-staffs self))
        ((eql char #\s) (stop-all-staffs self))
        ((eql char #\o) (tell (editor-objects self) 'open-object-editor (view-window self)))
        ((and (eql char #\c) (active-editor self))
         (when (active-note (active-editor self))
           (add-MN-to-note (active-note (active-editor self)) (view-window self) 0 0)))
        ((eql char #\w)
         (when (active-note (active-editor self))
           (add-PWwin-to-note (active-note (active-editor self)) (view-window self) 0 0)))
        ((eql char #\A) (Do-selections-all self))
        (t (when (active-editor self) (handle-key-event (active-editor self) char)) )))

(defgeneric Do-selections-all (self))
(defmethod Do-selections-all ((self C-mus-not-view)) )

(defmethod cut ((self C-mus-not-view))
  (and (active-editor self) (mn-cut (active-editor self))))

(defmethod copy ((self C-mus-not-view))
  (and (active-editor self) (mn-copy (active-editor self))))

(defmethod paste ((self C-mus-not-view))
  (and (active-editor self)(mn-paste (active-editor self))))

(defvar *playing-option* :pb)

(defun set-playing-option (option)
  (setf *playing-option* option)
  (cond ((eql option :pb)
         (set-menu-item-check-mark *play-Pbend-menu* t)
         (set-menu-item-check-mark *play-Multichan-menu* nil)
         (record-event :|PWst| :|gloo| `((,:|oppl| ,"Pitch Bend"))))
        ((eql option :mc)
         (set-menu-item-check-mark *play-Pbend-menu* nil)
         (set-menu-item-check-mark *play-Multichan-menu* t)
         (record-event :|PWst| :|gloo| `((,:|oppl| ,"Multi Channel"))))))



;;aaa from pw-modifs le 10-9-95
(defgeneric stop-all-staffs (self))
(defmethod stop-all-staffs ((self C-mus-not-view))
  (setf *MN-play-flag* nil)
  (tell (ask-all (editor-objects self) 'chord-line) 'stop-play))

;; (defmethod stop-all-staffs ((self C-mus-not-view))
;;   (tell (ask-all (editor-objects self) 'chord-line) 'stop-play))

(defgeneric play-all-staffs (self))
(defmethod play-all-staffs ((self C-mus-not-view))
  (let ((panels (editor-objects self)))
    (if (monofonic-mn? self)
        (progn (setf patchwork.scheduler::*print-on-late?* t)
               (start
                 (apdfuncall 10  2 20
                             'play-chords (chord-line (car panels)))))
        (progn (setf patchwork.scheduler::*print-on-late?* t)
               (start (setf (advance) 30)
                 (dolist (panel panels)
                   (dfuncall 40
                             'play-chords (chord-line panel))))))))

(defun compute-approx ()
  (cond ((eql *current-approx-scale* *1/4-tone-chromatic-scale*) 4)
        ((eql *current-approx-scale* *1/8-tone-chromatic-scale*) 8)
        (t 2)))

(defgeneric approx-for-playing (midic &optional approx))
(defmethod approx-for-playing (midic &optional approx)
  (epw::approx-m midic (or approx (compute-approx))))

(defmethod view-click-event-handler ((self C-mus-not-view) where)
  (declare (ignore where))
  (call-next-method))

(defmethod view-mouse-up ((self C-mus-not-view))
  (setf *default-MN-cursor* *note-head-cursor*)
  (if (and (active-editor self)
           (active-chord (active-editor self)))
      (view-mouse-up (active-editor self))))

;;=====================================================
(defvar  *MN-draw-offset* 40)
(defvar  *MN-C5* 70)
;;==========================================================================================================
;;==========================================================================================================

(defclass C-music-notation-panel (ui::scroller)
  ((chord-line :initform nil :initarg :chord-line :accessor chord-line)
   (visible-chords :initform nil :accessor visible-chords)
   (active-chord :initform nil :accessor active-chord)
   (active-note :initform nil :accessor active-note)
   (staff-list :initform *g2-g-f-f2-staffs* :initarg :staff-list :accessor staff-list)
   (staff-num :initform 6 :accessor staff-num)
   (origin :initform 0 :initarg :origin :accessor origin)
   ))

(defmethod decompile ((self C-music-notation-panel))
  `(make-instance ',(class-name (class-of self))
                  :view-position ,(view-position self)
                  :view-size ,(view-size self)
                  :h-scrollp nil
                  :track-thumb-p t
                  :chord-line ,(decompile (chord-line self))))

;;==============

(defmethod ui::normal-scroll-bar-limits ((view C-music-notation-panel) max-h &optional max-v)
  (declare (ignore max-h max-v))
  (values (make-point -250 250)
          (make-point -250 250)))

(defmethod reset-active-chord ((self C-music-notation-panel))
  (setf (active-chord self) nil)
  (setf (active-note self) nil))

(defgeneric set-visible-chords (self))
(defmethod set-visible-chords ((self C-music-notation-panel))
  (let* ((time1 (+ (scaled-origin self)
                   (scaled-mouse-h self (point-h (view-scroll-position  self)))))
         (time2 (+ time1 (scaled-mouse-h self (point-h (view-size self))))))
    (setf (visible-chords self) (find-visible-chords (chord-line self) time1 time2))))

(defgeneric scaled-origin (self))
(defmethod scaled-origin ((self C-music-notation-panel))
  (round (scaled-mouse-h self (origin self))))

(defgeneric update-all-notes (self))
(defmethod update-all-notes ((self C-music-notation-panel))
  (let* ((chords (chords (chord-line self))))
    (dolist (chord chords)
      (mapc #'update-note (notes chord))
      (update-chord chord))
    (erase+view-draw-contents self)))

;;==============
;;draw&print

(defgeneric print-draw-contents (self))
(defmethod print-draw-contents ((self C-music-notation-panel))
  (declare (special *current-MN-editor* *MN-global-ins-y*))
  (setf *current-MN-editor* self)
  (setf *MN-global-ins-y* (+ 60 *MN-C5*))
  (let ((my-view (view-container self)))
    (let ((*mn-view-ins-flag* (get-ctrl-setting my-view :ins))
          (*mn-view-dur-flag* (get-ctrl-setting my-view :dur))
          (*mn-view-dyn-flag* (get-ctrl-setting my-view :dyn))
          (*mn-view-offset-flag* (get-ctrl-setting my-view :offs))
          (*staff-num* (staff-num self))
          (*current-music-notation-scale*
            (or (local-scale my-view) *current-music-notation-scale*))
          (*current-approx-scale* (or (local-approx my-view) *current-approx-scale*)))
      (declare (special *mn-view-ins-flag* *mn-view-dur-flag*
                        *mn-view-dyn-flag* *mn-view-offset-flag*))
      (set-visible-chords self)
      ;;(with-focused-view self
      ;;(set-view-font (view-container (view-container  self)) *music-font-spec*))
      (with-font-focused-view self
        (tell (staff-list self) #'draw-staff
              (+ (point-h (view-scroll-position self)) 2)
              *MN-C5*)
        (view-draw-specific self (MN-zoom-scaler (view-container self))
                            (view-scroll-position self) *MN-draw-offset*
                            *MN-C5*)))))

(defmethod view-draw-contents :before ((self C-music-notation-panel))
  (setf *current-MN-window* (view-window self)))

(defmethod view-draw-contents ((self C-music-notation-panel))
  (declare (special *current-MN-editor* *MN-global-ins-y*))
  (setf *current-MN-editor* self)
  (setf *MN-global-ins-y* (+ 60 *MN-C5*))
  (let ((my-view (view-container self)))
    (let ((*mn-view-ins-flag* (get-ctrl-setting my-view :ins))
          (*mn-view-dur-flag* (get-ctrl-setting my-view :dur))
          (*mn-view-dyn-flag* (get-ctrl-setting my-view :dyn))
          (*mn-view-offset-flag* (get-ctrl-setting my-view :offs))
          (*staff-num* (staff-num self))
          (*current-music-notation-scale*
            (or (local-scale my-view) *current-music-notation-scale*))
          (*current-approx-scale* (or (local-approx my-view) *current-approx-scale*)))
      (declare (special *mn-view-ins-flag* *mn-view-dur-flag*
                        *mn-view-dyn-flag* *mn-view-offset-flag*))
      (set-visible-chords self)
      ;;(with-focused-view self
      ;;(set-view-font (view-container my-view) *music-font-spec*))
      (with-font-focused-view self
        (tell (staff-list self) #'draw-staff
              (+ (point-h (view-scroll-position self)) 2)
              *MN-C5*)
        (view-draw-specific self (MN-zoom-scaler my-view)
                            (view-scroll-position self) *MN-draw-offset*
                            *MN-C5*)))))

(defmethod view-draw-contents :after ((self C-music-notation-panel))
  (when (super-win (view-window self))
    (with-focused-view (view-window self)
      (set-view-font  (view-window self) *patchwork-font-spec*)
      (draw-string 5 10 (window-title (super-win (view-window self)))))))

(defgeneric view-draw-specific (self zoom-scale scroll-pos MN-offset MN-C5))
(defmethod view-draw-specific ((self C-music-notation-panel)
                               zoom-scale scroll-pos MN-offset MN-C5)
  (tell (set-visible-chords self) #'draw-chord
        zoom-scale (+ MN-offset (point-h scroll-pos))
        (point-h scroll-pos) MN-C5))

(defgeneric draw-ledger-lines-arp (self note x y-min y-max C5))
(defmethod draw-ledger-lines-arp ((self C-music-notation-panel) note x y-min y-max C5)
  (draw-ledger-for-notes (list note) x y-min y-max C5))

(defgeneric draw-ledger-line-arp (self x y))
(defmethod draw-ledger-line-arp ((self C-music-notation-panel) x y)
  (draw-line (- x 10) y (+ x 6) y))

(defvar *staff-lines-short* (make-string 1 :initial-element #\= ))

(defgeneric draw-staff-lines-short (self))
(defmethod draw-staff-lines-short  ((self C-music-notation-panel))
  (declare (special *mn-first-click-mouse*))
  (let ((y-off (- *MN-C5* 39)))
    (for (i 0 1 4)
      (draw-string
       (+ (point-h (view-scroll-position self))
          (-  (point-h
               *MN-first-click-mouse*) 10)) (+ (* 20 i) y-off) *staff-lines-short*))))

(defgeneric erase-yourself (view))
(defmethod erase-yourself ((view C-music-notation-panel))
  (let ((w (point-h (view-size view)))
        (h (point-v (view-size view)))
        (x (point-h (view-scroll-position view)))
        (y (point-v (view-scroll-position view))))
    (with-focused-view view
      (with-pen-state (:mode :srccopy :pattern *white-pattern*)
        (fill-rect*  x y (+ x w) (+ y h))))))

(defmethod scroll-bar-changed ((view C-music-notation-panel) scroll-bar)
  (let ((h-pos (point-h (view-scroll-position view))))
    (erase-yourself view)
    (set-origin view (make-point h-pos (scroll-bar-setting scroll-bar)))
    (view-draw-contents view)
    (update-all-selections view)))
;;==============
;;misc
(defgeneric scaled-mouse-h (self mouse-h))
(defmethod scaled-mouse-h ((self C-music-notation-panel) mouse-h)
  (/  mouse-h  (MN-zoom-scaler (view-container self))))

(defgeneric find-mouse-point-in-chords (self mouse-h))
(defmethod find-mouse-point-in-chords ((self C-music-notation-panel) mouse-h)
  (ask (visible-chords self) #'inside-chord-?
       (scaled-mouse-h self (+ mouse-h (origin self))) 5))

(defgeneric give-y-diatone (self y))
(defmethod give-y-diatone ((self C-music-notation-panel) y)
  (+ 35 (truncate (/ (- *MN-C5* y) 2))))

(defgeneric give-y-value (self y))
(defmethod give-y-value ((self C-music-notation-panel) y)
  (let ((c-c5-to-0 (- *MN-C5* y)))
    (* 100 (+ (* 12 (truncate (/  (give-y-diatone self (+ (if (< c-c5-to-0 0) 1 0) y)) 7)))
              (case (mod c-c5-to-0 14)
                (0 0)(1 1)(2 2)(3 3)(4 4)(5 4)(6 5)(7 6)(8 7)(9 8)
                (10 9)(11 10)(12 11)(13 11))))))

(defgeneric remove-all-chords-from-chord-line (self))
(defmethod remove-all-chords-from-chord-line ((self C-music-notation-panel))
  (tell (apply #'append (ask-all (chords (chord-line self)) 'notes)) 'remove-instrument-item ()())
  (setf (chords (chord-line self)) ())
  (setf (active-chord self) ())
  (setf (active-note self) ())
  (with-focused-view self
    (erase-draw-contents self)
    (view-draw-contents self)))

;;==============
;;events

(defmethod view-mouse-moved ((self C-music-notation-panel) mouse)
  (declare (ignore mouse))
  (let* ((mouse (view-mouse-position self))
         (mouse-h (point-h mouse))
         (mouse-v (point-v mouse)))
    (setf (active-chord self)
          (find-mouse-point-in-chords self (- mouse-h *MN-draw-offset*)))
    (when (active-chord self)
      (setf (active-note self)
            (ask (notes (active-chord self))
                 'inside-note?-3 mouse-h
                 (calc-chord-pixel-x (active-chord self)
                                     (MN-zoom-scaler (view-container  self))
                                     (+ *MN-draw-offset*
                                        (point-h (view-scroll-position self)))
                                     (point-h (view-scroll-position self)))
                 (give-y-diatone self  mouse-v ))))))

(defvar *MN-first-click-mouse* ())
(defgeneric get-old-click (self))
(defmethod get-old-click ((self C-music-notation-panel))
  *MN-first-click-mouse*)

(defvar *old-drag-value* ())
(defmethod view-click-event-handler ((self C-music-notation-panel) where)
  (declare (ignore where))
  (setf *MN-first-click-mouse* (view-mouse-position self)) )

(defvar *old-MN-dur* ())

(defmethod view-mouse-dragged ((self C-music-notation-panel) mouse)
  (declare (ignore mouse)) )

(defmethod view-mouse-up ((self C-music-notation-panel)) (erase+view-draw-contents self))

;;to be used by sub-classes
(defgeneric draw-single-dur-line-2 (self chord note))
(defmethod draw-single-dur-line-2  ((self C-music-notation-panel) chord note)
  (with-focused-view self
    (draw-single-dur-line chord note
                          (MN-zoom-scaler (view-container self))
                          *MN-draw-offset*
                          (origin self)
                          *MN-C5*)))

(defgeneric draw-dragged-duration (self))
(defmethod draw-dragged-duration ((self C-music-notation-panel))
  (let* ((mouse-diff (- (point-v (get-old-click self))
                        (point-v (view-mouse-position self)))))
    (draw-single-dur-line-2 self (active-chord self) (active-note self))
    (setf (dur (active-note self))  (max 0 (+ *old-MN-dur*  (* (if (shift-key-p) 10 1) mouse-diff))))
    (draw-single-dur-line-2 self (active-chord self) (active-note self))))

(defgeneric open-object-editor (self window))
(defmethod open-object-editor ((self C-music-notation-panel) window )
  (let ((*menubar-frozen* t))
    (if (active-note self)
        (open-instrument-editor (active-note self) window 0 0))
    (when (eql (class-name (class-of self)) 'C-MN-PANEL-MOD)
      (let ((active-notes (apply #'append (ask-all (selected-chords self) #'notes))))
        (when active-notes
          (tell active-notes #'open-instrument-editor  window 0 0)))))
  (draw-menubar-if))

(defgeneric remove-chord-or-note (self))
(defmethod remove-chord-or-note ((self C-music-notation-panel))  )

(defmethod mn-cut ((self C-music-notation-panel))  )

(defmethod mn-copy ((self C-music-notation-panel))  )

(defmethod mn-paste ((self C-music-notation-panel))  )

(defun list-of-chords (list)
  (typep (car list) 'C-chord))

(defun list-of-notes (list)
  (typep (car list) 'C-note))

(defgeneric add-pw-window (self window))
(defmethod add-pw-window ((self C-music-notation-panel) window)
  (declare (ignore window)))

(defgeneric handle-key-event (self char))
(defmethod handle-key-event ((self C-music-notation-panel) char )
  (declare (ignore char)))

(defgeneric update-all-selections (self))
(defmethod update-all-selections ((self C-music-notation-panel)) )

;;===========================================================

(defvar *MN-window-counter* 0)

(defun make-music-notation-editor (window-class view-class panel-class w-size
                                   &optional (staffs *g2-g-f-f2-staffs*)
                                     name)
  (let* ((win-string  (if name
                          (string-downcase name)
                          (format nil "MN~D" (incf *MN-window-counter*))))
         (mn-window   (make-instance window-class :close-box-p t :window-show nil
                                                  :window-title win-string
                                                  :view-position (make-point 10 50)
                                                  :view-size w-size ))   ;(make-point 600 170)))
         (editor-view (make-instance view-class
                                     :view-container mn-window
                                     :view-position (make-point 2 2)
                                     :view-size (subtract-points w-size (make-point 15 25)) ;(make-point 585 145)
                                     :bottom-boarder 20
                                     :v-scrollp nil
                                     :track-thumb-p t)))
    (setf (editor-objects editor-view)
          (list (make-instance panel-class
                               :view-container editor-view
                               :view-position (make-point 0 0)
                               :view-size (subtract-points w-size (make-point 24 67))  ;(make-point 576 143)
                               :h-scrollp nil
                               :track-thumb-p t
                               :view-font *music-font-spec*
                               :chord-line (make-instance 'C-chord-line))))
    (setf (staff-list (car (editor-objects editor-view))) staffs)
    mn-window))

(defun make-n-music-notation-editors (count patch-obj window-class view-class panel-class
                                      w-size &optional name)
  (declare (ignore w-size))
  (let* ((chordL-list (chord-line-list patch-obj))
         (new-list)
         (dif-len (- count (length chordL-list)))
         (win-string (if name (string-downcase name)
                         (format nil "MN~D" (incf *MN-window-counter*))))
         (mn-window (make-instance window-class :close-box-p t :window-show nil
                                                :window-title win-string
                                                :view-position (make-point 10 50)
                                                :view-size (make-point 600 (+ 25 (* count 145)))))
         (editor-view (make-instance view-class
                                     :view-container mn-window
                                     :view-position (make-point 2 2)
                                     :view-size (make-point 585 (* count 145))
                                     :bottom-boarder 20
                                     :v-scrollp nil
                                     :track-thumb-p t))
         position-now
         editor-objects)
    (unless (zerop dif-len)
      (dotimes (i dif-len)
        (push (make-instance 'C-chord-line) new-list))
      (setq chordL-list (nconc chordL-list new-list)))
    (for (i 0 1 (1- count))
      (setq position-now (make-point 0 (* i 145)))
      (push (make-instance panel-class
                           :view-container editor-view
                           :view-position position-now
                           :view-size #@(576 143)
                           :h-scrollp nil
                           :track-thumb-p t
                           :view-font *music-font-spec*
                           :chord-line (nth i chordL-list)) ;(make-instance 'C-chord-line))
            editor-objects))
    (setf (editor-objects editor-view) (nreverse editor-objects))
    (setf (chord-line-list patch-obj) chordL-list)
    mn-window))

;; (progn
;;   ;;=========================================================================================================
;;   ;;=========================================================================================================
;;   (defvar mn-window ())
;;   (defvar first-scroller ())
;;   (defvar second-scroller ())
;;   (defvar third-scroller ())
;;   (defvar fourth-scroller ())
;;
;;   (setf mn-window (make-instance 'C-mn-window :window-title "MN"))
;;
;;
;;   (setf first-scroller (make-instance 'C-mus-not-view
;;                                       :view-container mn-window
;;                                       :view-size #@(190 270)
;;                                       :view-position #@(5 5)
;;                                       :bottom-boarder 20
;;                                       :v-scrollp nil
;;                                       :track-thumb-p t))
;;
;;   (setf second-scroller (make-instance 'C-music-notation-panel
;;                                        :view-container first-scroller
;;                                        :view-size #@(180 120)
;;                                        :view-position #@(2 2)
;;                                        :h-scrollp nil
;;                                        :track-thumb-p t
;;                                        :chord-line (make-instance 'C-chord-line)))
;;   (setf third-scroller (make-instance 'C-music-notation-panel
;;                                       :view-container first-scroller
;;                                       :view-size #@(180 120)
;;                                       :view-position #@(2 132)
;;                                       :h-scrollp nil
;;                                       :track-thumb-p t
;;                                       :chord-line (make-instance 'C-chord-line)))
;;   (setf fourth-scroller (make-instance 'C-music-notation-panel
;;                                        :view-container first-scroller
;;                                        :view-size #@(180 120)
;;                                        :view-position #@(2 262)
;;                                        :h-scrollp nil
;;                                        :track-thumb-p t
;;                                        :chord-line (make-instance 'C-chord-line)))
;;
;;   (setf (editor-objects first-scroller) (list second-scroller third-scroller fourth-scroller))
;;
;;   ;;(font-codes *music-font-spec*)
;;   ;;*font-list*
;;   ;;(real-font *music-font-spec*)
;;   ;;(font-info *music-font-spec*)
;;   ;;(view-font  second-scroller)
;;   ;; (set-view-font second-scroller *patchwork-font-spec*)
;;   ;;(font-codes *music-font-spec*)
;;
;;   ;;(set-view-size  first-scroller (make-point 400 380))
;;   ;;(set-view-size second-scroller (make-point 390 125))
;;   ;;(set-view-size third-scroller (make-point 390 125))
;;   ;;(set-view-size fourth-scroller (make-point 390 125))
;;   ;;(view-size fourth-scroller)
;;   ;;(view-size first-scroller)
;;   ;;(point-v (view-position (car (editor-objects first-scroller))))
;;   ;;(point-h (view-scroll-position second-scroller))
;;   ;;(point-v (view-scroll-position third-scroller))
;;   ;;(point-v (view-scroll-position fourth-scroller))
;;   ;;=========================================================================================================
;;   ;;=========================================================================================================
;;   (setf (chords (chord-line second-scroller)) (list
;;                                                (make-chord-object '(7000 8900 9000) 0)(make-chord-object '(4500 6900 7200) 60)
;;                                                (make-chord-object '(3400 7600 9000) 80)(make-chord-object '(4500 5400 6600) 160)
;;                                                (make-chord-object '(7000 8800 9000) 200)(make-chord-object '(4300 5400 7200) 260)))
;;
;;   (setf (chords (chord-line third-scroller)) (list
;;                                               (make-chord-object '(4500 6700 8800) 0)(make-chord-object '(3300 6900 7200) 70)
;;                                               (make-chord-object '(7000 8900 9000) 88)(make-chord-object '(4500 6900 7200) 120)
;;                                               (make-chord-object '(3400 7600 9000) 140)(make-chord-object '(4500 5400 6700) 160)
;;                                               (make-chord-object '(6500 8800 8900) 180)(make-chord-object '(3300 5400 7200) 240)))
;;
;;   (setf (chords (chord-line fourth-scroller)) (list
;;                                                (make-chord-object '(4500 6700 8800) 0)(make-chord-object '(3300 6900 7200) 70)
;;                                                (make-chord-object '(3400 7600 9000) 90)(make-chord-object '(4500 5400 6700) 110)
;;                                                (make-chord-object '(7000 8900 9000) 128)(make-chord-object '(4500 6900 7200) 170)
;;                                                (make-chord-object '(6500 8800 8900) 180)(make-chord-object '(3300 5400 7200) 290)))
;;   ;;(decompile fourth-scroller)
;;
;;   )


;;;; THE END ;;;;
