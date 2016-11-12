;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pw-mn-collector.lisp
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

(defclass  C-patch-application-midi (C-patch-application) ())

(defmethod make-application-object ((self C-patch-application-midi))
  (setf (application-object self) (make-music-notation-editor
                                   'C-mn-window-mod 'C-MN-view-mod 'C-MN-panel-Mod
                                   (make-point 350 200)
                                   *g2-g-f-f2-staffs*
                                   "Default C Patch Application Midi")))

;;====================================================================================================

;;(defvar  *global-clock* (make-instance 'C-clock))

(defclass  C-patch-midi (C-patch-application-midi C-process-begin+end)
  ((clock :initform 0  :accessor clock)
   (clock-obj :initform *global-clock* :allocation :class :accessor clock-obj)
   (chord-line-list :initform nil :accessor chord-line-list)
   (play-flag :initform nil :accessor play-flag)
   (chord-seq :initform (make-instance 'C-chord-line)
              :initarg :chord-seq :accessor chord-seq)))

(defmethod initialize-instance :after ((self C-patch-midi) &key controls)
  (declare (ignore controls))
  (setf (process self) 'continue-record)
  self)

(defmethod decompile ((self C-patch-midi))
  (append (call-next-method)
          (list nil
                `(list ,(active-mode self)
                       ,(if *decompile-chords-mode*
                            `(list (list ,@(get-chordline-form (chord-seq self)))
                                   ,(if (wptr (application-object self))
                                        `(list ,@(get-window-state self (application-object self)))
                                        `(list ,@(window-state self)))))))))

(defmethod complete-box ((self C-patch-midi) args)
  (make-pw-chord-line-box self (first args) (first (second args)))
  (if (second (second args))
      (put-window-state self (application-object self) (second (second args)))))

(defgeneric get-chordline-form (self))
(defmethod get-chordline-form ((self C-chord-line))
  (mapcar
   (lambda (chord)
     `(list ,(t-time chord) ,@(mapcar #'get-useful-note-slots (notes chord))))
   (chords self)))

(defgeneric form-to-chord-line (self chords-form))
(defmethod form-to-chord-line ((self C-chord-line) chords-form)
  (if chords-form
      (setf (chords self)
            (mapcar (lambda (form)
                      (make-instance 'C-chord
                                     :t-time (car form)
                                     :notes (apply #'form-note-objs (cdr form))))
                    chords-form))))

(defun make-pw-chord-line-box (box mode chords-form)
  (form-to-chord-line (chord-seq box) chords-form)
  (setf (active-mode box) mode)
  (if  (give-MN-editor box)
       (setf (chord-line (give-MN-editor box)) (chord-seq box))
       (ui:uiwarn "in ~S ~S,  (give-MN-editor box) is nil" 'make-pw-chord-line-box '(box mode chords-form)))
  box)

(defgeneric yourself-if-collecting (self))
(defmethod yourself-if-collecting ((self C-patch-midi)) self)

(defmethod draw-patch-extra :after ((self C-patch-midi))
  (when (play-flag self) (fill-patch-outrect (out-put self))))

(defmethod remove-yourself-control ((self C-patch-midi))
  (when (application-object self)
    (remove-yourself-control (application-object self))))

(defgeneric give-structured-begin-time (self))
(defmethod give-structured-begin-time ((self C-patch-midi))
  (give-structured-begin-time (view-window self)))

;;changed by aaa 28-08-95 from pw-modif
(defmethod play ((self C-patch-midi))
  (let ((ch-l (chord-seq self))
        notes ;begint
        )
    (when (and ch-l (chords ch-l))
      (setq notes (order-the-notes ch-l (chords ch-l) 0))
                                        ;(setq begint (abs (cdr (car notes))))
      (start (apdfuncall  10 2 15     ;beginT 2 (+ beginT 10)
                          'play-chosen-chords ch-l notes (cdr (car notes)))))))

;; (defmethod play ((self C-patch-midi))
;;   (let ((ch-l (chord-seq self))
;;         notes begint)
;;     (when (and ch-l (chords ch-l))
;;       (setq notes (order-the-notes ch-l (chords ch-l) 0))
;;       (setq beginT (abs (cdr (car notes))))
;;       (start (apdfuncall  10 2 15;beginT 2 (+ beginT 10)
;;                           'play-chosen-chords ch-l notes (cdr (car notes)))))))

(defgeneric draw-clock (self))
(defmethod draw-clock ((self C-patch-midi))
  (with-focused-view self
    (set-view-font (view-container self) '(:srccopy))
    (draw-string  52 (- (h self) 4) (format nil "~5D" (clock (clock-obj self))))
    (set-view-font (view-container self) '(:srcor))))

(defgeneric continue-record (self))
(defmethod continue-record ((self C-patch-midi))
  (when (< (clock (clock-obj self)) (+ (begin-time self) (duration-time self)))
    (if  (>= (clock (clock-obj self)) (begin-time self))
         (progn
           (draw-clock self)
           (let ((clock (- (clock (clock-obj self)) (begin-time self)))
                 (delay (patch-value (first (input-objects self)) self))
                 dur key vel chan
                 note-list instrument)
             (if (minusp delay) ; if negative delay -> a rest and no evaluation of remaining args
                 (setq delay (abs delay))
                 (progn
                   (setq dur   (patch-value (nth 1 (input-objects self)) self))
                   (setq key   (patch-value (nth 2 (input-objects self)) self))
                   (setq vel   (patch-value (nth 3 (input-objects self)) self))
                   (setq chan  (patch-value (nth 4 (input-objects self)) self))
                   (unless (eql (nth 5 (input-objects self))(nth 5 (pw-controls self)))
                     (setq instrument (patch-value (nth 5 (input-objects self)) self)))
                   (if (listp key)
                       (while key
                         (push (make-instrument-note (pop key) dur chan vel instrument (application-object self))
                               note-list))
                       (and (numberp key)
                            (setq note-list
                                  (list (make-instrument-note key dur chan vel instrument
                                                              (application-object self))))))
                   (if (or (numberp key) (listp key))
                       (push (make-instrument-chord clock (nreverse note-list))
                             (chord-line-list self))
                       (push (make-instrument-for-chord key clock instrument
                                                        (application-object self))
                             (chord-line-list self)))))
             (incf (clock self) delay)
             (dfuncall-process self delay)))
         (dfuncall-process self (begin-time self)))))

;;for taking direct input from a chord object
(defgeneric make-instrument-for-chord (self clock instrument win))
(defmethod make-instrument-for-chord ((self C-chord) clock instrument win)
  (setf (t-time self) clock)
  (dolist (note (notes self))
    (unless (instrument note) ; to check if there is already an instrument in the note
      (setf (instrument note) instrument)
      (if (and instrument win)
          (make-super-note-connections instrument note win))))
  self)

(defmethod  begin-process  ((self C-patch-midi))
  (setf (chord-line-list self) ())
  (fill-patch-outrect (out-put self))
  (setf (clock self) 0)
  (funcall (process self) self))

(defgeneric rebuild-collector-win (self))
(defmethod rebuild-collector-win ((self C-patch-midi))
  (setf (application-object self) (make-application-object self))
  (set-pw-win+pw-obj (application-object self) *active-patch-window* self))

(defmethod stop-process ((self C-patch-midi))
  (if (window-killed-p (application-object self))
      (rebuild-collector-win self))
  (let ((mus-not-panel (give-MN-editor self)))
    (remove-all-chords-from-chord-line mus-not-panel)
    (setf (chords (chord-seq self)) (nreverse (chord-line-list self)))
                                        ;(setf (chords (chord-line mus-not-panel)) (nreverse (chord-line-list self)))
    (setf (chord-line mus-not-panel) (chord-seq self))
    (view-draw-contents (application-object self)))
  (with-focused-view self
    (with-pen-state (:mode :srccopy :pattern *white-pattern*)
      (fill-rect* 50 (- (h self) 12) 33 10)))
  (draw-appl-label self #\A)
  (fill-patch-outrect (out-put self)))

;;changed by aaa 28-08-95 from pw-modif
(defmethod stop-play ((self C-patch-midi))
  (setf *MN-play-flag* nil)
  (when (play-flag self)
    (setf (play-flag self) ())
    (fill-patch-outrect (out-put self))))


;; (defmethod stop-play ((self C-patch-midi))
;;   (when (play-flag self)
;;     (setf (play-flag self) ())
;;     (fill-patch-outrect (out-put self))))

(defmethod patch-value ((self C-patch-midi) obj) (declare (ignore obj))
  (chord-seq self))
                                        ;( give-MN-editor-chord-line self 0))

(defgeneric give-MN-editor-chord-line (self i))
(defmethod give-MN-editor-chord-line ((self C-patch-midi) i)
  (declare (ignore i))
  (chord-line (give-MN-editor self)))
(defgeneric give-MN-editor (self))
(defmethod give-MN-editor ((self C-patch-midi))
  (car (editor-objects (car (subviews (application-object self))))))

;;====================
(defvar *collector-popUp-menu*
  (new-menu " "
            (new-leafmenu "Save" (lambda () (save *target-action-object*)))))

(defclass C-patch-midi-Mod (C-patch-midi)
  ((clock-obj :initform *global-clock* :allocation :class :accessor clock-obj)
   (popUpBox :initform nil :accessor popUpBox)))

(defmethod initialize-instance :after ((self C-patch-midi-Mod) &key controls)
  (declare (ignore controls))
  (setf (popUpBox self)
        (make-popUpbox "" self
                       *collector-popUp-menu*
                       :view-position (make-point (- (w self) 10)
                                                  (- (h self) 14))
                       :view-container self
                       :view-font *patchwork-font-spec*)))

(defmethod make-application-object ((self C-patch-midi-Mod))
  (setf (application-object self)
        (make-music-notation-editor 'C-mn-window-mod 'C-MN-view-mod 'C-MN-panel-Mod
                                    (make-point 350 200) *g2-g-f-f2-staffs*
                                    (pw-function-string self)))
  (setf (chord-line (give-MN-editor self)) (chord-seq self))
  (application-object self))


(defmethod rebuild-collector-win ((self C-patch-midi-Mod))
  (setf (application-object self)
        (make-music-notation-editor 'C-mn-window-mod 'C-MN-view-mod 'C-MN-panel-Mod
                                    (make-point 350 200) *g2-g-f-f2-staffs*
                                    (pw-function-string self)))
  (set-pw-win+pw-obj (application-object self) *active-patch-window* self))

;; (defmethod open-patch-win ((self C-patch-midi-Mod))
;;   (let ((win (application-object self)))
;;     (unless (and win (wptr win))
;;       (rebuild-collector-win self)
;;       (setf (chord-line (give-MN-editor self)) (chord-seq self)))
;;     (window-select (application-object self))
;;     (draw-appl-label self #\*)))

;;(defmethod open-patch-win ((self C-patch-midi-Mod)) (call-next-method))

(defmethod put-window-state ((self C-patch-midi-Mod) win state)
  (let ((mus-view (car (subviews win))))
    (setf (chord-line (give-MN-editor self)) (chord-seq self))
    (set-view-size win (first state))
    (setf (MN-zoom-scaler mus-view) (second state))
                                        ;(for (i 2 1 (third state))
                                        ;(add-new-staff-to-MN-editor mus-view))
    (set-staff-count mus-view (third state))
    (view-window-grown mus-view)))

(defmethod get-window-state ((self C-patch-midi-Mod) win)
  (let ((mus-view (car (subviews win))))
    (list (view-size win) (MN-zoom-scaler mus-view)
          (length (editor-objects mus-view)))))

(defparameter *midicent-obj-pw-type*
  (make-instance 'C-pw-type
                 :control-form
                 `(make-instance 'C-numbox  :view-size (make-point 36 14)
                                            :value 6000 :min-val 0 :max-val 12700
                                            :type-list '(fixnum list chord))))

(defgeneric polifonic? (self))
(defmethod polifonic? ((self C-patch-midi-mod)) nil)

(defmethod collect ((self C-patch-midi-mod))
  (let ((stop-time (or (ask (controls (view-container self)) 'give-stop-time)
                       *pw-default-stop-time*)))
    (set-begin-time self 0)
    (set-duration-time self stop-time)
    (init-patch self)
    (start-clock *global-clock* stop-time (list self))
    (patch-value self ())))

;;;=================
;;;

(defclass C-patch-polifMN-mod (C-patch-PolifMN)
  ((popUpBox :initform nil :accessor popUpBox)))

(defmethod initialize-instance :after ((self C-patch-PolifMN-mod) &key controls)
  (declare (ignore controls))
  (setf (popUpBox self)
        (make-popUpbox "" self
                       *collector-popUp-menu*
                       :view-position (make-point (- (w self) 10)
                                                  (- (h self) 14))
                       :view-container self
                       :view-font *patchwork-font-spec*))
  (-make-lock self (make-point (- (w self) 37) (- (h self) 9))))

;;(defmethod decompile ((self C-patch-PolifMN)) (call-next-method))

;; (defmethod decompile ((self C-patch-polifMN-mod))
;;   (append (call-next-method)
;;           (list nil `(list ,(active-mode self)
;;                           ,(if *decompile-chords-mode*
;;                             `(list
;;                               ,@(mapcar (lambda (ch-line) `(list ,@(get-chordline-form ch-line)))
;;                                         (chord-line-list self))))))))

(defmethod decompile ((self C-patch-polifMN-mod))
  (append (call-next-method)
          (list nil `(list ,(active-mode self)
                           ,(if *decompile-chords-mode*
                                `(list
                                  ,@(mapcar (lambda (ch-line)
                                              `(list ,@(get-chordline-form ch-line)))
                                            (chord-line-list self))))
                           ,(if (wptr (application-object self))
                                `(list ,@(get-window-state self (application-object self)))
                                `(list ,@(window-state self)))))))

(defmethod complete-box ((self C-patch-polifMN-mod) args)
  (make-pw-Poly-chord-line-box self (first args) (second args) (third args))
  (set-window-title (application-object self) (pw-function-string self)))

;; (defun make-pw-Poly-chord-line-box (box mode ch-line-forms state)
;;   (setf (active-mode box) mode)
;;   (mapc (lambda (ch-line chords-form editor)
;;           (form-to-chord-line ch-line chords-form)
;;           (setf (chord-line editor) ch-line))
;;         (chord-line-list box) ch-line-forms
;;         (editor-objects (car (subviews (application-object box)))))
;;   (if state (put-window-state box (application-object box) state))
;;   box)

(defun make-pw-Poly-chord-line-box (box mode ch-line-forms state)
  (setf (active-mode box) mode)
  (let ((editors (editor-objects (car (subviews (application-object box))))))
    (when (/= (length editors) (length ch-line-forms))
      (setf (chord-line-list box) nil)
      (dotimes (i (length ch-line-forms))
        (push (make-instance 'C-chord-line) (chord-line-list box)))
      (window-close (application-object box))
      (rebuild-collector-win box))
    (mapc (lambda (ch-line chords-form editor)
            (form-to-chord-line ch-line chords-form)
            (setf (chord-line editor) ch-line))
          (chord-line-list box) ch-line-forms
          (editor-objects (car (subviews (application-object box)))))
    (if state (put-window-state box (application-object box) state))
    box))

(defmethod put-window-state ((self C-patch-polifMN-mod) win state)
  (let ((mus-view (car (subviews win))))
    (set-view-size win (first state))
    (when mus-view
      (setf (MN-zoom-scaler mus-view) (second state))
      ;;(set-staff-count mus-view (third state))
      (view-window-grown mus-view))))

(defmethod get-window-state ((self C-patch-polifMN-mod) win)
  (let ((mus-view (car (subviews win))))
    (list (view-size win) (MN-zoom-scaler mus-view)
          (length (editor-objects mus-view)))))

(defmethod make-application-object ((self C-patch-polifMN-mod))
  (let ((editor
          (make-n-music-notation-editors
           (max (length (pw-controls self)) (length (chord-line-list self)))
           self
           'C-mn-window-mod 'C-MN-view-mod 'C-MN-panel-Mod
           (make-point 350 200))))
    (set-window-title editor (pw-function-string self))
    editor))

;; (defmethod patch-value ((self C-patch-polifMN-mod) obj)
;;   (if (value self)
;;       (chord-line-list self)
;;       (let* ((the-list (chord-line-list self))
;;              (controls (pw-controls self))
;;              (objects (input-objects self))
;;              (dif-len (- (length controls) (length the-list)))
;;              (new-list nil) (i 0) control)
;;         (unless (zerop dif-len)
;;           (dotimes (i dif-len)
;;             (push (make-instance 'C-chord-line) new-list))
;;           (setq the-list (nconc the-list new-list)))
;;         (dolist (object objects)
;;           (setq control (nth i controls))
;;           (unless (eql control object)
;;             (setf (nth i the-list) (patch-value object obj)))
;;           (incf i))
;;         (setf (chord-line-list self) the-list))))
;;
;; (defmethod patch-value ((self C-patch-polifMN-mod) obj)
;;   (if (value self)
;;       (chord-line-list self)
;;       (let* ((win (application-object self))
;;              (objects (input-objects self))
;;              (controls (pw-controls self))
;;              (view-object (car (subviews win)))
;;              res)
;;         (setf (chord-line-list self)
;;               (dolist (in-obj objects res)
;;                 (setq res
;;                       (append  res (list! (if (eql in-obj (pop controls))
;;                                               (make-instance 'C-chord-line)
;;                                               (patch-value in-obj obj)))))))
;;         (when (and win (wptr win)
;;                    (/= (length (chord-line-list self))
;;                        (length (editor-objects view-object))))
;;           (window-close win))
;;         (when (and win (wptr win)) (update-editor  view-object))
;;         (chord-line-list self))))

;;changed by aaa 2-10-95
(defmethod patch-value ((self C-patch-polifMN-mod) obj)
  (if (value self)
      (chord-line-list self)
      (let* ((win (application-object self))
             (objects (input-objects self))
             (controls (pw-controls self))
             (view-object (car (subviews win)))
             res)
        (setf (chord-line-list self)
              (dolist (in-obj objects res)
                (setq res
                      (append  res (list! (if (eql in-obj (pop controls))
                                              (make-instance 'C-chord-line)
                                              (patch-value in-obj obj)))))))
        (when (and win (wptr win)
                   (/= (length (chord-line-list self))
                       (length (editor-objects view-object))))
          (window-close win))
        (let ((editors (editor-objects (car (subviews (application-object self))))))
          (for (i 0 1 (1- (length editors)))
            (setf (chord-line (nth i editors)) (nth i (chord-line-list self)))))
        (when (and win (wptr win)) (update-editor  view-object))
        (chord-line-list self))))


(defmethod correct-extension-box ((self C-patch-polifMN-mod) new-box values)
  (declare (ignore values))
  (let* ((new-editors-list
           (editor-objects (car (subviews (application-object new-box)))))
         (last-ed (1- (length new-editors-list))))
    (setf (nthcdr last-ed (chord-line-list self))
          (list (chord-line (nth last-ed  new-editors-list))))
    (setf (chord-line-list new-box) (chord-line-list self))))

(defmethod set-dialog-item-text-from-dialog ((self C-patch-polifMN-mod) str)
  (declare (ignore str))
  (call-next-method)
  (let ((editor (application-object self)))
    (if (and editor (wptr editor))
        (set-window-title editor (pw-function-string self)))))

(defmethod give-new-extended-title ((self C-patch-polifMN-mod))
  (pw-function-string self))

(defmethod open-patch-win ((self C-patch-polifMN-mod))
  (if (and (application-object self)
           (window-killed-p (application-object self)))
      (rebuild-collector-win self))
  (let ((editors (editor-objects (car (subviews (application-object self))))))
    (for (i 0 1 (1- (length editors)))
      (setf (chord-line (nth i editors)) (nth i (chord-line-list self)))))
  (window-select (application-object self))
  (draw-appl-label self #\*))

(defmethod rebuild-collector-win ((self C-patch-polifMN-mod))
  (setf (application-object self) (make-application-object self))
  (set-window-title (application-object self) (pw-function-string self))
  (set-pw-win+pw-obj (application-object self) *active-patch-window* self))

(defmethod polifonic? ((self C-patch-polifMn-mod)) t)

;;add by aaa 28-08-95 from pw-modif
(defmethod play ((self C-patch-polifMN-mod))
  (play-all-staffs (car (subviews (application-object self)))))



;;add by aaa 28-08-95 from pw-modif
(defmethod stop-play ((self C-patch-polifMN-mod))
  (setf *MN-play-flag* nil)
  (stop-all-staffs (car (subviews (application-object self)))))


;;==================

(defunp collector ((del (fix>0 (:value 100))) (dur (fix>0 (:value 75)))
                   (mid/ob (midic (:type-list (fixnum list chord))))
                   (vel (midic (:value 100))) (chan approx)
                   (ins (symbol (:type-list (midi-ins) :dialog-item-text "ins"))))
    collector
    "A PW-box that is able to play in real-time (p - play)
or to collect in non-real-time (c - collect) notes.
When the user plays or records,the collector-box begins to
evaluate its inputs in a loop.
The inputs
- del (when the next note is to be evaluated)
      (if del is negative then the evaluation of all
       other arguments is skipped and no pitches are played
       or recorded - this is an easy way of making rests)
- dur (duration)
- midic (midi-keynumber * 100
         -> 6000 = middle-C, 6050 = middle-C + 50 cents)
        (can be a midic or a list of midics or a chord object)
- vel (velocity)
- chan (channel)
- m-ins (midi-instrument - a collection of break-point-functions
         or fix-values)
        (this input works only in non-real-time mode)
are evaluated each time a note is played or collected.
The collector-box can be stopped in real-time by pressing s.
In non-real-time the process is stopped at a time-point
that can be set with a stime-box (stop-time-box).
This value is set by default to 1000 ticks (10 seconds)
\(the default value will be used if there is no stime-box in the
current PW-window).
The collector-box also owns a music-notation-editor (MN-editor) that can
be opened by selecting the collector-box and pressing o.
When collecting notes in non-real-time ,
all notes are collected inside this editor and each
aspect of the notes can later be edited.
When requested for a value, collector-box returns a chord line object.
"
  (declare (ignore del dur mid/ob vel chan ins)))
;;===================================

(defunp poly-coll ((coll1 (symbol (:dialog-item-text "obj" :type-list (collector))))
                   &rest (colln (symbol (:dialog-item-text "obj" :type-list (collector)))))
    list
    "A pmnn (polifonic MN-editor) box is used to represent
polifonic information.
For example if you have recorded a process with three collectors,
you can look at the result with three staffs by selecting
a Poly Collector from the Music edit menu and extending it
and connecting each output of the
three collectors to each input of the Poly Collector box.
Then you have to make a request by option-clicking the out-put-box
of the Poly Collector box.
This box can be both opened by selecting it and pressing o from
the keyboard or extended by option-clicking bottom-right
\(EA = extend+application)."
  (declare (ignore coll1 colln)))

(defunp multiseq ((coll1 (symbol (:dialog-item-text "obj" :type-list (collector list))))
                  &rest (colln (symbol (:dialog-item-text "obj" :type-list (collector list)))))
    list
    "A <multiseq> (polyphonic sequence) box represents polyphonic data. It takes
one or more chord sequence objects as input and returns them in a list. This
module works with the associated music notation editor, which displays as
many systems as chord sequence inputs have been defined for the module.
The multiseq module is extensible. Option-clicking on it (but not on its output
box!) adds a new chord sequence input. A popup menu is linked to the letter 'A'
just to the right of the output box. It offers the option of saving the module with
all its chord sequences into a file. The multiseq module is state preserving. It
only changes its output if there is a module connected to one of its inputs (or if
changes are made by hand in the editor, of course). This box can be opened by
selecting it and pressing 'o' from the keyboard or extended by option-clicking
bottom-right. Type 'h' with the music notation editor opened for more
information."
  (declare (ignore coll1 colln)))

;;======================================

;;(defvar *pw-stop-time* 1000)

(defclass C-pw-stop-time (C-patch) ())

;;(defmethod patch-value ((self C-pw-stop-time) obj) (patch-value (car (input-objects self)) obj))

(defgeneric give-stop-time (self))
(defmethod give-stop-time ((self C-patch)))
(defmethod give-stop-time ((self C-pw-stop-time)) (patch-value self ()))

(defunp stime ((stopt (fix (:value 1000)))) fix
    "This box sets the stop-time when collecting with collector-boxes
- by default 1000 ticks (10 seconds)."
  stopt)


;;;; THE END ;;;;
