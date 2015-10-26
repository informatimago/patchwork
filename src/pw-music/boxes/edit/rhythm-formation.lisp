;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               rhythm-formation.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;    
;;;;    PW rythmic input.
;;;;    
;;;;AUTHORS
;;;;    Camilo Rueda
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-07 <PJB> Changed license to GPL3; Added this header.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright IRCAM 1986 - 1992
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

(defvar *valid-rtm-expands* '(#\/))

(defun expand-lists (lists)  
  (and lists
       (let ((lists (expand-lst lists))  result)
         (while lists
           (let ((next-elem (pop lists)))
             (cond 
               ((symbolp next-elem)
                (let* ((form (coerce (format () "~A" next-elem) 'list))
                       (from-char (is-in form *valid-rtm-expands*))
                       (char-symb (car from-char))
                       (third (cdr from-char))
                       (int (butlast form (length from-char))))
                  (cond
                    ((and char-symb (char= #\/ char-symb)
                          (or (not third) 
                              (and (char= (car third) #\/) (pop third) third
                                   (numberp (setq third
                                                  (read-from-string (coerce third 'string)))))
                              (not third)
                              (numberp (setq third
                                             (read-from-string (coerce third 'string)))))
                          (or (not int)
                              (numberp (setq int (read-from-string (coerce int 'string))))))
                     (push (list
                            (list (or int 1)
                                  (if third (make-list third :initial-element 1)
                                      (expand-lists (pop lists));;;(pop lists)
                                      ))) result))
                    (t (push (list next-elem) result)))))
               ((and (rationalp next-elem) (> (denominator next-elem) 1))
                (push (list
                       (list (abs (numerator next-elem))
                             (make-list (denominator next-elem)
                                        :initial-element (/ (abs (numerator next-elem)) (numerator next-elem)) ;1
                                        ))) result))
               (t (push (list next-elem) result)))))
         (apply #'append (nreverse result)))))

;;(expand-lists '(2*(/3 3*(2/(1 2))) 2*(/9) /(1 2 3 1 1) 2//8 1.0))

(defunp score-voice ((signs list (:value "(4 4)"))
                     (beats list (:value "((4 (1 1 1 1)))")) 
                     (chords list (:value "(6000)" 
                                   :type-list (list fixnum chord note-obj)))
                     (tempi midics? (:value 60))
                     &optional (objs list (:value '()))) measure-line
    "see rtm box"
  (declare (ignore signs beats chords objs tempi)))

(defunp rtm ((signs list (:value "(4 4)"))
             (beats list (:value "((4 (1 1 1 1)))")) 
             (chords list (:value "(6000)" 
                           :type-list (list fixnum chord note-obj collector)))
             (tempi midics? (:value 60))
             &optional (objs list (:value '() :type-list ()))) measure-line
    "makes a measure object out of the input rhythm, where 
<signs> is a list of time signatures,
<beats> is a list of beat divisions in expand list notation.
For example '(3*(1/4) /(1 2 1) 2//4 2*(1/5 1/7 1/8)) takes:
three times: four 16ths notes,
 one time: a 8ths, a 16ths and a 8ths note,
 one time four  eighth notes  and
 two times: five 16ths notes (in a beat) seven 16ths notes (in a beat)
and eight 32ths notes (in a beat).
<chord> is a list (or list of lists) of midics, and
<tempi> is a list of tempos  in expand list notation (see <expand-list>). The 
module can be locked (to avoid new evaluations of the patch that is under 
'chord') to keep its contents by clicking on the small ‘o’ in the lower right of the 
module. The ‘o’ indicates that the module is open. An editor for the <score-
voice> object is entered either by selecting the module and typing the letter 'o',  
or by double-clicking on the module's name. Click 'h' with the rhythm-notation-
editor opened for more information."
  (declare (ignore signs beats chords objs tempi)))

(defclass C-patch-score-voice (C-patch-application-rtm-editor)
  ((popUpbox :initform nil :accessor popUpbox)))

(defmethod initialize-instance :after ((self C-patch-score-voice) &key controls)
  (declare (ignore controls))
  (setf (popUpBox self) 
        (make-popUpbox "" self
                       *collector-popUp-menu*
                       :view-position (make-point (- (w self) 10)
                                                  (- (h self) 14))
                       :view-container self
                       :view-font *patchwork-font-spec*)))

(defmethod yourself-if-collecting ((self C-patch-score-voice)) nil)

(defmethod draw-patch-extra ((self C-patch-score-voice))
  (call-next-method)
  (draw-char (+ -15 (w self)) (- (h self) 4) #\E))

(defgeneric get-measure-object (self))
(defgeneric build-a-chord (self))
(defgeneric build-a-note (self))

(defmethod get-measure-object ((self cons))
  (mapcar #'get-measure-object self))

(defmethod get-measure-object ((self C-measure-line)) (measures self))

(defmethod get-measure-object ((self C-measure)) self)

(defmethod get-measure-object ((self t))
  (error "invalid input ~S to 'rtm' module" self))

(defmethod patch-value ((self C-patch-score-voice) obj)
  (if (value self)
                                        ;add by aaa 2-10-95
                                        ;(measure-line self)
      (progn       (rtm-dim (measure-line self) 7) ;;;to be refined for keeping note durs!!
                   (measure-line self))
      (let* ((inputs (input-objects self))
             (objs (and (fifth inputs) (patch-value (fifth inputs) obj)))
             (in-chords (patch-value (third inputs) obj))
             (tempi (expand-lists (patch-value (fourth inputs) obj)))
             (chords (if (subtypep (type-of in-chords) 'C-chord-line)
                         (chords in-chords)  (construct-chords  (list! in-chords))))
             (measure-line (measure-line self))
             measures chord-objects)
        (if objs 
            (setq measures (get-measure-object objs))
            (let* ((in-signs (patch-value (first inputs) obj))
                   (beats (expand-lists (patch-value (second inputs) obj)))
                   (x-signs (expand-lists in-signs))
                   (signs (if (consp (car x-signs)) x-signs (list x-signs)))
                   (default-sign (car (last signs)))
                   (default-beat (car (last beats)))
                   (def-tempo (car (last tempi)))
                   (meas-beat-count (caar signs))
                   beat-objs
                   (meas-sign (pop signs))
                   the-beat already-warned)
              (when (and (numberp default-beat) (minusp default-beat))
                (setq default-beat (abs default-beat)))
              (while (or (setq the-beat (pop beats)) chords)
                (unless the-beat
                  (setq the-beat default-beat))
                (cond ( ; (zerop meas-beat-count)

                                        ; ----- GAS 110493. alllow for small rounding error
                       (or (zerop meas-beat-count) (< (abs meas-beat-count) 1.0e-10))
                       (setf meas-beat-count 0)
                                        ; ----- GAS 110493. alllow for small rounding error

                       (push (make-measure (low-of meas-sign) (nreverse beat-objs) 
                                           (or (pop tempi) def-tempo)) measures)
                       (setq meas-sign (or (pop signs) default-sign) meas-beat-count (car meas-sign)
                             beat-objs nil))
                      ((minusp meas-beat-count)
                       (if (cdr signs)
                           (error "measures ~S and beats do not agree" meas-sign)
                           (progn
                             (unless already-warned
                               (ui:ed-beep)
                               (ui:uiwarn "measures ~S and beats do not agree. Measure(s) will be changed" meas-sign)
                               (setq already-warned t))
                             (push (make-measure (low-of meas-sign) (nreverse beat-objs) 
                                                 (or (pop tempi) def-tempo)) measures)
                             (setq meas-sign (or (pop signs) default-sign) meas-beat-count (car meas-sign)
                                   beat-objs nil)))))
                (push (multiple-value-bind (beat rest-chords)
                          (beat-constructor (beats-of the-beat) (division-of the-beat) chords)
                        (setq chords rest-chords) beat) beat-objs)
                (decf meas-beat-count (beats-of the-beat)))
              (if beat-objs (push (make-measure (low-of meas-sign) (nreverse beat-objs) def-tempo) measures))
              (setq measures (nreverse measures))))
        (when (not (wptr (application-object self)))
          (setf (application-object self) (make-application-object self))
          (put-window-state self (application-object self) (window-state self)))
        (unless (listp measures) (setq measures (list measures)))
        (setf (measures measure-line) measures)
        (if objs
            (progn
              (setq chord-objects 
                    (ask-all (collect-all-chord-beat-leafs (measure-line self)) 'beat-chord))
              (setq chords (put-in-grace-notes (extra-measure-stuff (car measures)) chords))
              (dolist (chord chords)
                (unless chord-objects (return nil))
                (setf (notes (car chord-objects)) (notes chord))
                (update-chord (pop chord-objects)))))
        (rtm-dim measure-line 7) ;;;to be refined for keeping note durs!!
        (with-focused-view (application-object self) 
          (erase+view-draw-contents (application-object self)))
        (make-instance 'C-measure-line :measures (measures measure-line))
        )))

;;; version corrigée du 21/6/94. GA/CR. Dans Rythm-Formation.lisp
;;changed by aaa 28-08-95 from pw-modif
(defun put-in-grace-notes (grace-notes chords) ;(print grace-notes)
  (let (piece-chord res sorted-grace-notes pivot-chord needed-chords
                    (max-grace (and grace-notes (apply 'max (mapcar #'first grace-notes))))
                    )
    (when  max-grace
      (setf needed-chords (+ 1 max-grace (length grace-notes)))
      (when (> needed-chords (length chords))
        (setq chords 
              (append chords (dotimes (i (- needed-chords (length chords)) res)
                               (push (build-a-chord '(6000)) res)))
              )))
    (setf sorted-grace-notes
          (loop 
            while grace-notes
            for current-pos = (caar grace-notes) 
            collect
            (reverse (loop for grace in (reverse 
                                         (loop
                                           while (and grace-notes (= (caar grace-notes) current-pos))
                                           collect (pop grace-notes)))
                           with counter = 0 
                           collect (cons (first grace) (+ (prog1 counter (incf counter (cdr grace))) (cdr grace)))))))
    (dolist (set-of-graces sorted-grace-notes)
      (setq pivot-chord (nth (+ (length set-of-graces) (caar set-of-graces)) chords))
      (dolist (pair set-of-graces)
        (setq piece-chord (nth (first pair) chords)) 
        (mapc (lambda (note)
                (setf (offset-time note) (- (floor (cdr pair))))
                (setf (dur note) (floor (cdr pair)))
                (add-new-note pivot-chord note))
              (notes  piece-chord))
        (update-chord  pivot-chord)
        (setq chords (remove piece-chord chords))))
    chords))

;;add by aaa 28-08-95 from pw-modif
(defmethod stop-play ((self C-patch-score-voice)) 
  (start 
    (tell (ask-all (beat-editors (car (subviews (application-object self))))
                   'measure-line) 'stop-measure-line)))

(defun construct-chords (chords)
  (mapcar #'build-a-chord chords))

(defmethod build-a-chord ((self number))
  (make-instance 'C-chord :notes (list (build-a-note self))))

(defmethod build-a-chord ((self C-chord)) self)

(defmethod build-a-chord ((self C-note))
  (make-instance 'C-chord :notes (list self)))

(defmethod build-a-chord ((self cons))
  (make-instance 'C-chord :notes (mapcar #'build-a-note self)))

(defmethod build-a-note ((self number))
  (make-instance 'C-note :midic self))

(defmethod build-a-note ((self C-note)) self)

(defun expand-signs (signs)
  (let ((repet (second (car signs))))
    (if (consp repet)
        (if (consp (car repet))
            (append (apply #'append (make-list (caar signs) :initial-element repet))
                    (cdr signs))
            (append (make-list (caar signs) :initial-element repet) (cdr signs)))
        signs)))

(defun beats-of (sign) (if (consp sign) (first sign) (abs sign)))

(defun division-of (beat) (if (consp beat) (second beat) (if (minusp beat) '(-1)
                                                             (if (fixnump beat) '(1) '(1.0)))))

(defun low-of (sign) (second sign))

(defun rtm-tree (measure-lines)
  (mapcar (lambda (m-line) 
            (epw::flat-once
             (mapcar 
              (lambda (beats-in-meas) 
                (mapcar (lambda (beat) 
                          (let ((form (decompile beat)))
                            (list (second form) (eval (third form)))))                                   
                        beats-in-meas))
              (get-all-slots (get-all-slots m-line 'measures) 'beat-objects))))
          measure-lines))

(defun get-all-slots (objs slot)
  (if (consp objs)
      (mapcar (lambda (obj) (slot-value obj slot)) objs)
      (slot-value objs slot)))

(defun rtm-dels (measure-lines)
  (mapcar (lambda (m-line)
            (pw::calc-t-time-measure-line m-line 1.0)
            (epw::x->dx (ask-all
                         (ask-all (collect-all-chord-beat-leafs m-line) #'beat-chord)
                         #'t-time)))
          measure-lines))

;;changed by aaa le 26-09-95
(defun rtm-durs (measure-lines)
  (mapcar (lambda (m-line)
            (calc-t-time-measure-line m-line 1.0)
            (mapcar (lambda (notes) (dur (car notes)))
                    (ask-all
                     (ask-all (collect-all-chord-beat-leafs m-line) #'beat-chord)
                     #'notes)))
          measure-lines))


(defun rtm-signs (measure-lines)
  (mapcar (lambda (m-line) 
            (mapcar (lambda (measure)
                      (list (apply '+ (ask-all (beat-objects measure) 
                                               #'unit-length))
                            (read-from-string (low measure))))
                    (measures m-line)))
          measure-lines))

(defun rtm-tempo (measure-lines)
  (mapcar 
   (lambda (m-line) 
     (mapcar (lambda (measure) (metronome measure))
             (measures m-line)))
   measure-lines))

(defun rtm-chords (measure-lines)
  (mapcar (lambda (m-line) 
            (pw::calc-t-time-measure-line m-line 1.0)
            (get-rchords m-line))
          measure-lines))

(defunp rtm-dim ((m-lines list (:value '() :type-list (list measure-line)))
                 (dimension menu 
                            (:menu-box-list (("tree" . 1) ("delay". 2)
                                             ("sign" . 3) ("tempo" . 4) ("chords" . 5)
                                             ("durs" . 7))
                             :type-list (no-connection)))) list
    "<m-lines> input can be a measure-line object or a list of them. Dimension can 
be:
<tree>, 'delay' 'sign', 'tempo' or 'chords'. The corresponding output is:
<tree>:  The list of rhythm beat trees for each measure-line.
<delay>: The list of delays for each measure-line
<dur>:   The list of beat durations for each measure-line
<sign>: The list of measure signatures for each measure-line
<tempo>: The list of measure tempi for each measure-line. "

  (and m-lines
       (let* ((meas-lines (pw::list! m-lines))
              (result
                (case dimension
                  (1 (rtm-tree meas-lines))
                  (2 (rtm-dels meas-lines))
                  (3 (rtm-signs meas-lines))
                  (4 (rtm-tempo meas-lines))
                  (5 (rtm-chords meas-lines))
                  (7 (rtm-durs meas-lines)))))
         (if (consp m-lines) result (car result)))))


(defmethod C-get-note-slots::get-note-dimensions ((self pw::C-measure-line) the-slots &optional include?)
  (if include?
      (mapcar (lambda (chord t-time) (list t-time chord))
              (C-get-note-slots::get-note-dimensions (first (pw::rtm-chords (list self))) the-slots)
              (epw::dx->x 0 (first (pw::rtm-dels (list self)))))
      (C-get-note-slots::get-note-dimensions (first (pw::rtm-chords (list self))) the-slots)))


;;;; THE END ;;;;;
