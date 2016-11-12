;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               mn-note-chord-chordline.lisp
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

;;=================================================================================================
(defclass C-scale ()
   ((alteration-vector :initform nil :initarg :alteration-vector :accessor alteration-vector)
    (diatone-vector    :initform nil :initarg :diatone-vector    :accessor diatone-vector)
    (approx-factor :initform 100 :initarg :approx-factor :reader approx-factor)))

(defgeneric give-alteration (self midic))
(defmethod give-alteration  ((self C-scale) midic)
  (let* ((vlength (length (diatone-vector self)))
         (int-cents (mod midic 1200))
         (index (round (/ int-cents (approx-factor self) )))
         (up-octave (truncate (/ index (length (diatone-vector self))))))
    (setq index (mod index (length (diatone-vector self))))
    (if (and (= (approx-factor self) 25) (= index (1- vlength))) (incf up-octave))
    (list (svref  (diatone-vector self) index) (svref  (alteration-vector self) index) up-octave)))

(defparameter *chromatic-scale* (make-instance 'C-scale 
   :alteration-vector
;;          c   c#     d  d#   e     f   f#    g   g#   a   a#    h
  (vector  () #\Y    () #\Y  ()    ()  #\Y   ()  #\Y   ()  #\Y  ())
   :diatone-vector
  (vector  0   0      1  1     2     3   3     4   4    5     5   6)
  :approx-factor 100))

(defparameter *c-major-scale* (make-instance 'C-scale 
   :alteration-vector
;;          c   c#     d  es    e     f   f#    g   as   a   b    h
  (vector  () #\Y    () #\I   ()    ()  #\Y   ()  #\I   ()  #\I  ())
   :diatone-vector
  (vector  0   0      1   2     2     3   3     4   5    5   6   6)
  :approx-factor 100))

(defparameter *1/4-tone-chromatic-scale* (make-instance 'C-scale 
    :alteration-vector
;;          c  c+  c#  c++    d  d+  d#  d++   e  e+    f  f+   f#  f++      g  g+  g#  g++ 
  (vector  () #\y #\Y #\L    () #\y #\Y  #\L  () #\y   () #\y  #\Y #\L     () #\y  #\Y #\L 
;;           a   a+  a#  a++   h  h+ 
           ()  #\y  #\Y #\L   () #\y)
   :diatone-vector
  (vector  0   0    0  0      1   1   1   1    2   2    3   3   3   3       4   4    4   4
            5    5   5   5    6   6)
  :approx-factor 50))

;
(defparameter *1/8-tone-chromatic-scale* (make-instance 'C-scale 
    :alteration-vector
;;          c   c^  c+  c+!  c#  c#^ c++  C++!    d   d!      d+  d+! d#  d#^ d++  eb+ e  e!  e+   e+!   
  (vector  () #\Z #\y  #\u  #\Y #\U #\L   #\z ()   #\Z    #\y #\u #\Y #\U #\L  #\z  () #\Z #\y  #\z 
;;           f  f!  f+   f+! f#  f#^ f++  f++!   g  g^ g+  g+! g#  g#^ g++ ab+
           () #\Z #\y  #\u #\Y  #\U #\L #\z  () #\Z #\y #\u #\Y #\U #\L #\z
;;           a  a^ a+   a+! a#  a#^ a++  a++!  h h^   h+ h+!
           () #\Z #\y #\u #\Y #\U #\L  #\z () #\Z #\y  #\z ) ;#\u)        ;#\302
   :diatone-vector
  (vector  0   0    0  0   0  0 0 1  1   1   1   1  1 1 1  2   2  2 2 3  3   3   3   3  3 3 3 4  4   4    4   4
           4 4 4 5  5  5   5   5  5 5 5 6  6   6 6 0)
  :approx-factor 25))

(defparameter *standard-music-notation-scale* *c-major-scale*)
;;(setq *standard-music-notation-scale* *1/4-tone-chromatic-scale*)
(defparameter *current-music-notation-scale* *c-major-scale*)         ;the scale selected by the user
(defparameter *current-approx-scale* *1/4-tone-chromatic-scale*)      ; the scale for user selected approximations
;;(setf *current-approx-scale* *c-major-scale*)
;;================================

(defclass C-note ()
   ((midic :initform 6000 :initarg :midic :accessor midic)
    (diatone :initform nil :initarg :diatone :accessor diatone)
    (alteration :initform nil :initarg :alteration :accessor alteration)
    (delta-x :initform 0 :initarg :delta-x :accessor delta-x)
    (alt-delta-x :initform 0 :initarg :alt-delta-x :accessor alt-delta-x)
    (dur :initform 100 :initarg :dur :accessor dur)
    (vel :initform 100 :initarg :vel :accessor vel)
    (chan :initform 1 :initarg :chan :accessor chan)
    (instrument :initform nil :initarg :instrument :accessor instrument)
    (offset :initform 0 :initarg :offset-time :accessor offset-time)      ;time offset from chord (if any)
    (order  :initform 0 :initarg :order :accessor order)                   ;order in the chord (if any)
    (arp-view-x :initform 0 :initarg :arp-view-x :accessor arp-view-x)    ;the note x-offset in arpeggio view
    (comm :initform nil :initarg :comm :accessor comm)
    (extra :initform nil :accessor extra)))

;; more compact 

(defmethod decompile ((self C-note))
  `(make-C-note
       ,(midic self) ,(diatone self) ,(alteration self)
       ,(dur self) ,(vel self) ,(chan self)
       ,(when (instrument self)(decompile (instrument self)))
       ,(offset-time self)
       ,(order self)
       ,(comm self)))


(defun make-C-note (midic diatone alteration dur vel chan &optional instrument offset order comm)
  (let ((note (make-instance 'C-note 
           :midic midic :diatone  diatone :alteration alteration
           :dur  dur :vel vel :chan chan
           :instrument instrument
           :offset-time offset
           :order order
           :comm comm
           )))
    (when instrument
      (make-super-note-connections instrument note *current-MN-window*))
    note))

;;(make-C-note 6000 34 nil 100 120 1)

(defmethod initialize-instance :after  ((self C-note) &key midic)
  (declare (ignore midic))
  (when (not (alteration self))
    (update-note self)))

(defgeneric make-a-copy (self))
(defmethod make-a-copy ((self C-note))
  (make-C-note 
   (midic self) (diatone self) (alteration self)
   (dur self) (vel self) (chan self)
   (when (instrument self)(eval (decompile (instrument self))))
   (offset-time self)
   (order self)
   (comm self)))

;;Camilo 26/3/91

(defgeneric update-note (self))
(defmethod update-note ((self C-note))
  (let* ((scale-change? 
           (zerop (mod (* (approx-factor *current-approx-scale*)
                          (round (mod (midic self) 100)
                                 (approx-factor *current-approx-scale*))) 100)))
         (dia-alt (give-alteration 
                   (if scale-change?  *current-music-notation-scale* *current-approx-scale*) 
                   (midic self))))
    (setf (diatone self) (+ (* 7 (+ (truncate (/ (midic self) 1200)) (third dia-alt))) (first dia-alt)))
    (setf (alteration self) (second dia-alt))))

;
;; 35 = diatone of C5
;; 2 = 1 ditone in pixels

(defgeneric give-pixel-y (self C5))
(defmethod give-pixel-y  ((self C-note) C5)
  (+ 1 (- C5 (* 2 (- (diatone self) 35)))))

(defgeneric transpose-note (self cents))
(defmethod transpose-note ((self C-note) cents)
  (setf (midic self) (+ (midic self) cents))
  (update-note self))
 

;;___________________________
;;___________________________
;; instrument

(defgeneric open-instrument-editor (self win x y))
(defmethod open-instrument-editor ((self C-note) win x y)
  (if (instrument self) 
      (progn 
        (setq *global-selected-note* self)
        (open-instrument-editor (instrument self) win x y))
      (ui:ed-beep)))

(defgeneric remove-instrument-item (self x y))
(defmethod remove-instrument-item ((self C-note) x y)
  (when (instrument self) 
    (setq *global-selected-note* self)
    (remove-instrument-item (instrument self) x y)))

;;___________________________
(defgeneric draw-note-4 (self x C5 t-scfactor))
(defmethod draw-note-4  ((self C-note) x C5 t-scfactor)
  (declare (special *mn-view-time-flag*))
  (let ((y-now (give-pixel-y self C5))
        (x-now (+ x (delta-x self)))
        (alt (alteration self)))
    (draw-char x-now y-now #\w)
    (when t-scfactor
      (if *mn-view-dyn-flag*
          (draw-note-symbolic-dynamic self x-now y-now))
      (if *mn-view-dur-flag*
          (draw-note-duration-line self x-now y-now t-scfactor))
      (if (and *mn-view-offset-flag* (not *mn-view-time-flag*))
          (draw-note-offset-line self x-now y-now t-scfactor)) ) 
    (when (and (instrument self) *mn-view-ins-flag* t-scfactor) 
      (draw-char x-now y-now #\Ω)
      (draw-instrument (instrument self) x-now y-now (round (+ (* t-scfactor (dur self))))))
    (if (and alt (not (eql *staff-num* 7) )) ; empty staff
        (draw-char (+ x (alt-delta-x self)) y-now alt))))

(defgeneric draw-note-duration-line (self x1 y-now t-scfactor))
(defmethod draw-note-duration-line ((self C-note) x1 y-now t-scfactor)
  (draw-line (+ 3 x1) (- y-now 2)
             (round (+ (* t-scfactor (dur self)) (+ 3 x1))) (- y-now 2)))

;; used when editing

(defgeneric draw-note-duration-line-xor (self x C5 t-scfactor))
(defmethod draw-note-duration-line-xor ((self C-note) x C5 t-scfactor)
  (let ((y-now (give-pixel-y self C5))
        (x-now (+ x (delta-x self))))
    (with-pen-state  (:mode :patxor) 
      (draw-line (+ 3 x-now) (- y-now 2)
                 (round (+ (* t-scfactor (dur self)) (+ 3 x-now))) (- y-now 2)))))

(defgeneric draw-note-symbolic-dynamic-xor (self x C5 t-scfactor))
(defmethod draw-note-symbolic-dynamic-xor ((self C-note) x C5 t-scfactor)
  (declare (ignore t-scfactor))
  (let ((y-now (give-pixel-y self C5))
        (x-now (+ x (delta-x self))))
    (if *mn-view-arp-flag*
        (draw-char (- x-now 2) (+ y-now 8) (map-to-note-symbolic-dynamic self))
        (draw-char (+ 10 x-now) y-now  (map-to-note-symbolic-dynamic self)))))

(defgeneric draw-note-offset-line (self x1 y-now t-scfactor))
(defmethod draw-note-offset-line ((self C-note) x1 y-now t-scfactor)
  (draw-line (+ 3 x1) (- y-now 2)
             (round (+ (* t-scfactor (offset-time self)) (+ 3 x1))) (- y-now 2)))

(defgeneric draw-note-offset-line-xor (self x C5 t-scfactor))
(defmethod draw-note-offset-line-xor ((self C-note) x C5 t-scfactor)
  (let ((y-now (give-pixel-y self C5))
        (x-now (+ x (delta-x self))))
    (with-pen-state (:mode :patxor) 
      (draw-line (+ 3 x-now) (- y-now 2)
                 (round (+ (* t-scfactor (offset-time self)) (+ 3 x-now))) (- y-now 2)))))

;;__

(defgeneric map-to-note-symbolic-dynamic (self))
(defmethod map-to-note-symbolic-dynamic ((self C-note))
  (cond 
    ((< (vel self) 60) #\π)
    ((< (vel self) 75) #\p)
    ((< (vel self) 90) #\P)
    ((< (vel self) 105) #\F)
    ((< (vel self) 115) #\f)
    (t #\ƒ)))

(defgeneric draw-note-symbolic-dynamic (self x y-now))
(defmethod draw-note-symbolic-dynamic ((self C-note) x y-now)
  (if *mn-view-arp-flag*
      (draw-char (- x 2) (+ y-now 8) (map-to-note-symbolic-dynamic self))
      (draw-char (+ 10 x) y-now  (map-to-note-symbolic-dynamic self))))

(defgeneric inside-note\?-3 (self mouse-x chord-x y-val))
(defmethod inside-note?-3 ((self C-note) mouse-x chord-x y-val)
  (when (and (eql y-val (diatone self)) 
             (< (- (+ chord-x (delta-x self)) 3) mouse-x 
                (+ (+ chord-x (delta-x self)) 5)))
    self))

(defgeneric draw-note-channel (self x y))
(defmethod draw-note-channel ((self C-note) x y)
  (draw-string x y (format () "~D" (chan self))))

#|
(defmethod play-note ((self C-note))
  (when (instrument self) (play-instrument (instrument self) (chan self)(dur self)))
  (write-midicent-note (dur self) (chan self) (midic self) (vel self))) 
|#

(defgeneric play-note (self &optional approx))
(defmethod play-note ((self C-note) &optional approx)
  (if (instrument self) 
      (play-instrument (instrument self) self)
      (write-midicent-note (dur self) (chan self) 
                           (if approx (epw::approx-m (midic self) approx) (midic self))
                           (vel self))))

;;============================================

(defun make-chord-object (midics t-time &optional chord-object)
  (if  (eql (type-of (car midics))  chord-object)
    (progn (setf (t-time (car midics)) t-time) (car midics))
    (let ((order -1) notes)
      (dolist (pitch midics)
        (push (make-instance 'C-note 
                        :midic pitch :offset-time 0
                        :dur 100 :order (incf order) :vel 100)
              notes))
      (make-instance 'C-chord :notes (sort notes '< :key #'midic) :t-time t-time))))

(defun a-list-p (the-list)
  (listp (car the-list)))

(defclass C-chord ()
   ((t-time :initform 0 :initarg :t-time :accessor t-time)
    (notes  :initform nil :initarg :notes :accessor notes)
    (extra :initform nil :accessor extra)))

;; more compact 

(defmethod decompile ((self C-chord))
  `(make-C-chord ,(t-time self) (list ,@(ask-all (notes self) 'decompile)))) 

(defun make-C-chord (t-time notes)
  (make-instance 'C-chord :t-time t-time :notes notes))

;;(make-C-chord 0 (list  (make-C-note 6000 34 nil 60 99 1)(make-C-note 6000 34 nil 60 99 1)))

(defmethod initialize-instance :after  ((self C-chord) &key notes)
  (declare (ignore notes))
  (update-chord self))

(defgeneric make-copy (self t-time))
(defmethod make-copy ((self C-chord) t-time)
  (make-C-chord t-time (ask-all (notes self) 'make-a-copy)))

(defgeneric play-chord (self))
(defmethod play-chord ((self C-chord))
  (cond ((eql *playing-option* :pb) (tell (notes self) 'play-note))
        ((eql *playing-option* :mc)
         (let ((notes (notes self))
               approx-m)
           (dolist (note notes)
             (setq approx-m (approx-for-playing (midic note)))
             (write-midi-note (dur note) (+ (chan note) (micro-channel approx-m) -1)
                              (truncate approx-m 100) (vel note)))))))

(defgeneric add-new-note (self note))
(defmethod add-new-note ((self C-chord) note)
  (push note (notes self))
  (update-chord self))

(defgeneric remove-note (self note))
(defmethod remove-note ((self C-chord) note)
  (remove-instrument-item note ()())
  (setf (notes self) (remove note (notes self) :test 'eq))
  (update-chord self))

(defgeneric kill-notes (self))
(defmethod kill-notes ((self C-chord))
  (tell (notes self) 'remove-instrument-item ()()))
;;__________

(defgeneric calc-chord-pixel-x (self t-scfactor beg-x time1))
(defmethod calc-chord-pixel-x ((self C-chord) t-scfactor beg-x time1)
  (+ beg-x (round (* t-scfactor (- (t-time self) time1)))))

;;__________
;; (reverse (notes self)) ->  because of drawing of instrument information

(defgeneric give-all-draw-notes (self))
(defmethod give-all-draw-notes  ((self C-chord))
  (reverse (notes self)))

(defgeneric draw-chord (self t-scfactor beg-x time1 C5 &optional mode))
(defmethod draw-chord  ((self C-chord) t-scfactor beg-x time1 C5 &optional mode)
  (setq *MN-note-ins-y* *MN-global-ins-y*)
  (when (notes self)
    (let ((x-now (calc-chord-pixel-x self t-scfactor beg-x time1)))
      (draw-stem self x-now C5 mode)
      (tell (give-all-draw-notes self) 'draw-note-4 x-now C5 t-scfactor) 
      (draw-extra-info self x-now C5 mode))))
;;__________

(defgeneric draw-extra-info (self x-now C5 mode))
(defmethod draw-extra-info ((self C-chord) x-now C5 mode)
  (declare (ignore x-now C5 mode)))
 
(defgeneric draw-single-note (self note t-scfactor beg-x time1 C5))
(defmethod draw-single-note  ((self C-chord) note t-scfactor beg-x time1 C5)
  (let ((x-now (calc-chord-pixel-x self t-scfactor beg-x time1)))
    (draw-note-4 note x-now C5 ())))

(defgeneric draw-single-dur-line (self note t-scfactor beg-x time1 C5))
(defmethod draw-single-dur-line ((self C-chord) note t-scfactor beg-x time1 C5)
  (let ((x-now (calc-chord-pixel-x self t-scfactor beg-x time1)))
    (draw-note-duration-line-xor note x-now C5 t-scfactor))) 

(defgeneric draw-single-symbolic-dynamic (self note t-scfactor beg-x time1 C5))
(defmethod draw-single-symbolic-dynamic ((self C-chord) note t-scfactor beg-x time1 C5)
  (let ((x-now (calc-chord-pixel-x self t-scfactor beg-x time1)))
    (draw-note-symbolic-dynamic-xor note x-now C5 t-scfactor))) 

(defgeneric draw-single-offset-line (self note t-scfactor beg-x time1 C5))
(defmethod draw-single-offset-line ((self C-chord) note t-scfactor beg-x time1 C5)
  (let ((x-now (calc-chord-pixel-x self t-scfactor beg-x time1)))
    (draw-note-offset-line-xor note x-now C5 t-scfactor)))

;;___________
;; stem shows the exact time
(defgeneric draw-stem (self x C5 &optional mode))
(defmethod draw-stem ((self C-chord) x C5 &optional mode)
  (declare (ignore mode)) 
  (let ((y-min (1- (give-pixel-y (car (notes self)) C5)))
        (y-max (give-pixel-y (car (last (notes self))) C5)))
    ;;    (let-window-pen win :mode (if mode :patxor  :srcor)
    (draw-line x y-min x (- y-max 18))
    (draw-ledger-lines self x y-min y-max C5)))

(defgeneric inside-chord-? (self x rel-offset))
(defmethod inside-chord-? ((self C-chord) x rel-offset)
  (when (and (> (t-time self) (- x rel-offset))
             (< (t-time self) (+ x rel-offset))) self))

;;_______________

(defgeneric get-dias-with-alts (self))
(defmethod get-dias-with-alts ((self C-chord))
  (let ((dias (ask-all (notes self) 'diatone))
        (alts (ask-all (notes self) 'alteration))
        (dias-with-alts))
    (while dias
      (when (pop alts)
        (push (car dias) dias-with-alts))
      (pop dias))
    dias-with-alts))

(defvar *alt-tolerance* 6)

(defgeneric make-alt-groups (self))
(defmethod make-alt-groups ((self C-chord))
  (let ((notes (notes self))
        (group-length 1)
        (alt-now) (big-alt-temp))
    (while notes 
      (while (and notes (not (alteration (car notes)))) (pop notes))
      (if notes (setq alt-now (diatone (pop notes))))
      (while (and notes (not (alteration (car notes)))) (pop notes))
      (while (and notes 
                                        ;(< (- alt-now (diatone (car notes))) *alt-tolerance*)) 
                  (< (-  (diatone (car notes)) alt-now) *alt-tolerance*))
        (incf group-length)
        (pop notes)
        (while (and notes (not (alteration (car notes)))) (pop notes)))
      (push group-length big-alt-temp)
      (setq group-length 1))
    (nreverse big-alt-temp)))

(defgeneric make-alt-zig-zag (self))
(defmethod make-alt-zig-zag ((self C-chord))
  (let ((alt-groups (make-alt-groups self))
        (x-values)(left)(right)(alt-group-temp)
        (x-now)(x-shift 5)(left?))
    (while (setq alt-group-temp (pop alt-groups))
      (setq left (* x-shift (truncate (/ (1+  alt-group-temp) 2)))) 
      (setq right (- left x-shift))
      (setq x-now 0)
      (setq left? ())
      (push  x-now x-values)
      (repeat (1- alt-group-temp)
        (if left?
            (push (incf x-now right) x-values)
            (push (decf x-now left) x-values))
        (setq left? (not left?))))
    x-values))
;;___________
;; ledger-lines
;;*g2-g-staffs* *g-plain-staffs* -> (low-y-off-set  (+ C5 0)

(defun round-midic (midic)
  (let ((approx (approx-factor *current-approx-scale*)))
    (round (* approx (round midic approx)))))

(defun ledger-zone1-a (note)
  (let ((midic (round-midic (midic note))))
    (or (and (eql *current-music-notation-scale* *c-major-scale*) (= midic 8000))
        (and (>= midic 8075) (< midic 8575)))))
    ;(or (= midic 8000) (and (>= midic 8075) (< midic 8575)))))

(defun ledger-zone1-b (note)
  (let ((midic (round-midic (midic note))))
    (and (> midic 8350) (< midic 8575))))

(defun ledger-zone2-a (note)
  (let ((midic (round-midic (midic note))))
    (and (> midic 3550) (< midic 4075))))

(defun ledger-zone2-b (note)
  (let ((midic (round-midic (midic note))))
    (and (> midic 3550) (< midic 3775))))

(defgeneric draw-ledger-lines (self x y-min y-max C5))
(defmethod draw-ledger-lines ((self C-chord) x y-min y-max C5)
  (draw-ledger-for-notes (notes self) x y-min y-max C5))

(defun draw-ledger-for-notes (notes x y-min y-max C5)
 (unless (eql *staff-num* 7)   ; empty staff
   (multiple-value-bind (high low) (get-staff-offsets *staff-num*)
     (let ((high-y-off-set (- C5 high))
           (low-y-off-set  (- C5 low)))
       (setq y-max (- y-max 2))
       (setq y-min (+ y-min 2))
       (while (> high-y-off-set y-max)
         (draw-ledger-line-f x high-y-off-set)
         (decf high-y-off-set 4))
       (while (< low-y-off-set y-min)
         (draw-ledger-line-f x low-y-off-set)
         (incf low-y-off-set 4))
       (if (some #'ledger-zone1-a notes) (draw-ledger-line-f x (- C5 24)))
       (if (some #'ledger-zone1-b notes) (draw-ledger-line-f x (- C5 28)))
       (if (some #'ledger-zone2-a notes) (draw-ledger-line-f x (- C5 -24)))
       (if (some #'ledger-zone2-b notes) (draw-ledger-line-f x (- C5 -28)))
         ;(draw-ledger-line-f x (- C5 -24))
         ;(draw-ledger-line-f x (- C5 -28)))
       (if (member 35 (ask-all notes 'diatone) :test #'eq)
         (draw-ledger-line-f x C5))))))

(defun draw-ledger-line-f (x y)
  (draw-line (- x 10) y (+ x 6) y))

(defgeneric draw-ledger-line (self x y))
(defmethod draw-ledger-line ((self C-chord) x y)
  (draw-line (- x 10) y (+ x 6) y))

;;___________
;;  noteheads

(defgeneric make-diatone-groups (self))
(defmethod make-diatone-groups ((self C-chord))
  (let ((notes (notes self))
        (group-length 1)
        (dia-now)(dia-tolerance 2)(big-dia-temp))
    (while notes
      (setq dia-now (diatone (pop notes)))
      (while (and notes 
                  (< (abs (- dia-now (diatone (car notes)))) dia-tolerance))
        (incf group-length)
        (pop notes))
      (push group-length big-dia-temp)
      (setq group-length 1))
    (nreverse big-dia-temp)))

(defgeneric make-chord-zig-zag (self))
(defmethod make-chord-zig-zag ((self C-chord))
  (let ((dia-groups (make-diatone-groups self))
        (x-values)(left)(right)(dia-group-temp)
        (x-now)(x-shift 7)(left?))
    (while (setq dia-group-temp (pop dia-groups))
      (setq left (* x-shift (truncate (/ (1+  dia-group-temp) 2)))) 
      (setq right (- left x-shift))
      (setq x-now 0)
      (setq left? ())
      (push  x-now x-values)
      (repeat (1-  dia-group-temp)
        (if left?
            (push (decf x-now right) x-values)
            (push (incf x-now left) x-values))
        (setq left? (not left?))))
    (nreverse x-values)))

(defgeneric calc-chord-x-values (self))
(defmethod calc-chord-x-values ((self C-chord)) (make-chord-zig-zag self)) 

;;___________

(defgeneric sort-notes (self))
(defmethod sort-notes ((self C-chord))
  (let ((min (midic (car (notes self)))))
    (unless (dolist (note (cdr (notes self)) t)
              (if (< (midic note) min) (return nil) (setq min (midic note))))
      (setf (notes self)
            (sort (notes self) #'< :key (lambda (note)(midic note)))))
    (notes self)))
          
(defgeneric update-chord (self))
(defmethod update-chord ((self C-chord))
  (when (notes self)
    (unless *mn-view-arp-flag* (sort-notes self)) ;Camilo 910404
    (let ((notes (notes self))
          ;;[Camilo] 18/3/91
          (x-heads (calc-chord-x-values self))
          (alt-x-values (make-alt-zig-zag self)))
      (while notes
        (setf (alt-delta-x (car notes)) 
              (if  (alteration (car notes))  (- (pop alt-x-values) 12) 0))
        (setf (delta-x  (car notes)) (- (pop x-heads) 6))
        (pop notes))))
  self)

(defgeneric max-dur (self))
(defmethod max-dur ((self C-chord))
  (apply #'max (ask-all (notes self) 'dur)))

(defgeneric min-max-diatone (self))
(defmethod min-max-diatone ((self C-chord))
  (let ((dias (ask-all (notes self) 'diatone)))
    (cons (apply #'min dias)(apply #'max dias))))

;;================================

(defclass C-chord-line ()
    ((chords  :initform nil :initarg :chords :accessor chords)
     (play-flag :initform nil :accessor play-flag)
     (stems-flag :initform nil :initarg :stems-flag :accessor stems-flag)))

(defmethod decompile ((self C-chord-line))
  `(make-instance ',(class-name (class-of self))
                  :chords  (list ,@(ask-all (chords self) 'decompile)))) 

(defgeneric kill-chords (self))
(defmethod kill-chords ((self C-chord-line))
  (tell (chords self) 'kill-notes))

;;________________

(defmethod stop-play ((self C-chord-line))
  (setf (play-flag self) ()))


(defgeneric play-chords (self &optional begin-time))
(defmethod play-chords ((self C-chord-line) &optional begin-time)
  (setf *MN-play-flag* t)
  (when (chords self)
    (setf (play-flag self) t)
    (let ((start-time (or begin-time (t-time (car (chords self)))))
          (chords (chords self)))
      (dfuncall  start-time 'continue-play-chords self chords start-time))))

(defgeneric play-visible-chords (self visible-chords t-offset))
(defmethod play-visible-chords ((self C-chord-line) visible-chords t-offset)
  (when visible-chords
    (setf (play-flag self) t)
    (let ((start-time (t-time (car visible-chords))))
      (dfuncall (- start-time t-offset) 'continue-play-chords self visible-chords start-time))))

(defgeneric play-selected-chords (self chords t-offset))
(defmethod play-selected-chords ((self C-chord-line) chords t-offset)
  (when chords
    (setf (play-flag self) t)
    (let ((start-time (t-time (car chords))))
      (dfuncall  (- start-time t-offset) 'continue-play-chords self chords start-time))))

(defgeneric continue-play-chords (self chords start-time))
(defmethod continue-play-chords ((self C-chord-line) chords start-time)
  (when (and (play-flag self) *MN-play-flag*)
    (play-chord (pop chords)) 
    (when chords
      (let ((new-start-time (t-time (car chords))))
        (dfuncall (- new-start-time start-time) 'continue-play-chords self chords new-start-time)))))

;;_____________________

;;(defmethod draw-chord-line1  ((self C-chord-line) t-scfactor beg-x time1 time2 C5)
;;  (set-visible-chords self time1 time2)
;;  (tell (visible-chords self)  'draw-chord t-scfactor beg-x time1 C5)) 

(defgeneric draw-active-chord (self chord t-scfactor beg-x time1 C5))
(defmethod draw-active-chord  ((self C-chord-line) chord t-scfactor beg-x time1 C5) 
  (draw-chord chord t-scfactor beg-x time1 C5 t)) 

(defgeneric add-new-chord (self new-chord))
(defmethod add-new-chord ((self C-chord-line) new-chord)
  (sort-chord-by-time self new-chord))

(defgeneric remove-chord (self chord &optional kill-lock))
(defmethod remove-chord ((self C-chord-line) chord &optional kill-lock)
  (unless kill-lock 
    (tell (notes chord) 'remove-instrument-item ()()))
  (setf (chords self) (remove chord (chords self) :test 'eq)))

;;  (stable-sort  (chords self) '< :key '(lambda (obj) (t-time obj)))

;;(defmethod set-visible-chords ((self C-chord-line) time1 time2)
;;   (setf (visible-chords self) (find-visible-chords self time1 time2)))

(defgeneric find-visible-chords (self time1 time2))
(defmethod find-visible-chords ((self C-chord-line) time1 time2)
  (let ((chords (chords self))
        (visible-chords))
    (while (and chords (> time1 (t-time (car chords))))(pop chords))
    (while (and chords (> time2 (t-time (car chords))))
      (push  (pop chords) visible-chords))
    (nreverse visible-chords)))
     
(defgeneric sort-chord-by-time (self new-chord))
(defmethod sort-chord-by-time ((self C-chord-line) new-chord)
  (let ((chords (chords self))
        (time1 (t-time new-chord))
        (chords1))
    (while (and chords (> time1 (t-time (car chords))))(push (pop chords) chords1))
    (setf (chords self)
          (append  (nreverse chords1)(list new-chord) chords))))


