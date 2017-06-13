;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               structured-time.lisp
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


(defgeneric give-structured-duration2 (self rel-dur super-note)
  (:method ((self C-MN-window) rel-dur super-note)
    (if (super-win self)
        (give-structured-duration2 (super-win self)
                                   (* rel-dur
                                      (calc-relative-duration (car (editor-objects (editor-view-object self))) super-note))
                                   (super-note self))
        (round (* rel-dur (dur super-note))))))

;; if recursion -> relative begin (0 - 1)
;; if not -> absolute begin-time in ticks

(defgeneric give-structured-begin-time2 (self rel-begin-time super-note)
  (:method ((self C-MN-window) rel-begin-time super-note)
    (declare (ignore rel-begin-time))
    (let* ((MN-view (car (editor-objects (editor-view-object self))))
           (new-rel-begin (calc-relative-begin-time MN-view super-note))
           )
      (if (super-win self)
          (progn
            (setq new-rel-begin
                  (+ (give-t-time-for-note MN-view super-note)
                     (* new-rel-begin (dur super-note))))
            (give-structured-begin-time2 (super-win self)
                                         (calc-relative-begin-time new-rel-begin ())
                                         (super-note self)))
          (give-t-time-for-note MN-view super-note)))))

(defgeneric record-structured-process (self)
  (:method ((self C-MN-window))
    (with-cursor  *watch-cursor*
      (let ((chords (chords (chord-line (car (editor-objects (editor-view-object self))))))
            begin-times duration-times midi-boxes midi-boxes-temp pw-windows pw-windows-temp
            pw-win-now midi-box-now total-dur)
        (while chords
          (setq pw-windows-temp
                (ask-all (ask-all (notes (car chords)) 'instrument) 'pw-win)) ;PW returns nil
          (while pw-windows-temp
            (when (setq pw-win-now (pop pw-windows-temp))
              (when (setq midi-box-now
                          (car (find-all-midi-out-boxes pw-win-now)))
                (push pw-win-now pw-windows)
                (push midi-box-now midi-boxes))))
          (pop chords))
        (setq pw-windows (nreverse pw-windows))
        (setq midi-boxes-temp (setq midi-boxes (nreverse midi-boxes)))
        (setq begin-times (ask-all pw-windows 'give-structured-begin-time))
        (setq duration-times (ask-all pw-windows 'give-structured-duration1 1))
        (setq total-dur (apply #'max (mapcar #'+ begin-times duration-times)))
        (while midi-boxes-temp
          (set-begin-time (car midi-boxes-temp) (pop begin-times))
          (set-duration-time (car midi-boxes-temp) (pop duration-times))
          (pop midi-boxes-temp))
        (start-clock *global-clock* total-dur midi-boxes)))))

(defgeneric add-constant-t-time (self time))
(defmethod add-constant-t-time ((self C-chord) time)
  (setf (t-time self) (+ (t-time self) time)))
(defmethod add-constant-t-time ((self C-chord-line) time)
  (ask-all (chords self) 'add-constant-t-time time))

(defgeneric make-structured-score (self)
  (:method ((self C-MN-window))
    (with-cursor *watch-cursor*
      (let ((chords (chords (chord-line (car (editor-objects (editor-view-object self))))))
            MN-chord-lines pw-win-now super-notes super-notes-temp midi-box-now
            res-chord-lines last-diatone patch-box)
        (setq super-notes
              (ask-all
               (remove nil
                       (ask-all (ask-all (apply #'append (ask-all chords 'notes)) 'instrument) 'pw-win)
                       :test 'eq)
               'super-note))
        (setq super-notes (setq super-notes-temp (sort super-notes '> :key (lambda (a) (diatone a)))))
        (while super-notes
          (setq pw-win-now (pw-win (instrument (car super-notes))))
          (when (setq midi-box-now (car (find-all-midi-out-boxes pw-win-now)))
            (push
             (eval (decompile (give-MN-editor-chord-line midi-box-now 0)))
             MN-chord-lines)
            (add-constant-t-time (car MN-chord-lines)
                                 (give-structured-begin-time pw-win-now)))
          (pop super-notes))
        (setq MN-chord-lines (nreverse MN-chord-lines))
        (setq last-diatone (diatone (pop super-notes-temp)))
        (setq res-chord-lines (list (pop MN-chord-lines)))
        (while super-notes-temp
          (if (= last-diatone (diatone (car super-notes-temp))) ; to same staff
              (progn
                (setq chords (chords (pop MN-chord-lines)))
                (while chords
                  (sort-chord-by-time (car res-chord-lines) (pop chords))))
              (progn
                (push (pop MN-chord-lines) res-chord-lines)
                (setq last-diatone (diatone (car super-notes-temp)))))
          (pop super-notes-temp))
;;;Camilo====================== put in new typing scheme into dynamically created polif box 920120
        (setq patch-box
                                        ;(make-patch-box 'C-patch-polifMN-mod 'PMNN
                                        ;(make-pw-npolif-editor-list (length res-chord-lines)))
              (make-PW-standard-box 'C-patch-polifMN-mod 'poly-coll (make-point 15 15)
                                    (make-list (length res-chord-lines) :initial-element :default)))
        (set-all-chords (chord-line-list patch-box) (nreverse res-chord-lines))
;;;(setf (chord-line-list patch-box) (nreverse res-chord-lines))
        (set-pw-window-pointers patch-box (pw-win self))
        (add-patch-box (pw-win self) patch-box)
        (activate-control patch-box )
        ;;     (pretty-visible-layout (car (controls (application-object patch-box))))
        (open-patch-win patch-box)))))

(defun set-all-chords (ch-list from-ch-list)
  (mapc (lambda (ch-to ch-from) (setf (chords ch-to) (chords ch-from))) ch-list from-ch-list))

(defun make-pw-npolif-editor-list (count)
  (let ((arg-list))
    (for (i 0 1 (1- count))
      (push  '*MN-collector-type* arg-list)
      (push  (concatenate  'string  "coll" (format nil "~D" (1+ i))) arg-list))
    (nreverse arg-list)))

;;========================================================================================
;; C-music-notation-panel

(defgeneric max-t-time+dur (self)
  (:method ((self C-music-notation-panel))
    (let ((t-times+durs (mapcar #'+
                                (ask-all (chords (chord-line self)) 't-time)
                                (ask-all (chords (chord-line self)) 'max-dur))))
      (apply #'max t-times+durs))))

(defgeneric give-t-time-for-note (self note)
  (:method ((self C-music-notation-panel) note)
    (let ((chords (chords (chord-line self)))
          res-chord)
      (while chords
        (when (member note (notes (car chords)) :test 'eq)
          (setq res-chord (car chords))
          (setq chords ()))
        (pop chords))
      (when res-chord (t-time res-chord)))))

(defgeneric calc-relative-duration (self super-note)
  (:method ((self C-music-notation-panel) super-note)
    (let ((dur (dur super-note))
          (t-times+dur (max-t-time+dur self)))
      (/ dur t-times+dur))))

(defgeneric calc-relative-begin-time (self super-note &optional begintime)
  (:method ((self C-music-notation-panel) super-note &optional begintime)
    (let ((begin (give-t-time-for-note self super-note))
          (t-times+dur (max-t-time+dur self)))
      (if begintime
          (/ t-times+dur begintime)
          (if (zerop begin) 0 (/ t-times+dur begin))))))

;;====================================================================================================
;; C-note

(defvar *pw-struct-window-counter* 0)
(defvar *pw-struct-collector-counter* 0)

(defun make-new-structured-window (string &optional counter)
  (let ((win-string
          (concatenate  'string  string
                        (if counter
                            (format nil "~D" counter)
                            (format nil "~D" (incf *pw-struct-window-counter*))))))
    (make-instance 'C-pw-window
                   :window-title win-string
                   :view-position (make-point 50 38)
                   :view-size (make-point 200 200) :close-box-p nil :window-show ())))

(defun make-new-note-collector ()
  (make-PW-standard-box  'C-patch-midi-Mod 'collector (make-point 15 15)))

;;(make-new-note-collector)
(defgeneric add-MN-to-note (self win x y)
  (:method ((self C-note) win x y)
    (declare (ignore x y))
    (let ((pw-win (make-new-structured-window "Strcoll"))
          (midi-patch (make-new-note-collector)))
      (setf (pw-win (view-window (give-MN-editor midi-patch))) pw-win)
      (setf (super-win  (view-window (give-MN-editor midi-patch))) win)
      (setf (super-note (view-window (give-MN-editor midi-patch))) self)
      (setf (super-win pw-win) win)
      (setf (super-note pw-win) self)
      (add-patch-box pw-win midi-patch)
      (setf (instrument self) pw-win))
    (view-draw-contents win)))

(defgeneric add-PWwin-to-note (self win x y)
  (:method ((self C-note) win x y)
    (declare (ignore x y))
    (let ((pw-win (make-new-structured-window "Strwin")))
      (setf (super-win pw-win) win)
      (setf (super-note pw-win) self)
      (setf (instrument self) pw-win))
    (view-draw-contents win)))

;;====================================================================================================
;; C-pw-window

(defmethod make-super-note-connections ((self C-pw-window) super-note super-win)
  (setf (super-note self) super-note)
  (setf (super-win self) super-win)
  (let ((midi-patch (car (find-all-midi-out-boxes self))))
    (when midi-patch
      (setf (pw-win (view-window (give-MN-editor midi-patch))) self)
      (setf (super-win  (view-window (give-MN-editor midi-patch))) super-win)
      (setf (super-note (view-window (give-MN-editor midi-patch))) super-note))))

(defmethod remove-instrument-item ((self C-pw-window) x y)
  (declare (ignore x y))
  (kill-patch-window self)
  (setf (instrument (super-note self)) nil))
;;  (when win (draw-window win)))

(defmethod play-instrument ((self C-pw-window) note) (declare (ignore note)))

(defmethod open-instrument-editor ((self C-pw-window)  win x y)
  (declare (ignore x y))
  (setf (super-win self) win)
  (setf (super-note self) *global-selected-note*)
  (let ((midi-patch (car (find-all-midi-out-boxes self))))
    (when midi-patch
      (setf (super-win  (view-window (give-MN-editor midi-patch))) win)))
  (window-select self))


(defmethod pw-win ((self C-pw-window))
  (let ((midi-patch (car (find-all-midi-out-boxes self))))
    (if midi-patch self nil)))

(defmethod draw-instrument ((self C-pw-window)  x y t-scfactor)
  (declare (ignore y t-scfactor))
  (set-view-font  (view-window *current-MN-editor*) *patchwork-font-spec*)
  (draw-string x *MN-note-ins-y* (window-title self))
  (set-view-font  (view-window *current-MN-editor*) *music-font-spec*)
  (incf *MN-note-ins-y* 12))

(defgeneric give-structured-duration1 (self rel-dur)
  (:method ((self C-pw-window) rel-dur)
    (if (super-win self)
        (give-structured-duration2 (super-win self) rel-dur (super-note self))
        rel-dur)))

(defmethod give-structured-begin-time ((self C-pw-window))
  (if (super-win self)
      (give-structured-begin-time2 (super-win self) 0 (super-note self))
      0))

;;======================================================================================
;;======================================================================================
;;pw

(defmethod give-structured-begin-time ((self C-patch))
  (give-structured-begin-time (view-window self)))

;;=======================================
;;  structured duration
;;=======================================

(defclass C-structured-dur (C-patch) ())

(defmethod patch-value ((self C-structured-dur) obj)
  (give-structured-duration1 (view-window self)
                             (patch-value (car (input-objects self)) obj)))

(defunp str-dur ((time fix>0)) fix
    "Str-dur (structured duration) box returns the duration of a super-note.
This value is multiplied by the value of the input box (by default 1).
Str-dur box is normally used in context of structured time.
"
  (declare (ignore time)))

;;========================================================================================
;; structured begin time

(defclass C-structured-outbox (C-pw-out)
  ((clock :initform 0 :accessor clock)))

(defmethod patch-value ((self C-structured-outbox) obj)
  (setf (clock self)
        (+ (clock obj)
           (- (give-structured-begin-time obj)
              (give-structured-begin-time (view-window self)))))
  (patch-value (car (input-objects self)) self))

;; !!  obj will be me after this call and the clock will accessed from this object !!

(defunp strout ((patch user-out)) nil
    "Strout (structured out) is used always in combination with a 'in' box.
It is similar to to an 'out' box except that it adds to the clockvalue of
the requesting box the begintime of the super-note of the requesting box.
Strout box is normally used in context of structured time.
"
  (declare (ignorable patch))
  (values))
