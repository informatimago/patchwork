;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               midi-instrument-pw.lisp
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

;;==============================================================================
;; PW boxes
;;==============================================================================

(defclass C-patch-midi-fix (C-patch)())

(defgeneric decompile-ins-object (self obj))
(defmethod decompile-ins-object ((self C-patch-midi-fix) obj)
  `(make-instance 'C-midi-ins-fix
                  :status               ,(patch-value (nth 0 (input-objects self)) obj)
                  :controller           ,(patch-value (nth 1 (input-objects self)) obj)
                  :value                ,(patch-value (nth 2 (input-objects self)) obj)
                  :label               ',(patch-value (nth 3 (input-objects self)) obj)))

(defmethod patch-value ((self C-patch-midi-fix) obj)
  (eval (decompile-ins-object self obj)))


(defvar midi-fix ()
"A PW-box that should allways to be connected with an
midi-ins-box.
The inputs (status,controller,value) determine what midi
information is sent before a midi-note-on event.
Label is used for browsing inside the MN-editor.
")
;;==============================================================================

(defclass C-patch-midi-bpf (C-patch)())

(defmethod decompile-ins-object ((self C-patch-midi-bpf) obj)
  `(make-instance 'C-midi-ins-bpf
      :break-point-function ,(decompile (patch-value (car (input-objects self)) obj))
      :low-limit            ,(patch-value (nth 1 (input-objects self)) obj)
      :high-limit           ,(patch-value (nth 2 (input-objects self)) obj)
      :status               ,(patch-value (nth 3 (input-objects self)) obj)
      :controller           ,(patch-value (nth 4 (input-objects self)) obj)
      :label               ',(patch-value (nth 5 (input-objects self)) obj)
      :sample-rate          ,(patch-value (nth 6 (input-objects self)) obj)))

(defmethod patch-value ((self C-patch-midi-bpf) obj)
  (eval (decompile-ins-object self obj)))

(defvar midi-bpf ()
"A PW-box that should allways to be connected with an
midi-ins-box.
The input (bpf) is used to determine the bpf (break-point-function).
The inputs (low-limit,high-limit) are used to scale the bpf.
The inputs (status,controller) determine what kind of continuous
midi information is sent during a midi-note event.
The input (ticks) tells how often this information is sent.
Label is used for browsing inside the MN-editor.
")
;;_________________

(defparameter *bpf-lib-midi-ins-pw-type*
  (make-instance 'C-pw-type :control-form `(make-instance 'C-menubox-bpf
    :view-size (make-point 36 14)
    :menu-box-list ,*pw-BPF-library* :type-list '(bpf))))

(defparameter *midi-collection-label-pw-type*
  (make-instance 'C-pw-type
          :control-form
           `(make-instance 'C-ttybox-str
              :view-size (make-point 36 14)
              :dialog-item-text "label" :type-list '(no-connection))))

(defparameter *bpf-lib-midi-status-pw-type*
  (make-instance 'C-pw-type :control-form `(make-instance 'C-menubox-val
    :view-size (make-point 36 14)
    :menu-box-list '(("paftr" . #xA0) ("contr". #xB0) ("prog" . #xC0) ("maftr" . #xD0) ("ptchb" . #xE0))
    :type-list '(fixnum))))

;;==============================================================================
;;  midi-ins box
;; second argument is nargs

(defclass C-patch-midi-ins (C-patch)())

(defmethod init-patch ((self C-patch-midi-ins))
  (when (not (eql (nth 1 (input-objects self)) (nth 1 (pw-controls self))))
    (tell (nth 1 (input-objects self)) 'init-patch)))

(defmethod patch-value ((self C-patch-midi-ins) obj)
  (when (not (eql (nth 1 (input-objects self)) (nth 1 (pw-controls self))))
    (make-instance 'C-midi-ins-collection
        :ins-name (dialog-item-text (car (pw-controls self)))
        :ins-objects (ask-all (nth 1 (input-objects self)) 'patch-value obj))))

;;______________________

(defmethod connect-ctrl ((self C-patch-midi-ins) ctrl ctrl-panel)
   (if (eql ctrl (car (pw-controls self)))
     (setf (nth (find-nth-ctrl self ctrl) (input-objects self)) ctrl-panel)
     (if (eql (nth 1 (pw-controls self)) (nth 1 (input-objects self)))
       (setf (nth 1 (input-objects self)) (list ctrl-panel))
       (push ctrl-panel (nth 1 (input-objects self))))))

(defmethod connect-nth-control ((self C-patch-midi-ins) nth-ctrl ctrl-panel)
  (if (eql nth-ctrl 0)
     (setf (nth nth-ctrl (input-objects self)) ctrl-panel)
     (if (eql (nth 1 (pw-controls self)) (nth 1 (input-objects self)))
         (setf (nth nth-ctrl (input-objects self)) (list ctrl-panel))
         (push ctrl-panel (nth nth-ctrl (input-objects self)))))
  (setf (open-state (nth nth-ctrl (pw-controls self))) ()))

(defvar midi-ins ()
"A PW-box that should allways to be connected with an
midi-box (m-ins input).
The first input is used to give the midi-instrument a name.
The second input can accept any collection of midi-fix and
midi-bpf boxes - so this box acts as collector for
midi-instrument information.
")
;;_________________

(defparameter *midi-ins-label-pw-type*
  (make-instance 'C-pw-type
          :control-form
           `(make-instance 'C-ttybox-str
               :view-size (make-point  36 14)
               :dialog-item-text "flute" :type-list '(no-connection))))
(defparameter *midi-ins-nargs-pw-type*
  (make-instance 'C-pw-type
          :control-form
           `(make-instance 'C-ttybox
              :view-size (make-point 36 14)
              :dialog-item-text "nargs" :type-list '(midi-fix midi-bpf))))

;;==============================================================================
