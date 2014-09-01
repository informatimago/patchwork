;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               music-abst-fun.lisp
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

;;;=================================================
;;; A set of methods for abstraction compilation
;;;
;;; Each patch-class (other than the standard C-patch) must provide a
;;; "compile-me" method which, esentially, constructs a function form aquivalent
;;; to its "patch-value" behaviour. Note that if a patch-box's "patch-value"
;;; action is NOT functional (its behaviour depends on patch-boxes actions other
;;; than itself and its inputs) then compilation is going to be extremely hard
;;; if possible at all. See for instance the compilation of "C-pw-circ-end" below.
;;; WARNING: Abstractions MUST unambiguously order their inputs (use "absin2" box for
;;;          this). Inputs with conflicting orders are IGNORED!
;;;
;;; Classes considered in this file are:
;;;
;;;    C-patch, C-abstract-out, C-abstract-in, C-abstract, 
;;;    C-patch-chord-box-M (the chord box), 
;;;    C-pw-circ, C-pw-circ-end, 
;;;    C-patch-midi (the collector), 
;;;    C-patch-PolifMN (the polyphonic collector)
;;;    C-pw-stop-time, C-clock-constant
;;;    C-enum-collect-sink (the loop box)
;;;    C-enum-collect-source (the enum box)
;;;    C-patch-function (the BPF)
;;;    C-patch-env   (the BPF-ENV)
;;;    C-patch-transfer (BPF transfer)
;;;    C-patch-osc    (the oscillator)
;;;    C-patch-osc-period (the osc-period)
;;;    C-patch-MD-get-slot  (get-slot box)
;;;    C-patch-MD-set-slot
;;;=================================================  


(defmethod compile-me ((self C-patch-chord-box-M ) obj)
   (let ((my-in (car (pw-controls self)))
        (my-connect (car (input-objects self))))
     (if (eql my-in my-connect)
       (let ((my-chord (get-output-dimension self)))
         (if (eql (type-of my-chord) (type-of (car (chords (chord-line my-in)))))
           (decompile my-chord)
           `'',my-chord))
       `(let* ((a-chord ,(decompile (make-instance 'C-chord)))
               (my-chord 
                (list 'make-chord-object 
                      (list 'list! ,(compile-me my-connect obj)) 0 
                      (list 'type-of a-chord))))
          my-chord))))

(defmethod compile-me ((self C-patch-midi) obj)
  (declare (ignore obj))
  (let ((fun
         `(let ((*closure-data* ,(decompile (chord-seq self))))
           (lambda () *closure-data*))))
    `(list 'funcall ,fun)))

(defmethod compile-me ((self C-patch-PolifMN) obj)
  `(list 'list ,@
    (let ((the-list (chord-line-list self))
          (controls (pw-controls self))
          (objects (input-objects self))
          (temp-list))
      (loop for i upfrom 0 as control in controls for object in objects do
          (if (eql control object)
            (push (decompile (nth i the-list)) temp-list)
            (push (list 'eval (compile-me object obj)) temp-list)))
      (nreverse temp-list))))
