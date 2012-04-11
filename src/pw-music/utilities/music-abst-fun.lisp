;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;=========================================================
;;;;
;;;;  PATCH-WORK
;;;;  By Mikael Laurson, Jacques Duthen, Camilo Rueda.
;;;;  © 1986-1992 IRCAM 
;;;;
;;;;=========================================================

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

(in-package :pw)

(defmethod compile-me ((self C-patch-chord-box-M ) obj)
   (let ((my-in (car (pw-controls self)))
        (my-connect (car (input-objects self))))
     (if (eq my-in my-connect)
       (let ((my-chord (get-output-dimension self)))
         (if (eq (type-of my-chord) (type-of (car (chords (chord-line my-in)))))
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
           #'(lambda () *closure-data*))))
    `(list 'funcall ,fun)))

(defmethod compile-me ((self C-patch-PolifMN) obj)
  `(list 'list ,@
    (let ((the-list (chord-line-list self))
          (controls (pw-controls self))
          (objects (input-objects self))
          (temp-list))
      (loop for i upfrom 0 as control in controls for object in objects do
          (if (eq control object)
            (push (decompile (nth i the-list)) temp-list)
            (push (list 'eval (compile-me object obj)) temp-list)))
      (nreverse temp-list))))
