;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               abst-fun.lisp
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
;;; "compile-me" method which, esentially, constructs a function form equivalent
;;; to its "patch-value" behaviour. Note that if a patch-box's "patch-value"
;;; action is NOT functional (its behaviour depends on patch-boxes actions other
;;; than itself and its inputs) then compilation is going to be extremely hard
;;; if possible at all. See for instance the compilation of "C-pw-circ-end" below.
;;; WARNING: Abstractions MUST unambiguously order their inputs (use the input of the "absin" box for
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
;;;    C-patch-osc    (the oscillator)
;;;    C-patch-osc-period (the osc-period)
;;;=================================================

(defgeneric compile-me (self obj))


(defclass C-compiled-patch (C-patch)
  ((code :initform nil :initarg :code :accessor code)))

(defpackage "USER-COMP-ABSTR")   ;the compiled abstractions package

(defmethod save ((self C-compiled-patch))
  (if *pw-nosave-mode*
      (ui::message-dialog "Sorry this version cannot save files.")
      (actual-save self (choose-new-file-dialog :directory (format () "~A.comp" (string-downcase (pw-function self)))
                                                :button-string "Save patch as"))))

(defmethod actual-save ((self C-compiled-patch) file-name)
  (let* ((my-in-types  (get (pw-function self) '*type-intypes*))
         (my-out-types (get (pw-function self) '*type-outtype*))

         (*print-pretty* nil))
    (multiple-value-bind (file-name exists?)
        (intern (trim-extension (string-downcase (pathname-name file-name)))
                "USER-COMP-ABSTR")
      (when exists?
        (ui:uiwarn "the name ' ~D ' already exists for an abstraction.
A previous function called ~D will be redefined" file-name file-name)
        (ui:ed-beep))
      (setf (pw-function self) file-name)
      (init-pw-function-string self)
      (with-cursor *watch-cursor*
        (WITH-OPEN-FILE (out file-name
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
          (prin1 '(in-package :pw) out)
          (let ((*package* :pw))
            (prin1
             `(progn (setf (fdefinition ',(pw-function self)) (eval ,(code self)))
                     (setf (get ',(pw-function self) '*type-intypes*) ',my-in-types)
                     (setf (get ',(pw-function self) '*type-outtype*) ',my-out-types)
                     ;;(PW-addmenu *pw-menu-patch* (list ',(pw-function self)))
                     (let ((box ,(multiple-value-bind (args extensible?)
                                     (make-defunp-function-arg-list (pw-function self))
                                   `(make-PW-standard-box
                                     ,(if extensible? ''C-pw-functional
                                          (if (= (length args) 2) ''C-pw-resize-x ''C-patch))
                                     ',(pw-function self)))))
                       (set-view-position box 15 15)
                       (add-patch-box *active-patch-window* box))) out)))))))

(defmethod patch-value ((self C-compiled-patch) obj)
  (let ((*global-calling-object* obj))
    (declare (special *global-calling-object*))
    (call-next-method)))

(defmethod compile-me ((self C-abstract-out) obj)
  (compile-me (car (input-objects self)) obj))

(defmethod compile-me ((self C-abstract) obj)
  (setf (abstract-in-list self) nil)
  (let* ((code (compile-me (out-obj self) obj))
         (varlist (mapcar #'cdr (sort (abstract-in-list self) '< :key #'car)))
         (pw-fun (read-from-string (format nil "~A" (gensym "abs"))))
         (patches (controls (patch-win self)))
         (in-boxes (find-abstract-in-boxes (patch-win self) patches))
         (in-docs-temp)
         (in-put-docs
           (when in-boxes
             (setq in-docs-temp (get-absin-boxes-types-n patches in-boxes))
             (cond
               ((member nil in-docs-temp)
                (ui:message-dialog
                 "WARNING! absin box connected to irreducible types. ALL-type used")
                (mapcar (lambda (type-spec) (declare (ignore type-spec)) '(nilNum))
                        in-docs-temp))
               (t in-docs-temp))))
         new-patch-box)
    (setq new-patch-box (make-std-patch-box 'C-compiled-patch
                                            (string pw-fun)
                                            in-put-docs (patch-win self) in-boxes))
    (setq pw-fun (read-from-string (string (pw-function self))))
    (copy-PW-symbolic-type-data (pw-function new-patch-box) pw-fun)
    (set-function-arg-names pw-fun varlist)
    (setf (pw-function new-patch-box) pw-fun)
    (setf (code new-patch-box) `(list 'function (list 'lambda ',varlist ,code)))
    (save new-patch-box)
    ))

(defmethod compile-me ((self C-patch) obj)
  (let ((abs (mapcar (lambda (ctrl input)
                       (if (eql ctrl input)
                           `'',(patch-value ctrl obj)
                           (compile-me input obj) ))
                     (pw-controls self) (input-objects self))))
    `(list ',(pw-function self) ,@abs)))

(defmethod compile-me ((self C-abstract-in) obj)
  (declare (ignore obj))
  (let ((var        ; (read-from-string  (format nil "X~A" (in-index self)))))
            (read-from-string
                                        ;(format () "~A~A"
                                        ;        (doc-string (nth  (in-index self)
                                        ;                         (pw-controls (abstract-box self)))) (in-index self))
             (format () "~A~A"
                     (doc-string self) (in-index self))
             )))
    (unless (assoc (in-index self) (abstract-in-list (abstract-box self)))
      (push (cons (in-index self) var) (abstract-in-list (abstract-box self))))
    `',var))

(defmethod compile-me ((self C-pw-circ) obj)
  (let ((code (if (eql (car (input-objects self)) (car (pw-controls self)))
                  `',(patch-value (car (input-objects self)) obj)
                  (compile-me (car (input-objects self)) obj)))
        (fun
          `(let ((*closure-data* nil))
             (lambda (code)
               (if *closure-data*
                   (pop *closure-data*)
                   (progn (setf *closure-data*  code) (pop *closure-data*)))))))
    `(list 'funcall ,fun ,code)))

(defmethod compile-me ((self C-pw-circ-end) obj)
  (let ((code (if (eql (car (input-objects self)) (car (pw-controls self)))
                  `',(patch-value (car (input-objects self)) obj)
                  (compile-me (car (input-objects self)) obj)))
        (fun
          `(let ((*closure-data* nil)
                 (*closure-flag* nil))
             (lambda (code)
               (declare (special *global-calling-object*))
               (if (zerop (clock (clock-obj *global-calling-object*)))
                   (setf *closure-flag* nil))
               (cond
                 ((cdr *closure-data*) (pop *closure-data*))
                 ((not *closure-flag*)
                  (setf *closure-flag* t)
                  (setf *closure-data* code)
                  (pop *closure-data*))
                 (t (stop-clock (clock-obj *global-calling-object*))
                    (pop *closure-data*)))))))
    `(list 'funcall ,fun
           (list 'if '(zerop (clock (clock-obj *global-calling-object*))) ,code nil))))

;;best-way to compile C-pw-stop-time is to make it a standard patch (as it should!)
;;(defun stime (val) val)  ; (now it is...)

(defvar *global-calling-object*)

(defmethod compile-me ((self  C-clock-constant) obj)
  (let ((code (if (eql (car (input-objects self)) (car (pw-controls self)))
                  `',(patch-value (car (input-objects self)) obj)
                  (compile-me (car (input-objects self)) obj)))
        (fun
          `(let ((*closure-data* nil)
                 (*closure-clock* -1))
             (lambda (code)
               (if (eql *closure-clock* (clock *global-calling-object*))
                   *closure-data*
                   (progn
                     (setf *closure-clock* (clock *global-calling-object*))
                     (setf *closure-data* code)))))))
    `(list 'funcall ,fun ,code)))

(defmethod compile-me ((self C-enum-collect-sink) obj)
  (if (eql (first (pw-controls self)) (first (input-objects self)))
      '()
      (let* ((var (setf (slot-value (first (input-objects self)) 'comp-var)
                        (intern (format () "~S" (gensym "*enum-value*")))))
             (code (if (eql (second (input-objects self))
                           (second (pw-controls self)))
                       `'',(patch-value (second (input-objects self)) obj)
                       (compile-me (second (input-objects self)) obj)))
             (enum
               (compile-me (first (input-objects self)) (list self))))
        `(list 'let '(,var)
               '(declare (special ,var))
               (list 'mapcar (list 'function (list 'lambda '(val)
                                                   '(setf ,var val)
                                                   ,code))
                     ,enum)))))

(defmethod compile-me ((self C-map-first) obj)
  (let* ((inputs (input-objects self))
         (enums (list* (first inputs) (nthcdr 2 inputs)))
         (vars (mapcar (lambda (input)
                         (setf (slot-value input 'comp-var)
                               (intern (format () "~S" (gensym "*enum-value*"))))) enums))
         (code (if (eql (second inputs) (second (pw-controls self)))
                   `'',(patch-value (second inputs) obj)
                   (compile-me (second inputs) obj)))
         (enum-codes (ask-all enums #'compile-me (list obj)));;(list self)))
         res
         (local-vars (dotimes (k (length enums) res)
                       (push (intern (format () "~S" (gensym "*local-val*"))) res))))
    `(list 'let ',vars
           (list 'mapcar (list 'function (list 'lambda ',local-vars
                                               '(setf ,@(apply #'append (mapcar #'list vars local-vars)))
                                               ,code))
                 ,@ enum-codes))))

(defun my-car (list) (if (consp list) (first list) list))

(defmethod compile-me ((self C-enum-collect-source) obj)
  (let ((code (if (eql (car (input-objects self)) (car (pw-controls self)))
                  `'',(patch-value (car (input-objects self)) (my-car obj))
                  (compile-me (car (input-objects self)) (my-car obj)))))   ;obj))))
    (if (consp obj) code `',(slot-value self 'comp-var))))


(defmethod compile-me ((self C-patch-function) obj)
  (if (and (eql (first (pw-controls self)) (first (input-objects self)))
           (eql (second (pw-controls self)) (second (input-objects self))))
      (let* ((points (break-point-list (break-point-function (give-mini-bpf self))))
             (x (mapcar (lambda (x) (point-h x)) points))
             (y (mapcar (lambda (x) (point-v x))  points)))
        `(list 'make-break-point-function '',x '',y))
      (let ((tlist (if (eql (first (pw-controls self))
                           (first (input-objects self)))
                       `',(patch-value (first (input-objects self)) obj)
                       (compile-me (first (input-objects self)) obj)))
            (vlist (if (eql (second (pw-controls self))
                           (second (input-objects self)))
                       `',(patch-value (second (input-objects self)) obj)
                       (compile-me (second (input-objects self)) obj))))
        `(list 'make-break-point-function ,tlist ,vlist))))

(defmethod compile-me ((self C-patch-env) obj)
  (let ((code (compile-me (car (input-objects self)) obj)))
    `(list 'bpf-out ,code '(clock *global-calling-object*)
           (list 'give-x-points ,code))))

(defmethod compile-me ((self C-patch-osc) obj)
  (let ((code (compile-me (car (input-objects self)) obj)))
    `(list 'bpf-out-osc ,code '(clock *global-calling-object*) (list 'give-x-points ,code))))

(defmethod compile-me ((self C-patch-osc-period) obj)
  (let ((code (compile-me (car (input-objects self)) obj))
        (x-val (if (eql (second (pw-controls self)) (second (input-objects self)))
                   `',(patch-value (second (input-objects self)) obj)
                   (compile-me (second (input-objects self)) obj))))
    `(list 'bpf-out-osc-period ,code '(clock *global-calling-object*)
           ,x-val (list 'give-x-points ,code))))

(defmethod compile-me ((self  C-patch-osc-phase) obj)
  (let ((code (if (eql (car (input-objects self)) (car (pw-controls self)))
                  '(make-break-point-function '(0 0) '(0 0))
                  (compile-me (car (input-objects self)) obj)))
        (period (if (eql (second (input-objects self)) (second (pw-controls self)))
                    `',(patch-value (second (input-objects self)) obj)
                    (compile-me (second (input-objects self)) obj)))
        (fun
          `(let ((*osc-phase* 0)
                 (*old-time* 0))
             (lambda (code period)
               (declare (special *global-calling-object*))
               (let* ((points (give-x-points code))
                      (time-diff (- (car (last points)) (car points))))
                 (prog1
                     (bpf-out code
                              (+ (setf *osc-phase*
                                       (float (mod (+ *osc-phase*
                                                      (/ (* time-diff (- (clock *global-calling-object*)
                                                                         *old-time*)) period))
                                                   time-diff)))
                                 (car points))
                              points)
                   (setf *old-time* (clock *global-calling-object*))))))))
    `(list 'funcall ,fun ,code ,period)))

(defmethod compile-me ((self C-pw-test) obj)
  ;;(format t "PW WARNING: Compilation of a test box produces an (almost) equivalent
  ;;function that evaluates all entries") (ui:ed-beep)
  (let* ((objects (input-objects self))
         (controls (pw-controls self))
         (fun (if (eql (first objects) (first controls))
                  `',(patch-value (first objects) obj)
                  (compile-me (first objects) obj)))
         (input (if (eql (second objects) (second controls))
                    `',(patch-value (second objects) obj)
                    (compile-me (second objects) obj)))
         (length (length objects))
         cond-body)
    (for (k 2 2 (- length 2))
      (push `(list (list ,fun ,input
                         ,(if (eql (nth k objects) (nth k controls))
                              `',(patch-value (nth k objects) obj)
                              (compile-me (nth k objects) obj)))
                   ,(if (eql (nth (1+ k) objects) (nth (1+ k) controls))
                        `',(patch-value (nth (1+ k) objects) obj)
                        (compile-me (nth (1+ k) objects) obj))) cond-body))
    (push `(list t ,(if (eql (nth (1- length) objects) (nth (1- length) controls))
                        `',(patch-value (nth (1- length) objects) obj)
                        (compile-me (nth (1- length) objects) obj))) cond-body)
    `(list 'cond ,@ (nreverse cond-body))))


(defmethod compile-me ((self C-pw-loop) obj)
  (let ((times (if (eql (first (input-objects self)) (first (pw-controls self)))
                   `',(patch-value (first (input-objects self)) obj)
                   (compile-me (car (input-objects self)) obj)))
        (code (if (eql (second (input-objects self)) (second (pw-controls self)))
                  `',(patch-value (second (input-objects self)) obj)
                  (compile-me (second (input-objects self)) obj)))
        (var-times (intern (format () "~S" (gensym))))
        (var-list (intern (format () "~S" (gensym)))))
    `(list 'let (list ',var-list)
           (list 'dotimes (list ',var-times ,times (list 'nreverse ',var-list))
                 (list 'push ,code ',var-list)))))

;; (setq foo (subviews *active-patch-window*))
;; (code (car (last foo)))
;; (ask-all foo 'abstract (car foo))

;;;; THE END ;;;;
