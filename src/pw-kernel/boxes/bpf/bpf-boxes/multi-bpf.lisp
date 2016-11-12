;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               multi-bpf.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;  
;;;;    XXX
;;;;  
;;;;AUTHORS
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

;;================================================================================================================
(defclass C-multi-bpf-view (C-bpf-view)
  ((break-point-functions :initform nil :initarg :break-point-functions  :accessor break-point-functions)
   (break-point-functions-pointer :initform 0 :initarg :break-point-functions-pointer  :accessor break-point-functions-pointer)))

(defmethod view-draw-contents ((self C-multi-bpf-view))
  (set-pen-pattern self *dark-gray-pattern*)
  (with-focused-view self
    (let ((bpfs (break-point-functions self)))
      (while bpfs 
        (draw-bpf-function   (pop bpfs) self nil (h-view-scaler self)(v-view-scaler self)))))
  (set-pen-pattern self *black-pattern*)
  (call-next-method))

(defmethod print-draw-contents ((self C-multi-bpf-view))
  (set-pen-pattern self *dark-gray-pattern*)
  (with-focused-view self
    (dolist (bpf (break-point-functions self))
      (draw-bpf-function bpf self nil (h-view-scaler self)(v-view-scaler self))))
  (set-pen-pattern self *black-pattern*)
  (call-next-method))

(defmethod key-pressed-BPF-editor ((self C-multi-bpf-view) char)
  (case char
    (#\s 
        (let ((bpf (nth (break-point-functions-pointer self) (break-point-functions self))))
          (when bpf 
              (setf (break-point-functions self) (remove bpf (break-point-functions self)))
              (push (break-point-function self) (break-point-functions self))
              (setf  (break-point-function self) bpf)
              (set-break-point-function-to-mini (mini-view self) bpf)
              (setf (break-point-functions-pointer self) (mod (1+ (break-point-functions-pointer self)) (length (break-point-functions self))))
              (erase+view-draw-contents self)
              (update-mini-view (mini-view self)))))
    (#\a (push (eval (decompile (break-point-function self))) (break-point-functions self)))
    (#\d (when (break-point-functions self)
        (let ((bpf (nth (break-point-functions-pointer self) (break-point-functions self))))
              (setf (break-point-functions self) (remove bpf (break-point-functions self)))
              (setf  (break-point-function self) bpf)
              (set-break-point-function-to-mini (mini-view self) bpf)
              (if (break-point-functions self)
                (setf (break-point-functions-pointer self) (mod (1+ (break-point-functions-pointer self)) (length (break-point-functions self))))
                (setf (break-point-functions-pointer self) 0))
              (erase+view-draw-contents self)
              (update-mini-view (mini-view self)))))
    (t (call-next-method))))

;;================================================================================================================

(defclass  C-patch-multi-function (C-patch-function) ()) 

(defmethod make-application-object ((self C-patch-multi-function))
  (setf (application-object self)
        (make-BPF-editor (make-break-point-function '(0 100) '(0 100)) 'C-multi-bpf-view)))

#|(defmethod decompile ((self C-patch-multi-function))
  (let ((temp (call-next-method))
        (bpfs (break-point-functions (editor-view-object (application-object self)))))
    `(,@(butlast temp)
        ',(cons (eval (car (last temp))) (ask-all bpfs #'break-point-list )))))|#

(defmethod decompile ((self C-patch-multi-function))
  (let ((temp (call-next-method))
        (bpfs (cons (break-point-function (give-mini-bpf self)) 
                    (break-point-functions (editor-view-object (application-object self))))))
    `(,@(butlast temp)
        ',(cons (eval (car (last temp)))  (list (ask-all bpfs #'break-point-list ))))))

#|(defmethod complete-box ((self C-patch-multi-function) args)
  (let ((mini-bpf (give-mini-bpf self)) bpfs)
    (setf (break-point-function mini-bpf)
          (make-instance 'C-break-point-function :break-point-list (pop args)))
    (while args
      (push (make-instance 'C-break-point-function :break-point-list (pop args)) bpfs))
    (when bpfs (setf (break-point-functions (editor-view-object (application-object self))) bpfs))  
    (set-break-point-function-to-mini mini-bpf (break-point-function mini-bpf))
    (add-bpf-to-bpf-editor-from-PW (application-object self) (break-point-function mini-bpf))
    (set-mini-view (application-object self) mini-bpf)
    self))|#

(defmethod complete-box ((self C-patch-multi-function) list)
  (let ((state (first list)) (args (second list))
        (*no-line-segments* (display-only-points self)))
    (setf (points-state self) (second state))
    (set-output self (third state))
    (let ((mini-bpf (give-mini-bpf self)) bpfs)
      (setf (break-point-function mini-bpf)
            (make-instance 'C-break-point-function :break-point-list (pop args)))
      (while args
        (push (make-instance 'C-break-point-function :break-point-list (pop args)) bpfs))
      (when bpfs (setf (break-point-functions (editor-view-object (application-object self))) bpfs))  
      (set-break-point-function-to-mini mini-bpf (break-point-function mini-bpf))
      (add-bpf-to-bpf-editor-from-PW (application-object self) (break-point-function mini-bpf))
      (set-mini-view (application-object self) mini-bpf)
      self)))

(defmethod set-output ((self C-patch-multi-function) o-type)
  (cond ((and (eql o-type :bpf) (not (eql (out-type self) :bpf)))
         (setf (type-list self) '(bpf list))
         (erase-my-connections self))
        ((and (eql (out-type self) :bpf) (not (eql o-type :bpf)))
         (setf (type-list self) '(list))
         (erase-my-connections self)))
  (setf (out-type self) o-type)
  (setf (current-str self)
        (case o-type
          (:bpf #\B) (:x-points #\X) (:y-points #\Y)))
  (set-box-title (popUpBox self) "  ")
  (draw-appl-label self (current-str self))
  )

(defun fill-to-equal-length-lst (a b)
  (let ((short-l (if (< (length a) (length b)) a b)))
     (append short-l 
        (make-list (abs (- (length a) (length b))) :initial-element (car (last short-l))))))

;;(fill-to-equal-length-lst '(1 2 3) '(7 8 9 8 7))
;;(fill-to-equal-length-lst '(1 2 3 8 9 8 9 0) '(7 8 9 8 7))
;;(fill-to-equal-length-lst '(1 2 3) '(7 8 9))

#|
(defun convert-to-lst-lst (a)
  (cond ((atom a) (list a))
        ((and (listp a) (atom (car a))) (list a)) 
        ((and (listp a) (listp (car a)) (atom (caar a))) a)
        (t nil)))

(defmethod patch-value ((self C-patch-multi-function) obj)
  (when (or (nth-connected-p self 0)
            (nth-connected-p self 1))
    (let ((ts (g-round (patch-value (first (input-objects self)) obj)))
          (vs (g-round (patch-value (second (input-objects self)) obj) ))
          bpf bpfs)
      (if (and (listp vs) (print (eql (type-of (car vs)) 'C-break-point-function)))
        (progn
          (setq bpfs (mapcar #'eval (mapcar #'decompile vs)))
          (setq bpf (pop bpfs))
          (setf (break-point-function (give-mini-bpf self)) bpf)
          (update-mini-view (give-mini-bpf self))
          (add-bpf-to-bpf-editor-from-PW (application-object self) bpf)
          (setf (break-point-functions (editor-view-object (application-object self))) bpfs))
        (progn
          (setq ts (convert-to-lst-lst ts))
          (setq vs (convert-to-lst-lst vs))
          (if (> (length ts) (length vs)) 
            (setq vs (fill-to-equal-length-lst ts vs))
            (setq ts (fill-to-equal-length-lst ts vs)))
          (while (and ts vs)
            (if (not bpf) 
              (progn 
                (setq bpf (make-break-point-function (car ts) (car vs)))
                (setf (break-point-function (give-mini-bpf self)) bpf)
                (update-mini-view (give-mini-bpf self))
                (add-bpf-to-bpf-editor-from-PW (application-object self) bpf))
              (push  (make-break-point-function (car ts) (car vs)) bpfs))
            (pop ts)(pop vs))   
          (setf (break-point-functions (editor-view-object (application-object self))) bpfs)))))
  (cons (break-point-function (give-mini-bpf self)) 
        (break-point-functions (editor-view-object (application-object self)))))|#

;;So that it really becomes generic [Camilo 930113]

(defgeneric convert-to-lists (self))
(defmethod convert-to-lists ((self number)) (list (list self)))

(defmethod convert-to-lists ((self cons)) (if (consp (first self)) self (list self)))

(defmethod patch-value ((self C-patch-multi-function) obj)
  (let (bpfs)
    (if (and (not (value self)) (or (nth-connected-p self 0) (nth-connected-p self 1)))
      (let ((ts (epw::g-round (patch-value (first (input-objects self)) obj)))
            (vs (epw::g-round (patch-value (second (input-objects self)) obj) )))
        (setq bpfs (construct-bpfs-objects vs ts))
        (setf (break-point-function (give-mini-bpf self)) (first bpfs))
        (update-mini-view (give-mini-bpf self))
        (add-bpf-to-bpf-editor-from-PW (application-object self) (first bpfs))
        (setf (break-point-functions (editor-view-object (application-object self))) (rest bpfs)))
      (setq bpfs (cons (break-point-function (give-mini-bpf self)) 
                       (break-point-functions (editor-view-object (application-object self))))))
    (case (out-type self)
    (:bpf (if (second bpfs) bpfs (first bpfs)))
    (:x-points (mapcar (lambda (bpf) (slot-value bpf 'X-points)) bpfs))
    (:y-points (mapcar (lambda (bpf) (slot-value bpf 'Y-points)) bpfs)))))

(defgeneric construct-bpfs-objects (self &optional ts-exp))
(defmethod construct-bpfs-objects ((self number) &optional ts-exp)
  (let* ((ts (convert-to-lists ts-exp))
         (vs (fill-to-equal-length-lst (first ts) (list self))))
    (list (make-break-point-function (car ts) vs))))

(defmethod construct-bpfs-objects ((self cons) &optional ts-exp)
  (if (subtypep (type-of (first self)) 'C-break-point-function)
    (mapcan #'construct-bpfs-objects self)
    (let ((vs-exp (convert-to-lists self))
          (ts-exp (convert-to-lists ts-exp)))
      (setq ts-exp (equalize-lists vs-exp ts-exp))
      (mapcar (lambda (vs-list ts-list) (make-break-point-function ts-list vs-list))
              vs-exp ts-exp))))

(defmethod construct-bpfs-objects ((self C-break-point-function) &optional ts-exp)
  (declare (ignore ts-exp))
  (list (eval (decompile self))))

(defun equalize-lists (list1 list2)
  (let (res2 (default (first (last (first (last list2))))))
    (do ((sub1 list1 (cdr sub1)) (sub2 list2 (cdr sub2)))
        ((null sub1) (nreverse res2))
      (cond ((null sub2) (push default res2))
            ((not (rest (first sub2))) (push (first (first sub2)) res2))
            ((> (length (first sub1)) (length (first sub2)))
             (push (fill-to-equal-length-lst (first sub1) (first sub2)) res2))
            (t (push (butlast (first sub2) (- (length (first sub2))(length (first sub1)))) res2))))))

;;================================================================================================================
;;from ...:bpf:bpf-editors:bpf-mini-view
;;=====================================
;;draw

(defgeneric break-point-functions (self)
  (:method ((self simple-view))
    nil))

(defmethod application-object ((self C-menubox-bpf)) (declare (ignore self)) nil)


;; reversed drawing 1st bpfs then bpf
(defmethod view-draw-contents ((self C-mini-bpf-view))
  (let* ((object (application-object (view-container self)))
         (bpfs (and object (break-point-functions (editor-view-object object)))))
    (with-focused-view self
      (draw-rect* (point-h (view-scroll-position self)) (point-v (view-scroll-position self))(w self)(h self))
      (if (open-state self)
        (progn
          (when bpfs
            (set-pen-pattern self *light-gray-pattern*)
            (while bpfs
              (draw-bpf-function 
               (pop bpfs) self nil (h-view-scaler self)(v-view-scaler self)))
            (set-pen-pattern self *black-pattern*)) 
           (when (break-point-function self)
             (draw-bpf-function 
               (break-point-function self) self nil (h-view-scaler self)(v-view-scaler self))))
        (draw-string 3 9 (doc-string self))))))

;;================================================================================================================

(defunp multi-bpf ((tlist (fix>0s? (:value 10))) (vl/bpfs list (:value 100 :type-list (bpf list)))
             (mini-bpf bpf)) list
"The multi-bpf  module can be used to create and edit 
simultaneous breakpoint functions an once, create and edit 
coordinate pairs (x,y), display a series of coordinate pairs, 
either as a BPF or as a series of points, save and load multi-bpf
modules  to and from a library and retrieve data concerning the points 
contained in the multi-bpf  module. A function editor can be opened by 
selecting this box and typing o from the keyboard.
 The multi-bpfcan be changed from PatchWork by connecting a value-list to the vl/bpf
 (or a list of lists-values) input box and option-clicking its output box.
 If there is no connection in the first input box tlist then the points have a constant
 time-difference.If the first input box tlist  is connected, then the input should be a
list of ascending timepoints. One can change the representation of the modulemulti-bpf
  (in segments by default) to a representation in points by selecting flip-mode in the
 front menu. Click on the A of the module to open the front menu. For more information,
 type h with the window of the module open."
  (declare (ignore tlist vl/bpfs mini-bpf)))

;;(PW-addmenu-fun *pw-BPF-menu* 'multi-bpf  'C-patch-multi-function)
