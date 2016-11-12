;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               bpf-menu+library.lisp
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

;;============================================================================================
(defclass  C-menubox-bpf (ui:view ; needed to add subviews to it.
                          C-menubox)
  ((BPF-mini-view :initform (make-instance 'C-mini-bpf-view) :accessor BPF-mini-view)))

(defmethod initialize-instance :after ((self C-menubox-bpf) &rest ctrls)
  ctrls
  (add-subviews self (BPF-mini-view self))
  (set-view-position (BPF-mini-view self) 0 0)
   (set-view-size (BPF-mini-view self) (w self) (h self)))

(defmethod menubox-value ((self C-menubox-bpf))
   (let ((bpf-list (bpf-lib-objects (menu-box-list self))))
       (nth (mod (value self)(length bpf-list)) bpf-list)))

(defmethod set-numbox-item-text  ((self C-menubox-bpf) value)
  (declare (ignore value)))

;;=========================
;;draw

(defmethod view-draw-contents ((self C-menubox-bpf))
  (with-focused-view self
    (if (open-state self)
       (let ((bpf-mini (BPF-mini-view self)))
          (set-break-point-function-to-mini bpf-mini (menubox-value self))
          (erase-view-inside-rect self)  
          (view-draw-contents bpf-mini)) 
       (draw-string 3 9 (doc-string self)))))

(defmethod view-click-event-handler ((view C-menubox-bpf) where)
 (declare (ignore where))
 (when (open-state view)
    (let* ((win (view-window view))
           (first-v (point-v (view-mouse-position win)))
           (last-mp (view-mouse-position win))
           (last-value (value view)))
      ;(pfloop                                 ;!!
      (loop
        (event-dispatch)                                 ;!!
        (unless (mouse-down-p) (return))
        (let ((mp (view-mouse-position win)))
          (unless (eql mp last-mp)
            (setq last-mp mp)
               (setf (value view)
                  (+ last-value 
                    (* (map-mouse-increment view) (- first-v (point-v last-mp)))))
            (view-draw-contents view)))))))

;;============================================================================================

;;(defmethod patch-value ((self C-menubox-bpf) obj) (value-str self))

;;============================================================================================

(defclass C-pw-BPF-library ()
  ((bpf-lib-objects :initform nil :initarg :bpf-lib-objects :accessor bpf-lib-objects)
   (lib-name :initform nil :initarg :lib-name :accessor lib-name)))

(defgeneric library-decompile (self)
  (:method ((self C-break-point-function))
    `(add-new-bpf-lib-object *pw-BPF-library*
                             ,(decompile self))))

(defmethod decompile ((self C-pw-BPF-library))
  `(progn ,@(ask-all (bpf-lib-objects self) 'library-decompile)))

(defgeneric add-new-bpf-lib-object (self lib-object)
  (:method ((self C-pw-BPF-library) lib-object)
    (push lib-object (bpf-lib-objects self))))

;;============================================================================================

(defparameter *pw-BPF-library* (make-instance 'C-pw-BPF-library :lib-name '*pw-BPF-library*))

;;(bpf-lib-objects *pw-BPF-library*)

(add-new-bpf-lib-object *pw-BPF-library*
   (make-instance 'C-break-point-function :break-point-list
     (list 
       (make-point 0 0)
       (make-point 100 100))))

(add-new-bpf-lib-object *pw-BPF-library*
   (make-instance 'C-break-point-function :break-point-list
     (list 
       (make-point 0 50)
       (make-point 100 0))))

(add-new-bpf-lib-object *pw-BPF-library*
   (make-instance 'C-break-point-function :break-point-list
     (list 
       (make-point 0 0)
       (make-point 100 100)
       (make-point 200 0))))

(add-new-bpf-lib-object *pw-BPF-library*
   (make-instance 'C-break-point-function :break-point-list
     (list 
       (make-point -30 -50)
       (make-point 10 100)
       (make-point 20 80)
       (make-point 80 60)
       (make-point 100 0))))


;; (setq w2 (make-instance 'window :window-title "Test-bpflib"))
;; 
;; (setq bpfmenu1 (make-instance 'C-menubox-bpf 
;;    :view-position (make-point 50 150) :view-size (make-point 74 54) :doc-string "BPFlib"
;;                     :menu-box-list *pw-BPF-library*))
;; 
;; (add-subviews w2 bpfmenu1)
;; 
;; (setq bpfmenu2 (make-instance 'C-menubox-bpf 
;;     :view-position (make-point 50 20) :view-size (make-point 74 54) :doc-string "BPFlib"
;;                     :menu-box-list *pw-BPF-library*))
;; (add-subviews w2 bpfmenu2)
;; 
;; (setq bpfmenu3 (make-instance 'C-menubox-bpf 
;;     :view-position (make-point 140 5) :view-size (make-point 84 34) :doc-string "BPFlib"
;;                     :menu-box-list *pw-BPF-library*))
;; (add-subviews w2 bpfmenu3)
;; ;;(patch-value bpfmenu1 ())

;;==============================================================================


(defun add-BPF-to-lib (win lib)
  (add-new-bpf-lib-object lib
    (eval (decompile (break-point-function (BPF-editor-object win))))))

(defun save-BPF-lib (lib)
  (setq *print-pretty* ())
  (let* ((new-name (choose-new-file-dialog :directory  "BPF-lib" :prompt "Save BPF lib As…")))
     (delete-file new-name)
     (WITH-OPEN-FILE (out new-name :direction :output :if-does-not-exist :create :if-exists :supersede)
       (prin1 '(in-package :pw) out)
        (let ((*package* :pw))
          (prin1 (decompile lib) out))))
  (setq *print-pretty* t))

(defun load-BPF-lib ()
  (let ((name (CHOOSE-FILE-DIALOG :button-string "Load BPF lib")))
    (load name :verbose t)))

(defun reset-BPF-lib (lib) 
  (setf (bpf-lib-objects lib) ()) 
  (setf (bpf-lib-pointer *active-BPF-window*) 0)
  (add-new-bpf-lib-object lib
    (make-instance 'C-break-point-function :break-point-list
     (list 
       (make-point 0 0)
       (make-point 100 100)))))

(defun next-from-BPF-lib (win lib &optional dec-num)
   (setf (bpf-lib-pointer win) 
      (mod (+ (bpf-lib-pointer win) (if dec-num -1 1))
            (length (bpf-lib-objects lib))))
   (let ((bpf (nth (bpf-lib-pointer win) (bpf-lib-objects lib)))
         (BPF-editor-object (BPF-editor-object win)))
     (when (mini-view BPF-editor-object)
        (set-break-point-function-to-mini (mini-view BPF-editor-object) bpf))
     (add-bpf-to-bpf-editor-from-PW win bpf)
     (update-bpf-view BPF-editor-object)))

;;;; THE END ;;;;
