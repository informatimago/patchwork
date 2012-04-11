;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;=========================================================
;;;;
;;;;  PATCH-WORK
;;;;  By Mikael Laurson, Jacques Duthen, Camilo Rueda.
;;;;  © 1986-1992 IRCAM 
;;;;
;;;;=========================================================

(in-package :pw)

(provide 'MN-editor-polif)

;====================================================================================================

(defun make-polif-arg-list (count)
  (let ((arg-list))
    (for (i 0 1 (1- count)) 
         (push  '*MN-collector-type* arg-list)
         (push  (string-downcase 
                  (concatenate  'string  "coll" (format nil "~D" (1+ i)))) arg-list))
    (nreverse arg-list)))

;(make-polif-arg-list 6)
        
;====================================================================================================
(defclass  C-patch-PolifMN (C-patch-application)
  ((chord-line-list :initform nil :initarg :chord-line-list :accessor chord-line-list)))

(defmethod make-application-object ((self C-patch-PolifMN))
  (make-n-music-notation-editors (length (pw-controls self))))

;_________

(defmethod draw-patch-extra :after ((self C-patch-PolifMN))
  (draw-char (+ -16 (w self)) (- (h self) 4) #\E)) 
;_________
; extend

(defmethod correct-extension-box ((self C-patch-PolifMN) new-box values)
  (declare (ignore values))
  (let ((editors (editor-objects (car (subviews (application-object self)))))
        (new-editors-list (editor-objects (car (subviews (application-object new-box))))))
    (for (i 0 1 (1- (length editors)))
       (setf (chord-line (nth i new-editors-list)) (chord-line (nth i editors))))))

;(defmethod generate-extended-inputs ((self C-patch-PolifMN)) (call-next-method))

(defmethod give-new-extended-title ((self C-patch-PolifMN)) 'pmnn) 

(defmethod mouse-pressed-no-active-extra :after ((self C-patch-PolifMN) x y) 
  (declare (ignore x y))
  (when (option-key-p) 
    (remove-yourself-control self)))
;_________

(defmethod patch-value ((self C-patch-PolifMN) obj)
  (declare (ignore obj))
  (let ((editors (editor-objects (car (subviews (application-object self))))))
    (for (i 0 1 (1- (length editors)))
      (when (not (eq (nth i (pw-controls self))(nth i (input-objects self))))
        (setf (chord-line (nth i editors))
          (give-MN-editor-chord-line (nth i (input-objects self)) i))
        (view-draw-contents (nth i editors)))))
    (application-object self))

;=================================================================
