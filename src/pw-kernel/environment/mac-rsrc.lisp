;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;=========================================================
;;;;
;;;;  PATCH-WORK
;;;;  By Mikael Laurson, Jacques Duthen, Camilo Rueda.
;;;;  © 1986-1992 IRCAM 
;;;;
;;;;=========================================================

(in-package :pw)

;=================================================================================================
; 


(defun load-lisp-res ()
  (CCL:with-pstrs ((name (mac-namestring "CL:PW-inits;Drivers+Resources;CLPF.rsrc")))
    (when (= -1 (#_OpenResFile :ptr name :word))
      (error "Cannot open resource file!")))
  )

;BOX-CURSOR
(defvar *not-box-cursor* t)
(defvar *box-cursor* nil)

(defun get-box-cursor () 
  (ccl::with-pstrs ((name "box-cursor"))
  (setf *box-cursor* (#_getnamedresource :|CURS| name))))

(load-lisp-res)

;(new-restore-lisp-function 'load-lisp-res)

;=================================================================================================

(defun nth? (elem lst)
  (let ((rest-lst (memq elem lst)))
    (when rest-lst
      (- (length lst) (length rest-lst)))))

;======================================

(defun make-cursor (data-string mask-string hotspot)
  (when (or (> (length (string data-string)) 64)
            (> (length (string mask-string)) 64))
    (error "data-string & mask-string must be < 64 chars long"))
  (rlet ((data :bits16)
         (mask :bits16))
    (with-pstrs ((data-str data-string)
                 (mask-str mask-string))
      (#_StuffHex :ptr data :ptr data-str)
      (#_StuffHex :ptr mask :ptr mask-str))
    (make-record :cursor
               :data data
               :mask mask
               :hotspot hotspot)))

(defun make-grow-cursor ()
  (make-cursor "00003FC02040204027F82448244824483FC80408040807F80000000000000000"
               "00003FC03FC03FC03FF83FF83FF83FF83FF807F807F807F80000000000000000"
               #@(2 3)))

(defvar *grow-cursor* (make-grow-cursor))

;the cross-hair-cursor
(defun make-cross-hair-cursor ()
  (make-cursor "04000400040004000400FFE00400040004000400040004000000000000000000"
               "0000000000000000000000000000000000000000000000000000000000000000"
               #@(5 5)))

(defvar *cross-hair-cursor* (make-cross-hair-cursor))

(defun init-cursors ()
  (setq *grow-cursor* (make-grow-cursor))
  (setq *cross-hair-cursor* (make-cross-hair-cursor)))

;======================================
