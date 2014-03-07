;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               mac-rsrc.lisp
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
;;;;    
;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;=========================================================
;;;;
;;;;  PATCH-WORK
;;;;  By Mikael Laurson, Jacques Duthen, Camilo Rueda.
;;;;  © 1986-1992 IRCAM 
;;;;
;;;;=========================================================

(in-package :pw)
(enable-patchwork-reader-macros)

;;=================================================================================================
;; 


(defun load-lisp-res ()
  (niy load-lisp-res)
  ;; (ui:with-pstrs ((name (mac-namestring "CL:PW-inits;Drivers+Resources;CLPF.rsrc")))
  ;;   (when (= -1 (#_OpenResFile :ptr name :word))
  ;;     (error "Cannot open resource file!")))
  )

;;BOX-CURSOR
(defvar *not-box-cursor* t)
(defvar *box-cursor* nil)

(defun get-box-cursor ()
  (niy get-box-cursor)
  ;; (ui::with-pstrs ((name "box-cursor"))
  ;; (setf *box-cursor* (#_getnamedresource :|CURS| name)))
  )

(load-lisp-res)

;;(new-restore-lisp-function 'load-lisp-res)

;;=================================================================================================

(defun nth? (elem lst)
  (let ((rest-lst (memq elem lst)))
    (when rest-lst
      (- (length lst) (length rest-lst)))))

;;======================================


(defun make-grow-cursor ()
  (make-cursor "grow"
               2 3
               #(#*0000000000000000
                 #*0011111111000000
                 #*0010000001000000
                 #*0010000001000000
                 #*0010011111111000
                 #*0010010001001000
                 #*0010010001001000
                 #*0010010001001000
                 #*0011111111001000
                 #*0000010000001000
                 #*0000010000001000
                 #*0000011111111000
                 #*0000000000000000
                 #*0000000000000000
                 #*0000000000000000
                 #*0000000000000000)
               #(#*0000000000000000
                 #*0011111111000000
                 #*0011111111000000
                 #*0011111111000000
                 #*0011111111111000
                 #*0011111111111000
                 #*0011111111111000
                 #*0011111111111000
                 #*0011111111111000
                 #*0000011111111000
                 #*0000011111111000
                 #*0000011111111000
                 #*0000000000000000
                 #*0000000000000000
                 #*0000000000000000
                 #*0000000000000000)))



(defvar *grow-cursor* (make-grow-cursor))

;;the cross-hair-cursor
(defun make-cross-hair-cursor ()
  (make-cursor "hair"
               5 5
               #(#*0000010000000000
                 #*0000010000000000
                 #*0000010000000000
                 #*0000010000000000
                 #*0000010000000000
                 #*1111111111100000
                 #*0000010000000000
                 #*0000010000000000
                 #*0000010000000000
                 #*0000010000000000
                 #*0000010000000000
                 #*0000010000000000
                 #*0000000000000000
                 #*0000000000000000
                 #*0000000000000000
                 #*0000000000000000)
               #(#*0000000000000000
                 #*0000000000000000
                 #*0000000000000000
                 #*0000000000000000
                 #*0000000000000000
                 #*0000000000000000
                 #*0000000000000000
                 #*0000000000000000
                 #*0000000000000000
                 #*0000000000000000
                 #*0000000000000000
                 #*0000000000000000
                 #*0000000000000000
                 #*0000000000000000
                 #*0000000000000000
                 #*0000000000000000)))

(defvar *cross-hair-cursor* (make-cross-hair-cursor))

(defun init-cursors ()
  (setq *grow-cursor* (make-grow-cursor))
  (setq *cross-hair-cursor* (make-cross-hair-cursor)))

;;======================================
(defun hex-to-bitarray (hexes)
  (coerce (loop
            for i from 0 below (length hexes) by 4
            for b = (parse-integer hexes :radix 16 :start i :end (+ i 4))
            for v = (make-array 16 :element-type 'bit)
            do (loop for j from 0 to 15
                     do (setf (aref v j) (if (logbitp (- 15 j) b) 1 0)))
            collect v) 'vector))

