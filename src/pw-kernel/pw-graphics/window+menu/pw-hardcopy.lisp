;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pw-hardcopy.lisp
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

;;
;; Printing PW windows
;; 

(in-package :pw)

(eval-when (eval load compile)
  ;; (require 'traps)
  ;(load-once "ui:Examples;NotInROM;NotInROM")
  ;(load-once "ui:Library;interfaces;SERIAL")
  (defconstant $PrintErr #x944)  
  (defconstant $prJob.bjDocLoop (+ 62 6))
  (defconstant $iPrStatSize 26)
  (defconstant $bSpoolLoop 1)
  (defconstant $err-printer 94)
  (defconstant $err-printer-load 95)
  (defconstant $err-printer-start 97)
)

;;(import '(traps::$baud9600 traps::$data8 traps::$stop20 traps::$noParity
;;          traps::$boutrefnum traps::$sPortB))
;;
;;(defun reset-printer-port()
;;  (#~SerReset $boutRefNum 
;;              (+ $baud9600 $data8 $stop20 $noParity)))

(defmethod set-window-hardcopy-wptr ((self simple-view) wptr)
 (set-wptr self wptr))

(defmethod set-window-hardcopy-wptr ((self view) wptr)
 (set-wptr self wptr)
 (tell (subviews self) 'set-window-hardcopy-wptr wptr))

(defmethod win-print-setUp ((self window))
  (ui:uiwarn "~S ~S is not implemented yet" 'win-print-setUp '((self window)))
  ;; (unwind-protect
  ;;   (with-cursor *arrow-cursor*
  ;;     (#_PrOpen)
  ;;     (prchk $err-printer-load)
  ;;     (#_PrStlDialog :ptr (get-print-record) :boolean))
  ;;   (#_PrClose))
  )
  
(defmethod window-hardcopy ((self pw::C-pw-window) &optional show-fl)
  (ui:uiwarn "~S ~S is not implemented yet" 'window-hardcopy '((self pw::C-pw-window) &optional show-fl))

  show-fl
  ;; (unwind-protect
  ;;   (with-cursor *arrow-cursor*
  ;;     (#_PrOpen)
  ;;     ;;;(reset-printer-port)
  ;;     (prchk $err-printer-load)
  ;;     (let ((pRec (get-print-record)))
  ;;       (when (#_PrJobDialog :ptr (get-print-record) :boolean)
  ;;         (let ((*hc-page-open-p* nil) (ui::*inhibit-error* t)) ; err)
  ;;           (declare (special *hc-page-open-p* ui::*inhibit-error*))
  ;;           (without-interrupts
  ;;            (let* ((window-ptr (wptr self))
  ;;                   (hardcopy-ptr 
  ;;                    (#_PrOpenDoc :ptr (get-print-record) :long 0 :long 0 :ptr)))
  ;;              (unwind-protect
  ;;                (with-dereferenced-handles ((ppRec PRec))
  ;;                  pprec
  ;;                  (prchk $err-printer-start)
  ;;                  (unwind-protect
  ;;                     (progn
  ;;                       (set-window-hardcopy-wptr self hardcopy-ptr)
  ;;                       (#_PrOpenPage :ptr hardcopy-ptr :long 0)
  ;;                       (with-port hardcopy-ptr
  ;;                         (print-all-subviews self))
  ;;                       (set-window-hardcopy-wptr self window-ptr)
  ;;                       )
  ;;                    (#_PrClosePage :ptr hardcopy-ptr)
  ;;                    ))
  ;;                (#_PrCloseDoc  :ptr hardcopy-ptr)))
  ;;            (when (print (eq (%hget-byte pRec $prJob.bjDocLoop)
  ;;                             $bSpoolLoop))
  ;;              (prchk)
  ;;              (%stack-block ((StRec $iPrStatSize))
  ;;                (#_PrPicFile :ptr pRec :long 0 :long 0 :long 0 :ptr StRec))
  ;;              (prchk)))  ;))
  ;;           t))))
  ;;   (#_PrClose))
  )
    
(defmethod print-all-subviews ((self pw::C-pw-window))
  (let ((views (subviews self)))
    (tell views 'pw::print-connections)
    (dolist (a-view views)
      (set-view-font  (view-container a-view) '("Monaco"  9  :srcor))
      (view-draw-contents a-view))))

#|
                          
;;(window-hardcopy pw::*active-mn-window*)
;;(window-hardcopy pw::*active-bpf-window*)
;;(window-hardcopy  pw::*active-patch-window*)
;;(pw::super-win (view-window (car (subviews (car (subviews pw::*active-mn-window*))))))
;;(win-print-setUp  pw::*active-patch-window*)

|#
