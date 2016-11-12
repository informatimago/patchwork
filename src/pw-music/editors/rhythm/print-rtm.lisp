;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               print-rtm.lisp
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
(in-package :pw)

(defmethod window-hardcopy ((self pw::C-rtm-editor-window) &optional show-fl)
  (niy window-hardcopy self show-fl)
  ;; show-fl
  ;; (unwind-protect
  ;;   (with-cursor *arrow-cursor*
  ;;     (#_PrOpen)
  ;;     (prchk $err-printer-load)
  ;;     (let ((pRec (get-print-record)) (scroll-pos 0))
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
  ;;                  (while (numberp scroll-pos)
  ;;                    (unwind-protect
  ;;                      (progn
  ;;                        (set-window-hardcopy-wptr self hardcopy-ptr)
  ;;                        (#_PrOpenPage :ptr hardcopy-ptr :long 0)
  ;;                        (with-port hardcopy-ptr
  ;;                          (setq scroll-pos(print-all-subviews self)))
  ;;                        (set-window-hardcopy-wptr self window-ptr)
  ;;                        )
  ;;                      (#_PrClosePage :ptr hardcopy-ptr)
  ;;                      )
  ;;                    (when  scroll-pos (pw::scroll-beat (pw::editor-collection-object self) scroll-pos))))
  ;;                (#_PrCloseDoc  :ptr hardcopy-ptr)))
  ;;            (when (print (eql (%hget-byte pRec $prJob.bjDocLoop)
  ;;                             $bSpoolLoop))
  ;;              (prchk)
  ;;              (%stack-block ((StRec $iPrStatSize))
  ;;                (#_PrPicFile :ptr pRec :long 0 :long 0 :long 0 :ptr StRec))
  ;;              (prchk)))  ;))
  ;;           t))))
  ;;   (#_PrClose))
  )

(defmethod print-all-subviews ((self pw::C-rtm-editor-window))
  (let ((views (subviews (pw::editor-collection-object self)))
        measures)
    (dolist (view views)
      (if (typep view 'pw::C-beat-editor-panel)
        (push (pw::print-draw-contents view) measures)))
    (car (remove nil
       (mapcar #'position (mapcar #'car  (nreverse measures)) 
          (ask-all (ask-all (pw::draw-beat-editor-objects (pw::editor-collection-object self)) #'pw::measure-line) #'pw::measures))))))
