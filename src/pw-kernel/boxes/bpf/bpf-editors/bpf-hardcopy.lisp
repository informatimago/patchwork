;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               bpf-hardcopy.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;    
;;;;    Printing MN windows 
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


;; TODO:
#||

(defmethod window-hardcopy ((self pw::C-BPF-window) &optional show-fl)
  show-fl
  (unwind-protect
    (with-cursor *arrow-cursor*
      (#_PrOpen)
      (prchk $err-printer-load)
      (let ((pRec (get-print-record)))
        (when (#_PrJobDialog :ptr (get-print-record) :boolean)
          (let ((*hc-page-open-p* nil) (ui::*inhibit-error* t)) ; err)
            (declare (special *hc-page-open-p* ui::*inhibit-error*))
            (without-interrupts
             (let* ((window-ptr (wptr self))
                    (hardcopy-ptr 
                     (#_PrOpenDoc :ptr (get-print-record) :long 0 :long 0 :ptr)))
               (unwind-protect
                 (with-dereferenced-handles ((ppRec PRec))
                   pprec
                   (prchk $err-printer-start)
                   (unwind-protect
                     (progn
                       (set-window-hardcopy-wptr self hardcopy-ptr)
                       (#_PrOpenPage :ptr hardcopy-ptr :long 0)
                       (with-port hardcopy-ptr
                         (print-all-subviews self))
                       (set-window-hardcopy-wptr self window-ptr)
                       )
                     (#_PrClosePage :ptr hardcopy-ptr)
                     )
                   )
                 (#_PrCloseDoc  :ptr hardcopy-ptr)))
             (when (print (eql (%hget-byte pRec $prJob.bjDocLoop)
                              $bSpoolLoop))
               (prchk)
               (%stack-block ((StRec $iPrStatSize))
                 (#_PrPicFile :ptr pRec :long 0 :long 0 :long 0 :ptr StRec))
               (prchk)))  ;))
            t))))
    (#_PrClose)))

(defmethod print-all-subviews ((self pw::C-BPF-window))
  (let* ((view (pw::BPF-editor-object self))
         (pw::*no-line-segments* 
          (pw::display-only-points (view-container (pw::mini-view view)))))
    (pw::print-draw-contents view)))


||#
