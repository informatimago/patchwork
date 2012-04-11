;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;=========================================================
;;;;
;;;;  PATCH-WORK
;;;;  By Mikael Laurson, Jacques Duthen, Camilo Rueda.
;;;;  © 1986-1992 IRCAM 
;;;;
;;;;=========================================================

;;
;; Printing MN windows 
;; 

(in-package :ccl)

(defmethod window-hardcopy ((self pw::C-BPF-window) &optional show-fl)
  show-fl
  (unwind-protect
    (with-cursor *arrow-cursor*
      (#_PrOpen)
      (prchk $err-printer-load)
      (let ((pRec (get-print-record)))
        (when (#_PrJobDialog :ptr (get-print-record) :boolean)
          (let ((*hc-page-open-p* nil) (ccl::*inhibit-error* t)) ; err)
            (declare (special *hc-page-open-p* ccl::*inhibit-error*))
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
             (when (print (eq (%hget-byte pRec $prJob.bjDocLoop)
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

#|

|#
