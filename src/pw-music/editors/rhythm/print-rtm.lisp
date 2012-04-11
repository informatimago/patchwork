;;;; -*- mode:lisp; coding:utf-8 -*-
(in-package :ccl)

(defmethod window-hardcopy ((self pw::C-rtm-editor-window) &optional show-fl)
  show-fl
  (unwind-protect
    (with-cursor *arrow-cursor*
      (#_PrOpen)
      (prchk $err-printer-load)
      (let ((pRec (get-print-record)) (scroll-pos 0))
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
                   (while (numberp scroll-pos)
                     (unwind-protect
                       (progn
                         (set-window-hardcopy-wptr self hardcopy-ptr)
                         (#_PrOpenPage :ptr hardcopy-ptr :long 0)
                         (with-port hardcopy-ptr
                           (setq scroll-pos(print-all-subviews self)))
                         (set-window-hardcopy-wptr self window-ptr)
                         )
                       (#_PrClosePage :ptr hardcopy-ptr)
                       )
                     (when  scroll-pos (pw::scroll-beat (pw::editor-collection-object self) scroll-pos))))
                 (#_PrCloseDoc  :ptr hardcopy-ptr)))
             (when (print (eq (%hget-byte pRec $prJob.bjDocLoop)
                              $bSpoolLoop))
               (prchk)
               (%stack-block ((StRec $iPrStatSize))
                 (#_PrPicFile :ptr pRec :long 0 :long 0 :long 0 :ptr StRec))
               (prchk)))  ;))
            t))))
    (#_PrClose)))

(defmethod print-all-subviews ((self pw::C-rtm-editor-window))
  (let ((views (subviews (pw::editor-collection-object self)))
        measures)
    (dolist (view views)
      (if (typep view 'pw::C-beat-editor-panel)
        (push (pw::print-draw-contents view) measures)))
    (car (remove nil
       (mapcar #'position (mapcar #'car  (nreverse measures)) 
          (ask-all (ask-all (pw::draw-beat-editor-objects (pw::editor-collection-object self)) #'pw::measure-line) #'pw::measures))))))
