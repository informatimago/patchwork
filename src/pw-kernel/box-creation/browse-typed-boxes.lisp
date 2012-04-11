;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;=========================================================
;;;;
;;;;  PATCH-WORK
;;;;  By Mikael Laurson, Jacques Duthen, Camilo Rueda.
;;;;  © 1986-1992 IRCAM 
;;;;
;;;;=========================================================

(in-package :pw)

(provide 'browse-typed-boxes)

;====================================================================================================
;====================================================================================================

(setq *active-patch-window* ())
;(make-new-pw-window)

(defmethod browse ((self C-patch))(make-typed-box-list self)) 

;(defun cancel-pop ())

(defun init-PW-box-instance-list ()
  (for (i 0 1 (1- (length *PW-box-instance-list*)))
    (when (eq (type-of (nth i *PW-box-instance-list*)) 'cons)
       (setf (nth i *PW-box-instance-list*) (funcall (nth i *PW-box-instance-list*))))))

;Camilo..
;(init-PW-box-instance-list)

(defun make-typed-box-list (patch)
 (cond 
   ((eq (car (type-list patch)) 'no-connection) (print "no output connections allowed"))  
   ((not (type-list patch))(print "no output typing"))
   (t 
    (let ((box-list)(forms)(labels)(all-pw-boxes *PW-box-instance-list*))
      (init-PW-box-instance-list)
      (while all-pw-boxes
        (when (intersection  
                 (type-list patch) 
                 (apply 'append (ask-all (pw-controls (car all-pw-boxes)) 'type-list)) :test 'eq) 
          (push (car all-pw-boxes) box-list))
          (pop all-pw-boxes))
      (setq labels (ask-all box-list 'pw-function-string))
      (while box-list
        (push   
          `(browse-typed-boxes  *active-patch-window* ,patch
              ,(decompile (pop box-list))) forms))
      (if forms
        (make-pw-pop-up (pairlis (nreverse labels) forms))
        (print "no output typing?"))))))

(defun browse-typed-boxes (win patch new-patch)
  (set-changes-to-file-flag win)
  (add-subviews  win new-patch)  
  (set-view-position new-patch (make-point (x patch) (+ (y patch)(h patch) 8)))
  (view-draw-contents new-patch)
  (when (or (eq  (class-name (class-of patch)) 'C-patch-midi)
            (eq  (class-name (class-of patch)) 'C-patch-function))
    (connect-ctrl new-patch (car (pw-controls new-patch)) patch) 
    (setf (open-state (car (pw-controls new-patch)) ) nil)
;;;    (draw-control-open-state (car (pw-controls new-patch)) win)
    (tell (controls win) 'draw-connections)))

