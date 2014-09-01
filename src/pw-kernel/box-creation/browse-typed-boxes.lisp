;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               browse-typed-boxes.lisp
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

(defmethod browse ((self C-patch))
  (make-typed-box-list self)) 

;;(defun cancel-pop ())

(defun init-PW-box-instance-list ()
  (map-into *PW-box-instance-list*
            (lambda (item)
              (if (functionp item)
                  (funcall item)
                  item))
            *PW-box-instance-list*))


(defun make-typed-box-list (patch)
  (cond 
    ((eql (car (type-list patch)) 'no-connection)
     (print "no output connections allowed"))  
    ((not (type-list patch))
     (print "no output typing"))
    (t 
     (init-PW-box-instance-list)
     (let ((box-list)
           (forms)
           (labels)
           (all-pw-boxes *PW-box-instance-list*))
       (while all-pw-boxes
         (when (intersection  
                (type-list patch) 
                (apply 'append (ask-all (pw-controls (car all-pw-boxes)) 'type-list)) :test 'eq) 
           (push (car all-pw-boxes) box-list))
         (pop all-pw-boxes))
       (setq labels (ask-all box-list 'pw-function-string))
       (while box-list
         (push `(browse-typed-boxes  *active-patch-window* ,patch
                                     ,(decompile (pop box-list))) forms))
       (if forms
           (make-pw-pop-up (pairlis (nreverse labels) forms))
           (print "no output typing?"))))))

(defun browse-typed-boxes (win patch new-patch)
  (set-changes-to-file-flag win)
  (add-subviews  win new-patch)  
  (set-view-position new-patch (make-point (x patch) (+ (y patch)(h patch) 8)))
  (view-draw-contents new-patch)
  (when (or (eql  (class-name (class-of patch)) 'C-patch-midi)
            (eql  (class-name (class-of patch)) 'C-patch-function))
    (connect-ctrl new-patch (car (pw-controls new-patch)) patch) 
    (setf (open-state (car (pw-controls new-patch)) ) nil)
    ;;  (draw-control-open-state (car (pw-controls new-patch)) win)
    (tell (controls win) 'draw-connections)))

;;;; THE END ;;;;
