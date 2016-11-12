;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               nargs-input.lisp
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


(defclass C-nargs-input-patch (C-patch) ())

(defmethod patch-value ((self C-nargs-input-patch) obj)
  (declare (ignore obj)) 
  (nth (give-nargs-place self)(input-objects self)))

(defgeneric give-nargs-place (self)
  (:method ((self C-nargs-input-patch)) 0))

(defmethod disconnect-my-self ((self C-nargs-input-patch) patch)
   (for (i 0 1 (1- (length (input-objects self))))
      (if (listp (nth i (input-objects self)))
            (when  (member patch (nth i (input-objects self)) :test #'eq) 
               (setf (nth i (input-objects self))
                  (remove patch (nth i (input-objects self)) :test #'eq))
               (when (null (nth i (input-objects self)))
                 (setf (nth i (input-objects self))(nth i (pw-controls self)))
                 (setf (open-state (nth i (pw-controls self))) t)))
            (when  (eql (nth i (input-objects self)) patch)
                 (setf (nth i (input-objects self))(nth i (pw-controls self)))
                 (setf (open-state (nth i (pw-controls self))) t)))))

(defmethod connect-ctrl ((self C-nargs-input-patch) ctrl ctrl-panel)
  (let ((nargs-input (give-nargs-place self)))
   (if (not (eql ctrl (nth nargs-input (pw-controls self))))
     (setf (nth (find-nth-ctrl self ctrl) (input-objects self)) ctrl-panel)
     (if (not (nth-connected-p self nargs-input))
       (setf (nth nargs-input (input-objects self)) (list ctrl-panel))
       (setf (nth nargs-input (input-objects self))
            (adjoin ctrl-panel (nth nargs-input (input-objects self))))))))

;;       (push ctrl-panel (nth nargs-input (input-objects self)))))))

(defmethod connect-nth-control ((self C-nargs-input-patch) nth-ctrl ctrl-panel)
 (let ((nargs-input (give-nargs-place self)))
  (if (not (eql nth-ctrl nargs-input))
     (setf (nth nth-ctrl (input-objects self)) ctrl-panel)
     (if (not (nth-connected-p self nargs-input))
         (setf (nth nth-ctrl (input-objects self)) (list ctrl-panel))
         (setf (nth nth-ctrl (input-objects self))
            (adjoin ctrl-panel (nth nth-ctrl (input-objects self)))))))
  (setf (open-state (nth nth-ctrl (pw-controls self))) ()))


;;=================================
#|
    (new-PW-sub-menu-item *Midi-menu* "nargs-test" 'C-nargs-input-patch  'nargs-test
        '(*fix-float-pw-type* "1st" *fix-float-pw-type* "2nd" ) '())

    (new-PW-sub-menu-item *Midi-menu* "nargs-test" 'C-nargs-input-patch  'nargs-test2
        '(*fix-float-pw-type* "1st" *fix-float-pw-type* "2nd" *fix-float-pw-type* "3rd" ) '())
|#
