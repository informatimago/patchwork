;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               focus-rect-mixin.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Focus Rect Mixin.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-23 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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

(in-package "MCLGUI")

(defclass focus-rect-mixin ()
  ())


(defmethod erase-focus-rect ((item focus-rect-mixin))
  (let ((window (view-window item))
        (pos    (view-position item)))
    (niy w erase-focus-rect item)
    ;; (rlet ((rect :rect :topleft pos :bottomright (add-points pos (view-size item))))
    ;;       (draw-nil-theme-focus-rect window rect))
    ))


(defmethod set-view-size ((item focus-rect-mixin) h &optional v)
  (let ((pt (make-point h v)))
    (unless (point= pt (view-size item))
      (with-focused-view (view-container item)
        (erase-focus-rect item)
        (call-next-method)
        (frame-key-handler item)))))


(defmethod set-view-position ((item focus-rect-mixin) h &optional v)
  (let ((pt (make-point h v)))
    (unless (point= pt (view-position item))
      (with-focused-view (view-container item)    
        (erase-focus-rect item)
        (call-next-method)
        (frame-key-handler item)))))


(defmethod view-activate-event-handler   :after  ((item focus-rect-mixin))
  (frame-key-handler item))

(defmethod view-deactivate-event-handler :after ((item focus-rect-mixin))
  (frame-key-handler item))

(defmethod view-draw-contents            :after ((item focus-rect-mixin))
  (frame-key-handler item))


;;;; THE END ;;;;
