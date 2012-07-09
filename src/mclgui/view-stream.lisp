;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               view-stream.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements the Gray Stream methods for SIMPLE-VIEW.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-07-07 <PJB> Created.
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


(defmethod stream-tyo ((view simple-view) (char character))
  (with-focused-view view
    (let ((pos (pen-position (view-pen view))))
      (draw-char (point-h pos) (point-v pos) char)
      (move (view-pen view) (string-width (string char)) 0)))
  char)


(defmethod stream-write-char ((view simple-view) (char character))
  (stream-tyo view char)
  char)


(defmethod stream-line-column ((view simple-view))
  ;; We could compute a column, or a number of character, but that'd
  ;; depend on the font and not all fonts are proportional.
  nil)


(defmethod stream-start-line-p ((view simple-view))
  (zerop (pen-position (view-pen view))))


(defmethod stream-write-string ((view simple-view) (string string) &optional (start 0) end)
  (let ((string (nsubseq string start end)))
    (with-focused-view view
      (let ((pos (pen-position (view-pen view))))
        (draw-string (point-h pos) (point-v pos) string)
        (move (view-pen view) (string-width string) 0))))
  string)


(defmethod stream-terpri ((view simple-view))
  (move-to (view-pen view) 0 (+ (point-v (pen-position (view-pen view)))
                                (font-line-height)))
  nil)


;; (defmethod stream-finish-output ((view simple-view))
;;   ;; with-focused-view already flushed.
;;   (values))
;; 
;; (defmethod stream-finish-output ((view simple-view))
;;   ;; with-focused-view already flushed.
;;   (values))
;;
;; (defmethod stream-clear-output ((view simple-view))
;;   ;; with-focused-view already flushed.
;;   (values))

(defmethod stream-advance-to-column ((view simple-view) column)
  (move (view-pen view)
        (* column (string-width "m"))
        (point-v (pen-position (view-pen view))))
  t)


(defmethod close ((view simple-view) &key abort)
  (declare (ignore abort))
  t)

(defmethod close ((window window) &key abort)
  (declare (ignore abort))
  (window-close window)
  t)



;;;; THE END ;;;;
