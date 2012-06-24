;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               region.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Regions.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-15 <PJB> Created.
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


(defvar *temp-rgn* nil)

(defun new-rgn ()
  (niy new-rgn)
  nil)

(defun set-rect-region (region left top right bottom)
  (niy set-rect-region region left top right bottom))


(defmacro with-temp-rgns ((&rest rgn-vars) &body body)
  #-(and)
  `(with-macptrs ,rgn-vars
     (unwind-protect
          (progn
            ,@(mapcar #'(lambda (var) `(%setf-macptr ,var (require-trap #_NewRgn))) rgn-vars)
            ,@body)
       ,@(mapcar #'(lambda (var) `(unless (%null-ptr-p ,var) (require-trap #_DisposeRgn ,var)))
                 rgn-vars)))
  (niy with-temp-rgns rgn-vars)
  `(progn
     (niy 'with-temp-rgns ,rgn-vars)
     ,@body))


(defmacro with-hilite-mode (&body body)
  (niy with-hilite-mode body)
  `(niy with-hilite-mode body)
  #-(and)
  `(progn
     (let ((byte (require-trap #_lmgethilitemode)))
       (require-trap #_lmsethilitemode (%ilogand2 #x7f byte)))
     ,@body))


(defmacro with-clip-region (region &body body)
  (niy with-clip-region region body)
  `(niy with-clip-region region body)
  #-(and)
  (let ((rgn  (gensym))
        (rgn2 (gensym)))    
    `(with-temp-rgns (,rgn ,rgn2)
       (require-trap #_GetClip ,rgn)
       (require-trap #_sectrgn ,rgn ,region ,rgn2)
       (unwind-protect
            (progn
              (require-trap #_SetClip ,rgn2)
              ,@body)
         (require-trap #_SetClip ,rgn)))))


;;;; THE END ;;;;

