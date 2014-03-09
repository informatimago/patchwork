;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               mn-menu.lisp
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

(provide 'MN-menu)

;;=========================================
;; file
(defvar *pw-menu-Music* (new-menu "Music"))
(defvar *pw-MN-Edit-menu* (new-menu "Edit"))
(defvar *pw-Conv-approx-menu* (new-menu "Conv-Approx"))
(defvar *pw-Music-Extern-menu* (new-menu "Extern"))
(defvar *pw-Midi-menu* (new-menu "Midi"))
(defvar *pw-Multidim-Music-menu* (new-menu "Multidim"))
(ui:add-menu-items *pw-menu-Music* *pw-MN-Edit-menu* *pw-Conv-approx-menu*
                ;*pw-Music-Extern-menu*
                *pw-Midi-menu* *pw-Multidim-Music-menu*)

(defparameter *patch-work-menu-root*
  (list *pw-menu-apps*
        *pw-menu-file* *pw-menu-edit* 
        *PWoper-menu* *pw-kernel-menu* *pw-menu-Music* *pw-menu-patch* 
        *pw-windows-menu*))

(ui:set-menubar *default-CCL-menubar*)

;;==================================

(defvar *active-MN-window* nil)

(defvar *MN-menu-file* (new-menu "File"))

(ui:add-menu-items  *MN-menu-file* (new-leafmenu "Save as midifile..."  
    (lambda () (PW-midi-file-SAVE))))

;;============================================
;; MN
(defvar a-leaf-menu ())
(defvar *MN-menu* (new-menu "MN"))

#|
(ui:add-menu-items *MN-menu*
    (new-menu "Approximation"
        (prog1 (setf a-leaf-menu
                     (new-leafmenu "SemiTone" (lambda () (use-all-approx-scale  *c-major-scale*))))
          (set-command-key a-leaf-menu #\2))
        (prog1 (setf a-leaf-menu
                     (new-leafmenu "Quarter tone" 
                                   (lambda () (use-all-approx-scale  *1/4-tone-chromatic-scale*))))
          (set-command-key a-leaf-menu #\4))
        (prog1 (setf a-leaf-menu
                     (new-leafmenu "Eigth tone" 
                                   (lambda () (use-all-approx-scale  *1/8-tone-chromatic-scale*))))
          (set-command-key a-leaf-menu #\8)))
    (new-menu "Scale"
              (new-leafmenu "C-major" 
                            (lambda () (use-all-scale  *c-major-scale*)))
              (new-leafmenu "Chromatic" 
                            (lambda () (use-all-scale  *chromatic-scale*)))
              (new-leafmenu "Quarter Tone"
                            (lambda () (use-all-scale *1/4-tone-chromatic-scale*)))
              (new-leafmenu "Eighth Tone"
                        (lambda () (use-all-scale  *1/8-tone-chromatic-scale*)))))
|#

(defvar *play-Pbend-menu* 
  (new-leafmenu "Pitch Bend" (lambda () (set-playing-option :pb))))

(defvar *play-Multichan-menu* 
  (new-leafmenu "Multi Channel" (lambda () (set-playing-option :mc))))

#|(ui:add-menu-items *MN-menu*
                (new-menu "Play Option" *play-Pbend-menu* *play-Multichan-menu*))|#

;;============================================
;; menubar for MN

(defvar *undo-MN-menu* (item "Undo" #\Z (redo-MN-edit)))
(menu-item-disable *undo-MN-menu*)
(defvar *MN-menu-edit* (new-menu "Edit"
                                 *undo-MN-menu*
                                 (item "-" nil)
                                 (item "Cut"   #\X (cut   *active-MN-window*))
                                 (item "Copy"  #\C (copy  *active-MN-window*))
                                 (item "Paste" #\V (paste *active-MN-window*))
                                 ))

(defvar *MN-menu-root*
  (list
     *pw-menu-apps*
     *MN-menu-file* 
     *MN-menu-edit*
     (fifth (ui:menubar) )
     (sixth (ui:menubar) )
     ;*MN-menu*
     ))

;;============================================
;; application 

(defparameter  *apps-MN-menu-item* 
   (add-apps-item-to-apps-menu  "MN"
     (lambda () 
        (if *active-MN-window* 
          (if (wptr *active-MN-window*)
            (progn 
              (window-select *active-MN-window*)
              (enable-all-apps-menu-items)
              (menu-item-disable *apps-MN-menu-item*))
            (ui:ed-beep))
          (ui:ed-beep)))))

;;;; THE END ;;;;
