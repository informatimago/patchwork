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

;;=========================================
;; file
(defvar *pw-menu-Music* nil)

(defvar *pw-MN-Edit-menu* nil)
(defvar *pw-Conv-approx-menu* nil)
(defvar *pw-Music-Extern-menu* nil)
(defvar *pw-Midi-menu* nil)
(defvar *pw-Multidim-Music-menu* nil)

;;==================================

(defvar *active-MN-window* nil)

(defvar *MN-menu-file* nil)


;;============================================
;; MN
(defvar a-leaf-menu ())
(defvar *MN-menu* nil)

;; (ui:add-menu-items *MN-menu*
;;     (new-menu "Approximation"
;;         (prog1 (setf a-leaf-menu
;;                      (new-leafmenu "SemiTone" (lambda () (use-all-approx-scale  *c-major-scale*))))
;;           (set-command-key a-leaf-menu #\2))
;;         (prog1 (setf a-leaf-menu
;;                      (new-leafmenu "Quarter tone" 
;;                                    (lambda () (use-all-approx-scale  *1/4-tone-chromatic-scale*))))
;;           (set-command-key a-leaf-menu #\4))
;;         (prog1 (setf a-leaf-menu
;;                      (new-leafmenu "Eigth tone" 
;;                                    (lambda () (use-all-approx-scale  *1/8-tone-chromatic-scale*))))
;;           (set-command-key a-leaf-menu #\8)))
;;     (new-menu "Scale"
;;               (new-leafmenu "C-major" 
;;                             (lambda () (use-all-scale  *c-major-scale*)))
;;               (new-leafmenu "Chromatic" 
;;                             (lambda () (use-all-scale  *chromatic-scale*)))
;;               (new-leafmenu "Quarter Tone"
;;                             (lambda () (use-all-scale *1/4-tone-chromatic-scale*)))
;;               (new-leafmenu "Eighth Tone"
;;                         (lambda () (use-all-scale  *1/8-tone-chromatic-scale*)))))


(defvar *play-Pbend-menu* nil)

(defvar *play-Multichan-menu*  nil)

;; (ui:add-menu-items *MN-menu*
;;                 (new-menu "Play Option" *play-Pbend-menu* *play-Multichan-menu*))


;;============================================
;; menubar for MN

(defvar *undo-MN-menu* nil)
(defvar *MN-menu-edit* nil)


(defvar *MN-menu-root* '())

;;============================================
;; application 

(defvar *apps-MN-menu-item*  nil)

(defvar *MN-print-setUp* nil)
(defvar *print-MN-menu* nil) 

(defun initialize-mn-menu ()
  (setf *MN-menu*      (new-menu "MN"))
  (setf *MN-menu-file* (new-menu "File"))
  (ui:add-menu-items *MN-menu-file* (new-leafmenu "Save as midifile..."  
                                                  (lambda () (PW-midi-file-SAVE))))
  (setf *MN-print-setUp*
        (new-leafmenu "Page Setup…" (lambda () (win-print-setUp *active-mn-window*))))

  (setf *print-MN-menu*
        (new-leafmenu "Print…"      (lambda () (window-hardcopy *active-mn-window*))))

  (ui:add-menu-items *MN-menu-file* *MN-print-setUp* *print-MN-menu*)


  
  (setf *play-Pbend-menu*     (new-leafmenu "Pitch Bend"
                                            (lambda () (set-playing-option :pb))))
  (setf *play-Multichan-menu* (new-leafmenu "Multi Channel"
                                            (lambda () (set-playing-option :mc))))

  (setf *undo-MN-menu* (item "Undo" #\Z (redo-MN-edit)))
  (menu-item-disable *undo-MN-menu*)
  (setf *MN-menu-edit* (new-menu "Edit"
                                 *undo-MN-menu*
                                 (item "-" nil)
                                 (item "Cut"   #\X (cut   *active-MN-window*))
                                 (item "Copy"  #\C (copy  *active-MN-window*))
                                 (item "Paste" #\V (paste *active-MN-window*))))

  (setf *MN-menu-root*
        (list
         *pw-menu-apps*
         *MN-menu-file* 
         *MN-menu-edit*
         (fifth (ui:menubar) )
         (sixth (ui:menubar) )
         ;;*MN-menu*
         ))

  (setf *apps-MN-menu-item* 
        (add-apps-item-to-apps-menu "MN"
                                    (lambda () 
                                      (if *active-MN-window* 
                                          (progn 
                                            (window-select *active-MN-window*)
                                            (enable-all-apps-menu-items)
                                            (menu-item-disable *apps-MN-menu-item*))
                                          (ui:ed-beep)))))
  (values))



;;;; THE END ;;;;
