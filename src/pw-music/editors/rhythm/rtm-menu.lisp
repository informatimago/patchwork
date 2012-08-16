;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               rtm-menu.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;    
;;;;    XXX
;;;;    
;;;;AUTHORS
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
;;;;    
;;;; -*- mode:lisp; coding:utf-8 -*-
(in-package :pw)

(provide 'rtm-menu)

;;=========================================
;;=========================================
;; file

(defvar *RTM-menu-file* (new-menu "File"))

(ui:add-menu-items  *RTM-menu-file* (new-leafmenu "Save as midifile..."  
    (lambda () (RTM-midi-file-SAVE))))

;;=========================================
;; edit

(defun get-current-rtm-selection ()
  (rtm-selection-1 (editor-collection-object *active-rtm-window*)))
(defun update-all-beat-groupings ())

(defvar *RTM-menu-edit* (new-menu "Edit"))

(let ((menu-now))
  (ui:add-menu-items  *RTM-menu-edit* 
     (setq menu-now (new-leafmenu "Cut" 
       (lambda () 
           (when (get-current-rtm-selection)
                (setf *rtm-struct-selection-scrap* (decompile (get-current-rtm-selection)))
                (remove-beat-from-measure (get-current-rtm-selection))
                (update-all-beat-groupings)
                (erase+view-draw-contents *current-rtm-editor*))))))
   (set-command-key menu-now #\X)
   (ui:add-menu-items  *RTM-menu-edit* 
      (setq menu-now (new-leafmenu "Copy" 
        (lambda () 
            (when (get-current-rtm-selection) 
              (cond ((eq 'C-measure (class-name (class-of (get-current-rtm-selection))))
                        (setf *measure-selection-scrap* (decompile (get-current-rtm-selection))))
                    ((and (eq 'C-beat (class-name (class-of (get-current-rtm-selection))))
                          (beat-chord (get-current-rtm-selection)))
                        (setf *beat-chord-scrap* (decompile (beat-chord (get-current-rtm-selection)))))
                    ((eq 'C-beat (class-name (class-of (get-current-rtm-selection))))
                        (setf *rtm-struct-selection-scrap* (decompile (get-current-rtm-selection))))
                    (t   
                        (setf *measure-line-selection-scrap* (decompile (get-current-rtm-selection))))))))))
   (set-command-key menu-now #\C)
   (ui:add-menu-items  *RTM-menu-edit* 
      (setq menu-now (new-leafmenu "Paste" 
       (lambda ()    
           (when (get-current-rtm-selection) 
               (kill-chords (get-current-rtm-selection))
               (paste-beat (get-current-rtm-selection))
               (setf (rtm-selection-1 (editor-collection-object *active-rtm-window*)) ())  
               (setf (rtm-selection-2 (editor-collection-object *active-rtm-window*)) ())  
               (update-all-beat-groupings)
               (erase+view-draw-contents 
                  (current-rtm-editor (editor-collection-object *active-rtm-window*))))))))
   (set-command-key menu-now #\V) )

;;(remove-menu-items  *RTM-menu-edit* (find-menu-item *RTM-menu-edit* "Paste")) 

;;============================================
;; RTM

(defvar *RTM-menu* (new-menu "RTM"))

;;============================================
;; menubar for RTM

(defparameter *RTM-menu-root*
  (list *pw-menu-apps*
        *RTM-menu-file* 
        *RTM-menu-edit*
        (fifth (ui:menubar))
        (sixth (ui:menubar))
        *RTM-menu*))

;;============================================
;; application 


(defparameter *apps-RTM-menu-item* 
  (add-apps-item-to-apps-menu  "RTM"
                               (lambda () 
                                   (if *active-RTM-window* 
                                     (if (wptr *active-RTM-window*)
                                       (progn 
                                         (window-select *active-RTM-window*)
                                         (enable-all-apps-menu-items)
                                         (menu-item-disable *apps-RTM-menu-item*))
                                       (ui:ed-beep))
                                     (ui:ed-beep)))))

;;============================================
;; printing 

(defvar *rtm-print-setUp*
  (new-leafmenu "Page Setup…" (lambda () (ui::win-print-setUp *active-rtm-window*))))

(defvar *print-rtm-menu* 
  (new-leafmenu "Print…" (lambda () (ui::window-hardcopy *active-rtm-window*))))

(ui:add-menu-items *rtm-menu-file* *rtm-print-setUp* *print-rtm-menu*)

