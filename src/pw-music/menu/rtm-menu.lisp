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
(in-package :pw)


(defvar *RTM-menu* nil)
(defvar *RTM-menu-file* nil)
(defvar *RTM-menu-edit* nil)
(defvar *rtm-print-setUp* nil)
(defvar *print-rtm-menu* nil)
(defvar *RTM-selection-menu* nil)
(defvar *RTM-global-menu* nil)
(defvar *rtm-boxes-menu* nil)
(defvar *apps-RTM-menu-item*  nil)



(defun make-rtm-print-setup-item ()
   (new-leafmenu "Page Setup…" (lambda () (win-print-setUp *active-rtm-window*))))

(defun make-rtm-print-item ()
  (new-leafmenu "Print…" (lambda () (window-hardcopy *active-rtm-window*))))

;;=========================================
;; file

(defun make-rtm-menu/file ()
  (new-menu "File"
            (new-leafmenu "Save as midifile..."
                          (lambda () (RTM-midi-file-SAVE)))

            (new-leafmenu "Save as ENIGMA..."
                          (lambda () (save-cleni-rtm-score)))
            (new-leafmenu "Save as 1/4-ENIGMA..."
                          (lambda () (let ((*cleni-temperament-mode* 4))
                                       (save-cleni-rtm-score))))

            (setf *rtm-print-setUp* (make-rtm-print-setup-item))
            (setf *print-rtm-menu*  (make-rtm-print-item))))


;;=========================================
;; edit

(defun get-current-rtm-selection ()
  (rtm-selection-1 (editor-collection-object *active-rtm-window*)))
(defun update-all-beat-groupings ())



(defun make-rtm-menu/edit ()
  (new-menu "Edit"
            (item "Cut"  #\X
              (when (get-current-rtm-selection)
                (setf *rtm-struct-selection-scrap* (decompile (get-current-rtm-selection)))
                (remove-beat-from-measure (get-current-rtm-selection))
                (update-all-beat-groupings)
                (erase+view-draw-contents *current-rtm-editor*)))
            (item "Copy"  #\C
              (when (get-current-rtm-selection) 
                (cond ((eql 'C-measure (class-name (class-of (get-current-rtm-selection))))
                       (setf *measure-selection-scrap* (decompile (get-current-rtm-selection))))
                      ((and (eql 'C-beat (class-name (class-of (get-current-rtm-selection))))
                            (beat-chord (get-current-rtm-selection)))
                       (setf *beat-chord-scrap* (decompile (beat-chord (get-current-rtm-selection)))))
                      ((eql 'C-beat (class-name (class-of (get-current-rtm-selection))))
                       (setf *rtm-struct-selection-scrap* (decompile (get-current-rtm-selection))))
                      (t 
                       (setf *measure-line-selection-scrap* (decompile (get-current-rtm-selection)))))))
            (item "Paste"  #\V
              (when (get-current-rtm-selection) 
                (kill-chords (get-current-rtm-selection))
                (paste-beat (get-current-rtm-selection))
                (setf (rtm-selection-1 (editor-collection-object *active-rtm-window*)) ())
                (setf (rtm-selection-2 (editor-collection-object *active-rtm-window*)) ())
                (update-all-beat-groupings)
                (erase+view-draw-contents 
                 (current-rtm-editor (editor-collection-object *active-rtm-window*))))) ))

;;(remove-menu-items  *RTM-menu-edit* (find-menu-item *RTM-menu-edit* "Paste")) 


(defun initialize-rtm-menu ()
  (setf *RTM-menu* (new-menu "RTM"))

  (setf *RTM-menu-file* (make-rtm-menu/file))
  (setf *RTM-menu-edit* (make-rtm-menu/edit))

  ;; --------------------

  (setf *RTM-selection-menu* (new-menu "Edit selection"))
  (ui:add-menu-items *RTM-menu* *RTM-selection-menu*)

  (ui:add-menu-items  *RTM-selection-menu*                      
                      (new-leafmenu "Velocity..." #'edit-rtm-editor-velocity-menu))

  (ui:add-menu-items  *RTM-selection-menu*                      
                      (new-leafmenu "Transpose..." #'edit-rtm-editor-transpose-menu))

  (ui:add-menu-items  *RTM-selection-menu*                      
                      (new-leafmenu "Channel..." #'edit-rtm-editor-chans-menu))

  (ui:add-menu-items  *RTM-selection-menu*                      
                      (new-leafmenu "Duration..." #'edit-rtm-editor-duration-menu))

  (ui:add-menu-items  *RTM-selection-menu*                      
                      (new-leafmenu "Metronome..." #'edit-rtm-editor-Metronome-menu))


  ;; --------------------

  (setf *RTM-global-menu* (new-menu "Edit globally"))
  (ui:add-menu-items *RTM-menu* *RTM-global-menu*)

  (ui:add-menu-items  *RTM-global-menu*                      
                      (new-leafmenu "Transpose..." #'edit-rtm-editor-transpose-global-menu))

  (ui:add-menu-items  *RTM-global-menu*                      
                      (new-leafmenu "Channel..." #'edit-rtm-editor-chans-global-menu))

  (ui:add-menu-items  *RTM-global-menu*                      
                      (new-leafmenu "Increment channel" (lambda () (edit-rtm-editor-chans-global-2 *active-rtm-window*))))

  (ui:add-menu-items  *RTM-global-menu*                      
                      (new-leafmenu "Duration..." #'edit-rtm-editor-duration-global-menu))

  (ui:add-menu-items  *RTM-global-menu*                      
                      (new-leafmenu "Metronome..." #'edit-rtm-editor-Metronome-global-menu))

  (ui:add-menu-items  *RTM-menu*  
                      (new-menu "Choose Staff"
                                (new-menu "Selected staff"
                                          (new-leafmenu "G2-G" (lambda ()     (edit-rtm-editor-staff-layout *active-rtm-window* 1)))
                                          (new-leafmenu "G" (lambda ()        (edit-rtm-editor-staff-layout *active-rtm-window* 2))) 
                                          (new-leafmenu "G F" (lambda ()      (edit-rtm-editor-staff-layout *active-rtm-window* 3)))
                                          (new-leafmenu "F" (lambda ()        (edit-rtm-editor-staff-layout *active-rtm-window* 4)))
                                          (new-leafmenu "G F F2" (lambda ()   (edit-rtm-editor-staff-layout *active-rtm-window* 5)))
                                          (new-leafmenu "G2 G F F2" (lambda ()(edit-rtm-editor-staff-layout *active-rtm-window* 6)))
                                          (new-leafmenu "Empty" (lambda ()     (edit-rtm-editor-staff-layout *active-rtm-window* 7)))) 
                                (new-menu "All staffs"
                                          (new-leafmenu "G2-G" (lambda ()     (edit-rtm-editor-staffs-layout *active-rtm-window* 1)))
                                          (new-leafmenu "G" (lambda ()        (edit-rtm-editor-staffs-layout *active-rtm-window* 2))) 
                                          (new-leafmenu "G F" (lambda ()      (edit-rtm-editor-staffs-layout *active-rtm-window* 3)))
                                          (new-leafmenu "F" (lambda ()        (edit-rtm-editor-staffs-layout *active-rtm-window* 4)))
                                          (new-leafmenu "G F F2" (lambda ()   (edit-rtm-editor-staffs-layout *active-rtm-window* 5)))
                                          (new-leafmenu "G2 G F F2" (lambda ()(edit-rtm-editor-staffs-layout *active-rtm-window* 6)))
                                          (new-leafmenu "Empty" (lambda ()     (edit-rtm-editor-staffs-layout *active-rtm-window* 7))))))

  ;; --------------------

  (setf *rtm-boxes-menu* *pw-MN-Edit-menu*)


  (pw-addmenu pw::*rtm-boxes-menu* '(pw::quantify))

  ;;(pw-addmenu-fun *rtm-boxes-menu* 'score-voice 'C-patch-score-voice)
  (pw-addmenu-fun *rtm-boxes-menu* 'rtm 'C-patch-score-voice)



  ;;(setf *rtm-boxes-menu* (new-menu "Rhythm"))
  (ui:add-menu-items *rtm-boxes-menu* 
                     (new-leafmenu "-" ()))

  ;;(pw-addmenu  *rtm-boxes-menu* '(make-beat make-beat2 make-measure))

  ;;(pw-addmenu-fun *rtm-boxes-menu* 'rtm-collector 'C-patch-application-rtm-editor)

  (pw-addmenu-fun *rtm-boxes-menu* 'poly-rtm 'C-patch-PolifRTM)

  ;;(pw-addmenu  *rtm-boxes-menu* '(get-rchords))

  ;;(ui:add-menu-items *pw-MN-Edit-menu* *rtm-boxes-menu*)

  (pw-addmenu *rtm-boxes-menu* '(rtm-dim))

  ;; --------------------

  (setf *RTM-menu-root*
        (list 
         *pw-menu-apps*
         *RTM-menu-file*
         *RTM-menu-edit*
         (fifth (ui:menubar))
         (sixth (ui:menubar))
         *RTM-menu*))

  (setf *apps-RTM-menu-item* 
        (add-apps-item-to-apps-menu  "RTM"
                                     (lambda () 
                                       (if *active-RTM-window*
                                           (progn 
                                             (window-select *active-RTM-window*)
                                             (enable-all-apps-menu-items)
                                             (menu-item-disable *apps-RTM-menu-item*))
                                           (ui:ed-beep)))))
  (values))


;;;; THE END ;;;;



