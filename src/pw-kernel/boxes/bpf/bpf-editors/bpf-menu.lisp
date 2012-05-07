;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               bpf-menu.lisp
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
;;;;=========================================================
;;;;
;;;;  PATCH-WORK
;;;;  By Mikael Laurson, Jacques Duthen, Camilo Rueda.
;;;;  © 1986-1992 IRCAM 
;;;;
;;;;=========================================================

(in-package :pw)

(provide 'BPF-menu)

;;=========================================
(defvar *active-BPF-window* ())
(defvar *pw-BPF-library* ())
;;=========================================
;; file

(defvar *BPF-menu-file* (new-menu "File"))

(ui:add-menu-items *BPF-menu-file*
  (new-leafmenu "Save BPF lib..."
     #'(lambda () (save-BPF-lib *pw-BPF-library*))))

(ui:add-menu-items *BPF-menu-file*
  (new-leafmenu "Load BPF lib..."
     #'(lambda () (load-BPF-lib))))

;;;============================
;;Hardcopy printing

(defvar *BPF-print-setUp*
  (new-leafmenu "Page Setup…" #'(lambda () (ui::win-print-setUp *active-BPF-window*))))

(defvar *print-BPF-menu* 
  (new-leafmenu "Print…" #'(lambda () (ui::window-hardcopy *active-BPF-window*))))

(ui:add-menu-items *BPF-menu-file* *BPF-print-setUp* *print-BPF-menu*)


;;=========================================
;; edit

(defvar *BPF-menu-edit* (new-menu "Edit"))

(let ((menu-now))
  (ui:add-menu-items  *BPF-menu-edit* 
     (setq menu-now (new-leafmenu "Cut" 
       #'(lambda () (cut-bpf (editor-view-object *active-BPF-window*))))))
   (set-command-key menu-now #\X)
   (ui:add-menu-items  *BPF-menu-edit* 
      (setq menu-now (new-leafmenu "Copy" 
       #'(lambda () (copy-bpf (editor-view-object *active-BPF-window*))))))
   (set-command-key menu-now #\C)
   (ui:add-menu-items  *BPF-menu-edit* 
      (setq menu-now (new-leafmenu "Paste" 
       #'(lambda () (paste-bpf (editor-view-object *active-BPF-window*))))))
   (set-command-key menu-now #\V)
   (ui:add-menu-items  *BPF-menu-edit* 
      (setq menu-now (new-leafmenu "Select All" 
     #'(lambda () (select-all-bpf (editor-view-object *active-BPF-window*) )))))
   (set-command-key menu-now #\A))

;;============================================
;; BPF

(defvar *BPF-menu* (new-menu "BPF"))

(ui:add-menu-items *BPF-menu*
  (new-leafmenu "Add BPF to lib" 
      #'(lambda () (add-BPF-to-lib *active-BPF-window* *pw-BPF-library*))))

(let ((menu-now))
  (ui:add-menu-items *BPF-menu*
    (setq menu-now (new-leafmenu "Next BPF from lib" 
     #'(lambda () (next-from-BPF-lib *active-BPF-window* *pw-BPF-library*)))))
   (set-command-key menu-now #\N)
   (ui:add-menu-items *BPF-menu*
   (setq menu-now (new-leafmenu "Prev BPF from lib" 
     #'(lambda () (next-from-BPF-lib *active-BPF-window* *pw-BPF-library* -1)))))
   (set-command-key menu-now #\P))

(ui:add-menu-items *BPF-menu*
   (new-leafmenu "Reset BPF lib" 
       #'(lambda () (reset-BPF-lib *pw-BPF-library*))))

;;============================================
;; menubar for BPF

(defvar *BPF-menu-root*
  (list 
     *pw-menu-apps*
     *BPF-menu-file* 
     *BPF-menu-edit*
     (fifth (ui:menubar) )
     (sixth (ui:menubar) )
     *BPF-menu* 
 ))

;;============================================
;; application 

(defvar *apps-BPF-menu-item* ())


(setf *apps-BPF-menu-item* 
   (add-apps-item-to-apps-menu  "BPF"
     #'(lambda () 
        (if *active-BPF-window* 
          (if (wptr *active-BPF-window*)
            (progn 
              (window-select *active-BPF-window*)
              (enable-all-apps-menu-items)
              (menu-item-disable *apps-BPF-menu-item*))
            (ed-beep))
          (ed-beep)))))
