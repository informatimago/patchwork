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

(defvar *active-BPF-window* ())
(defvar *pw-BPF-library* ())
(defvar *BPF-menu-file* nil)
(defvar *BPF-print-setUp* nil)
(defvar *print-BPF-menu* nil)
(defvar *BPF-menu-edit* nil)
(defvar *BPF-menu* nil)
(defvar *BPF-menu-root* nil)
(defvar *apps-BPF-menu-item* nil)


(defun initialize-bpf-menu ()
  (setf *BPF-print-setUp* (new-leafmenu "Page Setup…" (lambda () (win-print-setUp *active-BPF-window*))))
  (setf *print-BPF-menu* (new-leafmenu "Print…" (lambda () (window-hardcopy *active-BPF-window*))))
  (setf *BPF-menu-file* (new-menu "File"
                                  (new-leafmenu "Save BPF lib..."
                                                (lambda () (save-BPF-lib *pw-BPF-library*)))
                                  (new-leafmenu "Load BPF lib..."
                                                (lambda () (load-BPF-lib)))
                                  *BPF-print-setUp*
                                  *print-BPF-menu*))
  (setf *BPF-menu-edit*
        (new-menu "Edit"
                  (item "Cut"   #\X (cut-bpf (editor-view-object *active-BPF-window*)))
                  (item "Copy"  #\C (copy-bpf (editor-view-object *active-BPF-window*)))
                  (item "Paste" #\V (paste-bpf (editor-view-object *active-BPF-window*)))
                  (item "Select All" #\A (select-all-bpf (editor-view-object *active-BPF-window*)))))

  (setf *BPF-menu*
        (new-menu "BPF"
                  (item "Add BPF to lib"     nil (add-BPF-to-lib *active-BPF-window* *pw-BPF-library*))
                  (item "Next BPF from lib"  #\N (next-from-BPF-lib *active-BPF-window* *pw-BPF-library*))
                  (item "Prev BPF from lib"  #\P (next-from-BPF-lib *active-BPF-window* *pw-BPF-library* -1))
                  (item "Reset BPF lib"      nil (reset-BPF-lib *pw-BPF-library*))))

  (setf *BPF-menu-root*
        (list *pw-menu-apps*
              *BPF-menu-file*
              *BPF-menu-edit*
              (fifth (ui:menubar))
              (sixth (ui:menubar))
              *BPF-menu*))

  (setf  *apps-BPF-menu-item*
         (add-apps-item-to-apps-menu  "BPF"
                                      (lambda ()
                                        (if *active-BPF-window*
                                            (progn
                                              (window-select *active-BPF-window*)
                                              (enable-all-apps-menu-items)
                                              (menu-item-disable *apps-BPF-menu-item*))
                                            (ui:ed-beep)))))
  (values))


;;;; THE END ;;;;
