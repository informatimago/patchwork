;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               epw-menus.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;    
;;;;    Defines the EPW menus.
;;;;    
;;;;AUTHORS
;;;;    Mikael Laurson, Jacques Duthen, Camilo Rueda.
;;;;    Contributions by Tristan Murail
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-07 <PJB> Changed license to GPL3; Added this header.
;;;;    1992-12-00 <Tristan Murail> révision finale
;;;;    1991-01-18 [jack] EPW-Menus.Lisp
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
(in-package "EPW")

;; defining the menu hierarchy

(defvar *epw-menu*                 nil)
(defvar *epw-FreqHarm-menu*        nil)
(defvar *epw-Harm-series-menu*     nil)
(defvar *epw-modulations-menu*     nil)
(defvar *epw-Htreatment-menu*      nil)
(defvar *epw-Hanalysis-menu*       nil)
(defvar *epw-Harmonicity-menu*     nil)
(defvar *epw-Intervals-menu*       nil)
(defvar *epw-Gen-menu*             nil)
(defvar *epw-Itreatment-menu*      nil)
(defvar *epw-Ianalysis-menu*       nil)
(defvar *epw-Utilities-menu*       nil)
(defvar *epw-midi-menu*            nil)

#|(let ((menus (memq patchwork::*pw-menu-Music* patch-work::*patchwork-menubar*))
      (menu *epw-menu*))
  (check-type menus cons)
  (when (and (rest menus)
             (string= (ui:menu-title menu) (ui:menu-title (second menus))))
    (rplacd menus (cddr menus)))
  (rplacd menus (cons menu (cdr menus))))|#


(defun initialize-epw-menus ()
  (setf *epw-menu*                 (new-menu "Esquisse"))
  (setf *epw-FreqHarm-menu*        (new-menu "Freq harmony"))
  (setf *epw-Harm-series-menu*     (new-menu "Harm Series"))
  (setf *epw-modulations-menu*     (new-menu "Modulations"))
  (setf *epw-Htreatment-menu*      (new-menu "Treatment"))
  (setf *epw-Hanalysis-menu*       (new-menu "Analysis"))
  (setf *epw-Harmonicity-menu*     (new-menu "Harmonicity"))
  (setf *epw-Intervals-menu*       (new-menu "Intervals"))
  (setf *epw-Gen-menu*             (new-menu "Generation"))
  (setf *epw-Itreatment-menu*      (new-menu "Treatment"))
  (setf *epw-Ianalysis-menu*       (new-menu "Analysis"))
  (setf *epw-Utilities-menu*       (new-menu "Utilities"))
  (setf *epw-midi-menu* (new-menu "Midi"))

  (ui:add-menu-items (pw::the-user-menu)  *epw-menu*)


  (ui:add-menu-items *epw-Intervals-menu*
                     *epw-Gen-menu*
                     *epw-Itreatment-menu*
                     *epw-Ianalysis-menu*)

  (ui:add-menu-items  *epw-FreqHarm-menu*
                      *epw-Harm-series-menu*
                      *epw-modulations-menu*
                      *epw-Htreatment-menu*
                      *epw-Hanalysis-menu*)


  (pw::pw-addmenu  *epw-midi-menu* '(epw::txtune))
  
  (ui:add-menu-items
   *epw-menu*
   *epw-Intervals-menu*
   *epw-FreqHarm-menu*
   *epw-Utilities-menu*
   *epw-midi-menu*)

  ;; =============================================================================-======

  ;; defining the menu leaves

  ;; intervals ---------

  (PW-addmenu
   *epw-Gen-menu*
   '(inter->chord chord->inter all-series combinatorial-interv::find-intervals))

  (PW-addmenu
   *epw-Itreatment-menu*
   '(remove-int transpoct mul-chord all-inversions
     auto-transp best-transp best-inv))

  (PW-addmenu
   *epw-Ianalysis-menu*
   '(exist-note? midi-center sort-mod))

  ;; freq harm  ------------

  (PW-addmenu
   *epw-Harm-series-menu*
   '(harm-series nth-harm ))

  (pw::pw-addmenu-fun *epw-modulations-menu* 'fm-spec 'C-fm-box)

  (PW-addmenu
   *epw-modulations-menu*
   '(freq-mod fm-ratio ring-mod ring-harm))

  (PW-addmenu
   *epw-Htreatment-menu*
   '(fshift fshift-proc fdistor fdistor-proc))

  (PW-addmenu
   *epw-Hanalysis-menu*
   '(harm-dist closest-harm  best-freq))

  (ui:add-menu-items *epw-Hanalysis-menu* (pw::new-leafmenu "-" ()))

  (PW-addmenu  *epw-Hanalysis-menu*
               '(virt-fund))

  ;; utilities  ---------

  (PW-addmenu
   *epw-Utilities-menu*
   '(l-distor/2 l-distor/3 l*line l*curb/2 l*curb/3 densifier min->sec sec->min))

  (ui:add-menu-items *epw-Utilities-menu* (pw::new-leafmenu "-" ()))

  (PW-addmenu
   *epw-Utilities-menu*
   '(special-char text-format insert-special))

  (values))



;;;; THE END ;;;;
