;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               epw-menus.lisp
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
;;;;  révision finale -  décembre 92 -  Tristan Murail
;;;;=========================================================

;; =============================================================================-======
;; [jack] 18.01.91               EPW-Menus.Lisp
;; =============================================================================-======

;; Esquisse PW menus 

(in-package "EPW")



;; =============================================================================-======

;; defining the menu hierarchy

(defparameter *epw-menu*                 (new-menu "Esquisse"))
(defparameter *epw-FreqHarm-menu*        (new-menu "Freq harmony"))
(defparameter *epw-Harm-series-menu*     (new-menu "Harm Series"))
(defparameter *epw-modulations-menu*     (new-menu "Modulations"))
(defparameter *epw-Htreatment-menu*      (new-menu "Treatment"))
(defparameter *epw-Hanalysis-menu*       (new-menu "Analysis"))
(defparameter *epw-Harmonicity-menu*     (new-menu "Harmonicity"))
(defparameter *epw-Intervals-menu*       (new-menu "Intervals"))
(defparameter *epw-Gen-menu*             (new-menu "Generation"))
(defparameter *epw-Itreatment-menu*      (new-menu "Treatment"))
(defparameter *epw-Ianalysis-menu*       (new-menu "Analysis"))
(defparameter *epw-Utilities-menu*       (new-menu "Utilities"))

#|(let ((menus (memq patch-work::*pw-menu-Music* patch-work::*patch-work-menu-root*))
      (menu *epw-menu*))
  (check-type menus cons)
  (when (and (rest menus)
             (string= (ui:menu-title menu) (ui:menu-title (second menus))))
    (rplacd menus (cddr menus)))
  (rplacd menus (cons menu (cdr menus))))|#

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

(defvar *epw-midi-menu* (new-menu "Midi"))

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

;; =============================================================================-======
