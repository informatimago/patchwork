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

(eval-when (eval compile load)
  (import '(pw::new-menu pw::PW-addmenu)))

;; =============================================================================-======

;; defining the menu hierarchy

(defparameter *epw-menu*               (new-menu "Esquisse"))
(defparameter *epw-FreqHarm-menu*        (new-menu "Freq harmony"))
(defparameter *epw-Harm-series-menu*     (new-menu "Harm Series"))
(defparameter *epw-modulations-menu*     (new-menu "Modulations"))
(defparameter *epw-Htreatment-menu*       (new-menu "Treatment"))
(defparameter *epw-Hanalysis-menu*        (new-menu "Analysis"))
(defparameter *epw-Harmonicity-menu*     (new-menu "Harmonicity"))
(defparameter *epw-Intervals-menu*       (new-menu "Intervals"))
(defparameter *epw-Gen-menu*             (new-menu "Generation"))
(defparameter *epw-Itreatment-menu*       (new-menu "Treatment"))
(defparameter *epw-Ianalysis-menu*        (new-menu "Analysis"))
(defparameter *epw-Utilities-menu*       (new-menu "Utilities"))

#|(let ((menus (memq patch-work::*pw-menu-Music* patch-work::*patch-work-menu-root*))
      (menu *epw-menu*))
  (check-type menus cons)
  (when (and (rest menus)
             (string= (CCL:menu-title menu) (CCL:menu-title (second menus))))
    (rplacd menus (cddr menus)))
  (rplacd menus (cons menu (cdr menus))))|#

(ccl:add-menu-items (pw::the-user-menu)  *epw-menu*)


(ccl:add-menu-items *epw-Intervals-menu*
                    *epw-Gen-menu*
                    *epw-Itreatment-menu*
                    *epw-Ianalysis-menu*)

(ccl:add-menu-items  *epw-FreqHarm-menu*
                     *epw-Harm-series-menu*
                     *epw-modulations-menu*
                     *epw-Htreatment-menu*
                     *epw-Hanalysis-menu*)

(defvar *epw-midi-menu* (new-menu "Midi"))

(pw::pw-addmenu  *epw-midi-menu* '(epw::txtune))
                                  
(ccl:add-menu-items
 *epw-menu*
 *epw-Intervals-menu*
 *epw-FreqHarm-menu*
 *epw-Utilities-menu*
 *epw-midi-menu*)

;; =============================================================================-======

;; defining the menu leaves

; intervals ---------

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

; freq harm  ------------

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

(ccl:add-menu-items *epw-Hanalysis-menu* (pw::new-leafmenu "-" ()))

(PW-addmenu  *epw-Hanalysis-menu*
    '(virt-fund))

; utilities  ---------

(PW-addmenu
 *epw-Utilities-menu*
 '(l-distor/2 l-distor/3 l*line l*curb/2 l*curb/3 densifier min->sec sec->min))

(ccl:add-menu-items *epw-Utilities-menu* (pw::new-leafmenu "-" ()))

(PW-addmenu
 *epw-Utilities-menu*
 '(special-char text-format insert-special))

;; =============================================================================-======
