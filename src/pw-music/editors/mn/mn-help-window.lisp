;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;=========================================================
;;;;
;;;;  PATCH-WORK
;;;;  By Mikael Laurson, Jacques Duthen, Camilo Rueda.
;;;;  © 1986-1992 IRCAM 
;;;;
;;;;=========================================================

(in-package :pw)

(provide 'MN-help-window)

;====================================================================================================
(defclass C-MN-help-window (C-pw-help-window)())

(defmethod view-key-event-handler ((self C-MN-help-window) char)
     (cond 
        ((eq char  #\Newline)
           (when *active-MN-window* (window-select *active-MN-window*))
           (window-hide self)) 
        ((eq char  #\Enter)
           (when *active-MN-window* (window-select *active-MN-window*))))) 

;====================================================================================================

(defvar *MN-help-window* ())

(defun make-MN-help-window ()
 (let (scroller)
  (setq *MN-help-window*
    (make-instance 'C-MN-help-window :window-title "MN help" :GROW-ICON-P t
         :view-position (make-point 30 25) :view-size (make-point 560 455) :close-box-p nil))
   (setq scroller (make-instance 'C-pw-help-window-view :view-size (make-point (- 560 8) (- 455 16)) 
        :view-container *MN-help-window* :v-scrollp t :h-scrollp nil :track-thumb-p t))
  (add-subviews scroller
   (make-instance 'static-text-dialog-item :view-position (make-point 5 5) 
    :view-font '("monaco" 9 :srcor)
    :dialog-item-text 
"    MN editor keyboard shortcuts and clicks:
   general:
    h         open help window
    Return    select PW window,hide MN window
    Enter     select PW window
    R         rename MN window
    H         scroll to 0 time
    K         kill all chords
    p         play all
    P         play selected or visible chords
    s         stop play 

    o         open active notes
    w         add window to active note 
    c         add window with collector to active note 
    r         remove instrument from active note

    Buttons:
      offs    display chords with note offset-time
      dur     show note durations
      dyn     show note dynamics
      ins     show instrument of notes
      tr      transpose selected chords by the amount (in midics)
              contained in the text box to its left
    Boxes
      stcnt   Sets the number of staffs for the editor
      zoom    zooms attack times of chords
"))))




