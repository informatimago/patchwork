;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;=========================================================
;;;;
;;;;  PATCH-WORK
;;;;  By Mikael Laurson, Jacques Duthen, Camilo Rueda.
;;;;  © 1986-1992 IRCAM 
;;;;
;;;;=========================================================

(in-package :pw)

(provide 'PW-help-window)

;====================================================================================================
(defclass C-pw-help-window (window)())

(defmethod view-key-event-handler ((self C-pw-help-window) char)
     (cond 
        ((eq char  #\Newline)
           (when *active-patch-window* (window-select *active-patch-window*))
           (window-hide self)) 
        ((eq char  #\Enter)
           (when *active-patch-window* (window-select *active-patch-window*))))) 

(defmethod window-grow-event-handler ((self C-pw-help-window) where)
  (declare (ignore where))
  (call-next-method)
  (set-view-size (car (subviews self)) (subtract-points (view-size self)(make-point 8 16))))

(defmethod window-zoom-event-handler ((self C-pw-help-window) where)
  (declare (ignore where))
  (call-next-method)
  (set-view-size (car (subviews self)) (subtract-points (view-size self)(make-point 8 16))))

(defclass C-pw-help-window-view (ccl::scroller)())

(defmethod ccl::scroll-bar-limits ((view C-pw-help-window-view))
  (ccl::normal-scroll-bar-limits view 1000 1000))

;====================================================================================================

(defvar *PW-help-window* ())

(defun make-PW-help-window ()
 (let (view-now scroller)
  (setq *PW-help-window*
    (make-instance 'C-pw-help-window :window-title "PW help" :GROW-ICON-P t
        :view-position (make-point 50 25) :view-size (make-point 550 455) :close-box-p nil))
    (setq scroller (make-instance 'C-pw-help-window-view :view-size (make-point (- 550 8) (- 455 16)) 
        :view-container *PW-help-window* :v-scrollp t :h-scrollp nil :track-thumb-p t))
(add-subviews scroller
  (setq view-now (make-instance 'static-text-dialog-item :view-position (make-point 5 5) :dialog-item-text 
"   PW keyboard shortcuts:
    h        open help window
    Return   select super-window,hide PW window 
    Enter    select super-window
    R        rename PW window 
    i        inspect  selected PW boxes
    d        show documentation of selected PW boxes
    e        edit definition of selected PW boxes
    b        browse all possible PW boxes according to typelist of the selected box
    o        open selected application boxes
    A        make abstraction
    D        redraw all
    X        pretty selected boxes X-coordinates
    Y        pretty selected boxes Y-coordinates
    p        play selected collector and oscilloscope boxes
    s        stop all collector and oscilloscope boxes
    c        collect selected collector boxes

    editing numboxes:                           
    click and drag the mouse                    
    click         +-    1 increments            
    control-click +-   10 increments            
    option-click  +-  100 increments            
    shift-click   +- 1000 increments            
    or doubleclick inside a numbox              
    to open a dialog window

    boxes with A = application boxes,B = buffer boxes,E = extensible boxes,
               * = current active application box

    documentation of PW boxes:
    (click in the function area -> where the name of the PW function is printed)
    click          toggle inputboxes between values and documentation string   
    command-click  print in the listener window the inputtypes of inputboxes     
    option-click   print in the listener window the outputtypes of the PW box    
    
    selection:
    Click inside a PW box (except inside the inputboxes or inside the drag-rectangle)
    to select a group of PW boxes click inside PW window and drag a hairline.

    dragging PW boxes:
    Click inside the small upper rectangle (drag-rectangle) of the box and drag,or
    control-click anywhere inside the box (except inside the inputboxes) and drag.        
    If many PW boxes are selected and you begin dragging one of them,then all the
    selected boxes will be dragged.    
    
")))
  (set-view-font  view-now '("monaco" 9 :srcor))))


