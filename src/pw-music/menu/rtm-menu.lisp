;;;; -*- mode:lisp; coding:utf-8 -*-
(in-package :pw)

(provide 'rtm-menu)

;=========================================
;=========================================
; file

(defvar *RTM-menu-file* (new-menu "File"))

(add-menu-items  *RTM-menu-file* (new-leafmenu "Save as midifile..."  
    #'(lambda () (RTM-midi-file-SAVE))))

;=========================================
; edit

(defun get-current-rtm-selection ()
  (rtm-selection-1 (editor-collection-object *active-rtm-window*)))
(defun update-all-beat-groupings ())

(defvar *RTM-menu-edit* (new-menu "Edit"))

(let ((menu-now))
  (add-menu-items  *RTM-menu-edit* 
     (setq menu-now (new-leafmenu "Cut" 
       #'(lambda () 
           (when (get-current-rtm-selection)
                (setf *rtm-struct-selection-scrap* (decompile (get-current-rtm-selection)))
                (remove-beat-from-measure (get-current-rtm-selection))
                (update-all-beat-groupings)
                (erase+view-draw-contents *current-rtm-editor*))))))
   (set-command-key menu-now #\X)
   (add-menu-items  *RTM-menu-edit* 
      (setq menu-now (new-leafmenu "Copy" 
        #'(lambda () 
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
   (add-menu-items  *RTM-menu-edit* 
      (setq menu-now (new-leafmenu "Paste" 
       #'(lambda ()    
           (when (get-current-rtm-selection) 
               (kill-chords (get-current-rtm-selection))
               (paste-beat (get-current-rtm-selection))
               (setf (rtm-selection-1 (editor-collection-object *active-rtm-window*)) ())  
               (setf (rtm-selection-2 (editor-collection-object *active-rtm-window*)) ())  
               (update-all-beat-groupings)
               (erase+view-draw-contents 
                  (current-rtm-editor (editor-collection-object *active-rtm-window*))))))))
   (set-command-key menu-now #\V) )

;(remove-menu-items  *RTM-menu-edit* (find-menu-item *RTM-menu-edit* "Paste")) 

;============================================
; RTM

(defvar *RTM-menu* (new-menu "RTM"))

;============================================
; menubar for RTM

(setf *RTM-menu-root*
  (list 
     *pw-menu-apps*
     *RTM-menu-file* 
     *RTM-menu-edit*
     (fifth (menubar))
     (sixth (menubar))
     *RTM-menu* 
 ))

;============================================
; application 


(setf *apps-RTM-menu-item* 
   (add-apps-item-to-apps-menu  "RTM"
     #'(lambda () 
        (if *active-RTM-window* 
          (if (wptr *active-RTM-window*)
            (progn 
              (window-select *active-RTM-window*)
              (enable-all-apps-menu-items)
              (menu-item-disable *apps-RTM-menu-item*))
            (ed-beep))
          (ed-beep)))))

;============================================
; printing 

(defvar *rtm-print-setUp*
  (new-leafmenu "Page Setup…" #'(lambda () (ccl::win-print-setUp *active-rtm-window*))))

(defvar *print-rtm-menu* 
  (new-leafmenu "Print…" #'(lambda () (ccl::window-hardcopy *active-rtm-window*))))

(add-menu-items *rtm-menu-file* *rtm-print-setUp* *print-rtm-menu*)

