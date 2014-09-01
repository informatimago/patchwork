;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pw-window.lisp
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

(defvar *global-clock* (make-instance 'C-clock))
(defvar *decompile-chords-mode* nil)
(defvar *pw-nosave-mode* nil)

;;===================================================================================

(defclass C-pw-window (window)
  ((patch-scrap :initform nil :allocation :class :accessor patch-scrap)
   (wins-menu-item :initform nil :accessor wins-menu-item)
   (abstract-box :initform nil :accessor abstract-box)
   (patch-win-pathname :initform nil :accessor patch-win-pathname)
   (save-changes-to-file-flag :initform nil :accessor save-changes-to-file-flag)
   (super-win :initform nil :accessor super-win)
   (super-note :initform nil :accessor super-note)))

(defvar *pw-window-no-select-mode* ())

(defmethod decompile ((self C-pw-window))
  `(make-win ',(type-of self) ,(window-title self) ,(view-position self) ,(view-size self)
             (list ,@(ask-all (controls self) 'decompile))
             (list ,@(window-decompile-connections self) )
             ,(and (top-level-patch-win? self) t)))

(defun make-win (class title position size controls connections &optional close-button)
  (let ((win (make-instance class
                 :window-title title 
                 :view-position position
                 :view-size size :window-show () :close-box-p close-button
                 )))
    (apply #'add-subviews win controls)
    (mapc (lambda (connection-list) 
              (connect-nth-control (nth (first connection-list) controls)
                                   (second connection-list)
                                   (nth (third connection-list) controls)))
          connections)
    (push win *pw-window-list*)
    (tell controls 'set-pw-window-pointers win)
    win))

;;=================
;;events

(defmethod view-click-event-handler ((self C-pw-window) where)
  (set-changes-to-file-flag self)
  (when (eql (call-next-method) self)  ; click inside window
    (when *current-small-inBox*
      (kill-text-item))                ; (see pw-controls)
    (unless (shift-key-p)
      (tell (controls self) #'deactivate-control))
    (let ((user-rect    (pt2rect where  (grow-gray-rect where 0 self nil)))
          (scratch-rect (make-rect 0 0))
          (lista        '()))
      (dolist (item (controls self))
        (setq where (view-position item))
        (let ((i-rect (make-rect where (add-points where (view-size item)))))
          (intersect-rect user-rect i-rect scratch-rect)
          (unless (empty-rect-p scratch-rect)
            (activate-control item)
            (push (mkSO :|cbox| nil :|name| (pw-function-string item)) lista)))
        (when lista
          (record-select-list lista))))))

;;=================
(defgeneric controls (self))
(defmethod controls ((self C-pw-window)) (subviews self))

(defgeneric activate-all (self))
(defmethod activate-all ((self C-pw-window)) (tell (controls self) 'activate-control))

(defgeneric active-patches (self))
(defmethod active-patches ((self C-pw-window))
  (let ((actives)(controls (controls self)))
    (while controls
      (when (active-mode (car controls)) (push (car controls) actives))
      (pop controls))
    actives))

;;=================================
;; draw

(defvar *pw-connections-drawing-mode* ())

(defmethod view-draw-contents :after ((self C-pw-window))
  #+debug-view (format-trace '(view-draw-contents :after c-pw-window))
  (draw-super-win-title self)
  (unless *pw-connections-drawing-mode*
    (tell (controls self) 'draw-connections)))

(defgeneric draw-super-win-title (self))
(defmethod draw-super-win-title ((self C-pw-window))
  (when (super-win self)
    (with-focused-view self
      (draw-string 5 10 (window-title (super-win self)))))) 

;;=============================
;; saving and loading patches

(defgeneric top-level-patch-win\? (self))
(defmethod top-level-patch-win? ((self C-pw-window))
  (and (not (abstract-box self))
       (not (super-win self))
       (not (super-note self)) self))

;; menu stuff !

(defgeneric set-changes-to-file-flag (self))
(defmethod set-changes-to-file-flag ((self C-pw-window))
  (when (controls self)
    (when (top-level-patch-win? self)
      (unless (save-changes-to-file-flag self)
        (setf (save-changes-to-file-flag self) t)
        (update-PW-file-menu-items t t)
        (set-window-title self (concatenate 'string "†" (window-title self)))
        (when (wins-menu-item self)
          (set-menu-item-title (wins-menu-item self) (window-title self)))))))

(defgeneric reset-changes-to-file-flag (self))
(defmethod reset-changes-to-file-flag ((self C-pw-window))
  (setf (save-changes-to-file-flag self) nil)
  (update-PW-file-menu-items t nil)
  (set-window-title self (remove  "†" (window-title self) :test 'string=))
  (when (wins-menu-item self)
    (set-menu-item-title (wins-menu-item self) (window-title self))))

(defgeneric save-window-title (self))
(defmethod save-window-title ((self C-pw-window))
  (remove  "†" (window-title self) :test 'string=))

;;==================================================
(defvar *save-compiled-file* nil)

(defgeneric PW-WINDOW-SAVE-MN (self))
(defmethod PW-WINDOW-SAVE-MN  ((self C-pw-window))
  (setq *decompile-chords-mode* t)
  (PW-WINDOW-SAVE self)
  (setq *decompile-chords-mode* nil))


(defgeneric PW-WINDOW-SAVE (self))
(defmethod PW-WINDOW-SAVE  ((self C-pw-window))
  (if *pw-nosave-mode*
      (ui::message-dialog "Sorry this version cannot save files.")
      (if (patch-win-pathname self)
          (let ((*print-pretty* ())
                newfile)
            (set-window-title self (save-window-title self))
            (when (wins-menu-item self)
              (set-menu-item-title (wins-menu-item self) (window-title self)))
            ;; (delete-file (patch-win-pathname self))
            (with-open-file (out (patch-win-pathname self)
                                 :direction :output 
                                 :if-does-not-exist :create
                                 :if-exists :supersede) 
              (prin1 '(in-package :pw) out)
              (let ((*package* :pw))
                (prin1 (decompile self) out)))
            (record-event :|core| :|save| (if *decompile-chords-mode* `((,:|----| ,:|cpat| ) (,:|mnpa| ,t))
                                              `((,:|----| ,:|cpat|) )))
            (when *save-compiled-file*
              (setf newfile (compile-file (patch-win-pathname self)))
              (delete-file (patch-win-pathname self))
              (rename-file newfile (patch-win-pathname self)))
            (reset-changes-to-file-flag self))
          (PW-WINDOW-SAVE-as self))))

(defgeneric PW-WINDOW-SAVE-MN-as (self))
(defmethod PW-WINDOW-SAVE-MN-as ((self C-pw-window))
  (setq *decompile-chords-mode* t)
  (PW-WINDOW-SAVE-as self)
  (setq *decompile-chords-mode* nil))

(defgeneric PW-WINDOW-SAVE-as (self))
(defmethod PW-WINDOW-SAVE-as ((self C-pw-window))
  (if *pw-nosave-mode*
      (ui::message-dialog "Sorry this version cannot save files.")
      (let ((new-name (choose-new-file-dialog    
                       :directory (concatenate 'string  (save-window-title self) ".pw")
                       :prompt "Save Patch As…"))
            newfile
            (*print-pretty* ()))
        (when new-name
          (let ((new-name (make-pathname :type "pwpatch" :defaults new-name)))
            (setf (patch-win-pathname self) new-name)
            (set-window-title self (pathname-name new-name))
            (when (wins-menu-item self)
              (set-menu-item-title (wins-menu-item self) (window-title self)))
            ;; (delete-file new-name)   ;ML
            (ui:with-cursor *watch-cursor* 
              (WITH-OPEN-FILE  (out new-name :direction :output 
                                             :if-does-not-exist :create :if-exists :supersede) 
                (prin1 '(in-package :pw) out)
                (let ((*package* :pw))
                  (prin1 (decompile self) out))))
            (record-event :|core| :|save| (if *decompile-chords-mode* `((,:|----| ,:|cpat|) 
                                                                        (,:|asna| ,(namestring new-name)) (,:|mnpa| ,t))
                                              `((,:|----| ,:|cpat|) (,:|asna| ,(namestring new-name)))))
            (when *save-compiled-file*
              (setf newfile (compile-file new-name))
              (delete-file new-name)
              (rename-file newfile new-name))
            (reset-changes-to-file-flag self))))))

(defun load-a-patch (name)
  (with-cursor *watch-cursor*
    (let ((*readtable* *readtable-patchwork*))
      (load name :verbose t :external-format :mac-roman))
    (PW-update-wins-menu name)))

(defun pw-load-patch ()
  (let ((name (choose-file-dialog :button-string "Open Patch"))
        #+ccl (ccl:*compile-definitions* nil))
    (when name
      (unless (and *pw-window-list* *active-patch-window*)
        (let ((win (make-instance 'c-pw-window)))
          (push win *pw-window-list*) 
          (view-activate-event-handler win)))
      (assert *pw-window-list*)
      (assert *active-patch-window*)
      (with-cursor *watch-cursor*
        (flet ((load-patch (name)
                 (let ((*readtable* *readtable-patchwork*))
                   (handler-case
                       (load name :verbose t :external-format :mac-roman)
                     (error (err)
                       (format t "~&Error: ~A~%" err)
                       (return-from pw-load-patch :error))))
                 (format t "~&File ~S loaded.~%" name) (finish-output)))
          (ui::time/stdout (load-patch name)))
        (record-event :|aevt| :|odoc| `((,:|----| ,(mkso :|cpat| nil :|name| (namestring name))))) 
        (pw-update-wins-menu name)))))

(defun PW-update-wins-menu (&optional pathname)
  (format t "~&Length window list: ~A~%" (length *pw-window-list*))
  (let ((window-now (car *pw-window-list*)))
    (window-select window-now)
    (when (and (not (abstract-box window-now))
               (not (wins-menu-item window-now)))
      (when pathname (setf (patch-win-pathname window-now) pathname)) 
      ;; (window-select window-now)
      (ui:add-menu-items  *pw-windows-menu* 
                          (setf (wins-menu-item window-now)
                                (new-leafmenu  (window-title window-now) 
                                               (lambda ()(window-select window-now))))))
    (when (eql (front-window)  window-now)
      (update-wins-menu-items window-now)))) 

;;___________
;; activate deactivate kill

(defgeneric update-wins-menu-items (self))
(defmethod update-wins-menu-items ((self C-pw-window))
  (when (wins-menu-item self)
    (menu-item-disable (wins-menu-item self)))) 

;; (defmethod view-activate-event-handler :after ((self C-pw-window))
;;   (when (abstract-box self)
;;     (draw-appl-label (abstract-box self) #\*))
;;   (ui:set-menubar *patchwork-menubar*)
;;   (enable-all-apps-menu-items)
;;   (menu-item-disable *apps-PW-menu-item*)
;;   (update-PW-file-menu-items
;;    (top-level-patch-win? self) (save-changes-to-file-flag self))
;;   (update-wins-menu-items self)
;;   (setq *active-patch-window* self))

(defmethod view-activate-event-handler :after ((self C-pw-window))
  (when (abstract-box self)
    (draw-appl-label (abstract-box self) #\*))
  (unless (equal (ui:menubar) *patchwork-menubar*)
    (ui:set-menubar *patchwork-menubar*)
    (enable-all-apps-menu-items))
  (menu-item-disable *apps-PW-menu-item*)
  (update-PW-file-menu-items (top-level-patch-win? self)
                             (save-changes-to-file-flag self))
  (update-wins-menu-items self)
  (when (not (equal *active-patch-window* self))
    (record-event :|PWst| :|sele| `((,:|----| ,(mkSO :|cpat| nil :|name| (win-title self))))))
  (setq *active-patch-window* self))

(defmethod view-activate-event-handler :after ((self ui::fred-window))
  (unless (equal (ui:menubar)  *lisp-menubar*)
    (ui:set-menubar *lisp-menubar*)
    ;;added 920818 [Camilo]
    (mapc #'menu-enable (cdr *lisp-menubar*))
    (menu-item-disable *apps-lisp-menu-item*)))

;; (defmethod view-deactivate-event-handler :after ((self C-pw-window))
;;   (when (abstract-box self)
;;     (draw-appl-label (abstract-box self) #\A))
;;   (when (eql *active-patch-window* self) ; no PW window selected
;;     (menu-item-enable *apps-PW-menu-item*)
;;     (enable-Lisp-apps-menu-item))
;;   (when (wins-menu-item self)
;;     (MENU-item-ENABLE (wins-menu-item self))))

(defmethod view-deactivate-event-handler :after ((self C-pw-window))
  (when (abstract-box self)
    (draw-appl-label (abstract-box self) #\A))
  (menu-item-enable *apps-PW-menu-item*)
  (if (wins-menu-item self)
      (MENU-item-ENABLE (wins-menu-item self))))

(defgeneric kill-patch-window (self))
(defmethod kill-patch-window ((self C-pw-window))
  (when (top-level-patch-win? self)
    (window-close self)
    (when (eql *active-patch-window* self)
      (search-for-next-pw-window))))

(defmethod window-close ((self C-pw-window))
  ;; (format-trace '(window-close c-pw-window) self)
  (when (and (top-level-patch-win? self) 
             (save-changes-to-file-flag self)
             (not *pw-nosave-mode*)
             (y-or-n-dialog (format nil "Save changes of window ~S to file~%~A"
                                    (window-title self)
                                    (save-window-title self))))
    (PW-WINDOW-SAVE-MN self))
  (view-deactivate-event-handler self)
  (when (wins-menu-item self)
    (remove-menu-items *pw-windows-menu* (wins-menu-item self)))
  (setq *pw-window-list* (remove self *pw-window-list* :test 'eq)) 
  (tell (controls self) 'remove-yourself-control)
  (call-next-method)
  (record-event :|core| :|clos| `((:|----| :|cpat|))))

;;=============================

(defgeneric decompile-selection (self))
(defmethod decompile-selection ((self C-pw-window))
  `(let ((controls (list ,@(ask-all (active-patches self) 'decompile))))
     ,@(decompile-all-selection-connections self)
     controls)) 

(defgeneric decompile-all-connections (self))
(defmethod decompile-all-connections ((self C-pw-window))
  (let ((controls (controls self)))
    (apply 'append (ask-all controls 'decompile-connections controls))))

(defgeneric decompile-all-selection-connections (self))
(defmethod decompile-all-selection-connections ((self C-pw-window))
  (let ((controls (active-patches self)))
    (apply 'append (ask-all controls 'decompile-connections controls))))

;;this method will dissapear at any moment....
(defgeneric window-decompile-connections (self))
(defmethod window-decompile-connections ((self C-pw-window))
  (mapcar (lambda (code) `(list ,(second (second code)) ,(third code) ,(second (fourth code))))
          (decompile-all-connections self)))

;;=================
;;cut copy paste

(defun connect/unconn (active rest &optional flag)
  (tell active 'draw-connections flag)
  (tell rest 'draw-connections flag 
        (apply #'append (cons active (ask-all active 'input-objects)))))

(defmethod copy ((self C-pw-window))
  (let ((*decompile-chords-mode* t))
    (when (active-patches self) 
      (setf (patch-scrap self) (decompile-selection self)))))

(defmethod paste ((self C-pw-window))
  (when (patch-scrap self)
    (let ((patches (eval (patch-scrap self))))
      (apply #'add-subviews self patches)
      (set-changes-to-file-flag self)
      (tell patches 'set-pw-window-pointers self)
      (tell patches 'draw-connections))))

(defmethod duplicate ((self C-pw-window))
  (when (active-patches self)
    (let ((patches (active-patches self)))
      (set-changes-to-file-flag self) 
      (copy self)
      (tell patches 'deactivate-control)
      (record-event :|core| :|clon| `((,:|----| ,:|csel| )))
      (let* ((*si-record* nil)
             #+ccl (ccl:*compile-definitions* nil) 
             (new-patches (eval (patch-scrap self))))
        (tell new-patches 'dmove-patch 20 20)
        (apply #'add-subviews self  new-patches)
        (tell new-patches 'set-pw-window-pointers self)
        (tell new-patches 'draw-connections)))))

(defgeneric disconnect-all-cut-patches (self cut-patches))
(defmethod disconnect-all-cut-patches ((self C-pw-window) cut-patches)
  (let* ((rest-patches  (set-difference (controls self) (active-patches self) :test 'eq)))
    (connect/unconn cut-patches rest-patches t)
    (dolist (alive-patch rest-patches)
      (dolist (dead-patch cut-patches)
        (disconnect-my-self alive-patch dead-patch))
      (repaint-connections alive-patch cut-patches))))

(defmethod cut ((self C-pw-window))
  (let ((active-patches (active-patches self)))
    (when active-patches
      (set-changes-to-file-flag self) 
      (disconnect-all-cut-patches self active-patches)
      (copy self)
      (dolist (patch active-patches)
        (remove-yourself-control patch)
        (remove-subviews self patch)))))

(defgeneric cut-delete (self))
(defmethod cut-delete ((self C-pw-window))
  (let ((active-patches (active-patches self)))
    (when active-patches
      (set-changes-to-file-flag self) 
      (disconnect-all-cut-patches self active-patches)
      (record-event :|core| :|delo| `((,:|----| ,:|csel| )))
      (dolist (patch active-patches)
        (remove-yourself-control patch)
        (remove-subviews self patch)))))

;;========================
;; key handler
(defvar *pw-default-stop-time* 1000)


(defmethod view-key-event-handler ((self C-pw-window) char)
  (format-trace 'view-key-event-handler char self)
  (cond
    ;; editing a field:
    (*current-small-inBox*
     (handle-edit-events (view-container *current-small-inBox*) char))
    ;; some control processes it:
    ((remove nil (ask-all (controls self) 'are-you-handling-keys? char))
     nil)
    (t
     (let ((no-change-flag nil))
       (case char
         (#\Return ;; Return
          (cond ((abstract-box self)
                 (window-hide self)
                 (window-select (view-window (abstract-box self))))
                ((super-win self) (window-hide self) (window-select (super-win self)))
                (t (ui:ed-beep) (setq no-change-flag t))))
         (#\Newline ;; C-j
          (cond ((abstract-box self) (window-select (view-window (abstract-box self))))
                ((super-win self) (window-select (super-win self)))
                (t (ui:ed-beep) (setq no-change-flag t))))
#|*|#    (#\b  (when (active-patches self)
                 (browse (car (active-patches self)))))
         (#\Rubout
          (cut-delete self))
#|*|#    (#\h 
          (if *PW-help-window*
              (unless (wptr *PW-help-window*)
                (make-PW-help-window))
              (make-PW-help-window))
          (window-select *PW-help-window*))
         (#\t     ;;tutorial patch
          (if (active-patches self)
              (tell (active-patches self) #'get-tutorial-patch)
              (ui:ed-beep)))
#|*|#    (#\R 
          (if (top-level-patch-win? self)
              (ui:ed-beep)
              (let ((title
                        (get-string-from-user  "New name" :size (make-point 200 85) :position :centered
                                                          :initial-string (window-title self))))
                (when title
                  (cond 
                    ((abstract-box self)
                     (set-dialog-item-text-from-dialog (car (pw-controls (out-obj (abstract-box self)))) title))
                    ((super-win self)
                     (set-window-title self title)(erase+view-draw-contents (super-win self))))))))
         (#\r (if (active-patches self)
                  (rename-boxes self)
                  (ui:ed-beep)))
         (#\X (if (active-patches self) 
                  (allign-patches-to-x-y self)
                  (setq no-change-flag t)))
         (#\Y (if (active-patches self) 
                  (allign-patches-to-y self)
                  (setq no-change-flag t)))
#|*|#    (#\D (view-draw-contents  self))
#|*|#    (#\i (tell (ask-all (active-patches self) 'pw-function) 'inspect))
         (#\e (tell (ask-all (active-patches self) 'pw-function) 'edit-definition))
         (#\d (tell (ask-all (active-patches self) 'pw-function) 'show-documentation))
         (#\o (tell (active-patches self) 'open-patch-win)) 
         (#\A (make-abstraction-M self))
#|*|#    (#\p (tell (active-patches self) 'init-patch) (tell (active-patches self) 'play))
         (#\c 
          (let ((stop-time (ask (controls self) 'give-stop-time))) 
            (if stop-time
                (record-midi-out-boxes- self stop-time)
                (record-midi-out-boxes- self *pw-default-stop-time*))))
         (#\C 
          (if (super-note self)
              (record-midi-out-boxes- self (give-structured-duration1 self 1))
              (ui:ed-beep)))
#|*|#    (#\s (tell (controls self) 'stop-play))
         (#\v (run-boxes self))
         (otherwise (ed-beep) (setq no-change-flag t)))
       (when (and (not (member char '(#\h #\i #\I #\R #\s #\p #\b #\D)))
                  (not no-change-flag))
         (set-changes-to-file-flag self))))))

(defgeneric rename-boxes (self))
(defmethod rename-boxes ((self C-pw-window))
  (rename-yourbox (car (active-patches self) )))

(defgeneric run-boxes (self))
(defmethod run-boxes ((self C-pw-window))
  (format-trace 'run-boxes self)
  (let ((boxes (active-patches self)))
    (dolist (box boxes)
      (eval-enqueue `(print (patch-value ,box ,box)))
                                        ;aaa
      (record-event :|PWst| :|eval| `((,:|----| ,(mkSO :|cbox| nil :|name| (pw-function-string box))))))))

;;==================================
;;recording 

(defgeneric find-all-midi-out-boxes (self))
(defmethod find-all-midi-out-boxes ((self C-pw-window))
  (remove nil (ask-all (controls self) #'yourself-if-collecting)))


(defgeneric find-midi-out-boxes (self))
(defmethod find-midi-out-boxes ((self C-pw-window))
  (let ((patches (active-patches self)) ) ;(res))
    (remove nil (ask-all patches 'yourself-if-collecting))))


(defgeneric record-midi-out-boxes- (self stop-time))
(defmethod record-midi-out-boxes- ((self C-pw-window) stop-time)
  (let ((boxes (find-midi-out-boxes self)))
    (tell boxes 'set-begin-time 0)  
    (tell boxes 'set-duration-time stop-time)  
    (tell boxes 'init-patch)            ; before scheduling !! 
    (eval-enqueue `(start-clock *global-clock* ',stop-time ',boxes))))

;;==================================
;;layout

(defgeneric allign-patches-to-x-y (self))
(defmethod allign-patches-to-x-y  ((self C-pw-window))
  (tell (controls self) 'draw-connections t) 
  (let ((ctrls (active-patches self))(x-now)(previous-ctrl))
    (setq ctrls (sort ctrls '< :key (lambda (obj) (y obj))))
    (setq x-now (x (car ctrls)))
    (setq previous-ctrl (pop ctrls))
    (while ctrls 
      (set-view-position (car ctrls) x-now (+ 8 (y previous-ctrl) (h previous-ctrl)))
      (setq previous-ctrl (pop ctrls))))
  (tell (controls self) 'draw-connections))

(defgeneric allign-patches-to-y (self))
(defmethod allign-patches-to-y  ((self C-pw-window))
  (tell (controls self) 'draw-connections t) 
  (let ((ctrls (active-patches self))(y-now)(previous-ctrl))
    (setq ctrls (sort ctrls '< :key (lambda (obj) (x obj))))
    (setq y-now (y (car ctrls)))
    (setq previous-ctrl (pop ctrls))
    (while ctrls 
      (set-view-position  (car ctrls) (make-point (+ 8 (x previous-ctrl)(w previous-ctrl)) y-now))
      (setq previous-ctrl (pop ctrls))))
  (tell (controls self) 'draw-connections))

