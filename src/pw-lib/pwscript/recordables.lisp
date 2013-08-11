;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               recordables.lisp
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
(enable-patchwork-reader-macros)

(defvar *record-enable* nil)

(defun check-s ()
  (declare (special *record-enable*))
  (setf *record-enable* nil)
  (niy *record-enable*)
  ;; (if (ui::gestalt :|ascr|) 
  ;;   (setf *record-enable* t)
  ;;   (setf *record-enable* nil))
  )

(on-load-and-now check-script
  (check-s))

;;RECORDABLES
(defun pw::send-appleevent (the-appleevent the-reply &key
                            (reply-mode :no-reply) (interact-mode nil)
                            (can-switch-layer nil) (dont-reconnect nil)
                            (want-receipt nil) (priority #$kAENormalPriority)
                            (timeout #$kAEDefaultTimeout)
                            (idleproc appleevent-idle)
                            filterproc)
  (ui:niy 'pw::send-appleevent)
  ;;   (let ((mode (+ (ecase reply-mode
  ;;                    (:no-reply #$kAENoReply)
  ;;                    (:queue-reply #$kAEQueueReply)
  ;;                    (:wait-reply #$kAEWaitReply))
  ;;                  (ecase interact-mode
  ;;                    ((nil) 0)
  ;;                    (:never-interact #$kAENeverInteract)
  ;;                    (:can-interact #$kAECanInteract)
  ;; ;;introduced for recording, only withn ver. 1.0.1 or greater aaa 15-2-94
  ;;                    (:notexecute #x2000)
  ;;                    (:always-interact #$kAEAlwaysInteract))
  ;;                  (if can-switch-layer #$kAECanSwitchLayer 0)
  ;;                  (if dont-reconnect #$kAEDontReconnect 0)
  ;;                  (if want-receipt #$nreturnreceipt 0))))
  ;;     (ae-error 
  ;;       (let ((*inside-aesend* t)
  ;;             (res (#_AESend the-appleevent the-reply mode priority 
  ;;                   timeout idleproc (or filterproc (%null-ptr)))))
  ;;         (if (eq res #$errAEWaitCanceled)        ; be silent about aborts
  ;;           #$NoErr
  ;;           res)))
  ;;     (when (eq reply-mode :wait-reply)
  ;;       (ui::check-reply-error the-reply)
  ;;       the-reply))
  )




(defun record--ae (class keyword lis)
  (ui::niy 'record--ae)
  ;; (when (and *si-record* *record-enable*)
  ;; (with-aedescs (event reply target )
  ;;   (create-self-target target)
  ;;   (create-appleevent event class keyword target)
  ;;   (mapcar (lambda (par)
  ;;               (put-appleevent-par event (first par) (second par))) lis)
  ;;   (send-appleevent event reply :interact-mode :notexecute)))
  )


(defun record-select-list (lista)
  (ui:niy 'record-select-list)
  ;; (when (and *si-record* *record-enable*)
  ;; (with-aedescs (event reply target )
  ;;     (create-self-target target)
  ;;     (create-appleevent event :|PWst| :|sele| target)
  ;;     (put-appleevent-par event :|----| (cl-user::getDescRecPtr (cl-user::asAEDesc  lista)))
  ;;     (send-appleevent event reply :interact-mode :notexecute)))
  )






;;RENAME
(defmethod set-dialog-item-text-from-dialog ((view C-patch) str)
  (ui:niy 'set-dialog-item-text-from-dialog)
  ;; (record--ae :|PWst| :|rena| `((,:|----| ,(mkSO :|cbox| nil :|name| (pw-function-string view)))
  ;;                                     (,:|newn| ,(string-downcase str))))
  ;; (setf (pw-function-string view) (string-downcase str))
  ;; (with-focused-view view
  ;;   (draw-function-name view 2 (- (h view) 5)))
  )

(defmethod set-dialog-item-text-from-dialog ((view C-patch-application) str)
  (ui:niy 'set-dialog-item-text-from-dialog)
  ;; (let ((win (application-object view)))
  ;;   (record--ae :|PWst| :|rena| `((,:|----| ,(mkSO :|cbox| nil :|name| (pw-function-string view)))
  ;;                                           (,:|newn| ,(string-downcase str))))
  ;;   (setf (pw-function-string view) (string-downcase str))
  ;;   (if (and win (wptr win)) 
  ;;     (set-window-title win (string-downcase str)))
  ;;   (with-focused-view view
  ;;     (draw-function-name view 2 (- (h view) 5))))
  )


;;SET

(defmethod set-dialog-item-text-from-dialog ((self C-ttybox-out) text)
  (ui:niy 'set-dialog-item-text-from-dialog)
  ;; (record--ae :|core| :|setd| `((,:|----| ,(mkSO :|cinp| (mkSO :|cbox| nil :|name| (pw-function-string (view-container self)))
  ;;                                                :|indx| 1))
  ;;                               (,:|data| ,text)))
  ;; (set-dialog-item-text self text)
  ;; (set (intern text "USER-SUPPLIED-IN-OUTS") (view-container self))
  )

(defmethod set-dialog-item-text-from-dialog ((self C-ttybox-in-box) text)
  (ui:niy 'set-dialog-item-text-from-dialog)
  ;; (record--ae :|core| :|setd| `((,:|----| ,(mkSO :|cinp| (mkSO :|cbox| nil :|name| (pw-function-string (view-container self)))
  ;;                                                :|indx| 1))
  ;;                               (,:|data| ,text)))
  ;; (set-dialog-item-text self text)
  )

(defmethod set-dialog-item-text-from-dialog ((self C-ttybox-instrument) text)
  (ui:niy 'set-dialog-item-text-from-dialog)
  ;; (set-dialog-item-text self text)
  ;; (when (dialog-item-action-function self)
  ;;    (funcall (dialog-item-action-function  self) self))
  )

(defmethod set-dialog-item-text-from-dialog ((self C-ttybox-absin) text)
  (ui:niy 'set-dialog-item-text-from-dialog)
  ;; (set-dialog-item-text self text)
  ;; (record--ae :|core| :|setd| `((,:|----| ,(mkSO :|cinp| (mkSO :|cbox| nil :|name| (pw-function-string (view-container self)))
  ;;                                                :|indx| (+ (position self (pw-controls (view-container self))) 1)))
  ;;                               (,:|data| ,text)))
  ;; (update-absin-doc-string (view-container self))
  )

(defmethod set-dialog-item-text-from-dialog ((self C-ttybox-absout) text)
  (ui:niy 'set-dialog-item-text-from-dialog)
  ;; (set-dialog-item-text self text)
  ;; (setf (doc-string self) text)
  ;; (record--ae :|core| :|setd| `((,:|----| ,(mkSO :|cinp| (mkSO :|cbox| nil :|name| (pw-function-string (view-container self)))
  ;;                                                :|indx| 1))
  ;;                               (,:|data| ,text)))
  ;; (update-absout-doc-string (view-container self))
  )


(defmethod item-action-after-drag ((self C-numbox))
  (ui:niy 'item-action-after-drag)
  ;; (if (patch-type-p (view-container self))
  ;;   (record--ae :|core| :|setd| `((,:|----| ,(mkSO :|cinp| (mkSO :|cbox| nil :|name| (pw-function-string (view-container self))) 
  ;;                                                        :|indx| (+ (position self (pw-controls (view-container self))) 1)))
  ;;                                       (,:|data| ,(value self)))))
  ;; (when (dialog-item-action-function self)
  ;;   (funcall (dialog-item-action-function  self) self))
  )

(defmethod set-dialog-item-text-from-dialog ((self C-ttybox) text)
  (ui:niy 'set-dialog-item-text-from-dialog)
  ;; (record--ae :|core| :|setd| `((,:|----| ,(mkSO :|cinp| (mkSO :|cbox| nil :|name| (pw-function-string (view-container self))) 
  ;;                                                     :|indx| (+ (position self (pw-controls (view-container self))) 1)))
  ;;                                     (,:|data| ,text)))
  ;; (set-dialog-item-text self text)
  )

(defmethod set-dialog-item-text-from-dialog ((self C-numbox) text)
  (ui:niy 'set-dialog-item-text-from-dialog)
  ;; (let ((value (read-from-string text)))
  ;;   (when (numberp value)
  ;;     (setf (value self) value)
  ;;     (if (patch-type-p (view-container self))
  ;;       (record--ae :|core| :|setd| `((,:|----| ,(mkSO :|cinp| (mkSO :|cbox| nil :|name| (pw-function-string (view-container self))) 
  ;;                                                           :|indx| (+ (position self (pw-controls (view-container self))) 1)))
  ;;                                           (,:|data| ,value))))
  ;;     (set-numbox-item-text self value)))
  )



(defun original-args (fun)
  (let* ((theargs (ccl:arglist fun))
         (i 0))
    (while (and theargs (not (equal (car theargs) '&optional))
                (not (equal (car theargs) '&rest))
                (not (equal (car theargs) '&key))
                (not (equal (car theargs) '&aux)))
      (incf i)
      (setf theargs (cdr theargs)))
    i))

(defmethod correct-extension-box-2 ((self C-map-first) new-box values)
  (declare (ignore new-box values)))

(defmethod correct-extension-box-2 ((self c-patch-chord-line:C-patch-chord-line) new-box values)
  (correct-extension-box self new-box values))

(defmethod correct-extension-box-2 ((self C-patch-polifMN-mod) new-box values)
  (if (wptr (application-object self)) (window-close (application-object self)))
  (correct-extension-box self new-box values))

(defmethod correct-extension-box-2 ((self C-pw-extend) new-box values)
  (correct-extension-box self new-box values))

(defmethod correct-extension-box-2 ((self C-pw-test) new-box values)
  (correct-extension-box self new-box values))


(defmethod correct-extension-box-2 ((self C-patch-PolifRTM) new-box values)
  (declare (ignore values))
  (if (wptr (application-object self)) (window-close (application-object self)))
  (let ((m-line (make-instance 'C-measure-line)))
    (setf (measure-line-list new-box) (append (measure-line-list self) (list m-line)))
                                        ;(unless (wptr (application-object self))
                                        ;  (setf (application-object self) (make-application-object self)))
    (let ((editors (beat-editors (car (subviews (application-object self)))))
          (new-editors-list (beat-editors (car (subviews (application-object new-box))))))
      (for (i 0 1 (- (length editors) 2))
           (setf (measure-line (nth i new-editors-list)) (measure-line (nth i editors))))
      (setf (measure-line (car (last new-editors-list))) m-line))))

;; I can't make delete input for the extended because the method
;; (mouse-pressed-no-active-extra ((self C-pw-extend) x y)) isn't rigth


(defmethod delete-extra-inputs ((self C-pw-extend)) 
  (let* ((values (butlast (ask-all (pw-controls self) 'patch-value ())))
         (box-now
          (make-patch-box  (class-name (class-of self)) (give-new-extended-title self)
                           (generate-extended-inputs self) (type-list self))))
    (setf (pw-function-string box-now) (pw-function-string self))
    (set-view-position box-now (make-point (x self) (y self))) 
    (for (i 0 1 (1- (length values)))
         (set-dialog-item-text (nth i (pw-controls box-now)) 
                               (if (numberp (nth i values))
                                   (format () "~D" (nth i values))
                                   (string (nth i values)))) 
         (when (not (eq (nth i (input-objects self)) (nth i (pw-controls self))))
           (setf (nth i (input-objects box-now)) (nth i (input-objects self)))
           (setf (open-state (nth i (pw-controls box-now))) nil)))
    (correct-extension-box self box-now values)
    (tell (controls (view-window self)) 'draw-connections t)
    (setf *position-new-box* (view-position self))
    (remove-subviews *active-patch-window* self)
    (setf *si-record* nil)
    (add-patch-box *active-patch-window* box-now)
    (setf *si-record* t)
    (tell (controls *active-patch-window*) 'connect-new-patch? self box-now)
    (tell (controls *active-patch-window*) 'draw-connections)
    (ui:niy 'delete-extra-inputs)
    ;; (record--ae :|PWst| :|pweb| `((,:|----| ,(mkSO :|cbox| nil :|name| (pw-function-string box-now)))))
    box-now))


(defmethod delete-extra-inputs ((self C-patch-application))
  (when (> (length (pw-controls self)) (original-args (pw-function self)))
    (let* ((box-now
            (make-PW-standard-box  (type-of self) (pw-function self)
                                   (make-point (x self) (y self))
                                   (butlast (ask-all (pw-controls self) 'value))))
           (values (butlast (ask-all (pw-controls self) 'patch-value ()))))
      (set-window-title (application-object box-now) (pw-function-string self) )
      (setf (pw-function-string box-now) (pw-function-string self))
      (if (and *current-small-inBox* (eq (view-container *current-small-inBox*) self))
          (kill-text-item))
      (for (i 0 1 (1- (length values)))
           (when (not (eq (nth i (input-objects self)) (nth i (pw-controls self))))
             (setf (nth i (input-objects box-now)) (nth i (input-objects self)))
             (setf (open-state (nth i (pw-controls box-now))) nil)))
      (correct-extension-box-2 self box-now values)
      (tell (controls (view-window self)) 'draw-connections t)
      (setf *position-new-box* (view-position self))
      (remove-subviews *active-patch-window* self)
      (setf *si-record* nil)
      (add-patch-box *active-patch-window* box-now)
      (setf *si-record* t)
      (tell (controls *active-patch-window*) 'connect-new-patch? self box-now)
      (tell (controls *active-patch-window*) 'draw-connections)
      ;;(record--ae :|PWst| :|pweb| `((,:|----| ,(mkSO :|cbox| nil :|name| (pw-function-string box-now)))))
      box-now)
    ))

(defmethod delete-extra-inputs ((self C-pw-functional) ) 
  (when (> (length (pw-controls self)) (original-args (pw-function self)))
    (let ((box-now
           (make-PW-standard-box  (type-of self) (pw-function self)
                                  (make-point (x self) (y self))
                                  (butlast (ask-all (pw-controls self) 'value))))
          (values (butlast (ask-all (pw-controls self) 'patch-value ()))))
      (setf (pw-function-string box-now) (pw-function-string self))
      (if (and *current-small-inBox* (eq (view-container *current-small-inBox*) self))
          (kill-text-item))
      (for (i 0 1 (1- (length values)))
           (when (not (eq (nth i (input-objects self)) (nth i (pw-controls self))))
             (setf (nth i (input-objects box-now)) (nth i (input-objects self)))
             (setf (open-state (nth i (pw-controls box-now))) nil)))
      (correct-extension-box-2 self box-now values)
      (tell (controls (view-window self)) 'draw-connections t)
      (setf *position-new-box* (view-position self))
      (remove-subviews *active-patch-window* self)
      (setf *si-record* nil)
      (add-patch-box *active-patch-window* box-now)
      (setf *si-record* t)
      (tell (controls *active-patch-window*) 'connect-new-patch? self box-now)
      (tell (controls *active-patch-window*) 'draw-connections)
      ;;(record--ae :|PWst| :|pweb| `((,:|----| ,(mkSO :|cbox| nil :|name| (pw-function-string box-now)))))
      box-now)
    ))



;;EXTENDED
(defmethod mouse-pressed-no-active-extra :after ((self C-patch-PolifMN) x y) 
  (declare (ignore x y))
  (remove-yourself-control self))

(defmethod mouse-pressed-no-active-extra :after ((self C-patch-PolifRTM) x y) 
  (declare (ignore x y))
  (remove-yourself-control self))





(defmethod mouse-pressed-no-active-extra ((self C-pw-extend) x y) 
  (declare (ignore x y)) 
  (let ((box-now
         (make-patch-box  (class-name (class-of self)) (give-new-extended-title self)
                          (generate-extended-inputs self) (type-list self)))
        (values))
    (setf (pw-function-string box-now) (pw-function-string self))
    (set-view-position box-now (make-point (x self) (y self))) 
    (setq values (ask-all (pw-controls self) 'patch-value ()))
    (for (i 0 1 (1- (length values)))
         (set-dialog-item-text (nth i (pw-controls box-now)) 
                               (if (numberp (nth i values))
                                   (format () "~D" (nth i values))
                                   (string (nth i values)))) 
         (when (not (eq (nth i (input-objects self)) (nth i (pw-controls self))))
           (setf (nth i (input-objects box-now)) (nth i (input-objects self)))
           (setf (open-state (nth i (pw-controls box-now))) nil)))
    (correct-extension-box self box-now values)
    (tell (controls (view-window self)) 'draw-connections t)
    (setf *position-new-box* (view-position self))
    (remove-subviews *active-patch-window* self)
    (setf *si-record* nil) 
    (add-patch-box *active-patch-window* box-now)
    (setf *si-record* t)
    (tell (controls *active-patch-window*) 'connect-new-patch? self box-now)
    (tell (controls *active-patch-window*) 'draw-connections)
    (ui:niy 'mouse-pressed-no-active-extra)
    ;; (record--ae :|PWst| :|pweb| `((,:|----| ,(mkSO :|cbox| nil :|name| (pw-function-string box-now)))))
    box-now)) 

(defmethod mouse-pressed-no-active-extra ((self C-pw-functional) x y) 
  (declare (ignore x y) )
  (let ((box-now
         (make-PW-standard-box  (type-of self) (pw-function self)
                                (make-point (x self) (y self))
                                (append (ask-all (pw-controls self) 'value) (list :default))))
        (values (ask-all (pw-controls self) 'patch-value ())))
    (setf (pw-function-string box-now) (pw-function-string self))
    (if (and *current-small-inBox* (eq (view-container *current-small-inBox*) self))
        (kill-text-item))
    (for (i 0 1 (1- (length values)))
         (when (not (eq (nth i (input-objects self)) (nth i (pw-controls self))))
           (setf (nth i (input-objects box-now)) (nth i (input-objects self)))
           (setf (open-state (nth i (pw-controls box-now))) nil)))
    (correct-extension-box self box-now values)
    (tell (controls (view-window self)) 'draw-connections t)
    (setf *position-new-box* (view-position self))
    (remove-subviews *active-patch-window* self)
    (setf *si-record* nil)
    (add-patch-box *active-patch-window* box-now)
    (setf *si-record* t)
    (tell (controls *active-patch-window*) 'connect-new-patch? self box-now)
    (tell (controls *active-patch-window*) 'draw-connections)
    (ui:niy 'mouse-pressed-no-active-extra)
    ;;(record--ae :|PWst| :|pweb| `((,:|----| ,(mkSO :|cbox| nil :|name| (pw-function-string box-now)))))
    box-now))


(defmethod mouse-pressed-no-active-extra ((self C-patch-application) x y) 
  (declare (ignore x y) )
  (let ((box-now
         (make-PW-standard-box  (type-of self) (pw-function self)
                                (make-point (x self) (y self))
                                (append (ask-all (pw-controls self) 'value) (list :default))))
        (values (ask-all (pw-controls self) 'patch-value ())))
    (set-window-title (application-object box-now) (pw-function-string self) )
    (setf (pw-function-string box-now) (pw-function-string self))
    (if (and *current-small-inBox* (eq (view-container *current-small-inBox*) self))
        (kill-text-item))
    (for (i 0 1 (1- (length values)))
         (when (not (eq (nth i (input-objects self)) (nth i (pw-controls self))))
           (setf (nth i (input-objects box-now)) (nth i (input-objects self)))
           (setf (open-state (nth i (pw-controls box-now))) nil)))
    (correct-extension-box self box-now values)
    (tell (controls (view-window self)) 'draw-connections t)
    (setf *position-new-box* (view-position self))
    (remove-subviews *active-patch-window* self)
    (setf *si-record* nil)
    (add-patch-box *active-patch-window* box-now)
    (setf *si-record* t)
    (tell (controls *active-patch-window*) 'connect-new-patch? self box-now)
    (tell (controls *active-patch-window*) 'draw-connections)
    (ui:niy 'mouse-pressed-no-active-extra)
    ;; (record--ae :|PWst| :|pweb| `((,:|----| ,(mkSO :|cbox| nil :|name| (pw-function-string box-now)))))
    box-now))






;;LOCK
(in-package "C-PATCH-BUFFER")
(defmethod get-lock-button-fun ((self C-patch-buffer))
  (lambda (item)
      (ui:niy 'get-lock-button-fun)
      ;; (if (value (view-container item))
      ;;   (progn 
      ;;       (set-dialog-item-text item "o")
      ;;       (pw::record--ae :|PWst| :|cann| `((,:|----| ,(pw::mkSO :|cbox| nil :|name| (pw-function-string self))))))
      ;;   (progn
      ;;       (set-dialog-item-text item "x")
      ;;       (pw::record--ae :|PWst| :|cand| `((,:|----| ,(pw::mkSO :|cbox| nil :|name| (pw-function-string self)))))))
      (setf (value (view-container item))
            (not (value (view-container item))))))

(in-package :pw)
(defmethod get-lock-button-fun ((self c-patch-accum:c-patch-accum))
  (eval `(function (lambda (item)
           (ui:niy 'get-lock-button-fun)
           ;; (if (value (view-container item))
           ;;   (progn (set-dialog-item-text item "o") 
           ;;          (record--ae :|PWst| :|cann| `((,:|----| ,(mkSO :|cbox| nil :|name| (pw-function-string self)))))
           ;;          (init-buffer ,self))
           ;;   (progn
           ;;   (set-dialog-item-text item "x")
           ;;   (record--ae :|PWst| :|cand| `((,:|----| ,(mkSO :|cbox| nil :|name| (pw-function-string self)))))))
           (setf (value (view-container item))
                 (not (value (view-container item))))))))



;;CLOSE 




;;NEW 



(defun record-patch (clase posi name)
  (ui:niy 'record-patch)
  ;; (when (and *si-record* *record-enable*)
  ;;   (if (equal (string-downcase clase) "text")
  ;;     (setf clase "Window Comment"))
  ;;   (with-aedescs (event reply target )
  ;;     (create-self-target target)
  ;;     (create-appleevent event :|core| :|crel| target)
  ;;     (put-appleevent-par event :|----|  :|cbox| )
  ;;     (put-appleevent-par event :|mbcn| (string-downcase clase))
  ;;     (when name
  ;;       (put-appleevent-par event :|mbnb| name))
  ;;     (put-appleevent-par event :|mbpb| (cl-user::getDescRecPtr (cl-user::asAEDesc  posi)))
  ;;     (send-appleevent event reply :interact-mode :notexecute)))
  )




;;PARA GENERACION DE NOMBRES

(defun application-window-name (obj string)
  (if (null (position obj (controls *active-patch-window*) :test 'equal))
      (mk-nuevo-name-box  string) (pw-function-string obj)))


(defun mk-nuevo-name-box (string )
  (if (null *active-patch-window*)
      string
      (let* ((nompot string)
             (i 0)
             (seguir (match-name nompot)))
        (while seguir
          (incf i)
          (setf nompot (concatenate 'string string (format nil "~D" i)))
          (setf seguir (match-name nompot)))
        nompot)))

(defun match-name (name)
  (let* ((lista (controls *active-patch-window*)) (i 0)
         (leng (length lista)) (next t))
    (while (and (< i leng) next)
      (if (equal name (pw-function-string (nth i lista)))
          (setf next nil))
      (incf i))
    (if next nil t)))



(defmethod init-pw-function-string ((self C-patch))
  (setf (pw-function-string self) 
        (mk-nuevo-name-box  (string-downcase (string (pw-function self))))))



(defmethod draw-function-name ((self C-patch-application) x y)
  (let* ((win (application-object self))
         (str (if (and win (wptr win))
                  (setf (pw-function-string self) (window-title win))  (pw-function-string self))))
    (if (second (pw-controls self))
        (draw-string x y str)
        (draw-string x y (subseq  str 0 (min 5 (length str) ))))))

;;EDITORES A CAMBIAR



(defun make-BPF-editor (bp &optional editor-view-class)
  (let* ((win-string (mk-nuevo-name-box  "bpf") )
         (win (make-instance 
                  'C-BPF-window :window-title win-string :close-box-p nil :window-show nil
                  :view-position #@(10 40) :view-size #@(250 275)))
         (bp-view 
          (make-instance 
              (if editor-view-class editor-view-class 'C-bpf-view) 
              :view-container win
              :view-position #@(2 2) :view-size #@(240 217) 
              :break-point-function bp 
              :track-thumb-p t)))
    (add-subviews win bp-view)
    (setf (BPF-editor-object win) bp-view)
    (scale-to-fit-in-rect bp-view)
    win))



(defun make-rtm-editor-window (measure-line)
  (let* ((win-string (mk-nuevo-name-box  "rtm"))
         (rtm-win (make-instance 'C-rtm-editor-window :window-show nil 
                                 :view-position (make-point 10 50) 
                                 :view-size (make-point 710 205) :window-title win-string))
         (rtm-col (make-instance 'C-beat-editor-collection 
                      :view-position (make-point 5 5) 
                      :view-size (make-point 700 200)  
                      :beat-editors 
                      (list 
                       (make-instance 'C-beat-editor-panel 
                           :view-position (make-point 10 2) :view-size (make-point 690 150)
                           :measure-line measure-line)))))
    (add-subviews rtm-win rtm-col)
    (resize-new-rtm-w rtm-col (view-size rtm-win))
    rtm-win))           




(defun make-n-rtm-editors-window (count measure-lines)
  (let* ((win-string (mk-nuevo-name-box   "prtm"))
         (rtm-win (make-instance 'C-rtm-editor-window :window-show nil :close-box-p t 
                                 :view-position (make-point 10 50) 
                                 :view-size (make-point 710 (+ 45 (* count 150))) :window-title win-string))
         (rtm-collection 
          (make-instance 'C-beat-editor-collection 
              :view-position (make-point 5 5) 
              :view-size (make-point 700 (+ 40 (* count 150)))))
         (rtm-editors))
    (for (i 0 1 (1- count))
         (push (make-instance 'C-beat-editor-panel 
                   :view-position (make-point 10 (+ (* i 150) 10 2)) :view-size (make-point 690 150)
                   :measure-line (nth i measure-lines))
               rtm-editors))
    (setf (beat-editors rtm-collection) (nreverse rtm-editors))
    (apply #'add-subviews rtm-collection (beat-editors rtm-collection))
    (add-subviews rtm-win rtm-collection)
    (setf (visible-staffs-count rtm-collection) (max 1 (length (beat-editors rtm-collection))))
    (resize-new-rtm-w rtm-collection (view-size rtm-win))
    rtm-win))


(defmethod make-application-object ((self C-table-displayer))
  (setf (application-object self)
        (make-instance 'C-disp-window
            :view-subviews 
          (list (make-instance 'C-array-item
                    :view-font '("Monaco" 9  :plain)
                    :table-dimensions (make-point 2 2)
                    :cell-size (make-point 20 12)
                    :view-size (make-point 100 80)))
          :window-show nil
          :window-title (application-window-name self "Tdisp"))))


(defmethod make-application-object ((self C-patch-midi-Mod))
  (setf (application-object self)
        (make-music-notation-editor 'C-mn-window-mod 'C-MN-view-mod 'C-MN-panel-Mod
                                    (make-point 350 200) *g2-g-f-f2-staffs*
                                    (application-window-name self "chordseq")))
  (if  (give-MN-editor self)
       (setf (chord-line (give-MN-editor self)) (chord-seq self))
       (ui:uiwarn "in ~S ~S,  (give-MN-editor self) is nil" 'make-application-object '((self C-patch-midi-Mod))))
  (application-object self))


(defmethod make-application-object ((self C-patch-polifMN-mod))
  (setf (application-object self)
        (let ((editor
               (make-n-music-notation-editors
                (max (length (pw-controls self)) (length (chord-line-list self)))
                self
                'C-mn-window-mod 'C-MN-view-mod 'C-MN-panel-Mod
                (make-point 350 200))))
          (set-window-title editor (mk-nuevo-name-box  (pw-function-string self)))
          editor)))

(in-package "C-PATCH-LIST-EDITOR")
(defmethod make-application-object ((self C-patch-list-editor))
  (setf (application-object self)
        (make-instance 'C-table-window
            :view-subviews 
          (list (make-instance 'C-list-item
                    :view-font '("Monaco" 9  :plain)
                    :table-dimensions (make-point 2 2)
                    :cell-size (make-point cell-width cell-hight)
                    :view-size (make-point 100 80)))
          :window-show nil
          :window-title (pw::application-window-name self "lst-ed"))))


(in-package "C-PATCH-FILE-BUFFER")
(defmethod get-file ((self C-patch-file-buffer))
  (cond ((and (fred-win self) (wptr (fred-win self)))
         (window-select (fred-win self))
         (pw::record-menu "Open" nil self))
        ((fred-win self) (get-selected-file self))
        ((file-name self)
         (let* ((f-name (file-name self))
                (w-name (file-namestring f-name))
                (win (find-window w-name)))
           (if (and win (wptr win))
               (window-select win)
               (get-selected-file self))))
        (t (get-selected-file self))))


(defmethod get-selected-file ((self C-patch-file-buffer))
  (niy 'get-selected-file)
  ;; (let ((name (if (and (file-name self) 
  ;;                      (not (string= (file-namestring (file-name self)) "New")))
  ;;                 (file-name self)
  ;;                 (CHOOSE-FILE-DIALOG))))
  ;;   (pw::record-menu "Open File"  (namestring name) self)
  ;;   (ui:with-cursor *watch-cursor*
  ;;     (setf (fred-win self) 
  ;;           (make-instance 'fred-window :window-show nil))
  ;;     (setf (file-name self) name)
  ;;     (set-window-filename (fred-win self) name)
  ;;     (buffer-insert-file (fred-buffer (fred-win self)) name)
  ;;     (update-box-name self name)
  ;;     (window-select (fred-win self))))
  )

(in-package "C-PATCH-LIST-EDITOR")
(defmethod open-patch-win ((self C-patch-list-editor))
  (when (not (wptr (application-object self)))
    (setf (application-object self) (make-application-object self))
    (set-pw-win+pw-obj (application-object self) pw::*active-patch-window* self))
  (let ((table (first (subviews (application-object self)))))
    (set-array table (the-list self))
    (set-view-size (application-object self)
                   (add-points (view-size table)
                               (make-point cell-width (* 2 cell-hight))))
    (call-next-method)))

(in-package :pw)

(defmethod save ((self C-patch))
  (if *pw-nosave-mode* 
      (ui::message-dialog "Sorry this version cannot save files.")
      (let* ((new-name 
              (choose-new-file-dialog :directory (string (pw-function self)) 
                                      :button-string "Save patch as"))
             
             (*print-pretty* nil)
             (*decompile-chords-mode* t))
        (record-menu (if (equal (class-name (class-of self)) 'C-patch-chord-box-M )
                         "Save Chord" "Save") (string-downcase  (pathname-name new-name)) self) 
        (setf (pw-function-string self) (string-downcase  (pathname-name new-name)))
        (delete-file new-name)   ;ML
        (with-cursor *watch-cursor*
          (WITH-OPEN-FILE (out new-name :direction :output :if-exists :supersede :if-does-not-exist :create)
            (prin1 '(in-package :pw) out)
            (let ((*package* :pw))
              (prin1 `(add-patch-box *active-patch-window* ,(decompile self)) out)))))))

(defun record-menu  (title para self)
  (ui:niy 'record-menu)
  ;; (if para
  ;;   (record--ae :|PWst| :|come| `((,:|----| ,(mkSO :|cmen| (mkSO :|cbox| nil :|name| (pw-function-string self)) :|name| 
  ;;                                                  title))
  ;;                                 (,:|fina| ,para)))
  ;;   (record--ae :|PWst| :|come| `((,:|----| ,(mkSO :|cmen| (mkSO :|cbox| nil :|name| (pw-function-string self)) :|name| 
  ;;                                                  title)))))
  )

(defun make-box-menu-recordables (theboxmenu)
  (mapc (lambda (par) 
            (if (not (or (igual (menu-item-title par) "Save Chord")
                         (igual (menu-item-title par) "Save")
                         (igual (menu-item-title par) "Open")
                         (igual (menu-item-title par) "Open File")))
              (let* ((fun (menu-item-action-function par)))
                (set-menu-item-action-function par 
                                               (lambda () 
                                                   (record-menu (menu-item-title par) nil *target-action-object*)
                                                 (funcall fun))))))
        (menu-items theboxmenu)))


(defmethod redraw-patch ((self C-abstract-M))
  (let* ((win (patch-win self))
         (patches (controls win))
         (out-box (car (find-abstract-out-box win patches)))
         (in-boxes (find-abstract-in-boxes win patches))
         (title (window-title win))
         (in-docs-temp)
         (in-put-docs 
          (when in-boxes 
            (setq in-docs-temp (get-absin-boxes-types-n patches in-boxes))
            (cond 
              ((member nil in-docs-temp)
               (ui:message-dialog 
                "WARNING! absin box connected to irreducible types. ALL-type used")
               (mapcar (lambda (type-spec) (declare (ignore type-spec)) '(nilNum)) in-docs-temp))
              (t in-docs-temp))))
         (abstract-box 
          (make-std-patch-box (type-of self)  
                              (read-from-string title) in-put-docs win in-boxes)))
    (dmove-patch abstract-box (x self) (y self))
                                        ;   connections
    (make-abstract-box-connections *active-patch-window* 
                                   abstract-box out-box in-boxes win)
    (tell (controls *active-patch-window*) 'draw-connections t)
    (when (member self *current-user-configuration*)
      (user-menu-remove self) (user-menu-include abstract-box))
    (tell (controls *active-patch-window*) 'deactivate-control)
    (setf (active-mode self) t)
    (setf (patch-win self) nil)              ;unconnect old patch from window
    (cut *active-patch-window*)
    (setf (pw-function-string abstract-box) title)
    (set-window-title (patch-win abstract-box) title)
    (setf *position-new-box* (view-position abstract-box))
    (setf *si-record* nil) 
    (add-patch-box *active-patch-window* abstract-box)
    (setf *si-record* t) 
    (ask-all (controls *active-patch-window*) 'connect-new-patch? self abstract-box)
    (tell (controls *active-patch-window*) 'draw-connections) ) )


(make-box-menu-recordables *Chord-box-popUpMenu*)
(make-box-menu-recordables C-PW-MIDI-IN::*Midi-box-popUpMenu*)
(make-box-menu-recordables c-patch-file-buffer::*file-box-popUpMenu*)


(defmethod view-position ((self null)) (make-point 0 0))
(defmethod view-size     ((self null)) (make-point 1000 1000))


(defun init-patch-pos (win patch)
  (unless *position-new-box*
    (setf *position-new-box*
          (if win
              (add-points (view-position win)
                          (make-point (random (point-h (view-size win)))
                                      (random (point-v (view-size win)))))
              (make-point 10 10))))
  (let* ((x (point-h *position-new-box*))
         (y (point-v *position-new-box*)))
    (when win
      (setf x (min x (point-h (view-size win)))
            y (min y (point-v (view-size win)))))
    (set-view-position patch (make-point x y))))



;;;; THE END ;;;;



