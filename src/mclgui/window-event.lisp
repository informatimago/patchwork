;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               window-event.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Window events.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-16 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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

(in-package "MCLGUI")


(defmethod view-activate-event-handler :before ((window window))
  ;; This is a :before method to make it unlikely to be user-shadowed.
  (view-remprop window :display-in-menu-when-hidden))


(defmethod view-activate-event-handler ((window window))
  (unless (or (not *foreground*) (window-active-p window))
    (setf (window-active-p window) t)
    #-(and)
    (unless (typep window 'windoid)
      (let ((attrs (view-get window 'bubble-attrs)))
        (when attrs
          (set-bubble-attributes window attrs))))
    (call-next-method)
    (let ((key (current-key-handler window)))
      (when key
        (dolist (v (key-handler-list window))
          (unless (or (eq v key) (view-contains-p v key))
            (view-deactivate-event-handler v)))))))


(defmethod view-deactivate-event-handler ((window window))
  (when (window-active-p window)
    (setf (window-active-p window) nil)
    #-(and)
    (unless (or (typep window 'windoid) *disable-bubbles-on-inactive-windows*)
      (let ((attrs (get-bubble-attributes window)))
        (view-put window 'bubble-attrs attrs)
        ;; if is collapsed, leave bubble so will uncollapse
        (niy view-deactivate-event-handler view)
        ;; (if (not (#_iswindowcollapsed wptr))
        ;;     (clear-bubble-attributes window))
        ))
    ;; deactivate subviews
    (call-next-method)))



(defmethod view-key-event-handler ((window window) key)
  (let ((key-hdlr (current-key-handler window)))
    (when (and (eql key #\esc) (not (any-modifier-keys-p)))
      (let ((cancel-button (cancel-button window)))
        (if cancel-button
            (when (dialog-item-enabled-p cancel-button)
              (press-button cancel-button)
              (return-from view-key-event-handler key))
            (unless key-hdlr
              (let ((x (look-for-a-button-named-cancel window)))
                (when (and x (dialog-item-enabled-p x))
                  (press-button x)
                  (return-from view-key-event-handler key)))))))      
    (unless (or (any-modifier-keys-p)
                (and key-hdlr 
                     (eq (fred-shadowing-comtab key-hdlr) ;; nil if not fred-mixin
                         *control-q-comtab*)))
      (case key
        (#.(ignore-errors (read-from-string "#\\tab"))
         (unless (and key-hdlr (allow-tabs-p key-hdlr))
           (change-key-handler window)
           (setq key-hdlr nil)))
        ((#\return
          #.(ignore-errors (read-from-string "#\\enter")))
         (let ((d-button (default-button window)))
           (cond
             ((and (eql key #\return)
                   key-hdlr
                   (or (allow-returns-p key-hdlr) (setq key-hdlr nil))))
             ((and d-button (dialog-item-enabled-p d-button))
              (press-button d-button)
              (setq key-hdlr nil)))))))
    (if key-hdlr
        (view-key-event-handler key-hdlr key)
        (if *top-listener*
            (view-key-event-handler *top-listener* key)))))


;;; --- window closing ----

(defmethod window-close-event-handler ((w window))
  "
NOTE:       It's called by -[MclguiWindow windowShouldClose:]
RETURN:     Whether the window should be closed (YES unless canceled).
"
  (catch-cancel
   (cond ((option-key-p)
          (let ((class (class-of w)))
            (dolist (w (nreverse (windows :class class :include-invisibles t)))
              (if (eq (class-of w) class)
                  (window-close w))))
          (return-from window-close-event-handler t))
         ((control-key-p)
          (view-put w :display-in-menu-when-hidden t)
          (window-hide w)
          (return-from window-close-event-handler nil))
         (t
          (window-close w)
          (return-from window-close-event-handler t))))
  nil)


(defmethod window-close :after ((window window))
  (setf (slot-value window 'my-item) nil)
  (let ((wm *windows-menu*))
    (when (and (typep wm 'menu) (menu-enabled-p wm))
      (update-windows-menu wm))
    (let ((em (edit-menu)))
      (when em (menu-update em)))))


;;; --- window zooming ----

(defgeneric window-do-zoom (window)
  (:documentation "Internal.
Called by -[MclguiWindow zoom:] which is called from WINDOW-ZOOM-EVENT-HANDLER.")
  (:method ((window window))
    (window-size-parts window)))

(defmethod window-zoom-event-handler ((window window) message)
  (with-handle (winh window)
    (when (xor [winh isZoomed] (eq :inZoomIn message))
      (on-main-thread [winh zoom:winh] :wait t)))
  nil)


;;; --- window growing ----

(defmethod window-grow-event-handler ((window window) where)
  (let ((*window-growing* t))
    (with-handle (handle  window)
      (multiple-value-bind (x y w h) (frame [handle frame])
        (declare (ignore x y))
        (set-view-size window (make-point (round w) (round h)))))))


;;; --- window moving ----

(defmethod window-move-event-handler ((window window) new-position)
  (let ((*window-moving* t))
    (set-view-position window new-position)))


;;; --- window update ----

(defmethod window-update-event-handler ((window window))
  (with-focused-view window
    (view-draw-contents window)))

;;; --- window select ----

(defmethod window-select-event-handler ((window window))
  (window-select window))

;;; --- window events ----

(defgeneric window-event-handler (window)
  (:documentation "
RETURN:   NIL, or the object that handles the window events.
")
  (:method ((window t))
    nil)
  (:method ((window window))
    window))


(defun get-window-event-handler ()
  (window-event-handler (front-window :include-windoids t)))




;;;; THE END ;;;;
