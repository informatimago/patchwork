;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pw-controls.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;
;;;;    XXX
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;    Mikael Laurson, Jacques Duthen, Camilo Rueda.
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

(defgeneric value (self))
(defgeneric set-value (self value))
(defgeneric patch-value (self obj))
(defgeneric set-special-text (self value))


;;=========================
;; dialog for tty-box and numbox
(defvar *pw-controls-dialog* ())
(defvar *pw-controls-dialog-text-item* ())
(defvar *pw-controls-current-pw-control* ())

(defvar *current-small-inBox* ())
(defvar *menu-action-cut-std* ())
(defvar *menu-action-copy-std* ())
(defvar *menu-action-paste-std* ())
(defvar *menu-action-cut-txt*   (lambda () (cut *pw-controls-dialog-text-item*)))
(defvar *menu-action-copy-txt*  (lambda () (copy *pw-controls-dialog-text-item*)))
(defvar *menu-action-paste-txt* (lambda () (paste *pw-controls-dialog-text-item*)))

(defun set-all-menu-actions ()
  (setf *menu-action-cut-std*
        (menu-item-action-function (find-menu-item *pw-menu-edit* "Cut")))
  (setf *menu-action-copy-std*
        (menu-item-action-function (find-menu-item *pw-menu-edit* "Copy")))
  (setf *menu-action-paste-std*
        (menu-item-action-function (find-menu-item *pw-menu-edit* "Paste"))))

(defun make-pw-controls-dialog (&optional class)
  (set-all-menu-actions)
  (setf *pw-controls-dialog-text-item*
        (MAKE-DIALOG-ITEM (or class 'EDITABLE-TEXT-DIALOG-ITEM)
                          #@(2 1)
                          #@(84 16)
                          "Untitled"
                          NIL
                          :VIEW-FONT *patchwork-font-spec*
                          :ALLOW-RETURNS NIL)))

(defun resize-text-item (text-item item &optional size)
  (when (and *current-small-inBox* (eql item (view-container *current-small-inBox*)))
    (set-view-size text-item (subtract-points (or size (view-size item)) (make-point 2 2)))))

(defvar *pw-controls-dialog-text-item-old* ())
(defvar *cancel-button* ())

(defun make-pw-controls-dialog-old ()
  (make-instance 'dialog :window-show nil
                         :window-type :single-edge-box
                         :view-position #@(-426 -60)
                         :view-size #@(91 20)
                         :close-box-p nil
                         :view-font '("Chicago" 12 :srcor :plain)
                         :view-subviews
                         (list (setf *pw-controls-dialog-text-item-old*
                                     (make-dialog-item 'editable-text-dialog-item
                                                       #@(2 1)
                                                       #@(84 16)
                                                       "Untitled"
                                                       nil
                                                       :view-font *patchwork-font-spec*
                                                       :allow-returns nil))
                               (make-dialog-item 'button-dialog-item
                                                 #@(6 47)
                                                 #@(62 16)
                                                 "OK"
                                                 (lambda (item)
                                                   (declare (ignore item))
                                                   (set-dialog-item-text-from-dialog
                                                    *pw-controls-current-pw-control*
                                                    (dialog-item-text *pw-controls-dialog-text-item-old*))
                                                   (window-hide *pw-controls-dialog*))
                                                 :view-font *patchwork-font-spec*
                                                 :default-button t)
                               (setf *cancel-button*
                                     (make-dialog-item 'button-dialog-item
                                                       #@(86 49)
                                                       #@(62 16)
                                                       "Cancel"
                                                       nil
                                                       :view-font *patchwork-font-spec*
                                                       :default-button nil)))))


(defun open-pw-controls-dialog-old (item &optional point size)
  (let ((*menubar-frozen* t)) ; to avoid flicker in the menubar
    (unless *pw-controls-dialog* (setf *pw-controls-dialog* (make-pw-controls-dialog-old)))
    (setf *pw-controls-current-pw-control* item)
    (set-dialog-item-text *pw-controls-dialog-text-item-old* (dialog-item-text item))
    (set-view-position *pw-controls-dialog*
                       (add-points
                        (local-to-global (view-container item) (view-position item))
                        (or point (make-point 1 0))))
    (set-view-size *pw-controls-dialog*
                   (subtract-points (or size (view-size item)) (make-point 2 2)))
    (set-view-size *pw-controls-dialog-text-item-old*
                   (subtract-points (view-size item) (make-point 2 2)))
    (window-select *pw-controls-dialog*)))


(defun open-edit-text-item (item container inbox text &key class position size)
  (unless *pw-controls-dialog-text-item* (make-pw-controls-dialog class))
  (setf *pw-controls-current-pw-control* item)
  (set-dialog-item-text *pw-controls-dialog-text-item* text)
  (multiple-value-call (function set-view-font-codes) *pw-controls-dialog-text-item* (view-font-codes item))
  (setf *current-small-inBox* inbox)
  (set-view-position *pw-controls-dialog-text-item* (add-points position #@(1 1)))
  (set-view-size *pw-controls-dialog-text-item* (subtract-points (or size (view-size item)) (make-point 2 2)))
  (add-subviews container *pw-controls-dialog-text-item*)
  (change-menu-actions))

(defun open-edit-text-item-for-box (box text)
  (open-edit-text-item box box (car (pw-controls box)) text
                       :class nil
                       :position (make-point 1 (- (h box) 14))
                       :size (make-point (w box) 13)))

(defun open-pw-controls-dialog (item &optional point size class)
  (if (eql (type-of (front-window)) 'c-pw-window)
      (let ((container (view-container item)))
        (push-to-top container)
        (open-edit-text-item item container item (dialog-item-text item)
                             :class class
                             :position (add-points (view-position item) (or point (make-point 1 0)))
                             :size size))
      (open-pw-controls-dialog-old item point size)))


(defun change-menu-actions ()
  (set-menu-item-action-function (find-menu-item *pw-menu-edit* "Cut")
                                 *menu-action-cut-txt*)
  (set-menu-item-action-function (find-menu-item *pw-menu-edit* "Copy")
                                 *menu-action-copy-txt*)
  (set-menu-item-action-function (find-menu-item *pw-menu-edit* "Paste")
                                 *menu-action-paste-txt*))

(defun restore-menu-actions ()
  (set-menu-item-action-function (find-menu-item *pw-menu-edit* "Cut")
                                 *menu-action-cut-std*)
  (set-menu-item-action-function (find-menu-item *pw-menu-edit* "Copy")
                                 *menu-action-copy-std*)
  (set-menu-item-action-function (find-menu-item *pw-menu-edit* "Paste")
                                 *menu-action-paste-std*))

(defun kill-text-item ()
  (when *current-small-inBox*
    (remove-subviews (view-container *current-small-inBox*) *pw-controls-dialog-text-item*)
    (setf *current-small-inBox* nil)
    (restore-menu-actions)))

;;===========================================================================
;;C-rect C-button C-button-latched C-ttybox C-menubox-val
;;===========================================================================
;;===========================================================================
;;                       C-ttybox
;;===========================================================================

(defclass C-ttybox (static-text-dialog-item)
  ((open-state :initform t  :initarg :open-state :accessor open-state
               :documentation "Selects whether to display the value (t), or the name of the doc-string (nil).")
   (doc-string :initform "" :initarg :doc-string  :accessor doc-string)
   (value :initform nil :initarg :value)
   (type-list :initform ()  :initarg :type-list :accessor type-list)))

#-(and)
(defmethod print-object ((self pw::c-ttybox) stream)
  (let ((*print-pretty* nil))
    (print-unreadable-object (self stream :identity t :type t)
      (format stream "~{~S~^ ~}"
              (list :open-state (pw::open-state self)
                    :doc-string (subseq (pw::doc-string self) 0 (position #\newline (pw::doc-string self)))
                    :value (slot-value self 'pw::value)
                    :type-list (pw::type-list self))))
    (terpri stream))
  self)

(defmethod initialize-instance :after ((self C-ttybox) &rest l)
  (declare (ignore l))
  (if (slot-value self 'value)
      (setf (value self) (eval (slot-value self 'value))))
  (set-view-font self *patchwork-font-spec*))


(defmethod value ((self C-ttybox))
  (dialog-item-text self))

(defgeneric (setf value) (value self)
  (:method (value (self C-ttybox))
    (set-dialog-item-text self
                          (if (not (stringp value))
                              (format nil "~D" value)
                              (string-downcase value)))))

(defgeneric decompile (self)
  (:method ((self C-ttybox))
    `(make-instance ',(class-name (class-of self))
                    :view-position ,(view-position self)
                    :view-size ,(view-size self)
                    :dialog-item-text ,(dialog-item-text self)
                    :doc-string ,(doc-string self)
                    :type-list ',(type-list self))))

(defmethod x+w ((self C-ttybox)) (+ (x self)(w self)))

;;=========================
;;draw

(defmethod view-draw-contents ((self C-ttybox))
  (with-font-focused-view (view-container self)
    (let* ((x     (point-h (view-position self)))
           (y     (point-v (view-position self)))
           (w     (w self))
           (h     (h self)))
      (format-trace '(view-draw-contents C-ttybox) (open-state self))
      (if (open-state self)
          (call-next-method)
          (draw-text (1+ x) (1+ y) (- w 2) (- h 2) (doc-string self)
                     (or (ui::text-truncation self) :clipping)
                     (or (ui::text-justification self) :natural)
                     (ui::compress-text self)))
      (draw-rect* x y (- w 1) (- h 1)))))


(defgeneric set-open-state (self fl)
  (:method ((self C-ttybox) fl)
    (setf (open-state self) fl)
    (erase+view-draw-contents self)))

;;=========================
;;events

(defmethod view-double-click-event-handler ((self C-ttybox) where)
  (declare (ignore where))
  (open-pw-controls-dialog self))

(defgeneric set-dialog-item-text-from-dialog (self text)
  (:method ((self C-ttybox) text)
    (set-dialog-item-text self text)))

(defmethod view-click-event-handler ((self C-ttybox) where)
  (when (and (open-state self) (double-click-p))
    (view-double-click-event-handler self where)))

;;=========================
;;PW

(defmethod patch-value ((self C-ttybox) obj)
  (declare (ignore obj))
  (read-from-string (dialog-item-text self)))

(defmethod set-special-text ((self C-ttybox) value)
  (declare (ignore value)))

;;===========================================================================

(defclass C-TTYBOX-EVAL (C-ttybox) ())

;;=========================
;;PW

(defmethod patch-value ((self C-TTYBOX-EVAL) obj)
  (declare (ignore obj))
  (eval (read-from-string (dialog-item-text self))))

;;===========================================================================
;; patch-value -> string
;; always open

(defclass C-ttybox-str (C-ttybox) ())

(defmethod patch-value ((self C-ttybox-str) obj)
  (declare (ignore obj))
  (dialog-item-text self))

(defmethod set-open-state ((self C-ttybox-str) fl)
  (declare (ignore fl))
  (setf (open-state self) t))

(defmethod (setf value) (value (self C-ttybox-str))
  (set-dialog-item-text self
                        (if (not (stringp value))
                            (format () "~D" value)
                            value)))

;;===========================================================================
(defclass C-ttybox-out (C-ttybox-str) ())

(defmethod set-dialog-item-text-from-dialog ((self C-ttybox-out) text)
  (set-dialog-item-text self text)
  (set (intern text "USER-SUPPLIED-IN-OUTS") (view-container self)))

;;===========================================================================
(defclass C-ttybox-in-box (C-ttybox-str) ())

(defmethod set-dialog-item-text-from-dialog ((self C-ttybox-in-box) text)
  (set-dialog-item-text self text))

(defmethod patch-value ((self C-ttybox-in-box) obj)
  (declare (ignore obj))
  (intern (dialog-item-text self) "USER-SUPPLIED-IN-OUTS" ))

;;===========================================================================
(defclass C-ttybox-absin (C-ttybox-str) ())

(defmethod set-dialog-item-text-from-dialog ((self C-ttybox-absin) text)
  (set-dialog-item-text self text)
  (update-absin-doc-string (view-container self)))

;;===========================================================================
(defclass C-ttybox-absout (C-ttybox-str) ())

(defmethod view-click-event-handler ((self C-ttybox-absout) where)
  (when (double-click-p)
    (view-double-click-event-handler self where)))

(defmethod set-dialog-item-text-from-dialog ((self C-ttybox-absout) text)
  (set-dialog-item-text self text)
  (setf (doc-string self) text)
  (update-absout-doc-string (view-container self)))

;;===========================================================================
;;                       C-button-latched
;;===========================================================================

(defclass C-button-latched (C-ttybox) ())

(defmethod view-click-event-handler ((self C-button-latched) where)
  (declare (ignore where))
  (setf (open-state self) (not (open-state self)))
  (with-focused-view self
    (with-pen-state (:mode :srcxor)
      (fill-rect*  1 1 (- (w self) 2)(- (h self) 2))))
  (when (dialog-item-action-function self)
    (funcall (dialog-item-action-function  self) self)))

(defmethod view-draw-contents ((self C-button-latched))
  (with-focused-view self
    (draw-string 3 9 (dialog-item-text self))
    (unless (open-state self)
      (with-pen-state (:mode :srcxor)
        (fill-rect*  1 1 (- (w self) 2)(- (h self) 2))))
    (draw-rect* 0 0 (w self)(h self))))

(defmethod value ((self C-button-latched)) (not (open-state self)))
(defmethod set-value ((self C-button-latched) value)  (setf (open-state self) (not value)))

(defmethod patch-value ((self C-button-latched) obj)
  (declare (ignore obj))
  (value self))

;;===========================================================================
;;                       C-numbox
;;===========================================================================

(defclass C-numbox (C-ttybox)
  ((value   :initform 0     :initarg :value   :accessor value)
   (min-val :initform 0     :initarg :min-val :accessor min-val)
   (max-val :initform 30000 :initarg :max-val :accessor max-val)))

(defmethod decompile ((self C-numbox))
  `(make-instance ',(class-name (class-of self))
                  :view-position ,(view-position self)
                  :view-size ,(view-size self)
                  :dialog-item-text ,(dialog-item-text self)
                  :VIEW-FONT ',(VIEW-FONT self)
                  :doc-string ,(doc-string self)
                  :type-list ',(type-list self)
                  :value   ,(value self)
                  :min-val  ,(min-val self)
                  :max-val ,(max-val self)))

(defmethod initialize-instance :after ((view C-numbox) &rest initargs)
  (declare (ignore initargs))
  (set-numbox-item-text view (value view)))

(defgeneric set-numbox-item-text (view value)
  (:method ((view C-numbox) value)
    (set-dialog-item-text view (format nil "~5D" value))))

(defmethod set-dialog-item-text-from-dialog ((self C-numbox) text)
  (let ((value (read-from-string text)))
    (when (numberp value)
      (setf (value self) value)
      (set-numbox-item-text self value))))

;;=========================
;;events

(defgeneric map-mouse-increment (view))
(defgeneric item-action-while-drag (self))
(defgeneric item-action-after-drag (self))

(defmethod item-action-while-drag ((self C-numbox)))
(defmethod item-action-after-drag ((self C-numbox))
  (when (dialog-item-action-function self)
    (funcall (dialog-item-action-function  self) self)))

(defmethod map-mouse-increment ((view C-numbox))
  (cond ((option-key-p) 100)
        ((shift-key-p) 10)
        ((command-key-p) 1000)
        (t 1)))

(defmethod view-click-event-handler ((self C-numbox) where)
  (declare (ignore where))
  (when (open-state self)
    (unless (call-next-method) ; processes the double-click in the c-ttybox
      (with-focused-view self
        (draw-rect* 1 1 (- (w self) 2) (- (h self) 2)))
      (let* ((win (view-window self))
             (first-v (point-v (view-mouse-position win)))
             (last-mp (view-mouse-position win))
             (last-value (value self)))
        (loop
          (event-dispatch)
          (unless (mouse-down-p) (return))
          (let ((mp (view-mouse-position win)))
            (unless (eql mp last-mp)
              (setq last-mp mp)
              (set-numbox-item-text self
                                    (setf (value self)
                                          (max (min-val self) (min (max-val self)
                                                                   (+ last-value
                                                                      (* (map-mouse-increment self) (- first-v (point-v last-mp))))))))
              (with-focused-view self
                (view-draw-contents self)
                (draw-rect* 1 1 (- (w self) 2)(- (h self) 2)))
              (item-action-while-drag self))))))
    (item-action-after-drag self)
    (view-draw-contents self)))

;;=========================
;;PW

(defmethod patch-value ((self C-numbox) obj)
  (declare (ignore obj))
  (value self))

;;===========================================================================
;;                       C-numbox-continuous
;;===========================================================================

(defclass C-numbox-continuous (C-numbox) ())

(defmethod item-action-while-drag ((self C-numbox-continuous))
  (when (dialog-item-action-function self)
    (funcall (dialog-item-action-function  self) self)))

(defmethod item-action-after-drag ((self C-numbox-continuous)))

;;===========================================================================
;;                       C-menubox
;;===========================================================================

(defclass C-menubox (C-numbox)
  ((menu-box-list :initform '("a" "b" "c" "d") :initarg :menu-box-list :accessor menu-box-list)))

(defmethod decompile ((self C-menubox))
  `(make-instance ',(class-name (class-of self))
                  :view-position ,(view-position self)
                  :view-size ,(view-size self)
                  :dialog-item-text ,(dialog-item-text self)
                  :VIEW-FONT ',(VIEW-FONT self)
                  :doc-string ,(doc-string self)
                  :type-list ',(type-list self)
                  :value   ,(value self)
                  :min-val  ,(min-val self)
                  :max-val ,(max-val self)
                  :menu-box-list ',(menu-box-list self)))

(defgeneric set-menu-box-list (self list)
  (:method ((self C-menubox) list) (setf (menu-box-list self) list)))

(defgeneric menubox-value (self)
  (:method ((self C-menubox))
    (nth (mod (value self) (length (menu-box-list self))) (menu-box-list self))))

(defmethod set-numbox-item-text ((self C-menubox) value)
  (if (stringp value)
      (setf (value self) (position value (menu-box-list self) :test #'string=)))
  (set-dialog-item-text self (menubox-value self)))

(defmethod view-double-click-event-handler ((self C-menubox) where)
  (declare (ignore where)))

(defmethod set-special-text ((self C-menubox) val)
  (set-numbox-item-text self val))


;;=========================
;;PW

(defmethod patch-value ((self C-menubox) obj)
  (declare (ignore obj))
  (menubox-value self))


;;===========================================================================
;;                      C-menubox-val
;;===========================================================================
;; menu-box-list is list of dotted pairs '(("foo" . 3)("glorp" . 123))

(defclass  C-menubox-val (C-menubox) ())

(defmethod menubox-value ((self C-menubox-val))
  (car (call-next-method)))

;;=========================
;;PW

(defmethod patch-value ((self C-menubox-val) obj)
  (declare (ignore obj))
  (cdr (nth (mod (value self) (length (menu-box-list self))) (menu-box-list self))))


;;===========================================================================
;;===========================================================================

;; (progn
;;   ;;=========================
;;   ;;for INTERFACE-TOOLS
;;   ;;=========================
;;   (INTERFACE-TOOLS::ADD-EDITABLE-DIALOG-ITEM
;;    (make-instance 'C-menubox
;;                   :value 0
;;                   ;;                     :VIEW-FONT *patchwork-font-spec*
;;                   :menu-box-list '("  ac" " czx" "  bx" "   z")))
;;
;;   (defmethod INTERFACE-TOOLS::add-editor-items :after ((menubox C-menubox) editor)
;;     (let ((position (add-points INTERFACE-TOOLS::*editor-items-start-pos* #@(0 66))))
;;       (add-subviews
;;        editor
;;        (make-dialog-item 'button-dialog-item
;;                          position #@(130 16) "Set Menubox Strings"
;;                          (lambda (item)
;;                            (declare (ignore item))
;;                            (set-menu-box-list
;;                             menubox
;;                             (INTERFACE-TOOLS::get-new-table-data
;;                              (menu-box-list menubox) "strings")))))))
;;
;;   (defmethod INTERFACE-TOOLS::copy-instance ((item C-menubox))
;;     (let* ((new-item (call-next-method)))
;;       (setf (value new-item) (value item))
;;       (setf (menu-box-list new-item) (menu-box-list item))
;;       new-item))
;;
;;   (defmethod INTERFACE-TOOLS::object-source-code ((item C-menubox))
;;     (nconc (call-next-method)
;;            `(:menu-box-list ',(menu-box-list item))))
;;
;;   ;;====================================================================================================
;;   |#
;; #|
;;   ;;=========================
;;   ;;for INTERFACE-TOOLS
;;   ;;=========================
;;   (INTERFACE-TOOLS::ADD-EDITABLE-DIALOG-ITEM
;;    (make-instance 'C-numbox
;;                   :view-position #@(5 22)
;;                   :value 0))
;;   ;;                     :VIEW-FONT *patchwork-font-spec*))
;;
;;   (defmethod INTERFACE-TOOLS::copy-instance ((item C-numbox))
;;     (let* ((new-item (call-next-method))
;;            (value (value item)))
;;       (set-numbox-item-text new-item value)
;;       new-item))
;;
;;   (defmethod INTERFACE-TOOLS::object-source-code ((item C-numbox))
;;     (nconc (call-next-method)
;;            `(:value ,(value item))))
;;
;;   ;;=========================
;;   )

;;;; THE END ;;;;
