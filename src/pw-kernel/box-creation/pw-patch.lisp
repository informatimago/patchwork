;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pw-patch.lisp
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
(enable-patchwork-reader-macros)

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



(defvar *object-connection-draw-mode* *gray-pattern*) 
(defvar *normal-connection-draw-mode* *black-pattern*) 


(defclass C-pw-outrect (BUTTON-DIALOG-ITEM)
  ((fill-state :initform nil :accessor %outrect-fill-state)))

(defmethod view-draw-contents ((self C-pw-outrect))
  (with-focused-dialog-item (self)
    (draw-rect* (x self)(y self)(w self)(h self))))

(defgeneric fill-patch-outrect (self)
  (:method ((self C-pw-outrect))
    (with-focused-view self 
      (with-pen-state (:mode :patsrc
                       :pattern (if (setf (%outrect-fill-state self)
                                          (not (%outrect-fill-state self)))
                                    *black-pattern*
                                    *white-pattern*))
        (fill-rect* 1 1 8 3)))))      

(defgeneric mid-x (self)
  (:method ((self C-pw-outrect))
    (+ (x (view-container self)) (x self) 4)))
(defgeneric mid-y (self)
  (:method ((self C-pw-outrect))
    (+ (y (view-container self)) (h (view-container self)) -2)))


(defvar *standard-click-eval* t)

(defun set-eval-click (option)
  (if option 
      (progn (set-menu-item-check-mark *option-click-menu* t)
             (set-menu-item-check-mark *click-menu* nil))
      (progn (set-menu-item-check-mark *option-click-menu* nil)
             (set-menu-item-check-mark *click-menu* t)))
  (setf *standard-click-eval* option))

(defun get-evaluation-option () *standard-click-eval*)

(defmethod view-click-event-handler ((self C-pw-outrect) where)
  #+debug-views
  (format-trace '(view-click-event-handler c-pw-outrect) :where (point-to-list where) :view self)
  (if (eql (not (get-evaluation-option)) (not (option-key-p)))
      (progn
        (incf (clock *global-clock*))
        (eval-enqueue
         `(format t "PW->~S~%" (patch-value ',(view-container self) ',(view-container self)))) ; aaa
        (record-event :|PWst| :|eval| `((,:|----| ,(mkSO :|cbox| nil :|name| (pw-function-string (view-container self)))))))
      (drag-out-line self where)))

(defgeneric drag-out-line (view where)
  (:method ((view C-pw-outrect) where)
    (let* ((win     (view-window view))
           (last-mp (view-mouse-position win))
           (where   (convert-coordinates where (view-container view) win)))
      (flet ((draw-the-line ()
               (with-focused-view win
                 (draw-line (point-h where)(point-v where)(point-h last-mp)(point-v last-mp)))))
        (with-instance-drawing win
          (draw-the-line)
          (loop :while (mouse-down-p) :do
            (let ((mp (view-mouse-position win)))
              (unless (eql mp last-mp)
                (setq last-mp mp)
                (new-instance win)
                (draw-the-line))))
          (new-instance win)))
      (connect-patch? view (find-view-containing-point win last-mp))
      (with-focused-view view
        (erase-view-inside-rect view)))))

(defgeneric connect-patch? (self ctrl)
  (:method ((self C-pw-outrect) ctrl)
    (unless (eql ctrl self)
      (let* ((patch (view-container ctrl)))
        (when patch
          (let ((inde (if (patch-type-p patch) (find-nth-ctrl patch ctrl))))
            (when (and (patch-type-p (view-container ctrl)) 
                       (not (eql (class-name (class-of ctrl)) 'C-pw-outrect)))
              (if (and (and (not (equal (type-list ctrl) '(no-connection)))  
                            (not (equal (type-list (view-container self)) '(no-connection))))  
                       (or (intersection  (type-list ctrl) (type-list (view-container self)) :test 'eq)
                           (not (type-list ctrl))(not (type-list (view-container self))))) ; no type-list specified
                  (progn 
                    (tell (subviews (view-window patch)) 'draw-connections t)
                    (connect-ctrl patch ctrl (view-container self))
                    (if (check-recursive-connections (view-container self)(view-container self))
                        (progn 
                          (ED-BEEP)
                          (setf inde nil)
                          (disconnect-ctrl-1 patch ctrl)
                          (print "Recursive connections not allowed !")
                          (set-open-state ctrl t)) 
                        (progn 
                          (when (or (eql (pw-function (view-container self)) 'absin)
                                    (eql (pw-function (view-container self)) 'absin2))
                            (set-dialog-item-text (car (pw-controls (view-container self))) (doc-string ctrl)))
                          (set-view-font (view-window self) *patchwork-font-spec*)
                          (set-open-state ctrl nil)))
                    (tell (subviews (view-window patch)) 'draw-connections)
                    (when inde 
                      (record-event :|PWst| :|conn| `((,:|----| ,(mkSO :|cbox| nil :|name| (pw-function-string (view-container self))))
                                                      (,:|data| ,(mkSO :|cinp| (mkSO :|cbox| nil :|name| (pw-function-string patch)) 
                                                                       :|indx| (+ inde 1)))))))
                  (progn 
                    (ED-BEEP)
                    (format t "Mismatch of type ! ~%~a~%" 
                            (list 'output (type-list (view-container self)) 'input (type-list ctrl))))))))))))

;;=======================================================================================
;;=======================================================================================

(defgeneric input-objects      (patch) (:method ((self simple-view)) (declare (ignorable self)) nil))
(defgeneric pw-controls        (patch) (:method ((self simple-view)) (declare (ignorable self)) nil))
(defgeneric type-list          (patch) (:method ((self simple-view)) (declare (ignorable self)) nil))
(defgeneric in-xs              (patch) (:method ((self simple-view)) (declare (ignorable self)) nil))
(defgeneric in-ys              (patch) (:method ((self simple-view)) (declare (ignorable self)) nil))
(defgeneric active-mode        (patch) (:method ((self simple-view)) (declare (ignorable self)) nil))
(defgeneric flip-flag          (patch) (:method ((self simple-view)) (declare (ignorable self)) nil))
(defgeneric out-put            (patch) (:method ((self simple-view)) (declare (ignorable self)) nil))
(defgeneric pw-function-string (patch) (:method ((self simple-view)) (declare (ignorable self)) nil))
(defgeneric pw-function        (patch) (:method ((self simple-view)) (declare (ignorable self)) nil))


(defclass C-patch (ui:view) 
  ((input-objects :initform nil  :accessor input-objects)
   (pw-controls :initform nil  :accessor pw-controls)
   (type-list :initform ()  :initarg :type-list :accessor type-list)
   (in-xs :initform nil  :accessor in-xs)
   (in-ys :initform nil  :accessor in-ys)
   (active-mode :initform nil :initarg :active-mode :accessor active-mode)
   (flip-flag :initform t  :accessor flip-flag)
   (out-put :initform nil :initarg :out-put :accessor out-put)
   (pw-function-string :initform "+"  :accessor pw-function-string)
   (pw-function :initform '+ :initarg :pw-function :accessor pw-function)))

(defgeneric decompile-connections (self patch-controls)
  (:method ((self C-patch) patch-controls)
    (let ((input-objects (input-objects self))
          (connections) input-objects-now
          (nth-control-self (nth? self patch-controls))
          nth-control)
      (for (i 0 1 (1- (length input-objects))) 
        (when (not (eql (nth i input-objects) (nth i (pw-controls self))))
          (setq input-objects-now
                (if (atom (nth i input-objects))
                    (list (nth i input-objects))
                    (nth i input-objects)))
          (while input-objects-now
            (setq nth-control (nth? (pop input-objects-now)  patch-controls))
            (when nth-control 
              (push  
               `(connect-nth-control (nth ,nth-control-self controls) ,i (nth ,nth-control controls)) 
               connections)))))
      (nreverse connections))))


(defgeneric actual-save (self file-name))
(defmethod actual-save ((self c-patch) file-name)
  (let ((*print-pretty* nil)
        (*decompile-chords-mode* t))
    (setf (pw-function-string self) (string-downcase (pathname-name file-name)))
    (with-cursor *watch-cursor*
      (WITH-OPEN-FILE (out file-name
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
        (prin1 '(in-package :pw) out)
        (let ((*package* :pw))
          (prin1 `(add-patch-box *active-patch-window* ,(decompile self)) out))))))

(defmethod save ((self C-patch))
  (if *pw-nosave-mode* 
      (ui::message-dialog "Sorry this version cannot save files.")
      (let ((path (choose-new-file-dialog :directory (string (pw-function self)) 
                                                :button-string "Save patch as")))
        (when path
          (actual-save self path)))))

(defmethod yourself-if-collecting ((self C-patch)) nil)
;;======================================================
;;init

(defmethod initialize-instance :after ((self C-patch) &key controls)
  (declare (ignore controls))
  (setf (input-objects self) (copy-list (subviews self)))
  (setf (pw-controls self) (copy-list (subviews self)))
  (let (out-box)
    (add-subviews self (setq out-box (make-instance 'C-pw-outrect 
                                                    :view-position 
                                                    (make-point (- (round (w self) 2) 6) 
                                                                (- (h self) 5))
                                                    :view-size (make-point 10 5))))
    (setf (out-put self) out-box))
  (init-pw-function-string self)
  (init-xs-ys self))

(defgeneric init-pw-function-string (self)
  (:method ((self C-patch))
    (setf (pw-function-string self) 
          (mk-nuevo-name-box  (string-downcase (string (pw-function self)))))))

(defgeneric init-xs-ys (self)
  (:method ((self C-patch))
    (let* ((ctrls  (pw-controls self))
           (nctrls (length ctrls)))
      (setf (in-xs self) (make-list nctrls)) 
      (setf (in-ys self) (make-list nctrls)) 
      (for (i 0 1 (1- nctrls)) 
        (setf (nth i (in-xs self)) (if (> (x (nth i ctrls)) 5) (- (w self) 3) 0))
        (setf (nth i (in-ys self)) (+ (y (nth i ctrls)) (- (truncate (h (nth i ctrls)) 2) 3)))))))

(defgeneric erase-BPF-label (self)
  (:method ((self C-patch))
    nil)) 

;;======================================================
;;events

;; can be used by subclasses
;; should return as value a flag

(defmethod mouse-pressed-no-active-extra ((self C-patch) x y) 
  (declare (ignore x y))
  nil)

(defvar *draw-dragging-mode* ())

(defun flip-draw-mode ()
  (declare (special *draw-dragging-mode* *PWoper-menu*))
  (let ((menu (find-menu-item *PWoper-menu* "Draw all when dragging")))
    (if (menu-item-check-mark menu)
        (set-menu-item-check-mark menu nil)
        (set-menu-item-check-mark menu t))
    (setf *draw-dragging-mode* (not *draw-dragging-mode*))))

(defmethod view-click-event-handler ((self C-patch) where)
  (let ((res (call-next-method)))
    #+debug-views
    (format-trace '(view-click-event-handler c-patch)
                  :where (point-to-list where)
                  :view-size (list (h self) (w self))
                  :dbl (double-click-p)
                  :c-o (and (control-key-p) (option-key-p))
                  :c (control-key-p)
                  :topbar (inside-rectangle? (point-h where) (point-v where) 0 0 (w self) 5)
                  :botright (inside-rectangle? (point-h where) (point-v where) (- (w self) 5) (- (h self) 5) 5 5)
                  :botbox (inside-rectangle? (point-h where) (point-v where) 0 (- (h self) 12) 15 12)
                  :o (option-key-p)
                  :methods (compute-applicable-methods (function view-click-event-handler) (list self where))
                  :res res
                  :self self)
    (cond ((eql self res)             ;inside patch,no active controls
           (cond ((double-click-p)
                  #+debug-click (format-trace'click 'double-click 'open-patch-win)
                  (open-patch-win self))
                 ((and (control-key-p) (option-key-p))
                  #+debug-click (format-trace'click '(control option) 'delete-extra-inputs)
                  (delete-extra-inputs self))
                 ((control-key-p)
                  #+debug-click (format-trace'click '(control) 'change-position)
                  (change-position self where))
                 ((inside-rectangle? (point-h where) (point-v where) 0 0 (w self) 5) ; top bar
                  #+debug-click (format-trace'click '(move top bar) 'change-position)
                  #+debug-views (format-trace 'view-click-event-handler "move top bar " (list (point-h where) (point-v where)) self)
                  (change-position self where))
                 ((inside-rectangle? (point-h where) (point-v where) (- (w self) 5) (- (h self) 5) 5 5) ; botright corner
                  #+debug-click (format-trace'click '(resize corner) 'change-size)
                  #+debug-views (format-trace 'view-click-event-handler "resize corner" (list (point-h where) (point-v where)) self)
                  (change-size self (subtract-points (view-size self) where)))
                 ((inside-rectangle? (point-h where) (point-v where) 0 (- (h self) 12) 15 12) ; bottom left box
                  
                  #+debug-views (format-trace 'view-click-event-handler "bottom box" (list (point-h where) (point-v where)) self)
                  (cond ((option-key-p)
                         #+debug-click (format-trace'click '(bottom left box) '(option) 'output-type)
                         (print (list 'outputtype (type-list self)))) 
                        ((command-key-p)
                         #+debug-click (format-trace'click '(bottom left box) '(command) 'input-type)
                         (print (list 'inputtypes (mapcar 'list 
                                                          (ask-all (pw-controls self) 'doc-string)
                                                          (ask-all (pw-controls self) 'type-list))))) 
                        (t
                         #+debug-click (format-trace'click '(bottom left box) '() 'flip-controls)
                         (flip-controls self (setf (flip-flag self) (not (flip-flag self))))))) 
                 ((option-key-p)
                  #+debug-click (format-trace'click '(option) 'no-active-extra)
                  #+debug-views (format-trace 'view-click-event-handler "no active extra" (list (point-h where) (point-v where)) self)
                  (mouse-pressed-no-active-extra self (point-h where) (point-v where)))
                 (t
                  #+debug-click (format-trace'click '() 'toggle-patch-active-mode)
                  #+debug-views (format-trace 'view-click-event-handler "toggle patch active" (list (point-h where) (point-v where)) self)
                  (toggle-patch-active-mode self))))
          ((option-key-p)               ;inside controls
           #+debug-click (format-trace'click '(inside controls) '(option) 'disconnect-control)
           #+debug-views (format-trace 'view-click-event-handler `(bad with option-key-p (call-next-method) -> ,res))
           (let ((ctrl (ask (pw-controls self) #'view-contains-point-p+self where)))
             (when ctrl 
               (disconnect-ctrl self ctrl)
               (record-event :|PWst| :|unco|
                             `((,:|----| ,(mkSO :|cinp| (mkSO :|cbox| nil :|name| (pw-function-string self)) 
                                                :|indx| (+ (position ctrl (input-objects self)) 1))))))))
          (t
           #+debug-click (format-trace'click '(inside controls) '() 'bad)
           #+debug-views (format-trace 'view-click-event-handler `(bad (call-next-method) -> ,res)))))) 


;;======================================================
;;draw


#-(and) (defmethod view-focus-and-draw-contents ((v C-patch) &optional visrgn cliprgn)
          (declare (ignore visrgn cliprgn))
          (with-focused-view v
            (view-draw-contents v)))

(defmethod view-draw-contents ((self C-patch))
  ;; #|PJB-DEBUG|#
  (format-trace "progn (view-draw-contents" 'c-patch 'before self)
  (with-font-focused-view self  
    (erase-rect* 0 0 (w self) (h self))
    #+debug-views-colors (with-pen-state (:pattern *light-gray-pattern*)
                           (with-fore-color *light-gray-color*
                             (fill-rect* 1 1 (- (w self) 2) (- (h self) 2))))
    (call-next-method)
    (draw-small-rects self)
    (draw-patch-view-outline self)
    (draw-function-name self 2 (- (h self) 5))
    (draw-rect* 0 0 (w self) (h self))
    #+debug-views-colors (with-fore-color ui::*blue-color*
                           (draw-rect* 0 0 (w self)(h self))))
  ;; #|PJB-DEBUG|# (format-trace "view-draw-contents)" 'c-patch 'after- self)
  )

(defgeneric print-connections (self &optional erase-mode)
  (:method ((self C-patch) &optional erase-mode)
    (setq erase-mode (if erase-mode *white-pattern* *black-pattern*))
    (with-pen-state (:mode :srccopy)    ;srccopy  
      (let* ((input-objects (input-objects self)) (pw-controls (pw-controls self))
             (x-self (x self))
             (y-self (y self))
             (in-xs (in-xs self))
             (in-ys (in-ys self))
             ctrl x-off x-off2 
             x1 y1 xi yi right?) 
        (for (i 0 1 (1- (length pw-controls)))
          (setq x-off (if (setq right? (> (car in-xs) 0)) 6 -4))
          (setq x-off2 (if right? (* (truncate i 2) 2) (* (truncate i 2) -2)))
          (when (and (setq ctrl (pop input-objects))
                     (not (eql ctrl (pop pw-controls))))
            (when (atom ctrl)(setq ctrl (list ctrl))) ; for nargs  
            (while ctrl
              (unless (eql erase-mode *white-pattern*) 
                (setq erase-mode (if (pw-object-p (type-list (car ctrl)))
                                     *object-connection-draw-mode* *normal-connection-draw-mode*)))
              (with-pen-state (:pattern erase-mode)
                (setq x1 (mid-x (out-put (car ctrl))))
                (setq y1 (mid-y (out-put (pop ctrl))))
                (setq xi (+ x-self (car in-xs)))
                (setq yi (+ y-self (car in-ys)))
                (draw-line x1 (+ 1 y1) x1 (+ 3 y1)) 
                (draw-line x1 (+ 4 y1) (+ x-off2  x-off xi) (+ 4 y1))
                (draw-line (+ x-off2  x-off xi) (+ 4 y1) 
                           (+ x-off2 x-off xi) (+ 2 yi))
                (draw-line (+ x-off2 x-off xi)(+ 2 yi)
                           (if (= x-off 6)(+ 3 xi)(1- xi)) (+ 2 yi)))))
          (pop in-xs)(pop in-ys))))))


(defmethod draw-patch-extra ((self C-patch))
  (values))

(defgeneric draw-patch-view-outline (self)
  (:method ((self C-patch))
    (when (active-mode self) 
      (with-pen-state (:pattern *black-pattern*)
        (fill-rect* 0 0 (w self) 3)))
    (draw-line 0 3 (1- (w self)) 3)
    (draw-patch-extra self)))

(defmethod draw-function-name ((self C-patch) x y)
  (draw-string x y (pw-function-string self)))

(defgeneric update-absin-doc-string (self)
  (:method ((self C-patch))
    (values)))

(defgeneric draw-small-rects (self)
  (:method ((self C-patch))
    (for (i 0 1 (1- (length (pw-controls self))))
      (unless (member 'no-connection (type-list (nth i (pw-controls self))) :test 'eq)
        (draw-rect* (nth i (in-xs self)) (nth i (in-ys self)) 3 6)))))

;; draw-connections is called only by the window not by a patch !


(defun pw-object-p (types-list) 
  (intersection types-list *pw-object-type-list* :test 'eq))

(defgeneric draw-connections (self &optional erase-mode from-patches)
  (:method ((self simple-view) &optional erase-mode from-patches)
    (declare (ignorable self erase-mode from-patches)))
  (:method ((self C-patch) &optional erase-mode from-patches)
    (with-focused-view (view-window self)
      (setq erase-mode (if erase-mode *white-pattern* *black-pattern*))
      (with-pen-state (:mode :srccopy)  ;srccopy  
        (let* ((input-objects (input-objects self)) (pw-controls (pw-controls self))
               (x-self (x self))
               (y-self (y self))
               (in-xs (in-xs self))
               (in-ys (in-ys self))
               ctrl x-off x-off2 
               x1 y1 xi yi right?) 
          (for (i 0 1 (1- (length pw-controls)))
            (setq x-off (if (setq right? (> (car in-xs) 0)) 6 -4))
            (setq x-off2 (if right? (* (truncate i 2) 2) (* (truncate i 2) -2)))
            (when (and (setq ctrl (pop input-objects))
                       (not (eql ctrl (pop pw-controls)))
                       (or (null from-patches) (member ctrl from-patches :test #'eq)))
              (if (atom ctrl) (setq ctrl (list ctrl))) ; for nargs  
              (while ctrl
                (unless (eql erase-mode *white-pattern*) 
                  (setq erase-mode (if (pw-object-p (type-list (car ctrl)))
                                       *object-connection-draw-mode*
                                       *normal-connection-draw-mode*)))
                (with-pen-state (:pattern erase-mode)
                  (setq x1 (mid-x (out-put (car ctrl))))
                  (setq y1 (mid-y (out-put (pop ctrl))))
                  (setq xi (+ x-self (car in-xs)))
                  (setq yi (+ y-self (car in-ys)))
                  (draw-line x1 (+ 1 y1) x1 (+ 3 y1)) 
                  (draw-line x1 (+ 4 y1) (+ x-off2  x-off xi) (+ 4 y1))
                  (draw-line (+ x-off2  x-off xi) (+ 4 y1) 
                             (+ x-off2 x-off xi) (+ 2 yi))
                  (draw-line (+ x-off2 x-off xi)(+ 2 yi)
                             (if (= x-off 6)(+ 3 xi)(1- xi)) (+ 2 yi)))))
            (pop in-xs)(pop in-ys)))))))

(defgeneric repaint-connections (self all-dead)
  (:method ((self C-patch) all-dead)
    (let ((objects (set-difference (input-objects self) all-dead)))
      (dolist (dead-patch all-dead)
        (if (intersection objects (input-objects dead-patch))
            (draw-connections self))))))

;;======================================================
;;connect

(defgeneric check-recursive-connections (self patch))
(defmethod check-recursive-connections ((self simple-view) patch)
  (declare (ignore patch))
  nil)

(defmethod check-recursive-connections ((self C-patch) patch)
  (if (member patch (input-objects self) :test #'eq)
      T
      (progn 
        (let ((inputs (input-objects self)) flag)
          (while inputs
            (if (listp (car inputs))
                (setq flag (ask (car inputs)  #'check-recursive-connections patch)) 
                (setq flag (check-recursive-connections (car inputs) patch)))
            (when flag (setq inputs ()))
            (pop inputs))
          flag))))

(defgeneric connect-nth-control (self nth-ctrl ctrl-panel)
  (:method ((self C-patch) nth-ctrl ctrl-panel)
    (setf (nth nth-ctrl (input-objects self)) ctrl-panel)
    (setf (open-state (nth nth-ctrl (pw-controls self))) ())))

(defgeneric find-nth-ctrl (self ctrl)
  (:method ((self C-patch) ctrl)
    (let ((ctrls (pw-controls self))
          (nth-count))
      (for (i 0 1 (1- (length ctrls)))
        (when (eql (nth i ctrls) ctrl)
          (setq nth-count i) (setq i 1000)))
      nth-count)))

(defgeneric connect-ctrl (self ctrl patch)
  (:method ((self C-patch) ctrl patch)
    (setf (nth (find-nth-ctrl self ctrl) (input-objects self)) patch)))

(defgeneric disconnect-my-self (self patch)
  (:method ((self C-patch) patch)
    (for (i 0 1 (1- (length (input-objects self))))
      (when (eql (nth i (input-objects self)) patch)
        (setf (nth i (input-objects self))(nth i (pw-controls self)))
        (setf (open-state (nth i (pw-controls self))) t)))))

(defgeneric  disconnect-ctrl-1 (self ctrl)
  (:method ((self C-patch) ctrl)
    (let ((nth-count (find-nth-ctrl self ctrl)))
      (setf (nth nth-count (input-objects self)) ctrl))))

(defgeneric disconnect-ctrl (self ctrl))
(defmethod  disconnect-ctrl ((self C-patch) ctrl)
  (let ((nth-count (find-nth-ctrl self ctrl)))
    (when (not (eql ctrl (nth nth-count (input-objects self))))
      (tell (controls (view-window self)) 'draw-connections t)
      (setf (nth nth-count (input-objects self)) ctrl)
      (tell (controls (view-window self)) 'draw-connections)
      (setf (open-state ctrl) t)
      (view-focus-and-draw-contents ctrl))))


;;======================================================
;;resize

(defmethod set-view-size :before ((view C-patch) h &optional v)
  (declare (ignore h v))
  (inval-r-view-sides view))

(defmethod set-view-size :after ((view C-patch) h &optional v)
  (declare (ignore h v))
  (inval-r-view-sides view))

(defgeneric inval-r-view-sides (view &optional top&left\?)
  (:method ((view C-patch) &optional top&left?)
    (niy inval-r-view-sides view top&left?)
    ;; (when (wptr view)
    ;;   (let* ((pos (view-scroll-position view))
    ;;          (size (view-size view))
    ;;          (end-pos (add-points pos size))
    ;;          (rgn *r-view-temp-region*))
    ;;     (unless top&left?
    ;;       (setq pos (subtract-points pos #@(1 1))))
    ;;     (#_SetRectRgn :ptr rgn :long pos :long end-pos)
    ;;     (#_InsetRgn :ptr rgn :long #@(1 1))
    ;;     (#_DiffRgn :ptr (view-clip-region view) :ptr rgn :ptr rgn)
    ;;     (with-focused-view view
    ;;       (#_EraseRgn :ptr rgn)
    ;;       (#_InvalRgn :ptr rgn))))
    ))

;;======================================================
;;move

(defgeneric dmove-patch (self dx dy)
  (:method ((self C-patch) dx dy)
    ;; (record-event :|core| :|move| `((,:|----| ,(mkSO :|cbox| nil :|name| (pw-function-string self)))
    ;;                                     (,:|insh| ,(cl-user::getDescRecPtr (cl-user::asAEDesc (list (+ (x self) dx) (+ (y self) dy)))))))
    (set-view-position self (+ (x self) dx) (+ (y self) dy))))

(defmethod set-view-scroll-position ((view C-patch) h &optional v scroll-visibly?)
  (declare (ignore scroll-visibly?))
  (without-interrupts
    (let ((old-scroll (view-scroll-position view)))
      (call-next-method)
      (let ((pos (view-position view)))
        (unwind-protect
             (setf (ui::%view-position view) (add-points pos (subtract-points old-scroll (make-point h v))))
          (inval-r-view-sides view t))
        (setf (ui::%view-position view) pos)))
    (inval-r-view-sides view t)))


(defgeneric handle-edit-events (self char)
  (:method ((self C-patch) char)
    (format-trace '(handle-edit-events c-path) :char char :text (dialog-item-text *pw-controls-dialog-text-item*))
    (case char
      ((#\Newline
        #|Enter:|# #\Etx
                   #+(and has-return (not newline-is-return)) #\Return
                   #+(and has-linefeed (not newline-is-linefeed)) #\Linefeed)
       (set-dialog-item-text-from-dialog *pw-controls-current-pw-control* 
                                         (dialog-item-text *pw-controls-dialog-text-item*))
       (kill-text-item))
      (otherwise
       (view-key-event-handler *pw-controls-dialog-text-item* char)
       (set-fred-display-start-mark *pw-controls-dialog-text-item* 0)
       (fred-update *pw-controls-dialog-text-item*)))))


(defun push-modules-to-back (actives)
  (let* ((modules (slot-value (view-container (car actives)) 'view-subviews))
         (length (1- (length modules)))
         (rest actives) (index length)  box)
    (for (i length -1 0)
      (when (member (setq box (aref modules i)) rest :test #'eq)
        (when (/= index i)
          (psetf (aref modules index) box 
                 (aref modules i) (aref modules index)
                 rest (remove box rest :test #'eq))
          (unless rest (return )))
        (decf index)))))

(defgeneric push-to-top (view)
  (:method ((view C-patch))
    (let* ((modules (slot-value (view-container view) 'view-subviews))
           (length (1- (length modules)))
           (index (for (i 0 1 length) (if (eql view (aref modules i)) (return i)))))
      (if (> index 2)
          (psetf (aref modules 0) (aref modules index) 
                 (aref modules index) (aref modules 0))))))

(defgeneric view-frame-patch (self offset)
  (:method ((self C-patch) offset)
    (let ((position (add-points (view-position self) offset)))
      (with-focused-view (view-window self)
        (with-pen-state (:pattern *gray-pattern* :mode :patCopy)
          (draw-rect* (point-h position) (point-v position) (w self) (h self)))))))

(defgeneric change-position (view where)
  ;; where = offset from the topleft
  (:method ((view C-patch) where)
    (let* ((container           (view-container view))
           (orig-mp             (add-points (view-position view) where))
           (prev-mp             orig-mp)
           (window              (view-window view))
           (active-patches      (active-patches window))
           (active-patches-rest (remove view active-patches :test 'eq))
           (group-move?         (and (member view active-patches :test 'eq)
                                     active-patches))
           (moving-patches      (or group-move? (list view)))
           (the-rest            (set-difference (controls window) moving-patches :test #'eq)))
      (connect/unconn moving-patches the-rest t)
      (unwind-protect
           (progn
             (with-instance-drawing container
               (tell moving-patches 'view-frame-patch (subtract-points prev-mp orig-mp))
               (loop
                 (event-dispatch)
                 (unless (mouse-down-p) (return))
                 (let ((mp (view-mouse-position container)))
                   (unless (eql prev-mp mp)
                     (new-instance container)
                     (tell moving-patches 'view-frame-patch (subtract-points prev-mp orig-mp))
                     (setq prev-mp mp))))
               (new-instance container))
             (unless (eql prev-mp orig-mp)
               (push-to-top view)
               (let ((new-position  (add-points (view-position view)
                                                (subtract-points prev-mp orig-mp))))
                 (set-view-position view new-position)
                 #-(and) ;; TODO
                 (record-event :|core| :|move| `((,:|----| ,(mkSO :|cbox| nil :|name| (pw-function-string view)))
                                                 (,:|insh| ,(cl-user::getDescRecPtr
                                                             (cl-user::asAEDesc
                                                              (point-to-list new-position)))))))
               (when group-move? 
                 (let ((dif-h (- (point-h prev-mp) (point-h orig-mp)))
                       (dif-v (- (point-v prev-mp) (point-v orig-mp)))
                       (max-h (- *screen-width* 10 
                                 (point-h (view-position container))))
                       (max-v (- *screen-height* 10 
                                 (point-v (view-position container)))))
                   (dolist (patch active-patches-rest)
                     (dmove-patch patch 
                                  (min dif-h (- max-h
                                                (point-h (view-position patch))))
                                  (min dif-v (- max-v 
                                                (point-v (view-position patch))))))))
               (when (shift-key-p)
                 (push-modules-to-back moving-patches))))
        (connect/unconn moving-patches the-rest)))))

(defgeneric change-size (view where)
  ;; where = offset from the botright.
  (:method ((view C-patch) where)
    (when (resize-patch? view)
      (let ((the-rest (remove view (controls (view-window view)) :test #'eq)))
        (connect/unconn (list view) the-rest t)
        (unwind-protect
             (let* ((container (view-container view))
                    (last-mp   (add-points (view-position view) where))
                    (delta     (subtract-points 0 last-mp)))
               (with-instance-drawing container
                 (view-frame-patch view 0)
                 (loop
                   (unless (mouse-down-p) (return))
                   (let ((mp (view-mouse-position view)))
                     (unless (eql mp last-mp)
                       (setq last-mp mp)        
                       (resize-patch-box view mp delta)
                       (new-instance container)
                       (view-frame-patch view 0))))
                 (new-instance container)))
          (connect/unconn (list view) the-rest))
        (view-draw-contents view)))))

(defmethod resize-patch-box ((self C-patch) mp delta)
  (declare (ignore mp delta))
  (values))

(defgeneric rename-yourbox (view)
  (:method ((view C-patch))
    (if (pw-controls view)
        (open-edit-text-item-for-box view (pw-function-string view))
        (ui:ed-beep))))

(defmethod set-dialog-item-text-from-dialog ((view C-patch) str)
  (setf (pw-function-string view) (string-downcase str))
  (with-focused-view view
    (draw-function-name view 2 (- (h view) 5)))) 

;;====================================================
;;misc

(defgeneric am-i-connected? (self patch)
  (:method ((self C-patch) patch)
    (member patch (input-objects self) :test 'eq)))

(defgeneric connected-out-side? (self win)
  (:method ((self C-patch) win)
    (let ((ctrls (controls win))(connect-flag))
      (while (and ctrls (not connect-flag))
        (setq connect-flag (am-i-connected? (pop ctrls) self)))
      (or connect-flag
          (not (equal (pw-controls self) (input-objects self)))))))

(defgeneric connect-new-patch? (self old-patch new-patch)
  (:method ((self C-patch) old-patch new-patch)
    (for (i 0 1 (1- (length (input-objects self))))
      (when (eql (nth i (input-objects self)) old-patch)
        (setf (nth i (input-objects self)) new-patch)))))

(defgeneric draw-active-mode (self)
  (:method ((self C-patch))
    (with-focused-view self
      (with-pen-state (:mode :patcopy :pattern (if (active-mode self) *black-pattern* *white-pattern*))
        (fill-rect* 1 1 (- (w self) 2) 2)))))

(defgeneric toggle-patch-active-mode (self)
  (:method ((self C-patch))
    (cond ((shift-key-p)
           (setf (active-mode self) (not (active-mode self)))
           (record-event :|PWst| :|sele| `((,:|----| ,(mkSO :|cbox| nil :|name| (pw-function-string self)))
                                           (,:|chif| ,t))))
          ((active-mode self))
          (t
           (tell (controls (view-window self)) 'deactivate-control)
           (setf (active-mode self) t)
           (record-event :|PWst| :|sele| `((,:|----| ,(mkSO :|cbox| nil :|name| (pw-function-string self)))))))
    (draw-active-mode self)))

(defgeneric activate-control (self)
  (:method ((self simple-view))
    (declare (ignorable self)))
  (:method ((self C-patch))
    (unless (active-mode self)
      (setf (active-mode self) t)
      (draw-active-mode self))))

(defgeneric deactivate-control (self)
  (:method ((self simple-view))
    (declare (ignorable self)))
  (:method ((self C-patch))
    (when (active-mode self)
      (setf (active-mode self) nil)
      (draw-active-mode self))))

(defgeneric flip-controls (self flag)
  (:method ((self simple-view) flag)
    (declare (ignorable self flag)))
  (:method ((self C-patch) flag)
    (for (i 0 1 (1- (length (pw-controls self)))) 
      (when (eql (nth i (pw-controls self))(nth i (input-objects self)))
        (set-open-state (nth i (pw-controls self)) flag)))))

(defmethod patch-value ((self C-patch) obj)
  (let ((args (ask-all (input-objects self) 'patch-value obj)))
    (apply (pw-function self) args)))

(defgeneric active-mode+self (self)
  (:method ((self C-patch)) (when (active-mode self) self)))

(defmethod x+w ((self C-patch)) (+ (x self)(w self)))

(defgeneric delete-r-view (view where)
  (:method ((view C-patch) where)
    (declare (ignore where))
    (set-view-container view nil)))

;;______________
(defgeneric nth-connected-p (self nth-input)
  (:method ((self C-patch) nth-input)
    (not (eql (nth nth-input (input-objects self))(nth nth-input (pw-controls self))))))

;;;(defmethod mouse-released-control-after ((self C-patch) ctrl win))
(defmethod  update-win-pointers (patch window)
  (declare (ignorable patch window))
  (values))

(defmethod clock ((self C-patch))
  (clock (clock-obj self)))

;; (defmethod browse ((self C-patch)) (print "no browse boxes"))

(defmethod remove-yourself-control ((self C-patch)))
(defmethod set-pw-window-pointers ((self C-patch) win)(declare (ignore win))) 
(defmethod resize-patch? ((self C-patch)) nil) 

(defgeneric patch-type-p (self))
(defmethod patch-type-p ((self T)) nil) 
(defmethod patch-type-p ((self simple-view)) nil) 
(defmethod patch-type-p ((self C-patch)) t) 

;;so that clocked modules don't give an error on clicking
(defmethod clock-obj ((self C-patch)) *global-clock*)

(defgeneric are-you-handling-keys? (self char))
(defmethod are-you-handling-keys? ((self C-patch) char)
  (declare (ignore char)) nil)

;; (defclass C-key-handler-box (C-patch) ())
(defclass C-key-handler-box (C-pw-functional) ())

(defmethod are-you-handling-keys? ((self C-key-handler-box) char)
  (when (char= (aref (format nil "~A" (patch-value (first (input-objects self)) nil))
                     0)
               char)
      (or (patch-value self ()) t)))

(defunp key-trigger ((char list (:type-list (no-connection) :value "a"))
                     (patch list (:type-list nil))) nil
    "triggers a patch by a key pressed"
  (declare (ignore char patch)))




;; ------------------------------ tutorial search scheme --------------------------------

(defgeneric get-tutorial-patch (self)
  (:method ((self C-patch))
    (let ((fun (pw-function self)))
      (if (symbolp fun)
          (or (get-patch-file fun) (format t "no tutorial available for box: ~S ~%" fun))
          (format t "no tutorial available for box: ~S ~%" (pw-function-string self))))))

(defvar *tutorial-a-list* ())
(defvar *tutorial-dirs* '("PW-HELP-DOC:**;"))


;; (eval-when (load eval compile)
;;   (load-once  "PW-HELP-DOC:box-name-list.lisp" :if-does-not-exist nil))

;; (defun get-patch-file (file)
;;   (let* ((a-list *tutorial-a-list*)
;;          (file (or (second (assoc file a-list :test #'string=)) file)))
;;     (let ((name (first (directory (format nil "PW-HELP-DOC:**;~A.pw" file)))))
;;       (and name (load-a-patch (namestring name)))
;;       name)))




(defun push-tutorial-dir (dir)
  " adds a directory path to the tutorial directory list. When PatchWork is started the direcotry list
contains one item: PW-HELP-DOC which is normally pw:documentation;online-help and all subdirectories"
  (when dir
    (pushnew dir *tutorial-dirs*)))

(defun push-tutorial-file (file)
  "adds an element to the tutorial list. This list is used to map a module's name to a 
tutorial file only if the names differ"
  (cond ((and (listp file) (not (member-if-not #'listp file)))
         (setf *tutorial-a-list* (append file *tutorial-a-list*)))
        ((and (listp file) (member-if-not #'listp file))
         (pushnew file *tutorial-a-list*))
        (t ())))

;;(push-tutorial-file '((filter-spdata llmod-read) (par-spdata iana-read)))

;; --------- remplace dans kernel:???:pw-patch.lisp ------------
(defun get-patch-file (file)
  (let* ((a-list *tutorial-a-list*)
         (file (or (second (assoc file a-list :test #'string=)) file))
         (name (get-tutorial-aux file *tutorial-dirs*)))
    (and name (load-a-patch (namestring (first name))))
    name))

(defun get-tutorial-aux (file dirs)
  (when (and file dirs)
    (let ((name (directory (format nil "~A~A.pw" (first dirs) file))))
      (if name name (get-tutorial-aux file (cdr dirs))))))






;;______________ 
;;init-patch 

(defmethod init-patch ((self C-ttybox)))

(defmethod init-patch ((self C-patch))
  (tell (input-objects self) 'init-patch))

;;====================================================

(defun add-patch-box (win patch)
  (init-patch-pos win patch)
  (add-subviews win patch)
  (set-changes-to-file-flag win)
  (setf *position-new-box* nil)
  (record-patch (pw-function patch)  
                (list (point-h (view-position patch)) (point-v (view-position patch))) nil)
  patch) 

(defvar *pw-window-counter* 0)

(defun make-new-pw-window (&optional close-button)
  (let* ((win-string (format nil "PW~D" (incf *pw-window-counter*)))
         (win        (make-instance 'C-pw-window 
                                    :window-title  win-string
                                    :close-box-p   close-button
                                    :view-position (make-point 50 38)
                                    :view-size     (make-point 500 300)
                                    :window-show   t)))
    (ui:add-menu-items *pw-windows-menu* 
                       (setf (wins-menu-item win)
                             (new-leafmenu win-string (lambda () (window-select win)))))
    (push win *pw-window-list*) 
    (update-wins-menu-items win)
    (record-event :|core| :|crel| `((,:|kocl| ,:|cpat| )))
    (setf *active-patch-window* win)))



;;;; THE END ;;;;


