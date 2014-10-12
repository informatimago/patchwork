;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               rtm-patch.lisp
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

(defun count-positive-rtms (rtm-list)
  (let ((res 0))
    (while rtm-list
      (unless (or (< (car rtm-list) 0)(floatp (car rtm-list))) (incf res))
      (pop rtm-list))
    res))
;;(count-positive-rtms '(1 -1 3 1.0 5))


(defun make-rtm-editor-window (measure-line)
 (let* ((win-string (concatenate  'string  "RTM" (format nil "~D" (incf *RTM-window-counter*))))
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


;;(window-select (setf gh (make-rtm-editor-window)))
;;(window-select (setf gh2 (eval (decompile gh))))    
;;(window-select (setf gh3 (eval (decompile gh2))))    
;;==================

(defclass  C-patch-application-rtm-editor (C-patch-application C-process-begin+end)  
  ((clock           :initform 0                               :accessor   clock)
   (clock-obj       :initform *global-clock*                  :allocation :class          :accessor clock-obj)
   (chord-objects   :initform nil                             :accessor   chord-objects)
   ;; (previous-t-time :initform nil                             :accessor   previous-t-time)
   (play-flag       :initform nil                             :accessor   play-flag)
   (measure-line    :initform (make-instance 'C-measure-line) :initarg    :measure-line   :accessor measure-line)))
;;==================
;;???
;; (defmethod decompile ((self C-patch-application-rtm-editor))
;;   `(let ((patch (make-instance ',(class-name (class-of self))
;;                                :view-position ,(view-position self)
;;                                :view-size ,(view-size self)
;;                                :active-mode  ,(active-mode self)
;;                                :pw-function  ',(pw-function self)
;;                                :type-list ',(type-list self) 
;;                                ;;               :measure-line-list (list ,@(ask-all (measure-line-list self) 'decompile))
;;                                :application-object 
;;                                ,(when (wptr (application-object self)) (decompile (application-object self)))
;;                                :view-subviews (list ,@(ask-all (pw-controls self) 'decompile)))))
;;      patch))

(defmethod decompile ((self C-patch-application-rtm-editor))
  (append (call-next-method) 
          (list nil  (if *decompile-chords-mode* 
                            `(list ,@(get-measureline-form self))))))

(defgeneric get-measureline-form (self))
(defmethod get-measureline-form ((self C-patch-application-rtm-editor))
  (let ((win (application-object self)))
    (if (wptr win)
        (list `',(get-window-state self win) (decompile (measure-line self)) (value self))
        (list `',(window-state self) (decompile (measure-line self)) (value self)))))

(defmethod complete-box ((self C-patch-application-rtm-editor) args)
  (when args
    (setf (measure-line self) (second args))
    (put-window-state self (application-object self) (first args))
    (setf (value self) (third args))
    (if (third args) (set-dialog-item-text (lock self) "x"))
    (setf (measure-line (car (beat-editors (editor-collection-object (application-object self)))))
          (measure-line self))))

(defmethod initialize-instance :after ((self C-patch-application-rtm-editor) &key controls)
  (declare (ignore controls))
  (setf (process self) 'continue-record)
  (make-rtm-lock self))


(defgeneric make-rtm-lock (self))
(defmethod make-rtm-lock ((self C-patch-application-rtm-editor))
  (-make-lock self (make-point (- (w self) 30) (- (h self) 10))))

(defmethod get-window-state ((self C-patch-application-rtm-editor) win)
  (let ((rtm-view (editor-collection-object win)))
    (append 
            (get-window-state rtm-view win)
            (get-window-state (car (beat-editors rtm-view)) win)
            (list (view-position win)(view-size win)(window-title win)))))  

(defmethod put-window-state ((self C-patch-application-rtm-editor) win state)
  (set-view-position win (nth 11 state))
  (set-view-size win (nth 12 state))
  (when (stringp (nth 13 state)) (set-window-title win (nth 13 state)))
  (put-window-state (editor-collection-object win) win (subseq  state 0 5)) 
  (put-window-state (car (beat-editors (editor-collection-object win))) win (subseq state 5 11))) 

;;==================

(defmethod make-application-object ((self C-patch-application-rtm-editor))
  (setf (application-object self) (make-rtm-editor-window (measure-line self))))

(defgeneric give-measure-line (self)
  (:method ((self C-patch-application-rtm-editor))
    (measure-line (car (beat-editors (editor-collection-object (application-object self)))))))

(defmethod patch-value ((self C-patch-application-rtm-editor) obj)
  (declare (ignore obj))
  (if (value self) 
    (measure-line self)
    (let ((measure-line (measure-line self))
          (measures (patch-value (car (input-objects self)) self)))
      (when (not (wptr (application-object self)))
        (setf (application-object self) (make-application-object self))
        (set-pw-win+pw-obj (application-object self) *active-patch-window* self)
        (restore-window-state self (application-object self))
        (erase+view-draw-contents self))
      (unless (listp measures) (setq measures (list measures)))
      (when (nth-connected-p self 0)
        ;(init-patch (car (input-objects self)))
        (setf (measures measure-line) measures)
        (with-focused-view (application-object self)
          (erase+view-draw-contents (application-object self))))
      measure-line)))

;;==================


(defmethod yourself-if-collecting ((self C-patch-application-rtm-editor)) self)

(defmethod draw-clock ((self C-patch-application-rtm-editor))
 (with-focused-view self
   (set-view-font  (view-container  self) '(:srccopy))
   (draw-string  52 (- (h self) 4) (format nil "~5D" (clock (clock-obj self))))
   (set-view-font  (view-container  self) '(:srcor))))

;; TODO: delete: (defmethod clock-obj  ((self C-patch-application-rtm-editor)) self)
(defmethod stop-clock  ((self C-patch-application-rtm-editor))
  (stop-play self))

(defmethod continue-record ((self C-patch-application-rtm-editor))
  (draw-clock self)
  (let ((clock (- (clock (clock-obj self)) (begin-time self)))
        (notes (patch-value (second (input-objects self)) self))
        (t-time (t-time (car (chord-objects self))))
        delay)
    (declare (ignore clock))
    ;;    (setf (previous-t-time self) t-time)
    (when (atom notes) (setq notes (list notes)))
    (setf (notes (car (chord-objects self))) notes)
    (update-chord (car (chord-objects self)))
    (pop (chord-objects self))
    (when (chord-objects self)
      (setq delay  (- (t-time (car (chord-objects self))) t-time))
      (incf (clock self) delay)
      (dfuncall-process self delay))))

(defmethod  begin-process  ((self C-patch-application-rtm-editor))
  (fill-patch-outrect (out-put self))  
  (setf (clock self) 0)
;;  (setf (previous-t-time self) 0)
  (setf (chord-objects self) 
    (ask-all (collect-all-chord-beat-leafs (give-measure-line self)) 'beat-chord))
;;  (print (ask-all (chord-objects self) 't-time))  
  (calc-t-time-measure-line  (give-measure-line self) 1) 
  (dfuncall-process self (t-time (car (chord-objects self)))))

(defmethod stop-process ((self C-patch-application-rtm-editor))
  (with-focused-view self
    (with-pen-state (:mode :srccopy :pattern *white-pattern*)
      (fill-rect* 50 (- (h self) 12) 33 10)))
  (draw-appl-label self #\A)
  (erase+view-draw-contents (application-object self))
  (fill-patch-outrect (out-put self)))

(defmethod draw-patch-extra :after ((self C-patch-application-rtm-editor))
  (when (play-flag self) (fill-patch-outrect (out-put self))))     

;;====================================================================================================
;;====================================================================================================
(defun make-rtm-polif-arg-list (count)
  (let ((arg-list))
    (for (i 0 1 (1- count)) 
         (push  '*rtm-collector-type* arg-list)
         (push  (string-downcase 
                  (concatenate  'string  "rtmcoll" (format nil "~D" (1+ i)))) arg-list))
    (nreverse arg-list)))

;;(make-rtm-polif-arg-list 6)
        
;;====================================================================================================

(defun make-n-rtm-editors-window (count measure-lines)
 (let* ((win-string (concatenate  'string  "RTM" (format nil "~D" (incf *RTM-window-counter*))))
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

;;(setq r1 (make-n-rtm-editors-window 2))
;;(window-select r1)
;;====================================================================================================

;;changed by aaa 28-08-95 from pw-modif
(defclass  C-patch-PolifRTM (C-patch-application)
  ((measure-line-list :initform nil :initarg :measure-line-list :accessor measure-line-list)
   (popUpbox :initform nil :accessor popUpbox)))

;; (defclass  C-patch-PolifRTM (C-patch-application)
;;   ((measure-line-list :initform nil :initarg :measure-line-list :accessor measure-line-list)))

;;??????
;; (defmethod decompile ((self C-patch-Polifrtm))
;;   `(let ((patch (make-instance ',(class-name (class-of self))
;;                                :view-position ,(view-position self)
;;                                :view-size ,(view-size self)
;;                                :active-mode  ,(active-mode self)
;;                                :pw-function  ',(pw-function self)
;;                                :type-list ',(type-list self) 
;;                                :measure-line-list (list ,@(ask-all (measure-line-list self) 'decompile))
;;                                :application-object 
;;                                ,(when (wptr (application-object self)) (decompile (application-object self)))
;;                                :view-subviews (list ,@(ask-all (pw-controls self) 'decompile)))))
;;      patch))
;; (defmethod initialize-instance :after ((self C-patch-Polifrtm) &key controls)
;;   (declare (ignore controls))
;;                                         ;(make-rtm-lock self)
;;   (let ((ms (ask-all (beat-editors (editor-collection-object (application-object self))) 'measure-line)))
;;     (setf (measure-line-list self) ms)))

;; (defmethod make-application-object ((self C-patch-PolifRTM))
;;   (let (res (length (length (pw-controls self))))
;;     (unless (measure-line-list self)
;;       (for (i 1 1 length) (push (make-instance 'C-measure-line) res))
;;       (setf (measure-line-list self) res))
;;     (make-n-rtm-editors-window length (measure-line-list self))))

(defmethod make-application-object ((self C-patch-PolifRTM))
  (setf (application-object self)
        (let* ((m-length (length (measure-line-list self)))
               (length (if (plusp m-length) m-length (length (pw-controls self))))
               res)
          (unless (measure-line-list self)
            (for (i 1 1 length) (push (make-instance 'C-measure-line) res))
            (setf (measure-line-list self) res))
          (make-n-rtm-editors-window length (measure-line-list self)))))

(defmethod decompile ((self C-patch-Polifrtm))
  (append (call-next-method) 
          (list nil  (if *decompile-chords-mode* 
                            `(list ,@(get-measureline-form self))))))

(defmethod get-measureline-form ((self C-patch-Polifrtm))
  (let ((win (application-object self)))
    (if (wptr win)
        (list `',(get-window-state self win) `(list ,@(ask-all (measure-line-list self) 'decompile)) (value self))
        (list `',(window-state self) `(list ,@(ask-all (measure-line-list self) 'decompile)) (value self) ))))



;; (defmethod complete-box ((self C-patch-Polifrtm) args)
;;   (when args
;;     (setf (measure-line-list self) (second args))
;;     (put-window-state self (application-object self) (first args))
;;     (let ((eds (beat-editors (editor-collection-object (application-object self)))))
;;       (for (i 0 1 (1- (length eds)))
;;         (setf (measure-line (nth i eds)) (nth i (measure-line-list self)))))))

(defmethod complete-box ((self C-patch-Polifrtm) args)
  (when args
    (let* ((win (application-object self))
           (editors (beat-editors (car (subviews win)))))
      (setf (measure-line-list self) (second args))
      (when (/= (length (measure-line-list self))
                (length editors))
        (window-close win)
        (setf (application-object self) 
              (make-application-object self))
        (set-pw-win+pw-obj (application-object self) *active-patch-window* self))   
      (put-window-state self (application-object self) (first args))   
    (setf (value self) (third args))
    (if (third args) (set-dialog-item-text (lock self) "x"))
    (let ((eds (beat-editors (editor-collection-object (application-object self)))))
      (for (i 0 1 (1- (length eds)))
        (setf (measure-line (nth i eds)) (nth i (measure-line-list self))))))))

;;changed by aaa 28-08-95 from pw-modif
(defmethod initialize-instance :after ((self C-patch-Polifrtm) &key controls)
  (declare (ignore controls))
  (make-rtm-lock self)
  (setf (popUpBox self) 
        (make-popUpbox "" self
                       *collector-popUp-menu*
                       :view-position (make-point (- (w self) 10)
                                                  (- (h self) 14))
                       :view-container self
                       :view-font *patchwork-font-spec*)))

;; (defmethod initialize-instance :after ((self C-patch-Polifrtm) &key controls)
;;   (declare (ignore controls))
;;   (make-rtm-lock self))


(defmethod make-rtm-lock ((self C-patch-Polifrtm))
  (-make-lock self (make-point (- (w self) 37) (- (h self) 9))))

(defmethod get-window-state ((self C-patch-Polifrtm) win)
  (let ((rtm-view (editor-collection-object win)))
    (append 
            (get-window-state rtm-view win)
            (ask-all (beat-editors rtm-view) 'get-window-state win)
            (list (view-position win)(view-size win)(window-title win)))))  

(defmethod put-window-state ((self C-patch-Polifrtm) win state)
  (let* ((length-ed (length (beat-editors (editor-collection-object win))))
         (beats-eds (beat-editors (editor-collection-object win)))
         (main-list (nthcdr 5 state))
         (offset-ed (+ 5 length-ed)))
    (set-view-position win (nth offset-ed state))
    (set-view-size win (nth (1+ offset-ed) state))
    (when (stringp (nth (+ 2 offset-ed) state)) (set-window-title win (nth (+ 2 offset-ed) state)))
    (put-window-state (editor-collection-object win) win (subseq  state 0 5)) 
    (for (i 0 1 (1- length-ed))
      (put-window-state (nth i beats-eds) win (nth i main-list)))))

;;_________

(defmethod draw-patch-extra :after ((self C-patch-PolifRTM))
  (draw-char (+ -16 (w self)) (- (h self) 4) #\E)) 
;;_________
;; extend

(defmethod correct-extension-box ((self C-patch-PolifRTM) new-box values)
  (declare (ignore values))
  (let ((m-line (make-instance 'C-measure-line)))
    (setf (measure-line-list new-box) (append (measure-line-list self) (list m-line)))
    ;(unless (wptr (application-object self))
    ;  (setf (application-object self) (make-application-object self)))
    (let ((editors (beat-editors (car (subviews (application-object self)))))
          (new-editors-list (beat-editors (car (subviews (application-object new-box))))))
      (for (i 0 1 (1- (length editors)))
        (setf (measure-line (nth i new-editors-list)) (measure-line (nth i editors))))
      (setf (measure-line (car (last new-editors-list))) m-line)   )))

;; (defmethod generate-extended-inputs ((self C-patch-PolifRTM)) 
;;   (make-rtm-polif-arg-list (1+ (length (pw-controls self)))))

(defmethod generate-extended-inputs ((self C-patch-PolifRTM)) 
  (make-rtm-polif-arg-list (1+ (length (measure-line-list self)))))

(defmethod give-new-extended-title ((self C-patch-PolifRTM)) 'rtmn) 

(defmethod mouse-pressed-no-active-extra :after ((self C-patch-PolifRTM) x y) 
  (declare (ignore x y))
  (when (option-key-p) 
    (remove-yourself-control self)))
;;_________

;; (defmethod patch-value ((self C-patch-PolifRTM) obj)
;;   (if (value self)
;;     (measure-line-list self)
;;     (let ((editors (beat-editors (car (subviews (application-object self)))))
;;            m-line)
;;       (when (not (wptr (application-object self)))
;;         (setf (application-object self) (make-application-object self))
;;         (set-pw-win+pw-obj (application-object self) *active-patch-window* self)
;;         (restore-window-state self (application-object self))
;;         (erase+view-draw-contents self))
;;        ; (put-window-state self (application-object self) (window-state self)))
;;       (for (i 0 1 (1- (length editors)))
;;         (when (nth-connected-p self i)
;;           (setq m-line (patch-value (nth i (input-objects self)) obj))
;;           (setf (nth i (measure-line-list self)) m-line) 
;;           (setf (measure-line (nth i editors)) m-line)
;;           (erase+view-draw-contents (nth i editors))))
;;       (measure-line-list self))))

(defmethod patch-value ((self C-patch-PolifRTM) obj)
  (if (value self)
    (measure-line-list self)
    (let* ((win (application-object self))
           (editors (beat-editors (car (subviews win))))
          res)
      (setf (measure-line-list self)
            (dolist (in-obj (input-objects self) res)
              (setq res (append  res (list! (or (patch-value in-obj obj) 
                                                (make-instance 'C-measure-line)))))))
      (when (/= (length (measure-line-list self))
                   (length editors))
        (if (wptr win) (window-close win))
        (setf (window-state self) nil))
      (when (not (wptr (application-object self)))
        (setf (application-object self) 
              (make-application-object self))
        (set-pw-win+pw-obj (application-object self) *active-patch-window* self)
        (if (window-state self)
          (restore-window-state self (application-object self)))
        (setq editors (beat-editors (car (subviews (application-object self)))))
        (erase+view-draw-contents self))
      (for (i 0 1 (1- (length editors))) 
          (setf (measure-line (nth i editors)) (nth i (measure-line-list self)))
          (erase+view-draw-contents (nth i editors)))
      (measure-line-list self))))


;;====================================================================================================
;;====================================================================================================

(defunp make-beat ((unit float (:min-val 1 :value 1)) (rtm-list list (:value "(1 1 1)"))
                   &optional (ch-objs (list (:type-list (chord) :value 'chob)))) beat
  "Creates a beat object according to unit and rtm-list.
If ch-objs input if not connected a default chord is produced,
else the input has to be a list of chord-objects. "
  (if (symbolp ch-objs) ; no connection
    (beat-constructor unit rtm-list ())
    (beat-constructor unit rtm-list (list! ch-objs))))

(defun make-car+cdr-list (lst)
  (let ((first-elem (pop lst))
         res)
    (while lst
      (if (atom (car lst))
         (push (car lst) res)
         (push (make-car+cdr-list (car lst)) res))
       (pop lst))
    (list first-elem (nreverse res))))

;;(make-car+cdr-list '(2 3 4 7 8 5)) 
;;(make-car+cdr-list '(2 3 4)) 
;;(make-car+cdr-list '(2 3 (3 4 5) 7))
;;(make-car+cdr-list '(2 (3 4 5) (4 (5 6 7)) 7))
  
(defunp make-beat2 ((unit+rtm list (:value "(1 2 1)"))) beat
  "Creates a beat object according to unit+rtm where unit is car of
unit+rtm and rtm-list is cdr of unit+rtm."
   (let ((car+cdr-list (make-car+cdr-list unit+rtm)))
      (beat-constructor (car car+cdr-list) (second car+cdr-list) ())))

(defunp make-measure ((low float (:min-val 1 :value 4)) (beats list (:type-list (beat list) :value "beats"))
                      &optional (metro (float (:value 60)))) measure
"Creates a measure object where low is the denominator and has to be a positive number
\(usually 1,2,4,8,16 or 32,but can be any positive number,even a float).
The numerator is never given but is calculated as the sum of all units
of the beat-objects.
Beats can be a single beat object or a list of beat objects"
 (unless (listp beats)(setq beats (list beats)))
 (make-instance 'C-measure  :low (format () "~A"  low)  :beat-objects beats :metronome metro))


(add-output-type 'measure-line '(measure-line))
(add-output-type 'measure-lines '(list measure-lines))


(defunp rtm-collector ((measure list (:type-list (list measure) :value "measures"))
             (note-obj list (:type-list (list note-obj) :value "notes"))) measure-line
"1st input should be a measure or a list of measures.
Measures can be collected by option-clicking at the out-box of the rtm-collector.
2nd input should be note-objets or a list of note-objects.
Note-objects can only be collected if there are already measures inside the rtm-collector.
To collect note-objects select the rtm-collector and press c."
  (declare (ignore measure note-obj)))

(defunp poly-rtm ((rtmcol list (:type-list (list measure-line) :value '()))
                  &rest (rtmcn list (:type-list (list measure-line) :value '()))) measure-lines ;!! not list
"The inputs of poly-rtm-box should be connected only with rtm-collectors. If the 
out-box of  a poly-rtm-box is double-clicked, then all the measure-line objects 
are taken inside the poly-rtm-box. The module can be locked (to avoid new 
evaluations of the patch that is under 'chord') to keep its contents by clicking on 
the small ‘o’ in the lower right of the module. The ‘v’ indicates that the module is 
open. 
An editor for the <poly-rtm> object is entered either by selecting the module and 
typing the letter 'o',  or by double-clicking on the module's name.
Click 'h' with the <poly-rtm> editor opened for more information."
  (declare (ignore rtmcol rtmcn)))


;;====================================================================================================
(defmethod play ((self C-patch-application-rtm-editor)) (play-from-pw self))
(defmethod play ((self C-patch-PolifRTM)) (play-from-pw self))

;;add by aaa 28-08-95 from pw-modif
(defmethod stop-play ((self c-patch-polifrtm))
  (start 
    (tell (ask-all (beat-editors (car (subviews (application-object self))))
                   'measure-line) 'stop-measure-line))) 

;;changed by aaa 28-08-95 from pw-modif
(defun play-from-pw (self) 
 (let* ((editor (car (subviews (application-object self))))
         (editors (give-selected-editors editor)))
    (setf *mn-view-offset-flag* (check-box-checked-p (third (rtm-radio-ctrls editor))))
    (start 
      (apdfuncall 100 (priority) 15
                  (lambda ()
                      (tell (ask-all editors 'measure-line) 'play-measure-line (get-play-speed editor)))))))

;; (defun play-from-pw (self) 
;;  (let* ((editor (car (subviews (application-object self))))
;;          (editors (give-selected-editors editor)))
;;     (setf *mn-view-offset-flag* (check-box-checked-p (third (rtm-radio-ctrls editor))))
;;     (start 
;;       (apdfuncall 100 (priority) 15
;;                   (lambda ()
;;                       (tell (ask-all editors 'measure-line) 'play-measure-line (get-play-speed editor)))))))

;;====================================================================================================

(defunp get-rchords ((rtmcol list (:type-list (measure-line) :value "rtmco"))) list
 "Returns all chord objects from a rhythm-editor"
  (ask-all (collect-all-chord-beat-leafs rtmcol) #'beat-chord))


;;====================================================================================================


(push-to-object-types 'beat)
(push-to-object-types 'measure)
(push-to-object-types 'measure-line)
(push-to-object-types 'measure-lines)
     
;;;; THE END ;;;;
