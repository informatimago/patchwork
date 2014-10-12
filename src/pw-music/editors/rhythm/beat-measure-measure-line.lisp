;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               beat-measure-measure-line.lisp
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

;;================================================================
(defvar *global-rtm-level-x-list* ())
(defvar *global-rtm-level-list* ())
(defvar *notehead-type-list* ())
(defvar *rtm-editor-measure-x-offset* 15) ;pixels
;;================================================================

;; units 3  .
;; units 5  slur 4 + 1
;; units 6  .
;; units 7  ..
;; slur ....
;; (defun calc-extra-note-head-info (units)
;;   (case (round units) (3 'dot)
;;               (5 'slur)
;;               (6 'dot)
;;               (7 'double-dot)))

;;; modif GAS 10/7/93. add 9, 10, 11, 12 cases
(defun calc-extra-note-head-info (units)
  (case (round units) (3 'dot)
        (5 'slur)
        (6 'dot)
        (7 'double-dot)
        (9 'slur)
        (10 'slur)
        (11 'slur)
        (12 'dot)
        (13 'dot)
        (14 'double-dot)
        (15 'dot)
        ;(17 'slur)
        ;(18 'slur)
        (19 'slur)
        (20 'slur)
        (21 'slur)
        (22 'slur)
        (23 'dot)
        (24 'dot)
        (25 'dot)
        (26 'dot)
        (27 'dot)
        (28 'double-dot)
        (48 'dot)))


(defun calc-next-note-level (unit-count rtm-sum)
  (let ((level-counter 0)
        (unit-count-now unit-count))
    (if (> unit-count rtm-sum)
      (while (> unit-count-now rtm-sum)
        (setq unit-count-now (/ unit-count-now 2))
        (when (>= unit-count-now rtm-sum) (decf level-counter)))
      (while (< unit-count-now rtm-sum)
        (setq unit-count-now (* 2 unit-count-now))
        (when (<= unit-count-now rtm-sum) (incf level-counter))))
    level-counter))

;;#\w  whole note
;;#\h  1/2 note
;;#\q  1/4 note
;;#\e  1/8 note
;;#\x  1/16
;;#\≈  1/32
;;#\…  1/64 (appogiatura) ??

;;#\W  whole 1/2  rest
;;#\Œ 1/4 rest
;;#\‰ 1/8 rest
;;#\r 1/16 rest
;;#\R 1/32 rest
;; 64 rest ???


(defun calc-next-note-head (unit-count rtm-sum rtm base-unit)
;; (print (list unit-count rtm-sum rtm base-unit))
 (let ((notehead-type (if (minusp rtm) 'rest 'notehead))
        note-head-info level)
   (setq rtm (abs rtm))
   (setq note-head-info (calc-extra-note-head-info (if (and (> unit-count 2) (= rtm rtm-sum)) unit-count rtm))) ;;for groupings
   (setq level (+ base-unit
                  (- (calc-next-note-level unit-count rtm-sum)
                     (truncate (log rtm 2)))))  ; (sqrt 2 N) 
   (push level *global-rtm-level-list*) 
   (push notehead-type *notehead-type-list*) 
   (list (case level
           (4 (if (eql notehead-type 'rest) #\R #\q))   ;1/64
           (3 (if (eql notehead-type 'rest) #\R #\q))   ;1/32
           (2 (if (eql notehead-type 'rest) #\r #\q))   ;1/16
           (1 (if (eql notehead-type 'rest) #\‰ #\q))   ;1/8
           (0  (if (eql notehead-type 'rest) #\Œ #\q))  ;1/4
           (-1  (if (eql notehead-type 'rest) #\W #\h)) ;1/2
           (t (if (eql notehead-type 'rest) #\W #\w)))  ;1/1
           note-head-info))) 
;; (case level
;;            (4 (if (eql notehead-type 'rest) #\R #\…))   ;1/64
;;            (3 (if (eql notehead-type 'rest) #\R #\≈))   ;1/32
;;            (2 (if (eql notehead-type 'rest) #\r #\x))   ;1/16
;;            (1 (if (eql notehead-type 'rest) #\‰ #\e))   ;1/8
;;            (0  (if (eql notehead-type 'rest) #\Œ #\q))  ;1/4
;;            (-1  (if (eql notehead-type 'rest) #\W #\h)) ;1/2
;;            (t (if (eql notehead-type 'rest) #\W #\w)))  ;1/1

;;rtm-sum > unit-count
;;(calc-next-note-head 2 5 1 0)
;;(calc-next-note-head 3 11 1 0)
;;(calc-next-note-head 3 4 1 1)
;;(calc-next-note-head 3 5 1 1)
;;(calc-next-note-head 3 5 2 1)
;;(calc-next-note-head 3 5 4 1)
;;(calc-next-note-head 3 6 1 1)

;;unit-count > rtm-sum  
;;(calc-next-note-head 3 2 1 1)
;;(calc-next-note-head 4 2 1 1)
;;(calc-next-note-head 4 3 1 1)
;;(calc-next-note-head 7 3 1 1)

;;unit-count = rtm-sum  
;;(calc-next-note-head 2 2 1 1)

;; (defun give-note-head-minus-2-levels (note-head)
;; ;;  (print note-head)
;;   (case note-head
;;      (#\w #\q)
;;      (#\h #\e)
;;      (#\q #\x)
;;      (#\e #\≈)
;;      (#\x #\…)
;; ;; rests
;;      (#\W #\‰)
;;      (#\Œ #\r)
;;      (#\‰ #\R)
;; ))

(defun give-note-head-minus-2-levels (note-head)
;;  (print note-head)
  (case note-head
     (#\w #\q)
     (#\h #\e)
     (#\q #\x)
     (#\e #\≈)
     (#\x #\…)
;; rests
     (#\W #\‰)
     (#\Œ #\r)
     (#\‰ #\R)
     (#\r #\ç)       ;;;;clumsy 64th rest
     (otherwise (error "cannot draw smaller than a 32th rest or a 64th note"))
))

;;================================================================
(defun make-rtm-chords (count midics)
  (let (chords)
    (repeat count (push (make-chord-object (list (pop midics)) 0) chords))
    (nreverse chords)))
;;(make-rtm-chords 6 '(6000 6100 6200 6300 6400 6500))

(defun beat-constructor (unit-length rtm-list &optional chords)
 (let (res rtm-list-now beat chord-beats)
   (while rtm-list
     (setq rtm-list-now (pop rtm-list))
     (push 
        (if (listp rtm-list-now)
          (beat-constructor (car rtm-list-now)(second rtm-list-now) chords)
         `(make-instance 'C-beat :unit-length ,rtm-list-now)) 
          res))
    (setq beat (eval `(make-instance 'C-beat :unit-length ,unit-length :rtm-list (list ,@(nreverse res)))))
    (setq *beat-leaf-objs* ())
    (setq chord-beats (collect-all-chord-beat-leafs2 beat))
    (while chord-beats
      (setf (beat-chord (pop chord-beats))
         (if chords (pop chords) (make-chord-object '(6000) 0))))
    (values beat chords)))


;;================================================================
;;================================================================
;;   unit-length
;; if in a leaf - fix -> note-head , negative fix -> rest , float -> slur 

(defclass C-beat ()
  ((rtm-list :initform nil :initarg :rtm-list :accessor rtm-list)
   (unit-length :initform 1 :initarg :unit-length :accessor unit-length)
   (beat-chord :initform nil :initarg :beat-chord :accessor beat-chord)
   (super-beat :initform nil :accessor super-beat)
   (beat-hash-table :initform nil :initarg :beat-hash-table :accessor beat-hash-table))) ;;;

(defgeneric decompile-rtm (self rtm-list))
(defmethod decompile-rtm ((self C-beat) rtm-list)  
  (declare (ignore rtm-list))
  (let ((rtm-list (rtm-list self)) rtm-obj res)
    ;;    (print rtm-list)
    (while rtm-list
      (setq rtm-obj (pop rtm-list))
      (if (no-beat-leaf? rtm-obj)
          (push (list (unit-length rtm-obj) (decompile-rtm rtm-obj rtm-list)) res)
          (progn (push (unit-length rtm-obj) res)
                 (when (beat-chord rtm-obj)(push (decompile (beat-chord rtm-obj)) *rec-rtm-chs-list*)))))
    (nreverse res))) 

(defmethod decompile  ((self C-beat))  
  (setq *rec-rtm-chs-list* ())
 `(beat-constructor ,(unit-length self) ',(decompile-rtm self ()) (list ,@(nreverse *rec-rtm-chs-list*)))) 

(defgeneric decompile-to-dialog (self))
(defmethod decompile-to-dialog  ((self C-beat)) ;;; 
  (setq *rec-rtm-chs-list* ())
  `(beat-constructor ,(unit-length self) ',(decompile-rtm self ()) (list ,@(nreverse *rec-rtm-chs-list*)))) 

;;(setq b (beat-constructor 1 '(1 1 -1 (1 (1 1))) (make-rtm-chords 4 '(6000 6100 6200 6300))))
;;(decompile-rtm b ())
;;(decompile (eval (decompile b)))

(defgeneric no-beat-leaf? (self)
  (:method ((self C-beat)) (rtm-list self)))
(defgeneric beat-leaf? (self)
  (:method ((self C-beat)) (null (rtm-list self))))

;;=======================================
(defgeneric add-beat-after-myself (self beat)
  (:method ((self C-beat) beat) (declare (ignore beat))))  
(defgeneric add-beat-before-myself (self beat)
  (:method ((self C-beat) beat) (declare (ignore beat))))  
(defgeneric remove-myself-from-measure (self beat)
  (:method ((self C-beat) beat) (declare (ignore beat))))  
;;(defmethod paste-beat-myself ((self C-beat) beat) (declare (ignore beat)) (print (rtm-list self)))  

(defgeneric paste-beat-myself (self beat))
(defmethod paste-beat-myself ((self C-beat) beat)  
  (when *rtm-struct-selection-scrap*
    (let ((nth-beat (nth? beat (rtm-list self))))
      (setf (rtm-list self)
            (append (firstn nth-beat (rtm-list self))
                    (list (eval *rtm-struct-selection-scrap*))
                    (nthcdr (1+ nth-beat) (rtm-list self))))))) 

(defgeneric add-beat-after (self)
  (:method ((self C-beat))  
    (when (super-beat self) (add-beat-after-myself (super-beat self) self))))

(defgeneric add-beat-before (self)
  (:method ((self C-beat))  
    (when (super-beat self) (add-beat-before-myself (super-beat self) self))))

(defgeneric remove-beat-from-measure (self)
  (:method ((self C-beat))  
    (when (super-beat self) 
      (kill-chords self)    
      (remove-myself-from-measure (super-beat self) self))))

(defgeneric paste-beat (self))
(defmethod paste-beat ((self C-beat))  
  (if (check-box-checked-p (chord-edit-ctrl (editor-collection-object *active-RTM-window*)))
      (when (beat-chord self)
        (when *beat-chord-scrap* (setf (beat-chord self) 
                                       (eval *beat-chord-scrap*))))
      (when (super-beat self) (paste-beat-myself (super-beat self) self))))
;;=======================================

(defgeneric replace-old-beat (self old-beat new-beat))
(defmethod replace-old-beat ((self C-beat) old-beat new-beat) 
  (setf (rtm-list self) (substitute new-beat old-beat (rtm-list self) :test 'eq)))

;;(setf a (read-from-string (delete  "'" "(2 '(2 3 4 5))" :test #'string=)))
;;`(,(car a) ',@(cdr a)) 

(defun substitute-nil-to-paren (str)
  (setf (elt  (setf str (substitute  #\Space #\n  (substitute #\Space #\i  (substitute #\Space #\l  str)))) 4)  #\( ) 
  (setf (elt str 5)  #\))
  str)

(defgeneric open-beat-editor-win (self ctrl))
(defmethod open-beat-editor-win ((self C-beat) ctrl)  
  (setf *current-rtm-editor* (rtm-view-obj ctrl))
  (when (double-click-p)
    (let ((edit-data (decompile-to-dialog self)) str) ;;;
      (setq str  
            (delete  "'" (PRIN1-TO-STRING (list (abs (second edit-data))(third edit-data))) :test #'string=)) ;to avoid "'"
      (when (eql (type-of (eval (third edit-data))) 'null)
        (setq str (substitute-nil-to-paren str))) ;to avoid "nil" ??? 
      (set-value-from-global-tty self 
                                 (get-string-from-user  "rtm" :size (make-point 300 85) :position :centered
                                                              :initial-string str))))) 

;; (window-select-ccl-tty-dialog  (PRIN1-TO-STRING (list (second edit-data)(third edit-data))) 50 50 t))))

(defmethod draw-appl-label ((self C-beat) label)(declare (ignore label)))
(defmethod update-editor ((self C-beat)))

(defclass C-chord-boxMN-window-rtm (C-chord-boxMN-window) ())

(defvar *the-chord-rtm-editor* ())


(defun make-MN-editor-chordMN-rtm (staff)
  (make-music-notation-editor 'C-chord-boxMN-window-rtm
                              'C-chord-mus-not-view
                              'C-MN-panel-ChordBox
                              (make-point 230 200) staff))



(defmethod save-window-state ((self C-beat) win)
  (declare (ignore win self)))

(defmethod view-deactivate-event-handler ((self C-chord-boxMN-window-rtm))
  (let ((chord (car (chords (chord-line (car (editor-objects (car (subviews self))))))))
        (beat-chord (beat-chord (pw-object self))))
    (unless (eql chord beat-chord)
      (setf (beat-chord (pw-object self)) chord))
    (call-next-method)))

(defgeneric open-beat-chord-editor (self ctrl))
(defmethod open-beat-chord-editor ((self C-beat) ctrl)
  (declare (special *the-chord-rtm-editor*))
  (if (not (check-box-checked-p (chord-edit-ctrl (editor-collection-object *active-RTM-window*))))
      (open-beat-editor-win self ctrl)
      (when (double-click-p) 
        (unless (beat-chord self)
          (let ((new-chord (make-chord-object '(6000) 0)))
            (setf (beat-chord self) new-chord)))
        (if (window-killed-p *the-chord-rtm-editor*)
            (setf *the-chord-rtm-editor*
                  (make-MN-editor-chordMN-rtm *g2-g-f-f2-staffs*)))
        (open-chord-ed *the-chord-rtm-editor*
                       *active-rtm-window* self (beat-chord self) (+ -35 *selection-buttons-x*) (+ -22 *selection-buttons-y*) ))))

(defun check-rtm-input-string (str)
  (let ((open-paren-count (count "("  str :test 'string=))
        (close-paren-count (count ")"  str :test 'string=))
        temp (test-fl t))
    (cond ((or (not (= open-paren-count open-paren-count close-paren-count))
               (< open-paren-count 2)
               (< open-paren-count 2))
             (setq test-fl ()))
          (t (setq temp (coerce str 'list))
             (while temp
               (unless (member (pop temp) 
                        '(#\( #\. #\) #\Space #\- #\+ #\'   
                          #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
                 (setq test-fl ())))))
    (if test-fl (read-from-string str)  nil)))

(defgeneric set-value-from-global-tty (self beat-string))
(defmethod set-value-from-global-tty ((self C-beat) beat-string) 
  (let ((rtm-info (check-rtm-input-string beat-string)))
    (if (not rtm-info)
        (ui:ed-beep)
        (progn 
          (setq rtm-info `(,(abs (car rtm-info)) ',@(cdr rtm-info))) ;to avoid "'" 
          (replace-old-beat (super-beat self) self (eval `(beat-constructor ,(car rtm-info) ,(second rtm-info))))
          (setf (rtm-selection-1 (editor-collection-object *active-rtm-window*)) ())  
          (setf (rtm-selection-2 (editor-collection-object *active-rtm-window*)) ())  
          (when *current-rtm-editor* 
            (erase+view-draw-contents *current-rtm-editor*))))))
    
;;==============================

(defgeneric play-beat-continue (self beats beat-unit-ticks))
(defmethod play-beat-continue ((self C-beat) beats beat-unit-ticks)
  (when beats 
    (let ((delay (play-beat (pop beats) beat-unit-ticks)))
      (dfuncall (truncate (abs delay)) 'play-beat-continue self beats beat-unit-ticks))))
 

(defgeneric play-beat (self beat-unit-ticks))
(defmethod play-beat ((self C-beat) beat-unit-ticks)
  (setq beat-unit-ticks (* (unit-length self) beat-unit-ticks))
  ;;  (print (list 'beat-unit-ticks beat-unit-ticks))
  (if (beat-leaf? self) 
      (progn  (when (beat-chord self) 
                (play-chord-with-offset (beat-chord self)))
              beat-unit-ticks)
      (let* ((rtm-list (rtm-list self))
             (rtm-sum (apply #'+ (mapcar #'abs (ask-all (rtm-list self) 'unit-length))))
             (ticks-incr (/ beat-unit-ticks rtm-sum)))
        (play-beat-continue self rtm-list ticks-incr)
        beat-unit-ticks)))

(defgeneric collect-all-chord-beat-leafs (self))
(defmethod collect-all-chord-beat-leafs ((self C-beat))
  (if (and (beat-chord self)  (beat-leaf? self))
      (setf *beat-leaf-objs* (append *beat-leaf-objs* (list self)))
      (tell  (rtm-list self) 'collect-all-chord-beat-leafs))
  *beat-leaf-objs*)   

(defgeneric collect-all-chord-beat-leafs2 (self))
(defmethod collect-all-chord-beat-leafs2 ((self C-beat))
  ;;  (print (list 'test (plusp (unit-length self)) (ui:fixnump (unit-length self)) (beat-leaf? self)(rtm-list self)))
  (if (and (plusp (unit-length self)) (ui:fixnump (unit-length self)) (beat-leaf? self))
      (setf *beat-leaf-objs* (append *beat-leaf-objs* (list self)))
      (tell  (rtm-list self) 'collect-all-chord-beat-leafs2))
  *beat-leaf-objs*)   
;;==============================

(defgeneric draw-beat-slur/tuplet (self x1 x2 y1 y2))
(defmethod draw-beat-slur/tuplet  ((self C-beat) x1 x2 y1 y2)
  (draw-line (+ 5 x1) y1 (- x2 3) y1)
  (draw-line (+ 2 x1)  (+ y1 y2) (+ 5 x1) y1)
  (draw-line x2 (+ y1 y2) (- x2 3) y1))

(defgeneric draw-beat-extra-stuff (self x C5 t-scfactor))
(defmethod draw-beat-extra-stuff  ((self C-beat) x C5 t-scfactor)
  (declare (ignore x C5 t-scfactor)))

(defgeneric draw-beat- (self obj C5 super-beat pixel-beat-unit beg-x y2 typlet-y low))
(defmethod draw-beat- ((self C-beat) obj C5 super-beat pixel-beat-unit beg-x y2 typlet-y low)
  (let* ((rtm-list (rtm-list self))
         (rtm-sum (round (apply #'+ (mapcar #'abs (ask-all (rtm-list self) 'unit-length)))))
         (real-pixel-beat-unit (* (unit-length self) pixel-beat-unit))
         (pixel-incr (/ real-pixel-beat-unit rtm-sum))
         (pixel-now beg-x)
         (end-pixel (+ real-pixel-beat-unit beg-x))
         (tuplet-flag (draw-tuplets? rtm-sum (unit-length self))) 
         rtm-now rtm-obj note-head-info)
    (when (eql (class-name  (class-of  super-beat)) 'C-measure)
      (setf *global-rtm-level-x-list* nil)
      (setf *notehead-type-list* nil)
      (setf *global-rtm-level-list* nil))
    (while rtm-list
      (setq rtm-obj (pop rtm-list))
      (setq rtm-now (unit-length rtm-obj))
      (if (no-beat-leaf? rtm-obj)
          (setq pixel-now 
                (draw-beat- rtm-obj obj C5 self pixel-incr pixel-now y2 
                            (if (edit-mode obj) (+ 10 typlet-y) (+ (if tuplet-flag 10 0) typlet-y)) 
                            (+ (calc-next-note-level (unit-length self) rtm-sum) low))) 
          (progn
            (push pixel-now  *global-rtm-level-x-list*)
            (setq note-head-info 
                  (calc-next-note-head  (unit-length self) rtm-sum rtm-now low))
            (draw-char (- pixel-now (if (minusp rtm-now) 2 5)) 
                       (+  (if (or (floatp rtm-now) (beat-chord rtm-obj)) 10 17) y2) (car note-head-info))
            (when (and (not (= *staff-num* 7)) (beat-chord rtm-obj))  
              (draw-beat-extra-stuff rtm-obj pixel-now C5 (beat-zoom obj))
              (draw-beat-chord-5 (beat-chord rtm-obj) pixel-now C5 (beat-zoom obj) (+ 10 y2) (car note-head-info)))
            (when (and (edit-mode obj)(ui:fixnump rtm-now))
              (connect-to-selection-button obj rtm-obj  (- pixel-now 7) y2 10 15 'open-beat-chord-editor)  
              (setf (super-beat rtm-obj) self))
            (draw-extra-note-info self note-head-info rtm-now pixel-now y2 pixel-incr)  
            (when rtm-list (setq pixel-now (round (+ pixel-now (* (abs rtm-now) pixel-incr))))))))
    (when (eql (class-name  (class-of  super-beat)) 'C-measure)
      (draw-beam-info- self (- y2 6) (nreverse *global-rtm-level-x-list*)(nreverse *global-rtm-level-list*)
                       (nreverse *notehead-type-list*)))
    (when (edit-mode obj)
      (connect-to-selection-button obj self beg-x  (- typlet-y 5)  (- (+ 4 *rtm-last-note-pixel*) beg-x) 7 
                                   'open-beat-editor-win)
      (setf (super-beat self) super-beat))
    (when tuplet-flag
      (draw-tuplets  self (view-container (view-container  obj)) (unit-length self) beg-x typlet-y rtm-sum))
    (round end-pixel)))
      
;;================================================================
;; draw beams 
;;================================================================

(defun make-1+2-groupings-list (lst)
  (let ((count 1) 1-res (ref (pop lst))) ;2-res)
    (while lst
      (if (= (car lst) ref)
         (incf count)
         (progn 
           (push (if (<= ref 0) (- count) count) 1-res)
           (setq count 1)
           (setq ref (car lst))))
       (pop lst))
    (push (if (<= ref 0) (- count) count) 1-res)
    (nreverse 1-res)))

;;(make-1+2-groupings-list '(0 0 0 1 1 0 0 1 1)) ; 3 -2 1 ; negative no draw group
;;(make-1+2-groupings-list(substitute 1 2 '(2 1 0 1 1 0 0 1 1))) -> also 2s
;;(make-1+2-groupings-list(substitute 0 1 '(2 1 0 1 1 0 0 1 1))) -> only 2s
;;(time (make-1+2-groupings-list (substitute-list* '(1 1 1) '(4 3 2) '(1 2 1 2 2))))

(defun substitute-list* (new-lst old-lst lst)
  (while new-lst
     (setq lst (substitute (pop new-lst) (pop old-lst) lst)))
  lst)

(defgeneric draw-beam-info- (self y x-lst level-lst note-types))
(defmethod draw-beam-info-  ((self C-beat) y x-lst level-lst note-types)
  (unless (and (eql (car note-types) 'rest)(= (length note-types) 1))
    (let ((1s-list (make-1+2-groupings-list (substitute-list* '(1 1 1) '(4 3 2) level-lst))) ;->  1s,2s,3s.4s
          (2s-list (make-1+2-groupings-list (substitute-list* '(2 2 0) '(4 3 1) level-lst))) ;->  2s,3s,4s
          (3s-list (make-1+2-groupings-list (substitute-list* '(3 0 0) '(4 2 1) level-lst))) ;->  3s,4s
          (4s-list (make-1+2-groupings-list (substitute-list* '(0 0 0) '(3 2 1) level-lst))) ;->  only 4s ; max 1/64 notes
          (y-start y)
          first-x main-list list-now  nth-first-x1 nth-first-x2 first-time? single-case)
      ;;  (print (list "x" x-lst "level" level-lst))
      ;;  (print (list "1s" 1s-list "2s" 2s-list "3s" 3s-list "4s" 4s-list))
      ;;  (print note-types)
      (setq main-list (list 1s-list 2s-list 3s-list 4s-list))
      (setq single-case (= 4 (apply #'+ (mapcar #'length main-list))))
      (with-pen-state  (:size (make-point 1 2)) 
        (while main-list
          (setq list-now (pop main-list))
          (setq first-x 0)
          (setq first-time? t)
          (for (i 0 1 (1- (length list-now)))
            (when (> (nth i list-now) 0)
              (setq nth-first-x1 (nth first-x x-lst))
              (setq nth-first-x2 (1- (+ first-x (nth i list-now))))
              (if (= first-x nth-first-x2)
                  (if (and first-time? (or single-case (< i (1- (length list-now)))))
                      (draw-line  nth-first-x1 y (+ 3 nth-first-x1) y)
                      (draw-line  (- nth-first-x1 3) y nth-first-x1  y))
                  (draw-line  nth-first-x1 y (nth nth-first-x2 x-lst)  y))
              (setq first-time? nil))
            (incf first-x (abs (nth i list-now))))
          (incf y 3)))
      (while (and x-lst note-types)
        (when (eql (car note-types) 'rest)
          (draw-line (car x-lst) y-start  (car x-lst) (+ 11 y-start)))
        (pop x-lst)(pop note-types)))))



;;================================================================
;; draw extra note info  
;;================================================================

(defgeneric draw-extra-note-info (self note-head-info rtm-now pixel-now y2 pixel-incr))
(defmethod draw-extra-note-info  ((self C-beat) note-head-info rtm-now pixel-now y2 pixel-incr)
  (let (slur-x (slur-y (- y2 6)) (dot-y (if (minusp rtm-now) (+ 19 y2) (+ 12 y2)))) ; rest?
    (case (second note-head-info)
      (dot (draw-char (+ 4 pixel-now) dot-y #\.))
      (double-dot (draw-char (+ 4 pixel-now) dot-y #\.)
       (draw-char (+ 7 pixel-now) dot-y #\.))
      (slur 
       (setq slur-x (round (+ pixel-now (* (1- (abs rtm-now)) pixel-incr))))
       (if (plusp rtm-now)              ; note or rest? 
           (progn
             (draw-char (- slur-x 5) (+ 10 y2) #\q)
             (with-pen-state  (:size (make-point 1 2)) 
               (for (i 0 1 (1- (+ (car *global-rtm-level-list*) 2)))
                 (draw-line (- slur-x 3)  slur-y  slur-x slur-y)
                 (incf slur-y 3)))
             (draw-beat-slur/tuplet self (- pixel-now 5) (- slur-x 5) (+ 15 y2) -2))
           (draw-char 
            (- slur-x 5) (+ 17 y2) 
            (give-note-head-minus-2-levels (car note-head-info))))))
    (when (floatp rtm-now)  
      (if (> *rtm-last-note-pixel* pixel-now)
          (draw-beat-slur/tuplet self (- pixel-now 10 5) (- pixel-now 5) (+ 15 y2) -2)
          (draw-beat-slur/tuplet self (- *rtm-last-note-pixel* 5) (- pixel-now 5) (+ 15 y2) -2)))
    (setq *rtm-last-note-pixel* (if slur-x slur-x  pixel-now))))

;;================================================================
;; tuplets  
;;================================================================

(defgeneric draw-tuplets (self win unit-length beg-x typlet-y rtm-sum))
(defmethod draw-tuplets ((self C-beat) win unit-length beg-x typlet-y rtm-sum)
  (let ((tuplet-num  (give-tuplet-num2 rtm-sum unit-length)))
    (unless (= (* tuplet-num 2)  rtm-sum)
      (set-view-font  win '("Courier" 9 :srcor)) ; *patchwork-font-spec*
      (let ((middle-pixel (round (+ (/ (- *rtm-last-note-pixel* beg-x) 2) beg-x))))
        (when (> (unit-length self) 1) (setq middle-pixel (- middle-pixel 5)))
        (draw-beat-slur/tuplet self beg-x (+ 4 *rtm-last-note-pixel*) (- typlet-y 5) 3)
        (draw-string (- middle-pixel 5) (- typlet-y 6) (format () "~2D" rtm-sum))
        (when (> unit-length 1)
          (draw-char (+ 5 middle-pixel) (- typlet-y 6) #\:)
          (draw-string (+ 10 middle-pixel) (- typlet-y 6) (format () "~D" tuplet-num))))
      (set-view-font  win *music-font-spec*))))

;; 9 2  -> 9:8
;; 9 1  -> 9
;; 6 7  -> 6:3.5
(defun give-tuplet-num2 (rtm-sum unit)
;;  (format () "~D" 
    (let ((num (cond ((>  unit rtm-sum) 
                         (while (> unit rtm-sum) (setq unit (/ unit 2))) unit)
                     ((<  unit rtm-sum) 
                         (while (< (* unit 2) rtm-sum) (setq unit (* unit 2))) unit)
                      (t unit))))
      (if (fixnump num) num (float num))))

(defun draw-tuplets? (rtm-sum unit)
;;  (print (list rtm-sum unit))
  (and
    (not (and (= rtm-sum 4)(= unit 2)))    
    (not (and (= rtm-sum 8)(= unit 4)))    
    (not (and (= rtm-sum 8)(= unit 2)))    
    (not (and (= rtm-sum 6)(= unit 3)))    
    (not (= rtm-sum 1))    
    (not (= rtm-sum unit))   ; 2:2
    (or (and (> rtm-sum 1)(oddp rtm-sum))
        (= rtm-sum 6)(= rtm-sum 10)(= rtm-sum 12)(= rtm-sum 14) 
        (> unit 1))))
;;(draw-tuplets? 8 4) 

;;================================================================
;;================================================================

(defclass C-measure ()
  ((high :initform "3" :initarg :high :accessor high)  
   (low  :initform "4" :initarg :low  :accessor low)  
   (super-measure-line  :initform ()  :accessor super-measure-line)  
   (beat-objects :initform nil :initarg :beat-objects :accessor beat-objects)  
   (rtm-list :initform nil :initarg :rtm-list :accessor rtm-list) ; for compound meters
   (metronome-unit :initform 4 :initarg :metronome-unit :accessor metronome-unit)
   (metronome :initform 60 :initarg :metronome :accessor metronome)
   (extra-measure-stuff :initform nil :accessor extra-measure-stuff)))

(defmethod decompile ((self C-measure))
  `(make-instance 'C-measure :low ,(low self) ;:rtm-list ,(rtm-list self) 
                             :metronome ,(metronome self)
                             :metronome-unit ,(metronome-unit self)
                             :beat-objects (list ,@(ask-all (beat-objects self) 'decompile))))

(defgeneric calc-high-value (self)
  (:method ((self C-measure))
    (apply #'+ (mapcar #'abs (ask-all (beat-objects self) 'unit-length)))))


;; metronome units 
;;  1 =1 2/3 =1/2.  2=1/2   4/3 =1/4.  4 =1/4   8/3   =1/8.  8   =1/8  16/3   =1/16. 16  =1/16

(defgeneric calc-measure-length (self t-scfactor &optional midi-flag)
  (:method ((self C-measure) t-scfactor &optional midi-flag)
    (let ((low (read-from-string (low self)))
          (high (calc-high-value self)))
      (round (* 
              (/ (metronome-unit self) low)
              (* high t-scfactor (/ (*  60 (if midi-flag 96 100)) (metronome self))))))))

(defgeneric set-metronome (self num))
(defmethod set-metronome ((self C-measure) num)(setf (metronome self) num))
(defgeneric set-unit+metronome (self unit metr))
(defmethod set-unit+metronome ((self C-measure) unit metr)
  (setf (metronome-unit self) unit) (setf (metronome self) metr))
;;=======================================

(defgeneric play-measure-continue (self beats beat-unit-ticks))
(defmethod play-measure-continue ((self C-measure) beats beat-unit-ticks)
  (when beats 
    (let ((delay (play-beat (pop beats) beat-unit-ticks)))
      (dfuncall (truncate (abs delay)) 'play-measure-continue self beats beat-unit-ticks))))
 
(defgeneric play-measure (self t-scfactor))
(defmethod play-measure ((self C-measure) t-scfactor)
  (let* ((measure-length (calc-measure-length self t-scfactor))
         (beats (beat-objects self))
         (beat-unit-ticks (/ measure-length (calc-high-value self))))
    ;;    (print (list 'measure-length measure-length 'beat-unit-ticks beat-unit-ticks))
    (play-measure-continue self beats beat-unit-ticks)
    measure-length))
;;=======================================
(defmethod collect-all-chord-beat-leafs ((self C-measure))
  (tell  (beat-objects self) 'collect-all-chord-beat-leafs)
  *beat-leaf-objs*)

;;=======================================

(defgeneric calc-measure-pixel-x (self t-scfactor beg-x)
  (:method ((self C-measure) t-scfactor beg-x)
    (+ beg-x (calc-measure-length self t-scfactor))))
      
;;1 -> -2  2 -> -1  4 -> 0  8 -> 1  16 -> 2  32 -> 3  64 -> 4
(defun give-low-number (low) (- (truncate (log low 2)) 2))

(defgeneric draw-all-beats (self obj C5 pixel-beat-unit beg-x y1 y2))
(defmethod draw-all-beats ((self C-measure) obj C5 pixel-beat-unit beg-x y1 y2)
  (let ((new-beat-x beg-x)
        (beats (beat-objects self))
        (low-now (give-low-number (read-from-string (low self)))))
    (while beats
      (setq new-beat-x 
            (draw-beat- (pop beats) obj C5 self pixel-beat-unit new-beat-x y2 (- y1 8) low-now)))))

(defgeneric draw-measure (self obj C5 t-scfactor beg-x y1 y2 &optional previous-info))
(defmethod draw-measure ((self C-measure) obj C5 t-scfactor beg-x y1 y2 &optional previous-info)
  (setf (high self) (format () "~D" (calc-high-value self)))
  (let ((new-beg-x (calc-measure-pixel-x self t-scfactor beg-x))(high)(x-incr))
    (unless (and (string=  (first previous-info) (low self))(string=  (second previous-info) (high self)))
      (set-view-font  (view-container (view-container  obj)) '("Courier" 12 :srcor))
      (draw-string (- beg-x (if (= (length (high self)) 1) 1 6)) (+ -25 y1) (high self)) 
      (draw-string (- beg-x (if (= (length (low self)) 1) 1 6)) (+ -16 y1) (low self))) 
    (unless (and (third previous-info) (=  (third previous-info) (metronome  self))
                 (=  (fourth previous-info) (metronome-unit  self)))
      (set-view-font  (view-container (view-container  obj)) *small-music-font-spec*) 
      (draw-char (+ beg-x 26) (+ -20 y1) 
                 (case (metronome-unit self)
                   (2/3 #\w)(1 #\w)
                   (4/3 #\h)(2 #\h)
                   (8/3 #\q)(4 #\q)
                   (16/3 #\e)(8 #\e)
                   (32/3 #\x)(16 #\x)
                   (64/3 #\≈)(32 #\≈))) 
      (when (or  (= 2/3 (metronome-unit self))(= 4/3 (metronome-unit self))(= 8/3 (metronome-unit self))
                 (= 16/3 (metronome-unit self))(= 32/3 (metronome-unit self))(= 64/3 (metronome-unit self)))
        (draw-char (+ beg-x 34) (+ -18 y1) #\.)) 
      (set-view-font  (view-container (view-container  obj)) '("Courier" 10 :srcor)) ;;("times"  12  :srcor)
      (draw-string (+ beg-x 38) (+ -22 y1) "=") 
      (draw-string (+ beg-x 45) (+ -22 y1) (format () "~D"(metronome self)))) ;;(+ beg-x 50) 
    (set-view-font  (view-container (view-container obj)) *music-font-spec*)
    (if (= *staff-num* 7)
        (draw-line (+ *rtm-editor-measure-x-offset* new-beg-x) (- y1 10) (+ *rtm-editor-measure-x-offset* new-beg-x) (+ y1 25)) 
        (draw-rtm-measure-line-ys (+ *rtm-editor-measure-x-offset* new-beg-x) C5))
    (setq high (read-from-string (high self)))
    (setq x-incr (round (/ (- new-beg-x beg-x) high))) 
    (draw-all-beats self obj C5 x-incr (+ *rtm-editor-measure-x-offset* beg-x) y1 y2) 
    (when (edit-mode obj) 
      (connect-to-selection-button obj self (- beg-x  9) (- y1 36) 17 22 'open-measure-editor-win)
      (setf (super-measure-line self) (measure-line obj)))
    (+ *rtm-editor-measure-x-offset* new-beg-x)))

(defun draw-rtm-measure-line-ys (x C5)
  (let ((ys (ask-all (symbol-value (nth (1- *staff-num*) *global-staff-list*)) #'delta-y)))
     (draw-line x (- C5 (first ys) 16) x (- C5 (car (last ys)) 1))))
;;=================================

(defmethod replace-old-beat ((self C-measure) old-beat new-beat) 
  (setf (beat-objects self) (substitute new-beat old-beat (beat-objects self) :test 'eq)))

(defgeneric open-measure-editor-win (self ctrl))
(defmethod open-measure-editor-win ((self C-measure) ctrl)  
  (setf *current-rtm-editor* (rtm-view-obj ctrl))
  (when (double-click-p) 
    (if (shift-key-p)
        (progn (setq *measure-edit-mode* 'metr)
               (set-value-from-global-tty self (get-string-from-user  "metronome" :size (make-point 150 85) :position :centered
                                                                                  :initial-string 
                                                                                  (PRIN1-TO-STRING (list (metronome-unit self)(metronome self))))))
        (progn (setq *measure-edit-mode* 'low)
               (set-value-from-global-tty self (get-string-from-user  "measure low" :size (make-point 150 85) :position :centered
                                                                                    :initial-string (low self)))))))

(defmethod set-value-from-global-tty ((self C-measure) beat-string) 
 (let ((num-or-list (read-from-string beat-string)))
     (if (eql *measure-edit-mode* 'metr)
        (if (not (listp num-or-list))  
          (ui:ed-beep)
          (progn 
            (setf (metronome-unit self) (first num-or-list))
            (setf (metronome self) (second num-or-list))))
         (if (not (numberp num-or-list))  
           (ui:ed-beep)
           (setf (low self) beat-string)))
      (when *current-rtm-editor* 
        (erase+view-draw-contents *current-rtm-editor*))))

(defmethod add-beat-after-myself ((self C-measure) beat)
  (let ((nth-beat (nth? beat (beat-objects self))))
    (setf (beat-objects self)
      (append (firstn (1+ nth-beat) (beat-objects self))
              (list (beat-constructor 1 '(1)))
              (nthcdr (1+ nth-beat) (beat-objects self)))))) 

(defmethod add-beat-before-myself ((self C-measure) beat)  
    (let ((nth-beat (nth? beat (beat-objects self))))
      (setf (beat-objects self)
        (append (firstn nth-beat (beat-objects self))
                (list (beat-constructor 1 '(1)))
                (nthcdr nth-beat (beat-objects self)))))) 

(defmethod remove-myself-from-measure ((self C-measure) beat)  
  (setf (beat-objects self) (remove beat (beat-objects self) :test 'eq)))

(defmethod paste-beat ((self C-measure))  
  (paste-beat-myself (super-measure-line self) self))

(defmethod paste-beat-myself ((self C-measure) beat)  
 (when *rtm-struct-selection-scrap*
  (let ((nth-beat (nth? beat (beat-objects self))))
    (setf (beat-objects self)
      (append (firstn nth-beat (beat-objects self))
              (list (eval *rtm-struct-selection-scrap*))
              (nthcdr (1+ nth-beat) (beat-objects self))))))) 

;;=================================

(defmethod add-beat-after ((self C-measure))  
  (when (super-measure-line self) (add-beat-after-myself (super-measure-line self) self)))

(defmethod add-beat-before ((self C-measure))  
  (when (super-measure-line self) (add-beat-before-myself (super-measure-line self) self)))

(defmethod remove-beat-from-measure ((self C-measure))  
  (when (super-measure-line self)
    (kill-chords self)    
    (remove-myself-from-measure (super-measure-line self) self)))


;;========================================================

(defclass C-measure-line ()
  ((measures :initform () :initarg :measures :accessor measures) 
   (stop-flag :initform t :accessor stop-flag)))  

(defmethod decompile ((self C-measure-line))
  `(make-instance 'C-measure-line  
        :measures (list ,@(ask-all (measures self) 'decompile))))

(defmethod paste-beat-myself ((self C-measure-line) measure)  
 (when *measure-selection-scrap*
  (let ((nth-measure (nth? measure (measures self))))
    (setf (measures self)
      (append (firstn nth-measure (measures self))
              (list (eval *measure-selection-scrap*))
              (nthcdr (1+ nth-measure) (measures self))))))) 

(defgeneric kill-all-measures (self))
(defmethod  kill-all-measures ((self C-measure-line)) 
  (kill-chords self)
  (setf (measures self) ()))

(defgeneric draw-measures- (self obj C5 t-scfactor first-beat-num beg-x end-x y1 y2 &optional mode measuresl))
(defmethod draw-measures-  ((self C-measure-line) obj C5 t-scfactor first-beat-num beg-x end-x y1 y2 &optional mode measuresl)
  (declare (ignore mode))
  (let ((measures (if measuresl measuresl (nthcdr first-beat-num (measures self)))) 
        previous-measure previous-info (new-beg-x beg-x))
    (when measures
      (while (and (< new-beg-x end-x) measures)
        (when previous-measure 
          (setq previous-info (list (low previous-measure)(high previous-measure)
                                    (metronome previous-measure)(metronome-unit previous-measure))))
        (setq beg-x (draw-measure (car measures) obj C5 t-scfactor beg-x y1 y2 previous-info))  
        (setq previous-measure (pop measures))
        (if (car measures)
            (setq new-beg-x (+ *rtm-editor-measure-x-offset* (calc-measure-pixel-x (car measures) t-scfactor beg-x)))
            (setq new-beg-x beg-x)))
      (unless measures
        (if (= *staff-num* 7)
            (progn 
              (with-pen-state  (:size (make-point 2 1)) 
                (draw-line (+ 2 beg-x) (- y1 10) (+ 2 beg-x) (+ y1 25)))) 
            (progn 
              (with-pen-state  (:size (make-point 2 1)) 
                (draw-rtm-measure-line-ys (+ 2 beg-x) C5))))))
    measures))

;;=======================================
(defmethod  collect-all-chord-beat-leafs ((self C-measure-line))
  (setf *beat-leaf-objs* ())
  (tell  (apply 'append (ask-all (measures self) 'beat-objects)) 'collect-all-chord-beat-leafs)
  *beat-leaf-objs*)

;;=======================================
(defgeneric stop-measure-line (self))
(defmethod stop-measure-line ((self C-measure-line))
  (setf *MN-play-flag* ())
  (setf (stop-flag self) t))

(defgeneric play-measure-line-continue (self measures t-scfactor))
(defmethod play-measure-line-continue ((self C-measure-line) measures t-scfactor)
  (unless (stop-flag self)
    (when measures 
      (let ((delay (play-measure (pop measures) t-scfactor)))
        (dfuncall (truncate (abs delay)) 'play-measure-line-continue self measures t-scfactor)))))
 
(defgeneric play-measure-line (self t-scfactor))
(defmethod play-measure-line ((self C-measure-line) t-scfactor)
  (setf (stop-flag self) nil)
  (play-measure-line-continue  self (measures self) t-scfactor))

;;================================ 
(defun make-default-measure-object ()
  (make-instance 'C-measure  :low "4" :beat-objects  
    (list (beat-constructor 1 '(-1))(beat-constructor 1 '(-1))(beat-constructor 1 '(-1))(beat-constructor 1 '(-1)))))

(defmethod add-beat-after-myself ((self C-measure-line) measure)
  (if (not measure)
    (setf (measures self) (list (make-default-measure-object)))
    (let ((nth-measure (nth? measure (measures self))))
      (setf (measures self)
        (append (firstn (1+ nth-measure) (measures self))
              (list (make-default-measure-object))
              (nthcdr (1+ nth-measure) (measures self))))))) 

(defmethod add-beat-before-myself ((self C-measure-line) measure)  
  (let ((nth-measure (nth? measure (measures self))))
    (setf (measures self)
      (append (firstn nth-measure (measures self))
              (list (make-default-measure-object))
              (nthcdr nth-measure (measures self)))))) 

(defmethod remove-myself-from-measure ((self C-measure-line) measure)  
  (setf (measures self) (remove measure (measures self) :test 'eq)))

;;========================================================

(defgeneric draw-beat-chord-5 (self beg-x C5 beat-zoom y-beat note-head))
(defmethod draw-beat-chord-5  ((self C-chord) beg-x C5 beat-zoom y-beat note-head)
  (setq *MN-note-ins-y* *MN-global-ins-y*)
  (when (notes self)
    (draw-beat-stem-4 self beg-x C5 y-beat note-head)
    (tell (give-all-draw-notes self) 'draw-note-4-rtm beg-x C5 beat-zoom note-head))
  (draw-extra-info self beg-x C5 ()))

(defgeneric draw-beat-stem-4 (self x C5 y-beat note-head))
(defmethod draw-beat-stem-4 ((self C-chord) x C5 y-beat note-head)
  (let ((y-min (1- (give-pixel-y (car (notes self)) C5)))
        (y-max (give-pixel-y (car (last (notes self))) C5)))
    (unless (eql note-head #\w) 
      (draw-line x y-min x y-beat))
    (draw-ledger-lines self x y-min y-max C5)))

(defvar *rtm-only-white-heads* ())
;;(setf *rtm-only-white-heads* ())

(defgeneric draw-note-extra-stuff (self x C5 t-scfactor note-head))
(defmethod draw-note-extra-stuff  ((self C-note) x C5 t-scfactor note-head)
  (declare (ignore x C5 t-scfactor note-head)))

;; (defmethod draw-note-4-rtm  ((self C-note) x C5 t-scfactor note-head)
;;   (declare (special *mn-view-time-flag*))
;;   (let ((y-now (give-pixel-y self C5))
;;         (x-now (+ x (delta-x self)))
;;         (alt (alteration self)))
;;     (if (or *rtm-only-white-heads* (eql note-head #\w)(eql note-head #\h))
;;         (draw-char x-now y-now #\w) 
;;         (draw-char (1+ x-now) y-now #\Ω))
;;     (when t-scfactor
;;       (if *mn-view-dyn-flag*
;;           (draw-note-symbolic-dynamic self x-now y-now))
;;       (if *mn-view-dur-flag*
;;           (draw-note-duration-line self x-now y-now t-scfactor))
;;       (if (and *mn-view-offset-flag* (not *mn-view-time-flag*))
;;           (draw-note-offset-line self x-now y-now t-scfactor)) )   
;;     (when (and (instrument self) *mn-view-ins-flag* t-scfactor) 
;;                                         ;(draw-char x-now y-now #\Ω)
;;       (draw-instrument (instrument self) x-now y-now (round (+ (* t-scfactor (dur self))))))
;;     (if (and alt (not (eql *staff-num* 7) ))  ; empty staff
;;         (draw-char (+ x (alt-delta-x self)) y-now alt))
;;     (draw-note-extra-stuff self  x C5 t-scfactor note-head)))

(defgeneric draw-note-4-rtm (self x C5 t-scfactor note-head))
(defmethod draw-note-4-rtm  ((self C-note) x C5 t-scfactor note-head)
  (declare (special *mn-view-time-flag*))
  (let* ((offs-fl *mn-view-offset-flag*)
         (y-now (give-pixel-y self C5))
         (x-now (if offs-fl
                    (+ (round (* t-scfactor (offset-time self)))  -6 x)  
                    (+ x (delta-x self))))
         (alt (alteration self)))
    (if (or *rtm-only-white-heads* (eql note-head #\w)(eql note-head #\h))
        (draw-char x-now y-now (if (and (not (zerop (offset-time self))) offs-fl) (code-char 201) #\w))
        (draw-char (1+ x-now) y-now (if (and (not (zerop (offset-time self))) offs-fl) (code-char 201) #\Ω) ))
    (when t-scfactor
      (if *mn-view-dyn-flag*
          (draw-note-symbolic-dynamic self x-now y-now))
      (if *mn-view-dur-flag*
          (draw-note-duration-line self x-now y-now t-scfactor))
      ;;      (if (and *mn-view-offset-flag* (not *mn-view-time-flag*))
      ;;        (draw-note-offset-line self x-now y-now t-scfactor))
      )
    (when (and (instrument self) *mn-view-ins-flag* t-scfactor) 
      (draw-instrument (instrument self) x-now y-now (round (+ (* t-scfactor (dur self))))))
    (if (and alt (not (eql *staff-num* 7) )) ; empty staff
        (draw-char 
         (if offs-fl
             (+ x-now -5)
             (+ x (alt-delta-x self))) 
         y-now alt))
    (draw-note-extra-stuff self  (+ (if offs-fl (offset-time self) 0) x) C5 t-scfactor note-head)))

(defgeneric transpose-all-notes (self cents))
(defmethod transpose-all-notes ((self C-chord) cents)
  (let ((notes (notes self)))
    (while notes (transpose-note (pop notes) cents)))
  (update-chord self))

;; (defmethod set-all-durs ((self C-chord) dur)
;;   (let ((notes (notes self)))
;;     (while notes (setf (dur (pop notes)) dur))))


(defgeneric set-all-durs (self dur))
(defmethod set-all-durs ((self C-chord) dur)
  (let* ((off-fl 
           (or (not *active-rtm-window*)
               (third (ask-all 
                       (rtm-radio-ctrls (editor-collection-object *active-rtm-window*)) 'check-box-checked-p))))
         (notes (sort (copy-list (notes self)) '< :key (lambda (note) (offset-time note))))
         (offsets (ask-all notes #'offset-time))
         diff-lst diff-lst+)
    (if (not (rest offsets))
        (setf (dur (car notes)) (- dur (car offsets)))
        (progn 
          (setq diff-lst (mapcar #'- (cdr offsets) offsets))
          (setq diff-lst+ (append diff-lst (list (- dur (car (last diff-lst))))))
          (while notes 
            (setf (dur (car notes))  
                  (if off-fl
                      (1- (car diff-lst+)) 
                      dur))
            (pop notes)(pop diff-lst+))))))

(defgeneric set-all-chans (self chan))
(defmethod set-all-chans ((self C-chord) chan)
  (let ((notes (notes self)))
    (while notes (setf (chan (pop notes)) chan))))
(defgeneric set-all-vels (self vel))
(defmethod set-all-vels ((self C-chord) vel)
  (let ((notes (notes self)))
    (while notes (setf (vel (pop notes)) vel))))
(defgeneric set-all-instruments (self ins))
(defmethod set-all-instruments ((self C-chord) ins)
  (when ins
    (let ((notes (notes self)))
      (while notes 
        (setf (instrument (car notes)) (eval (decompile ins)))
        (setf  (super-note (instrument (car notes))) (car notes))
        (pop notes)))))

(defgeneric play-chord-with-offset (self))
(defmethod play-chord-with-offset ((self C-chord))
  (if *mn-view-offset-flag* 
      (let ((notes (notes self)))
        (while notes (dfuncall (offset-time (car notes)) 'play-note (car notes)) (pop notes)))
      (play-chord self)))

;; (defmethod draw-beat-note  ((self C-note) x C5 note-head)
;;  (declare (ignore note-head)) 
;;  (let ((y-now (give-pixel-y self C5))
;;         (x-now (+ x (delta-x self)))
;;         (alt (alteration self)))
;;      (draw-char x-now y-now #\w)
;; ;;    (draw-char x-now y-now #\w)
;;     (when alt
;;       (unless (eql *staff-num* 7)   ; empty staff
;;         (draw-char (+ x (alt-delta-x self)) (1- y-now) alt)))))

;;=====================
;; t-time + duration

(defgeneric calc-t-time-beat-continue (self beats beat-unit-ticks time-now &optional sel-chords))
(defmethod calc-t-time-beat-continue ((self C-beat) beats beat-unit-ticks time-now &optional sel-chords)
  (when beats 
    (setq time-now (calc-t-time-beat (pop beats) beat-unit-ticks time-now sel-chords))
    (calc-t-time-beat-continue self beats beat-unit-ticks time-now sel-chords)))
 
(defgeneric calc-t-time-beat (self beat-unit-ticks time-now &optional sel-chords))
(defmethod calc-t-time-beat ((self C-beat) beat-unit-ticks time-now &optional sel-chords)
  (setq beat-unit-ticks (* (unit-length self) beat-unit-ticks))
  (let ((first-time time-now))
    ;; (print (list 'beat-unit-ticks beat-unit-ticks 'time time-now)))
    (if (beat-leaf? self) 
        (progn  
          (when (< beat-unit-ticks 0)   ;rest
            (when *rtm-last-chord-object*
              (when (check-update-dur-flag *rtm-last-chord-object* sel-chords) 
                (set-all-durs *rtm-last-chord-object* 
                              (truncate (* *rtm-duration-scaler* 
                                           (- time-now (t-time *rtm-last-chord-object*))))) )
              (setf *rtm-last-chord-object* ()))) 
          (when (beat-chord self)  
            (setf (t-time (beat-chord self)) (truncate time-now))   
            (when *rtm-last-chord-object*
              (when (check-update-dur-flag *rtm-last-chord-object* sel-chords) 
                (set-all-durs *rtm-last-chord-object* 
                              (truncate (* *rtm-duration-scaler* 
                                           (- (t-time (beat-chord self))(t-time *rtm-last-chord-object*)))))))
            (setf *rtm-last-chord-object* (beat-chord self)) ) 
          (setq time-now (+ time-now (abs beat-unit-ticks))))
        (let* ((rtm-list (rtm-list self))
               (rtm-sum (apply #'+ (mapcar #'abs (ask-all (rtm-list self) 'unit-length))))
               (ticks-incr (/ beat-unit-ticks rtm-sum)))
          (calc-t-time-beat-continue self rtm-list ticks-incr time-now sel-chords)
          (setq time-now (+ time-now ticks-incr))))
    (+ first-time (abs beat-unit-ticks))))

;;(print (list (t-time (beat-chord self)) (ask-all (notes  (beat-chord self)) 'midic))))   

 
(defgeneric calc-t-time-measure (self t-scfactor time-now &optional sel-chords))
(defmethod calc-t-time-measure ((self C-measure) t-scfactor time-now &optional sel-chords)
  (let* ((measure-length (calc-measure-length self t-scfactor ())) ;midiflag 
         (first-time time-now)
         (beats (beat-objects self))
         (beat-unit-ticks (/ measure-length (calc-high-value self))))
    ;;    (print (list 'measure-length measure-length 'beat-unit-ticks beat-unit-ticks))
    (while beats 
      (setq time-now (calc-t-time-beat (pop beats) beat-unit-ticks time-now sel-chords)))
    (+ first-time measure-length)))

(defgeneric calc-t-time-measure-line (self t-scfactor &optional sel-chords))
(defmethod calc-t-time-measure-line ((self C-measure-line) t-scfactor &optional sel-chords)
  (let ((time-now 0) (measures (measures self)))
    (setf *rtm-last-chord-object* ())
    (while measures 
      (setq time-now (calc-t-time-measure (pop measures) t-scfactor time-now sel-chords)))
    (when *rtm-last-chord-object*
      (when (check-update-dur-flag *rtm-last-chord-object* sel-chords) 
        (set-all-durs *rtm-last-chord-object* 
                      (truncate (* *rtm-duration-scaler* 
                                   (- time-now (t-time *rtm-last-chord-object*)))))))))

(defun check-update-dur-flag (last-chord sel-chords)
  (if sel-chords (member last-chord sel-chords) t))

;;========================================================

;; ;;(beat-constructor 1 '(1 2 (1 (1 1 1))))
;; ;;(beat-constructor 1 '(1  (1 (1 1 1))  (1 ((1 (1 1 1)) 1))))
;; ;;(beat-constructor 1 '(1 2 1))
;; (setq bs (make-instance 'C-beat :unit-length 1
;;     :rtm-list  
;;       (list (make-instance 'C-beat :unit-length 1)
;;             (make-instance 'C-beat :unit-length 2)
;;             (make-instance 'C-beat :unit-length 1))))
;; ;;(beat-constructor 1 '(1  (1 (1 1 1))  (1 ((1 (1 1 1)) 1))))
;; (setq bs2 (make-instance 'C-beat :unit-length 1
;;     :rtm-list  
;;       (list  (make-instance 'C-beat :unit-length 1)
;;              (make-instance 'C-beat :unit-length 1 
;;                :rtm-list  
;;                   (list (make-instance 'C-beat :unit-length 1)
;;                         (make-instance 'C-beat :unit-length 1)
;;                         (make-instance 'C-beat :unit-length 1)))
;;              (make-instance 'C-beat :unit-length 1 
;;                :rtm-list  
;;                   (list (make-instance 'C-beat :unit-length 1 
;;                           :rtm-list  
;;                            (list (make-instance 'C-beat :unit-length 1)
;;                                  (make-instance 'C-beat :unit-length 1)
;;                                  (make-instance 'C-beat :unit-length 1)))
;;                         (make-instance 'C-beat :unit-length 1))))))


;; ;;============================
;; ;;____________________________
;; 
;; (setq bc11 (beat-constructor 2 '((1 (3 4))  -1  (1 (1 (2 (1 1 1)))))))
;; (setq bc12 (beat-constructor 1 '(1 -1 1)))
;; (setq bc13 (beat-constructor 1 '(1.0 -3)))
;; (setq bc14 (beat-constructor 1 '(1 (2 (4 3 7)) 1 1)))
;; (setq bc15 (beat-constructor 1 '(1 2 1 1)))
;; (setq bc16 (beat-constructor 1 '(1 4 1 1)))
;; (setq bc17 (beat-constructor 1 '(1.0 -3 1 1 -1 1 1)))
;; 
;; ;;(decompile bc17) 
;; 
;; 
;; (setq bed1 (make-instance 'C-beat-editor-panel :view-position (make-point 10 2) :view-size (make-point  690 140)))
;; 
;; (setf (measure-line bed1)
;;   (make-instance 'C-measure-line :measures
;;     (list 
;;       (make-instance 'C-measure  :low "4" 
;;          :beat-objects (list bc11 bc12 bc13))
;;       (make-instance 'C-measure  :low "4" 
;;          :beat-objects (list bc14 bc15 bc16 ))
;;       (make-instance 'C-measure  :low "4" 
;;          :beat-objects (list bc17)))))
;; 
;; ;;(setf (measure-line bed1) (eval (decompile (measure-line bed1))))
;; 
;; ;;(decompile  (beat-constructor 1 '((2 (1.0 1 3)) 1)))
;; 
;; ;;____________________________
;; ;;(beat-constructor 1 '(1 (1)))  
;; (setq bc1 (beat-constructor 1 '(1  (1 (1 1 1))  (1 ((1 (1 1 1)) 1)) )))
;; (setq bc2 (beat-constructor 1 '(1  (2 (1 3 1))  (1 (1 2)) 1 (2 (1 1 1)))))
;; (setq bc3 (beat-constructor 1 '(1 (3 (1 1 1 3)) 1 (1 (1 1)) 1)))
;; (setq bc4 (beat-constructor 2 '((1 (1 -1 1 1 1))(1 (1 -1 1 -1 1)) (1 (1 1 -1 1 1)))))
;; (setq bc5 (beat-constructor 1 '(1 1 1 1 1 1)))
;; (setq ml6 (beat-constructor 1 '((2 (1.0 1 3)) 1)))
;; (setq ml7 (beat-constructor 2 '(2.0 2 2 (3 (1 -1 1 -1)))))
;; 
;; (setq bed2 (make-instance 'C-beat-editor-panel :view-position (make-point 10 150) :view-size (make-point 690 140)))
;; 
;; 
;; (setf (measure-line bed2)
;;   (make-instance 'C-measure-line :measures
;;     (list 
;;       (make-instance 'C-measure  :low "4" 
;;          :beat-objects  (list bc3 bc4 bc5))
;;       (make-instance 'C-measure :low "4" 
;;          :beat-objects  
;; (list ml6 ml7)))))
;; ;;(list bc1 bc2 bc3 bc4 bc5)))))
;; 
;; ;;====================================
;; (setq wws (make-instance 'C-rtm-editor-window :view-position (make-point 10 50) 
;;                                               :view-size (make-point 710 345) :window-title "Measure test"))
;; 
;; (setq bedcol (make-instance 'C-beat-editor-collection 
;;                    :view-position (make-point 5 5) 
;;                    :view-size (make-point 700 340)  
;;                    :beat-editors (list bed1 bed2)))
;; 
;; (add-subviews wws bedcol)
;; 
;; ;;====================================
;; (window-select wws)


(defun initialize-beat-measure-line ()
  (setf *the-chord-rtm-editor* (make-MN-editor-chordMN-rtm *g2-g-f-f2-staffs*)))

;;;; THE END ;;;;
