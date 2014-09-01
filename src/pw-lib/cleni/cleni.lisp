;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               cleni.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;    
;;;;    XXX
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;    Gérard Assayag
;;;;MODIFICATIONS
;;;;    2012-05-07 <PJB> Changed license to GPL3; Added this header.
;;;;    1991       Gérard Assayag CLENI: Common Lisp to Enigma Interface 2.0
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


(defpackage "CLENI" 
  (:use "COMMON-LISP" "MCLGUI")
  (:export  "NEW-SCORE" "DESCRIBE-SCORE" "TRANSLATE-SCORE"
            "*HALF-TONE*" "*QUARTER-TONE*" "*G-KEY*" "*C-3-KEY*"
            "*C-4-KEY*" "*F-KEY*"))
(in-package "CLENI")



(defclass Score ()
  (
   (temperament :initform 2)
   (voice-system :initform () :accessor voice-system)
   (current-voice :initform 0 :accessor current-voice)
   (staff-system :initform () :accessor staff-system)
   (measure-system :initform () :accessor measure-system)
   (current-measure :initform () :accessor current-measure)
   (event-system :initform () :accessor event-system)
   (current-event :initform () :accessor current-event)
   (tuplet-list :initform () :accessor tuplet-list)
   (current-tuplet :initform () :accessor current-tuplet)
   ))

(defclass VoiceSystem ()
  (
   (parent-score :initform () :accessor parent-score :initarg :parent-score)
   (voice-list :initform () :accessor voice-list)
   ))

(defclass Voice ()
  (
   (parent-system :initform () :initarg :parent-system :accessor parent-system)
   (event-list :initform () :accessor event-list)
   ))

(defclass EventSystem ()
  (
   (parent-score :initform () :accessor parent-score :initarg :parent-score)
   (event-list :initform () :accessor event-list)
   (event-count :initform 0 :accessor event-count)
   ))

(defclass StaffSystem ()
  (
   (parent-score :initform () :accessor parent-score :initarg :parent-score)
   (staff-list :initform () :accessor staff-list)
   ))

(defclass Staff ()
  (
   (parent-system :initform nil :initarg :parent-system :accessor parent-system)
   (start-key :initform 0 :accessor start-key :initarg :start-key)
   (voice :initform () :accessor voice :initarg :voice)
   ))

(defclass MeasureSystem ()
  (
   (parent-score :initform () :accessor parent-score :initarg :parent-score)
   (measure-list :initform () :accessor measure-list)
   (biggest-measure :initform ())
   ))

(defclass Measure ()
  (
   (parent-system :initform nil :initarg :parent-system :accessor parent-system)
   (num :initform 4 :accessor num :initarg :num)
   (denum :initform 4 :accessor denum :initarg :denum)
   (width :initform 600 :accessor width :initarg :width)
   ))

(defclass ScoreEvent ()
  (
   (parent-score  :initform nil :initarg :parent-score :accessor parent-score)
   ;;; 4ter note = 1024. 8th note = 256. etc.
   (event-duration :initform 1024 :accessor event-duration :initarg :event-duration)
   (rank :initform 0 :accessor rank)
   (event-voice :accessor event-voice)
   (event-measure :accessor event-measure)
   (event-tuplet :initform () :accessor event-tuplet)
   (follow :initform () :accessor follow :initarg :follow)
   (first-in-tuplet :initform () :accessor first-in-tuplet)
   ))

(defclass Note (ScoreEvent)
  (
   (pitch :initform #x0000 :accessor pitch :initarg :pitch)
   (begin-tie :initform () :accessor begin-tie)
   ))


(defclass Rest (ScoreEvent)
  (   
   (pitch :initform #x0060 :accessor pitch :initarg :pitch)
   ))

(defclass Chord (ScoreEvent)
  (
   (note-list
    :initform (list (make-instance 'note) (make-instance 'note :pitch 6600)) 
    :accessor note-list :initarg :note-list)
   ))


(defclass Tuplet ()
  (
   (nbunit1 :initform 3 :accessor nbunit1 :initarg :nbunit1)
   (unit1 :initform 512 :accessor unit1 :initarg :unit1)
   (nbunit2 :initform 3 :accessor nbunit2 :initarg :nbunit2)
   (unit2 :initform 512 :accessor unit2 :initarg :unit2)
   (event-count :initform 0 :accessor event-count)
))


;;; ====================================================================
;;;     Globals
;;; ====================================================================

;;; Keys

(defvar *G-key* 0)
(defvar *C-3-key* 1)
(defvar *C-4-key* 2)
(defvar *F-key* 3)

;;; note reference for rest positioning

(defvar *rest-reference* #(b4 c4 a3 d3))

;;; Temperaments

(defvar *half-tone* 2)
(defvar *quarter-tone* 4)


;;; ====================================================================
;;;     Macros and utilities
;;; ====================================================================

;;; extend a list towards right

(defmacro pushr (item place)
  `(if ,place
     (nconc  ,place (list ,item))
     (setf ,place (list ,item))))


;;; convert the ... 4, 8, 16 ... convention for duration  to Enigma
;;; covention ...1024, 512, 256...

(defmacro enigma-duration (denum)
  `(/ (* 1024 4) ,denum))


;;; pitch unit conversions

(defvar *temperament* 2)

(defvar *diatonic-table*  #(0 0 1 1 2 3 3 4 4 5 5 6) )
(defvar *accidental-table* #(0 1 0 1 0 0 1 0 1 0 1 0) )

(defun chromatic-to-diatonic (d)
  (values (elt *diatonic-table* (mod d 12))
          (elt *accidental-table* (mod d 12))) )


(defun midics-to-enigma (midics)
  (let (m1 m2 octave eni)
    (setf m1 (truncate midics 100)
          m2 (mod m1 12)
          octave (- (truncate m1 12) 5)
          eni (+ (* (+ (* octave 7) (elt *diatonic-table* m2)) #x0010)
                 (truncate (- midics
                              (* (- m1 (elt *accidental-table* m2))
                                 100))
                           (truncate 200 *temperament*))))
    (if (< eni 0) (+ #x10000 eni) eni)))


(defun symbol-to-enigma (symb)
  (let ((symb (string-upcase (symbol-name symb)) ) deg oct (acc 0) (index 0))
    (setf deg (elt symb index)
          oct (elt symb (incf index)))
    (setf deg (mod (- (char-code deg) (char-code #\C)) 7)
          oct (* 7 (- (- (char-code oct) (char-code #\0)) 4)))
    (when (> (length symb) 2)
      (case (elt symb (incf index))
        (#\S (setf acc (/ *temperament* 2) ))
        (#\B (setf acc (- (/ *temperament* 2))))
        (t (decf index))))
    (do ((i (incf index) (1+ i))) ( (>= i (length symb)) )
      (case (elt symb i)
        (#\+ (incf acc))
        (#\- (decf acc)) ))
    (when (< acc 0) (setf acc (+ 8 (abs acc))))
    (setf deg (+ (* 16 deg) (* 16 oct) acc))
    (if (< deg 0) (+ #x10000 deg) deg) ))


;;; ====================================================================
;;;      methods for Score
;;; ====================================================================


(defmethod initialize-instance :after ((self Score) &key controls)
  (declare (ignore controls))
  (setf  (staff-system self)
         (make-instance 'StaffSystem :parent-score self))
  (setf  (measure-system self)
         (make-instance 'MeasureSystem :parent-score self))
  (setf  (voice-system self)
         (make-instance 'VoiceSystem :parent-score self))
  (setf  (event-system self)
         (make-instance 'EVentSystem :parent-score self))
  )


(defmethod set-temperament ((self Score) temperament)
  (setf (slot-value self 'temperament) temperament)
  )

(defmethod push-staff ((self Score) &rest more)
  (apply 'push-staff (staff-system self) more))

(defmethod push-measure ((self Score) &rest more)
  (apply 'push-measure (measure-system self) more))

(defmethod push-voice ((self Score) &rest more)
  (apply 'push-voice (voice-system self) more))

(defmethod push-event ((self Score) &rest more)
  (apply 'push-event (event-system self) more))

(defmethod number-objects ((self Score))
    (number-objects (event-system self)))
    
(defmethod rewind-measure ((self Score))
  (setf (current-measure self)
        (first (measure-list (measure-system self)))))

(defmethod go-next-measure ((self Score))
  (setf (current-measure self)
        (second (member (current-measure self)
                     (measure-list (measure-system self))))))

(defmethod rewind-voice ((self Score))
  (setf (current-voice self)
        (first (voice-list (voice-system self)))))

(defmethod go-next-voice ((self Score))
  (setf (current-voice self)
        (second (member (current-voice self)
                     (voice-list (voice-system self))))))

(defmethod select-staff ((self Score) index)
  (let ( staff
         (staff-list (staff-list (staff-system self))) )
    (setf staff (nth (1- index) staff-list))
    (unless staff
      (dotimes  (i (- index (length staff-list)))
        (setf staff (push-staff self))))
    (setf (current-voice self) (voice staff))
    staff))

   
(defmethod select-measure ((self Score) index)
  (let ( measure
         (measure-list (measure-list (measure-system self))) )
    (setf measure (nth (1- index) measure-list))
    (unless measure
      (dotimes  (i (- index (length measure-list)))
        (setf measure (push-measure self)) ))
    (setf (current-measure self) measure)
    measure))


(defmethod open-tuplet ((self Score) nbunit1 unit1 nbunit2 unit2)
  (setf (current-tuplet self)
        (make-instance 'tuplet :nbunit1 nbunit1 :unit1 unit1 :nbunit2 nbunit2 :unit2 unit2))
  (pushr (current-tuplet self) (tuplet-list self))
)

(defmethod close-tuplet ((self Score))
  (setf (current-tuplet self) nil))

(defmethod get-map ((self Score))
  (let ((event-list (event-list (event-system self))) (measure-nb 0) map event1 event2)
    (do* ( (voice-list (voice-list (voice-system self)))
           (voice (pop voice-list) (pop voice-list))
           (nv 1 (1+ nv)) )
         ((null voice) (reverse map))
      (do* ( (measure-list (measure-list (measure-system self)) )
             (measure (pop measure-list) (pop measure-list))
             (nm 1 (1+ nm)) )
           ((null measure)) 
        (when event-list
          (setf event1 (car event-list))
          (when (and (eql (event-voice event1) voice)
                     (eql (event-measure event1) measure))
            (do () ((or (null event-list)
                        (not (eql  (event-measure (car event-list)) measure))
                        (not (eql  (event-voice (car event-list)) voice))))
              (setf event2 (pop event-list)))
            (push  (list nv nm (incf measure-nb) (rank-left event1) (rank-left event2))
                   map) ))  ))))


(defmethod put-map-to-enigma ((self score))
  (let ((map (get-map self)) (staff-list (staff-list (staff-system self)))
        (measure-list (measure-list (measure-system self))))
  (format t "~%~%^others~%~%")
  (dolist (map-elt map)
      (format t "^FR(~D) ~D ~D ~D 0~%"
                (third map-elt)  (fourth map-elt) (fifth map-elt)
                (width (elt measure-list (1- (second map-elt))))))
  (format t "~%~%^details~%~%")
  (dolist (map-elt map)
      (format t "^GF(~D,~D) ~D ~D 0 0 0 0~%"
                (first map-elt)  (second map-elt) (third map-elt)
                (start-key (elt staff-list (1- (first map-elt))))))
  (dolist (event (event-list (event-system self)))
    (when (first-in-tuplet event)
      (format t "^TN(0,~D) 24 0 0 0 1~%"
              (rank event))
      (format t "^TP(0,~D) ~D ~D ~D ~D ~D ~%"
              (rank event)
              (nbunit1 (event-tuplet event))
              (unit1 (event-tuplet event))
              (nbunit2 (event-tuplet event))
              (unit2 (event-tuplet event))
              (1- (event-count (event-tuplet event))))))
  t))


(defmethod translate-to-enigma ((self Score))
  (setf *temperament* (slot-value self 'temperament))
  (number-objects self)
  (put-map-to-enigma self)
  (format t "~%~%^others~%~%")
  (translate-to-enigma (staff-system self))
  (translate-to-enigma  (measure-system self))
  (format t "~%~%^entries~%~%")
  (translate-to-enigma (voice-system self))
  (translate-to-enigma (event-system self))
  t)


;;; ====================================================================
;;;      methods for VoiceSystem
;;; ====================================================================

(defmethod push-voice ((self VoiceSystem) &rest more)
  (declare (ignore more))
  (let ((voice (make-instance 'voice ':parent-system self)))
    (setf (current-voice (parent-score self)) voice)
    (pushr voice (voice-list self))
    voice))


(defmethod translate-to-enigma ((self VoiceSystem))
)


;;; ====================================================================
;;;      methods for Voice
;;; ====================================================================


(defmethod translate-to-enigma  ((self Voice))
)


(defmethod parent-score ((self Voice)) (parent-score (parent-system self)))



;;; ====================================================================
;;;      methods for EventSystem
;;; ====================================================================

(defmethod push-event ((self EventSystem) &key (duration 1024) (type 'note) pitch (follow nil) (tie nil))
  (let ( (event (make-instance type :event-duration duration :follow follow)) )
    (setf *temperament* (slot-value (parent-score self) 'temperament))
    ;(when (and  (typep event 'rest) (not pitch))
    ;  (setf (pitch event) (elt *rest-reference* (start-key (parent-score self)))))
    (when (and (or (typep event 'note) (typep event 'rest)) pitch)
      (setf (pitch event)
            (if (numberp pitch)
              (midics-to-enigma pitch)
              (symbol-to-enigma pitch))))
    (when (and (typep event 'chord) pitch)
      (setf (note-list event)
            (mapcar
             (lambda (pitch)
                 (make-instance 'note 
                                :pitch (if (numberp pitch)
                                         (midics-to-enigma pitch)
                                         (symbol-to-enigma pitch))
                                :event-duration duration
                                :follow follow))
             pitch)))
    (when tie
      (if (typep event 'note)
        (setf (begin-tie event) tie)
        (mapc (lambda (note tie-val) (setf (begin-tie note) tie-val))
              (note-list event)
              tie)))
    (setf (current-event (parent-score self)) event)
    (pushr event (event-list  self))
    (setf (event-voice event) (current-voice (parent-score self))
          (event-measure event) (current-measure (parent-score self))
          (event-tuplet event) (current-tuplet (parent-score self)))
    (when (event-tuplet event)
      (when (zerop (event-count (event-tuplet event)))
        (setf (first-in-tuplet event) t))
      (incf (event-count (event-tuplet event))))
    (when (typep event 'chord)
      (mapc (lambda (note)
                (setf (first-in-tuplet note) (first-in-tuplet event)))
            (note-list event)))
    t))

(defmethod number-objects ((self EventSystem))
  (let ((i 0) (count 0))
    (dolist (event (event-list self))
      (case (class-name (class-of event))
        ((note rest)
         (setf (rank event) (incf i))
         (incf count))
        (chord 
         (setf (rank event) (1+ i))
         (dolist (note (note-list event))
           (setf (rank note) (incf i))
           (incf count) ))))
    (setf (event-count self) count)))

(defmethod translate-to-enigma  ((self EventSystem))
  (let ((prev 0) next (cprev 0) cnext)
    (format t "^eE(0) ~D ~D 0 0 0 0 $00000000 $00000000 $00000000~%"
            (event-count  self)
            (1+ (event-count  self)))
    (mapl
     (lambda (levent)
         (setf next (if (cdr levent) (rank (cadr levent)) 0)  
               cprev 0 cnext 0)
         (if (not (typep (car levent) 'chord))
           (translate-event-to-enigma (car levent) prev next cprev cnext)
           (mapl 
            (lambda (lnote) 
                (setf cnext (if (cdr lnote) (rank (cadr lnote)) 0))  
                (translate-event-to-enigma (car lnote) prev next cprev cnext )
                (setf prev 0 next 0 cprev (rank (car lnote))) )
            (note-list (car levent))))
         (setf prev (rank (car levent))))
     (event-list self)) ))



;;; ====================================================================
;;;      methods for Staff
;;; ====================================================================


(defmethod parent-score ((self Staff)) (parent-score (parent-system self)))

(defmethod translate-staff-to-enigma ((self Staff) &key (rank 1))
  (let ((biggest-measure
         (biggest-measure (measure-system (parent-score (parent-system self))))))
    (format t "^PL(~D) 0 0 0 0 0 0~%" rank)
    (format t
            "^IS(~D) ~D ~D ~D ~D 0 ~D~%"
            rank
            (start-key self)
            (if (> (slot-value (parent-score self) 'temperament) 2)
                16384
                0)
            (num biggest-measure)
            (enigma-duration (denum biggest-measure))
            (start-key self)) ))


;;; ====================================================================
;;;      methods for Measure
;;; ====================================================================

(defmethod parent-score ((self Measure)) (parent-score (parent-system self)))


(defmethod translate-measure-to-enigma ((self Measure) &key (rank 1))
  (format t
          "^MS(~D) ~D ~D ~D ~D 1 0~%"
          rank
          (width self)
          (if (> (slot-value (parent-score self) 'temperament) 2)
            16384
            0)
          (num self)
          (enigma-duration (denum self))) )


;;; ====================================================================
;;;      methods for StaffSystem
;;; ====================================================================


(defmethod push-staff ((self StaffSystem) &key (start-key 0))
  (let ((staff (make-instance 'Staff
                              :parent-system self 
                              :start-key start-key
                              :voice (push-voice (parent-score self)) ) ))
    (pushr staff  (staff-list self))
    staff))


(defmethod translate-to-enigma ((self StaffSystem))
  (let ((i 0))
    (format t "^IP(65534) ~D 0 0 0 0 0~%" (length (staff-list self)))
    (do ( (staff-list (staff-list self) (cddr staff-list))
           (i 1 (+ i 2)) 
           (offset -80 (- offset 500)) )
         ( (null staff-list) t )
      (format t "^IU(0) ~D 0 ~D " i offset )
      (if (cdr staff-list)
        (format t "~D 0 ~D~%" (1+ i) (- offset 250))
        (format t "0 0 0~%")) )
    (dolist (staff (staff-list self))
      (translate-staff-to-enigma staff :rank (incf i))) ))


;;; ====================================================================
;;;      methods for MeasureSystem
;;; ====================================================================

(defmethod push-measure ((self MeasureSystem) &key (num 4) (denum 4))
  (let ((measure (make-instance 'Measure
                                ':parent-system self 
                                ':num num
                                ':denum denum)))
    (setf (current-measure (parent-score self))
          measure)
    (pushr measure (measure-list self))
    (setf (slot-value self 'biggest-measure) nil)
    measure))

(defmethod biggest-measure ((self MeasureSystem))
  (or (slot-value self 'biggest-measure)
      (let (length the-measure (the-length 0))
        (dolist (a-measure (measure-list self))
          (setf length (/ (num a-measure) (denum a-measure)))
          (when (or (> length the-length)
                    (and (= length the-Length)
                         (> (denum a-measure) (denum the-measure))))
            (setf the-measure a-measure
                  the-length length)) )
        (setf (slot-value self 'biggest-measure) the-measure))))

(defmethod translate-to-enigma ((self MeasureSystem))
  (let ((i 0))
    (dolist (measure (measure-list self))
      (translate-measure-to-enigma measure :rank (incf i) ))) )


;;; ====================================================================
;;;      methods for ScoreEvent
;;; ====================================================================


(defmethod translate-event-to-enigma ((self ScoreEvent) prev next cprev cnext)
  (let ((pitch (pitch self)))
    (format t "^eE(~D) ~D ~D ~D ~D ~D ~D "
            (rank self)
            prev next cprev cnext
            (event-duration self)
            0)
    (format t "$~A~A~A~A~A~A~A~A "
            (if (typep self 'note) "C" 8)
            0 0 (if (first-in-tuplet self) 8 0) 0 (if (follow self) 0 8) 0 0)
    (format t "$~A~A~A~A~4,'0,X "
            (if (and (typep self 'note) (begin-tie self)) "C" 8)
            (if (zerop (mod pitch 16)) 0 1)
            0 0 
            pitch)
    (format t "$00000000~%")))


(defmethod rank-left ((self ScoreEvent)) (rank self))
(defmethod rank-right ((self ScoreEvent)) (rank self))


;;; ====================================================================
;;;      methods for Chord
;;; ====================================================================


(defmethod rank-right ((self Chord))
  (rank (first (last (note-list self)))))



;;; ====================================================================
;;;     User/ Programmer Interface
;;; ====================================================================


;;; redirect terminal io stream

(defmacro with-output-to-file ((filename &key if-exists) &body forms)
  (let ((fvar (gensym)))
    `(with-open-file (,fvar ,filename
                            :direction :output :if-exists ,(or if-exists :overwrite)
                            :if-does-not-exist :create)
       (let ((*standard-output* ,fvar) (*load-verbose* nil) )
         ,@ forms ))))

(defun cleni-error (format &rest args)
  (format *error-output* "Common Lisp to Enigma Interface. Error.~%")
  (apply 'error format args))

(defun check-pop-token (tok-list type want-err)
  (let ((errcount nil))
    (cond
       ((null tok-list)
        (when want-err (cleni-error "End of description reached")))
       (t
        (cond
         ((consp type)
          (unless (some (lambda (single-type) (check-pop-token tok-list single-type nil))
                        type)
            (setf errcount t)))
         (t
          (if (keywordp type)
            (unless  (eql type (car tok-list))
              (setf errcount t))
            (unless  (and (subtypep (type-of (car tok-list)) type)
                          (not (and (eql type 'symbol) (keywordp (car tok-list)))))
              (setf errcount t)))))
        (or (not errcount)
            (and want-err
                 (cleni-error "Syntax error.A token of type ~S was expected.~%~S"
                              type (subseq tok-list 0 )))))) ))
                    
(defmacro pop-token (tok-list type err)
  `(when (check-pop-token ,tok-list ,type ,err) 
         (pop ,tok-list)))

(defun new-score (&key temperament)
  (let ((score (make-instance 'score)))
    (when temperament  (set-temperament score temperament))
    score))

(defun describe-score (score &rest event-list) 
  (let (token index staff measure dur pitch token2 follow tie
              ;;; GA 170996 because of unconsistencies in MCL 3.9 type system
              (number-type (list (type-of 1) (type-of 1/4))))
    (loop
      (unless (setf token (pop event-list)) (return))
      (unless (keywordp token)
        (cleni-error "Syntax error. Keyword was expected.~%~S"
                     (cons token (subseq event-list 0 10)) ))
      (case token
        (:temperament (set-temperament score (pop-token event-list number-type t)))
        (:staff 
           (setf index (pop-token event-list number-type t)
                 staff (select-staff score index))
           (when (setf token (pop-token event-list :key nil))
             (setf (start-key staff)
                   (pop-token event-list number-type t))))
        (:measure 
         (setf index  (pop-token event-list number-type t)
               measure (select-measure score  index))
         (when (setf token (pop-token event-list :signature nil))
           (setf token (pop-token event-list 'list t)
                 (num measure) (car token)
                 (denum measure) (cadr token))))
        (:open-tuplet
         (open-tuplet score
                      (pop-token event-list number-type t)
                      (truncate (* (pop-token event-list number-type t) 4096))
                      (pop-token event-list number-type t)
                      (truncate (* (pop-token event-list number-type t) 4096))))
        (:close-tuplet
         (close-tuplet score))
        ((:note :rest :chord)
           (setf dur (pop-token event-list number-type t))
           (setf pitch nil follow nil tie nil)
           (case token
             (:rest
              (setf pitch (pop-token event-list (cons 'symbol number-type) nil)))
             (:note
              (setf pitch (pop-token event-list (cons 'symbol number-type) t)))
             (:chord
              (setf pitch (pop-token event-list 'list t))))           
           (setf token2 nil)      
           (loop      
             (setf token2 (or (pop-token event-list :follow nil)
                              (pop-token event-list :tie nil)))
             (unless token2 (return))
             (case token2
               (:tie
                (setf tie t)
                (when (eql token :chord)
                  (setf tie (pop-token event-list 'list t))))
               (:follow  (setf follow t))))
           (push-event score
                       :type (intern (symbol-name token) 'cleni)
                       :duration (truncate (* dur 4096))
                       :pitch pitch
                       :follow follow
                       :tie tie ))
        (t (cleni-error "Syntax Error. Unknown token~%~S"
                        (cons token (subseq event-list 0 10)) ))) )
    t))
                       

(defun translate-score (score filename)
  (let ((template-file
         (full-pathname
          (coerce (format nil "CLENI:template~D.etf" (slot-value score 'temperament))
                  'simple-string)))
        (target-file (full-pathname (merge-pathnames filename ".etf"))))    
    (format *error-output* "~%copying ~S to ~S~%"  template-file target-file)
    (alexandria:copy-file template-file target-file :if-to-exists :overwrite)
    (format *error-output* "appending score desc. to ~S~%" target-file)
    (with-output-to-file (target-file :if-exists :append)
      (translate-to-enigma score))
    (format *error-output* "done~%~%")))

;;;; THE END ;;;;
