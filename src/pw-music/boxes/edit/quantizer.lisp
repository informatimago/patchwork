;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               quantizer.lisp
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
;;;;
;;;; -*- mode:lisp; coding:utf-8 -*-
;;;=========================================================================
;;;
;;;PW rythmic Quantizing. By Camilo Rueda
;;; (c) 1992 IRCAM
;;;===========================================================================

(defpackage "QUANTIZING"
  (:use "COMMON-LISP")
  (:use "LELISP-MACROS" "PATCHWORK")
  (:import-from "EPW"  "L-SCALER/SUM" "DX->X" "X->DX")
  (:export "QUANT-MEASURE"))
(in-package "QUANTIZING")

(defparameter *maximum-pulses* 32)
(defvar *max-division* 8)
(defvar *min-pulses* 5)

(defvar *forbidden-rythmic-divisions* ())
;;;;;========================================
;;minimum-pulses suggested by J. Duthen
(defun minimum-pulses (attack-times tmin tmax)
  (min *maximum-pulses*
       (max 1 ;;;;;(1- (length attack-times))
              (truncate (- tmax tmin) (* 3 (apply 'min (- tmax tmin) (epw::x->dx attack-times)))))))
;;;;=====================
(defun needed-pulses (nb-pulse-max attack-times tmin tmax)
  (if (not attack-times) '(1)
      (let* ((minimum-pulses (minimum-pulses attack-times tmin tmax))
             (pulses (epw::arithm-ser minimum-pulses 1 (1- nb-pulse-max))))
        (unless pulses (setf pulses (list (1- nb-pulse-max))))
        (if nil ;; GA 10/10/94 (< (length pulses) *min-pulses*)
          (epw::arithm-ser minimum-pulses 1 (1- (max nb-pulse-max (* 2 minimum-pulses))))
           pulses))))

(defun compute-quanta (tmin tmax max-pulses attack-times)
  (let (quants quantized-times)
    (dolist (needed-pulses (needed-pulses max-pulses attack-times tmin tmax) (nreverse quants))
      (setq quantized-times
            (mapcar (lambda (time) (adjust-time-to-grid time needed-pulses tmin tmax)) attack-times))
      (push
       (make-quanta needed-pulses (deletions quantized-times) (distance attack-times quantized-times)
                    (proportion-distance attack-times quantized-times tmin tmax) quantized-times)
       quants))))

(defun make-quanta (pulses deletions euclidean-distance proportion-distance quantized-times)
  (cons (list pulses deletions euclidean-distance proportion-distance)
        quantized-times))

(defun adjust-time-to-grid (time pulses tmin tmax)
  (+ tmin (* (- tmax tmin) (/ pulses) (round (* (- time tmin) pulses) (- tmax tmin)))))

(defun deletions (quantized-times) (count 0 (epw::x->dx quantized-times) :test #'=))

;;euclidean distance
(defun distance (list qlist)
  (let ((accum 0.0) (length 0))
    (mapc (lambda (x y) (incf accum (sqr (- x y))) (incf length)) list qlist)
    (/ (sqrt accum) (max 1.0 (float length))) ))

(defun x->propx (l)
  (let ((new-list (remove 0 l :test #'=))) (or (mapcar #'/ new-list (rest new-list)) (list 1))))

(defvar *proportions-iota* 0.001)
;;(setf *proportions-iota* 0.001)

(defun scale-proportion-difference (diff) (* diff diff ))

(defun count-different-proportions (prop1 prop2 count)
  (let (diff-prop)
    (cond ((null prop1) (+ count (scale-proportion-difference (apply '+ prop2))))   ;;(length prop2)))
          ((null prop2) (+ count (scale-proportion-difference (apply '+ prop1)))) ;;(length prop1)))
          ((> (setq diff-prop (/ (max (first prop1) (first prop2))
                                 (min (first prop1) (first prop2)))) *proportions-iota*)
           (count-different-proportions (rest prop1) (rest prop2)
                                        (+ (scale-proportion-difference diff-prop) count) ))
          (t (count-different-proportions (rest prop1) (rest prop2) count)))))

(defun count-slope-signs (prop1 prop2)
  (abs (apply '+ (epw::g- (mapcar (lambda (x) (if (>= x 1) 1 -1)) prop1)
                          (mapcar (lambda (x) (if (>= x 1) 1 -1)) prop2)))))

;;;This is computer generated code from Joshua's error measure patch [931026]

(defun win3 (var1 var2)
  (apply '+
    (epw::g-power
     (let (A)
       (mapcar (lambda (B) (setf A B) (epw::g/ (apply '+ A) (length A)))
         (let (C D)
           (mapcar (lambda (E F)
                       (setf C E D F)
                       (let (G)
                         (mapcar (lambda (H) (setf G H) (epw::g-power (apply '/ (epw:sort-list G '>)) '3))
                                 (epw:mat-trans (list C D)))))
                   (let (J)
                     (mapcar (lambda (K) (setf J K) (cond ((= (length J) 1) J) (t (epw::g-scaling/sum J '100))))
                             (mapcar 'list
                                     (epw:x-append (patchwork:const var1)
                                                   (epw::create-list (epw::g-abs (epw::g- (max (length (patchwork:const var1))
                                                                                               (length (patchwork:const var2)))
                                                                                          (length (patchwork:const var1)))) 1)))))
                   (let (M)
                     (mapcar (lambda (N)
                                 (setf M N)
                                 (cond ((= (length M) 1) M)
                                       (t (epw::g-scaling/sum M '100))))
                             (mapcar 'list
                                     (epw:x-append (patchwork:const var2)
                                                   (epw::create-list (epw::g-abs (epw::g- (max (length (patchwork:const var1))
                                                                                               (length (patchwork:const var2)))
                                                                                          (length (patchwork:const var2)))) 1)))))))))
     3)))

(defun proportion-distance (l ll tmin tmax)
  (win3 (remove 0 (epw::x->dx (cons tmin (append l (list tmax)))) :test #'=)
        (remove 0 (epw::x->dx (cons tmin (append ll (list tmax)))) :test #'=)))

;;;another possibility: proportion distance weighted by proportions slope difference
(defvar *weight-geom* 1)
;;(setf *weight-geom* 0.7)

#|
(defun proportion-distance (l ll tmin tmax)
  (let* ((prop1 (x->propx (epw::x->dx (cons tmin (append l (list tmax))))))
         (prop2 (x->propx (epw::x->dx (cons tmin (append ll (list tmax))))))
         (length (length prop1)))
    (if (<= length 1) (distance l ll)      ;;;no proportions
      (+ (* *weight-geom* (/ (count-different-proportions prop1 prop2 0) length))
         (* (- 1 *weight-geom*) (/ (count-slope-signs prop1 prop2) length))))))
|#

(defun sqr (n) (* n n))

;;(compute-quanta 32.78688524590164 65.57377049180327 12 '( 48.85774285327191 49.85774285327191))

(defvar *distance-weight* 1)
;;(setf *distance-weight* 0.2)

(defun score-approx (times tmin  &optional (sec/black 1.0)
                           nb-pulse-max)
  (unless nb-pulse-max (setq nb-pulse-max (min *maximum-pulses* *max-division*)))
    (let ((option1 (sort (compute-quanta tmin (+ tmin sec/black) nb-pulse-max times)
                         #'quant-test1))
          (option2 (sort (compute-quanta tmin (+ tmin sec/black) nb-pulse-max times)
                         #'quant-test2)))
      ;;(format t " ~% option proportions: ~S option number of divs: ~S ~%" option1 option2)
      ;;(print
       (mapcar (lambda (item1 item2) (if (< (* (get-distance item1) (* 1.2 (- 1 *distance-weight*)))
                                             (* (get-distance item2) *distance-weight*) )
                                        item1 item2)) option1 option2)))   ;;)

;;(score-approx '(0 0.33 0.7 1 1.50 1.7) 0 2)
;;(score-approx '( 49.85774285327191 52.586885245901634) 32.78688524590164 33)

(defvar *iota* 0.0001)
(defvar *dist-iota* 0.03)

;;;(setf *dist-iota* 0.0005)

;;;Error distance measures (works INSIDE a beat).
;;;     A hierarchy of possibilities (in order of importance):
;;;        1) Number of notes should be the same
;;;        2) Sum of squares of distances between proportions (of durations) should be minimum.
;;;        3) Euclidean distance between corresponding attack times should be minimum
;;;        4) Number of pulses necessary for quantify should be minimum.
#|
(defun quant-test (quant1 quant2)
  (cond ((< (get-nb-error quant1) (get-nb-error quant2)) t)     ;;1
        ((> (get-nb-error quant1) (get-nb-error quant2)) nil)

        ((and (< (get-nb-pulse quant1) (get-nb-pulse quant2))   ;;2
              (< (get-proportion-distance quant1) *dist-iota*)) t)
        ((< (get-proportion-distance quant1) (- (get-proportion-distance quant2) *proportions-iota*)) t)
        ((> (get-proportion-distance quant1) (+ (get-proportion-distance quant2) *proportions-iota*)) nil)

        ((and (< (get-nb-pulse quant1) (get-nb-pulse quant2))   ;;3
              (< (get-distance quant1) *dist-iota*)) t)
        ((< (get-distance quant1) (- (get-distance quant2) *iota*)) t)
        ((> (get-distance quant1) (+ (get-distance quant2) *iota*)) nil)

        ((< (get-nb-pulse quant1) (get-nb-pulse quant2)) t)))
|#
(defvar *unit-division-hierarchy*
  '(1 2 4 3 6 5 8 7 10 12 16 9 14 11 13 15 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32
    33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50))

(defun is-better (n-pulse1 n-pulse2)
  (< (position n-pulse1 *unit-division-hierarchy*) (position n-pulse2 *unit-division-hierarchy*)))

(defun quant-test1 (quant1 quant2)
  (cond ((< (get-nb-error quant1) (get-nb-error quant2)) t)     ;;1
        ((> (get-nb-error quant1) (get-nb-error quant2)) nil)

        ((and (is-better (get-nb-pulse quant1) (get-nb-pulse quant2))   ;;2
              (< (get-proportion-distance quant1) *dist-iota*)) t)
        ((< (get-proportion-distance quant1) (- (get-proportion-distance quant2) *proportions-iota*)) t)
        ((> (get-proportion-distance quant1) (+ (get-proportion-distance quant2) *proportions-iota*)) nil)))

(defun quant-test2 (quant1 quant2)
  (cond ((< (get-nb-error quant1) (get-nb-error quant2)) t)     ;;1
        ((> (get-nb-error quant1) (get-nb-error quant2)) nil)
        ((is-better (get-nb-pulse quant1) (get-nb-pulse quant2)) t)))


(defun get-nb-error (quant) (second (first quant)))

(defun get-distance (quant) (third (first quant)))

(defun get-proportion-distance (quant) (fourth (first quant)))

(defun get-nb-pulse (quant) (first (first quant)))

(defun pulse-number (qtime nb-pulse tmax tmin)
  (round (* nb-pulse (- qtime tmin)) (- tmax tmin)))

(defun get-all-pulse-nums (note tmax tmin)
  (mapcar (lambda (qtime) (pulse-number qtime (caar note) tmax tmin)) (cdr note)))

(defun get-scaled-atimes (atimes dur-max)
  (epw::dx->x (car atimes) (epw::l-scaler/sum (epw::x->dx atimes) dur-max)))

(defun get-list-section (list from to)
  (let (result (epsilon -1e-3))
    (dolist (elem list)
      (cond ((>= (- elem to) epsilon) ;; GA 10/10/94 (> elem to)
             (return ()))
            ((>= (- elem from) epsilon) (push elem result))
            (t )))
    ;(unless result) (break))
     (nreverse result)))

(defvar *unquantized-notes* 0)
(defvar *global-grace-notes* ())

(defun keep-unquantized-statistics (atime previous)
  (incf *unquantized-notes*)
  (when atime
       (push (cons previous (- atime previous)) *global-grace-notes*)))
;;;===============================
(defvar *accum-error* 0)

(defun set-error (to from) (setf *accum-error* (- to from  *iota*)))
(defun reset-error () (setf *accum-error* 0))
(defun accum-error? () (plusp *accum-error*))
(defun get-accum-error () *accum-error*)

(defun test-quantize-constraints (list tmin beat-dur prev-slur)
  (let ((q-structures (score-approx list tmin beat-dur)) result)
    (if
      (dolist (current-atimes q-structures nil)
        (unless (forbidden-structure (beat-structure current-atimes prev-slur tmin (+ tmin beat-dur)))
          (return (progn (setq result (if (plusp (deleted-of current-atimes))
                                        (less-bad-quanta (list current-atimes) list tmin beat-dur)
                                        current-atimes)) t))))
      result
      (less-bad-quanta q-structures list tmin beat-dur))))

(defun less-bad-quanta (q-structures times from dur)
  (let ((try (first (member 1 q-structures :test #'<= :key #'get-nb-error))))
    (when try (try-eliminating-one try times from dur))))

(defun get-rid-of-duplicates (x times)
  (cond ((not (rest x)) times)
        ((= (first x) (second x))
         (keep-unquantized-statistics (second times) (first times))
         (get-rid-of-duplicates (rest x) (rest times)))
        (t  (cons (first times) (get-rid-of-duplicates (rest x) (rest times))))))

(defun try-eliminating-one (q-structure times from dur)
  (dolist (current-atimes
           (score-approx  (get-rid-of-duplicates (quanti-of q-structure) times) from dur) nil)
    (unless (forbidden-structure (beat-structure current-atimes nil from (+ from dur)))
      (return current-atimes))))

(defun get-optimal-time-section (list to beat-dur tmin prev-slur)
"Given a beat duration span and an initial time (tmin), quantizes the attack times in list. Beat duration
is beat-dur. Beat's onset time is tmin. Beat's end time is 'to'. If prev-slur is on, the first
onset time of this beat should be slurred with the last one of the previous beat. Prev-slur may change in
this function.If  Slur? is on, last onset of this beat should be linked with first of next beat (i.e. slur?
becomes prev-slur in the next call)."
  ;;(reset-error)
  (let* ((atimes (and list  (test-quantize-constraints list tmin beat-dur prev-slur)))
         (last-list (first (last list)))
         (q-list (and atimes (quanti-of atimes)))
         slur? lagging-count head end-surplus partition)
    (setq lagging-count (and (plusp tmin) q-list (not (= tmin (car q-list)))))
    (when (and  (accum-error?) (not lagging-count))   ;;note deleted at the frontier between beats
      (when list
        (keep-unquantized-statistics (or (first list) tmin) (- tmin (get-accum-error))))
      (reset-error))
    (setq head (and q-list (if lagging-count (cons tmin q-list) (progn (setq prev-slur nil) q-list))))
    (setq end-surplus (and head (- to (first (last head)))))
    (setq partition (and end-surplus
                         (if (> end-surplus 1e-4) ; GA 21/10/94 (plusp end-surplus)
                           (progn (reset-error) (setq slur? t) (nconc head (list to)))
                           (if (> to last-list) (progn (set-error to last-list) head) head))))
    (cond
     ((null partition)
      (if (and list (not atimes))
        (progn
               (mapcar (lambda (time) (keep-unquantized-statistics to time)) (butlast list 2))
               (setq prev-slur nil)
               (unless (or (= last-list to) (= last-list tmin) (not (rest list)))
                 (set-error to last-list))
               (values (test-quantize-constraints (list tmin to) tmin beat-dur prev-slur) t nil))
        (values (test-quantize-constraints (list tmin to) tmin beat-dur prev-slur) t prev-slur)))
     (t (setf (rest atimes) partition) (values atimes slur? prev-slur)))))


(defparameter *minimum-quant-dur* (/ 100 16))

(defun within-quantizing-limits (value beat-dur)
  (declare (ignore beat-dur))
  (>= value *minimum-quant-dur*))

(defun pulses-of (quant-structure) (caar quant-structure))

(defun quanti-of (quant-structure) (cdr quant-structure))

(defun deleted-of (quant-structure) (second (first quant-structure)))

(defun compound-beats (beat-list)
  (mapcar (lambda (beat) (if (null (cdr beat)) (car beat) (list 1 beat))) beat-list))

(defun form-atimes-list (times from to)
  (and times
       (if (= (first times) from)
         (if (= (first (last times)) to) times (nconc times (list to)))
         (cons from (if (= (first (last times)) to) times (nconc times (list to)))))))

(defun beat-structure (quants slur? from to)
  (let* ((atimes (form-atimes-list (copy-list (quanti-of quants)) from to))
         (durs (remove 0.0 (epw::x->dx atimes) :test #'=))
         (min-dur (/ (- (car (last atimes)) (first atimes)) (pulses-of quants)))
         (beats (and durs (remove 0
                                  (if slur?
                                    (cons (float (round (pop durs) min-dur))
                                          (mapcar (lambda (dur) (round dur min-dur)) durs))
                                    (mapcar (lambda (dur) (round dur min-dur)) durs)) :test #'=))))
     beats))

(defun search-rythm (a-section tmin prev-slur)
  "Finds the beat list structure corresponding to the quantized durations"
  (beat-structure a-section prev-slur tmin (first (last (quanti-of a-section)))))

(defun simplify (beats) (and beats (if (= (length beats) 1) (list (/ (first beats) (first beats))) beats)))  ;this should be done in "rtm" !

(defun get-rhythms (notes &key (tempo 60) (sign '(4 . 4)) (nb-pulse-max 16) start-time  old-slur? forbid)
  (declare (ignore nb-pulse-max))
  (let* ((measure-dur (* 24000.0 (car sign) (/ 1 (* (cdr sign) tempo))))
         (atimes notes)
         (beat-dur (/ measure-dur  (car sign)))
         (tmin start-time)
         (from-dur tmin) (to-dur  (+ beat-dur tmin))
         (measure-end (+ measure-dur start-time))
         (forbids (if (or (null (first forbid)) (consp (first forbid))) forbid (list forbid)))
         (max-list (if (or (null (first *max-division*)) (consp (first *max-division*))) *max-division* (list *max-division*)))
         (preci-list (if (or (null (first *distance-weight*)) (consp (first *distance-weight*))) *distance-weight* (list *distance-weight*)))
         (default-max (first (last max-list)))
         (default-preci (first (last preci-list)))
         (default-forbid (first (last forbids)))
         partition beat-rythm-forms
         (i 0))
    ;aaa
    (setf *distance-weight* (or (nth i (car preci-list)) (car (last default-preci))))
    (setf *max-division* (or (nth i (car max-list)) (car (last default-max))))
    (setf *minimum-quant-dur*
          (/ 60 tempo *max-division*))
    ;aaa
    (while (and (>= (- to-dur from-dur) *minimum-quant-dur*) atimes)
      (setq partition
            (get-list-section atimes from-dur to-dur)
            atimes (nthcdr (length partition) atimes))
      (setq *forbidden-rythmic-divisions* (or (pop forbids) default-forbid))
      (multiple-value-bind (a-section slur? prev-slur)
                           (get-optimal-time-section partition to-dur beat-dur from-dur old-slur?) ;;tmin?
        (if a-section
          (let ((chosen-rythm (simplify (search-rythm a-section from-dur prev-slur))))  ;;tmin?
            (if chosen-rythm
              (progn (push chosen-rythm beat-rythm-forms)
                     (setq tmin (car (last a-section))))
              (progn (setq beat-rythm-forms nil atimes nil)
                     (print "cannot quantize with the given constraints")
                     (pw::ed-beep))
              ))
          )
        (setq old-slur? slur?)
        (psetq to-dur (min (+ to-dur beat-dur) measure-end) from-dur to-dur))
      ;aaa
      (incf i)
      (setf *distance-weight* (or (nth i (car preci-list)) (car (last default-preci))))
      (setf *max-division* (or (nth i (car max-list)) (car (last default-max))))
      (setf *minimum-quant-dur*
            (/ 60 tempo *max-division*))
      ;aaa
      )
    (when (not atimes)
      (when old-slur? (last-division-is-silence beat-rythm-forms) (setq old-slur? nil))
      (while (>= (- measure-end to-dur) *minimum-quant-dur*) (push '(-1) beat-rythm-forms) (incf to-dur beat-dur)))
    (values (compound-beats (nreverse beat-rythm-forms)) atimes old-slur? to-dur beat-dur)))

(defun last-division-is-silence (form)
  (let ((last-beat (last (first form))))
    (setf (first last-beat) (- (floor (first last-beat))))))


(defun forbidden-structure (struct)
  (let ((division (apply '+ struct)))
    (or (> division *max-division*)
        (if (and (first *forbidden-rythmic-divisions*)
                 (symbolp (first *forbidden-rythmic-divisions*)) (string=  (first *forbidden-rythmic-divisions*) '!))
          (not (member division (rest *forbidden-rythmic-divisions*) :test #'=))
          (member division *forbidden-rythmic-divisions* :test #'=)))))

(defun get-beat-duration (tempo unit)
  (/ 24000 tempo unit))

(defun rearrange-silences (beats)
  (let (res ok new-beats)
    (labels ((loop-beats (beat-tree)
               (cond ((null beat-tree) beat-tree)
                     ((consp (car beat-tree))
                      (setq new-beats (loop-beats (cadar beat-tree)))
                      (if (and (every 'numberp new-beats) (every 'minusp new-beats))
                        (cons  (- (caar beat-tree)) (loop-beats (cdr beat-tree)))
                        (cons  (list (caar beat-tree) new-beats) (loop-beats (cdr beat-tree))))  )
                     ((every 'numberp beat-tree)
                      (setq ok nil res nil)
                      (if (every 'minusp beat-tree)
                        beat-tree
                        (dolist (item beat-tree (nreverse (if res (push (apply '+ res) ok) ok)))
                          (if (minusp item)
                            (push item res)
                            (if res (progn (push (apply '+ res) ok) (push item ok) (setq res nil))
                                (push item ok))))))
                     (t (cons (car beat-tree) (loop-beats (cdr beat-tree)))))))
      (loop-beats beats))))

;;(put-in-silences '((1 (1)) (1 (1.0 1))) '(-10 20 20))

(defun put-in-silences (beats durs &optional prev-silence)
  (if (every #'plusp durs)
    (values beats 0 nil)
    (let ((count 0) new-silence)
      (labels ((loop-beats (beat-tree)
                 (cond ((null beat-tree) beat-tree)
                       ((consp (car beat-tree))
                        (cons  (list (caar beat-tree) (loop-beats (cadar beat-tree)))
                               (loop-beats (cdr beat-tree))))
                       ((not (integerp (car beat-tree)))
                        (if (or (and (not (zerop count)) (silence? durs (1- count)))
                                (and (zerop count) prev-silence))
                          (progn (setq prev-silence t new-silence t)
                                 (cons (- (truncate (car beat-tree))) (loop-beats (cdr beat-tree))))
                          (progn (setq prev-silence nil new-silence nil)
                                 (cons (car beat-tree) (loop-beats (cdr beat-tree))))))
                       ((silence? durs count)
                        (incf count) (setq new-silence t)
                        (cons (- (car beat-tree)) (loop-beats (cdr beat-tree))))
                       (t (incf count) (setq new-silence nil)
                          (cons (car beat-tree) (loop-beats (cdr beat-tree)))))))
        (values (rearrange-silences (loop-beats  beats)) count new-silence)))))

(defun silence? (durs position) (or (not (nth position durs)) (minusp (nth position durs))))

(defvar *min-percent* 0.6)
(defvar *tempo-scalers* '(1 2 3 4 5 6 7 8))
(defvar *min-tempo* 40)
(defvar *max-tempo* 200)

(defun select-tempo (durs)
  (declare (ignore durs))
  (ui:uiwarn "Automatic tempo option is no longuer available") '(60))

(defunp quant-edit ((durs list) (tempi fix/fl/list (:value 60)) (measures list (:value '(4  4)))
                        (max/ fix/float (:value 8 :min-val 1))
                        &optional
                        (forbid (list (:value '())))
                        (offset fix/float)
                        (autom menu (:menu-box-list (("no" . 1) ("yes". 2))))) list
  " Quantizes a list of <durs> (100 = 1 sec.) into the given measure(s),
with the given <tempi>.
<max/> is the maximum unit division that is taken to be a
significant duration.
A list of forbidden <forbid> unit divisions can optionally be
specified. The output is a list of
'measure-objects' that can be entered directly into an rtm-
box."
  (let* ((tempos (if (= autom 2) (select-tempo durs) (pw::expand-lists (pw::list! tempi))))
        (measures-x (pw::expand-lists measures))
        (measures (if (consp (car measures-x)) measures-x (list measures-x)))
        (def-measure (cons (first (car (last measures))) (second (car (last measures)))))
        (durs (if (zerop offset) durs
                  (cons (- (* offset (get-beat-duration (car tempos) (second (car measures))))) durs)))
        (positive-durs (epw::ll-abs durs))
        (deftempo (car (last tempos)))
        (atimes (dx->x 0.0 positive-durs)) (c-time 0)
        result slur-fl current-tempo current-unit
        (max-list (and max/ (if (or (null (first max/)) (consp (first max/))) max/ (list max/))))
        (max-preci (and *distance-weight* (if (or (null (first *distance-weight*)) (consp (first *distance-weight*))) *distance-weight* (list *distance-weight*))))
        (forbids (and forbid (if (or (null (first forbid)) (consp (first forbid))) forbid (list forbid))))
        (*unquantized-notes* 0) (measure-number -1)
        (def-forbid (first (last forbids)))
        (def-max (first (last max/)))
        (def-preci (first (last *distance-weight*)))
        *global-grace-notes* old-silence (*max-division* max/))
    (reset-error)
    (while  atimes
      (setf *max-division* (or (nth (+ 1 measure-number) max-list) def-max))
      (setf *distance-weight* (or (nth (+ 1 measure-number) max-preci) def-preci))
      ;(setf *minimum-quant-dur*
       ;     (/ 60 (or (car tempos) deftempo) (car *max-division*)))
      (multiple-value-bind (beats times slur? current-time )
                    (get-rhythms atimes :tempo (setq current-tempo (or (pop tempos) deftempo))
                                :sign (if (car measures)
                                        (cons (first (car measures))
                                              (setq current-unit (second (pop measures))))
                                        (progn (setq current-unit (cdr def-measure)) def-measure))
                                :start-time c-time :forbid (or (nth (incf measure-number) forbids) def-forbid)
                                :old-slur? slur-fl)
        (setq atimes times c-time current-time)
        (setq slur-fl slur?)
        (if beats
          (multiple-value-bind (beats-with-silences modifs new-silence)
                               (put-in-silences beats durs old-silence)
            (setq old-silence new-silence)
            (when beats-with-silences
              (push (pw::make-measure  current-unit
                                      (mapcar (lambda (beat) (pw::make-beat 1 (list beat)))
                                              beats-with-silences)
                                      current-tempo) result)
              (setq durs (nthcdr modifs durs))))
          (setq atimes nil))))
    (unless (zerop *unquantized-notes*)
      (if result
        (format t
                "Warning: with given constraints, ~D notes were transformed into grace notes while quantizing ~%"
                 *unquantized-notes*)
        (print "cannot quantize with the given constraints"))
        (pw::ed-beep))
    (setq result (nreverse result))
    (unless (zerop *unquantized-notes*)
      (and result (setf (pw::extra-measure-stuff (car result))
                       (set-grace-notes-pos *global-grace-notes* positive-durs))))
    (make-instance 'pw::C-measure-line :measures result)))

;;;;;;;;;;;;;;;;;===============================================
(defun set-grace-notes-pos (grace-notes durs)
  (let ((atimes (epw::g-round (dx->x 0 durs) 1)) (count -1))
    (mapcar (lambda (pair) (cons (- (position (epw::g-round (first pair) 1) atimes :test #'=) (incf count))
                                   (cdr pair)))
            (sort grace-notes '< :key #'first))))
;;;;;;;;;;;;;;;================================================

(defvar *distance-function* ())

(defunp pw::quantify ((durs list (:value '(100))) (tempi fix/fl/list (:value 60)) (measures list (:value '(4  4)))
                        (max/ fix/fl/list (:value 8 :min-val 1))
                        &optional
                        (forbid (list (:value '())))
                        (offset fix/float)
                        (precis fix/fl/list (:value 0.5))) list
  "Quantizes a list of durs (100 = 1 sec.) into the given measure(s), with the
given tempi. max/   is the maximum unit division that is taken to be a significant
duration. A list of forbid   forbidden unit divisions can optionally be specified.

With this variable, you can control the subdivisions of the beats, either by
avoiding them or by imposing them, at a global level or at the beat level.

A simple list, such as ( 11 9 7 6), does not permit at a global level divisions by
11, 9, 7, or 6. The introduction of sub-lists at the first level indicates a control
over the measures. For example,  ((5 6) (7 4) () () (4 7)) indicates that in the first
measure, subdivisions by 5 and by 6 are forbidden, and in the second and fifth
measure, subdivisions by 7 and by 4 are forbidden. As the third and fourth sub-
lists are empty lists, there are no restrictions for these measures. A second
level of sub-lists will permit to control subdivisions of beats. The list (   ((5 4) ()
\(3 6) ())  (() () ( 8 7) ())  (  3 2)  ()  )  indicates :

first measure
	first beat - fourth beat : no restriction
	second beat : no restriction
	third beat : subdivisions by 3 and by 6 forbidden
	fourth beat : no restriction

second measure
	first beat - fourth beat : no restrictions
	second beat : no restrictions
	third beat : subdivisions by 8 and by 7 forbidden
	fourth beat : no restrictions

third measure
	all beats : subdivisions by 3 and by 2 forbidden

fourth measure
	all beats : no restrictions

To impose subdivisions, you add a !  at the beginning of the lists.

At a global level

\(! 5)		imposes a subdivision by five on the entire sequence
\(! 5 7 6)  	imposes a subdivision by 5, by 7, or by 6 on the entire sequence.
The module will do the necessary computations et will choose one of the
subdivisions in such a way that approximation errors are reduced.

The syntax is the same for all other levels:

For measures

\((!  3 4) (! 5) () () ())

and for time units

\(   ((! 5 4) () (!  3 6) ())  (() () ( ! 8 7) ())  (!  3 2)  ()  ) .

Of course, it is possible to mix syntaxes at the measure level as well as at the
beat level. Here is an example:

\(   (( 5 4) () (!  3 6) ())  ((! 6) () (  8 7) ())  (!  3 2)  (6 8)  ),

In this example, some measures and time units have impositions of
subdivisions, where in others, we have restrictions of subdivisions.
Warning : because the algorithm of the   quantify  ;module has been modified,
input autom   is unused. It's maintained for compatibility purposes.

The output is a list of measure-objects that can be entered directly into  the
optional Input  objs   of  the rtm module."
  (setf *distance-weight*  (if (numberp precis) (list (list precis)) precis))
  (quant-edit durs tempi measures  (if (numberp max/) (list (list max/)) max/) forbid offset 1))

(defunp quant-measures ((durs list (:value '(100))) (tempi fix/float (:value 60)) (measures list (:value '(4  4)))
                        (max/ fix/float (:value 8))
                        &optional (forbid (list (:value '())))) list
  "Quantizes a list of given durations (100 = one second) into the given measure(s),
 with the given tempo(s). Max/ is the maximum unit division which is taken to be a significant duration.
A list of forbidden unit divisions can optionally be given."
  (declare (ignore durs tempi measures max/ forbid))
  nil)


;; (progn
;; ;;;===================================================================
;; ;;;Tempo variations. Linear, for the moment...
;;
;;   (defun linear-tempo-ch (vf v0 tf t0)
;;     (lambda (d) (cuad-solve (* 0.5 (/ (- vf v0) (- tf t0))) v0 (- d))))
;;
;;   (defun cuad-solve (a b c)
;;     (/ (+ (- b) (sqrt (- (sqr b) (* 4.0 a c)))) (* 2.0 a)))
;;
;;   (defunp transf-durs ((durs list) (b-tempo fix/float (:value 60))
;;                        (e-tempo fix/float (:value 120))) list
;;       "transforms durations according to a continuous linear tempo change between the given
;; initial ('b-tempo') and final ('e-tempo') tempi. "
;;     (let ((tattack (dx->x 0 durs))
;;           (final-tempo (/ e-tempo b-tempo 100))
;;           (start-tempo (/ 100)))
;;       (epw::ll/round
;;        (epw::l/ (epw::x->dx
;;                  (mapcar (linear-tempo-ch final-tempo start-tempo
;;                                           (/ (car (last tattack)) (* 0.5 (+ final-tempo start-tempo))) 0)
;;                          tattack)) 100) 1)))
;;
;;   ;;(pw-addmenu pw::*rtm-boxes-menu* '(transf-durs))
;;
;;   ;;(transf-durs '(100 100 100 100 100 100 100 100 100) 120 60)
;;   ;;(transf-durs '(100) 120 60)
;;
;;   (defunp mul&subm ((numbers numbers? (:value 100)) (%err fix/float )
;;                     (unit fix>=0 (:value 100))) numbers?
;;       "reorganizes numbers"
;;     (let ((numbers (pw::list! numbers)) result div)
;;       (dolist (num numbers (nreverse result))
;;         (setq div (/ unit num))
;;         (push
;;          (if (> div 1.0)
;;              (get-adjustement (- (/ unit (truncate div)) num)
;;                               (-  num (/ unit (1+ (truncate div)))) num %err)
;;              (get-adjustement (- (* unit (1+ (truncate (/ div))))  num)
;;                               (-  num (* unit (truncate (/ div)))) num %err))
;;          result)
;;         )))
;;
;;   (defun get-adjustement (plus minus num %err)
;;     (let ((zone (* num %err)))
;;       (cond ((and (<= plus zone) (> minus zone)) (round (+ plus num)))
;;             ((and (<= minus zone) (> plus zone)) (round (- num minus)))
;;             ((and (> plus zone) (> minus zone)) num)
;;             ((< plus minus) (round (+ plus num)))
;;             (t (round (- num minus))))))
;;
;;   ;;(pw-addmenu pw::*pw-menu-patch* '(mul&subm))
;;   )

;;;; THE END ;;;;


