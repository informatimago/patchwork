;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               conversion.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;
;;;;    PW-Music Conversion boxes
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
(in-package "EPW")

;; ==== Pitch approximation ====

;; midics? : a midic or a list of midics
;; approx  : 2=1/2 4=1/4
;; output  : a midic or a list of midics approximated

(defun approx-m1 (midic approx &optional (ref-midic 0))
  "Approximates <midic> to the closest. (2=semi-tone 4=quarter-tone).
The optional argument <ref-midic> is a reference (always approximated to itself),
allowing to get, for example, any of the two whole-tone scales (with <approx>=1)."
  (if (<= approx 0)
    midic
    ;; [jack] 910617 I must use floor instead of round to avoid
    ;; (approx-m '(6050 6150 6250 6350) 2) => (6000 6200 6200 6400) !
    (+ ref-midic
       (round (* (floor (+ (* (- midic ref-midic) approx) 100) 200) 200) approx) )))

(defunp approx-m ((midics? midics?)
                  (approx approx (:value 2)) &optional (ref-midic fix)) midics?
  "approx-m takes a midicent value   midicents ;and returns an approximation
to the nearest division of the octave as defined by the user, approx. The value
of resolution determines the resolution of approximation. An argument of 1,
results in an output where all values are  rounded to the nearest whole tone; 2,
to the nearest semitone; 4, to the nearest  quartertone; 4.5, to the nearest 4.5th
of a tone, etc. When approx = 1, the optional argument ref-m in midicents
specifies the frequency resolution of the approximation. A value of 100 specifies
semitone resolution, 50 specifies quartertone resolution, and so on.
"
  (deep-mapcar 'approx-m 'approx-m1 midics? approx ref-midic))

;;(defunt approx-m1 ((midic midic) (approx approx)) midic)
;;(defunt approx-m ((midics? midics?) (approx approx)) midics?)

;; ==== Pitch conversions ====

(defvar *diapason-freq* 440.0)
(defvar *diapason-midic* 6900)

;; ---- midic -> frequency ----

(defunp mc->f1 ((midic midic)) freq
  "Converts a midicent pitch to a frequency (Hz)."
  (* *diapason-freq*
     (expt 2.0 (/ (- midic *diapason-midic*) 1200.0)) ))

(defunp mc->f ((midics? midics?)) freqs?
  "Converts a midi-cent pitches <midics>  to frequencies (Hz)."
  (deep-mapcar 'mc->f 'mc->f1 midics?))

;; ---- absolute value of a frequency ----

#|(defvar *lowest-freq*
  ;(progn (setq f 1.0) (while t (f->mc1 (setq f (/ f 2.0)))))
  (* 256 (max (mc->f1 most-negative-fixnum) least-positive-short-float)))|#

(defvar *lowest-freq*
  ;(progn (setq f 1.0) (while t (f->mc1 (setq f (/ f 2.0)))))
  (* 256 (max (mc->f1 most-negative-fixnum) least-positive-long-float)))

(defun abs-f1 (freq)
  (max *lowest-freq* (abs freq)))

;; ---- frequency -> midic ----

(defun f->mf (freq)
  "Converts <freq> (Hz) to a float midicent pitch."
  (+ (* (log (abs-f1 (/ freq *diapason-freq*))) #.(/ 1200 (log 2.0)))
     *diapason-midic*))

(defunp f->mc1 ((freq freq) &optional (approx (fix/float (:value 100)))
                                      (ref-midic fix/float)) midic
  "Converts <freq> (Hz) to a midicent pitch."
  (approx-m1 (f->mf freq) approx ref-midic))

(defunp f->mc ((freqs? freqs?) &optional (approx (fix/float (:value 100)))
                                         (ref-midic fix/float)) midics?
  "Converts   frequency ;to midicents.  It takes a frequency (Hz) or list of
frequencies and returns corresponding midicent values. The optional approx
argument lets one limit returned values to a given approximation (see  approx-
m).  When approx = 1, the optional argument ref-m in midicents specifies the
frequency resolution of the approximation. A value of 100 specifies   semitone
;;resolution, 50 specifies   quartertone ;resolution, and so on."
  (deep-mapcar 'f->mc 'f->mc1 freqs? approx ref-midic))

;;(defunt f->mc1 ((freq freq)) midic)
;;(defunt f->mc ((freqs? freqs?)) midics?)

;; ---- midic -> symbol ----



(defparameter *ascii-note-C-scale*
  (mapc (lambda (x) (setf (car x) (string-upcase (string (car x)))))
        '((C) (C . :q) (C . :s) (D . :-q)
          (D) (D . :q) (E . :f) (E . :-q)
          (E) (E . :q)
          (F) (F . :q) (F . :s) (G . :-q)
          (G) (G . :q) (G . :s) (A . :-q)
          (A) (A . :q) (B . :f) (B . :-q)
          (B) (B . :q)  )))

(defparameter *ascii-note-do-scale*
  (mapc (lambda (x) (setf (car x) (string-downcase (string (car x)))))
        '((do) (do . :q) (do . :s) (re . :-q)
          (re) (re . :q) (mi . :f) (mi . :-q)
          (mi) (mi . :q)
          (fa) (fa . :q) (fa . :s) (sol . :-q)
          (sol)(sol . :q)(sol . :s)(la . :-q)
          (la) (la . :q) (si . :f) (si . :-q)
          (si) (si . :q)  )))

(defparameter *ascii-note-alterations*
  '((:s "#" +100) (:f "b" -100)
    (:q "+" +50) (:qs "#+" +150) (:-q "_" -50) (:f-q "b-" -150)
    (:s "d" +100)))

(defparameter *ascii-note-scales* (list *ascii-note-C-scale* *ascii-note-do-scale*)
  "The scales used by the functions mc->n and n->mc.")

(defun mc->n1 (midic &optional (ascii-note-scale (car *ascii-note-scales*)))
  "Converts <midic> to a string representing a symbolic ascii note."
  (let ((dmidic (/ 1200 (length ascii-note-scale))) note)
    (multiple-value-bind (midic/50 cents) (round midic dmidic)
      (multiple-value-bind (oct+2 midic<1200) (floor (* midic/50 dmidic) 1200)
        (setq note (nth (/ midic<1200 dmidic) ascii-note-scale))
        (format nil "~A~A~A~A~A"
          (car note) (or (car (cassq (cdr note) *ascii-note-alterations*)) "")
          (- oct+2 2) (if (> cents 0) "+" "") (if (zerop cents) "" cents) )))))

;; (mc->n1 6000)
;; (mc->n1 6000 *ascii-note-do-scale*)

(defun n->mc1 (str &optional (*ascii-note-scale* (car *ascii-note-scales*)))
  "Converts a string representing a symbolic ascii note to a midic."
  (setq str (string str))
  (let ((note (some (lambda (note)
                        (when (and (null (cdr note))
                                   (eql 0 (search (car note) str :test #'string-equal)))
                          note)) *ascii-note-scale*))
        index midic alt)
    (unless note (error "Note not found in ~S using the ~S ~%~S"
                        str '*ascii-note-scale* *ascii-note-scale*))
    (setq midic (* (position note *ascii-note-scale*)
                   (/ 1200 (length *ascii-note-scale*))))
    ;; at this point: "C" -> 0 ; "D" -> 100 ; "E" -> 200 ; etc.
    (setq index (length (car note)))
    ;; alteration
    (when (setq alt (some (lambda (alt)
                              (when (eql index (search (cadr alt) str :start2 index
                                                       :test #'string-equal))
                                alt)) *ascii-note-alterations*))
      (incf midic (third alt))
      (incf index (length (second alt))))
    ;; octave
    (multiple-value-bind (oct i) (parse-integer str :start index :junk-allowed t)
      (incf midic (* (+ oct 2) 1200))
      (setq index i))
    (unless (= index (length str))
      (incf midic (parse-integer str :start index)))
    midic))


(defparameter *ascii-intervals*
  '("1" "2m" "2M" "3m" "3M" "4" "4A" "5" "6m" "6M" "7m" "7M"))

(defun int->symb1 (int)
  "Converts a midic interval to a symbolic interval."
  (multiple-value-bind (oct cents) (floor int 1200)
    (let ((index (/ cents 100)))
      (unless (typep index 'fixnum) (error "Not yet implemented"))
      (if (zerop oct)
        (nth index *ascii-intervals*)
        (format () "~A~@D" (nth index *ascii-intervals*) oct)))))

;;(defunt int->symb1 ((int fix)) string)

;; called "itv->ascii" by CR
(defunp int->symb ((ints fixs)) list
  "<int->symb> takes an interval expressed in midi-cents, and returns a
symbolic interval name.
Intervals are labeled as follows:

        1 = unison              2m = minor second
        2M = major second       3m = minor third
        3M = major third        4 = perfect fourth
        4A = tritone            5 = perfect fifth
        6m = minor sixth        6M = major sixth
        7m = minor seventh      7M = major seventh

All intervals larger than an octave are expressed by adding or  subtracting an
octave displacement after the simple interval name;
 for example, a major tenth becomes 3M+1, etc.  Note: for the time being,  the
program has a strange way of expressing downward intervals:
 it labels the interval as its inversion, and then transposes downwards as
 necessary.  Thus, a major third down (-400 in midicents), returns 6m-1."
  (deep-mapcar #'int->symb #'int->symb1 ints))

(defun symb->int1 (int)
  (let* ((int-str (coerce (string int) 'list))
         (neg-oct (member #\- int-str :test #'char=))
         (rest-oct (or (member #\+ int-str :test #'char=) neg-oct))
         (oct (if rest-oct
                (read-from-string (coerce (cdr rest-oct) 'string))
                0))
         (pclass (coerce (butlast int-str (length rest-oct)) 'string)))
    (* 100  (+ (position pclass *ascii-intervals* :test #'string=)
               (* 12 (if neg-oct (- oct) oct))))))

(defunp symb->int ((ints fixs)) list
  "<symb->int> takes a symbolic interval name  , and returns an interval
expressed in midi-cents. Intervals are labeled as follows:

        1 = unison                      2m = minor second
        2M = major second       3m = minor third
        3M = major third                4 = perfect fourth
        4A = tritone            5 = perfect fifth
        6m = minor sixth                6M = major sixth
        7m = minor seventh      7M = major seventh

All intervals larger than an octave are expressed by adding or subtracting an
octave displacement after the simple interval name;
 for example, a major tenth becomes 3M+1, etc.  Note: for the time being,
Patchwork has a strange way of expressing downward intervals:  it labels the
interval as its inversion, and then transposes downwards as necessary. Thus, a
major third down 6m-1, returns -400 in midicents ."
  (deep-mapcar #'symb->int #'symb->int1 ints))

;;(defunt mc->n1 ((midic midic)) string)

(defunp mc->n ((midics? midics?)) strings?
  "mc->n takes a midi-cent value <midics> or list of midi-cent values,
 and returns corresponding symbolic (ASCII) note names.
 Symbolic note names follow standard notation with middle c
\(midi-cent 6000) being C3.  Semitones are labeled with a '#' or a 'b.'
Quartertone flats are labeled with a '_', and quartertone sharps with a '+'.  Thus,
C3 a quartertone sharp (midi-cent 6050), would be labeled 'C+3'.  Gradations
smaller than a quartertone are expressed as the closest  quartertone + or - the
remaining cent value (i.e., midi-cent 8176 would be expressed as Bb4-24)."
  (deep-mapcar 'mc->n 'mc->n1 midics?))

;;(defunt n->mc1 ((str string)) midic)

(defunp n->mc ((strs list)) midics?
  "<n->mc> takes a symbolic <strs> note name or list of note names,
 and returns corresponding midi-cent values. Symbolic note names
 follow standard notation with middle C (midi-cent 6000) being C3.
  Semi-tones are labeled with a '#' or 'b.'  Quartertone flats are
 labeled with a '_', and quartertone sharps with a '+'.  Thus, C3 a
 quartertone sharp (midi-cent 6050), would be labeled 'C+3'.  Gradations
 smaller than a quartertone are expressed as the closest quartertone + or - the
remaining cent value (i.e., mid-cent 8176 would be expressed as Bb4-24)."
  (deep-mapcar 'n->mc 'n->mc1 strs))

;; (equal (mc->n1 6940) "A+3-10")
;; (equal (mc->n1 6001) "C3+1")

;; (n->mc1 "do+3-10")
;; (n->mc1 "a#-3-10")
;; (n->mc1 "C3+1")
;; (n->mc1 "C3")

(defunp cents->coef ((nb-cents midic)) float
  "<cents->coef> takes an interval expressed in midi-cents and returns the ratio
between two frequencies separated by that interval; i.e., the value: (freq + <nb-
cents>) / freq."
  (expt 2.0 (/ nb-cents 1200.0)))

;; (cents->coef 1200)  => 2
;; (cents->coef  200)  => 1.122462048309373
;; (cents->coef  100)  => 1.059463094359295
;; (cents->coef   50)  => 1.029302236643492

(defunp coef->cents ((coef float)) midic
  "<coef->cents> takes a frequency ratio <coef> f1/f2 and returns the interval,
expressed in midi-cents, between f1 and f2."
  (round (log coef) #.(/ (log 2) 1200)))

;; (coef->cents 2)                 => 1200
;; (coef->cents 1.122462048309373) =>  200
;; (coef->cents 1.059463094359295) =>  100
;; (coef->cents 1.029302236643492) =>   50

(defunp nbcents-f ((f-lo freq) (f-hi freq)) midic
  "Returns the interval in cents from <f-lo> to <f-hi>."
  (coef->cents (/ f-hi f-lo)))

;;; Duplicates with /home/pjb/works/patchwork/patchwork/src/pw-lib/epw-1.0b/freq-harmony.lisp ?
;; (defun sec->min1 (sec &optional (nbdec 2) (format 1))
;;   (let ((min (truncate sec 60)))
;;     (if (and (> format 0 )(= 0 min))
;;         (list (lldecimals sec nbdec))
;;         (list min 'min (lldecimals (mod sec 60) nbdec)))))
;;
;; (defune sec->min ((lsec numbers?) (format fix)) list
;; "conversion secondes en minutes+secondes
;; Format 0 : normal (ex: 1 min 15 ,  0 min 32)
;; Format 1 : 0 min n'est pas noté (ex: 1 min 15 , 32)"
;;   (deep-mapcar/1  'sec->min1 lsec 2 format))
;;
;; (defune min->sec ((minutes numbers?)) numbers? ;;faire marcher pour atoms
;; "conversion  minutes -> secondes
;; orthographe : (3 x 25.3) ,  ou (3 25.3), ou (25.3), ou 25.3
;; x étant un caractère quelconque"
;;   (less-deep-mapcar  'min->sec1 (list! minutes)))
;;
;; (defun min->sec1 (minutage)
;;  (setq minutage (carlist! minutage))
;;   (let ((sec (if (atom minutage) minutage (l-last minutage)))
;;         (minutes (if (atom minutage) 0 (car minutage))))
;;     (lldecimals (+  sec (* minutes 60)) 2)))

(defun microton1  (midics approx)
  (let ((result nil))
    (while midics (push (+ 1 (/ (mod (approx-m (nextl midics) approx) 100) 25)) result))
    (nreverse result)))

(defunp microtone  ((midics midic) (approx fix (:value 4))) list
"microtone  returns a list of microinterval numbers given a list of midi-cent
values <midics>. The micro-interval numbers  and their corresponding
microinterval value, in <approx>, is given below:
1 = no micro-interval.
2 = eighth tone
3 = quarter tone
4 = three eighth tones

For example, the call (microtone '(6025 6200 6347 6750 6176) 8)
returns the list (2 1 3 3 4)."
  (unless (member approx '(2 4 8)) (setq approx 2))
  (less-deep-mapcar 'microton1 (list! midics) approx))

#|(defun lin->db1 (amp)
  (lldecimals (- (* 3.6322497830622554E9 (expt amp 2.391325004739499E-9))
 3.6322497830622554E9 ) 2))|#

(defun lin->db1 (amp)
  (if (zerop amp) -3.63224978306E9
      (lldecimals (* 20.0 (log amp 10)) 2)))

(defunp lin->db ((amps fix/fl/list (:value 1.0))) list
"<lin->db> takes a  number <amps> and returns the corresponding value
expressed in decibels. The input can be a list of numbers. In this case a list of
db values is returned."
  (deep-mapcar/1 'lin->db1 amps))

(defun epw::dB->lin1 (amp) (expt 10.0 (/ amp 20.0)))
(defunp epw::db->lin ((amps fix/fl/list (:value 0.0))) list
        "<dB->lin> takes a  number <amps> in decibels and converts it
to linear. The input can be a list of numbers. In this case a list of
linear values is returned."
  (epw::deep-mapcar/1  'epw::db->lin1 amps))

;; (defvar *midic* -6000)
;; (for (midic *midic* 10 20000)
;;   (unless (= (setf *midic* midic) (n->mc1 (mc->n1 midic)))
;;     (error "Bad midic ~S" midic)))

;;;; THE END ;;;;
