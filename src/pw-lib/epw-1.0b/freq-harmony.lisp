;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               freq-harmony.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;
;;;;    ESQUISSE PATCHWORK
;;;;
;;;;AUTHORS
;;;;    Mikael Laurson, Jacques Duthen, Camilo Rueda.
;;;;    Contributions by Tristan Murail
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-07 <PJB> Changed license to GPL3; Added this header.
;;;;    1992-12-00 <Tristan Murail> révision finale
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


;;=============================================================
;;===================== freq harmony ==========================
;;=============================================================


;; ==================== série harm  ===========================

(defun rankcalc (numer denom begin end )
  (let ((cdeb (mod begin denom)) (pas (if (< end begin) -1 1)) res)
    (for (n begin pas end)
      (if (not (>= (mod (- n cdeb) denom) numer)) (push n res)) )
    (nreverse (remove 0 (if (and (not (null (find 1 res))) (not (null (find -1 res))))
                            (remove -1 res) res)))))

(defun hrmcalc1 ( listharm fond)
  (let (res)
    (dolist ( n listharm)
      (cond ( ( > n 0 ) (push (* fond  n ) res))
            ( ( < n 0 ) (push (/ fond (abs n))  res))
            ( t () ) ))
    (nreverse  res)))

(defun hrmcalc (fond listharm )
  (less-deep-mapcar #'hrmcalc1 listharm fond))

(defun sercalc (fund  numer denom begin end )
  (hrmcalc fund (rankcalc numer denom begin end) ))


(defunp harm-series ((fund midics? (:value 3600)) (numer fix>0s? (:value 1))
                     (denom fix>0s? (:value 1)) (begin fix (:value 1))
                     (end fix (:value 7))
                     &optional (unit menu (:menu-box-list (("midic" . 1) ("freq". 2))))
                     (type menu (:menu-box-list (("seq" . 1) ("chord". 2)))))
    list
    "Builds the harmonic and/or sub-harmonic series starting with the
fundamental <fund>. The harmonic series is the series of positive integer
multiples of the fundamental frequency. The sub-harmonic series is the
series of positive integer divisions of the fundamental frequency.

The arguments <numer> and <denom> determine what sample (<numer>/<denom>)
of the partials is taken. (e.g. 1/1 = all; 1/2 = every other; 2/5 = the
first two of each group of five)

The arguments <begin> and <end> determine the lowest and highest partials
generated. The fundamental is represented by '1' or '-1' sub-harmonics are
represented by negative numbers, overtones by positive. (e.g. partial
number 7 is 7 times the fundamental frequency,partial -7 is the
fundamental frequency divided by 7; thus to go from the seventh undertone
to the seventh overtone <begin> would equal '-7' and <end> would equal
'7')

The optional argument <unit> determines whether the <fund> is entered in
midicents, ('midic'), or in hertz ('freq'). If 'midic' is selected the
value will be converted to frequency inside the function and then the
output is reconverted to midicents. If 'freq' is selected the entry,
calculation and output are all in hertz.

When <fund> is a list, the optional argument <type> is used to determine
the format of the output. The value 'seq' returns a list of chords
representing the partials requested for each successive fundamental. The
value 'chord' returns a single chord containing all the partials of all
the fundamentals."
  (let* ((fund (if (= unit 2) fund (mc->f  fund)))
         (res (car-mapcar 'sercalc  fund  numer denom begin end)))
    (setq res (if (= unit 2) res (f->mc res) ))
    (if (= type 2) (flat res) res)))



(defunp nth-harm ((fund midics?(:value 3600)) (nth fix>0s? (:value 1))
                  &optional (unit menu (:menu-box-list (("midic" . 1) ("freq". 2))))
                  (type menu (:menu-box-list (("seq" . 1) ("chord". 2))))) midics?
    "Receives a fundamental <fund>, or list of fundamentals and returns
the <nth> harmonic or sub-harmonic of each fundamental. The harmonic
series is the series of positive integer multiples of the fundamental
frequency. The sub-harmonic series is the series of positive integer
divisions of the fundamental frequency. Partial numbers are determined
by their relationship to the fundamental. (e.g. partial number 7 is 7
times the fundamental frequency,partial -7 is the fundamental frequency
divided by 7)

If <nth> is a list, a corresponding series of partials will be returned
for each <fund>. If <nth> contains non-integers the returned partials
and/or sub-partials will be non-harmonic, and correspond to the
fundamental frequency multiplied by <nth> (when <nth> is positive) or
the fundamental frequency divided by the absolute value of <nth>
\(when <nth> is negative).

The optional argument <unit> determines whether the <fund> is entered in
midicents, ('midic'), or in hertz ('freq'). If 'midic' is selected the
value will be converted to frequency inside the function and then the
output is reconverted to midicents. If 'freq' is selected the entry,
calculation and output are all in hertz.

When <fund> is a list, the optional argument <type> is used to determine
the format of the output. The value 'seq' returns a list of chords
representing the partials requested for each successive fundamental. The
value 'chord' returns a single chord containing all the partials of all
the fundamentals."
  (let* ((nth (list! nth)) (fund (list! fund))
         (listfund  (not (one-elem fund)))
         (listnth (not (atom (car nth))))
         (listchord (not (atom (car fund))))
         (fund (if (= unit 2)  fund   (mc->f fund)))
         res)
    (cond  (listchord (setq res (seqfund-nth fund nth type listnth)))
           ((and listfund listnth) (setq res (doublenth fund nth type)))
           ((not listfund)  (setq res (flat-once (simplenth fund nth type))))
           (t (setq res (simplenth fund nth type))))
    (if (one-elem res) (setq res (first res)))
    (if (= unit 2) res (f->mc res))))


(defun simplenth (fund nth type)
  (let ((res (deep-mapcar/1  'hrmcalc  fund  nth)))
    (if (= type 2) (flat-once res) res)))


(defun doublenth (fund nth type)
  (if (= type 1) (flat-once (simplenth fund nth type))
      (mapcar (lambda (x) (flat (simplenth fund x type))) nth)))

(defun seqfund-nth (fund nth type listnth)
  (cond ((not listnth) (mapcar (lambda (x) (simplenth x nth 2)) fund))
        ((= type 1) (flat-once (mapcar (lambda (x) (doublenth x nth  2)) fund)))
        ((= type 2)  (mapcar (lambda (x) (flat (simplenth x nth type))) fund) )))


;; anciens noms, pour compatibilité

(defunp overtones ((fund midics? (:value 3600)) (numer fix>0s? (:value 1))
                   (denom fix>0s? (:value 1)) (begin fix (:value 1))
                   (end fix (:value 7))
                   &optional (unit menu (:menu-box-list (("midic" . 1) ("freq". 2))))
                   (type menu (:menu-box-list (("seq" . 1) ("chord". 2)))))
    list
    ""
  (harm-series fund numer denom begin end unit type))

(defunp nth-overtones ((fund midics?(:value 3600)) (nth fix>0s? (:value 1))
                       &optional (unit menu (:menu-box-list (("midic" . 1) ("freq". 2))))
                       (type menu (:menu-box-list (("seq" . 1) ("chord". 2))))) midics?
    ""
  (nth-harm fund nth unit type))

;; ==================== frequency shifting  ======================

;; (defun fshift1 (freq dfreq)
;;   "adds an interval (in Hz) to a freq"
;;   (g+   freq dfreq ))

(defunp fshift ((chord midic) (dfreq fix/float)
                &optional (unit menu (:menu-box-list (("midic" . 1) ("freq". 2))))
                (type menu (:menu-box-list (("seq" . 1) ("chord". 2))))
                (output menu (:menu-box-list (("exclus" . 1) ("inclus". 2)))))  midics?
    "Shifts the frequency of each note of <chord> by a frequency <dfreq>
\(positive or negative, but always in hertz).

The optional argument <unit> determines whether <chord> is entered in
midicents, ('midic'), or in hertz ('freq'). If 'midic' is selected the
values will be converted to frequencies inside the function and then the
output is reconverted to midicents. If 'freq' is selected the entry,
calculations and output are all in hertz.

If <chord> is a list of chords the optional argument <type> is used to
determine whether the output will be a list of chords ('seq'), each one
shifted by <dfreq> or a single chord combining the notes of all the
shifted chords ('chord'). If <dfreq> is a list the same argument is used
to choose between a list of chords shifted by each successive <dfreq> or a
single chord combining the different distortions. If both <chord> and
<dfreq> are lists the position 'seq' will return a list of chords
containing  each chord shifted by each frequency; the position 'chord'
will return a list of chords containing each chord shifted by all the
listed frequencies.

The optional argument <output> determines whether the original <chord> is
included ('inclu') or excluded ('exclu') from the output list."
  (let ((listchord  (not (atom (car chord))))
        (listfreq (not (atom  dfreq)))
        (fchord (if (= unit 2)  chord   (mc->f chord)))    res)
    (cond  ((and listchord listfreq)
            (setq res (doubleshift fchord dfreq type output)))
           (listchord (setq res (lcshift fchord dfreq type output)))
           (listfreq  (setq res (lfshift fchord dfreq type output)))
           (t (setq res (simpleshift fchord dfreq output))))
    (if (= unit 2) res (f->mc res))))


(defun simpleshift (fchord dfreq output)
  (let ((res (g+ dfreq  fchord )))
    (if (= output 2) (x-append fchord res) res)))

(defun lcshift (fchord dfreq type output)
  (let ((res (deep-mapcar/1 #'g+ dfreq  fchord )))
    (if (= output 2) (setq res (mapcar #'x-append fchord res)) res)
    (if (= type 2) (flat-once res) res)))

(defun lfshift (fchord dfreq type output)
  (let ((res (deep-mapcar/1 #'g+ dfreq  fchord )))
    (cond ((and (= output 2) (= type 1))
           (mapcar (lambda (x) (x-append x fchord)) res))
          ((and (= output 2) (= type 2))
           (x-append fchord (flat-once res)))
          (t (if (= type 2) (flat-once res) res)))))


(defun doubleshift (fchord dfreq type output)
  (cond ((= type 2)
         (mapcar (lambda (x) (lfshift x dfreq type output)) fchord))
        ((and (= output 1) (= type 1))
         (flat-once (mapcar (lambda (x) (lfshift fchord x type output)) dfreq)))
        ((and (= output 2) (= type 1))
         (flat-once
          (mapcar (lambda (x) (lcshift fchord x type output)) dfreq)))))



(defunp fshift-proc ((chord midic) (dfreq fix/float) (steps fix>0)
                     &optional (unit menu (:menu-box-list (("midic" . 1) ("freq". 2))))
                     (output menu (:menu-box-list (("inclus" . 1) ("exclus". 2))))) midics?
    "Progressively shifts <chord> until the final chord which is shifted by
<dfreq> (positive or negative, but always in hertz). The argument <steps>
determines the number of intermediate distortions to be produced between
the unaltered <chord> and the chord shifted by <dfreq>.

The argument <chord> may be a list, in which case the same process of
shifting is carried out for each successive chord.

<dfreq> and <steps> may not be lists.

The optional argument <unit> determines whether <chord> is entered in
midicents, ('midic'), or in hertz ('freq'). If 'midic' is selected the
values will be converted to frequencies inside the function and then the
output is reconverted to midicents. If 'freq' is selected the entry,
calculations and output are all in hertz.

The optional argument <output> determines whether the non-shifted <chord>
is included ('inclu') or excluded ('exclu') from the output list of
chords."
  (let* ((dfreq (car! dfreq)) (steps (/ dfreq (car! steps)) )
         (deb (if (/= output 2) 0 steps  )) res)
    (for (n deb steps dfreq ) (newl res (fshift chord n unit 1 1)))
    (setq res (nreverse res))
    (if  (not (atom (car chord))) (flat-once res) res)))


;; ========== distorsion des fréquences ==============================================


(defunp fdistor ((chord midic) (minout midic (:value 5700)) (maxout midic (:value 6300))
                 &optional (minin list (:value '() ))
                 (maxin list (:value '() ))
                 (unit menu (:menu-box-list (("midic" . 1) ("freq". 2))))
                 (output menu (:menu-box-list (("exclus" . 1) ("inclus". 2))))) midics?
    "Distorts the frequencies of <chord> so that the lowest note is changed to
<minout> and the highest note to <maxout>. Interior notes are rescaled so
as to preserve the relative positions of their frequencies.

The optional inputs <minin> and <maxin> allow the scaling to be done
relative to two selected reference notes rather than the highest and
lowest notes of the chord. The note entered as <minin> will be moved to
<minout>, and <maxin> to <maxout> the rest of the chord is then
rescaled accordingly.

If <chord> is a list of chords, output will be a corresponding list of
distorted chords.

The optional argument <unit> determines whether <chord> is entered in
midicents, ('midic'), or in hertz ('freq'). If 'midic' is selected the
values will be converted to frequencies inside the function and then the
output is reconverted to midicents. If 'freq' is selected the entry,
calculations and output are all in hertz.

The optional argument <output> determines whether the non-distorted
<chord> is included ('inclu') or excluded ('exclu') from the output list.
If included the non-distorted notes will be mixed with the distorted into
a single chord."
  (if (/= unit 2) (setq chord (mc->f chord) minout (mc->f minout) maxout (mc->f maxout)
                        minin (if (not (null minin )) (mc->f minin))
                        maxin (if (not (null maxin )) (mc->f maxin))))
  (setq minin (if (null minin) most-negative-fixnum (car! minin))
        maxin (if (null maxin) most-positive-fixnum (car! maxin)))
  (let ((res (g-scaling chord (car! minout) (car! maxout) minin maxin)))
    (setq res (if (= output 2) (if (atom (car chord)) (x-append chord res)
                                   (mapcar #'x-append chord res)) res))
    (if (= unit 2) res (f->mc res))))

(defunp fdistor-proc ((chord midic) (steps fix>0)
                      (minout midic (:value 5700)) (maxout midic (:value 6300))
                      &optional (minin list (:value '() ))
                      (maxin list (:value '() ))
                      (unit menu (:menu-box-list (("midic" . 1) ("freq". 2))))
                      (output menu (:menu-box-list (("inclus" . 1) ("exclus". 2))))) midics?
    "Progressively distorts <chord> until the distortion specified by <minout>
and <maxout> is reached. The argument <steps> determines the number of
intermediate distortions to be produced between the unaltered <chord> and
the final distortion. (For explanation of frequency distortion, as well as
the use of <minout>,<maxout>,<minin> and <maxin> see the box 'fdistor')

<chord> may not be a list of chords.

The optional argument <unit> determines whether <chord> is entered in
midicents, ('midic'), or in hertz ('freq'). If 'midic' is selected the
values will be converted to frequencies inside the function and then the
output is reconverted to midicents. If 'freq' is selected the entry,
calculations and output are all in hertz.

The optional argument <output> determines whether the non-distorted
<chord> is included ('inclu') or excluded ('exclu') from the output list
of chords."
  (let*  (
          (valmin (if (null minin) (l-min (list! chord)) (car! minin)))
          (valmax (if (null maxin) (l-max (list! chord)) (car! maxin)))
          (stepmin (if (/= steps 0) (/ (- (car! minout) valmin) steps) 0))
          (stepmax (if (/= steps 0) (/ (- (car! maxout) valmax) steps) 0))
          (deb (if (/= output 2) 0 1 )) res)
    (for (n deb 1 steps)
      (newl res (fdistor chord (+ valmin (* n stepmin)) (+ valmax (* n stepmax))
                         (car! minin) (car! maxin) unit 1)))
    (nreverse res)))


;; (defun fdist1 (freq minin maxin minout maxout )
;;   (+ minout (/ (* (- freq minin) (- maxout minout)) (- maxin minin))))
;;
;; (defun fdist? (freqs?  minin maxin minout maxout)
;;   (deep-mapcar 'fdist? 'fdist1 freqs?
;;                (car! minin) (car! maxin) (car! minout) (car! maxout)))
;;
;; (defunp fdistor ((midics? midics?) (minin midic (:value 5900)) (maxin midic)
;;                  (minout midic (:value 5700)) (maxout midic (:value 5750))) midics?
;;     "Frequential distorsion of the chord <midics?> :
;; <minin> and <maxin> are changed into <minout> and <maxout>, all other pitches
;; are changed proportionally "
;;   (f->mc (fdist? (mc->f midics?) (mc->f minin) (mc->f maxin) (mc->f minout) (mc->f maxout))))
;;
;; (defunp fdistor-simple ((midics? midics?) (minout midic) (maxout midic)) midics?
;;     "Similar to fdistor. The reference notes are necessarily the lowest and the highest
;; pitches of the chord.>"
;;   (fdistor midics? (l-min (list! midics?)) (l-max (list! midics?)) minout maxout))
;;
;; (defunp fdist-simp-proc ((midics? list (:value "(5900 6000)")) (minout midic (:value 5900))
;;                          (maxout midic) (steps fix>0)) midics?
;;     "Process of distorsion : <minout> and <maxout> are considered the target pitches
;; for the lowest and highest pitch of the chord <midics?> to be reached in <steps>
;; steps. All other pitches are distorted proportionally."
;;   (let ((res) (deb (l-min (list! midics?))) (fin (l-max (list! midics?))))
;;     (for (n 0 1 steps)
;;       (newl res (fdistor-simple midics? (+ deb (* n (/ (- (car! minout) deb) steps)))
;;                                 (+ fin (* n (/ (- (car! maxout) fin) steps))))))
;;     (nreverse res) ))

;; ==== central frequency ====

(defunp midi-center2 ((midic1 midic) (midic2 midic)) midic
    "central midic between <midic1> and <midic2>"
  (round (+ (car! midic2) (car! midic1)) 2) )

(defun midi-center-1 (chord)
  (round (+  (l-min chord) (l-max chord)) 2))

(defunp midi-center ((chord midic)) midic
    "Calculates the value in midicents exactly halfway between the lowest
and highest notes of the chord <chord>.

If <chord> is a list of chords the output is a list of the central
values."
  (less-deep-mapcar  'midi-center-1  chord)  )

;; fonctions supprimées du menu ------------------------------------
(defunp freq-center2 ((freq1 freq) (freq2 freq)) freq
    "central freq between <freq1> and <freq2>"
  (sqrt (* (car! freq2) (car! freq1))) )

(defunp freq-center ((freqs freq)) freq
    "central freq between the lowest and highest notes of the chord <freqs>"
  (freq-center2 (l-min (list! freqs)) (l-max (list! freqs))))
;;------------------------------------------------------------------

(defunp best-freq ((chord midic)
                   &optional (unit menu (:menu-box-list (("midic" . 1) ("freq". 2))))) numbers?
    "Returns the note which is the minimum possible distance from the
frequencies of all the notes of <chord>. (minimum sum of the squares of
the distances) This note can be thought of as a sort of center of gravity
for <chord> (it is not usually a member of the chord).

If <chord> is a list of chords the box returns a list of best frequencies.

The optional argument <unit> determines whether <chord> is entered in
midicents, ('midic'), or in hertz ('freq'). If 'midic' is selected the
values will be converted to frequencies inside the function and then the
output is reconverted to midicents. If 'freq' is selected the entry,
calculations and output are all in hertz."
  (if (= unit 2)
      (less-deep-mapcar 'best-freq1 chord)
      (f->mc (less-deep-mapcar 'best-freq1 (mc->f chord)))))


(defun best-freq1 (freqs)
  (let ((sum 0) (nb-freq (length freqs)))
    (while freqs (incf sum (log (nextl freqs))))
    (exp (/ sum nb-freq))))




;; ============ modulations ==========================================================

;; ----------   modulation de fréquence

(defun fmcalc (p m index)
  (let ((res (list p)))
    (for (i 1 1 index)
      (push (+ p (* i m)) res)
      (push (- p (* i m)) res))
    (reverse res)))

(defunp fm/freq ((fcarrier freqs) (fmod freqs) (index fix>0)) freqs
    ""
  (let* (ll (x (one-elem fcarrier)) (fcarrier (list! fcarrier)) (fmod (list! fmod)))
    (while fcarrier
      (let ((a (pop fcarrier)))
        (push (unique (flat (mapcar (lambda (x) (fmcalc a x index)) fmod))) ll) ))
    (if  x (flat (nreverse ll)) (nreverse ll))))

(defunp fm/midic ((mcarrier midics?) (mmod midics?) (index fix>0)) list
    "Returns a list of midics according to frequency modulation:
  (mcarrier +- i * mmod) i=0 -->index  . Input and output = midic
   mcarrier and mmod may be lists"
  (f->mc (fm/freq (mc->f mcarrier) (mc->f mmod) index)))

;; nouvelles fonctions plus performantes ---------

(defun fmnth (p m lindex output)
  (let ((res (if (= output 2) () (list p))))
    (while  lindex
      (let ((i (pop lindex)))
        (push (+ p (* i m)) res)
        (push (- p (* i m)) res)))
    (reverse res)))

(defun freq-mod/f (fcarrier fmod  index output )
  (let* (ll (x (one-elem fcarrier)) (fcarrier (list! fcarrier)) (fmod (list! fmod))
            (index (if (atom index) (arithm-ser 1 1 index) index)))
    (while fcarrier
      (let ((a (pop fcarrier)))
        (push (unique (flat (mapcar (lambda (x) (fmnth a x index output)) fmod))) ll) ))
    (if  x (flat (nreverse ll)) (nreverse ll))))

(defun fm/f/ratio (fcarrier ratio  index output )
  (let* (ll (x (one-elem fcarrier)) (fcarrier (list! fcarrier)) (ratio (list! ratio))
            (index (if (atom index) (arithm-ser 1 1 index) index)))
    (while fcarrier
      (let ((a (pop fcarrier)))
        (push (unique (flat (mapcar (lambda (x) (fmnth a x index output))
                                    (g* a ratio) ))) ll) ))
    (if  x (flat (nreverse ll)) (nreverse ll))))


(defunp freq-mod ((carrier midics?) (modul midics? (:value 6600)) (index numbers? (:value 1))
                  &optional (unit menu (:menu-box-list (("midic" . 1) ("freq". 2))))
                  (output menu (:menu-box-list (("inclus" . 1) ("exclus". 2))))) list
    "Simulates the pitches generated by frequency modulation. The frequencies
of the carrier <carrier> and the modulator <modul> are treated according
to the following formula:
carr. ± (i * mod.), i = 0,1,...<index>

If <carrier> is a list the output is a list of modulations around each
successive carrier.

If <modul> is a list the carrier or carriers are each modulated by all
the notes in <modul> as well as the partials of those notes up to the
<index> specified.

If <index> is a list the formula is computed with 'i' equal to only the
listed values. (e.g. <index> = (1 3), notes calculated are:
carr. + 1 * mod.; carr. - 1 * mod.;carr. + 3 * mod.; carr. - 3 * mod.)

The optional argument <unit> determines whether the <carrier> and <modul>
are given in midicents, ('midic'), or in hertz ('freq'). If 'midic' is
selected the values will be converted to frequency inside the function
and then the output is reconverted to midicents. If 'freq' is selected the
entry, calculation and output are all in hertz.

The optional argument <output> determines whether the carriers are
included ('inclu') or excluded ('exclu') from the output list or lists."
  (if (= unit 2) (freq-mod/f  carrier modul index output)
      (f->mc (freq-mod/f (mc->f carrier) (mc->f modul) index output))))

(defunp fm-ratio ((carrier midics?) (ratio numbers? (:value 1.41)) (index numbers? (:value 1))
                  &optional (unit menu (:menu-box-list (("midic" . 1) ("freq". 2))))
                  (output menu (:menu-box-list (("inclus" . 1) ("exclus". 2))))) list
    "Simulates the pitches generated by frequency modulation. The frequency of
the <carrier> is modulated by a modulator whose frequency is equal to the
frequency of the carrier multiplied by the ratio. Once the frequency of
the modulator is determined, the calculations follow the same formula as
the box 'freq-mod' :
carr. ± (i * mod.), i = 0,1,...<index>

If <carrier> is a list the output is a list of modulations around each
successive carrier.

If <ratio> is a list the carrier or carriers are each modulated by the
frequencies that form all the requested ratios as well as the partials of
those notes up to the <index> specified.

If <index> is a list the formula is computed with 'i' equal to only the
listed values. (e.g. <index> = (1 3), notes calculated are:
carr. + 1 * mod.; carr. - 1 * mod.;carr. + 3 * mod.; carr. - 3 * mod.)

The optional argument <unit> determines whether the <carrier> is entered in
midicents, ('midic'), or in hertz ('freq'). If 'midic' is selected the
value will be converted to frequency inside the function and then the
output is reconverted to midicents. If 'freq' is selected the entry,
calculation and output are all in hertz.

The optional argument <output> determines whether the carrier or carriers
are included ('inclu') or excluded ('exclu') from the output list or
lists."
  (if (= unit 2) (fm/f/ratio carrier ratio index output)
      (f->mc (fm/f/ratio (mc->f carrier) ratio index output))))





;; --------------  modulation en anneau

(defune ringharm/f/struc ((fond1 freq) (fond2 freq) (hqa fix>0) (hqb fix>0)) chord
    ""
  (setq fond1 (car! fond1) fond2 (car! fond2))
  (if (and (atom hqa) (atom hqb))
      (let ( (mx (max hqa hqb)) (res ()) (resinter ()) )
        (for (m 1 1 mx)
          (for (ia 1 1 (min m hqa))
            (for (ib 1 1 (min m hqb))
              (if (and (< ia m) (< ib m)) ()
                  (progn (push (+ (* ia fond1) (* ib fond2)) resinter)
                         (push (- (* ia fond1) (* ib fond2)) resinter)))
              ))
          (push (reverse resinter) res)
          (setq resinter () ) )
        (reverse res))
      (ringlist fond1 fond2 hqa hqb)))


(defun ringlist (fonda fondb hqa hqb )
  (let ((lfonda (g* fonda hqa)) (lfondb (list!(g* fondb hqb))))
    (flat-once (mapcar (lambda (x) (ring-mod lfonda x 2 1 1)) lfondb))))




(defunp ring-harm ((funda midic) (fundb midic (:value 6600))
                   (hqa fix>0) (hqb fix>0 (:value 2))
                   &optional (unit menu (:menu-box-list (("midic" . 1) ("freq". 2))))
                   (type menu (:menu-box-list (("seq" . 1) ("chord". 2))))
                   (output menu (:menu-box-list (("exclus" . 1) ("inclus". 2))))) list
    "Simulates the ring-modulation between the harmonic series (see box
'harm-series') built on <funda> and the harmonic series on <fundb>. The
arguments <hqa> and <hqb> determine the number of partials present for
each fundamental. The frequencies of each partial of the harmonic series
on <funda> is added to and subtracted from the frequency of each partial
of the harmonic series on <fundb>; thus, all the possible additive and
subtractive combinations are produced.

If the arguments <hqa> or <hqb> are a list, rather then including all the
partials up to and including the number given: only the listed partials
for both fundamentals will included in the calculations.

The optional argument <unit> determines whether <funda> and <fundb> are
given in midicents, ('midic'), or in hertz ('freq'). If 'midic' is
selected the values will be converted to frequencies inside the function
and then the output is reconverted to midicents. If 'freq' is selected the
entries, calculations and output are all in hertz. (note: Ring-modulation
can produce negative frequencies; conversion to midicents will
automatically reflect these notes back into the positive domain.)

The optional argument <type> is used to determine the format of the
output. The value 'seq' returns a list of chords in which each successive
chord represents the notes involving the next partial or partials.
Thus the first chord contains: funda ± fundb; the second: 2*funda ± fundb,
funda ± 2*fundb and 2*funda ± 2*fundb; etc. The value 'chord' returns a
single chord containing all the notes of all the combinations and
differences.

The optional argument <output> determines whether the notes <funda> and
<fundb> are included ('inclu') or excluded ('exclu') from the output list
or lists."
  (let ((res (if (= unit 2) (ringharm/f/struc  funda fundb hqa hqb)
                 (f->mc (ringharm/f/struc (mc->f funda) (mc->f fundb) hqa hqb)))))
    (if (= output 2) (setq res (push (list funda fundb) res)) res)
    (if (= type 2) (flat res) res)))


(defunp ring-mod ((ch1 midic) (ch2 midic (:value 6600))
                  &optional (unit menu (:menu-box-list (("midic" . 1) ("freq". 2))))
                  (type menu (:menu-box-list (("seq" . 1) ("chord". 2))))
                  (output menu (:menu-box-list (("exclus" . 1) ("inclus". 2))))) chord
    "Simulates the ring modulation of each note of <ch1> by all the notes of
<ch2>. The frequency of each note of <ch2> is added to and subtracted
from the frequency of each note of <ch1>; thus, all the possible
additive and subtractive combinations are produced.

The optional argument <unit> determines whether <ch1> and <ch2> are
entered in midicents, ('midic'), or in hertz ('freq'). If 'midic' is
selected the values will be converted to frequencies inside the function
and then the output is reconverted to midicents. If 'freq' is selected the
entries, calculations and output are all in hertz. (note: Ring-modulation
can produce negative frequencies; conversion to midicents will
automatically reflect these notes back into the positive domain.)

When <ch1> contains multiple notes, the optional argument <type> is used
to determine the format of the output. The value 'seq' returns a list of
chords representing the modulation of each successive note of <ch1> by
all the notes of <ch2>. The value 'chord' returns a single chord
containing all the notes of all the modulations.

The optional argument <output> determines whether the original notes of
<ch1> and <ch2> are included ('inclu') or excluded ('exclu') from the
output list or lists."
  (let ((res (if (= unit 2) (ring/freq  ch1  ch2)
                 (f->mc (ring/freq (mc->f ch1) (mc->f ch2))))))
    (if (= output 2) (setq res (push (x-append ch1 ch2) res)) res)
    (if (or (= type 2) (one-elem ch1)) (flat res) res)))


(defunp ring/freq ((freqs1 freqs) (freqs2 freqs)) freqs
    "Rend une liste de listes de fréquences contenant la modulation en anneau
de chaque fréquence de la liste <freqs1> par la liste <freqs2>"
  (let* (ll (freqs1 (list! freqs1)) (freqs2 (list! freqs2))
            (x (one-elem freqs1)))
    (while freqs1
      (let ((a (pop freqs1)))
        (push (append (l+ a freqs2) (l- a freqs2)) ll) ))
    (if  x (flat (nreverse ll)) (nreverse ll))))

;; anciennes fonctions - pour compatibilité
(defunp ring/midic ((ch1 chord) (ch2 chord (:value 6200))) chord ""
  (ring-mod ch1 ch2))

(defune ringharm/m ((mfond1 midic) (mfond2 midic) (hqa fix>0) (hqb fix>0)) chord ""
  (f->mc (ringharm/f  (mc->f1 (car! mfond1)) (mc->f1 (car! mfond2)) hqa hqb)))

(defune ringharm/f ((fond1 freq) (fond2 freq) (hqa fix>0) (hqb fix>0)) chord ""
  (flat (ringharm/f/struc fond1 fond2 hqa hqb)))

;; ***********************************  INTERVALS  *************************************

;; ==== chord multiplication ====


(defunp mul-chord ((ch1 list (:value '(6000 7100)))
                   (ch2 list (:value '(6400 6700)))
                   &optional (type menu (:menu-box-list (("seq" . 1) ("chord". 2)))))
    list
    "Generates a list of chords in which the intervallic structure of <ch2>
\(a single chord in midicents) is reproduced beginning on each successive
note of <ch1> (also a single chord in midicents).

The optional argument <type> allows the choice of whether the output is
a list of chords ('seq') or a single chord ('chord') containing all the
transpositions combined."
  (let ((ch1 (list! ch1)) (int2 ()) (base-note-2 (apply 'min ch2)) res)
    (while ch2 (newl int2 (- (nextl ch2) base-note-2)))
    (setq int2 (nreverse int2))
    (setq res (mapcar
               (lambda (midic) (mapcar (lambda (iv) (+ iv midic)) int2))
               ch1 ))
    (if (= type 2) (flat res) res )))

;; ==== (usually) for chord interpolation ====

;;from Rhythms:Functions [magnus]

(defun -power-function (begin end time curve)
  (+ (* (- end begin) (expt time curve)) begin))

;; ==== chord proliferation ====

;; after CR
(defunp espace-ch ((chord list (:value "(6000 6300)"))) list
    "Computes all chord's downward transpositions from the first note, keeping intervals"
  (let ((int2 ()) (base-note-2 (car chord)) (ch2 chord))
    (while ch2 (newl int2 (- (nextl ch2) base-note-2)))
    (setq int2  (nreverse int2))
    (mapcar
     (lambda (midic) (mapcar (lambda (iv) (- iv midic)) chord))
     int2 )))

;; n0  n1  n2  n3  n4  n5
;;   i1  i2  i3  i4  i5
;;   ^       ^       ^
;;   |       |       |
;; ints   curints revints

;;(defunt espace-ch ((chord chord)) chords)

(defunp auto-transp ((chord list (:value "(6000 6300)"))
                     &optional (output menu (:menu-box-list (("inclus" . 1) ("exclus". 2))))
                     (min (midic (:value 0))) (max (midic (:value 0)))) list
    "Outputs a list of all possible transpositions of <chord> (a single
chord in midicents) which contain the first note of the chord.

The optional argument <output> allows the choice of whether the original
<chord> will be included, 'inclu' or excluded, 'exclu' from the output
list.

The optional arguments <max> and <min> (in midicents) will cause the
notes to be transposed by octaves to fit within the specified range."
  (let ((int2 ()) (base-note-2 (car chord)) (ch2 chord)
        chords)
    (if (zerop max) (setq max most-positive-fixnum))
    (while ch2 (newl int2 (- (nextl ch2) base-note-2)))
    (setq int2  (nreverse int2))
    (setq chords
          (mapcar
           (lambda (midic) (mapcar (lambda (iv) (- iv midic)) chord))
           int2 ))
    (setq chords (mapcar (lambda (list) (transpoct list min max)) chords))
    (if (= output 2) (cdr chords) chords)))

(defunp down-transp ((chord list (:value "(6000 6300)")) &optional (min (midic (:value 0)))
                     (max (midic (:value 0)))) list
    "Computes all chord's downward transpositions from the first note,
keeping intervals.  Each note might be transposed by some octaves to fit
in the (optional) register given."
  "Computes all chord's downward transpositions from the first note, keeping intervals"
  (let* ((chords ()) (base-note (car chord))
         (ints (x->dx chord)) (revints (last ints)) (curints ints) pints
         ch)
    (if (zerop max) (setq max (apply #'max chord)))
    (while curints
      (setq ch (list base-note))
      (setq pints curints)
      (while pints
        (newl ch (transpoct-prox (+ (car ch) (nextl pints)) min max (car ch))))
      (setq ch (nreverse ch))
      (setf ints (nreverse ints))
      (setq pints (cdr curints))
      (while pints
        (newl ch (transpoct-prox (- (car ch) (nextl pints)) min max (car ch))))
      (newl chords ch)
      (setf revints (nreverse revints))
      (nextl curints))
    (setq ch (list base-note))
    (setf ints (nreverse ints))
    (setq pints revints)
    (while pints
      (newl ch (transpoct-prox (- (car ch) (nextl pints)) min max (car ch))))
    (newl chords ch)
    (setf revints (nreverse revints))
    (nreverse chords)))

;;(defunt espace-chn ((chord chord) (min midic) (max midic)) chords)


;; --------------transpositions, filterings ------------------------------

(defunp transpoct-prox1 ((midics midics?) (min midic) (max midic) (pivot midic)) midic
    ""
  (let ((mmin (max min (- pivot 1200)))
        (mmax (min max (+ pivot 1200))))
    (cond
      ((> mmin mmax)
       (error "The intervals [~S ~S] [~S-1200 ~S+1200] don't intersect."
              min max pivot pivot))
      ((< (- mmax mmin) 1200)
       (error "The intersection between [~S ~S] and [~S-1200 ~S+1200] is smaller than one octave."
              min max pivot pivot)))
    (transpoct midics mmin mmax)))

(defunp transpoct-prox ((chord midics?) (min midic) (max midic) (pivot midic)) midic
    "Transposes <chord> (midics) by octaves to fit into the interval [<min> <max>]
while making with <pivot> an interval smaller than one octave.
<Chord> may be a list of chords"
  (less-deep-mapcar 'transpoct-prox1 chord min max pivot))

(defunp transpoct1 ((midics midics?) (min midic) (max midic)) midic
    ""
  (let ((result (mapcar (lambda (midic)
                          (while (< midic min) (incf midic 1200))
                          (while (> midic max) (decf midic 1200))
                          midic) (list! midics))))
    (if (cdr result) result (car result))))


(defunp transpoct ((chord list (:value '(6000 6500)))
                   (min midic (:value 4800)) (max midic (:value 6000))
                   &optional (pivot midic (:value 0))) list
    "Transposes notes of a chord or list of chords <chord> by octaves such
that all its notes will be contained within the range between <min> and
<max>, given in midicents.

The optional argument <pivot>(a note, in midicents) forces all notes
to be transposed so that they will be within one octave of that note.
<Pivot> must be within the specified range, or an error will be
produced."
  (if (< max min) (rotatef max min))
  (if (zerop pivot)
      (less-deep-mapcar 'transpoct1 (list! chord) min max)
      (less-deep-mapcar 'transpoct-prox1 (list! chord) min max pivot)))



(defun replier-aux (midic mcible delta-inf delta-sup)
  (cond
    ((> midic mcible)
     (setq mcible (+ mcible (abs delta-sup)))
     (while (> midic mcible)
       (decf midic 1200)))
    (t
     (setq mcible (- mcible (abs delta-inf)))
     (while (< midic mcible)
       (incf midic 1200))))
  midic)

(defunp fold1 ((midics? midics?) (mcible midic)) midics?
    "replier par octave le plus pres possible
d'une frequence cible (sans approximation des hauteurs)."
  (deep-mapcar 'replier-1 'fold1 midics? mcible))

(defun replier-1 (midic mcible) (replier-aux midic mcible 600 600))

(defunp fold2 ((midics? midics?) (mcible midic)) midics?
    "replier par octave a moins d'une octave
d'une frequence cible (sans approximation des hauteurs)."
  (deep-mapcar 'replier-2 'fold1 midics? mcible))

(defun replier-2 (midic mcible) (replier-aux midic mcible 1200 1200))


(defunp reject ((midic midic) (min midic) (max midic)
                &optional (pivot midic)) midic
    "Transpose <midic> by octaves outside of the interval [<min> <max>]."
  (if (< midic pivot)
      (while (> midic min) (decf midic 1200))
      (while (< midic max) (incf midic 1200))))

;;(defunt reject ((midic midic) (min midic) (max midic)) midic)


;; =============================================================================-======
;;By Pierre-François Baisnée 26-04-90
;;Copyright 1990 IRCAM
;;==============================================================================
;;; SUPRIMER OU REMPLACER DES INTERVALLES DANS UN ACCORD
;;==============================================================================
#|
remove-inter replace-inter replace-inter-i:
SUPPRIMER/REMPLACER DANS UN ACCORD LES NOTES COMPRISES DANS
UN AMBITUS DONNE ET FORMANT UN INTERVALLE <forbid>
\(avec l'une quelconque des notes de l'accord, les notes etant prise
l'une apres l'autre dans l'accord ordonne)
Rend un resultat ordonne et sans notes en double

remove-inter2 replace-inter2 replace-inter3 replace-inter4:
SUPPRIMER/REMPLACER DANS UN ACCORD LES NOTES COMPRISES DANS
UN AMBITUS DONNE ET FORMANT UN INTERVALLE <forbid> AVEC LA NOTE
<PIVOT> (uniquement)
N'ordonne pas l'accord, ni ne supprime les notes en double.
replace-inter2: 2de = 2de, 9ieme, etc.
replace-inter3: 2de = 2de

inputs et outputs au format: midicent

approximation f(echelle discrete) prealable necessaire
|#

;;; XXX L'APPROXIMATION DES HAUTEURS EST REJETE EN AMONT (OBLIGATOIRE)

;;; XXX L'ORDRE DES NOTES DE L'ACCORD N'EST PAS CONSERVE -> PB POUR MULTI DIMENSIONS

;;; XXX COPIE DE MIDICS (modification (sort et rplacd))

;; (defunp remove-int1 ((midics chord) (forbid cents)
;;                      &optional (replace list (:value '() :type-list (fixnum list)))
;;                      (inf fix>=0) (sup fix>=0)) chord
;;     ""
;;   (let ((midics (list! midics)))
;;     (if (and (zerop inf) (zerop sup))
;;         (setq inf (apply #'min midics) sup  (1+ (apply #'max midics))))
;;     (sort&no-repeat (copy-list midics) inf
;;                     (if (zerop  sup) (1+ (apply #'max midics)) sup) forbid replace)))
;;
;; (defunp remove-int ((chord midic) (forbid midic (:value 1200))
;;                     &optional (replace list (:value '() :type-list (fixnum list)))
;;                     (inf fix>=0) (sup fix>=0)) chord
;;     "removes interval <forbid> (midic) from chord
;; Order of chord is not kept. Be careful of approximation
;; problems (this function does not approximate chords).
;; Extensions:
;; <inf> and <sup> (midics) : limits of range to be treated
;; <replace>  interval which will replace <forbid>
;; Accepts lists of chords, but not lists of intervals"
;;   (less-deep-mapcar  'remove-int1 chord forbid replace inf sup))

(defunp remove-int ((chord list (:value '(6000 6100) :type-list (fixnum list pw::chord pw::collector)))
                    (forbid cents (:value 100))
                    &optional (replace list (:value '() :type-list (fixnum list)))
                    (inf fix>=0) (sup fix>=0 (:value most-positive-fixnum))) chord
    "Removes the interval <forbid> (given in midicents) from the list
<chord>. The upper note of the forbidden interval is deleted.

A list of chords may be entered in <chord> in which case the interval
will be removed from each successive chord. A list of intervals for the
argument <forbid> is also possible.

The optional argument <replace> will take the notes that would have been
deleted and, instead, moves them to convert the forbidden interval into
the replacement.

The optional arguments <inf> and <sup> allow the low, <inf>, and high,
<sup>, limits in which changes can take place to be defined. These values
are given in midicents, and refer only to the upper note in the interval
pair.

Warnings:
The lists returned by this box will be ordered lowest note to highest
note, regardless of the order in the entries.
Entries are in true midicents, without any internal approximation, this
may provoke certain problems which can be rectified through the use of
the function approx-m prior to the entries."
  (let ((new-midics (get-list-of-midics chord)) res)
    (dolist (ch new-midics)
      (push (sort&no-repeat (copy-list ch) inf sup forbid replace) res))
    (get-midic-from-type chord (nreverse res))))

(defmethod get-list-of-midics ((midics cons))
  (if (consp (first midics)) midics
      (mapcan #'get-list-of-midics midics)))

(defmethod get-list-of-midics ((midics pw::C-chord)) (list (ask-all (pw::notes midics) 'pw::midic)))

(defmethod get-list-of-midics ((midics pw::C-chord-line))
  (mapcar (lambda (chord) (ask-all (pw::notes chord) 'pw::midic))
          (pw::chords midics)))

(defmethod get-list-of-midics ((midics number)) (list (list midics)))

(defmethod get-midic-from-type ((input number) result) (first (first result)))

(defmethod get-midic-from-type ((input pw::C-chord) result) (first result))

(defmethod get-midic-from-type ((input pw::C-chord-line) result) result)

(defmethod get-midic-from-type ((input cons) result)
  (if (consp (first input)) result
      (cond ((numberp (first input)) (first result))
            ((subtypep (type-of (first input)) 'pw::C-chord) result))))


(defunp replace-int ((midics chord) (inf midic) (sup midic) (forbid cents) (replace cents)) chord
    ""
  (sort&no-repeat (sort (copy-list (list! midics)) #'<)
                  inf (if (zerop sup) (car (last (list! midics))) sup) forbid replace))

;; =============================================================================
;;fonctions pour famille replace-inter

;;rend une liste ordonnee de valeurs uniques
(defun sort&no-repeat (l-inter inf sup forbid replace)
  (setq l-inter (sort l-inter #'<))
                                        ;on remplace
  (foo1 l-inter inf sup forbid replace)
                                        ;on rend une liste des valeurs uniques
  (sort (unique l-inter) #'<)
                                        ;l-inter
  )

;;iteration jusqu'a effet nul, sans protection contre bouclage
(defun foo-iter (l-inter inf sup forbid replace)
  (setq l-inter (sort l-inter #'<))
  (until (equal l-inter (setq l-inter (foo1 l-inter inf sup forbid replace))))
  l-inter )

;;l-inter: liste ORDONNEE de midic "APPROXIME"
;;inf et sup : midic, bornes de l'ambitus pris en compte
;;forbid replace : intervalles exprimes en midic, valeur a supprimer
;;                   et valeur eventuelle de remplacement.
;;les pointeurs sur la liste de depart sont impredictibles
                                        ;
                                        ;
;;fonction auxiliaire: ne rend pas de valeur, mais fonctionne par
;;modifications physiques de la liste de longueur n,
;;en prenant comme pivot successivement les n-1 premieres notes et en verifiant
;;les intervalles formés avec les notes suivantes. Le sens des intervalles est
;;pris en compte.
;;SI forbid= 9eme replace=octave =>
;;   Pour les notes comprises dans l'ambitus precisé, et formant un intervalle
;;   sur ou sous-multiple de 9ieme avec le pivot courant:
;;   les secondes ne sont pas modifiees
;;   les 9iemes, 16iemes, 23iemes... sont remplacees respectivement par des
;;       8iemes, 15Iemes, 22iemes... de meme direction par rapport au pivot
;;   utilise.
;;VARIANTES POSSIBLES:
;; - prendre au sens strict (9ieme != 2de) forbid ou bien replace ou bien les
;;   deux.
;; - Tenir ou ne pas tenir compte du sens
;; - ordonner ou non
;; - laisser les notes en double ou non

(defun foo1 (l-inter inf sup forbid replace)
  (let ( (tete l-inter) (pivot (car l-inter)) (octaves 0) (tem) (direction) )
                                        ;(print "FOO1 " tete " " pivot)
    (while (cdr l-inter)
      (ifnot (and (< inf (cadr l-inter)) (< (cadr l-inter) sup)) ;dans la bande
          (nextl l-inter)
        (setq tem (- (cadr l-inter) pivot)
              direction (if (plusp tem) 'up 'down)
              octaves 0
              tem (abs tem) )
        (while (> tem (max 1200 forbid)) (setq tem (- tem 1200)) (incf octaves))
        (if (= tem forbid)
            (ifnot replace
                (rplacd l-inter (cddr l-inter))
                                        ;(print pivot " " (cadr l-inter) " " direction " " octaves " " replace)
              (rplaca (cdr l-inter)
                      (funcall (if (eql direction 'up) #'+ #'-)
                               pivot (funcall (if (plusp replace) #'+ #'-)
                                              replace (* 1200 octaves))))
              (nextl l-inter))
            (nextl l-inter) ) ))
    (when (cddr tete) (foo1 (cdr tete) inf sup forbid replace)) ))


;; =============================================================================
;;fonctions pour famille replace-inter2

;;rend la liste transformee
(defun foo2 (l-inter inf sup pivot forbid replace oct-flag oct-flag2)
  (let ( (tete (setq l-inter (cons '? l-inter)))
         (octaves 0) (tem) (direction) )
                                        ;(print "FOO2 " l-inter " " pivot)
    (while (cdr l-inter)
      (ifnot (< inf (cadr l-inter) sup) ;ambitus a traiter
          (nextl l-inter)
        (setq tem (- (cadr l-inter) pivot)
              direction (if (plusp tem) 'up 'down)
              octaves 0
              tem (abs tem) )
        (when oct-flag2
          (while (> tem (max 1200 forbid)) (setq tem (- tem 1200)) (incf octaves)))
        (if (eql tem forbid)
            (ifnot replace
                (progn
                                        ;(print "XX")
                  (rplacd l-inter (cddr l-inter)))
                                        ;(print pivot " " (cadr l-inter) " " direction " " octaves " " replace)
              (rplaca (cdr l-inter)
                      (funcall (if (eql direction 'up) #'+ #'-)
                               pivot (funcall (if (plusp replace) #'+ #'-)
                                              replace (if oct-flag (* 1200 octaves) 0)
                                              )))
              (nextl l-inter))
            (nextl l-inter) ) ))
    (cdr tete) ))

;; =============================================================================-======

(defunp interp-oct ((ch chord) (mel chord) (order list (:value "(1)"))) chords
    "Returns a list of (length <order>) chords from <ch> to <mel> by substitution
and octaviation."
  (let ((chords (list (list! ch))) (mel (list! mel)) new-ch new-notes)
    (while order
      (setq new-notes ())
      (repeat (nextl order) (newl new-notes (nextl mel)))
      (setq new-ch (copy-list new-notes))
      (mapc
       (lambda (n)
         (unless (some (lambda (nn) (zerop (mod (- nn n) 1200))) new-notes)
           (newl new-ch n)))
       (car chords))
      (newl chords (nreverse new-ch)))
    (nreverse chords)))

;;(defunt interp-oct ((ch chord) (mel chord) (order list)) chords)

;; =============================================================================-======





(defun all-series (serie notes ints &aux note note-oct)
  "Prints and returns all the series beginning with the notes of (reverse <serie>)
and filled with permutations of <notes> and intervals among <intervals>."
  (let ((serie (list! serie)) (notes (list! notes)) (ints (list! ints)))
    (if (endp ints)
        (list (prog1 serie (format t "~A~%" (mc->n (reverse serie)))))
        (mapcan
         (lambda (int)
           (when (setq note-oct (exist-note? (setq note (+ int (car serie))) notes))
             (all-series (cons note serie) (remove note-oct notes)
                         (delete int (npermut-random (copy-list ints))))))
         ints))))

(defunt all-series ((serie chord) (notes chord (:value 6300)) (ints chord (:value 300))) chords)

;; (all-series '(6700 6400 6000) '(6900 6800 7000 7300) '(100 200 300))
;; (time (setq ls (all-series '(2500 2400) (make-list3 2600 100 3500)
;;                    (make-list3 200 100 1100))))


;;------------ intervals --> chord -------------

;;

(defunp inter->chord ((base midic) (inter list)
                      &optional
                      (format menu (:menu-box-list (("inter" . 1) ("notes". 2)) ))) list
    "<insert DOC here>"
  (inter->chord1 base inter format))

(defun inter->chord1 (base inter format)
  (less-deep-mapcar 'inter->chord2 inter base format))

(defun inter->chord2 ( inter base format)
  (g+ base (g* 100 (if (= format 2) inter (push 0 inter) ))))


(defunp chord->inter ((chord list (:value '(6000 6400 6700)))
                      &optional (format menu (:menu-box-list (("inter" . 1) ("notes". 2)) )))
    list
    "Returns a list containing the intervals (given in semitones; positive
or negative) between the first note of <chords> and each successive
note. Micro-intervals are represented as floating point number parts of a
semitone (e.g. a quarter-tone = 0.5).

If <chords> is a list of chords it will return a structured list
containing the intervals of each successive chord.

The optional argument <format> allows a choice of whether the box
returns real intervals (setting 'inter') or set notation (setting
'notes'). 'Inter' will return only the intervals between notes. 'Notes'
returns the intervals present as well as the first note, given as '0'."
  (less-deep-mapcar 'chord->inter1 chord format))

(defun chord->inter1 (chord format)
  (if (= format 2) (g/ (g- chord (first chord)) 100)
      (remove 0 (g/ (g- chord (first chord)) 100)) ))

;; ------------ analyse --------------


(defunp exist-note? ((chord midic) (note midic)) midic
    "Outputs the first note in <chord> (a single chord in midicents) which is
the same as <note> (regardless of octave). The value returned is the
midicents value of that note at the octave it exists in the <chord>. If
the note is not present the value 'nil' is returned.

The argument <chord> may receive a list of chords; in which case the
output is a list of lists containing the analyses of each successive
chord.

The argument <note> may receive a list; in which case the output is a
corresponding list of notes found and/or nils."
  (less-deep-mapcar (lambda (x) (car-mapcar 'exist-note1 note x)) chord ))


(defun exist-note1 ( note chord)
  (some (lambda (n) (when (zerop (mod (- note n) 1200)) n)) (list! chord)))



;;;=======================================
;;; Utilities

(defunp puiss/TO9 ((x0 float) (y0 float)(x1 float)(y1 float)(x2 float)
                   (y2 float) (sym list (:value "fpuiss9")) ) ()
    "calcule les paramètres de l'équation  y=a (x+c)^b+d en fct de trois points
\(x0,y0) (x1,y1) (x2,y2) .
La fct doit être continûment croissante ou décroissante.
Les points doivent être donnés dans l'ordre (donc : x2 > x1 > x0).
Utilise l'ancien algorithme du TO9.
Extrapolation à gauche interdite"
  (let* ((c (- x0))
         (d y0)
         (b (/ (- (log (abs (- y2 y0))) (log (abs (- y1 y0))))
               (- (log (- x2 x0)) (log (- x1 x0)))))
         (a (if (>= y2 y0) (/ (- y1 y0) (expt (- x1 x0) b)) (/ (- y2 y0) (expt (- x2 x0) b)))))
    (if (not (null sym)) (format t "y = ~S ( x + ~S ) ^ ~S + ~S  ~%"
                                 (lldecimals a 6) (lldecimals c 6) (lldecimals b 6) (lldecimals d 6)))
    (set (if (null sym) 'fpuiss9 sym)
         (eval `(function
                 (lambda (x) (+ ,d (* ,a (expt (+ x ,c) ,b))))) ) )
    ))

(defunp l-distor/2 ((newmin float (:value 1)) (newmax float (:value 2))
                    (liste list (:value '(0.5 0.7)))) list
    "Distorts a list, <liste>, by a power function, thus if the list is linear
the result follow the power function, if the list is non-linear the
result will be a hybrid of the old liste and the power function.

The arguments <newmin> and <newmax> determine the scaling of the new
list. (<newmin> will be the smallest value present, <newmax> the largest)"
  (let ((liste (list! liste)))
    (mapcar (power/2 (l-min liste) newmin (l-max liste) newmax () ) liste)))

(defunp l-distor/3 ( (newmin float (:value 0.5)) (newmax float (:value 1))
                     (ref float (:value 0.7)) (newref float (:value 2))
                     (liste list (:value '(0.6 0.9))) ) list
    "Distorts a list, <liste>, by a power function, thus if the list is linear
the result will follow the power function, if the list is non-linear the
result will be a hybrid of the old liste and the power function. This box
is identical to 'l-distor/2' except that a reference point is
controllable.

The arguments <newmin> and <newmax> determine the scaling of the new
list. (<newmin> will be the smallest value present, <newmax> the largest)
The values <ref> and <newref> are used to specify that the element of the
original list with a value of <ref> will be moved to the value of
<newref>. The curve will be altered in order to accommodate the reference
point."
  (if (or (> newmin newref newmax) (< newmin newref newmax))
      (mapcar (puiss/TO9 (l-min liste) newmin  ref newref (l-max liste) newmax () ) liste)
      (mapcar (parabole/3 (l-min liste) newmin  ref newref (l-max liste) newmax () ) liste)))


(defune L*line ((fact1st float) (factlast float) (liste numbers?)) list
    "Multiplies a list, <liste>, by a linear function. The first element is
multiplied by <fact1st>, the last by <factlast> and all intermediate
elements by linear interpolations between those values."
  (let ((long (length liste)))
    (g* liste (sample-fun (linear 1 fact1st long factlast () ) 1 1 long))))

(defunp L*curb/2 ((fact1st float (:value 1))
                  (factlast float (:value 2)) (liste list)) list
    "Multiplies a list, <liste>, by a power function. The first element is
multiplied by <fact1st>, the last by <factlast> and all intermediate
elements by interpolations along a power function between those values."
  (let ((long (length liste)))
    (g* liste (sample-fun (power/2 1 fact1st long factlast () ) 1 1 long))))

(defunp L*curb/3 ( (fact1st float (:value 1)) (factlast float (:value 4))
                   (ref fix>=0 (:value 2)) (factref float (:value 3))
                   (liste list (:value "(1 2 3 4)")) ) list
    "Multiplies a list, <liste>, by a power function. This box is identical to
'l*curb/2' except that a reference point is controllable. The first
element is multiplied by <fact1st>, the last by <factlast> and the
element of the original list with a value of <ref> will be multiplied by
<factref>. All intermediate elements will be multiplied by interpolations
along a power function between <fact1st> and <factlast>. The power
function, however, will be altered to accommodate the reference point."
  (let ((long (length liste)))
    (if (or (> fact1st factref factlast) (< fact1st factref factlast))
        (g* liste (sample-fun (puiss/TO9 1 fact1st  ref factref long factlast () ) 1 1 long))
        (g* liste (sample-fun (parabole/3 1 fact1st  ref factref long factlast () ) 1 1 long)))
    ))

;;-----------------------------------------
(defun sec->min1 (sec nbdec format)
  (let ((min (truncate sec 60)))
    (if  (and (= format 2 )(= 0 min)) (list(lldecimals sec nbdec))
         (list min 'min (lldecimals (mod sec 60) nbdec)))))

(defunp sec->min ((lsec numbers? (:value 65)) &optional (nbdec fix (:value 2))
                  (format menu (:menu-box-list (("normal" . 1) ("abbrev". 2))))) list
    "Converts values in seconds (<lsec>)to values in minutes and seconds. The
optional argument <nbdec> determines the number of decimals in the
seconds column of the output.

The output is in the format '1 min 15' for an <lsec> equal to '75'. If the
number of seconds is less than sixty the output will be in the form
'0 min 32'. The optional argument <format>, if set to the position
'abbrev', will eliminate the minutes column if it has a value of '0'.
\(The first example would remain '1 min 15' while the second would become
'32')"
  (deep-mapcar/1  'sec->min1 lsec nbdec format))

(defun min->sec1 (minutage)
  (let ((sec 0) (minutes 0) (minutage (list! minutage)))
    (cond  ((= (length minutage) 1)  (setq sec (car minutage)))
           (( or (numberp (second minutage)) (= (length minutage) 3))
            (setq minutes (car minutage)) (setq sec (l-last minutage)))
           (t (setq minutes (car minutage))))
    (lldecimals (+  sec (* minutes 60)) 2)))

(defunp min->sec ((minutes list (:value '(1 min 30)))) numbers?
    "Converts values in minutes into values in seconds. The value in minutes
may be entered as a list in any of the following formats: (3 min), or
\(3 0); (3 min 30), or (3 30), or (3.5 min); (3 min 30.2), or (3 30.2).
\(the letters 'min' may be replaced by simply 'm' or any other non-numeric
character or characters) "
  (less-deep-mapcar  'min->sec1 (list! minutes)))

;;--------------------------------------------

(defun monnayage (list density)
  (let* ((interv (/ (- (second list) (car list)) (1+ density))) res)
    (for (n 1 1  density)
      (push (+ (car list) (*  n interv)) res))
    res))

(defunp densifier ((list list) (density fix>0 )
                   &optional (min list (:value '() :type-list (fixnum float)))
                   (max list (:value '() :type-list (fixnum float)))) list
    "Increases the density of a <list> by adding equally spaced notes between
each of its elements. The number of added notes is determined by the
argument <density>, which is equal to the number of notes added between
each pair of elements.

The optional arguments <min> and <max> allow the densification to take
place within only a portion of the list. Notes will only be added between
pairs of elements in which both members are at least <min> and no greater
than <max>; all other parts of the list will be returned unchanged."
  (setq min (if min  min (l-min list)))
  (setq max (if max  max (l-max list)))
  (let ((long (1- (length list))) res)
    (dotimes (n long)
      (push (car list) res)
      (if (and (>= (min (car list) (second list)) min)
               (<= (max (car list) (second list)) max)
               (> density 0))
          (push (monnayage list density) res))
      (setq list (cdr list)))
    (nreverse (flat (push (last-elem list) res)))))


;;---------------------formatage de texte-----------------------

(defunp special-char ((sep menu (:menu-box-list (("space" . 1) ("line". 2) ("tab". 3)) ))
                      (nb fix>0) ) list
    "générateur de caractères spéciaux (espace, newline, tab)
<nb> = nombre de ces séparateurs"
  (let* ((char (case sep
                 (1  (if (> nb 1) #\Space nil))
                 (2 #\newline)
                 (3 #\tab)))
         (nb (if (= sep 1) (1- nb) nb)))
    (make-list   nb :initial-element char)))


(defunp text-format ((sep menu (:menu-box-list (("space" . 1) ("line". 2) ("tab". 3)) ))
                     (nb fix>0) (word1 list) (word2 list) &rest (words list)) list
    "formate du texte pour entrer dans text-win.
<sep> séparateur de mot (espace, newline, tab)
<nb> = nombre de ces séparateurs"
  (let* ((separ (special-char sep nb))
         (res (list word2 separ word1 )))

    (dolist (m  words) (push  (x-append separ m ) res))

    (x-append (flat (nreverse res)) separ)))


(defunp insert-special ((sep menu (:menu-box-list (("space" . 1) ("line". 2) ("tab". 3)) ))
                        (nb fix>0) (inter fix>0) (text list)  &rest (texts list)) list
    "insère un séparateur dans un texte à intervalle régulier
<sep> séparateur de mot (espace, newline, tab)
<nb> = nombre de ces séparateurs
<inter> = intervalle de séparation"
  (insert  (special-char sep nb)  inter (flat (x-append text texts ))))


;; pourrait être mis dans kernel

(defunp insert ((item list (:value 'toto) ) (inter fix>0)
                (liste list) &optional (nb fix>0)  ) list
    "insère un élément <item> dans une liste, à intervalle régulier
<inter> = intervalle de séparation
ext.: <nb> = nombre de ces éléments"
  (let* ((separ (make-list   nb :initial-element item))
         (liste (list-div liste inter))
         res)
    (dolist (m  liste) (push  (x-append m separ  ) res))
    (flat (nreverse res))))



(defunp list-div ((list list) (divisor fix>0 (:value 2))) list
    "divise la liste en sous-listes de dimension <divisor>"
  (let ((long (1- (length list))) res )
    (for  (i 0 divisor long)
      (let* ((inter (1- (+ i divisor )))
             (inter (if (< inter long) inter long)))
        (setq res (push (l-nth list (arithm-ser i 1 inter)) res))))
    (nreverse res)))



;;--------------------------------------------


(defunp TXtun1 ((tune fix (:value 0 :min-val -64  :max-val 63))
                (canal approx)) list
    "accord TX: 0= bécarre  21= +1/8  42= +1/4  63= + 3/8"
  (pw::midi-o (list 240 67 (+ 15 canal) 4 64 (+ 64 tune) 247)))

(defunp TXtune ((tunings fix/fl/list) (chans fix/fl/list (:value 1))) list
    "Sends global tuning parameters to a Yamaha TX-816. The value <tuning>
will be sent to the midi channel specified, <chans>. If <chans> is a list
the tuning will be sent to all listed channels."
  (cond ((not (consp tunings))
         (mapc (lambda (canal) (TXtun1 tunings canal)) (list! chans)))
        ((not (consp chans))
         (mapc (lambda (tun) (TXtun1 tun chans)) (list! tunings)))
        (t (mapc #'TXtun1 tunings chans))))
