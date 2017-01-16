;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               chord-filter.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;
;;;;    XXX
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;    Mikael Laurson
;;;;    Jacques Duthen
;;;;    Camilo Rueda
;;;;    Tristan Murail
;;;;MODIFICATIONS
;;;;    2012-05-07 <PJB> Changed license to GPL3; Added this header.
;;;;    1992-12-25 Tristan Murail révision finale
;;;;    1991-09-13 [jack] Chord Filtering -- CLPF V2.0
;;;;    1991-06-12 [jack] after Magnus
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


(defunp sort-mod ((chord midic)) list
    "Returns a sorted list of lists in which each note of the chord <chord>
\(in midicents) is converted into a list containing the interval in
midicents of that note from the 'c' below it, followed by the midicents
value of that 'c.' A list of chords also may be entered for <chord> the
resulting list will, however, have a higher level of structure and may
not be acceptable as entry for some other boxes."
  (less-deep-mapcar 'sort-mod1 chord))

(defun sort-mod1 (ch)
  (sort (mapcar (lambda (m) (let ((pc (mod m 1200))) (list pc (- m pc)))) (list! ch))
        #'< :key #'car))

;; ---- max-abs-idt ----

(defun max-abs-idt (ch1 ch2)
  "Uses as intervalic distance the maximum of the absolute intervals (in cents)
between the corresponding notes of the two chords <ch1> and <ch2>.
Returns the minimum intervalic distance between <ch1> and the best transposition
of <ch2> and returns this transposition as second value."
  (let* ((ints (mapcar #'- ch1 ch2))
         (int-min (apply #'min ints))
         (int-max (apply #'max ints)))
    (values (/ (- int-max int-min) 2) (/ (+ int-max int-min) 2))))

;;so that user extended box works with max-abs-idt as default value...!!!
(defun CL-USER::max-abs-idt (ch1 ch2) (max-abs-idt ch1 ch2))

(defunp ma-best-transp ((ch1 chord) (ch2 chord)) chord
    "Uses as intervalic distance the maximum of the absolute intervals (in cents)
between the corresponding notes of the two chords <ch1> and <ch2>.
Computes the minimum intervalic distance between <ch1> and the best transposition
of <ch2> and returns this transposition."
  (multiple-value-bind (dist ch) (max-abs-idt ch1 ch2)
    (declare (ignore dist))
    ch))

(defunp ma-min-interv ((ch1 chord) (ch2 chord)) chord
    "Uses as intervalic distance the maximum of the absolute intervals (in cents)
between the corresponding notes of the two chords <ch1> and <ch2>.
Returns the minimum intervalic distance between <ch1> and the best transposition
of <ch2>."
  (multiple-value-bind (dist ch) (max-abs-idt ch1 ch2)
    (declare (ignore ch))
    dist))

;;(mapcar #'- '(4700 5500 6000) '(6000 6300 6500))
;;(max-abs-idt '(4600 5600 6000) '(6000 6300 6500))
;;(max-abs-idt '(0 100 200 300) (mapcar #'1+ '(0 100 200 300)))
;;(max-abs-idt '(0 100 200 300) '(1 101 200 302))
;;(max-abs-idt '(0 100 200 300) '(1 101 195 311))

;; ---- sum-abs-idt ----

;; - the best transposition "Tbest" is the middle point of the list:
;;   (cond ((oddp (length ints)) (nth (/ n 2) ints))
;;         ((evenp (length ints)) (/ (+ (nth (floor n 2) ints)
;;                                     (nth (floor (1+ n) 2) ints)) 2)))
;; - the corresponding distance is:
;;   D = (Sum (i 0 n/2) |Tbest-INTi|) + (Sum (i n/2 n+1/2) |INTi-Tbest|)
;;   D = (Sum (i n/2 n+1/2) INTi) - (Sum (i 0 n/2) INTi)

(defun sum-abs-idt (ch1 ch2)
  "Uses as intervalic distance the arevage of the absolute intervals (in cents)
between the corresponding notes of the two chords <ch1> and <ch2>.
Returns the minimum intervalic distance between <ch1> and the best transposition
of <ch2> and returns this transposition as second value."
  (let* ((ints (sort (mapcar #'- ch1 ch2) #'<))
         (1-length (1- (length ints)))
         (summin 0) (summax 0)
         transpos)
    (repeat (floor 1-length 2)
      (incf summin (nextl ints)))
    (if (evenp 1-length)
        (nextl ints transpos)
        (progn
          (incf summin (nextl ints transpos))
          (setq transpos (/ (+ transpos (car ints)) 2))))
    (while ints (incf summax (nextl ints)))
    (values (- summax summin) transpos)))

(defunp sa-best-transp ((ch1 chord) (ch2 chord)) chord
    "Uses as intervalic distance the arevage of the absolute intervals (in cents)
between the corresponding notes of the two chords <ch1> and <ch2>.
Computes the minimum intervalic distance between <ch1> and the best transposition
of <ch2> and returns this transposition."
  (multiple-value-bind (dist ch) (sum-abs-idt ch1 ch2)
    (declare (ignore dist))
    ch))

(defunp sa-min-interv ((ch1 chord) (ch2 chord)) chord
    "Uses as intervalic distance the arevage of the absolute intervals (in cents)
between the corresponding notes of the two chords <ch1> and <ch2>.
Returns the minimum intervalic distance between <ch1> and the best transposition
of <ch2>."
  (multiple-value-bind (dist ch) (sum-abs-idt ch1 ch2)
    (declare (ignore ch))
    dist))

(defvar *default-intervalic-distance* 'max-abs-idt
  "Contains the intervalic distance function (which defaults to 'max-abs-idt) used by
the function closest-renv.
This function should return 2 values: the distance and the best transposition.")


;;-----------------------------------------------------------------------------------

(defunp closest-inv ((regch chord) (intch chord (:value 6500))
                     &optional (fct (list (:value 1)))) chord
    "Transpose globaly the intervallic chord <intch> and individually each of its notes
by octaves to produce a chord where each note is as close as possible to the
chord <regch>.
i.e distorts <regch> as little as possible to take the intervallic structure of <intch>."
  (multiple-value-bind (dist result)
      (closest-renv regch intch
                    (if (= fct 2) #'epw::max-abs-idt #'epw::sum-abs-idt ))
    (declare (ignore dist))
    (ll/round result 1)))



(defunp best-closest-inv ((regch chord) (intchs list (:value '((6400))))
                          &optional (fct (list (:value 1)))) chord
    "Like \"closest-renv\" but chooses the best intervallic-chord among <intchs>,
comparing the distorsions of <regch>."
  (multiple-value-bind (dist chord)
      (best-closest-renv regch intchs
                         (if (= fct 2) #'epw::max-abs-idt #'epw::sum-abs-idt ))
    (declare (ignore dist))
    (ll/round chord 1)))


;;approx:Changer???
(defunp closest-renv ((regch chord) (intch chord (:value 6500))
                      &optional (int-dist (list (:value 'epw::sum-abs-idt)))) chord
    "Transpose globaly the intervallic chord <intch> and individually each of its notes
by octaves to produce a chord where each note is as close as possible to the
chord <regch>.
i.e distorts <regch> as little as possible to take the intervallic structure of <intch>."
  (let* ((regch (list! regch))
         (intch (list! intch))
         (length (length intch))
         (regch-sort-mod (sort-mod regch))
         (regch-mod (mapcar #'car regch-sort-mod))
         (regch-transp (mapcar #'second regch-sort-mod))
         (intch-mod (sort (mapcar (lambda (m) (mod m 1200)) intch) #'<))
         (dist-min most-positive-fixnum)
         best-renvers best-transpos)
    (unless (= (length regch) length)
      (error "The two chords ~S and ~S should have the same length." regch intch))
    (for (renvers 0 1 (1- length))
      (multiple-value-bind (dist transpos) (funcall int-dist regch-mod intch-mod)
        (when (< dist dist-min)
          (setq dist-min dist best-renvers renvers best-transpos transpos)))
      (setf (car intch-mod) (+ 1200 (car intch-mod)))
      (setq intch-mod (permut-circn intch-mod)))
    (repeat best-renvers ; could be faster
      (setf (car intch-mod) (+ 1200 (car intch-mod)))
      (setq intch-mod (permut-circn intch-mod)))
    (values dist-min
            (mapcar (lambda (intmidic regtransp)
                      (+ intmidic best-transpos regtransp -1200))
                    intch-mod regch-transp))))



(defunp best-closest-renv ((regch chord) (intchs list (:value '((6400)) ))
                           &optional (int-dist (list (:value 'sum-abs-idt)))) chord
    "Like \"closest-renv\" but chooses the best intervallic-chord among <intchs>,
comparing the distorsions of <regch>."
  (let ((best-dist-min most-positive-fixnum) best-ch (regch (list! regch)))
    (mapc
     (lambda (intch)
       (multiple-value-bind (dist-min ch) (closest-renv regch intch int-dist)
         (when (< dist-min best-dist-min)
           (setq best-dist-min dist-min best-ch ch))))
     intchs)
    (values best-dist-min best-ch)))


;;------------------  fonctions du menu esquisse:intervals:treatments   ----------

(defunp best-transp ((ch1 list (:value '(4600 5600 6000)))
                     (ch2 list (:value '(6000 6300 6500)))
                     &optional (fct menu (:menu-box-list (("sum" . 1) ("max". 2))))) list
    "Transposes the chord <ch2> (a single chord in midicents) so that its
intervallic distance to <ch1> (also a single chord in midicents) is as
small as possible. Thus the distance between each note of <ch2> and each
note of <ch1> becomes as small as possible.This is essentially the same
as the box 'best-inv' except the ordering of <ch2> is preserved.

The optional argument <fct> allows the choice between two different
algorithms for calculating this function,'sum' and 'max'. The default
is sum because 'max' can produce quarter tones from demi-tone input. For
best results one should experiment with both and chose according to
context."
  (g+ ch2 (if (= fct 2) (ma-best-transp ch1 ch2) (sa-best-transp ch1 ch2))))


(defunp best-inv ((regch list (:value '(4600 5600 6000)))
                  (intch list (:value '(6000 6300 6500)))
                  &optional (fct menu (:menu-box-list (("sum" . 1) ("max". 2))))) list
    "Extracts the intervallic content of the chord <intch> (in midicents)
and through a global transposition of the chord followed by octave
transpositions of individual notes produces a chord with the intervals
of <intch> whose notes are as close as possible to those of the chord
<regch> (also in midicents). This in essence distorts the chord <regch>
by the smallest amount possible for it to take on the intervallic
structure of <intch>.

<regch> must be a single chord.

<intch> may be a list of chords (also in midicents) from which the one
that distorts least <regch> is used for the operation. The output will
still be a single chord.

The optional argument <fct> allows the choice between two different
algorithms for calculating this function,'sum' and 'max'. The default
is sum because 'max' can produce quarter tones from demi-tone input. For
best results one should experiment with both and choose according to
context."
  (if (atom (car intch)) (closest-inv regch intch fct)
      (best-closest-inv regch intch fct)))

(defun inversion1 (chord direction)
  "gives first inversion of <chord>, going down or up ,
according to <direction> ( > or < )"
  (setq chord (sort-list chord  direction))
  (let ( (oper (if (equal direction '>) '- '+)) (note (pop chord)))
    (while (funcall direction note (epw::l-last chord))
      (setq note (funcall oper note 1200)))
    (append chord (list note))))

(defunp all-inversions ((chord list (:value '(6000 6400 6700)))
                        (direction menu (:menu-box-list ((">" . 1) ("<". 2))))
                        &optional (format menu (:menu-box-list (("inclus" . 1) ("exclus". 2)))))
    list
    "Outputs a structured list of all possible inversions of <chord>
\(in midicents). Inversion here means the moving of the highest note down
by octaves so as to make it the new bass note (when direction is '>'), or
moving the lowest note up by octaves to make it the highest (when
direction is '<'). The output is a list of chords on which this operation
has been performed to each successive chord until all notes of <chord>
have served as the base note.

The optional argument <format> allows the choice of whether the original
<chord> will be included, 'inclu' or excluded, 'exclu' from the output
list.

Warning: <chord> can take only a single chord."
  (if (= direction 1) (setq direction '>) (setq direction '<))
  (let ((res (list chord)))
    (dotimes (n (1- (length chord)))
      (setq chord (inversion1 chord direction))
      (push chord res))
    (if (= format 2) (cdr (nreverse res)) (nreverse res))))


;;;; THE END ;;;;
