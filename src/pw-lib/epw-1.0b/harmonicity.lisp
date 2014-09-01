;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               harmonicity.lisp
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
;;;;    Camilo Rueda.
;;;;MODIFICATIONS
;;;;    2012-05-07 <PJB> Changed license to GPL3; Added this header.
;;;;    1992-12-25 Tristan Murail révision finale 
;;;;    1991       [jack] Esquisse PW harmonicity functions -- CLPF V1.0
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

;; ==== harmonic distance ====

;;  f0                   f0*n   freq    f0*(n+1)
;;---|---------------------|------|--------|---------->
;;                         <--d1--><--d2--->
;; ratio:=freq/f0; n:=floor(ratio)
;; ;; d1<=d2 <=> ratio*ratio <= n*(n+1)
;; d1 := freq/(f0*n)   := ratio/n
;; d2 := f0*(n+1)/freq := (n+1)/ratio

(defunp harm-dist-f ((f0 freq) (freq freq)) float
    "Returns the ratio between the closest harmonic of <f0> and <freq>."
  (if (<= freq f0) (/ f0 freq)
      (let* ((ratio (/ freq f0))
             (n-partial (floor ratio))
             (d1 (/ ratio n-partial))
             (d2 (/ (1+ n-partial) ratio)))
        (min d1 d2))))

(defunp harm-dist ((chord midic) (fund midic (:value 2400))
                   &optional (unit menu (:menu-box-list (("midic" . 1) ("freq". 2))))) list
    "Calculates the ratios between each note of <chord> and the closest 
partial of the harmonic series built on <fund>. (For explanations of 
harmonic series and partials, see the box 'harm-series')

If <chord> is a list of chords the result will be the analyses of each 
successive chord.

The optional argument <unit> determines whether <chord> and <fund> are
entered in midicents, ('midic'), or in hertz ('freq'). If 'midic' is 
selected the notes will be converted to frequencies inside the function
before analysis." 
  (if (/= unit 2) (setq fund (mc->f fund) chord (mc->f chord)))
  (deep-mapcar/1 (lambda (x) (harm-dist-f fund x)) chord))


(defunp closest-harm-f ((f0 freq) (freq freq)) float
    ""
  (if (<= freq f0) (/ f0 freq)
      (let* ((ratio (/ freq f0))
             (n-partial (floor ratio))
             (d1 (/ ratio n-partial))
             (d2 (/ (1+ n-partial) ratio)))
        (if (< d1 d2) n-partial (1+ n-partial) ))))


(defunp closest-harm ( (chord midic) (fund midic (:value 2400)) 
                       &optional (unit menu (:menu-box-list (("midic" . 1) ("freq". 2))))
                       (type menu (:menu-box-list (("rank" . 1) ("notes". 2))))) list
    "Calculates the closest partial of the harmonic series built on <fund> to
each note of <chord>. (For explanations of harmonic series and partials, 
see the box 'harm-series')

If <chord> is a list of chords the result will be the analyses of each 
successive chord.

The optional argument <unit> determines whether <chord> is entered in 
midicents, ('midic'), or in hertz ('freq'). If 'midic' is selected the 
values will be converted to frequencies inside the function and then the 
output (if appropriate) is reconverted to midicents. If 'freq' is selected 
the entry, calculations and output (if appropriate) are all in hertz.

The optional argument <type> determines whether the output is a list of 
partial rankings or the notes corresponding to those partials." 
  (let* ((ffund  (car! (if (/= unit 2) (mc->f fund) fund)))
         (fchord (if (/= unit 2) (mc->f chord) chord))
         (res (deep-mapcar/1 (lambda (x) (closest-harm-f ffund x)) fchord)))
    (if (= type 2) (nth-overtones  fund res unit) res)))



;; (defunp harm-ser ((mfonda midic) (step fix/float) (begin fix/float) (end fix/float)) chord
;;     "every <step> harmonics of <mfonda> from <begin> up to <end> harmonics"
;;   (let ((freq (mc->f1 (car! mfonda))) res)
;;     (for (n begin step end) (newl res (f->mc1 (* freq n))))
;;     (nreverse res) ))

;; ==== fondamental virtuel ====

(defun funda-virt-choice (fmin fmax l-fmin.fmax)
  (when (< fmax .1) (error "fundamental out of bounds ~S ~S~%" fmin fmax))
  (let (r n1 n2 (f1min (caar l-fmin.fmax)) (f1max (cdar l-fmin.fmax)))
    (pop l-fmin.fmax)
    (setq n1 (/ f1min fmax))
    (when (> n1 32767.) (error "partial out of bounds ~S~%" n1))
    (setq n1 (ceiling n1)
          n2 (min 32767 (floor f1max fmin)))
    (ifnot l-fmin.fmax
        (when (<= n1 n2)
          (cons (list n1) (cons (max fmin (/ f1min n1)) (min fmax (/ f1max n1)))))
      (for (n n1 1 n2)
        (setq r (funda-virt-choice (max fmin (/ f1min n)) (min fmax (/ f1max n))
                                   l-fmin.fmax))
        (when r
          (return-from funda-virt-choice
            (cons (cons n (car r)) (cdr r))))))))

(defun interval-around-f (freq approx)
  "Returns (fmin . fmax), fmax/freq=freq/fmin, int(fmin,fmax)=1/approx tone."
  (let ((coef (cents->coef (/ 100 approx))))
    (cons (/ freq coef) (* freq coef))))

;; (interval-around-f 440. (/ 1. 12)) => (220.0 . 880.0)
;; (interval-around-f 440. 1) => (415.3046975799451 . 466.1637615180899) (G# . A#)
;; (f->mc (interval-around-f 440. 1)) => (6800 . 7000)

;; (defun fond-virt-f (freqs approx)
;;   (let*
;;       ((l-fmin.fmax (mapcar (lambda (freq) (interval-around-f freq approx)) freqs))
;;        (f1min (caar l-fmin.fmax))
;;        (f1max (cdar l-fmin.fmax))
;;        (fvc (funda-virt-choice (- f1max f1min) f1max l-fmin.fmax))
;;        (f0min (cadr fvc))
;;        (f0max (cddr fvc))
;;        (m0s (f->mc (mapcar #'/ freqs (car fvc))))
;;        (m0 (/ (apply #'+ m0s) (length m0s))))
;;                                         ;(format t "fvc:~S~%" fvc)          ;
;;     (cond ((< m0 (f->mc1 f0min)) f0min)
;;           ((> m0 (f->mc1 f0max)) f0max)
;;           (t (mc->f1 m0)))))

(defun fond-virt-f (freqs approx)
  (tolerant-gcd freqs approx))

;;From Gerard Assayag [93 07 16]

(defun tolerant-gcd (values grid-ratio)
  "floating gcd with tolerance grid-ratio around the values."
  (labels ((grid-above (val) (* val (1+ grid-ratio)))
           (grid-below (val) (/ val (1+ grid-ratio)))
           (gcd-try (values gcd-min gcd-max)
             (when (<= gcd-min gcd-max)
               (ifnot values
                   (/ (+ gcd-min gcd-max) 2.0)
                 (let* ((val-below (grid-below (first values)))
                        (val-above (grid-above (first values)))
                        (quo-min (ceiling (/ val-below gcd-max)))
                        (quo-max (floor (/ val-above gcd-min))))
                   (do* ((quotient quo-min (1+ quotient)) (gcd-interval))
                        ((> quotient quo-max) nil)
                     (setf gcd-interval
                           (gcd-try (rest values)
                                    (max gcd-min (/ val-below quotient))
                                    (min gcd-max (/ val-above quotient))))
                     (when gcd-interval
                       (return-from gcd-try gcd-interval))))))))
    (gcd-try values .1 (grid-above (apply 'min values)))))

;; (fond-virt-f '(400. 500. 601.) 64) => ((4 5 6) 100.0763034890829 . 100.0902942798978)
;; (fond-virt-f '(400. 500. 601.) 32) => ((4 5 6) 99.98602183067244 . 100.1806700903654)

;; (f->mc (fond-virt-f (mc->f '(6000 6400 6700)) 4)) => 3604

;; ==== harmonic distances ====

(defun max-abs (f0 freqs)
  "Returns the greatest interval (in cents) between <f0> and each of the <freqs>."
  (let ((max 1.0))
    (while freqs
      (setq max (max max (harm-dist-f f0 (nextl freqs)))))
    (coef->cents max)))

(defun sum-abs (f0 freqs)
  "Returns the average sum of the absolute intervals (in cents) between <f0>
and each of the <freqs>."
  (let ((prod 1.0) (nb-freq (length freqs)))
    (while freqs
      (setq prod (* prod (harm-dist-f f0 (nextl freqs)))))
    (round (coef->cents prod) nb-freq)))

(defun sum-square (f0 freqs)
  "Returns the euclidian average of the intervals (in cents) between <f0>
and each of the <freqs>."
  (let ((sum 0.0) (nb-freq (length freqs)) coef)
    (while freqs
      (incf sum (* (setq coef (log (harm-dist-f f0 (nextl freqs)))) coef)))
    (round (/ (sqrt sum) nb-freq) #.(/ (log 2) 12000))
                                        ;(round #.(/ (log 2) .12) (/ (sqrt sum) nb-freq))
    ))

(defunp fund-virt-plot ((m0s midics?) (ch chord) (fun symbol)) list
    "Returns the list of distances (according to <fun>) between each f0 and <ch>."
  (let ((freqs (mc->f ch)))
    (mapcar (lambda (f0) (funcall fun f0 freqs)) (list! (mc->f m0s)))))

;; CAO:From-Mika-910218:PW-CL:PatchWork:PW-Library:PW-BPF-boxes.lisp

(defunp sumabs-allmin ((m0min midic) (ch chord) (m0max midic)) list
    "Returns the list of midics between <m0min> and <m0max> which correspond to a
minimum \"sum-abs\" harmonic distance to the chord <ch>.
They are the combinations of each frequency of <ch> divided by any integer n."
  (let ((freqs (sort (mc->f (list! ch)) #'<))
        (f0min (mc->f1 (car! m0min)))
        (f0max (mc->f1 (car! m0max)))
        (l ()) (k 0) freq)
    (while freqs
      (incf k)
      (mapc
       (lambda (f)
         (if (< (setq freq (/ f k)) f0min) (nextl freqs)
             (when (< freq f0max) (newl l (f->mc1 freq)))))
       freqs))
                                        ;(sort (nreverse (remove-duplicates l)) #'<) 20 times slower !
    (unique-sorted (sort l #'<) #'=)))

(defunp sumabs-allmax ((m0min midic) (ch chord) (m0max midic)) list
    "Returns the list of midics between <m0min> and <m0max> which correspond to a
maximum \"sum-abs\" harmonic distance to the chord <ch>.
They are the combinations of each frequency of <ch> divided by any (sqrt (* n (1+ n)))."
  (let ((freqs (sort (mc->f (list! ch)) #'<))
        (f0min (mc->f1 (car! m0min)))
        (f0max (mc->f1 (car! m0max)))
        (l ()) (k 0) kk freq)
    (while freqs
      (incf k)
      (setq kk (sqrt (* k (1+ k))))
      (mapc
       (lambda (f)
         (if (< (setq freq (/ f kk)) f0min) (nextl freqs)
             (when (< freq f0max) (newl l (f->mc1 freq)))))
       freqs))
                                        ;(sort (nreverse (remove-duplicates l)) #'<) 20 times slower !
    (unique-sorted (sort l #'<) #'=)))

(defunp sumabs-allminmax ((m0min midic) (ch chord) (m0max midic)) list
    "Returns the list of midics between <m0min> and <m0max> which correspond to a
potential minimum or maximum \"sum-abs\" harmonic distance to the chord <ch>.
They are the combinations of each frequency of <ch> divided by any (sqrt (* n (1+ n)))
for the maximums and divided by any n for the minimums."
  (unique-sorted
   (merge 'list
          (sumabs-allmin m0min ch m0max)
          (sumabs-allmax m0min ch m0max)
          #'<) #'=))

(defunp sumabs-minmax ((m0min midic (:value 2400)) (ch chord) (m0max midic (:value 4800))) list
    "Returns the list of midics between <m0min> and <m0max> which correspond to a
real minimum or maximum \"sum-abs\" harmonic distance to the chord <ch>."
  (fun-minmax
   (let ((freqs (mc->f (list! ch))))
     (lambda (midic) (sum-abs (mc->f1 midic) freqs)))
   (sumabs-allminmax m0min ch m0max)))

(defun fun-minmax (fun xs)
  "Returns the sublist of <xs> which correspond to real minimum or maximum of <fun>.
If two or more adjacent min or max have the same value, they are collected in a list."
  (let* ((l ())
         (state 'D)
         read
         (1st-0x xs)
         (oxs xs)
         (ox (car xs))
         (oy (funcall fun ox))
         x y )
    (loop
      (setq read
            (when (setq xs (cdr xs))
              (setq x (car xs) y (funcall fun x))
              (cond ((> y oy) '+)
                    ((< y oy) '-)
                    (t '0))))
      (ccase state
        (++ (ccase read
              (- (newl l ox) (setq state '--))
              (+)
              (0 (setq state '+= 1st-0x oxs))
              ((()) (newl l ox) (return))))
        (-- (ccase read
              (+ (newl l ox) (setq state '++))
              (-)
              (0 (setq state '-= 1st-0x oxs))
              ((()) (newl l ox) (return))))
        (+= (ccase read
              (- (newl l (list-sub 1st-0x xs)) (setq state '--))
              (+ (setq state '++))
              (0)
              ((()) (newl l (list-sub 1st-0x xs)) (return))))
        (-= (ccase read
              (+ (newl l (list-sub 1st-0x xs)) (setq state '++))
              (- (setq state '--))
              (0)
              ((()) (newl l (list-sub 1st-0x xs)) (return))))
        (D (ccase read
             (+ (newl l (list-sub 1st-0x xs)) (setq state '++))
             (- (newl l (list-sub 1st-0x xs)) (setq state '--))
             (0)
             ((()) (newl l (list-sub 1st-0x xs)) (return)))))
      (setq oxs xs ox x oy y))
    (nreverse l) ))


;;(defun list-sub (l1 l2) (car l1))

(defun list-sub (l1 l2)
  (if (eql l2 (cdr l1)) (car l1)
      (let ((l ()))
        (while (neq l1 l2)
          (when (null l1) (error "~S is not a sublist~%" l2))
          (newl l (nextl l1)))
        (nreverse l))))

;; (defunp virt-fund ((ch chord) (cents fix>=0 (:value 50))) midic
;;     "Returns the highest midic which corresponds to a \"sum-abs\" harmonic distance
;; to the chord <ch> smaller than or equal to <cents>."
;;   (let* ((ch (list! ch))
;;          (max (+ (l-min ch) 600)) (freqs (mc->f ch)) minmax)
;;     (loop
;;       (setq minmax (flat (sumabs-minmax (- max 1200) ch max)))
;;       (setq max
;;             (cond ((null minmax) (- max 1100))
;;                   ((null (cdr minmax)) (min (- max 150) (+ (car! minmax) 100)))
;;                   (t (min (- max 150) (+ (car! (cdr minmax)) 100)))))
;;       (when (cdr minmax) (nextl minmax))
;;       (mapc
;;        (lambda (midic)
;;          (when (<= (sum-abs (mc->f1 (car! midic)) freqs) cents)
;;            (return-from virt-fund midic)))
;;        (nreverse minmax)))))


;;; [Camilo] This is  probably wrong, but at least it terminates.
;;; TM: ajouté options et traitement de listes d'accords et d'approx

;; (defunp virt-fund ((ch chord) (cents fix>=0 (:value 50))) midic
;;     "Returns the highest midic which corresponds to a \"sum-abs\" harmonic distance
;; to the chord <ch> smaller than or equal to <cents>."
;;   (f->mc (fond-virt-f (mc->f ch) (round (/ 100 cents)))))



(defun virt-fund1 (chord cents)
  (car-mapcar (lambda (c) (fond-virt-f  chord (1- (cents->coef c)) ;;(round (/ 100 c))
                                        )) cents))


(defunp virt-fund ((chord list (:value '(6000 6400))) (cents fix>=0 (:value 50))
                   &optional (unit menu (:menu-box-list (("midic" . 1) ("freq". 2))))) midic
    "Returns the highest fundamental for which the notes of <chord> could be 
thought of as harmonic partials. In general, the lower the virtual 
fundamental, the more dissonant the chord. 

The argument <cents> determines the precision of the analysis. (a value of 
'0' would return the real fundamental; the larger the value the more 
approximate the result) 

If <chord> is a list of chords the box returns a list of virtual 
fundamentals.

The optional argument <unit> determines whether <chord> is entered and the 
result returned in midicents, ('midic'), or in hertz ('freq'). The argument
<cents> remains, however, unchanged."
  (if (= unit 2)
      (less-deep-mapcar 'virt-fund1 chord cents)
      (f->mc (less-deep-mapcar 'virt-fund1 (mc->f chord) cents))))


(defunp sumabs-minsd ((ch chord) (hicents fix>=0) (locents fix>=0)) midic
    "Like \"sumabs-mins\", but returns pairs (midic . distance)."
  (let ((mins ()) (cents hicents) (max (+ (l-min ch) 600)) (freqs (mc->f ch))
        minmax dist)
    (loop
      (setq minmax (flat (sumabs-minmax (- max 1200) ch max)))
      (setq max
            (cond ((null minmax) (- max 1100))
                  ((null (cdr minmax)) (min (- max 150) (+ (car! minmax) 100)))
                  (t (min (- max 150) (+ (car! (cdr minmax)) 100)))))
      (when (cdr minmax) (nextl minmax))
      (mapc
       (lambda (midic)
         (when (<= (setq dist (sum-abs (mc->f1 (car! midic)) freqs)) cents)
           (when (< dist locents) (return))
           (unless (cassq midic mins) (newl mins (cons midic dist)))
           (setq cents dist)))
       (nreverse minmax)))
    mins))

(defunp sumabs-mins ((ch chord) (hicents fix>=0) (locents fix>=0)) midic
    "Returns the highest midics (from high to low) which corresponds to a \"sum-abs\"
harmonic distance to the chord <ch> between <locents> and <hicents>."
  (mapcar #'car (sumabs-minsd ch hicents locents)))

;;(oct 1)(nreverse (sumabs-minmax (+ min (* (1- oct) 1200)) ch (+ min (* oct 1200)))))

;; (time (length (sumabs-allminmax -700 '(4100 4900 5100 5700 6000 6600 7000 7400) 2900)))
;; took 1800 ticks (30.000 seconds) to run. with remove-duplicates
;; took 93 ticks (1.550 seconds) to run. with unique-sorted
;;657

(defunp fv-sumabs-plot ((m0s midics?) (ch chord)) list
    "Returns the list of distances (according to <fun>) between each f0 and <ch>."
  (let ((freqs (mc->f (list! ch))))
    (mapcar (lambda (f0) (sum-abs f0 freqs)) (mc->f (list! m0s)))))

;; =============================================================================-======
(defun unique-sorted (l &optional (fun #'=))
  "Fast implementation of destructive \"unique\" (or \"remove-duplicates\"),
where equal elements according to <fun> are supposed to be contiguous in <l>."
  (prog1 l
    (while l
      (while (and (cdr l) (funcall fun (first l) (second l)))
        (rplacd l (cddr l)))
      (nextl l))))

;;;; THE END ;;;;
