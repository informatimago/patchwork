;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               crime-fm.lisp
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
(in-package "EPW")


(defvar *maxorder* 25)

(defun fix (x) (truncate x))
(defun ceil (x) (ceiling x))
(defun fixp (x) (integerp x))
(defun add1 (x) (1+ x))

(defun fmSpec (c m imod &optional order)
  (let ((spec) s p q MI)
    (declare (ignorable q))
    (setq MI imod)
    (if (floatp imod) (setq imod (ceil imod)) (setq imod (fix imod)))
    (if (null order)
                                        ;(setq order (car order))
        (setq order (add1 imod)) )
    (setq order (min order *maxorder*))
    (setq spec `((, c . 0 )))
    (for (i 1 1 order)
      (newl spec (cons (- c (* i m)) (- i)))
      (setq  spec (nconc spec (list (cons (+ c (* i m)) i))))
      (when (and (null p) (< (caar spec) 0))
        (setq p spec)))
    (setq s spec)
    (while s
                                        ;(when (and (null p) (>= (caar s) 0))
                                        ;      (setq p q))
      (cond
        ( (< (cdar s) 0)
          (if (oddp (cdar s))
              (rplacd (car s) (- (bessel MI (abs (cdar s)))))
              (rplacd (car s) (bessel MI (abs (cdar s)))))
          (when (< (caar s) 0)
            (rplaca (car s) (- (caar s)))
            (rplacd (car s) (- (cdar s)))))
        ( t
          (rplacd (car s) (bessel MI (cdar s)))))
      (setq q s)
      (nextl s))
    (setq spec
          (if (not p)
              spec
              (fmMerge (cdr p)
                       (progn (rplacd p ()) (nreverse spec)))))
    (mapc (lambda (comp)
            (rplacd comp (abs (cdr comp))))
          spec)
    (when (<= (caar spec) 0) (nextl spec))
    (fmNormalize spec)
    spec))


(defun fmNormalize (spec)
  (let ((etot 0) ratio)
    (mapc (lambda (x) (setf  etot (+ etot (cdr x))))
          spec)
    (setq ratio (/ 1000.0 etot))
    (mapc (lambda (x)
            (rplacd x (fix (* (cdr x) ratio))))
          spec)
    spec))



(defparameter *bessel*
  (make-array
   '(26 25)
   :initial-contents
   '((1000 765 223 -260 -397 -177 150 300 171 -90 -245 -171 47 206 171 -14 -174 -169 -13 146 167 36 -120 -162 -56)
     (0 440 576 339 -66 -327 -276 -4 234 245 43 -176 -223 -70 133 205 90 -97 -187 -105 66 171 117 -39 -154)
     (0 114 352 436 364 46 -242 -301 -112 144 254 139 -84 -217 -152 41 186 158 -7 -157 -160 -20 131 158 43)
     (0 19 128 309 430 364 114 -167 -291 -180 58 227 195 3 -176 -194 -43 134 186 72 -98 -174 -93 67 161)
     (0 2 33 132 281 391 357 157 -105 -265 -219 -15 182 219 76 -119 -202 -110 69 180  130 -29 -156 -141 -3)
     (0 0 7 43 132 261 362 347 185 -55 -234 -238 -73 131 220 130 -57 -187 -155 3 151 163 36 -116 -162)
     (0 0 1 11 49 131 245 339 337 204 -14 -201 -243 -118 81 206 166 0 -155 -178 -55 107 173 90 -64)
     (0 0 0 2 15 53 129 233 320 327 216 18 -170 -240 -150 34 182 187 51 -116 -184 -102 58 163 130)
     (0 0 0 0 4 18 56 127 223 305 317 224 45 -141 -231 -173 -7 153 195 92 -73 -175 -136 8 140)
     (0 0 0 0 0 5 21 58 126 214 291 308 230 66 -114 -220 -189 -42 122 194 125 -31 -157 -157 -36)
     (0 0 0 0 0 1 6 23 60 124 207 280 300 233 85 -90 -206 -199 -73 91 186 148 7 -132 -167)
     (0 0 0 0 0 0 2 8 25 62 123 201 270 292 235 99 -68 -191 -204 -98 61 173 164 42 -103)
     (0 0 0 0 0 0 0 2 9 27 63 121 195 261 285 236 112 -48 -176 -205 -118 32 156 173 72)
     (0 0 0 0 0 0 0 0 3 10 28 64 120 190 253 278 236 122 -30 -161 -204 -135 6 137 176)
     (0 0 0 0 0 0 0 0 1 3 11 30 65 118 185 246 272 236 131 -15 -146 -200 -148 -17 118)
     (0 0 0 0 0 0 0 0 0 1 4 13 31 65 117 181 239 266 235 138 0 -132 -195 -158 -38)
     (0 0 0 0 0 0 0 0 0 0 1 5 13 32 66 116 177 234 261 234 145 12 -118 -189 -166)
     (0 0 0 0 0 0 0 0 0 0 0 1 5 14 33 66 114 173 228 255 233 150 23 -105 -183)
     (0 0 0 0 0 0 0 0 0 0 0 0 2 6 15 34 66 113 170 223 251 231 154 34 -93)
     (0 0 0 0 0 0 0 0 0 0 0 0 0 2 6 16 35 67 112 167 218 246 229 158 43)
     (0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 7 17 36 67 111 164 214 242 228 161)
     (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 3 7 18 36 67 110 162 210 238 226)
     (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 3 8 18 37 67 109 159 206 234)
     (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 3 8 19 38 67 108 157 203)
     (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 3 9 19 38 67 107 155)
     (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 4 9 20 39 67 106))))


(defun bessel (imod i)
  (if (fixp imod)
      (aref *bessel* i imod)
      (let ((i1 (aref *bessel* i (fix imod))) (i2 (aref *bessel* i (ceil imod))))
        (fix (+ i1 (* (- imod (fix imod)) (- i2 i1)))))))

(defun fmMerge   (f1 f2)
  (let ((r (list ())))
    (fmMerge2 r f1 f2)
    (cdr r)))


(defun fmMerge2 (r f1 f2)
  (cond
    ((null f1) (rplacd r f2))
    ((null f2) (rplacd r f1))
    ((< (caar f1) (caar f2))
     (rplacd r f1)
     (fmMerge2 f1 (cdr f1) f2))
    ((= (caar f1) (caar f2))
     (rplaca f1 (cons (caar f1) (+ (cdar f1) (cdar f2))))
     (rplacd r f1)
     (fmMerge2 f1 (cdr f1) (cdr f2)))
    (t (rplacd r f2)
       (fmMerge2 f2 f1 (cdr f2)))))


(defun fm (c m i)
  (let ((spec (fmspec c m i)))
    (cons (mapcar #'car spec)
          (mapcar (lambda (x) (round (* (/ 127 3.0) (log (cdr x) 10)))) spec))))


(defunp fm/p ((car fix>0 (:value 5)) (mod fix>0 (:value 7)) (ind fix/float (:value 4))) list
    "Computes a FM spectrum. Outputs a list of partials and a list of dyns"
  (let ((spec (fmspec car mod ind)))
    (cons (mapcar #'car spec)
          (mapcar (lambda (x) (round (* (/ 127 3.0) (log (cdr x) 10)))) spec))))


(defunp fm-chord ((carrier midics?) (modul midics? (:value 6600)) (index fix/float (:value 1))
                  &optional
                  (unit menu (:menu-box-list (("midic" . 1) ("freq". 2))))) ch-ob
    "Computes a FM spectrum. Outputs a chord object"
  (when (= unit 1)
    (setf carrier (mc->f carrier) modul (mc->f modul)))
  (let* ((spec (fmspec carrier modul index)) (slength (length spec)))
    (pw::mk-chord
     (f->mc (epw::band-filter (mapcar #'car spec) '((15.0 20000.0))))
     (make-list slength :initial-element 50)
     (make-list slength :initial-element 0)
     (mapcar (lambda (x) (round (* (/ 127 3.0) (log (cdr x) 10)))) spec) )))


(defclass C-fm-box (pw::C-patch&popUp)
  ((pw::local-menu :initform '(("Midics" "M") ("Velocity" "V") ("Frequency" "F")("Chord object" "C")) :accessor pw::local-menu)
   (pw::default-char :initform "C" :accessor pw::default-char)))

(defmethod do-menu-action ((self C-fm-box) str)
  (declare (ignorable str))
  (call-next-method))

(defmethodp fm-spec C-fm-box ((carrier midics?) (modul midics? (:value 6600)) (index fix/float (:value 1))
                              (unit menu (:menu-box-list (("midic" . 1) ("freq". 2))))) ch-ob
    "Computes a FM spectrum. index is an int or float value between 1 and 25
\(float values are interpolated).<carrier> and <modul> can be midics or freqs (default midics)
depending on the value of the <unit> argument. Outputs either a chord object (default) or
midics, or freqs, or velocities depending on the popup menu"
  (when (= unit 1)
    (setf carrier (mc->f carrier) modul (mc->f modul)))
  (let* ((spec (fmspec carrier modul index))
         (vel (mapcar (lambda (x) (round (* (/ 127 3.0) (if (<= (cdr x) 0.0) 0 (log (cdr x) 10))))) spec))
         (spec (epw::band-filter (mapcar #'car spec) '((15.0 20000.0))))
         (slength (length spec)))
    (cond ((string= (pw::current-str self) "M") (f->mc spec))
          ((string= (pw::current-str self) "V") vel)
          ((string= (pw::current-str self) "F") spec)
          ((string= (pw::current-str self) "C")
           (pw::mk-chord
            (f->mc spec)
            (make-list slength :initial-element 50)
            (make-list slength :initial-element 0)
            vel)))))



;;(pw::pw-addmenu-fun (pw::the-user-menu) 'fm-spec 'C-fm-box)

;;;; THE END ;;;;
