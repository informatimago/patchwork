;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               point.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines the point operators.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-09 <PJB> Extracted from pw-macosx/ui.lisp.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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

(in-package "MCLGUI")

(declaim (declaration stepper))
(declaim (inline point-string point-h point-v
                 make-point add-points subtract-points
                 make-big-point add-big-points subtract-big-points
                 point-list
                 signed-byte-16-p))

(deftype point      () '(unsigned-byte 32))



(defun require-type (value type)
  (declare (stepper disable))
  (if (typep value type)
      value
      (error 'type-error :datum value :expected-type type)))

(defun signed-byte-16-p (value)
  (declare (stepper disable))
  (and (integerp value) (<= #x-8000 value #x7fff)))


(defun make-point (h &optional v)
  "
Points are couples of coordinates in the range [-32768,+32767].

Points are represented as integers with the vertical coordinate in the
bits of weight 31-16 and the horizontal coordinate in the bits of
weight 15-0.

RETURN:         If V is given then the encoded point #@(H V), else H.
"
  (declare (stepper disable))
  (if v
      (if (and (signed-byte-16-p h)
               (signed-byte-16-p v))
          (logior (logand #xffff h) (ash v 16))
          (make-point (max #x-8000 (min (round h) #x7fff))
                      (max #x-8000 (min (round v) #x7fff))))
      (if (consp h)
          (make-point (car h) (cdr h))
          (require-type h 'integer))))


(defun make-big-point (h &optional v)
  "
Big points are couples of coordinates.
Big points are represented as integers or cons cells.
"
  (declare (stepper disable))
  (if v
      (if (and (signed-byte-16-p h)
               (signed-byte-16-p v))
          (logior (logand #xffff h) (ash v 16))
          (cons (round h) (round v)))
      (if (consp h)
          (if (and (integerp (car h))
                   (integerp (cdr h)))
              h
              (cons (round (car h)) (round (cdr h))))
          (require-type h 'integer))))


(defun point-string (point)
  "
RETURN:         A string representation of POINT.

EXAMPLE:        (point-string (make-point 10 20)) --> \"#@(10 20)\"
"
  (declare (stepper disable))
  (format nil "#@(~A ~A)" (point-h point) (point-v point)))


(defun point-h (point)
  "
RETURN:         The horizontal coordinate of POINT.
"
  (declare (stepper disable))
  (if (consp point)
      (require-type (car point) 'integer)
      (let ((u (ldb (byte 16  0) point)))
        (if (< 32767 u)
            (- u 65536)
            u))))


(defun point-v (point)
  "
RETURN:         The vertical coordinate of POINT.
"
  (declare (stepper disable))
  (if (consp point)
      (require-type (cdr point) 'integer)
      (let ((u (ldb (byte 16 16) point)))
        (if (< 32767 u)
            (- u 65536)
            u))))



(defun point<= (point &rest other-points)
  "
RETURN:         T or NIL, whether the points are ordered by
                nondecreasing size in both coordinates.
                ⇔ ∀i ∀j 0<=i<j<(length pts)
                             ⇒ (and (<= (point-h (elt pts i))
                                         (point-h (elt pts j)))
                                     (<= (point-v (elt pts i))
                                         (point-v (elt pts j))))
                with pts = (cons point other-points).

POINT:          A point, represented by an integer.

OTHER-POINTS:   Zero or more other points represented by integers.
"
  (declare (stepper disable))
  (if (null other-points)
    t
    (let ((h (point-h point))
          (v (point-v point)))
      (dolist (p other-points t)
        (unless (and (<= h (setq h (point-h p)))
                     (<= v (setq v (point-v p))))
          (return nil))))))



(defun add-points (a b)
  "
RETURN:         The point that is the vectorial sum of points A and B.
"
  (declare (stepper disable))
  (make-point (+ (point-h a) (point-h b))
              (+ (point-v a) (point-v b))))

(defun subtract-points (a b)
  "
RETURN:         The point that is the vectorial difference of points A from B.
"
  (declare (stepper disable))
  (make-point (- (point-h a) (point-h b))
              (- (point-v a) (point-v b))))




(defun add-big-points (a b)
  "
RETURN:         The point that is the vectorial sum of points A and B.
"
  (declare (stepper disable))
  (make-big-point (+ (point-h a) (point-h b))
                  (+ (point-v a) (point-v b))))

(defun subtract-big-points (a b)
  "
RETURN:         The point that is the vectorial difference of points A from B.
"
  (declare (stepper disable))
  (make-big-point (- (point-h a) (point-h b))
                  (- (point-v a) (point-v b))))



(defun point-to-list (p)
  "
RETURN:         The point P as a list of coordinates (H V).
"
  (declare (stepper disable))
  (list (point-h p) (point-v p)))




(defun sharp-at-dispatch-reader-macro (stream subchar arg)
  "#@(x y) reads a Point."
  (declare (ignore subchar arg))
  (declare (stepper disable))
  (let ((coord  (read stream)))
    (if *read-suppress*
      (values)
      (values (apply (function make-point) coord)))))


(defmacro enable-sharp-at-reader-macro ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (set-dispatch-macro-character #\# #\@ (function sharp-at-dispatch-reader-macro))
     (values)))


(defmacro disable-sharp-at-reader-macro ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (set-dispatch-macro-character #\# #\@ nil)
     (values)))


;;;; THE END ;;;;
