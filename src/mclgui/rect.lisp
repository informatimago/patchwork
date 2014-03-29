;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               rect.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    QuickDraw rect functions.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-07-07 <PJB> Created.
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

(eval-when (:compile-toplevel :load-toplevel :execute) ; to be able to use #S in the same file.

  (defstruct (rect
               (:constructor %make-rect))
    (topLeft     (make-point 0 0) :type point)
    (bottomRight (make-point 0 0) :type point))

  (defmethod make-load-form ((object rect) &optional environment)
    (declare (ignore environment))
    ;; => creation-form[, initialization-form]
    `(%make-rect :topleft ,(rect-topleft object)
                 :bottomright ,(rect-bottomright object)))

  );;eval-when


(defun make-rect (left &optional top right bottom)
  "
The function MAKE-RECT can be called with either:
- a single RECT argument, a copy is then made;
- two points, topLeft and bottomRight;
- four coordinates: left, top, right, bottom.
"
   (cond
    ((and (null top) (null right) (null bottom))
     (copy-rect left))
    ((and (null right) (null bottom))
     (%make-rect :topleft left :bottomright top))
    (t
     (%make-rect :topleft (make-point left top) :bottomright (make-point right bottom)))))

(defun pt2rect (p1 p2)
  (make-rect (min (point-h p1) (point-h p2))
             (min (point-v p1) (point-v p2))
             (max (point-h p1) (point-h p2))
             (max (point-v p1) (point-v p2))))

(defun rect-to-list (rect)
  "
RETURN:         A list of two lists containing the coordinates of the
                topLeft and bottomRight points of the RECT rectangle.
"
  (list (point-to-list (rect-topleft rect))
        (point-to-list (rect-bottomright rect))))

;; (make-rect 1 2 3 4)
;; #S(rect :topleft 131073 :bottomright 262147)
;; (make-rect (make-point 1 2) (make-point 3 4))
;; #S(rect :topleft 131073 :bottomright 262147)
;; (make-rect (make-rect (make-point 1 2) (make-point 3 4)))
;; #S(rect :topleft 131073 :bottomright 262147)

(defun rect-left   (rect) (point-h (rect-topleft     rect)))
(defun rect-top    (rect) (point-v (rect-topleft     rect)))
(defun rect-right  (rect) (point-h (rect-bottomright rect)))
(defun rect-bottom (rect) (point-v (rect-bottomright rect)))

(defun rect-width  (rect) (- (point-h (rect-bottomright rect))
                             (point-h (rect-topleft rect))))
(defun rect-height (rect) (- (point-v (rect-bottomright rect))
                             (point-v (rect-topleft rect))))

(defun (setf rect-left)   (value rect)
  (setf  (rect-topleft     rect) (make-point value  (point-v (rect-topleft     rect))))
  value)

(defun (setf rect-top)    (value rect)
  (setf  (rect-topleft     rect) (make-point (point-h (rect-topleft     rect))  value))
  value)

(defun (setf rect-right)   (value rect)
  (setf  (rect-bottomright rect) (make-point value  (point-v (rect-bottomright rect))))
  value)

(defun (setf rect-bottom)    (value rect)
  (setf  (rect-bottomright rect) (make-point (point-h (rect-bottomright rect))  value))
  value)

(defun (setf rect-width)  (new-width rect)
  "Moves the botright point to accomodate the new width"
  (setf (rect-right rect) (+ (rect-left rect) new-width))
  new-width)

(defun (setf rect-height)  (new-height rect)
  "Moves the botright point to accomodate the new height"
  (setf (rect-bottom rect) (+ (rect-top rect) new-height))
  new-height)

(defun equal-rect (rect1 rect2)
  "
The EQUAL-RECT function returns T if RECT1 and RECT2 are equal and NIL
otherwise.

RECT1           A rectangle.
RECT2:          A rectangle.
"
  (and (= (rect-topleft     rect1) (rect-topleft     rect2))
       (= (rect-bottomright rect1) (rect-bottomright rect2))
       t))


(defun empty-rect-p (left &optional top right bot)
  "
The EMPTY-RECT-P function returns T if the rectangle specified by arg
is empty (contains no points) and NIL otherwise.  A rectangle is empty
if its bottom coordinate is less than or equal to the top or if the
right coordinate is less than or equal to the left.

LEFT, TOP, RIGHT, BOTTOM:
                These four arguments are used together to specify the
                rectangle. If only left is given, it should be a
                pointer to a rectangle record. If only two arguments
                are given, they should be points specifying the
                upper-left and lowerright coordinates of the
                rectangle. If all four arguments are given, they
                should be coordinates representing the left, top,
                right, and bottom of the rectangle.

"
  (if (and (null top) (null right) (null bot))
    (let ((l (rect-left   left))
          (r (rect-right  left))
          (a (rect-top    left))
          (b (rect-bottom left)))
      (not (and (< l r) (< a b))))
    (empty-rect-p (make-rect left top right bot))))


(defun rect-size   (rect)
  "
RETURN:        The size of the RECT, as a POINT.
"
  (if (empty-rect-p rect)
    (make-point 0 0)
    (make-point (- (point-h (rect-bottomright rect)) (point-h (rect-topleft rect)))
                (- (point-v (rect-bottomright rect)) (point-v (rect-topleft rect))))))


(defun rect-center (rect)
  "
RETURN:         The center point of the RECT.
"
  (make-point (round (+ (rect-left rect) (rect-right  rect)) 2)
              (round (+ (rect-top  rect) (rect-bottom rect)) 2)))



(defun offset-rect (rect h &optional v)
  "
The OFFSET-RECT function moves rectangle H to the right and V down.
It returns the destructively modified rectangle.

RECTANGLE:      A rectangle.

H:              Horizontal position.

V:              Vertical position.  If V is NIL (the default), H is
                assumed to represent a point.
"
  (let ((d  (make-point h v)))
    (setf (rect-topleft     rect) (add-points (rect-topleft     rect) d)
          (rect-bottomright rect) (add-points (rect-bottomright rect) d))
    rect))


(defun inset-rect (rect h &optional v)
  "
The INSET-RECT function shrinks or expands rectangle by H and V.  It
returns the destructively modified rectangle.  If H and V are
positive, the left and right sides and the top and bottom move toward
the center.  If H and V are negative, the sides move outward.

RECTANGLE:      A rectangle.

H:              Horizontal position.

V:              Vertical position.  If V is NIL (the default), H is
                assumed to represent a point.
"
  (let ((d (make-point h v)))
    (setf (rect-topleft     rect) (add-points      (rect-topleft     rect) d)
          (rect-bottomright rect) (subtract-points (rect-bottomright rect) d))
    rect))


(defun intersect-rect (rect1 rect2 dest-rect)
  "
The INTERSECT-RECT function stores in DEST-RECT the rectangle created
by the intersection of RECT1 and RECT2 and returns DEST-RECT.  A
single rectangle may be passed as DEST-RECT and as RECT1 or RECT2,
making it unnecessary to allocate one extra rectangle.

RECT1:          A rectangle.

RECT2:          A rectangle.

DEST-RECT:      A rectangle structure used to hold the intersection of
                RECT1 and RECT2.
"
  (let ((l1 (rect-left   rect1))
        (t1 (rect-top    rect1))
        (r1 (rect-right  rect1))
        (b1 (rect-bottom rect1))
        (l2 (rect-left   rect2))
        (t2 (rect-top    rect2))
        (r2 (rect-right  rect2))
        (b2 (rect-bottom rect2)))
    (if (or (<= r1 l2) (<= r2 l1)
            (<= b1 t2) (<= b2 t1))
      (setf (rect-left   dest-rect) 0
            (rect-right  dest-rect) 0
            (rect-top    dest-rect) 0
            (rect-bottom dest-rect) 0)
      ;; (and (< l1 r2) (< l2 r1)
      ;;      (< t1 b2) (< t2 b1))
      (setf (rect-left   dest-rect) (max l1 l2)
            (rect-right  dest-rect) (min r1 r2)
            (rect-top    dest-rect) (max t1 t2)
            (rect-bottom dest-rect) (min b1 b2)))
    dest-rect))


(defun union-rect (rect1 rect2 dest-rect)
  "
The UNION-RECT function stores in DEST-RECT the rectangle created by
the union of RECT1 and RECT2 and returns DEST-RECT.  A single
rectangle may be passed as DEST-RECT and as RECT1 or RECT2, making it
unnecessary to allocate one extra rectangle.

RECT1:          A rectangle.

RECT2:          A rectangle.

DEST-RECT:      A rectangle structure used to hold the intersection of
                RECT1 and RECT2.
"
  (let ((l1 (rect-left   rect1))
        (t1 (rect-top    rect1))
        (r1 (rect-right  rect1))
        (b1 (rect-bottom rect1))
        (l2 (rect-left   rect2))
        (t2 (rect-top    rect2))
        (r2 (rect-right  rect2))
        (b2 (rect-bottom rect2)))
    (setf (rect-left   dest-rect) (min l1 l2)
          (rect-right  dest-rect) (max r1 r2)
          (rect-top    dest-rect) (min t1 t2)
          (rect-bottom dest-rect) (max b1 b2))
    dest-rect))


(defun point-in-rect-p (rect h &optional v)
  "

The POINT-IN-RECT-P function returns T if the point specified by H and
V is inside rectangle; otherwise, it returns NIL.

RECTANGLE:      A rectangle.

H:              Horizontal position.

V:              Vertical position.  If V is NIL (the default), H is
                assumed to represent a point.
"
  (let ((p (make-point h v))
        (l (rect-left   rect))
        (a (rect-top    rect))
        (r (rect-right  rect))
        (b (rect-bottom rect)))
    (and (<= l (point-h p) r)
         (<= a (point-v p) b)
         t)))


(defun points-to-rect (point1 point2 dest-rect)
  (let ((x1 (point-h point1))
        (y1 (point-v point1))
        (x2 (point-h point2))
        (y2 (point-v point2)))
    (setf (rect-left   dest-rect) (min x1 x2)
          (rect-right  dest-rect) (min y1 y2)
          (rect-top    dest-rect) (max x1 x2)
          (rect-bottom dest-rect) (max y1 y2))
    dest-rect))


(defun point-to-angle (rect h &optional v)
  (let ((p      (make-point h v))
        (size   (rect-size rect))
        (center (rect-center rect)))
    (if (or (zerop (point-h size)) (zerop (point-v size))
            (= p center))
      0
      (let* ((pv  (subtract-points p center))
             (d   (sqrt (coerce (+ (* (point-h pv) (point-h pv))
                                   (* (point-v pv) (point-v pv)))
                                'double-float)))
             (cos-theta (/ (- (point-v pv)) d))
             (angle     (acos cos-theta)))
        (when (minusp (point-h pv))
          (setf angle (- (* 2 pi) angle)))
        (values (round angle (/ pi 180)))))))


(defun test/point-to-angle ()
  (flet ((test (r x y angle)
           (assert (= angle (point-to-angle r x y))
                   ()
                   "rect = ~S ; point = ~S ; expected angle = ~S ; obtained angle = ~S~%"
                   (rect-to-list r)
                   (point-to-list (make-point x y))
                   angle
                   (point-to-angle r  x y))))
    
    (let ((r (make-rect 0 0 10 10)))
      (test r  5   0   0)
      (test r 10 -10  18)
      (test r 10   0  45)
      (test r 10   5  90)
      (test r 10  10 135)
      (test r  5  10 180)
      (test r  0  10 225)
      (test r  0   5 270)
      (test r  0   5 270)
      (test r  0   0 315)
      (test r  0  -5 333)
      (test r  5   5   0)                 ; degenerate: center
      (test (make-rect 0 0 0 0)  10 10 0) ; degenerate: empty rect
      (test (make-rect 0 0 0 10) 10 10 0) ; degenerate: empty flat rect
      (test (make-rect 0 0 10 0) 10 10 0) ; degenerate: empty flat rect
      ))
  :success)

#+test (test/point-to-angle)

;;;; THE END ;;;;
