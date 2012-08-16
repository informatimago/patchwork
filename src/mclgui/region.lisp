;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               region.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Regions.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-15 <PJB> Created.
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






(defgeneric window-open-region (window)
  (:documentation "RETURN: NIL or the open region of the window"))
(defgeneric (setf window-open-region) (new-region window)
  (:documentation "DO: Sets the open-region of the window.
NEW-REGION: a region or NIL."))


(defun get-clip (region)
  region)

(defun set-clip (region)
  region)



(defvar *temp-rgn* nil)


(defmacro with-temp-rgns ((&rest rgn-vars) &body body)
  `(let ,(mapcar (lambda (var) `(,var (new-region))) rgn-vars)
     (unwind-protect
         (progn ,@body)
       ,@(mapcar (lambda (var) `(dispose-region ,var)) rgn-vars))))


(defmacro with-hilite-mode (&body body)
  (niy with-hilite-mode body)
  `(progn
     (niy with-hilite-mode ',body)
     ,@body)
  #-(and)
  `(progn
     (let ((byte (require-trap #_lmgethilitemode)))
       (require-trap #_lmsethilitemode (%ilogand2 #x7f byte)))
     ,@body))


(defmacro with-clip-region (region &body body)
  (let ((rgn1 (gensym))
        (rgn2 (gensym)))    
    `(with-temp-rgns (,rgn1 ,rgn2)
       (get-clip ,rgn1)
       (intersect-region ,rgn1 ,region ,rgn2)
       (unwind-protect
           (progn
             (set-clip ,rgn2)
             ,@body)
         (set-clip ,rgn1)))))



;;; Regions

(eval-when (:compile-toplevel :load-toplevel :execute) ; to be able to use #S in the same file.

  (defstruct (region
               (:predicate regionp)
               (:copier nil))
    (bounds            (make-rect 0 0 0 0) :type rect)
    (segments          #()                 :type vector))

  
  (defmethod make-load-form ((object region) &optional environment)
    (declare (ignore environment))
    ;; => creation-form[, initialization-form]
    `(make-region :bounds ,(region-bounds object)
                  :segments ,(region-segments object)))

  );;eval-when





;; 0000000000111111111122222222223
;; 0123456789012345678901234567890
;;        *****************          0
;;        *****************          1
;; *******************************   2
;; *******************************   3
;; ********         **************   4
;; ********         **************   5
;; ****************************      6
;; ****************************      7
;;                  *******          8
;;                  *******          9
;;                                  10
;; 0123456789012345678901234567890
;; 0000000000111111111122222222223


;;----------------------------------------------------------------------

(defun rectangular-region-p (region)
  (zerop (length (region-segments region))))


(defun expanded-segments (region)
  (if (rectangular-region-p region)
    (let ((bounds (region-bounds region)))
      (vector (cons (rect-top    bounds) (vector (rect-left bounds) (rect-right bounds)))
              (cons (rect-bottom bounds) #())))
    (region-segments region)))


;; (defun expanded-inversion-points (region)
;;   (if (rectangular-region-p region)
;;     (let ((bounds (region-bounds region)))
;;       (vector (vector (rect-top    bounds) (rect-left bounds) (rect-right bounds))
;;               (vector (rect-bottom bounds) (rect-left bounds) (rect-right bounds))))
;;     (region-inversion-points region)))

;; (set-rect-region (new-region) 10 10 200 100)
;; #S(region :bounds #S(rect :topleft 655370 :bottomright 6553800)
;;           :inversion-points #(#(10 10 200) #(100 10 200)))
;; (expanded-inversion-points (set-rect-region (new-region) 10 10 200 100))
;; #(#(10 10 200) #(100 10 200))


;;----------------------------------------------------------------------
;; segments-operate works on vectors of segments, where even-indexed
;; elements are start positions of the segments, and odd-indexed
;; elements are end positions of the segments.
;; [a,b],[c,d],[e,f] = #(a b c d e f)
;;

(defun segment-writer (segments)
  "
SEGMENTS:  An adjustable vector with fill-pointer.
RETURN:    A function taking start and end of a segment, that
           appends them to the adjustable vector SEGMENTS.
"
  (lambda (a b)
      (let ((i (1- (length segments))))
        (if (and (<= 0 i) (= (aref segments i) a))
          (setf (aref segments i) b)
          (progn
            (vector-push-extend a segments)
            (vector-push-extend b segments))))))


(defun segment-reader (segments)
  "
SEGMENTS:  A vector containing an even number of elements, start and
           end of each segment. eg. [a,b],[c,d],[e,f] = #(a b c d e f)
RETURN:    A function of no arguments, that returns two values: the
           next segment limit, and whether the first value is an end
           limit.
"
  (let ((i 0))
    (lambda ()
        (when (< i (length segments))
          (let ((j i))
            (incf i)
            (values (aref segments j) (oddp j)))))))


(defun segments-operate (operator s1 s2)
  "
OPERATOR:       A binary boolean function.
S1:             A segments vector..
S2:             A segments vector.
RETURN:         A segments vector, that is the combination bythe
                OPERATOR of the segments in S1 and S2.
"
  (let* ((s  (make-array (+ (length s1) (length s2)) :adjustable t :fill-pointer 0))
         (r1 (segment-reader s1))
         (r2 (segment-reader s2))
         (w  (segment-writer s))
         (c1 -1) v1
         (c2 -1) v2
         nc1 nv1
         nc2 nv2)
    (multiple-value-setq (nc1 nv1) (funcall r1))
    (multiple-value-setq (nc2 nv2) (funcall r2))
    (loop :do
      ;; (print (list '<- c1 c2 (list v1 v2 '-> (funcall operator v1 v2)) '/ nc1 nv1 '/ nc2 nv2))
      (setf c1 c2
            v1 nv1
            v2 nv2)
      (cond
        ((null nc1)
         (if (null nc2)
           (loop-finish)
           (progn
             (setf c2 nc2)
             (multiple-value-setq (nc2 nv2) (funcall r2)))))
        ((or (null nc2) (< nc1 nc2))
         (setf c2 nc1)
         (multiple-value-setq (nc1 nv1) (funcall r1)))
        ((< nc2 nc1)
         (setf c2 nc2)
         (multiple-value-setq (nc2 nv2) (funcall r2)))
        (t
         (setf c2 nc1)
         (multiple-value-setq (nc1 nv1) (funcall r1))
         (multiple-value-setq (nc2 nv2) (funcall r2))))
      ;; (print (list '<- c1 c2 (list v1 v2 '-> (funcall operator v1 v2)) '/ nc1 nv1 '/ nc2 nv2))
      (when (and (< c1 c2) (funcall operator v1 v2))
        (funcall w c1 c2)))
    ;; (format-trace "segments-operate" s1 s2 :-> s)
    s))


(defun test/segments-operate ()

  (assert (equalp (loop
                     with s = (make-array 10 :adjustable t :fill-pointer 0)
                     with w = (segment-writer s)
                     for (a b) in '((1 2) (3 4) (4 5) (6 7))
                     do (funcall w a b)
                     finally (return s))
                  #(1 2 3 5 6 7)))

  (assert (equalp (loop with r = (segment-reader #(1 3 5 7 9 10 11 13 14 15))
                     with c = 0 with v = 0
                     do (multiple-value-setq (c v) (funcall r))
                     collect (list c v)
                     while c)
                  '((1 nil) (3 t) (5 nil) (7 t) (9 nil) (10 t) (11 nil)
                    (13 t) (14 nil) (15 t) (nil nil))))

  (assert (equalp (segments-operate (lambda (a b) (and a b))
                                    #(1 3 5 7 9 10 11 13 14 15)
                                    #(0 2 4 6 8 11 12 13))
                  #(1 2 5 6 9 10 12 13)))

  (assert (equalp (segments-operate (lambda (a b) (or a b))
                                    #(1 3 5 7 9 10 11 13 14 15)
                                    #(0 2 4 6 8 11 12 13))
                  #(0 3 4 7 8 13 14 15)))

  (assert (equalp (segments-operate (lambda (a b) (or (and a (not b)) (and (not a) b)))
                                    #(1 3 5 7 9 10 11 13 14 15)
                                    #(0 2 4 6 8 11 12 13))
                  #(0 1 2 3 4 5 6 7 8 9 10 12 14 15)))

  (assert (equalp (segments-operate (lambda (a b) (and a (not b)))
                                    #(1 3 5 7 9 10 11 13 14 15)
                                    #(0 2 4 6 8 11 12 13))
                  #(2 3 6 7 11 12 14 15)))

  :success)


;;----------------------------------------------------------------------
;; update-segments converts inversion-points into segments.
;;

#-(and)
(defun update-segments (segments inversion-points)
  "
SEGMENTS:           A segments vector.
INVERSION-POINTS:   A vector of inversion points (the first element is
                    the vertical coordinate).
RETURN:             A new segment vector made from SEGMENTS and
                    INVERSION-POINTS.
"
  (if (zerop (length segments))
    (loop
      :with len =  (length inversion-points)
      :with segments = (make-array (1- len))
      :for i :from 1 :below len :by 2
      :for j :from -1
      :do (setf (aref segments (incf j)) (aref inversion-points i)
                (aref segments (incf j)) (aref inversion-points (1+ i)))
      :finally (return segments))
    (flet ((segment-xor (a1 b1  a2 b2)
             (cond
               ((= a2 b1) (values a1 b2 nil nil))
               ((= a1 b2) (values a2 b1 nil nil))
               ((= a1 a2) (values (min b1 b2) (max b1 b2) nil nil))
               ((= b1 b2) (values (min a1 a2) (max a1 a2) nil nil))
               ((< b1 a2) (values a1 b1 a2 b2))
               ((< b2 a1) (values a2 b2 a1 b1))
               ;; a2<=b1 & a1<=b2
               ((< a1 a2 b2 b1) (values a1 a2 b2 b1))
               ((< a2 a1 b1 b2) (values a2 a1 b1 b2))
               ((< a1 a2 b1 b2) (values a1 a2 b1 b2))
               (t               (values a2 a1 b2 b1)))))
      (loop
        :with ilen-1 = (1- (length inversion-points))
        :with slen-1 = (1- (length segments))
        :with new = (make-array (1+ slen-1) :adjustable t :fill-pointer 0)
        :with i = 0
        :with s = -1
        :with a1 = (aref inversion-points (incf i))
        :with b1 = (aref inversion-points (incf i))
        :with a2 = (aref segments (incf s))
        :with b2 = (aref segments (incf s))
        :do (multiple-value-bind (an bn ar br) (segment-xor a1 b1 a2 b2)
              (if (null ar)
                (if (< i ilen-1)
                  (if (or (<= slen-1 s)
                          (< (aref inversion-points (1+ i)) (aref segments (1+ s))))
                    (setf a1 an
                          b1 bn
                          a2 (aref inversion-points (incf i))
                          b2 (aref inversion-points (incf i)))
                    (setf a1 an
                          b1 bn
                          a2 (aref segments (incf s))
                          b2 (aref segments (incf s))))
                  (if (< s slen-1)
                    (setf a1 an
                          b1 bn
                          a2 (aref segments (incf s))
                          b2 (aref segments (incf s)))
                    (progn
                      (unless (= an bn)
                        (vector-push-extend an new)
                        (vector-push-extend bn new))
                      (loop-finish))))
                (progn
                  (unless (= an bn)
                    (vector-push-extend an new)
                    (vector-push-extend bn new))
                  (if (< i ilen-1)
                    (if (or (<= slen-1 s)
                            (< (aref inversion-points (1+ i)) (aref segments (1+ s))))
                      (setf a1 ar
                            b1 br
                            a2 (aref inversion-points (incf i))
                            b2 (aref inversion-points (incf i)))
                      (setf a1 ar
                            b1 br
                            a2 (aref segments (incf s))
                            b2 (aref segments (incf s))))
                    (if (< s slen-1)
                      (setf a1 ar
                            b1 br
                            a2 (aref segments (incf s))
                            b2 (aref segments (incf s)))
                      (progn
                        (unless (= ar br)
                          (vector-push-extend ar new)
                          (vector-push-extend br new))
                        (loop-finish)))))))
        :finally (return new)))))


#-(and)
(defun test/update-segments ()  
  (assert (equalp (loop
                    :for invpt :across #(#( 0   7    23)
                                         #( 2 0 7    23 30)
                                         #( 4   7 17      )
                                         #( 6   7 17 27 30)
                                         #( 8 0   17 23 27)
                                         #(10     17 23   ))
                    :for s = (subseq invpt 1) :then (update-segments s invpt)
                    :collect (cons (aref invpt 0) s))
                  '((0 . #(7 23))
                    (2 . #(0 30))
                    (4 . #(0 7 17 30))
                    (6 . #(0 27))
                    (8 . #(17 23))
                    (10 . #()))))
  :success)

;; (loop
;;   :for invpt :across #(#( 0   7    23)
;;                        #( 2 0 7    23 30)
;;                        #( 4   7 17      )
;;                        #( 6   7 17 27 30)
;;                        #( 8 0   17 23 27)
;;                        #(10     17 23   )
;;                        #(20   7    23)
;;                        #(22 0 7    23 30)
;;                        #(24   7 17      )
;;                        #(26   7 17 27 30)
;;                        #(28 0   17 23 27)
;;                        #(30     17 23   ))
;;   :for s = (subseq invpt 1) :then (update-segments s invpt)
;;   :collect (cons (aref invpt 0) s))
;; 
;; (equalp (inversion-points-from-segments #((-1 . #())
;;                                           (0 . #(7 23))
;;                                           (2 . #(0 30))
;;                                           (4 . #(0 7 17 30))
;;                                           (6 . #(0 27))
;;                                           (8 . #(17 23))
;;                                           (10 . #())
;;                                           (20 . #(7 23))
;;                                           (22 . #(0 30))
;;                                           (24 . #(0 7 17 30))
;;                                           (26 . #(0 27))
;;                                           (28 . #(17 23))
;;                                           (30 . #())
;;                                           (32 . #())))
;;         #(#( 0   7    23)
;;           #( 2 0 7    23 30)
;;           #( 4   7 17      )
;;           #( 6   7 17 27 30)
;;           #( 8 0   17 23 27)
;;           #(10     17 23   )
;;           #(20   7    23)
;;           #(22 0 7    23 30)
;;           #(24   7 17      )
;;           #(26   7 17 27 30)
;;           #(28 0   17 23 27)
;;           #(30     17 23   )))

;;----------------------------------------------------------------------
;; 
;;

#-(and)
(defun inversion-points-from-segments (segments)
  "
SEGMENTS:  A vector of conses (vertical-coordinate . segments-vector).
RETURN:    An inversion-points vector of vectors; bounds.
"
  (flet ((skip-empty (i)
           (loop
             :while (and (< i (length segments))
                         (zerop (length (cdr (aref segments i)))))
             :do (incf i)
             :finally (return i)))
         (done (i)
           (<= (length segments) i)))
    (let ((invpts (make-array (length segments) :adjustable t :fill-pointer 0))
          (top    nil)
          (left   nil)
          (bottom nil)
          (right  nil)
          (i (skip-empty 0)))
      (unless (done i)
        (setf top (car (aref segments i)))
        (loop
          :until (done i)
          :do (setf bottom (car (aref segments i)))
          
          
          ))
      (values invpts
              (make-rect (or left 0) (or top 0) (or right 0) (or bottom 0))))))


;;----------------------------------------------------------------------

(defun segments-trim (segments)
  (let ((start 0)
        (end (length segments)))
   (loop
     :while (and (< start (length segments))
                 (zerop (length (cdr (aref segments start)))))
     :do (incf start))
   (loop
     :do (decf end)
     :while (and (< 0 end)
                 (zerop (length (cdr (aref segments (1- end)))))
                 (zerop (length (cdr (aref segments end)))))
     :finally (incf end))
   (nsubseq segments start end)))


(defun segments-bounds (segments)
  "
SEGMENTS:
RETURN:         The bounds rect.
"
  (if (zerop (length segments))
    (make-rect 0 0 0 0)
    (let* ((first  (aref segments 0))
           (last   (aref segments (1- (length segments))))
           (top    (car first))
           (bottom (car last))
           (left   most-positive-fixnum)
           (right  most-negative-fixnum))
      (loop
        :for row :across segments
        :for segs = (cdr row)
        :do (unless (zerop (length segs))
              (setf left  (min left  (aref segs 0))
                    right (max right (aref segs (1- (length segs)))))))
      (make-rect left top right bottom))))


(defun test/segments-trim ()
  (assert (equalp (segments-trim #((10 . #(10 100))
                                   (20 . #())))
                  #((10 . #(10 100))
                    (20 . #()))))
  (assert (equalp (segments-trim #((0 . #())
                                 (1 . #())
                                 (10 . #(10 100))
                                 (50 . #(10 100 200 400))
                                 (60 . #(10 50 350 400))
                                 (80 . #(10 100 200 400))
                                 (100 . #(200 400))
                                 (150 . #())
                                 (200 . #())
                                 (201 . #())))
                #((10 . #(10 100))
                  (50 . #(10 100 200 400))
                  (60 . #(10 50 350 400))
                  (80 . #(10 100 200 400))
                  (100 . #(200 400))
                  (150 . #()))))
  :success)


;;----------------------------------------------------------------------
;; region-operate implements the region operations: intersection,
;; union, difference, xor.
;;


(defun region-operate-not-easy (op r1 r2 rd)
  "
NOTE:           This function computes the operation on the inversion
                points of r1 and r2.
OP:             A keyword denoting an operation, one of :intersection
                :difference :union :xor.
R1:             A region.
R2:             A region.
RD:             A region.
DO:             Compute the operation OP between R1 and R2, and set RD
                to the result of that operation.
"
  (check-type op (member :intersection :difference :union :xor :horizontal-inset))
  (flet ((operate (op v s1 s2 res)
           (vector-push-extend
            (cons v (case op
                      (:intersection     (segments-operate (lambda (a b) (and a b))        s1 s2))
                      (:difference       (segments-operate (lambda (a b) (and a (not b)))  s1 s2))
                      (:union            (segments-operate (lambda (a b) (or a b))         s1 s2))
                      (:xor              (segments-operate (function xor)                  s1 s2))
                      ;; (:horizontal-inset (segments-horizontal-inset s1 s2))
                      ))
            res)))
    (loop
      :with segs1 = (expanded-segments r1)
      :with segs2 = (expanded-segments r2)
      :with r = (make-array (+ (length segs1) (length segs2)) :adjustable t :fill-pointer 0)
      :with i1 = 0
      :with i2 = 0
      :with ip1 = (aref segs1 i1) :with v1 = (car ip1)
      :with ip2 = (aref segs2 i2) :with v2 = (car ip2)
      :with s1 = #()
      :with s2 = #()
      :do (cond
            ((and (null v1) (null v2))
             (loop-finish))
            ((or (null v2) (and v1 (< v1 v2)))
             (setf s1 (cdr ip1))
             (operate op v1 s1 s2 r)
             (incf i1)
             (if (< i1 (length segs1))
               (setf ip1 (aref segs1 i1)
                     v1  (car ip1))
               (setf v1 nil)))
            ((or (null v1) (< v2 v1))
             (setf s2 (cdr ip2))
             (operate op v2 s1 s2 r)
             (incf i2)
             (if (< i2 (length segs2))
               (setf ip2 (aref segs2 i2)
                     v2  (car ip2))
               (setf v2 nil)))
            (t
             (setf s1 (cdr ip1)
                   s2 (cdr ip2))
             (operate op v2 s1 s2 r)
             (incf i1)
             (if (< i1 (length segs1))
               (setf ip1 (aref segs1 i1)
                     v1  (car ip1))
               (setf v1 nil))
             (incf i2)
             (if (< i2 (length segs2))
               (setf ip2 (aref segs2 i2)
                     v2  (car ip2))
               (setf v2 nil))))
      :finally (let ((r (segments-trim r)))
                 (setf (region-bounds   rd) (segments-bounds r)
                       (region-segments rd) r)
                 (return rd)))))


#-(and)
(defun region-operate-not-easy (op r1 r2 rd)
  "
NOTE:           This function computes the operation on the inversion
                points of r1 and r2.
OP:             A keyword denoting an operation, one of :intersection
                :difference :union :xor.
R1:             A region.
R2:             A region.
RD:             A region.
DO:             Compute the operation OP between R1 and R2, and set RD
                to the result of that operation.
"
  (check-type op (member :intersection :difference :union :xor :horizontal-inset))
  (flet ((operate (op v s1 s2 res)
           (vector-push-extend
            (cons v (case op
                      (:intersection     (segments-operate (lambda (a b) (and a b))        s1 s2))
                      (:difference       (segments-operate (lambda (a b) (and a (not b)))  s1 s2))
                      (:union            (segments-operate (lambda (a b) (or a b))         s1 s2))
                      (:xor              (segments-operate (function xor)                  s1 s2))
                      ;; (:horizontal-inset (segments-horizontal-inset s1 s2))
                      ))
            res)))
    (loop
      :with invpt1 = (expanded-inversion-points r1)
      :with invpt2 = (expanded-inversion-points r2)
      :with r = (make-array (+ (length invpt1) (length invpt2)) :adjustable t :fill-pointer 0)
      :with s1 = #()
      :with s2 = #()
      :with i1 = 0
      :with i2 = 0
      :with ip1 = (aref invpt1 i1) :with v1 = (aref ip1 0)
      :with ip2 = (aref invpt2 i2) :with v2 = (aref ip2 0)
      :do (cond
            ((and (null v1) (null v2))
             (loop-finish))
            ((or (null v2) (and v1 (< v1 v2)))
             (setf s1 (update-segments s1 ip1))
             (operate op v1 s1 s2 r)
             (incf i1)
             (if (< i1 (length invpt1))
               (setf ip1 (aref invpt1 i1)
                     v1  (aref ip1 0))
               (setf v1 nil)))
            ((or (null v1) (< v2 v1))
             (setf s2 (update-segments s2 ip2))
             (operate op v2 s1 s2 r)
             (incf i2)
             (if (< i2 (length invpt2))
               (setf ip2 (aref invpt2 i2)
                     v2  (aref ip2 0))
               (setf v2 nil)))
            (t
             (setf s1 (update-segments s1 ip1)
                   s2 (update-segments s2 ip2))
             (operate op v2 s1 s2 r)
             (incf i1)
             (if (< i1 (length invpt1))
               (setf ip1 (aref invpt1 i1)
                     v1  (aref ip1 0))
               (setf v1 nil))
             (incf i2)
             (if (< i2 (length invpt2))
               (setf ip2 (aref invpt2 i2)
                     v2  (aref ip2 0))
               (setf v2 nil))))
      :finally (return (values r rd)))))


(defun region-operate (op r1 r2 rd)
  "
NOTE:           This function computes the operations in simple cases,
                and defers to region-operate-not-easy in the remaining
                cases.
OP:             A keyword denoting an operation, one of :intersection
                :difference :union :xor.
R1:             A region.
R2:             A region.
RD:             A region.
DO:             Compute the operation OP between R1 and R2, and set RD
                to the result of that operation.
"
  (check-type op (member :intersection :difference :union :xor))
  (if (equal-region-p r1 r2)
    (if (or (eq op :intersection)
            (eq op :union))
      (copy-region r1 rd)
      (set-empty-region rd))
    (cond
      ((and (eq op :difference) (empty-region-p r2))
       (copy-region r1 rd))
      ((or (eq op :intersection) (eq op :difference))
       (let ((bounds.intersection (intersect-rect (region-bounds r1) (region-bounds r2)
                                                  (make-rect 0 0 0 0))))
         (if (empty-rect-p bounds.intersection)
           (if (eq op :intersection)
             (set-empty-region rd)
             (copy-region r1 rd))
           (if (and (rectangular-region-p r1) (rectangular-region-p r2))
             (set-rect-region rd bounds.intersection)
             (region-operate-not-easy op r1 r2 rd)))))
      ;; (or (eq op :union) (eq op :xor))
      ((empty-region-p r2)
       (copy-region r1 rd))
      ((empty-region-p r1)
       (copy-region r2 rd))
      (t
       (region-operate-not-easy op r1 r2 rd))))
  rd)


(defun test/region-operate ()
  (assert (equalp (region-operate :intersection
                                  (set-rect-region (new-region) 0 0 200 100)
                                  (set-rect-region (new-region) 50 20 250 120)
                                  (new-region))
                  #S(region :bounds #S(rect :topleft 1310770 :bottomright 6553800)
                            :segments #((20 . #(50 200)) (100 . #())))))
  (assert (equalp
           (region-operate
            :difference
            (region-operate
             :union
             (set-rect-region (new-region) 10 10 100 100)
             (set-rect-region (new-region) 200 50 400 150)
             (new-region)) 
            (set-rect-region (new-region) 50 60 350 80)
            (new-region))
           #S(region :bounds #S(rect :topleft 655370 :bottomright 9830800)
                     :segments #((10 . #(10 100))
                                 (50 . #(10 100 200 400))
                                 (60 . #(10 50 350 400))
                                 (80 . #(10 100 200 400))
                                 (100 . #(200 400))
                                 (150 . #())))))
  :success)


;;----------------------------------------------------------------------

(defun new-region ()
  "The new-region function allocates a new empty region and returns it."
  (make-region))


(defun new-rgn ()
  (new-region))


(defun dispose-region (region)
  "The dispose-region function reclaims storage space used by region and returns NIL."
  (set-empty-region region)
  nil)

(declaim (inline new-region new-rgn dispose-region))


(defun copy-region (region &optional (dest-region (new-region)))
  "
The COPY-REGION function either copies REGION into DEST-REGION, if it
is supplied, or creates a new region equivalent to region.  It returns
the new region or DEST-REGION.

REGION:         A region.

DEST-REGION:    Another region.
"
  (setf (region-bounds   dest-region) (make-rect (region-bounds   region))
        (region-segments dest-region) (copy-seq  (region-segments region)))
  dest-region)


(defun set-empty-region (region)
  "
The SET-EMPTY-REGION function destructively modifies region so that it
is empty and returns the empty region.

REGION:         A region.
"
  (setf (region-bounds    region) #.(make-rect 0 0 0 0)
        (region-segments  region) #())
  region)


(defun set-rect-region (region left &optional top right bot)
  "
The SET-RECT-REGION function sets region so that it is equivalent to the
rectangle specified by the arguments and returns the rectangular region.

REGION:         A region.

LEFT, TOP, RIGHT, BOTTOM:

                These four arguments are used together to specify the
                rectangle. If only left is given, it should be a
                pointer to a rectangle record. If only two arguments
                are given, they should be points specifying the
                upper-left and lowerright coordinates of the
                rectangle. If all four arguments are given, they
                should be coordinates representing the LEFT, TOP,
                RIGHT, and BOTTOM of the rectangle.
"
  (setf (region-bounds    region) (make-rect left top right bot)
        (region-segments  region) (vector (cons top (vector left right))
                                          (cons bot #())))
  region)



(defgeneric open-region (view)
  (:documentation "
The OPEN-REGION generic function hides the pen and begins recording a
region.  Subsequent drawing commands to the window add to the region.
Recording ends when CLOSE-REGION is called. The function returns NIL.
It is an error to call OPEN-REGION a second time without first calling
CLOSE-REGION.

VIEW:           A window or a view contained in a window.
")
  (:method ((view simple-view))
    (when (window-open-region (view-window view))
      (error "Cannot call ~S twice in a row before calling ~S."
             'open-region 'close-region))
    (niy open-region view)
    (pen-hide view)
    (setf (window-open-region (view-window view)) (new-region))))


(defgeneric close-region (view &optional dest-region)
  (:documentation "

The CLOSE-REGION generic function shows the pen and returns a region
that is the accumulation of drawing commands in the window since the
last open-region for the window.  It returns the result in
DEST-REGION, if supplied, or else in a newly created region.  It is an
error to call CLOSE-REGION before OPEN-REGION has been called.  Note
that if a new region is created, you must dispose of it explicitly to
reclaim its storage space.

VIEW:           A window or a view contained in a window.

DEST-REGION:    A region.
")
  (:method ((view simple-view) &optional dest-region)
    (unless (window-open-region (view-window view))
      (error "Cannot call ~S without calling ~S before."
             'close-region 'open-region))
    (niy close-region view dest-region)
    (prog1 (if dest-region
             (copy-region (window-open-region (view-window view)) dest-region)
             (window-open-region (view-window view)))
      (setf (window-open-region (view-window view)) nil)
      (pen-show view))))


(defun offset-region (region h &optional v)
  "
The OFFSET-REGION function destructively offsets region by H to the right
and V down and returns the offset region.  If only H is given, it is interpreted
as an encoded point, and its coordinates are used.

REGION:         A region.

H:              Horizontal position.

V:              Vertical position.  If V is NIL (the default), H is
                assumed to represent a point.
"
  (offset-rect (region-bounds region) h v)
  (loop
    :for segs :across (region-segments region)
    :do (incf (car segs) v)
    :do (loop
          :with s = (cdr segs)
          :for i :below (length s)
          :do (incf (aref s i) h)))
  region)


(defun inset-region (region h &optional v)
  "
The INSET-REGION function destructively shrinks or expands region by H
horizontally and V vertically and returns it. If only H is given, it
is interpreted as an encoded point, and its coordinates are used.

REGION:         A region.

H:              Horizontal position.

V:              Vertical position.  If V is NIL (the default), H is
                assumed to represent a point.
"
  (niy inset-region region h v)
  ;; This zooms in or out the region.
  region)


(defun intersect-region (region1 region2 &optional (dest-region (new-region)))
  "
The INTERSECT-REGION function returns a region that is the
intersection of region1 and region2. It returns the result in
dest-region, if supplied, or else in a newly created region.

REGION1:        A region.
REGION2:        A region.
DEST-REGION:    A region.
"
  (region-operate :intersection region1 region2 dest-region))


(defun union-region (region1 region2 &optional (dest-region (new-region)))
  "
The UNION-REGION function returns a region that is the union of
region1 and region2. It returns the result in dest-region, if
supplied, or else in a newly created region.

REGION1:        A region.
REGION2:        A region.
DEST-REGION:    A region.
"
  (region-operate :union region1 region2 dest-region))


(defun difference-region (region1 region2 &optional (dest-region (new-region)))
  "
The DIFFERENCE-REGION function returns a region that is the difference
of region1 and region2. It returns the result in dest-region, if
supplied, or else in a newly created region.

REGION1:        A region.
REGION2:        A region.
DEST-REGION:    A region.
"
  (region-operate :difference region1 region2 dest-region))


(defun xor-region (region1 region2 &optional (dest-region (new-region)))
  "
The XOR-REGION function returns a region that consists of all the
points that are in region1 or region2, but not both. It returns the
result in dest-region, if supplied, or else in a newly created region.

REGION1:        A region.
REGION2:        A region.
DEST-REGION:    A region.
"
  (region-operate :xor region1 region2 dest-region))


(defun point-in-region-p (region h &optional v)
  "
The POINT-IN-REGION-P function returns T if the point specified by H
and V is contained in region; otherwise, it returns NIL.  If only H is
given, it is interpreted as an encoded point.

REGION:         A region.

H:              Horizontal position.

V:              Vertical position.  If V is NIL (the default), H is
                assumed to represent a point.
"
  (niy point-in-region-p region h v))


(defun rect-in-region-p (region left &optional top right bot)
  "
The RECT-IN-REGION-P function returns T if the intersection of the
rectangle specified by the arguments and region contains at least one
point; otherwise it returns NIL.

REGION:         A region.

LEFT, TOP, RIGHT, BOTTOM:

                These four arguments are used together to specify the
                rectangle. If only left is given, it should be a
                pointer to a rectangle record. If only two arguments
                are given, they should be points specifying the
                upper-left and lowerright coordinates of the
                rectangle. If all four arguments are given, they
                should be coordinates representing the LEFT, TOP,
                RIGHT, and BOTTOM of the rectangle.
"
  (niy rect-in-region-p region left top right bot))


(defun equal-region-p (region1 region2)
  "
The EMPTY-REGION-P function returns T if region1 and region2 contain
the same points and NIL otherwise.

REGION1:        A region.
REGION2:        A region.
"
  (and (equal-rect (region-bounds region1) (region-bounds region2))
       (equalp (region-segments region1) (region-segments region2))))


(defun empty-region-p (region)
  "
The EMPTY-REGION-P function returns T if region contains no points and
NIL otherwise.

REGION:         A region.
"
  (empty-rect-p (region-bounds region)))


(defun initialize/region ()
  (test/segments-operate)
  (test/segments-trim)
  ;; (test/update-segments)
  (test/region-operate)
  (setf *temp-rgn* (new-region)))

;;;; THE END ;;;;

