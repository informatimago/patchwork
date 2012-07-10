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


(defvar *temp-rgn* nil)


(defmacro with-temp-rgns ((&rest rgn-vars) &body body)
  #-(and)
  `(with-macptrs ,rgn-vars
     (unwind-protect
          (progn
            ,@(mapcar #'(lambda (var) `(%setf-macptr ,var (require-trap #_NewRgn))) rgn-vars)
            ,@body)
       ,@(mapcar #'(lambda (var) `(unless (%null-ptr-p ,var) (require-trap #_DisposeRgn ,var)))
                 rgn-vars)))
  (niy with-temp-rgns rgn-vars)
  `(progn
     (niy 'with-temp-rgns ',rgn-vars)
     ,@body))


(defmacro with-hilite-mode (&body body)
  (niy with-hilite-mode body)
  `(niy with-hilite-mode body)
  #-(and)
  `(progn
     (let ((byte (require-trap #_lmgethilitemode)))
       (require-trap #_lmsethilitemode (%ilogand2 #x7f byte)))
     ,@body))


(defmacro with-clip-region (region &body body)
  (niy with-clip-region region body)
  `(niy with-clip-region region body)
  #-(and)
  (let ((rgn  (gensym))
        (rgn2 (gensym)))    
    `(with-temp-rgns (,rgn ,rgn2)
       (require-trap #_GetClip ,rgn)
       (require-trap #_sectrgn ,rgn ,region ,rgn2)
       (unwind-protect
            (progn
              (require-trap #_SetClip ,rgn2)
              ,@body)
         (require-trap #_SetClip ,rgn)))))




;;;Regions

(defstruct (region
             (:predicate regionp)
             (:copier nil))
  (bounds (make-rect 0 0 0 0) :type rect)
  (rects  '()                 :type list))


(defun new-region ()
  "The new-region function allocates a new empty region and returns it."
  (make-region))

(defun new-rgn ()
  (new-region))

(defun dispose-region (region)
  "The dispose-region function reclaims storage space used by region and returns NIL."
  (declare (ignore region))
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
  (setf (region-bounds dest-region) (make-rect (region-bounds region))
        (region-rects  dest-region) (mapcar (function make-rect) (region-rects region)))
  dest-region)


(defun set-empty-region (region)
  "
The SET-EMPTY-REGION function destructively modifies region so that it
is empty and returns the empty region.

REGION:         A region.
"
  (setf (region-bounds region) (make-rect 0 0 0 0)
        (region-rects  region) '())
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
  (setf (region-bounds region) (make-rect left top right bot)
        (region-rects  region) (list (make-rect left top right bot)))
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
  (niy intersect-region region1 region2 dest-region)
  dest-region)


(defun union-region (region1 region2 &optional (dest-region (new-region)))
  "
The UNION-REGION function returns a region that is the union of
region1 and region2. It returns the result in dest-region, if
supplied, or else in a newly created region.

REGION1:        A region.
REGION2:        A region.
DEST-REGION:    A region.
"
  (niy union-region region1 region2 dest-region)
  dest-region)


(defun difference-region (region1 region2 &optional (dest-region (new-region)))
  "
The DIFFERENCE-REGION function returns a region that is the difference
of region1 and region2. It returns the result in dest-region, if
supplied, or else in a newly created region.

REGION1:        A region.
REGION2:        A region.
DEST-REGION:    A region.
"
  (niy difference-region region1 region2 dest-region)
  dest-region)


(defun xor-region (region1 region2 &optional (dest-region (new-region)))
  "
The XOR-REGION function returns a region that consists of all the
points that are in region1 or region2, but not both. It returns the
result in dest-region, if supplied, or else in a newly created region.

REGION1:        A region.
REGION2:        A region.
DEST-REGION:    A region.
"
  (niy xor-region region1 region2 dest-region)
  dest-region)


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
The EMPTY-REGION-P function returns T if region contains no points and
NIL otherwise.

REGION1:        A region.
REGION2:        A region.
"
  (niy equal-region-p region1 region2))


(defun empty-region-p (region)
  "
The EMPTY-REGION-P function returns T if region contains no points and
NIL otherwise.

REGION:         A region.
"
  (empty-rect-p (region-bounds region)))


(defun initialize/region ()
  (setf *temp-rgn* (new-region)))

;;;; THE END ;;;;

