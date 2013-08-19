;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               quickdraw.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    The drawing functions of QuickDraw.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-06-11 <PJB> Created.
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Quickdraw.lisp
;;
;;  version 2.0
;;
;;  Copyright 1989-1994 Apple Computer, Inc.
;;  Copyright 1995 Digitool, Inc.
;;
;;  This file implements a full error-checked interface to Quickdraw.
;;  It is meant to be useful both in programs and as an example of how to use
;;  the low-level interface to the Mac.
;;
;;  You can compile selected portions of this file, but if you do, make sure to
;;  include the macros and utility functions from the top.
;;
;;  Because these functions perform a view-focus on every drawing command,
;;  they can be slow.  For faster drawing you should only focus the view
;;  once, and then issue a series of drawing commands.  You can use
;;  this file as an example of how to call the Quickdraw traps directly
;;  in such a situation.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#-(and)
(defmacro with-rectangle-arg ((var left &optional top right bottom) &body body)
  "takes a rectangle, two points, or four coordinates and makes a rectangle.
body is evaluated with VAR bound to that rectangle."
  `(rlet ((,var :rect))
     (setup-rect ,var ,left ,top ,right ,bottom)
     ,@body))

#-(and)
(defun setup-rect (rect left top right bottom)
  (cond (bottom
         (setf (pref rect rect.topleft) (make-point left top))
         (setf (pref rect rect.bottomright) (make-point right bottom)))
        (right
         (error "Illegal rectangle arguments: ~s ~s ~s ~s"
                left top right bottom))
        (top
         (setf (pref rect rect.topleft) (make-point left nil))
         (setf (pref rect rect.bottomright) (make-point top nil)))
        (t (%setf-macptr rect left))))

;; not used now
#-(and)
(defvar *32-bit-qd-pen-modes*
  '((:blend . 32)
    (:addPin . 33)
    (:addOver . 34)
    (:subPin . 35)
    (:transparent . 36)
    (:adMax . 37)
    (:subOver . 38)
    (:adMin . 39)
    (:hilite . 50)))


(defun mode-arg (thing)
  (or
   (and (fixnump thing) (<= 0 thing 64) thing)
   (position thing *pen-modes*)
   (cdr (assq thing *pen-modes-alist*))
   (error "Unknown pen mode: ~a" thing)))

(defgeneric origin (view)
  (:method ((view simple-view))
    (view-scroll-position view)))

(defgeneric set-origin (view h &optional v)
  (:method set-origin ((view simple-view) h &optional v)
           (set-view-scroll-position view h v nil)))


(defmethod clip-region ((view simple-view) &optional (save-region))
  ;; (let ((save-region (or save-region (#_NewRgn))))
  ;;   (with-focused-view view
  ;;     (#_GetClip save-region))
  ;;   save-region)
  )

(defmethod set-clip-region ((view simple-view) new-region)
  (with-focused-view view
    ;;(#_SetClip new-region)
    )
  new-region)

(defmethod clip-rect ((view simple-view) left &optional top right bot)
  (with-rectangle-arg (r left top right bot)
    (with-focused-view view
      ;;(#_ClipRect r)
      ))
  nil)

;;;---------------------------------------------------------------------
;;; Drawing Rectangles
;;;---------------------------------------------------------------------

(defmethod frame-rect ((view simple-view) left &optional top right bot)
 (with-focused-view view
   (with-rectangle-arg (r left top right bot)
     ;; (#_FrameRect r)
     )))

(defmethod paint-rect ((view simple-view) left &optional top right bot)
  (with-focused-view view
    (with-rectangle-arg (r left top right bot)
      ;; (#_PaintRect r)
      )))

(defmethod erase-rect ((view simple-view) left &optional top right bot)
  (with-focused-view view
    (with-rectangle-arg (r left top right bot)
      ;; (#_EraseRect r)
      )))

(defmethod invert-rect ((view simple-view) left &optional top right bot)
  (with-focused-view view
    (with-rectangle-arg (r left top right bot)
      ;; (#_InvertRect r)
      )))

(defmethod fill-rect ((view simple-view) pattern left &optional top right bot)
  (with-focused-view view
    (with-rectangle-arg (r left top right bot)
      ;; (#_FillRect r pattern)
      )))

(defmethod frame-oval ((view simple-view) left &optional top right bot)
 (with-focused-view view
   (with-rectangle-arg (r left top right bot)
     ;; (#_FrameOval r)
     )))

(defmethod paint-oval ((view simple-view) left &optional top right bot)
  (with-focused-view view
    (with-rectangle-arg (r left top right bot)
      ;; (#_PaintOval r)
      )))

(defmethod erase-oval ((view simple-view) left &optional top right bot)
  (with-focused-view view
    (with-rectangle-arg (r left top right bot)
      ;; (#_EraseOval r)
      )))

(defmethod invert-oval ((view simple-view) left &optional top right bot)
  (with-focused-view view
    (with-rectangle-arg (r left top right bot)
      ;; (#_InvertOval r)
      )))

(defmethod fill-oval ((view simple-view) pattern left &optional top right bot)
  (with-focused-view view
    (with-rectangle-arg (r left top right bot)
      ;; (#_FillOval r pattern)
      )))

(defmethod frame-round-rect ((view simple-view) oval-width oval-height 
                             left &optional top right bot)
 (with-focused-view view
   (with-rectangle-arg (r left top right bot)
     ;; (#_FrameRoundRect r oval-width oval-height)
     )))

(defmethod paint-round-rect ((view simple-view) oval-width oval-height 
                             left &optional top right bot)
 (with-focused-view view
   (with-rectangle-arg (r left top right bot)
     ;; (#_PaintRoundRect r oval-width oval-height)
     )))

(defmethod erase-round-rect ((view simple-view) oval-width oval-height 
                             left &optional top right bot)
 (with-focused-view view
   (with-rectangle-arg (r left top right bot)
     ;; (#_EraseRoundRect r oval-width oval-height)
     )))

(defmethod invert-round-rect ((view simple-view) oval-width oval-height 
                              left &optional top right bot)
 (with-focused-view view
   (with-rectangle-arg (r left top right bot)
     ;; (#_InvertRoundRect r oval-width oval-height)
     )))

(defmethod fill-round-rect ((view simple-view) pattern oval-width oval-height 
                            left &optional top right bot)
  (with-focused-view view
    (with-rectangle-arg (r left top right bot)
      ;; (#_FillRoundRect r oval-width oval-height pattern)
      )))

(defmethod frame-arc ((view simple-view) start-angle arc-angle 
                      left &optional top right bot)
 (with-focused-view view
   (with-rectangle-arg (r left top right bot)
     ;; (#_FrameArc r start-angle arc-angle)
     )))

(defmethod paint-arc ((view simple-view) start-angle arc-angle 
                      left &optional top right bot)
 (with-focused-view view
   (with-rectangle-arg (r left top right bot)
     ;; (#_PaintArc r start-angle arc-angle)
     )))

(defmethod erase-arc ((view simple-view) start-angle arc-angle 
                      left &optional top right bot)
 (with-focused-view view
   (with-rectangle-arg (r left top right bot)
     ;; (#_EraseArc r start-angle arc-angle)
     )))

(defmethod invert-arc ((view simple-view) start-angle arc-angle 
                       left &optional top right bot)
 (with-focused-view view
   (with-rectangle-arg (r left top right bot)
     ;; (#_InvertArc r start-angle arc-angle)
     )))

(defmethod fill-arc ((view simple-view) pattern start-angle arc-angle
                     left &optional top right bot)
  (with-focused-view view
    (with-rectangle-arg (r left top right bot)
      ;; (#_FillArc r start-angle arc-angle pattern)
      )))


;;;---------------------------------------------------------------------
;;; Regions
;;;---------------------------------------------------------------------

(defmethod frame-region ((view simple-view) region)
  (with-focused-view view
    ;; (#_FrameRgn region)
    ))

(defmethod paint-region ((view simple-view) region)
  (with-focused-view view
    ;; (#_PaintRgn region)
    ))

(defmethod erase-region ((view simple-view) region)
  (with-focused-view view
    ;; (#_EraseRgn region)
    ))

(defmethod invert-region ((view simple-view) region)
  (with-focused-view view
    ;; (#_InvertRgn region)
    ))

(defmethod fill-region ((view simple-view) pattern region)
  (with-focused-view view 
    ;; (#_FillRgn region pattern)
    ))


;;;---------------------------------------------------------------------
;;; Pictures
;;;---------------------------------------------------------------------

(defmethod start-picture ((view simple-view) &optional left top right bottom) 
  (let ((wptr (wptr view)))
    ;; (rlet ((arect :rect))      
    ;;   (with-macptrs ((port (#_getwindowport wptr)))
    ;;     (when (#_isportpicturebeingdefined port)
    ;;       (error "A picture may not be started for window: ~a.
    ;;        since one is already started" view))
    ;;     ;; wtf is this
    ;;     (unless left 
    ;;       (#_getportbounds port arect)
    ;;       (setq left arect))
    ;;     (with-rectangle-arg (r left top right bottom)
    ;;       (with-focused-view view
    ;;         (#_cliprect r)
    ;;         (setf (view-get view 'my-hPic) (#_OpenPicture r))))
    ;;     nil))
    ))


(defmethod get-picture ((view simple-view))
  (let ((my-hPic (view-get view 'my-hPic))
        (wptr (wptr view)))
    ;; (with-macptrs ((port (#_getwindowport wptr)))
    ;;   (if (and my-hPic (#_isportpicturebeingdefined port))
    ;;     (prog1
    ;;       my-hPic
    ;;       (with-port wptr (#_ClosePicture))
    ;;       (setf (view-get view 'my-hPic) nil))
    ;;     (error "Picture for window: ~a is not started" view)))
    ))

(defmethod draw-picture ((view simple-view) picture &optional left top right bottom)
 (cond ((not left)
        (setq left (href picture picture.picFrame.topleft)
              top (href picture picture.picFrame.bottomright)))
       ((macptrp left) ;(pointerp left)
        ())  ;everythings fine
       ((and (not right)
             (not top))
        (setq top
              (add-points left
                          (subtract-points
                           (href picture picture.picframe.bottomright)
                           (href picture picture.picframe.topleft))))))
 (with-rectangle-arg (r left top right bottom)
   (with-focused-view view
     ;;  (#_DrawPicture picture r)
     ))
 picture)

(defun kill-picture (picture)
  ;; (#_KillPicture picture)
  )


;;;---------------------------------------------------------------------
;;; Polygons
;;;---------------------------------------------------------------------

(defmethod start-polygon ((view simple-view))
  (let ((wptr (wptr view)))
    (when (view-get view 'my-poly)
      (error "A new polygon may not be started for window: ~a.
           since one is already started" view))
    (with-port wptr
      ;; (setf (view-get view 'my-poly) (#_OpenPoly))
      ))
  nil)

(defmethod get-polygon ((view simple-view))
  ;; (let ((my-poly (view-get view 'my-poly))
  ;;       (wptr (wptr view)))
  ;;   (if (and my-poly t)
  ;;     (prog1
  ;;       my-poly
  ;;       (with-port wptr (#_ClosePoly))
  ;;       (setf (view-get view 'my-poly) nil))
  ;;     (error "Polygon for window: ~a has not been started" view)))
  )

(defun kill-polygon (polygon)
  ;; (#_KillPoly polygon)
  )

(defun offset-polygon (polygon h &optional v)
  ;; (#_OffsetPoly :ptr polygon :long (make-point h v))
  polygon)

(defmethod frame-polygon ((view simple-view) polygon)
  (with-focused-view view
    ;; (#_FramePoly polygon)
    ))

(defmethod paint-polygon ((view simple-view) polygon)
  (with-focused-view view
    ;; (#_PaintPoly polygon)
    ))

(defmethod erase-polygon ((view simple-view) polygon)
  (with-focused-view view
    ;; (#_ErasePoly polygon)
    ))

(defmethod invert-polygon ((view simple-view) polygon)
  (with-focused-view view
    ;; (#_InvertPoly polygon)
    ))

(defmethod fill-polygon ((view simple-view) pattern polygon)
 (with-focused-view view
   ;; (#_FillPoly polygon pattern)
   ))


;;;---------------------------------------------------------------------
;;; Coordinates
;;;---------------------------------------------------------------------


(defmethod local-to-global ((view simple-view) h &optional v)
  (with-focused-view view
    ;; (rlet ((p :point))
    ;;   (%put-long p (make-point h v))
    ;;   (#_LocalToGlobal p)
    ;;   (%get-long p))
    ))
 
(defmethod global-to-local ((view simple-view) h &optional v)
  (with-focused-view view
    ;; (rlet ((p :point))
    ;;   (%put-long p (make-point h v))
    ;;   (#_GlobalToLocal p)
    ;;   (%get-long p))
    ))

(defmethod get-pixel ((view simple-view) h &optional v)
  (with-focused-view view
    (setq h (make-point h v))
    (with-temp-rgns (visrgn)
      (get-window-visrgn (wptr view) visrgn)
      ;; (if (#_PtInRgn h visrgn)
      ;;   (#_GetPixel :long h))
      )))

(defun scale-point (source-rect dest-rect h &optional v)
  ;; (rlet ((pt :point))
  ;;   (%put-long pt (make-point h v))
  ;;   (#_ScalePt pt source-rect dest-rect)
  ;;   (%get-long pt))
  )

(defun map-point (source-rect dest-rect h &optional v)
  ;; (rlet ((pt :point))
  ;;   (%put-long pt (make-point h v))
  ;;   (#_MapPt pt source-rect dest-rect)
  ;;   (%get-long pt))
  )

(defun map-rect (source-rect dest-rect rect)
  ;; (#_MapRect rect source-rect dest-rect)
  rect)

(defun map-region (source-rect dest-rect region)
  ;; (#_MapRgn region source-rect dest-rect)
  region)

(defun map-polygon (source-rect dest-rect polygon)
  ;; (#_MapPoly polygon source-rect dest-rect)
  polygon)

(defun make-bitmap (left &optional top right bottom &aux rowbytes bm)
  ;; (with-rectangle-arg (r left top right bottom)
  ;;   (setq rowbytes 
  ;;         (logand
  ;;          #xfffe 
  ;;          (+ 2  (ash (- (pref r rect.right) (pref r rect.left) 1) -3))))
  ;;   (setq bm 
  ;;         (#_NewPtr :errchk
  ;;                   (+ 14 (* rowbytes (- (pref r rect.bottom) (pref r rect.top))))))
  ;;   (setf (pref bm bitmap.bounds) r)
  ;;   (setf (pref bm bitmap.rowbytes) rowbytes)
  ;;   (setf (pref bm bitmap.baseaddr) (%inc-ptr bm 14)))
  bm)

(defun copy-bits (source-bitmap dest-bitmap source-rect dest-rect
                                &optional (mode 0) mask-region)
  ;; (with-macptrs ((mask-region (if mask-region mask-region (%null-ptr))))
  ;;   (new-with-pointers ((sb source-bitmap)
  ;;                   (db dest-bitmap))
  ;;     (#_CopyBits sb db source-rect dest-rect (mode-arg mode) mask-region)))
  )

(defmethod scroll-rect ((view simple-view) rect dh &optional dv)
  "ignores any clipping regions"
  ;; (with-focused-view view
  ;;   (let* ((reg (#_newrgn)))
  ;;     (#_ScrollRect :ptr rect :long (make-point dh dv) :ptr reg)
  ;;     #-carbon-compat
  ;;     (#_invalrgn reg)
  ;;     #+carbon-compat
  ;;     (inval-window-rgn (wptr view) reg)
  ;;     (#_disposergn reg)))
  )


;;;; THE END ;;;;
