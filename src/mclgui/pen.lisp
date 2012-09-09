;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pen.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Pen.
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
(objcl:enable-objcl-reader-macros)


(defgeneric view-pen (view)
  (:documentation "
RETURN:         The pen-state of the WINDOW of the VIEW.
")
  (:method ((view simple-view))
    ;; there's a method specialized on window.
    (view-pen (view-window view))))


(defgeneric pen-show (view)
  (:documentation "
The PEN-SHOW generic function shows the pen.  Drawing occurs only when
the pen is shown.

VIEW:           A window or a view contained in a window.
")
  (:method ((view simple-view))
    (pen-show (view-pen view))))


(defgeneric pen-hide (view)
  (:documentation "
The PEN-HIDE generic function hides the pen.  Drawing occurs only when
the pen is shown.

VIEW:           A window or a view contained in a window.
")
  (:method ((view simple-view))
    (pen-hide (view-pen view))))


(defgeneric pen-shown-p (view)
  (:documentation "
The PEN-SHOWN-P generic function returns T if the pen is shown and NIL
if the pen is hidden.

VIEW:           A window or a view contained in a window.
")
  (:method ((view simple-view))
    (pen-shown-p (view-pen view))))


(defgeneric pen-size (view)
  (:documentation "
The PEN-SIZE generic function returns the current pen size as a point
\(expressing a width and height).

VIEW:           A window or a view contained in a window.
")
  (:method ((view simple-view))
    (pen-size (view-pen view))))


(defgeneric set-pen-size (view h &optional v)
  (:documentation "
The SET-PEN-SIZE generic function sets the pen size to the point
indicated by H and V.

VIEW:           A window or a view contained in a window.

H:              The width of the new pen size (or a point representing
                the width and height, if V is not given).

V:              The height of the new pen size.
")
  (:method ((view simple-view) h &optional v)
    (setf (pen-size (view-pen view)) (make-point h v))))


(defgeneric pen-pattern (view &optional save-pattern)
  (:documentation "
The PEN-PATTERN generic function returns the window’s pen pattern.

VIEW:           A window or a view contained in a window.

SAVE-PATTERN    A pattern record; the pattern is returned in this record. If
                save-pattern is not given, a new pattern record is allocated
                to hold the returned pattern.
")
  (:method ((view simple-view) &optional save-pattern)
    (copy-object-from (or save-pattern (make-instance 'pattern))
                      (pen-state-pattern (view-pen view)))))


(defgeneric set-pen-pattern (view new-pattern)
  (:documentation "
The SET-PEN-PATTERN generic function sets the window’s pen pattern.

VIEW:           A window or a view contained in a window.

NEW-PATTERN:    A PATTERN object.

                A pattern is stored as a 2D 8x8 bit array. Methods of
                a pattern object allows patterns to be accessed as 8
                bytes or four words.
")
  (:method ((view simple-view) new-pattern)
    (copy-object-from (setf (pen-state-pattern (view-pen view))
                            (make-instance 'pattern))
                      new-pattern)))


(defgeneric pen-mode (view)
  (:documentation "
The PEN-MODE generic function returns a keyword indicating the
window’s current pen mode.

VIEW:           A window or a view contained in a window.
")
  (:method ((view simple-view))
    (pen-mode-to-name (pen-mode (view-pen view)))))


(defgeneric set-pen-mode (view new-mode)
  (:documentation "
The SET-PEN-MODE generic function sets the window’s current pen mode.

VIEW:           A window or a view contained in a window.

NEW-MODE:       The new pen mode. This value should be one of the
                following keywords: :patCopy, :patOr, :patXor,
                :patBic, :notPatCopy, :notPatOr, :notPatXor,
                :notPatBic.
")
  (:method ((view simple-view) new-mode)
    (setf (pen-mode (view-pen view)) (pen-mode-arg new-mode t))))


(defgeneric pen-state (view &optional save-pen-state)
  (:documentation "
The PEN-STATE generic function returns the current pen state, an instance
containing the pen’s location, size, mode (as an integer), and pattern.

VIEW:           A window or a view contained in a window.

SAVE-PEN-STATE: A PEN-STATE instance; the returned state is stored in
                this instance.  If SAVE-PEN-STATE is not given, the
                pen state is returned in a newly allocated instance.
")
  (:method ((view simple-view) &optional save-pen-state)
    (copy-object-from (or save-pen-state (make-instance 'pen-state))
                      (view-pen view))))

(defgeneric set-pen-state (view new-state)
  (:documentation "
The SET-PEN-STATE generic function sets the window’s pen state.

VIEW:           A window or a view contained in a window.

NEW-STATE:      A PEN-STATE object.
")
  (:method ((view simple-view) new-state)
    (copy-object-from (view-pen view) new-state)))


(defgeneric pen-normal (view)
  (:documentation "
The pen-normal generic function sets the pen size to #@(1 1), the pen
mode to :patCopy, and the pen pattern to *black-pattern*. The pen
location is not changed.

VIEW:           A window or a view contained in a window.

NEW-STATE:      A PEN-STATE object.
")
  (:method ((view simple-view))
    (pen-normal (view-pen view))))


(defgeneric move-to (view h &optional v)
  (:documentation "
The MOVE-TO generic function moves the pen to the point specified by H
and V without doing any drawing.  It returns the point to which the
pen moved.

VIEW:           A window or a view contained in a window.

H:              Horizontal position.

V:              Vertical position.  If V is NIL (the default), H is
                assumed to represent a point.
")
  (:method ((view simple-view) h &optional v)
    (setf (pen-position (view-pen view)) (make-point h v))))


(defgeneric move (view h &optional v)
  (:documentation "
The MOVE generic function moves the pen H points to the right and V
points down without doing any drawing.

VIEW:           A window or a view contained in a window.

H:              Horizontal position.

V:              Vertical position.  If V is NIL (the default), H is
                assumed to represent a point.
")
  (:method ((view simple-view) h &optional v)
    (let ((pen (view-pen view)))
      (setf (pen-position pen) (add-points (pen-position pen) (make-point h v))))))


(defgeneric line-to (view h &optional v)
  (:documentation "
The LINE-TO generic function draws a line from the pen’s current
position to the point represented by H and V.

VIEW:           A window or a view contained in a window.

H:              Horizontal position.

V:              Vertical position.  If V is NIL (the default), H is
                assumed to represent a point.
")
  (:method ((view simple-view) h &optional v)
    (let* ((pen (view-pen view))
           (src (pen-position pen))
           (dst (make-point h v)))
      (niy line-to view h v "if there's an open region, add it to the region; if there's an open picture, add it to the picture")
      (when (pen-shown-p view)
        ;; TODO: set the compositingOperation, the pattern, the color, the size of the pen, etc.
        (draw-line (point-h src) (point-v src) (point-h dst) (point-v dst)))
      (setf (pen-position pen) dst))))


(defgeneric line (view h &optional v)
  (:documentation "
The LINE generic function draws a line to a point H points to the
right and V points down from the current pen position.

VIEW:           A window or a view contained in a window.

H:              Horizontal position.

V:              Vertical position.  If V is NIL (the default), H is
                assumed to represent a point.
")
  (:method ((view simple-view) h &optional v)  
    (let* ((pen (view-pen view))
           (src (pen-position pen))
           (dst (add-points src (make-point h v))))
      (niy line view h v "if there's an open region, add it to the region; if there's an open picture, add it to the picture")
      (when (pen-shown-p view)
        ;; TODO: set the compositingOperation, the pattern, the color, the size of the pen, etc.
        (draw-line (point-h src) (point-v src) (point-h dst) (point-v dst)))
      (setf (pen-position pen) dst))))


(defgeneric stream-tyo (view char)
  (:documentation "
The STREAM-TYO generic function draws CHAR at the current pen
position, in the current font, using the current text transfer
mode.  It then moves the pen to the right the width of the
character.  Because windows are streams, all stream output functions
\(such as PRIN1) can be performed on them.  The STREAM-TYO function is
not normally called directly but instead by stream output functions.
"))






;;;---------------------------------------------------------------------
;;;
;;; Pen
;;;

(defclass pen-state ()
  ((visiblep   :initarg :visible  :initform t :type boolean  :reader pen-shown-p)
   (position   :initarg :position :initform (make-point 0 0) :type point   :accessor pen-position)
   (size       :initarg :size     :initform (make-point 1 1) :type point   :accessor pen-size)
   (mode       :initarg :mode     :initform 8                :type fixnum  :accessor pen-mode)
   (pattern    :initarg :pattern  :initform *black-pattern*  :type pattern :accessor pen-state-pattern))
  (:documentation "A Quickdraw Pen."))


(defmethod copy-object-from ((dst pen-state) (src pen-state))
  (setf (slot-value   dst 'visiblep) (slot-value   src 'visiblep)
        (pen-position dst)           (pen-position src)
        (pen-size     dst)           (pen-size     src)
        (pen-mode     dst)           (pen-mode     src))
  (copy-object-from (pen-state-pattern dst) (pen-state-pattern src))
  dst)


(defmethod pen-show ((pen pen-state))
  (setf (slot-value pen 'visiblep) t)
  pen)


(defmethod pen-hide ((pen pen-state))
  (setf (slot-value pen 'visiblep) nil)
  pen)


(defmethod pen-normal ((pen pen-state))
  (setf (pen-size          pen) (make-point 1 1)
        (pen-mode          pen) 8
        (pen-state-pattern pen) *black-pattern*)
  pen)


(defmethod print-object ((pen pen-state) stream)
  (declare (stepper disable))
  (print-parseable-object (pen stream :type t :identity t)
                          visiblep
                          (:position (point-to-list (pen-position pen)))
                          (:size     (point-to-list (pen-size pen)))
                          (:mode     (pen-mode-to-name (pen-mode pen)))
                          pattern)
  pen)



(defvar *mode-to-compositing-operation* nil)

(defgeneric mode-to-compositing-operation (mode)
  (:method ((mode integer))
    (second (elt *mode-to-compositing-operation* mode)))
  (:method ((mode keyword))
    (let ((entry (assoc mode *mode-to-compositing-operation*)))
      (if entry
        (second entry)
        (error "Invalid transfer mode.")))))


;; A PEN-STATE uses NSGraphicContext, NSBezierPath and NSColor to draw.
;;
;; NSGraphicContext:
;;    (isFlipped)
;;    compositingOperation
;;    patternPhase
;;
;; NSBezierPath:
;;    -[NSBezierPath setClip] sets the clip region of the NSGraphicContext.
;;    -[NSBezierPath stroke]
;;    -[NSBezierPath fill]
;;
;; NSColor:
;;    +[NSColor colorWithPatternImage:]         creates a Pattern "color".
;;    +[NSColor colorWithCalibratedRed:green:blue:alpha:] creates a color.
;;    -[NSColor set]       sets the current color in the NSGraphicContext.
;;    -[NSColor setFill]   sets the current color in the NSGraphicContext.
;;    -[NSColor setStroke] sets the current color in the NSGraphicContext.


(defgeneric pen-shadow (pen &key position size mode pattern)
  (:method ((pen pen-state) &key position size mode pattern)
    (make-instance 'pen-state
      :visible (pen-shown-p pen)
      :position (or position (pen-position pen))
      :size (or size (pen-size pen))
      :mode (or (pen-mode-arg mode) (pen-mode pen))
      :pattern (or pattern (pen-state-pattern pen)))))


(defun call-with-pen-state (thunk pen)
  (when *current-view*
    (let* ((window       (view-window *current-view*))
           (original-pen (view-pen window)))
      (with-handle (winh window)
        (let ((context [winh graphicsContext]))
          (unwind-protect
              (progn
                [context saveGraphicsState]
                (setf (slot-value window 'view-pen) pen)
                (with-handle (color (pen-state-pattern pen))
                  [color setFill]
                  [color setStroke]
                  [color set])
                [context setCompositingOperation:(mode-to-compositing-operation (pen-mode pen))]
                (funcall thunk))
            [context restoreGraphicsState]
            (unless (eq original-pen (view-pen window))
              (setf (slot-value window 'view-pen) pen))))))))


(defmacro with-pen-state ((&key location size mode pattern) &body body)
  "
WITH-PEN-STATE can be called only in the dynamic context of a
WITH-FOCUSED-VIEW.

It sets temporarily the view-pen of the focused view.
"
  (let ((vwindow (gensym)))
    `(when *current-view*
       (let ((,vwindow (view-window *current-view*)))
         (when ,vwindow
           (call-with-pen-state (lambda () ,@body)
                                (pen-shadow (view-pen ,vwindow)
                                            :position ,location
                                            :size ,size
                                            :mode ,mode
                                            :pattern ,pattern)))))))


(defun initialize/pen ()
  (setf *mode-to-compositing-operation*
        `((:srcCopy        ,#$NSCompositeCopy)
          (:srcOr          ,#$NSCompositeSourceOver)
          (:srcXor         ,#$NSCompositeXOR)
          (:srcBic         ,#$NSCompositeDestinationOut)
          (:notSrcCopy     ,#$NSCompositePlusLighter)    ; no match
          (:notSrcOr       ,#$NSCompositeDestinationAtop) ; no match
          (:notSrcXor      ,#$NSCompositeSourceIn)
          (:notsrcbic      ,#$NSCompositeSourceOut)
          (:patCopy        ,#$NSCompositeCopy)
          (:patOr          ,#$NSCompositeSourceOver)
          (:patXor         ,#$NSCompositeXOR)
          (:patBic         ,#$NSCompositeDestinationOut)
          (:notPatCopy     ,#$NSCompositePlusLighter)    ; no match
          (:notPatOr       ,#$NSCompositeDestinationAtop) ; no match
          (:notPatXor      ,#$NSCompositeSourceIn)
          (:notPatBic      ,#$NSCompositeSourceOut))))


;;;; THE END ;;;;

