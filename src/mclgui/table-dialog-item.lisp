;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               table-dialog-item.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Table Dialog Item.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-25 <PJB> Created.
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


(defclass table-dialog-item (dialog-item)
  ((width-correction     :allocation :class
                         :initform   50)
   (table-max-width      :accessor   table-max-width
                         :allocation :class
                         :initform   300)
   (table-max-height     :accessor   table-max-height
                         :allocation :class
                         :initform   300)
   (rows                 :accessor   table-rows
                         :initarg    :rows
                         :initform   0)
   (columns              :accessor   table-columns
                         :initarg    :columns
                         :initform   0)
   (top-row              :accessor   table-top-row
                         :initform   0)
   (left-column          :accessor   table-left-column
                         :initform   0)
   (table-vscrollp       :initarg    :table-vscrollp
                         :initform   :undetermined
                         :reader     table-vscrollp
                         :writer     (setf table-vscrollp-slot)
                         :documentation  "
Determines whether the table dialog item has a vertical scroll
bar. The default is to include a scroll bar if one is needed in order
to view the entire table.
")
   (table-hscrollp       :initarg    :table-hscrollp
                         :initform   :undetermined
                         :reader     table-hscrollp
                         :writer     (setf table-hscrollp-slot)
                         :documentation "
Determines whether the table dialog item has a horizontal scroll
bar. The default is to include a scroll bar if one is needed in order
to view the entire table.
")
   (table-hscroll-bar    :initform   nil
                         :accessor   table-hscroll-bar)
   (table-vscroll-bar    :initform   nil
                         :accessor   table-vscroll-bar)
   (grow-icon-p          :accessor   table-grow-icon-p
                         :initarg    :grow-icon-p
                         :initform   nil
                         :documentation "
The value passed as the HasGrow parameter to the #_LNew trap when
install-view-in-window creates the table.  The default value is NIL.
")
   (cell-fonts           :accessor   table-cell-fonts
                         :initform   nil
                         :documentation "
A property list of cells and font specs. See the description of
set-cell-font, later in this section.
")
   (table-print-function :initarg    :table-print-function
                         :initform   #'princ
                         :accessor   table-print-function
                         :documentation "
The function used by draw-cell-contents to print the contents of the
cell.  The default value is #'princ.  If given, this should be a
function of two arguments, the value to be printed and the stream.
")
   (cell-size            :reader     cell-size
                         :writer     (setf cell-size-slot)
                         :initarg    :cell-size
                         :initform   nil
                         :documentation "
Horizontal and vertical dimensions of the cells in the table dialog
item. The default value is nil, meaning that the cell size is computed
to be big enough to accommodate the values of all the cells.
")
   (row-heights-hash     :accessor   row-heights-hash
                         :initform   nil)
   (column-widths-hash   :accessor   column-widths-hash
                         :initform   nil)
   (selection-type       :accessor   selection-type
                         :initarg    :selection-type
                         :initform   :single
                         :documentation "
Determines whether the table dialog item allows single or multiple
selections, and whether multiple selections must be contiguous.
Possible keywords are :single, :contiguous, and :disjoint.  The
default value is :single.

Note: To get a :disjoint selection, you must hold down the Command key
as you select items.  To get a :contiguous selection, hold down the
Shift key.
")
   (visible-dimensions   :accessor   visible-dimensions-slot
                         :initarg    :visible-dimensions
                         :initform   nil
                         :documentation "
The visible dimensions of the table.  The default value is NIL,
meaning that the visible dimensions of the table are calculated and
the entire table is visible.
")
   (view-active-p        :accessor   view-active-p
                         :initform   t)
   (selection-hash       :accessor   table-selection-hash
                         :initform   nil)
   (first-selected-cell  :accessor   first-selected-cell-slot
                         :initform   nil)
   (font-hash            :accessor   table-font-hash
                         :initform   nil)
   (selection-region     :accessor   table-selection-region
                         :initform   nil)
   (outline-region       :accessor   table-outline-region
                         :initform   nil)
   (track-thumb-p        :reader     scroll-bar-track-thumb-p
                         :initform   t
                         :initarg    :track-thumb-p)
   (separator-size       :accessor   separator-size
                         :initform   #@(0 0)
                         :initarg    :separator-size)
   (separator-visible-p  :accessor   separator-visible-p
                         :initform   t
                         :initarg    :separator-visible-p)
   (separator-color      :accessor   separator-color
                         :initform   *gray-color*
                         :initarg    :separator-color)
   (separator-pattern    :reader     separator-pattern-slot
                         :writer     (setf separator-pattern)
                         :initform   '*black-pattern*
                         :initarg    :separator-pattern)
   (cell-colors          :accessor   cell-colors
                         :initform   :text
                         :initarg    :cell-colors)
   (cell-color-hash      :accessor   table-cell-color-hash
                         :initform   nil)))


(defmethod initialize-instance ((item table-dialog-item)
                                &rest initargs
                                &key (selection-type :single)
                                (cell-colors :text)
                                table-dimensions                                   
                                table-print-function
                                cell-fonts)
  (check-type selection-type (member :single :contiguous :disjoint))
  (check-type cell-colors    (member :text :background))
  (if table-dimensions
      (apply (function call-next-method)
             item
             :rows    (point-v table-dimensions)
             :columns (point-h table-dimensions)
             :table-print-function (or table-print-function (function princ))
             initargs)
      (call-next-method))
  (loop
    :while cell-fonts
    :do (let ((cell      (pop cell-fonts))
              (font-spec (pop cell-fonts)))
          (set-cell-font item cell font-spec))))


(defmethod cell-contents ((item table-dialog-item) h &optional v)
  (declare (ignore h v))
  nil)



(defmethod draw-cell-contents ((item table-dialog-item) h &optional v rect)
  (draw-string-in-rect (cell-contents-string-new item h v) rect :truncation :end))


(defmacro normalize-h&v (h v)
  `(unless ,v
     (setf ,v (point-v ,h)
           ,h (point-h ,h))))


(defmethod redraw-cell ((item table-dialog-item) h &optional v)
  (normalize-h&v h v)
  (let* ((cell-pos    (cell-position item h v))
         (cell-width  (table-column-width item h))
         (cell-height (table-row-height item v)))
    (declare (ignore cell-width cell-height))
    (when cell-pos
      (with-focused-view (view-container item)
        (niy redraw-cell item h v)
        #-(and)
        (rlet ((cell-rect :rect
                          :topleft cell-pos
                          :bottomright (add-points cell-pos (make-point cell-width cell-height))))
              (%draw-table-cell-new item h v cell-rect (cell-selected-p item h v)))))))



(defun highlight-rect-frame (rect)
  (niy highlight-rect-frame rect)
  #-(and)
  (with-macptrs ((rgn  (#_newrgn))  ; temp regions already in use  
                 (rgn2 (#_newrgn)))
    (unwind-protect
         (progn
           (#_RectRgn rgn rect)
           (#_insetrect  rect 1 1)
           (#_rectrgn rgn2 rect)
           (#_DiffRgn  rgn rgn2 rgn)     
           (#_LMSetHiliteMode (%ilogand2 (%ilognot (ash 1 #$hiliteBit)) (#_LMGetHiliteMode)))
           (#_InvertRgn rgn))
      (when (and rgn (not (oclo:nullp rgn)))
        (#_disposergn rgn))
      (when (and rgn2 (not (oclo:nullp rgn2)))
        (#_disposergn rgn2)))))


(defgeneric highlight-table-cell (item cell rect selectedp)
  (:documentation "

The HIGHLIGHT-TABLE-CELL generic function highlights cell.  This
function may be shadowed to provide a specialized display.  The
HIGHLIGHT-TABLE-CELL function should not be called directly.  It is
automatically called by the VIEW-CLICK-EVENT HANDLER for
TABLE-DIALOG-ITEM.

ITEM:           A table dialog item.

CELL:           The cell to be drawn.

RECT:           The bounding rectangle of cell.

SELECTEDP:      The state (selected or unselected) of the cell.  If
                the value of SELECTEDP is true, the cell is
                selected. If it is NIL, the cell is unselected.

")
  (:method ((item table-dialog-item) cell rect selectedp)
    (declare (ignore selectedp cell))
    (let ((window (view-window item)))
      (when (and window
                 (not (and (window-active-p window)
                           (eq item (current-key-handler window)))))
        (highlight-rect-frame rect)))))


(defgeneric table-dimensions (item)
  (:documentation "

The TABLE-DIMENSIONS generic function returns a point indicating the
number of cells horizontally and vertically in the table dialog item.

ITEM:           A table dialog item.

")
  (:method ((item table-dialog-item))
    (make-point (table-columns item) (table-rows item))))



(defmethod installed-item-p ((item table-dialog-item))
  (let ((dialog (view-container item)))
    dialog))


(defgeneric map-selected-cells (item fun)
  (:documentation "

Calls function on the coordinates of each selected cell of ITEM.

ITEM:           A table dialog item.

FUN:            A (function item h v)

")
  (:method ((item table-dialog-item) fun)
    (let ((hash (table-selection-hash item)))
      (when hash
        (maphash (lambda (key value)
                   (declare (ignore value))
                   (funcall fun item (car key) (cdr key)))
                 hash)))))


(defmacro get-adjusted-size (where hash def sep)
  (let ((tmp      (gensym))
        (new-hash (gensym)))
    `(let* ((,new-hash ,hash)
            (,tmp (or (and ,new-hash (gethash ,where ,new-hash)) ,def)))
       (if (zerop ,tmp)
           0
           (+ ,tmp ,sep)))))


(defun map-column-widths (function item &optional start end from-end)
  (let ((column-width (point-h (cell-size item)))
        (hash         (column-widths-hash item))
        (sep-width    (point-h (separator-size item)))
        (start        (or start 0))
        (end          (or end (point-h (table-dimensions item)))))
    (if from-end
        (let ((column (1- end)))
          (loop
            (when (< column start) (return column))
            (funcall function
                     (get-adjusted-size column hash column-width sep-width)
                     column)
            (decf column)))
        (let ((column start))
          (loop
            (when (>= column end) (return column))
            (funcall function
                     (get-adjusted-size column hash column-width sep-width)
                     column)
            (incf column))))))


(defun map-row-heights (function item &optional start end from-end)
  (let ((row-height   (point-v (cell-size item)))
        (hash         (row-heights-hash item))
        (sep-height   (point-v (separator-size item)))
        (start        (or start 0))
        (end          (or end (point-v (table-dimensions item)))))
    (if from-end
        (let ((row (1- end)))
          (loop
            (when (< row start) (return row))
            (funcall function
                     (get-adjusted-size row hash row-height sep-height)
                     row)
            (decf row)))
        (let ((row start))
          (loop
            (when (>= row end) (return row))
            (funcall function
                     (get-adjusted-size row hash row-height sep-height)
                     row)
            (incf row))))))


(defmacro do-column-widths ((item column-width &optional (column (gensym)))
                            (&optional start end from-end)
                            &body body)
  `(block nil
     (map-column-widths (lambda (,column-width ,column)
                            (declare (ignorable ,column))
                          ,@body)
                        ,item ,start ,end ,from-end)))


(defmacro do-row-heights ((item row-height &optional (row (gensym)))
                          (&optional start end from-end)
                          &body body)
  `(block nil
     (map-row-heights (lambda (,row-height ,row)
                          (declare (ignorable ,row))
                        ,@body) ,item ,start ,end ,from-end)))



(defgeneric set-table-dimensions (item h &optional v)
  (:documentation "

The SET-TABLE-DIMENSIONS generic function sets the number of cells
horizontally and vertically according to h and v.  There is an 8 KB
limit on the total number of cells.

ITEM:           A table dialog item.

H:              Horizontal index.

V:              Vertical index. If the value of v is NIL, h is assumed
                to represent a point.

")
  (:method ((item table-dialog-item) h &optional v)
    (let* ((pt (make-point h v))
           (h  (point-h pt))
           (v  (point-v pt)))
      (when (installed-item-p item)
        (with-focused-dialog-item (item)
          (let* ((old-dims          (table-dimensions item))
                 (old-dims-greater? (or (< h (point-h old-dims)) (< v (point-v old-dims))))
                 (old-bottom-right  (cell-position-possibly-invisible item old-dims)))
            (when old-dims-greater?
              (let* ((sh (table-left-column item))
                     (sv (table-top-row item)))
                (when (or (<= h sh) (<= v sv))                
                  (let ((vis-dims (visible-dimensions item)))
                    (scroll-to-cell item 
                                    (max 0 (- h (point-h vis-dims)))
                                    (max 0 (- v (point-v vis-dims)))))))
              (map-selected-cells item (lambda (item sh sv)
                                         (when (or (<= h sh) (<= v sv))
                                           (cell-deselect item sh sv)))))
            (let* ((pos              (view-position item))
                   (size             (add-points (table-inner-size item) pos))
                   (pos-h            (point-h pos))
                   (pos-v            (point-v pos))
                   (size-h           (point-h size))
                   (size-v           (point-v size))
                   (new-bottom-right (cell-position-possibly-invisible item pt))
                   (old-br-h         (min (point-h old-bottom-right) size-h))
                   (old-br-v         (min (point-v old-bottom-right) size-v))
                   (new-br-h         (min (point-h new-bottom-right) size-h))
                   (new-br-v         (min (point-v new-bottom-right) size-v))
                   (parent           (view-container item))
                   (old-bottom-right (make-point old-br-h old-br-v))
                   (new-bottom-right (make-point new-br-h new-br-v)))
              ;; this was invalidating the wrong stuff - 0 vs pos-v etc
              (cond ((< old-br-h new-br-h)
                     (invalidate-corners parent (make-point old-br-h pos-v) new-bottom-right nil))
                    ((< new-br-h old-br-h)
                     (invalidate-corners parent (make-point new-br-h pos-v) old-bottom-right t)))
              (cond ((< old-br-v new-br-v)
                     (invalidate-corners parent (make-point pos-h old-br-v) new-bottom-right nil))
                    ((< new-br-v old-br-v)
                     (invalidate-corners parent (make-point pos-h new-br-v) old-bottom-right t)))))))
      (setf (table-rows    item) v
            (table-columns item) h)
      (fixup-scroll-bars item)
      pt)))



(defgeneric visible-dimensions (item)
  (:documentation "

The VISIBLE-DIMENSIONS generic function returns a point indicating the
number of cells visible in the horizontal and vertical dimensions.

ITEM:           A table dialog item.

")
  (:method ((item table-dialog-item))
    (or (visible-dimensions-slot item)  ; cache
        (let ((size      (view-size item))
              (cell-size (cell-size item)))
          (when (and size cell-size)
            (setf (visible-dimensions-slot item) 
                  (let* ((size   (table-inner-size item))
                         (width  (point-h size))
                         (height (point-v size)))
                    (make-point (if (column-widths-hash item)
                                    (let ((left-column (table-left-column item))
                                          (h 0))
                                      (- (do-column-widths (item column-width column) (left-column)
                                                           (incf h column-width)
                                                           (when (<= width h) (return column)))
                                         left-column))
                                    (floor width (point-h cell-size)))
                                (if (row-heights-hash item)
                                    (let ((top-row (table-top-row item))
                                          (v 0))
                                      (- (do-row-heights (item row-height row) (top-row)
                                                         (incf v row-height)
                                                         (when (<= height v) (return row)))
                                         top-row))
                                    (floor height (point-v cell-size)))))))))))


(defgeneric set-visible-dimensions (item h &optional v)
  (:documentation "

The SET-VISIBLE-DIMENSIONS generic function resizes the table so that
H cells are visible per row and V cells are visible per column.  The
new dimensions are returned as a point.

ITEM:           A table dialog item.

H:              Horizontal index.

V:              Vertical index. If the value of v is NIL, h is assumed
                to represent a point.

")
  (:method ((item table-dialog-item) h &optional v)
    (let ((cell-size (cell-size item)))
      (when cell-size
        (normalize-h&v h v)
        (let* ((table-vscrollp     (table-vscroll-bar item))
               (table-hscrollp     (table-hscroll-bar item))
               (cell-width         (point-h cell-size))
               (cell-height        (point-v cell-size))
               (row                (table-top-row item))
               (column             (table-left-column item))
               (row-heights-hash   (row-heights-hash item))
               (column-widths-hash (column-widths-hash item))
               (sep-width          (point-h (separator-size item)))
               (sep-height         (point-v (separator-size item)))
               (new-width (if (null column-widths-hash)
                              (* h (if (zerop cell-width) 0 (+ cell-width sep-width)))
                              (let ((width 0))
                                (dotimes (i h width)
                                  (incf width (get-adjusted-size column column-widths-hash cell-width sep-width))
                                  (incf column)))))
               (new-height (if (null row-heights-hash)
                               (* v (if (zerop cell-height) 0 (+ cell-height sep-height)))
                               (let ((height 0))
                                 (dotimes (i v height)
                                   (incf height (get-adjusted-size column row-heights-hash cell-height sep-height))
                                   (incf row))))))
          ;; is it really the right thing to mess with view-size?
          (set-view-size item
                         (+ new-width  (if table-vscrollp 15 0))
                         (+ new-height (if table-hscrollp 15 0))))))
    (setf (slot-value item 'visible-dimensions) (make-point h v))))





(defgeneric set-cell-size (item h &optional v)
  (:documentation "

The SET-CELL-SIZE generic function sets the cell size according to H
and V and returns the new size as a point.

ITEM:           A table dialog item.

H:              Horizontal index.

V:              Vertical index. If the value of v is NIL, h is assumed
                to represent a point.

"
                  )
  (:method ((item table-dialog-item) h &optional v)
    ;; This still has a bug that it validates more than it should.
    (let ((new-size (make-point h v)))
      (unless (and (plusp (point-h new-size))
                   (plusp (point-v new-size)))
        (error 'view-size-error :view item :size new-size
               :format-control "Invalid size ~S"
               :format-arguments (list new-size)))
      (let ((old-cell-size (cell-size item)))
        (setf (slot-value item 'visible-dimensions) nil)
        (setf (slot-value item 'cell-size) new-size)
        (when (installed-item-p item)
          (if (eql (point-v new-size) (point-v old-cell-size))
              (let ((inner-size (table-inner-size item)))
                (invalidate-all-but-left-column item inner-size inner-size))
              (invalidate-view item t))
          (fixup-scroll-bars item))))))


(defgeneric cell-font (item h &optional v)
  (:documentation "

The CELL-FONT generic function returns the font used by a cell
\(specified by H and V) or NIL if the cell uses the font of the dialog
item.

ITEM:           A table dialog item.

H:              Horizontal index.

V:              Vertical index. If the value of v is NIL, h is assumed
                to represent a point.

")
  (:method ((item table-dialog-item) h &optional v)
    (normalize-h&v h v)
    (let ((cell-fonts (table-cell-fonts item))
          (key        (cons h v)))
      (when cell-fonts
        (let ((one.two (gethash key cell-fonts)))
          (and one.two
               (font-spec (car one.two) (cdr one.two))))))))


(defgeneric set-cell-font (item cell font-spec)
  (:documentation "

The SET-CELL-FONT generic function sets the font of cell to FONT-SPEC.

ITEM:           A table dialog item.

CELL:           The cell to be drawn.

FONT-SPEC:      A font spec.

")
  (:method ((item table-dialog-item) cell font-spec)
    (let ((cell-fonts (table-cell-fonts item))
          (key        (cons (point-h cell) (point-v cell))))
      (if font-spec
          (multiple-value-bind (one two)
              (multiple-value-bind (ff ms) (view-font-codes item)
                (font-codes font-spec ff ms))
            (unless cell-fonts
              (setf cell-fonts
                    (setf (table-cell-fonts item) (make-hash-table :test 'equal))))
            (setf (gethash key cell-fonts) (cons one two)))
          (when cell-fonts
            (remhash key cell-fonts))))
    (let* ((container (view-container item))
           (pos (and container (cell-position item cell))))
      (when pos
        (invalidate-corners container pos
                            (add-points pos
                                        (make-point (table-column-width item (point-h cell))
                                                    (table-row-height   item (point-v cell)))))))
    font-spec))


(defun invert-cell-selection (item h v selected-p)
  (with-focused-dialog-item (item)
    (with-back-color (or (and (eq (cell-colors item) :background)
                              (part-color-h-v item h v))
                         (part-color item :body))
      (let* ((rgn     (if (view-active-p item)
                          (table-selection-region item)
                          (table-outline-region item)))
             (pos      (view-position item))
             (botright (add-points pos (table-inner-size item))))
        (declare (ignore botright rgn))
        (niy invert-cell-selection item h v selected-p)
        #-(and)
        (with-temp-rgns (temp-rgn)
          (#_SetRectRgn temp-rgn (point-h pos) (point-v pos) (point-h botright) (point-v botright))
          (with-clip-region temp-rgn
            (#_CopyRgn rgn temp-rgn)
            (add-to-selection-region item selected-p h v)
            (#_XorRgn rgn temp-rgn temp-rgn)
            (with-hilite-mode (#_InvertRgn temp-rgn))))))))


(defun invalidate-cell (item h &optional v)
  (normalize-h&v h v)
  (with-focused-dialog-item (item)
    (let* ((item-pos  (view-position item))
           (item-size (table-inner-size item)))
      (declare (ignore item-pos item-size))
      (niy invalidate-cell h v)
      #-(and)
      (rlet ((item-rect :rect :topleft item-pos :botright (add-points item-pos item-size))) 
            (multiple-value-bind (viz cell-size top-left) (cell-position item h v)
              (when viz
                (rlet ((cell-rect :rect :topleft top-left :botright (add-points top-left cell-size)))
                      (#_sectrect cell-rect item-rect cell-rect)
                                        ;(with-fore-color *black-color* (#_paintrect cell-rect))
                      (#_invalwindowrect (wptr item) cell-rect))))))))


(defgeneric part-color-h-v (item h v)
  (:method ((item table-dialog-item) h v)  
    (let ((hash (table-cell-color-hash item)))
      (when hash
        (let ((key (make-point h v)))
          (gethash key hash))))))


(defgeneric set-part-color-h-v (item h v new-color)
  (:method ((item table-dialog-item) h v new-color)
    (let ((hash (table-cell-color-hash item)))
      (when (null hash)
        (setf hash (make-hash-table :test (function equal)))
        (setf (table-cell-color-hash item) hash))    
      (let ((key (make-point h v)))
        (if new-color
            (setf (gethash key hash) new-color)
            (remhash key hash))
        ;; (redraw-cell item h v)
        (invalidate-cell item h v)))))


(defmethod part-color ((item table-dialog-item) key)
  (if (or (integerp key) (consp key))
      (part-color-h-v item (point-h key) (point-v key))
      (let ((color (when (symbolp key)
                     (getf (part-color-list item) key))))
        (or color
            (case key
              (:body *white-color*))))))


(defmethod set-part-color ((d table-dialog-item) part new-color)
  (without-interrupts   
      (if (or (integerp part) (consp part))
          ;; Change the color of one cell
          (set-part-color-h-v d (point-h part) (point-v part) new-color)     
          ;; Change some other color attribute
          (progn
            (call-next-method)
            (let ((hscroll (table-hscroll-bar d))
                  (vscroll (table-vscroll-bar d)))
              (when hscroll
                (set-part-color hscroll part new-color))
              (when vscroll
                (set-part-color vscroll part new-color)))
            (invalidate-view d)))))




(defgeneric cell-select (item h &optional v)
  (:documentation "

The CELL-SELECT generic function selects the cell specified by H and
V.  Previously selected cells are not affected.

ITEM:           A table dialog item.

H:              Horizontal index.

V:              Vertical index. If the value of v is NIL, h is assumed
                to represent a point.

")
  (:method ((item table-dialog-item) h &optional v)
    (normalize-h&v h v)
    (let ((hash (table-selection-hash item))
          (key (cons h v)))
      (unless (and hash (gethash key hash))
        (unless hash
          (setf hash (setf (table-selection-hash item)
                           (make-hash-table :test 'equal))))
        (if (eql 0 (hash-table-count hash))
            (setf (first-selected-cell-slot item) (make-point h v))
            (let ((first-slot (first-selected-cell-slot item)))
              (when (and first-slot
                         (or (< h (point-h first-slot))
                             (< v (point-v first-slot))))
                (setf (first-selected-cell-slot item) nil))))
        (setf (gethash key hash) t)
        (invert-cell-selection item h v t)))))


(defgeneric cell-deselect (item h &optional v)
  (:documentation "

The CELL-DESELECT generic function deselects the cell specified by H
and V.

ITEM:           A table dialog item.

H:              Horizontal index.

V:              Vertical index. If the value of v is NIL, h is assumed
                to represent a point.

")
  (:method ((item table-dialog-item) h &optional v)
    (normalize-h&v h v)
    (let ((hash (table-selection-hash item))
          (key  (cons h v)))
      (when (and hash (gethash key hash))
        (remhash key hash)
        (when (or (zerop (hash-table-count hash))
                  (let ((first (first-selected-cell-slot item)))
                    (and first
                         (= (point-h first) h)
                         (= (point-v first) v))))                  
          (setf (first-selected-cell-slot item) nil))
        (invert-cell-selection item h v nil)))))


(defgeneric cell-selected-p (item h &optional v)
  (:documentation "

The CELL-SELECTED-P generic function returns T if the cell specified
by H and V is selected.  Otherwise, it returns NIL.

ITEM:           A table dialog item.

H:              Horizontal index.

V:              Vertical index. If the value of v is NIL, h is assumed
                to represent a point.

")
  (:method ((item table-dialog-item) h &optional v)
    (normalize-h&v h v)
    (let ((hash (table-selection-hash item)))
      (when hash
        (gethash (cons h v) hash)))))


(defgeneric selected-cells (item)
  (:documentation "

The SELECTED-CELLS generic function returns a list of all the cells
selected in the table dialog item.  Each cell is represented by a
point.  If no cells are selected, NIL is returned.

ITEM:           A table dialog item.

")
  (:method ((item table-dialog-item))
    (let ((ret nil))
      (map-selected-cells item (lambda (item h v)
                                 (declare (ignore item))
                                 (push (make-point h v) ret)))
      ret)))



(defgeneric scroll-to-cell (item h &optional v)
  (:documentation "

The SCROLL-TO-CELL generic function causes the table dialog item to
scroll so that the cell specified by H and V is in the upper-left
corner.

ITEM:           A table dialog item.

H:              Horizontal index.

V:              Vertical index. If the value of v is NIL, h is assumed
                to represent a point.

")
  (:method ((item table-dialog-item) h &optional v)
    (normalize-h&v h v)
    (let* ((old-top-row          (table-top-row item))
           (old-left-column      (table-left-column item))
           (rows                 (table-rows item))
           (columns              (table-columns item))
           (visible-end-rows     (table-visible-row-count
                                  item
                                  :end-row rows
                                  :from-end t))
           (visible-end-columns  (table-visible-column-count
                                  item
                                  :end-column columns
                                  :from-end t))
           (new-top-row          (max 0 (min v (- rows    visible-end-rows))))
           (new-left-column      (max 0 (min h (- columns visible-end-columns))))
           (hscroll              (table-hscroll-bar item))
           (vscroll              (table-vscroll-bar item)))
      (setf (table-top-row item)     new-top-row
            (table-left-column item) new-left-column)
      (when hscroll
        (setf (scroll-bar-setting hscroll) new-left-column))
      (when vscroll
        (setf (scroll-bar-setting vscroll) new-top-row))
      (setf (visible-dimensions-slot item) nil)
      (with-focused-dialog-item (item)
        (let* ((pos            (view-position item))
               (inner-size     (table-inner-size item))
               (cell-size      (cell-size item))
               (separator-size (separator-size item))
               (cell-size-h    (+ (point-h cell-size) (point-h separator-size)))
               (cell-size-v    (+ (point-v cell-size) (point-v separator-size)))
               (delta-rows     (- old-top-row new-top-row))
               (delta-columns  (- old-left-column new-left-column))
               (delta-v        0)
               (delta-h        0))
          (if (row-heights-hash item)
              (cond ((< old-top-row new-top-row)
                     (do-row-heights (item row-height) (old-top-row new-top-row)
                                     (decf delta-v row-height)))
                    ((< new-top-row old-top-row)
                     (do-row-heights (item row-height) (new-top-row old-top-row)
                                     (incf delta-v row-height))))
              (setf delta-v (* delta-rows cell-size-v)))
          (if (column-widths-hash item)
              (cond ((< old-left-column new-left-column)
                     (do-column-widths (item column-width) (old-left-column new-left-column)
                                       (decf delta-h column-width)))
                    ((< new-left-column old-left-column)
                     (do-column-widths (item column-width) (new-left-column old-left-column)
                                       (incf delta-h column-width))))
              (setf delta-h (* delta-columns cell-size-h)))
          (niy scroll-to-cell item h v)
          #-(and)
          (rlet ((rect :rect :topleft pos :botright (add-points pos inner-size)))
                (without-interrupts
                    (let ((container (view-container item)))
                      (with-temp-rgns (update-rgn)
                        (get-window-updatergn wptr update-rgn)
                        (unless (#_EmptyRgn update-rgn)
                          (let* ((container-origin (subtract-points (view-origin container) (view-position (view-window container)))))
                            (with-temp-rgns (new-update-rgn item-rgn)
                              (#_CopyRgn update-rgn new-update-rgn)
                              (#_CopyRgn (view-clip-region item) item-rgn)
                                        ; Work in the container's coordinate system, since we're already focused on it.
                                        ; The windowrecord.updatergn is in global coordinates
                              (#_OffsetRgn new-update-rgn (point-h container-origin) (point-v container-origin))
                              (#_OffsetRgn item-rgn (point-h pos) (point-v pos))
                              (#_SectRgn new-update-rgn item-rgn new-update-rgn)
                              (unless (#_EmptyRgn new-update-rgn)
                                (validate-region container new-update-rgn)
                                (#_OffsetRgn new-update-rgn delta-h delta-v)
                                (#_SectRgn new-update-rgn item-rgn new-update-rgn)
                                (invalidate-region container new-update-rgn))))))
                      (if (or (> (abs delta-h) #x7fff)(> (abs delta-v) #x7fff)) ;; actually only needed if osx-p
                          (invalidate-view item)
                          (with-temp-rgns (invalid-rgn)
                            (with-back-color (part-color item :body) ;; << added
                              (#_ScrollRect rect delta-h delta-v invalid-rgn)
                              (Invalidate-region container invalid-rgn)))))))
          ;; Could just call compute-selection-regions here, but that makes
          ;; scrolling take a long time if there's a large selection.
          ;; This code does incremental selection region calculation.
          (let ((selection-region (table-selection-region item))
                (outline-region   (table-outline-region item))
                (pos-h            (point-h pos))
                (pos-v            (point-v pos))
                (inner-size-h     (point-h inner-size))
                (inner-size-v     (point-v inner-size)))
            (declare (ignore inner-size-h inner-size-v pos-v pos-h outline-region selection-region))
            (niy scroll-to-cell item h v)
            #-(and)
            (when selection-region
              (#_OffsetRgn selection-region delta-h delta-v)
              (#_OffsetRgn outline-region delta-h delta-v)
              (with-temp-rgns (rgn)
                (#_SetRectRgn rgn
                              (- pos-h cell-size-h)
                              (- pos-v cell-size-v)
                              (+ pos-h inner-size-h cell-size-h)
                              (+ pos-v inner-size-v cell-size-v))
                (#_SectRgn selection-region rgn selection-region)
                (#_SectRgn outline-region rgn outline-region))))
          (let* ((min-column (1- (table-left-column item)))
                 (left-column (table-left-column item))
                 (visible-columns (table-visible-column-count item :start-column left-column :end-column columns))
                 (max-column (+ min-column visible-columns 2))
                 (top-row (table-top-row item))
                 (visible-rows (table-visible-row-count item :start-row top-row :end-row rows))
                 (min-row (1- (table-top-row item)))
                 (max-row (+ min-row visible-rows 2)))
            (if (minusp delta-rows)
                (setf min-row (+ max-row delta-rows))
                (setf max-row (+ min-row delta-rows)))
            (if (minusp delta-columns)
                (setf min-column (+ max-column delta-columns))
                (setf max-column (+ min-column delta-columns)))
            (compute-selection-regions item min-row max-row min-column max-column)))))))


(defgeneric scroll-position (item)
  (:documentation "

The SCROLL-POSITION generic function returns the cell indices of the
cell in the upper-left corner of the table dialog item.  (This is not
a position in window coordinates but indicates which cell is in the
upper-left corner).

ITEM:           A table dialog item.

")
  (:method ((item table-dialog-item))
    (make-point (table-left-column item) (table-top-row item))))


(defun cell-position-possibly-invisible (item h &optional v)
  (normalize-h&v h v)
  (let* ((cell-size   (cell-size item))
         (cell-size-h (point-h cell-size))
         (cell-size-v (point-v cell-size))
         (sep-width   (point-h (separator-size item)))
         (sep-height  (point-v (separator-size item)))
         (top-row     (table-top-row item))
         (left-column (table-left-column item))
         (pos         (view-position item)))
    (values (make-point (+ (point-h pos)
                           (if (column-widths-hash item)
                               (let ((width 0))
                                 (do-column-widths (item column-width) (left-column h)
                                                   (incf width column-width))
                                 width)
                               (* (- h left-column)
                                  (if (zerop cell-size-h) 0 (+ cell-size-h sep-width)))))
                        (+ (point-v pos)
                           (if (row-heights-hash item)
                               (let ((height 0))
                                 (do-row-heights (item row-height) (top-row v)
                                                 (incf height row-height))
                                 height)
                               (* (- v top-row)
                                  (if (zerop cell-size-v) 0 (+ cell-size-v sep-height))))))
            (make-point (table-column-width item h)
                        (table-row-height   item v)))))


(defgeneric cell-position (item h &optional v)
  (:documentation "

The CELL-POSITION generic function returns the position of the
upperleft corner of the cell if the cell is visible.  It returns NIL
if the cell is not currently visible.  The position returned is in the
coordinate system of the ITEM’s container.

ITEM:           A table dialog item.

H:              Horizontal index.

V:              Vertical index. If the value of v is NIL, h is assumed
                to represent a point.

")
  (:method ((item table-dialog-item) h &optional v)
    (normalize-h&v h v)
    (let* ((pos        (view-position item))
           (pos-h      (point-h pos))
           (pos-v      (point-v pos))
           (inner-size (table-inner-size item)))
      (multiple-value-bind (res cell-size) (cell-position-possibly-invisible item h v)
        (values (and (<= pos-h (point-h res) (1- (+ pos-h (point-h inner-size))))
                     (<= pos-v (point-v res) (1- (+ pos-v (point-v inner-size))))
                     res)
                cell-size
                res)))))


(defgeneric point-to-cell (item h &optional v)
  (:method ((item table-dialog-item) h &optional v)
    (when (installed-item-p item)
      (normalize-h&v h v)
      (let* ((cell-size   (cell-size item))
             (cell-size-h (point-h cell-size))
             (cell-size-v (point-v cell-size))
             (sep-width   (point-h (separator-size item)))
             (sep-height  (point-v (separator-size item)))
             (top-row     (table-top-row item))
             (left-column (table-left-column item))
             (pos         (view-position item))
             (pos-h       (point-h pos))
             (pos-v       (point-v pos))
             (inner-size  (table-inner-size item))
             (inner-h     (point-h inner-size))
             (inner-v     (point-v inner-size)))
        (when (and (<= pos-h h (+ pos-h inner-h))
                   (<= pos-v v (+ pos-v inner-v)))
          (decf h pos-h)
          (decf v pos-v)
          (let ((cell-h (if (column-widths-hash item)
                          (let ((width 0))
                            (do-column-widths (item column-width column) (left-column)
                                              (when (> (incf width column-width) h)
                                                (return column))))
                          (+ left-column
                             (if (eql cell-size-h 0)
                               0
                               (floor h (+ cell-size-h sep-width))))))
                (cell-v (if (row-heights-hash item)
                          (let ((height 0))
                            (do-row-heights (item row-height row) (top-row)
                                            (when (> (incf height row-height) v)
                                              (return row))))
                          (+ top-row
                             (if (eql cell-size-v 0)
                               0
                               (floor v (+ cell-size-v sep-height)))))))
            (when (and (< cell-h (table-columns item))
                       (< cell-v (table-rows item)))
              (make-point cell-h cell-v))))))))






(defgeneric cell-contents-string (item cell)
  (:method ((item table-dialog-item) cell)
    (%cell-contents-string-new item cell nil)))


(defvar *default-cell-contents-string-combined-method* nil)
(defvar *default-draw-table-cell-combined-method*      nil)



#-(and)
(defun %find-nth-arg-combined-method (dt arg args)  
  (declare (optimize (speed 3)(safety 0)))
  (flet ((get-wrapper (arg)
           (if (not (%standard-instance-p arg))
             (let* ((class (class-of arg)))
               (or (%class.own-wrapper class)
                   (progn
                     (update-class class nil)
                     (%class.own-wrapper class))))
             (instance.class-wrapper arg))))
    (declare (inline get-wrapper))
    (let ((wrapper (get-wrapper arg)))
      (when (eql 0 (%wrapper-hash-index wrapper))
        (update-obsolete-instance arg)
        (setq wrapper (get-wrapper arg)))
      (let* ((mask (%gf-dispatch-table-mask dt))
             (index (%ilsl 1 (%ilogand mask (%wrapper-hash-index wrapper))))
             table-wrapper flag)
        (declare (fixnum index mask))
        (loop 
          (if (eq (setq table-wrapper (%gf-dispatch-table-ref dt index)) wrapper)
            (return (%gf-dispatch-table-ref dt (the fixnum (1+ index))))
            (progn
              (when (null (%gf-dispatch-table-ref dt (the fixnum (1+ index))))
                (if (or (not (eq table-wrapper (%unbound-marker-8)))
                        (eql 0 flag))
                  (without-interrupts ; why?
                   (let ((gf (%gf-dispatch-table-gf dt)))
                     (if (listp args)
                       (return (nth-arg-combined-method-trap-0 gf dt wrapper args))
                       (with-list-from-lexpr (args-list args)
                         (return (nth-arg-combined-method-trap-0 gf dt wrapper args-list))))))
                  (setq flag 0 index -2)))
              (setq index (+ 2 index)))))))))


(defun find-1st-arg-combined-method (gf arg)
  ;; TODO:
  #-ccl (niy find-1st-arg-combined-method gf arg)
  #+ccl (ccl::%find-1st-arg-combined-method (ccl::%gf-dispatch-table gf) arg))



(defgeneric cell-contents-string-new (item h &optional v)
  (:method ((item table-dialog-item) h &optional v)
    (%cell-contents-string-new item h v))
  (:method :around ((item table-dialog-item) h &optional v)
           ;; These two methods make users of the old cell-contents-string
           ;; and draw-table-cell continue to work.
           (let ((cm (find-1st-arg-combined-method #'cell-contents-string item)))
             (if (eq cm *default-cell-contents-string-combined-method*)
               (call-next-method)
               (funcall cm item (make-point h v))))))


(defgeneric draw-table-cell (item cell rect selectedp)
  (:method ((item table-dialog-item) cell rect selectedp)
    (%draw-table-cell-new item (point-h cell) (point-v cell) rect selectedp)))



;; why not call this draw-table-cell-h-v 
(defgeneric draw-table-cell-new (item h v rect selectedp)
  (:method :around ((item table-dialog-item) h v rect selectedp)
           (let ((cm (find-1st-arg-combined-method #'draw-table-cell item)))
             (if (eq cm *default-draw-table-cell-combined-method*)
               (call-next-method)
               (funcall cm item (make-point h v) rect selectedp))))
  (:method ((item table-dialog-item) h v rect selectedp)
    (%draw-table-cell-new item h v rect selectedp)))



(defmethod view-default-size ((item table-dialog-item))
  (let* ((max-width (table-max-width item))
         (max-height (table-max-height item)) 
         (rows (table-rows item))
         (columns (table-columns item))
         (visible-dimensions (visible-dimensions-slot item))
         (visible-rows (if visible-dimensions 
                           (point-v visible-dimensions)
                           rows))
         (visible-columns (if visible-dimensions
                              (point-h visible-dimensions)
                              columns))
         (cell-size (cell-size item))
         (table-hscrollp (table-hscrollp item))
         (table-vscrollp (table-vscrollp item))
         (top-row (table-top-row item))
         (left-column (table-left-column item))
         (bottom-row (+ top-row visible-rows))
         (right-column (+ left-column visible-columns)))
    (let (width height cell-height cell-width)
      (unless cell-size
        (setf (cell-size-slot item)
              (setf cell-size (default-cell-size item))))
      (setf cell-width (point-h cell-size)
            cell-height (point-v cell-size)
            width (* cell-width visible-columns)
            height (* cell-height visible-rows)
                                        ;max-width (- max-width (mod max-width cell-width))
                                        ;max-height (- max-height (mod max-height cell-height))
            )
      (if (> max-width cell-width)
          (setf max-width (- max-width (mod max-width cell-width)))
          (setf max-width cell-width))
      (if (> max-height cell-height)
          (setf max-height (- max-height (mod max-height cell-height)))
          (setf max-height cell-height))
      (let ((row-heights-hash (row-heights-hash item)))
        (when row-heights-hash
          (flet ((mapper (row height)
                   (when (and (<= top-row row) (< row bottom-row))
                     (incf height (- height cell-height)))))
            (declare (dynamic-extent #'mapper))
            (maphash #'mapper row-heights-hash))))
      (let ((column-widths-hash (column-widths-hash item)))
        (when column-widths-hash
          (flet ((mapper (column width)
                   (when (and (<= left-column column) (< column right-column))
                     (incf width (- width cell-width)))))
            (declare (dynamic-extent #'mapper))
            (maphash #'mapper column-widths-hash))))
      (if (eq table-hscrollp :undetermined)
          (setf table-hscrollp 
                (or (> width max-width)
                    (> columns visible-columns))))
      (if (eq table-vscrollp :undetermined)  ; dont mess with the slot
          (setf table-vscrollp
                (or (> height max-height)
                    (> rows visible-rows))))
      (make-point (+ (min width max-width) (if table-vscrollp 15 0))
                  (+ (min height max-height) (if table-hscrollp 15 0))))))


(defgeneric maybe-need-scroll-bars (table &optional installing)
  (:method ((table table-dialog-item) &optional installing)
    (let ((size (view-size table)))
      (when size
        (let ((h (point-h size))
              (v (point-v size))
              (container (view-container table))
              (computed-cell-size nil)
              (vscrollp (table-vscrollp table))
              (hscrollp (table-hscrollp table))
              (changed nil))
          (declare (fixnum h v))
          (when (or (eq vscrollp :undetermined)
                    (eq hscrollp :undetermined))
            (flet ((compute-cell-size ()
                     (let ((columns (table-columns table)))
                       (setf (cell-size-slot table)
                             (default-cell-size
                                 table
                                 (truncate h
                                           (if (zerop columns)
                                             1
                                             columns))))
                       (setf computed-cell-size t))))
              (declare (dynamic-extent #'compute-cell-size))
              (unless (cell-size table) (compute-cell-size))
              (let (undetermined-vscrollp)
                (flet ((do-vscrollp (&optional (vsp vscrollp))
                         (when (eq vsp :undetermined)
                           (setf undetermined-vscrollp t)
                           (let ((rows (table-rows table))
                                 (height 0))
                             (setf vscrollp
                                   (and (> rows 1)
                                        (progn (do-row-heights (table row-height) () (incf height row-height))
                                               (> height v))))
                             ))
                         (when vscrollp
                           (decf h 15)
                           (when computed-cell-size (compute-cell-size))))
                       (do-hscrollp ()
                         (when (eq hscrollp :undetermined)
                           (let ((cols (table-columns table))
                                 (width 0))
                             (setf hscrollp
                                   (and (> cols 1)
                                        (progn (do-column-widths (table column-width) () (incf width column-width))
                                               (> width h))))
                             ))
                         (when hscrollp
                           (decf v 15))))
                  (do-vscrollp)
                  (do-hscrollp)
                  (when (and undetermined-vscrollp (not vscrollp) hscrollp)
                    (do-vscrollp :undetermined))))))
          (let ((vbar (table-vscroll-bar table))
                (hbar (table-hscroll-bar table)))
            (if vscrollp
              (when (not vbar)
                (let ((bar (make-scroll-bar-for-table table :vertical)))
                  (setf changed t)
                  (setf (table-vscroll-bar table) bar)))
              (when vbar
                (set-view-container vbar nil)
                (setf changed t)
                (setf (table-vscroll-bar table) nil)))
            (if hscrollp
              (when (not hbar)
                (let ((bar (make-scroll-bar-for-table table :horizontal)))
                  (setf changed t)
                  (setf (table-hscroll-bar table) bar)))
              (when hbar ; maybe its not cool to remove existing bars??
                (set-view-container hbar nil) 
                (setf changed t)
                (setf (table-hscroll-bar table) nil))))
          ;; fixup-scroll-bars only does something if table installed and visible-dimensions makes sense.
          ;; - get size and pos right before set container
          (when (and changed (not installing))
            (fixup-scroll-bars table)
            (when vscrollp
              (when container (set-view-container (table-vscroll-bar table) container)))
            (when hscrollp
              (when container (set-view-container (table-hscroll-bar table) container)))
            (maybe-fix-cell-size table))          
                                        ;(when changed (maybe-fix-cell-size table))
          changed
          )))))


(defmethod set-view-level :after ((item table-dialog-item) level)
         (let ((hscroll (table-hscroll-bar item))
               (vscroll (table-vscroll-bar item))
               (level+1 (1+ level))
               (container (view-container item)))
           (when (and container (< level+1 (length (view-subviews container))))
             (when hscroll (set-view-level hscroll level+1))
             (when vscroll (set-view-level vscroll level+1)))))

(defmethod set-view-container :after ((item table-dialog-item) container)
  (let ((hscroll (table-hscroll-bar item))
        (vscroll (table-vscroll-bar item)))
    (when hscroll (set-view-container hscroll container))
    (when vscroll (set-view-container vscroll container))))

(defgeneric (setf separator-size) (new-size item)
  (:method :around (new-size (item table-dialog-item))
           (let ((old-size (separator-size item)))
             (prog1 (call-next-method)
               (unless (= new-size old-size)
                 (setf (visible-dimensions-slot item) nil) ; clear cache
                 (when (installed-item-p item)
                   (compute-selection-regions item)
                   (invalidate-view item t)
                   (fixup-scroll-bars item)))))))

(defgeneric (setf separator-visible-p) (value item)
  (:method :around (value (item table-dialog-item))
           (let ((old-visible-p (separator-visible-p item)))
             (prog1
                 (call-next-method)
               (unless (eq (not (null value)) (not (null old-visible-p)))
                 (invalidate-view item t)
                 (fixup-scroll-bars item))))))

(defgeneric (setf separator-color) (new-color item)
  (:method :around (new-color (item table-dialog-item))
           (let ((old-color (separator-color item)))
             (prog1
                 (call-next-method)
               (unless (equal new-color old-color)
                 (invalidate-view item t))))))

(defgeneric separator-pattern (item)
  (:method ((item table-dialog-item))
    (let ((pattern (separator-pattern-slot item)))
      (typecase pattern
        (pattern  pattern)
        (symbol   (and (boundp pattern) (symbol-value pattern)))
        (function (funcall pattern))
        (t        nil)))))

(defgeneric (setf separator-pattern) (new-pattern item)
  (:method :around (new-pattern (item table-dialog-item))
           (let ((old-pattern (separator-pattern item)))
             (prog1
                 (call-next-method)
               (unless (equal new-pattern old-pattern)
                 (invalidate-view item t))))))

(defun string-width-for-focused-control (string ff ms)
  (niy string-width-for-focused-control string ff ms)
  #-(and)
  (let ((len (length string)))
    (%stack-block ((sb (%i+ len len)))
                  (copy-string-to-ptr string 0 len sb)
                  (xtext-width sb len ff ms)))
  100)


(defgeneric default-cell-size (item &optional width)
  (:method ((item table-dialog-item) &optional width)
    (let ((dialog (view-container item)))
      (with-focused-dialog-item (item dialog)
        (multiple-value-bind (ff ms) (view-font-codes item)
          (let ((table-max-width (table-max-width item)))
            (let ((rows (table-rows item))
                  (columns (table-columns item))
                  (dwidth width)
                  height string)
              (niy default-cell-size item width)
              #-(and)
              (rlet ((fp :fontinfo))
                    (#_GetFontinfo fp)
                    (setf height
                          (+ (rref fp fontinfo.ascent)
                             (rref fp fontinfo.descent)
                             (rref fp fontinfo.leading))))
              (unless dwidth
                (setf dwidth 1)
                (dotimes (h columns)
                  (dotimes (v rows)
                    (setf string (cell-contents-string-new item h v))
                    (setf dwidth (max dwidth (string-width-for-focused-control string ff ms)))))
                (setf dwidth (min (+ dwidth 6) table-max-width)))
              (make-point (max 5 dwidth) (max 5 height)))))))))





(defparameter %draw-cell-string-stream
  ;; (make-truncating-string-stream 255)
  (make-string-output-stream))

(defun %cell-contents-string-new (item h v)
  (normalize-h&v h v)
  (let ((contents (cell-contents item h v))
        (print-function (slot-value item 'table-print-function)))
    (if (or (and (or (eq print-function #'princ)
                     (eq print-function #'write-string))
                 (or (stringp contents)
                     (and (symbolp contents)(setf contents (symbol-name contents))))))
        contents
        (progn
          (catch :truncate
            (funcall print-function contents %draw-cell-string-stream))
          (get-output-stream-string %draw-cell-string-stream)))))







(defclass table-scroll-bar (scroll-bar-dialog-item)
  ())


(defvar *table-scroll-bar-tracked-part* nil)

(defmethod track-scroll-bar :around ((item table-scroll-bar) value part)
  (declare (ignore value))
  (let ((*table-scroll-bar-tracked-part* part))
    (call-next-method)))

(defmethod scroll-bar-page-size ((item table-scroll-bar))
  (let* ((table (scroll-bar-scrollee item))
         (vertical? (eq item (table-vscroll-bar table)))
         (dimensions (table-dimensions table))
         (inner-size (table-inner-size table)))
    (if vertical?
        (let ((size-v (point-v inner-size)))
          (if (eq *table-scroll-bar-tracked-part* :in-page-up)
              (table-visible-row-count table 
                                       :end-row (table-top-row table)
                                       :from-end t
                                       :size-v size-v)
              (table-visible-row-count table
                                       :start-row (table-top-row table)
                                       :end-row (point-v dimensions)
                                       :size-v size-v)))
        (let ((size-h (point-h inner-size)))
          (if (eq *table-scroll-bar-tracked-part* :in-page-up)
              (table-visible-column-count table
                                          :end-column (table-left-column table)
                                          :from-end t
                                          :size-h size-h)
              (table-visible-column-count table
                                          :start-column (table-left-column table)
                                          :end-column (point-h dimensions)
                                          :size-h size-h))))))

(defun table-visible-column-count (table &key
                                         (start-column 0)
                                         (end-column (point-h (table-dimensions table)))
                                         (from-end nil)
                                         (size-h (point-h (table-inner-size table))))
  (let ((col-count 0)
        (width 0))
    (do-column-widths (table w) (start-column end-column from-end)
                      (incf width w)
                      (when (> width size-h) (return))
                      (incf col-count))
    col-count))

(defun table-visible-row-count (table &key
                                      (start-row 0)
                                      (end-row (point-v (table-dimensions table)))
                                      (from-end nil)
                                      (size-v (point-v (table-inner-size table))))
  (let ((row-count 0)
        (height 0))
    (do-row-heights (table h) (start-row end-row from-end)
                    (incf height h)
                    (when (> height size-v) (return))
                    (incf row-count))
    row-count))

(defmethod install-view-in-window ((item table-dialog-item) dialog)
  (declare (ignore dialog))
  (without-interrupts                   ; still necessary?
      ;; (compute-selection-regions item)     ; moved up from below
      (call-next-method)
    (let* ((changed (maybe-need-scroll-bars item t))
           (item-size (table-inner-size item nil))
           (cell-size (cell-size item))
           (columns (table-columns item))
           (visible-dimensions (visible-dimensions-slot item))  ; huh
           )
      (unless cell-size
        (setf (cell-size-slot item)
              (setf cell-size 
                    (default-cell-size
                        item
                        (truncate (point-h item-size)
                                  (if (zerop columns)
                                      1
                                      columns))))))
      (compute-selection-regions item)     ; moved here by wws guess
      (if visible-dimensions (set-visible-dimensions item visible-dimensions))
      (fixup-scroll-bars item)  ;; moved this
      (when changed
        (let ((container (view-container item)))
          (when (table-vscroll-bar item)            
            (set-view-container (table-vscroll-bar item) container))
          (when (table-hscroll-bar item)
            (set-view-container (table-hscroll-bar item) container))))
                                        ;(compute-selection-regions item)
      )))

;; CLIM adds a method for this
(defgeneric make-scroll-bar-for-table (table direction)
  (:method ((table table-dialog-item) direction)
    (make-instance 'table-scroll-bar
      :scrollee table
      :view-position #@(-3000 -3000)    ; mAYBE no longer needed
      :track-thumb-p (scroll-bar-track-thumb-p table)
      :direction direction)))



                                        ; These are only safe to use when the table is not in its window.
(defgeneric (setf table-hscrollp) (value item)
  (:method (value (item table-dialog-item))
    (unless value
      (let ((scroll-bar (table-hscroll-bar item)))
        (when scroll-bar
          (setf (table-hscroll-bar item) nil)
          (set-view-container scroll-bar nil))))
    (setf (table-hscrollp-slot item) (not (null value)))))


(defgeneric (setf table-vscrollp) (value item)
  (:method (value (item table-dialog-item))
    (unless value
      (let ((scroll-bar (table-vscroll-bar item)))
        (when scroll-bar
          (setf (table-vscroll-bar item) nil)
          (set-view-container scroll-bar nil))))
    (setf (table-vscrollp-slot item) (not (null value)))))


(defun fixup-scroll-bar-levels (item)
  (declare (ignore item)))


(defun wholly-visible-columns (table &optional
                                     (visible-dimensions (visible-dimensions table))
                                     (size-h (point-h (table-inner-size table))))
  (if (not visible-dimensions)
      0
      (let ((visible-columns (point-h visible-dimensions))
            (width 0))
        (do-column-widths (table w) (0 visible-columns) (incf width w))
        (if (> width size-h)
            (1- visible-columns)
            visible-columns))))

(defun wholly-visible-rows (table &optional
                                  (visible-dimensions (visible-dimensions table))
                                  (size-v (point-v (table-inner-size table))))
  (if (not visible-dimensions)
      0
      (let ((visible-rows (point-v visible-dimensions))
            (height 0))
        (do-row-heights (table h) (0 visible-rows) (incf height h))
        (if (> height size-v)
            (1- visible-rows)
            visible-rows))))

(defun fixup-scroll-bars (table)
  (when (installed-item-p table)
    (when (visible-dimensions table)
      (let* ((size (table-inner-size table))
             (size-h (point-h size))
             (size-v (point-v size))
             (grow-icon-p (table-grow-icon-p table))
             (position (view-position table))
             (pos-h (point-h position))
             (pos-v (point-v position))
             (hscroll (table-hscroll-bar table))
             (vscroll (table-vscroll-bar table))
             changed)
        (when hscroll
          (let* ((columns (table-columns table))
                 (visible-end-columns (table-visible-column-count
                                       table
                                       :end-column columns
                                       :from-end t)))
            (set-view-position hscroll pos-h (+ pos-v size-v))
            (set-view-size hscroll
                           (+ size-h (if (and (not vscroll) grow-icon-p) -15 0))
                           16)
            (let ((old-max (scroll-bar-max hscroll))
                  (new-max (max 0 (- columns visible-end-columns))))
              (set-scroll-bar-max hscroll new-max)
              (when (not (eql new-max old-max))
                (setf changed t)))))
        (when vscroll
          (let* ((rows (table-rows table))
                 (visible-end-rows (table-visible-row-count
                                    table
                                    :end-row rows
                                    :from-end t)))
            (set-view-position vscroll (+ pos-h size-h)  pos-v)
            (set-view-size vscroll 
                           16
                           (+ size-v  (if (and (not hscroll) grow-icon-p) -15 0)))
            (let ((old-max (scroll-bar-max vscroll))
                  (new-max (max 0 (- rows visible-end-rows))))
              (set-scroll-bar-max vscroll new-max)
              (when (not (eql new-max old-max))                
                (setf changed t)))))
        (when changed
          (scroll-bar-changed table t)
          (invalidate-view table))))))


(defmethod remove-view-from-window :after ((item table-dialog-item))  
  (setf (visible-dimensions-slot item) nil)
  (dispose-selection-regions item))


(defun compute-selection-regions (item &optional min-row max-row min-column max-column)
  ;; If the selection crosses a visible boundary right at a
  ;; cell boundary, then the outline-region stops there and
  ;; it appears that else is selected.
  ;; Let them report it as a bug.
  (niy compute-selection-regions item min-row max-row min-column max-column)
  #-(and)
  (let ((rgn (table-selection-region item)))
    (if (and rgn (macptrp rgn))
        (unless min-row
          (#_SetEmptyRgn rgn))
        (setf (table-selection-region item) (#_NewRgn)
              min-row nil)))
  #-(and)
  (let ((rgn (table-outline-region item)))
    (if (and rgn (macptrp rgn))
        (unless min-row
          (#_SetEmptyRgn rgn))
        (setf (table-outline-region item) (#_NewRgn)
              min-row nil)))
  (let* ((something-selected? nil))
    (map-selected-cells item (lambda (item h v)
                               (when (or (null min-row)
                                         (<= min-row v max-row)
                                         (<= min-column h max-column))
                                 (when (add-to-selection-region item t h v nil)
                                   (setf something-selected? t)))))
    (when something-selected?
      (add-to-selection-region item t nil nil t))))


(defun dispose-selection-regions (item)
  (niy dispose-selection-regions item)
  #-(and) 
  (let ((rgn (table-selection-region item)))
    (when rgn
      (setf (table-selection-region item) nil)
      (#_DisposeRgn rgn)))
  #-(and) 
  (let ((rgn (table-outline-region item)))
    (when rgn
      (setf (table-outline-region item) nil)
      (#_DisposeRgn rgn))))


(defgeneric add-to-selection-region (item selected-p h &optional v dont-compute-outline-region)
  (:method ((item table-dialog-item) selected-p h &optional v
            dont-compute-outline-region)
    (let ((selection-region (table-selection-region item))
          (visible-dimensions (visible-dimensions item)))
      (when visible-dimensions
        (prog1
            (when (and selection-region h)
              (normalize-h&v h v)
              (let* ((top-row (1- (table-top-row item)))
                     (left-column (1- (table-left-column item)))
                     ;;(visible-dimensions (visible-dimensions item))
                     (bottom-row (+ top-row (point-v visible-dimensions) 2))
                     (right-column (+ left-column (point-h visible-dimensions) 2)))
                (when (and (<= top-row v bottom-row)
                           (<= left-column h right-column))
                  (multiple-value-bind (ignore cell-size top-left) (cell-position item h v)
                    (declare (ignore ignore))
                    (when top-left
                      (let ((bottom-right (add-points top-left cell-size)))
                        (declare (ignore bottom-right))
                        (niy add-to-selection-region item selected-p h v dont-compute-outline-region)
                        #-(and)
                        (with-temp-rgns (rgn)
                          (#_SetRectRgn rgn
                                        (point-h top-left)(point-v top-left)
                                        (point-h bottom-right)(point-v bottom-right))
                          (if selected-p
                            (#_UnionRgn rgn selection-region selection-region)
                            (#_DiffRgn selection-region rgn selection-region))
                          t)))))))      ; did something
          #-(and)
          (unless dont-compute-outline-region
            (let ((outline-region (table-outline-region item)))
              (#_CopyRgn selection-region outline-region)
              (#_InsetRgn outline-region 1 1)
              (#_XorRgn selection-region outline-region outline-region))))))))


(defun table-inner-size (table &optional size)
  (when (not size)(setq size (view-size table)))
  (if (null size)
      #@(0 0)
      (let ((h (point-h size))
            (v (point-v size)))      
        (if (table-vscroll-bar table)
            (setq h (- h 15)))
        (if (table-hscroll-bar table)
            (setq v (- v 15)))
        (make-point h v))))







(defmethod set-view-position ((item table-dialog-item) h &optional v
                              &aux (new-pos (make-point h v)))
  (let ((pos (view-position item)))
    (unless (eql pos new-pos)
      (without-interrupts
          (call-next-method)   ; just plain MCL bug - set new-pos before calling fixup-scroll-bars   
        (if pos
            (let ((diff (and pos (subtract-points new-pos pos)))
                  (hscroll (table-hscroll-bar item))
                  (vscroll (table-vscroll-bar item)))
              (when hscroll
                (set-view-position hscroll (add-points (view-position hscroll) diff)))
              (when vscroll
                (set-view-position vscroll (add-points (view-position vscroll) diff))))
            (fixup-scroll-bars item))
                                        ;(call-next-method)
        (when (wptr item)
          (compute-selection-regions item))))))

(defgeneric maybe-fix-cell-size (item)
  (:method ((item table-dialog-item))
    (when (<= (point-h (table-dimensions item)) 1) ; or (sequence-order wrap-length)
      (when (and (view-size item)(cell-size item))
        (let ((inner-size (table-inner-size item)))
          (set-cell-size item
                         (make-point (max 1 (point-h inner-size))
                                     (max 1 (point-v (cell-size item))))))))))

(defmethod view-corners ((item table-dialog-item))
  (multiple-value-call #'inset-corners #@(-1 -1) (call-next-method)))


(defvar *updating* nil)


(defgeneric table-row-height (item row)
  (:method ((item table-dialog-item) row)
    (check-type row integer)
    (let ((row-heights-hash (row-heights-hash item)))
      (or (and row-heights-hash (gethash row row-heights-hash))
          (let ((cell-size (cell-size item)))
            (and cell-size (point-v cell-size)))))))

(defgeneric table-column-width (item column)
  (:method ((item table-dialog-item) column)
    (check-type column integer)
    (let ((column-widths-hash (column-widths-hash item)))
      (or (and column-widths-hash (gethash column column-widths-hash))
          (let ((cell-size (cell-size item)))
            (and cell-size (point-h cell-size)))))))

(defgeneric (setf table-row-height) (value item row)
  (:method (value (item table-dialog-item) row)
    (check-type row integer)
    (check-type value (or null fixnum))
    (let ((hash (or (row-heights-hash item)
                    (and value (setf (row-heights-hash item) (make-hash-table :test 'eql))))))
      (if value
        (setf (gethash row hash) value)
        (when hash
          (remhash row hash)
          (when (eql 0 (hash-table-count hash))
            (setf (row-heights-hash item) nil)))))
    (setf (visible-dimensions-slot item) nil) ; clear cache
    (when (installed-item-p item)
      (compute-selection-regions item)
      (invalidate-view item t)) ; optimize this to scroll appropriately and invalidate minimally
    (when (table-vscroll-bar item)
                                        ;(scroll-bar-changed item t)
      (fixup-scroll-bars item))
    value))

(defgeneric (setf table-column-width) (value item column)
  (:method (value (item table-dialog-item) column)
    (check-type column integer)
    (check-type value (or null fixnum))
    (let ((hash (or (column-widths-hash item)
                    (and value (setf (column-widths-hash item) (make-hash-table :test 'eql))))))
      (if value
        (setf (gethash column hash) value)
        (when hash
          (remhash column hash)
          (when (eql 0 (hash-table-count hash))
            (setf (column-widths-hash item) nil)))))
    (setf (visible-dimensions-slot item) nil) ; clear cache
    (when (installed-item-p item)
      (compute-selection-regions item)
      (invalidate-view item t)) ; optimize this to scroll appropriately and invalidate minimally
    (when (table-hscroll-bar item)
                                        ;(scroll-bar-changed item t)
      (fixup-scroll-bars item))
    value))

(defmethod view-draw-contents ((item table-dialog-item))
  (without-interrupts
      (let* ((dialog (view-container item)))
        (with-focused-dialog-item (item dialog)
          (let* ((dialog-item-enabled-p (dialog-item-enabled-p item))
                 (color-p               (and (not dialog-item-enabled-p)
                                             (color-or-gray-p item))) 
                 (color-list            (part-color-list item))
                 (back-color            (part-color item :body))
                 (pos                   (view-position item))
                 (inner-size            (table-inner-size item)))
            (declare (ignore inner-size pos back-color color-list color-p))
            (niy view-draw-contents item)
            #-(and)
            (rlet ((rect :rect :topleft pos :botright (add-points pos inner-size)))
                  (with-clip-rect-intersect rect
                    (with-temp-rgns (rgn #+carbon-compat rgn3)
                      (#_getclip rgn)
                      (with-back-color back-color
                        (when back-color
                          (#_erasergn rgn)
                          #+ignore
                          (with-fore-color back-color
                            (#_paintrgn rgn)))
                        (when (and *updating* dialog-item-enabled-p)
                          (let ((selection-rgn (if (view-active-p item)
                                                   (table-selection-region item)
                                                   (table-outline-region item))))
                            (with-hilite-mode
                                (#_InvertRgn selection-rgn))))                   
                        (let ()
                          (get-window-visrgn wptr rgn3)
                          (#_sectrgn rgn rgn3 rgn))
                        (let* ((row (table-top-row item))
                               (column (table-left-column item))
                               (rows (table-rows item))
                               (columns (table-columns item))
                               (first-column column)
                               (cell-size (cell-size item))
                               (column-width (point-h cell-size))
                               (row-height (point-v cell-size))
                               (column-widths-hash (column-widths-hash item))
                               (row-heights-hash (row-heights-hash item))
                               (separator-visible-p (separator-visible-p item))
                               (separator-size (separator-size item))
                               (separator-color (separator-color item))
                               (separator-pattern (separator-pattern item))
                               (might-draw-separator (and separator-visible-p
                                                          (not (eql separator-size #@(0 0)))
                                                          (macptrp separator-pattern)))
                               (draw-col-separator (and might-draw-separator (> columns 1))) ;nil)
                               (top-left (view-position item))
                               (bottom-right (add-points top-left (table-inner-size item)))
                               (top (point-v top-left))
                               (left (point-h top-left))
                               (right (point-h bottom-right))
                               (bottom (point-v bottom-right)))
                          (rlet ((rect :rect :topleft top-left :botright bottom-right))
                                (with-clip-rect-intersect rect
                                  (loop
                                    (let ((row-height (or (and row-heights-hash (gethash row row-heights-hash)) row-height)))
                                      (when (plusp row-height)
                                        (setf (pref rect :rect.bottom) (+ (pref rect :rect.top) row-height))
                                        (setf (pref rect :rect.left) left)
                                        (setq column first-column)
                                        (loop
                                          (let ((column-width (or (and column-widths-hash (gethash column column-widths-hash))
                                                                  column-width)))
                                            (setf (pref rect :rect.right) 
                                                  (+ (pref rect :rect.left) column-width))
                                            (when (and (plusp column-width)
                                                       (#_RectInRgn rect rgn))
                                              (unless (or (>= column columns) (>= row rows))
                                                (draw-table-cell-new item column row rect (cell-selected-p item column row))
                                                (when draw-col-separator
                                                  ;; draw the column separator to the right of the current
                                                  (with-fore-color separator-color
                                                    (with-pen-saved-simple
                                                        (#_PenSize (point-h separator-size) (point-v separator-size))
                                                      (#_PenPat separator-pattern)
                                                      (#_MoveTo (pref rect :rect.right) top)
                                                      (#_LineTo (pref rect :rect.right) (pref rect :rect.bottom)))))))
                                            (incf column)
                                            (when (or (>= column columns)
                                                      (>= (incf (pref rect :rect.left) 
                                                                (if (zerop column-width) 
                                                                    0 
                                                                    (+ column-width (point-h separator-size))))
                                                          right))
                                              (return))))
                                        (when (and might-draw-separator (< row rows))
                                          ;; draw the row separator below the current row
                                          (with-fore-color separator-color
                                            (with-pen-saved-simple
                                                (#_PenSize (point-h separator-size)(point-h separator-size))
                                              (#_PenPat separator-pattern)
                                              (#_MoveTo left (pref rect :rect.bottom))
                                              (#_LineTo (pref rect :rect.right) (pref rect :rect.bottom))))))
                                      (incf row)
                                      (when (or (>= row rows)
                                                (>= (incf (pref rect :rect.top) 
                                                          (if (zerop row-height) 
                                                              0 
                                                              (+ row-height (point-v separator-size))))
                                                    bottom))
                                        (return)))))))))))
            #-(and)
            (with-item-rect (r item)
              (with-fore-color (getf color-list :frame nil)               
                (#_insetRect r -1 -1)
                (#_FrameRect r))
              (when (and (not dialog-item-enabled-p) (not color-p))
                (rlet ((ps :penstate))
                      (#_GetPenState ps)
                      (#_PenPat *gray-pattern*)
                      (#_PenMode 11)
                      (#_PaintRect r)
                      (#_SetPenState  ps)))))))))


(defmethod scroll-bar-changed ((item table-dialog-item) scroll-bar)
                                        ;(declare (ignore scroll-bar))
  (let* ((hscroll (table-hscroll-bar item))
         (vscroll (table-vscroll-bar item))
         (hscroll-setting (if hscroll (scroll-bar-setting hscroll) 0))
         (vscroll-setting (if vscroll (scroll-bar-setting vscroll) 0)))
    (scroll-to-cell item hscroll-setting vscroll-setting)
    (when (and (wptr item) (not (eq scroll-bar t)))  
      ;; dont bother (FROM FIXUP-SCROLL-BARS), it does invalidate view after this and further
      ;; when doing list-definitions-dialog the grafport-back-color is wrong at this point
      ;; because the dialog-item-action for the editable-text-dialog-item is called
      ;; within the scope of with-text-colors. (phooey). That once mattered because
      ;; set-table-dimensions was invalidating (with erase) some stuff outside the table.
      ;; That bug is fixed now but there is still no need to call window-update-event-handler.
      (window-update-event-handler (view-window item)))))





(defvar *table-fore-color* nil)

;;; draw gray if not enabled and color-p
(defun %draw-table-cell-new (item h v rect selectedp)
  (let* ((container (view-container item))
         (enabled-p (dialog-item-enabled-p item))
         (color-p (if (not enabled-p)(color-or-gray-p item))))
    (with-focused-view container
      (let ((cell-fonts (table-cell-fonts item)))
        (multiple-value-bind (ff ms) (view-font-codes item)
          (let* ((top (rect-top rect))
                 (key (cons h v))
                 (back-color-p (eq (cell-colors item) :background))
                 (cell-color (part-color-h-v item h v)))
            (declare (ignore top))
            (declare (dynamic-extent key))
            (without-interrupts
                (let* ((font (and cell-fonts
                                  (gethash key cell-fonts)))
                       (back-color (or (and back-color-p cell-color)
                                       (part-color item :body)))
                       (fore-color (if (and (not enabled-p) color-p)
                                       *gray-color*
                                       (or (and (not back-color-p) cell-color)
                                           (part-color item :text)
                                           *table-fore-color*)))
                       (pos (view-position item))
                       (botright (add-points pos (table-inner-size item))))
                  (declare (ignore botright fore-color back-color))
                  (setq ff (or (car font) ff)
                        ms (or (cdr font) ms))
                  (niy %draw-table-cell-new item h v rect selectedp)
                  #-(and)
                  (rlet ((table-inner-rect :rect :topleft pos :botright botright))
                        (with-clip-rect-intersect table-inner-rect
                          (progn ;with-clip-rect-intersect rect - draw-string-in-rect does it
                            (with-back-color back-color
                              (#_eraserect rect)  ;;  change scope -weird?? - from Gilles Bisson  UNDO change
                              (rlet ((my-rect :rect))
                                    (copy-record rect :rect my-rect)
                                    (incf (pref my-rect :rect.left) 3)  ;; ok to clobber rect? - not if multiple columns
                                    (let ((string (cell-contents-string-new item h v)))
                                      (draw-string-in-rect string my-rect :truncation :end :ff ff :ms ms :color fore-color)))
                              (when (and selectedp (not *updating*) enabled-p)
                                (with-hilite-mode
                                    (#_InvertRgn (if (view-active-p item)
                                                     (table-selection-region item)
                                                     (table-outline-region item)))))))))))))))))



(defgeneric first-selected-cell (item)
  (:method ((item table-dialog-item))
    (or (first-selected-cell-slot item)
        (let ((hash (table-selection-hash item)))
          (when (and hash (plusp (hash-table-count hash)))
            (let ((min-h most-positive-fixnum)
                  (min-v most-positive-fixnum))
              ;; find min row in min column
              (maphash (lambda (key val)
                           (declare (ignore val))
                         (let ((ph (car key))
                               (pv (cdr key)))
                           (if (eql ph min-h)
                             (setq min-v (min min-v pv))
                             (if (< ph min-h)
                               (progn                               
                                 (setq min-h ph min-v pv))))))
                       hash)                                         
              (setf (first-selected-cell-slot item)(make-point min-h min-v))))))))








(defmethod view-activate-event-handler ((item table-dialog-item))
  (unless (view-active-p item)
    (setf (view-active-p item) t)
    (when (dialog-item-enabled-p item)
      (toggle-cell-outlining item))))


(defmethod view-deactivate-event-handler ((item table-dialog-item))
  (when (view-active-p item)
    (setf (view-active-p item) nil)
    (when (dialog-item-enabled-p item)
      (toggle-cell-outlining item))))


(defun toggle-cell-outlining (item)
  ;; Toggle between selection & outline mode in all but the update region.
  ;; The update region will get drawn by view-draw-contents.
  (with-focused-dialog-item (item)
    (let ((body-color (part-color item :body)))
      (with-back-color body-color
        (niy toggle-cell-outlining)
        #-(and)
        (with-temp-rgns (rgn clip-rgn update-rgn)
          (#_GetClip clip-rgn)          
          (get-window-updatergn (wptr item) update-rgn)
          (let ((off (subtract-points #@(0 0) (view-position (view-window item)))))
            (#_OffsetRgn update-rgn (point-h off)(point-v off)))
          (let ((off (view-origin (view-container item))))
            (#_OffsetRgn  update-rgn (point-h off)(point-v off)))
          (#_DiffRgn clip-rgn update-rgn clip-rgn)
          (let ((pos (view-position item))
                (size (table-inner-size item)))
            (let ((br (add-points pos size)))
              (#_SetRectRgn rgn (point-h pos)(point-v pos) (point-h br)(point-v br)))
            (#_SectRgn clip-rgn rgn clip-rgn))
          (with-clip-region clip-rgn
            (let ((selection-region (table-selection-region item))
                  (outline-region (table-outline-region item)))
              (#_CopyRgn selection-region rgn)
              (#_XorRgn outline-region rgn rgn)
              (if (not (colored-cells-p  item))
                  (with-hilite-mode
                      (#_InvertRgn rgn))
                  (with-temp-rgns (cell-rgn)  ; << do colored cells 1 by 1
                    (let ((hash (table-selection-hash item)))
                      (when hash
                        (let ((f (lambda (key value)
                                     (declare (ignore value))
                                     (let ((h (car key))(v (cdr key)))
                                       (multiple-value-bind (pos size)
                                           (cell-position item h v)
                                         (when pos
                                           (with-back-color (or (part-color-h-v item h v) body-color)
                                             (let ((br (add-points pos size)))
                                               (#_setrectrgn cell-rgn (point-h pos)(point-v pos)(point-h br)(point-v br)))  ; coords?
                                             (#_sectrgn cell-rgn clip-rgn cell-rgn)
                                             (#_sectrgn cell-rgn rgn cell-rgn)                                     
                                             (with-hilite-mode (#_invertrgn cell-rgn)))))))))
                          (declare (dynamic-extent f))
                          (maphash f hash)))))))))))))


(defmethod view-click-event-handler ((item table-dialog-item) where)
  (niy view-click-event-handler item where)
  (progn                                ;without-interrupts
    (let* ((pos (view-position item))
           (botright (add-points pos (table-inner-size item))))
      (if (not (point<= where botright))
          (if (> (point-h where) (point-h botright))
              (let ((vscroll (table-vscroll-bar item)))
                (when vscroll
                  (view-click-event-handler vscroll where)))
              (let ((hscroll (table-hscroll-bar item)))
                (when hscroll
                  (view-click-event-handler hscroll where))))
          (let* ((type (selection-type item))
                 (shift-key-p (shift-key-p))
                 (command-key-p (command-key-p))
                 (container (view-container item))
                 (top-row (table-top-row item))
                 (left-column (table-left-column item))
                 (rows (table-rows item))
                 (bottom-row (+ top-row rows))
                 (columns (table-columns item))
                 (right-column (+ left-column columns))
                 (left (point-h pos))
                 (top (point-v pos))
                 (right (point-h botright))
                 (bottom (point-v botright))
                 h v where-h where-v start-selected-p now-in-range last-h last-v)
            (with-focused-dialog-item (item)
              (with-back-color (part-color item :body)
                (with-timer             ;without-interrupts ;; <<
                    (multiple-value-bind (start-h start-v start-in-range) (find-clicked-cell item where)
                      (if start-in-range
                          (setq start-selected-p (cell-selected-p item start-h start-v))
                          (deselect-cells item))
                      (loop
                        (without-interrupts
                            (setq where-h (point-h where)
                                  where-v (point-v where))
                          (multiple-value-setq (h v now-in-range) (find-clicked-cell item where))
                          (multiple-value-setq (left-column top-row)
                            (do-auto-scroll item left-column top-row columns rows where-h where-v left top right bottom))
                          (if (and (not now-in-range)(not start-in-range)(not command-key-p)) ;(not shift-key-p))
                              (deselect-cells item)
                              (when (and now-in-range
                                         (<= left-column h)
                                         (< h right-column)
                                         (<= top-row v)
                                         (< v bottom-row)
                                         (not (and (eql h last-h) (eql v last-v))))
                                (setq last-h h last-v v)
                                (cond ((and (eq type :disjoint)
                                            (or shift-key-p command-key-p)                                 
                                            (eql h start-h)(eql v start-v))
                                       (if shift-key-p
                                           (cell-select item h v)
                                           (if start-selected-p
                                               (cell-deselect item h v)
                                               (cell-select item h v))))
                                      ((and (eq type :disjoint)
                                            command-key-p
                                            start-selected-p)
                                       (deselect-cells-between item start-h start-v h v))
                                      ((or (eq type :single)
                                           (and (not shift-key-p)
                                                (or ;(eq type :contiguous)
                                                 (not command-key-p))))
                                       (let* ((hash (table-selection-hash item))
                                              (colored-cells-p (colored-cells-p item)))
                                         (declare (ignore hash colored-cells-p))
                                         (niy view-click-event-handler item where)
                                         #-(and)
                                         (with-temp-rgns (rgn)
                                           (#_SetRectRgn rgn (point-h pos)(point-v pos) (point-h botright)(point-v botright))
                                           (with-clip-region rgn
                                             (with-hilite-mode
                                                 (if (cell-selected-p item h v)
                                                     (if (eq type :single)
                                                         (cell-select item h v)
                                                         (when hash
                                                           (when colored-cells-p
                                                             (let ((f (lambda (k val)
                                                                          (declare (ignore val))
                                                                          (unless (and (eql (car k) h)
                                                                                       (eql (cdr k) v))
                                                                            (cell-deselect item k)))))
                                                               (declare (dynamic-extent f))
                                                               (maphash f hash)))
                                                           (clrhash hash)
                                                           (setf (gethash (cons h v) hash) t)
                                                           (setf (first-selected-cell-slot item) (make-point h v))                                          
                                                           (with-temp-rgns (invert-region)
                                                             (let ((selection-region
                                                                    (if (view-active-p item)
                                                                        (table-selection-region item)
                                                                        (table-outline-region item))))
                                                               (#_CopyRgn selection-region invert-region)
                                                               (compute-selection-regions item)
                                                               (when (not colored-cells-p)
                                                                 (#_DiffRgn invert-region selection-region invert-region)
                                                                 (#_InvertRgn invert-region))
                                                               (cell-select item h v))))) ; << fixes bengtsons double click thing
                                                     (progn 
                                                       (when hash
                                                         (when colored-cells-p ; <<
                                                           (deselect-cells item))
                                                         (clrhash hash)
                                                         (setf (first-selected-cell-slot item) nil)
                                                         (when (not colored-cells-p) ; <<
                                                           (#_InvertRgn (if (view-active-p item)
                                                                            (table-selection-region item)
                                                                            (table-outline-region item))))
                                                         (compute-selection-regions item))
                                                       (cell-select item h v))))))))
                                      ((and (eq type :contiguous)
                                            command-key-p
                                            (eql h start-h)(eql v start-v))                          
                                       (deselect-cells item)
                                       (when (not start-selected-p)(cell-select item h v)))
                                      ((and (eq type :contiguous)
                                            shift-key-p
                                            (cell-selected-p item h v))
                                       (deselect-cells-above item h v))                           
                                      (t 
                                       (let* ((p (if (eq type :contiguous)(first-selected-cell item)))
                                              (first-h (if p (point-h p) start-h))
                                              (first-v (if p (point-v p) start-v)))
                                         (if (and (eq type :contiguous)  ; don't know bout this
                                                  shift-key-p
                                                  (/= 1 (point-h (table-dimensions item))))
                                             (multiple-value-bind (max-h max-v)(max-selected-h&v item)
                                               (select-cells-between item
                                                                     (min first-h h)
                                                                     (min first-v v)
                                                                     (max first-h h max-h)
                                                                     (max first-v v max-v)))
                                             (select-cells-between item first-h first-v h v))))))))
                        #-(and)
                        (when (not (#_stilldown))
                          (return))
                        #-(and)
                        (if (eql where (%get-local-mouse-position))                   
                            (unless (wait-mouse-up-or-moved)
                              (return)))
                        (setq where (view-mouse-position container))))))
              (dialog-item-action item)))))))


(defun get-local-mouse-position ()
  (niy get-local-mouse-position)
  #-(and)
  (rlet ((pt :point))
        (#_GetMouse pt)
        (%get-point pt)))

(defgeneric deselect-cells-between (item first-h first-v h v)
  (:method ((item table-dialog-item) first-h first-v h v)
    (when (< h first-h)
      (rotatef h first-h))
    (when (< v first-v)
      (rotatef v first-v))
    (loop
      :for i :from first-h :to h
      :do (loop
            :for j :from first-v :to v
            :do (cell-deselect item i j)))))


(defgeneric select-cells-between (item first-h first-v h v)
  (:method ((item table-dialog-item) first-h first-v h v)
    (when (< h first-h)
      (rotatef h first-h))
    (when (< v first-v)
      (rotatef v first-v))
    (loop
      :for i :from first-h :to h
      :do (loop
            :for j :from first-v :to v
            :do (cell-select item i j)))))


(defgeneric deselect-cells-above (item h v)
  (:method ((item table-dialog-item) h v)
    (let ((hash (table-selection-hash item)))
      (when hash
        (maphash (lambda (k val)
                     (declare (ignore val))
                   (when (or (> (car k) h)
                             (> (cdr k) v))
                     (cell-deselect item k)))
                 hash)))))


(defgeneric max-selected-h&v (item)
  (:method ((item table-dialog-item))
    (let* ((hash (table-selection-hash item))
           (max-h -1)
           (max-v -1))    
      (when hash        
        (maphash (lambda (k val)
                     (declare (ignore val))
                   (setq max-h (max max-h (car k)))
                   (setq max-v (max max-v (cdr k))))
                 hash))
      (values max-h max-v))))


(defgeneric deselect-cells (item)
  (:method ((item table-dialog-item))
    (let ((hash (table-selection-hash item)))
      (when hash
        (maphash (lambda (k v)
                     (declare (ignore v))
                   (cell-deselect item k))
                 hash)))))



(defgeneric colored-cells-p (item)
  (:method ((item table-dialog-item))
    (and (eq (cell-colors item) :background)
         (let ((hash (table-cell-color-hash item)))
           (and hash (plusp (hash-table-count hash)))))))


(defvar *last-auto-scroll-time* 0)
(defparameter *auto-scroll-period* 6)    ; minimum 1/10 second between auto-scrolls

(defun do-auto-scroll (item left-column top-row columns rows where-h where-v left top right bottom)
  (declare (fixnum where-h where-v left top right bottom))
  (unless (or (and (<= left where-h) (< where-h right)
                   (<= top where-v) (< where-v bottom))
              (< (- (get-tick-count) *last-auto-scroll-time*) *auto-scroll-period*))
    (setq *last-auto-scroll-time* (get-tick-count))
    (let* ((cell-size (cell-size item))
           (cell-size-h (point-h cell-size))
           (cell-size-v (point-v cell-size)))
      (cond ((< where-h left)
             (decf left-column (ceiling (- left where-h) cell-size-h))
             (when (< left-column 0)
               (setq left-column 0)))
            ((>= where-h right)
             (let ((delta-columns (floor (- where-h right) cell-size-h)))
               (declare (fixnum delta-columns))
               (unless (< (- where-h (* delta-columns cell-size-h)) right)
                 (incf delta-columns))
               (incf left-column delta-columns)
               (let ((visible-columns (floor (- right left) cell-size-h)))
                 (declare (fixnum visible-columns))
                 (when (> (+ left-column visible-columns) columns)
                   (setq left-column (- columns visible-columns)))))))
      (cond ((< where-v top)
             (decf top-row (ceiling (- top where-v) cell-size-v))
             (when (< top-row 0)
               (setq top-row 0)))
            ((>= where-v bottom)
             (let ((delta-rows (floor (- where-v bottom) cell-size-v)))
               (declare (fixnum delta-rows))
               (unless (< (- where-v (* delta-rows cell-size-v)) bottom)
                 (incf delta-rows))
               (incf top-row delta-rows)
               (let ((visible-rows (floor (- bottom top) cell-size-v)))
                 (declare (fixnum visible-rows))
                 (when (> (+ top-row visible-rows) rows)
                   (setq top-row (- rows visible-rows))))))))
    (scroll-to-cell item left-column top-row)
    (window-update-event-handler (view-window item)))
  (values left-column top-row))

(defgeneric find-clicked-cell (item where)
  (:method ((item table-dialog-item) where)
    (setq where (convert-coordinates where (view-container item) item))  
    (let* ((top-row (table-top-row item))
           (left-column (table-left-column item))
           (cell-size (cell-size item))
           (sep-width (point-h (separator-size item)))
           (sep-height (point-v (separator-size item)))
           (where-h (point-h where))
           (where-v (point-v where))
           (column (if (column-widths-hash item)
                     (let ((width 0))
                       (do-column-widths (item column-width column) (left-column)
                                         (when (> (incf width column-width) where-h)
                                           (return column))))
                     (+ left-column 
                        (if (zerop (point-h cell-size))
                          0
                          (floor where-h (+ (point-h cell-size) sep-width))))))
           (row (if (row-heights-hash item)
                  (let ((height 0))
                    (do-row-heights (item row-height row) (top-row)
                                    (when (> ( incf height row-height) where-v)
                                      (return row))))
                  (+ top-row 
                     (if (zerop (point-v cell-size))
                       0
                       (floor where-v (+ (point-v cell-size) sep-height)))))))
      (let* ((table-cols (table-columns item))
             (table-rows (table-rows item)))
        (values (max 0 (min (1- table-cols) column))
                (max 0 (min (1- table-rows) row))
                (and (<= 0 column (1- table-cols))
                     (<= 0 row (1- table-rows))))))))




(defun invalidate-all-but-left-column (item old-inner-size inner-size)
  (when (installed-item-p item)
    (with-focused-dialog-item (item)
      (let* ((pos (view-position item))                  
             (2-chars-wide (multiple-value-bind (a d w) (font-info) a d (* 2 w))))
        (let ((isize (if (< (point-h inner-size)(point-h old-inner-size))
                         inner-size
                         old-inner-size)))
          (invalidate-corners 
           *current-view*
           (add-points pos (make-point (- (point-h isize) 2-chars-wide) 0))
           (add-points pos (view-size item))   ;; was isize
           t))))))

(defmethod set-view-size ((item table-dialog-item) h &optional v &aux
                          (new-size (make-point h v))
                          (inner-size (table-inner-size item new-size))
                          (old-size (view-size item))
                          (visible-dimensions (visible-dimensions item)))
  
  (unless (eql new-size old-size)
    (without-interrupts
        (setf (slot-value item 'visible-dimensions) nil)     
      (if (installed-item-p item)
          (progn
            (let ((old-inner-size (table-inner-size item)))
              (call-next-method)
              (when t ;(osx-p)  ;; << workaround for resize turds - the real bug is elsewhere, but where?
                (let ((w (view-window item)))
                  (when (and (not (window-theme-background w))(slot-value w 'back-color))
                    (with-focused-view w
                      (window-update-event-handler w)))))           
              (maybe-need-scroll-bars item)
              (setq inner-size (table-inner-size item new-size))  ; may have changed
              (cond ((<= (point-h (table-dimensions item)) 1)    ; or (sequence-order wrap-length)
                     (set-cell-size item
                                    (make-point (max 1 (point-h inner-size))
                                                (max 1 (point-v (cell-size item))))))
                    (t (fixup-scroll-bars item)))
              ;; was this wrong in original??
              (invalidate-all-but-left-column item  old-inner-size inner-size)))
          (call-next-method))     
      (let* ((left-column (table-left-column item))
             (top-row (table-top-row item)))       
        (when (installed-item-p item)
          (scroll-to-cell item left-column top-row)
          (let* ((new-left-column (table-left-column item))
                 (new-top-row (table-top-row item))
                 (old-dims-v (point-v visible-dimensions))
                 (old-dims-h (point-h visible-dimensions)))
            (unless (and (eql left-column new-left-column)
                         (eql top-row new-top-row))
              (with-focused-dialog-item (item)
                (let ((2-chars-wide (multiple-value-bind (a d w) (font-info) a d (* 2 w)))
                      (top-left (cell-position item left-column top-row)))
                  (when top-left
                    (validate-corners (view-container item)
                                      top-left
                                      (add-points top-left
                                                  (make-point (- (if (column-widths-hash item)
                                                                     (let ((width 0))
                                                                       (do-column-widths (item column-width)
                                                                         (left-column (+ left-column old-dims-h))
                                                                         (incf width column-width))
                                                                       width)
                                                                     (* (point-h (cell-size item)) old-dims-h))
                                                                 2-chars-wide)
                                                              (if (row-heights-hash item)
                                                                  (let ((height 0))
                                                                    (do-row-heights (item row-height)
                                                                      (top-row (+ top-row old-dims-v))
                                                                      (incf height row-height))
                                                                    height)
                                                                  (* (point-v (cell-size item)) old-dims-v))))))))))))
      (compute-selection-regions item)))
  new-size)


(defun toggle-selection-region (item)
  (with-focused-dialog-item (item)
    (let ((selection-rgn (if (view-active-p item)
                             (table-selection-region item)
                             (table-outline-region item))))
      (declare (ignore selection-rgn))
      (niy toggle-selection-region item)
      #-(and)
      (with-hilite-mode
          (#_InvertRgn selection-rgn)))))


(defmethod dialog-item-disable ((item table-dialog-item))
  (when (dialog-item-enabled-p item)
    (setf (dialog-item-enabled-p item) nil)
    (let ((hscroll (table-hscroll-bar item))
          (vscroll (table-vscroll-bar item)))
      (when hscroll
        (dialog-item-disable hscroll))
      (when vscroll
        (dialog-item-disable vscroll)))
    (when (installed-item-p item)
      (invalidate-view item))))


(defmethod dialog-item-enable ((item table-dialog-item))
  (unless (dialog-item-enabled-p item)
    (setf (dialog-item-enabled-p item) t)
    (let ((hscroll (table-hscroll-bar item))
          (vscroll (table-vscroll-bar item)))
      (when hscroll
        (dialog-item-enable hscroll))
      (when vscroll
        (dialog-item-enable vscroll)))
    (when (installed-item-p item)
      (invalidate-view item))))


(defun two-byte-script-p (script)
  (niy two-byte-script-p script)
  #-(and)
  (not (logbitp #$smsfSingByte (#_getscriptvariable script #$smscriptflags))))



(defgeneric cell-to-index (item h &optional v)
  (:documentation  "

The CELL-TO-INDEX generic function returns an index into the sequence
associated with the dialog item, corresponding to the cell whose indices in
the table are H and V.  If there is no such cell, it returns NIL.
This index is suitable for passing to the Common Lisp function elt.

ITEM:           A sequence dialog item.

H:              Horizontal index.

V:              Vertical index.  If the value of V is NIL, H is
                assumed to represent a point.

")
  (:method ((item table-dialog-item) h &optional v)
    (when (null v)
      (setq v (point-v h) h (point-h h)))
    (let* ((dims (table-dimensions item))
           (h-dim (point-h dims))
           (v-dim (point-v dims)))
      (if (and (< v v-dim)(< h h-dim))
        (+ h (* v h-dim))))))


(defun initialize/table-dialog-item ()
  (setf *default-cell-contents-string-combined-method*
        (find-1st-arg-combined-method (function cell-contents-string)
                                      (class-prototype (find-class 'table-dialog-item)))
        
        *default-draw-table-cell-combined-method*
        (find-1st-arg-combined-method (function draw-table-cell)
                                      (class-prototype (find-class 'table-dialog-item)))

        *table-fore-color* *black-color*)
  (values))


;;;; THE END ;;;;
