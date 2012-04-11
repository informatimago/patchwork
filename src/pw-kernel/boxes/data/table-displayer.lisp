;;;; -*- mode:lisp; coding:utf-8 -*-
;;===================
;;Allegro CL version of the table displayer, for now .....
;;======================

(in-package :patch-work)

(defclass C-disp-window (C-application-window) ())

(defmethod window-grow-event-handler ((self C-disp-window) where)
  (declare (ignore where))
  (call-next-method)
  (update-size (first (subviews self)) (view-size self)))

(defclass C-array-item (table-dialog-item)
  ((my-array :initform (make-array '(2 2)) :initarg :my-array :accessor my-array)))

(defmethod update-size ((self C-array-item) size)
  (set-view-size self 
                 (subtract-points  size (make-point 20 20))))

(defmethod cell-contents ((self C-array-item) h &optional v)
  (if v (aref (my-array self) h v) (aref (my-array self) (point-h h) (point-v h))))

(defmethod set-array ((self C-array-item) the-array)
  (let* ((dims (array-dimensions the-array))
        (dialog (view-container self))
        (max-dims (apply #'max dims))
        (size-h (+ 20 (* 20 max-dims)))
        (size-v (+ 12 (* 12 max-dims))))
    (remove-subviews dialog self)
    (set-view-size self size-h size-v)
    (set-table-dimensions self  (first dims) (second dims))
    (setf (my-array self) the-array)
    (add-subviews dialog self)))
    
(defclass C-table-displayer (C-patch-application)
  ((table :initform 
          (make-array '(11 7) :initial-element #\Space :adjustable t)
          :initarg :table :accessor table)
   (current-layout :initform 'l )
   (MenuBox :initform () :initarg :MenuBox :accessor menubox)
   ))

(defvar *Table-disp-popUpMenu*
  (new-menu 
   " "
   (new-leafmenu "Open" #'(lambda() (open-patch-win *target-action-object* )))
   (new-leafmenu "Column Center" #'(lambda() (table-layout *target-action-object* 'cc)))
   ;(new-leafmenu "Row Center" #'(lambda() (table-layout *target-action-object* 'rc)))
   (new-leafmenu "Right justify" #'(lambda() (table-layout *target-action-object* 'r)))
   ;(new-leafmenu "Left justify" #'(lambda() (table-layout *target-action-object* 'l)))
))


(defmethod initialize-instance :after((self C-table-displayer) &key rest)
  (declare (ignore rest))
  (let ((popUpBox (make-popUpbox  "" 
                                  self
                                  *Table-disp-popUpMenu*
                                  :view-position (make-point (- (w self) 10)
                                                  (- (h self) 14))
                                  :view-container self
                                  :view-font '("monaco"  9  :srcor)
                                  )))
    (setf (menubox self) popUpBox)
    ))

(defmethod make-application-object ((self C-table-displayer))
  (make-instance 'C-disp-window
                 :view-subviews 
                 (list (make-instance 'C-array-item
                                      :view-font '("Monaco" 9  :plain)
                                      :table-dimensions (make-point 2 2)
                                      :cell-size (make-point 20 12)
                                      :view-size (make-point 100 80)))
                 :window-show nil
                 :window-title "Tdisp"))



(defmethod open-patch-win ((self C-table-displayer))
  (if (not (wptr (application-object self)))
    (setf (application-object self) (make-application-object self)))
  (set-array (car (subviews (application-object self))) (table self))
  (window-select (application-object self)))

(defmethod table-layout ((self C-table-displayer) option)
  (let* ((data (patch-value (car (input-objects self)) self))
         (maxlength (apply 'max 
                           (mapcar #'(lambda(row) (length (list! row))) data)))
         )
    
    (cond ((eq option 'cc)
           (fill-table-disp self  maxlength data 2))
           ((eq option 'r) 
            (fill-table-disp self maxlength data 1)))))

(defmethod fill-table-disp ((self C-table-displayer) maxlength data increm)
  (let ((init-pos 0) (index 0))
    (setf (table self)
          (make-array  (list  (* maxlength increm)(length data))
                  :initial-element #\Space))
    (dolist (row data)
      (setq init-pos (- maxlength (length (list! row)) increm))
      (mapc #'(lambda (elem) 
                (setf (aref (table self) (incf init-pos increm) index) elem))
            (list! row))
      (incf index))))

(defmethod patch-value ((self C-table-displayer) obj)
  (patch-value (car (input-objects self)) obj))

(defunp Tdisp ((list list)) list
"displays a list or table (list of lists) supplied on input, centered
by column or right justified according to the popUpMenu selection"
  (declare (ignore list)))
