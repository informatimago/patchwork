;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;=========================================================
;;;;
;;;;  PATCH-WORK
;;;;  By Mikael Laurson, Jacques Duthen, Camilo Rueda.
;;;;  © 1986-1992 IRCAM 
;;;;
;;;;=========================================================

;;===========================================================================
;; Classes for looping and Multi-Dimensional treatment
;;
;;Class: C-enumerator  ; ALLWAYS in connection with C-collector (on output)
;;
;; Inherits from : C-patch
;; methods:
;;  store-buffer          ; sets up the box's buffer with a given value
;;  get-list              ; collects a list from a patch linked to its input
;;  patch-value           ; returns the buffer's current value
;;
;;Class: C-collector    ;ALLWAYS in connection with a  C-enumerator and a patch.
;; Inherits from : C-patch
;; Methods:
;;  Patch-value          ;Asks C-enumerator for its input list and cycles trough it.
;;
;;===========================================================================

(in-package :pw)

(defclass C-enum-collect-source (C-patch)
  ((buffer :initform () )
   (comp-var)))

(defmethod store-buffer ((self C-enum-collect-source) val)
  (setf (slot-value self 'buffer) val))

(defmethod get-list ((self C-enum-collect-source) obj)
  (patch-value (car (input-objects self)) obj))

(defmethod patch-value ((self C-enum-collect-source) obj)
  (declare (ignore  obj))
  (slot-value self 'buffer))

(defclass C-enum-collect-sink (C-patch) ())

(defmethod patch-value ((self C-enum-collect-sink) obj)
  (mapcar #'(lambda (val)
              (store-buffer (first (input-objects self)) val)
              (patch-value (second (input-objects self)) obj))
          (get-list (first (input-objects self)) obj)))

(defmethod disconnect-ctrl ((self C-enum-collect-sink) ctrl)
  (if (eq ctrl (car (pw-controls self)))
     (progn (ed-beep)(format t "Disconnection of the first input is not allowed !"))
     (call-next-method)))

(setf *enum-collect-type*
       (make-instance 'C-pw-type
  :control-form 
  `(make-instance 'C-ttybox :view-size (make-point 36 12) 
        :dialog-item-text "coll" :type-list '(collector))))

(setf *MD-object-type*
       (make-instance 'C-pw-type
  :control-form `(make-instance 'C-ttybox :view-size (make-point 36 12)
      :dialog-item-text "coll" :type-list '())))

(defunp enum+loop ((en-ob (symbol (:type-list (loop))))
                   (patch (list (:type-list ())))) list
   "loops thru a list of elements supplied as input to Enum box and evaluates
    the second input"
  (declare (ignore en-ob patch)))

(defunp enum ((list list)) nil "to be used only with enum+loop or enum+reduce"
  (declare (ignore list)))

(defunp trigger ((patch1 list (:value "patch" :type-list ()))
                 (patch2 list (:value "patch" :type-list ()))
 &rest (patch list (:value "patch" :type-list ()))) nil "This extensible module launches the evaluation of many patches in sequence. 
The sequence of evaluation is equal to the sequence of the inputs."
(declare (ignore patch1 patch2 patch)))

(defclass C-map-first (C-pw-functional) ())

(defunp pwmap ((en-ob (symbol (:type-list (loop))))
                   (patch (list (:type-list ())))
                   &rest (en-obs (symbol (:type-list (loop))))) list
   "This group of modules creates a list starting with the evaluation of a patch that takes 
into account all the elements of a list connected to the module enum. The output of the 
patch must be connected to the input patch of pwmap. Since this module is extensible, 
it is possible to control many lists, by opening the inputs arg to which is connected a 
module enum. In the case of lists ofvarious sizes, pwmap will select the shortest one. "
  (declare (ignore en-ob patch en-obs)))

(defunp map-first ((en-ob (symbol (:type-list (loop))))
                   (patch (list (:type-list ())))
                   &rest (en-obs (symbol (:type-list (loop))))) list
   "loops thru a list of elements supplied as input to Enum box and evaluates
    the second input"
  (declare (ignore en-ob patch en-obs)))

(defmethod patch-value ((self C-map-first) obj)
  (let* ((inputs (input-objects self))
         (enums (list* (first inputs) (nthcdr 2 inputs)))
         (lists (ask-all enums #'get-list obj))
         (min-length (apply #'min (mapcar #'length lists)))
         res)
    (dotimes (item min-length (nreverse res))
      (dotimes (enum-item (length enums))
        (store-buffer (nth enum-item enums) (nth item (nth enum-item lists))))
      (push (patch-value (second inputs) obj) res))))

(defmethod correct-extension-box ((self C-map-first) new-box values)
  (call-next-method)
  (let ((enum (make-PW-standard-box 'C-enum-collect-source 'enum))
        (even (zerop (rem (length (pw-controls new-box)) 2))))
    (add-subviews *active-patch-window* enum)
       (set-view-position enum 
          (make-point (if even 
                        (+ (x new-box) (w new-box))
                        (x new-box)) (- (y new-box) (h new-box) )))
       (connect-ctrl new-box (car (last (pw-controls new-box))) enum)
       (setf (open-state (car (last (pw-controls new-box)) )) nil)
       (tell (controls *active-patch-window*) 'draw-connections)))

(defmethod disconnect-ctrl ((self C-map-first) ctrl)
  (if (eq ctrl (second (pw-controls self)))
    (call-next-method)
     (progn (ed-beep)(format t "Disconnection of the enum input is not allowed !"))
     ))
    

    
