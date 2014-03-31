;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pw-graphics.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;    
;;;;    XXX
;;;;    
;;;;AUTHORS
;;;;    Mikael Laurson, Jacques Duthen, Camilo Rueda.
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
(in-package :pw)
(enable-patchwork-reader-macros)
(objcl:enable-objcl-reader-macros)


(defvar *r-view-temp-region* nil)
(on-load-and-now init/r-view-temp-region
  (setq *r-view-temp-region* (new-region)))

;;======================================
(defun grow-gray-rect (anchor float view limit)
  (let* ((float     (add-points float anchor))
         (old-mouse anchor)
         (offset    (subtract-points float old-mouse))
         (delta     (add-points offset old-mouse))
         (limit     (and limit (add-points anchor (make-point limit limit))))
         (event     (make-event)))
    (flet ((draw-gray-rect (rect)
             (with-focused-view view
               (with-pen-state (:pattern *gray-pattern* :mode :patCopy)
                 (draw-rect rect)))))
      (with-instance-drawing view
        (draw-gray-rect (pt2rect anchor delta))
        (loop :while (mouse-down-p) :do
          (let ((new-mouse (view-mouse-position view)))
           (unless (eql old-mouse new-mouse) ;has the mouse moved?
             (new-instance view)
             (setf delta (add-points offset new-mouse))
             (when limit (setq delta (point-max delta limit)))
             (draw-gray-rect (pt2rect anchor delta))
             (get-next-event event nil 0 1)
             (setf old-mouse new-mouse))))      
        (new-instance view)))
    delta))


(defun point-max (point-1 point-2)
  (make-point (max (point-h point-1) (point-h point-2))
              (max (point-v point-1) (point-v point-2))))


;;======================================
;; simple-view methods

(defgeneric x (self)
  (:method ((self simple-view)) (point-h (view-position self))))
(defgeneric y (self)
  (:method ((self simple-view)) (point-v (view-position self))))
(defgeneric w (self)
  (:method ((self simple-view)) (point-h (view-size self))))
(defgeneric h (self)
  (:method ((self simple-view)) (point-v (view-size self))))
(defgeneric x+w (self)
  (:method ((self simple-view)) (+ (x self)(w self))))
(defgeneric y+h (self)
  (:method ((self simple-view)) (+ (y self)(h self))))

(defgeneric active-point (self)
  (:method ((self simple-view)) (values)))
(defgeneric active-chord (self)
  (:method ((self simple-view)) (values)))
(defgeneric active-note (self)
  (:method ((self simple-view)) (values)))
(defgeneric reset-active-point (self)
  (:method ((self simple-view)) (values)))
(defgeneric reset-active-chord (self)
  (:method ((self simple-view)) (values)))
(defgeneric view-mouse-dragged (self mouse)
  (:method ((self simple-view) mouse)
    (declare (ignore mouse))
    (values)))
(defgeneric view-mouse-moved (self mouse)
  (:method ((self simple-view) mouse)
    (declare (ignore mouse))
    (values)))
(defgeneric view-mouse-up (self)
  (:method ((self simple-view)) (values)))
;; check !

(defgeneric view-contains-point-p+self (self mouse)
  (:method ((self simple-view) mouse)
    (when (view-contains-point-p self mouse)
      self)))

(defgeneric erase-draw-contents (self)
  (:method ((self simple-view))  
    (ui::with-view-frame (x y w h) self (erase-rect* x y w h))))

(defgeneric erase-view-inside-rect (self)
  (:method ((self simple-view))
    (with-pen-state (:pattern *white-pattern* :mode :patcopy)
      (fill-rect* 1 1 (- (w self) 2)(- (h self) 2))))
  (:method ((self view))
    (with-pen-state (:pattern *white-pattern* :mode :patcopy)
      (fill-rect* (1+ (point-h (view-scroll-position self))) 
                  (1+ (point-v (view-scroll-position self))) (- (w self) 2)(- (h self) 2)))))

(defgeneric erase+view-draw-contents (self)
  (:method ((self simple-view))
    (with-focused-view self
      (erase-view-inside-rect self))
    (view-draw-contents self)))

(defgeneric view-draw-out-line (self)
  (:method ((self simple-view))
    (let* ((topleft     (view-position  self))
           (bottomright (add-points topleft (view-size self))))
      (draw-rect* (point-h topleft)     (point-v topleft)
                  (point-h bottomright) (point-v bottomright)))))


(defun inside-rectangle? (x1 y1 x y w h)
  (and (<= x x1 (+ x w)) (<= y y1 (+ y h))))

(defgeneric view-window-grown (self)
  (:method ((self simple-view))
    (values)))

(defgeneric dmove-view-scroll-position (self dx dy)
  (:method ((self view) dx dy)
    (set-view-scroll-position self (+ (x self) dx)(+ (y self) dy))))


;;(time (repeat 1000 (inside-rectangle? 77 77  50 60 28 18)))
;;================================================
;;pop-up-menu

(defvar *pw-pop-menu-window* ())
(defvar *pw-pop-menu-window-TABLE-SEQUENCE* ())

(defun make-pop-up-menu-instance ()
  (setf *pw-pop-menu-window*
        (MAKE-INSTANCE 'DIALOG
            :WINDOW-TYPE :DOCUMENT 
            :VIEW-POSITION #@(426 42)
            :VIEW-SIZE #@(118 128)
            :CLOSE-BOX-P NIL
            :VIEW-FONT '("Chicago" 12 :SRCOR :PLAIN)
            :window-show nil
            :window-title "Browse"
            :VIEW-SUBVIEWS
            (LIST (MAKE-DIALOG-ITEM 'SEQUENCE-DIALOG-ITEM
                                    #@(2 24)
                                    #@(114 95)
                                    "Untitled"
                                    (lambda (item)
                                        (window-hide *pw-pop-menu-window*)
                                        (call-pw-pop-up-function 
                                         (cell-contents item 
                                                        (car (selected-cells ITEM)))))
                                    :VIEW-FONT '("Monaco" 9 :SRCCOPY :PLAIN)
                                    :CELL-SIZE #@(200 12)
                                    :TABLE-HSCROLLP NIL
                                    :TABLE-VSCROLLP T
                                    :TABLE-SEQUENCE '(FOO GLORP NJ))
                  (MAKE-DIALOG-ITEM 'BUTTON-DIALOG-ITEM
                                    #@(4 4)
                                    #@(46 17)
                                    "Cancel"
                                    (lambda (item) item (window-hide *pw-pop-menu-window*))
                                    :VIEW-FONT '("Monaco" 9 :SRCCOPY :PLAIN)
                                    :DEFAULT-BUTTON NIL))))) 


(defun call-pw-pop-up-function (res-string)
  (when res-string
    (eval (cdr (assoc res-string *pw-pop-menu-window-TABLE-SEQUENCE* :test #'string=)))))

(defun make-pw-pop-up (lst)
  (setf *pw-pop-menu-window-TABLE-SEQUENCE* lst)
  ;; (set-view-size  (car (subviews *pw-pop-menu-window*)) (make-point (max  (* (length lst) 12) 220) 55))
  (set-TABLE-SEQUENCE (car (subviews *pw-pop-menu-window*)) (mapcar #'car lst))
  (window-select *pw-pop-menu-window*))


;;;; THE END ;;;;
