;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               create-patch-box.lisp
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

(defgeneric set-default-pw-value (self value))
(defmethod set-default-pw-value ((self simple-view) value)
  (declare (ignore value))
  (values))

(defmethod set-default-pw-value ((self C-numbox) value)
  (when (numberp value)
    (setf (value self) value)
    (set-dialog-item-text self (format nil "~5D" value))))

(defmethod set-default-pw-value ((self C-menubox) value)
  (when (numberp value)
    (setf (value self) value)
    (set-dialog-item-text self (menubox-value self))))

(defmethod set-default-pw-value ((self C-ttybox) value)
  (when value
    (cond ((stringp value) (set-dialog-item-text self (string-downcase value)))
          ((listp value) (set-dialog-item-text self (prin1-to-string value)))
          ((numberp value) (set-dialog-item-text self (format nil "~5D" value)))
          ((symbolp value) (set-dialog-item-text self (string-downcase (string value)))))))


;;==========================

(defun order-inside-box-by-two (ws hs w-max)
  (let (res temp h-temp
            (w-sum 0))
    (setq w-max (1+ (max (apply #'max ws) w-max)))
    (while ws
      (setq w-sum 0)
      (setq temp () h-temp ())
      (while (and ws (< (length temp) 3) (< (+ (car ws) w-sum)  w-max))
        (incf w-sum (car ws))
        (push (pop ws) temp)
        (push (pop hs) h-temp))
      (push (list (nreverse temp) (apply #'max h-temp)) res))
    (nreverse res)))

;;(order-inside-box-by-two '(56) '(65) 78)
;;(order-inside-box-by-two '(56 36 36 55 36) '(65 15 5 14 66) 78)
;;(order-inside-box-by-two '(56 36 36 42 36 42 87 36 36 36) '(5 5 5 5 5 5 65 5 15 5) 78)

(defun make-patch-box
    (patch pw-function pw-control-type-list &optional out-type-list defaults-list)
  (let (input-boxes (w-now 46)(y-now 5))
    (when pw-control-type-list
      (while pw-control-type-list
        (push (eval (control-form (eval (pop pw-control-type-list)))) input-boxes)
        (setf (doc-string (car input-boxes)) (pop pw-control-type-list)))
      (setq input-boxes (nreverse input-boxes))
      (let* ((input-boxes-temp input-boxes)
             (ws (ask-all input-boxes 'w))
             (hs (ask-all input-boxes 'h))
             (box-list+ys (order-inside-box-by-two ws hs 84))
             (box-w-list (mapcar #'first box-list+ys))
             (row-ys (mapcar #'second box-list+ys))
             )
        (while box-w-list
          (set-view-position (car input-boxes-temp) (make-point 5 y-now))
          ;;       (setf (x (car input-boxes-temp)) 5)
          ;;       (setf (y (car input-boxes-temp)) y-now)
          (pop input-boxes-temp)
          (when (= (length  (car box-w-list)) 2)
            (set-view-position (car input-boxes-temp)
                               (make-point (+ 5 2 (car (first box-w-list))) y-now))
            ;;         (setf (x (car input-boxes-temp)) (+ 5 2 (car (first box-w-list))))
            ;;         (setf (y (car input-boxes-temp)) y-now)
            (pop input-boxes-temp))
          (incf y-now (+ 2 (pop row-ys)))
          (pop box-w-list))
        (setq w-now (+ 5 (apply #'max (ask-all input-boxes 'x+w))))
        ;;     (tell input-boxes 'dmove-control 15 15)
        (when defaults-list
          (for (i 0 1 (1- (length input-boxes)))
            (set-default-pw-value (nth i input-boxes) (pop defaults-list))))))
    (make-instance patch :view-position (make-point 15 15)
                         :view-size  (make-point w-now (+ 13 y-now))
                         :pw-function pw-function
                         :type-list out-type-list
                         :VIEW-SUBVIEWS input-boxes)))

;;__________________________________________

(defun make-pw-narg-arg-list (count)
  (let ((arg-list))
    (push  '*symbol-argfn-type* arg-list)
    (push  "fn" arg-list)
    (for (i 0 1 (1- count))
      (push  '*symbol-test-type* arg-list)
      (push  (concatenate  'string  "arg" (format nil "~D" (1+ i))) arg-list))
    (nreverse arg-list)))

(defun make-pw-ntest-arg-list (count)
  (let ((arg-list))
    (push  '*symbol-testfn-type* arg-list)
    (push  "testfn" arg-list)
    (push  '*nil-numbox-pw-type* arg-list)
    (push  "input" arg-list)
    (for (i 0 1 (1- count))
      (push  '*nil-numbox-pw-type* arg-list)
      (push  (concatenate  'string  "test" (format nil "~D" (1+ i))) arg-list)
      (push  '*nil-numbox-pw-type* arg-list)
      (push  (concatenate  'string  "val" (format nil "~D" (1+ i))) arg-list))
    (push  '*nil-numbox-pw-type* arg-list)
    (push "else" arg-list)
    (nreverse arg-list)))

;;====================================
;; (defun make-lisp-function-arg-list (function)
;;   (let ((arg-list (get-arglist function))
;;         (res))
;;    (when (car arg-list)
;;      (while arg-list
;;        (push  '*symbol-test-type* res)
;;        (push  (string (pop arg-list)) res)))
;;     (nreverse res)))
;;=============================================

(defun get-lisp-function-arg-list (function)
  (make-lisp-function-arg-list function))


(defvar *lisp-keywords-for-extension* '(&optional &rest &key &allow-other-keys))

(defun make-lisp-function-arg-list (function &optional (nb-arg 0))
  (let ((arg-list (get-arglist function))
        (res ())
        (keyword nil)
        extensible?)
    (when (null (car arg-list)) ; no argument == (())
      (setq arg-list ()))
    (while (and arg-list (not (memq (car arg-list) lambda-list-keywords)))
      (setq res
            (list* (string-downcase (string (pop arg-list))) '*symbol-test-type* res))
      (decf nb-arg))
    (setq extensible? (not (null (intersection arg-list *lisp-keywords-for-extension*))))
    (while (> nb-arg 0)
      (while (and arg-list
                  (not (eql keyword '&rest))
                  (not (eql keyword '&allow-other-keys))
                  (memq (car arg-list) lambda-list-keywords))
        (setq keyword (pop arg-list)))
      (case keyword
        (&rest
         (cond
           ((memq '&key arg-list) (setq keyword nil) (pop arg-list))
           (t (repeat nb-arg (setq res (list* "arg" '*symbol-test-type* res)))
              (setq nb-arg 0))))
        (&allow-other-keys
         (repeat nb-arg
           (setq res (list* "otval" '*symbol-test-type*
                            "otkey" '*symbol-test-type* res)))
         (setq nb-arg 0))
        (t (when (endp arg-list)
             (error "Function ~S cannot provide ~D extra argument~:P.~%"
                    function nb-arg))
         (ccase keyword
           (&optional
            (setq res (list* (string-downcase (string (pop arg-list)))
                             '*symbol-test-type* res))
            (decf nb-arg))
           ((&aux &body &environment &whole) (pop arg-list))
           (&key
            (setq res (list* (string-downcase (string (pop arg-list)))
                             '*symbol-test-type* "key" '*symbol-test-type* res))
            (decf nb-arg))))))
    (values (nreverse res) extensible?)))

;; (get-arglist 'print)
;; (make-lisp-function-arg-list 'print 4)
;; (make-lisp-function-arg-list 'make-list)
;; (make-lisp-function-arg-list 'car)
;; (make-lisp-function-arg-list '+)

(defun make-lisp-pw-boxes (function win)
  (if (not (and (symbolp function) (fboundp function)))
      (format t "~15A~25A" function "no such function !" )
      (if (defunp-function? function)
          (make-functional-pw-boxes function win)
          (multiple-value-bind (args extensible?) (get-lisp-function-arg-list function)
            (add-patch-box win
                           (make-patch-box
                            (cond
                              (extensible?           'C-pw-lispfun)
                              ((= (length args) 2)   'C-pw-resize-x)
                              (t                     'C-patch))
                            function args))))))

;; =============================================================================-======


(defclass C-pw-lispfun (C-pw-extend) ())

(defmethod give-new-extended-title ((self C-pw-lispfun))
  (pw-function self))

(defmethod generate-extended-inputs ((self C-pw-lispfun))
  (make-lisp-function-arg-list (pw-function self) (1+ (length (pw-controls self)))))



;;;; THE END ;;;;
