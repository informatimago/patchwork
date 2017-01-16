;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pw-lisp-functions.lisp
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


(defun scale% (num %scfc)
  "Returns an integer that is scaled as %scfc percentage of num."
  (round (* num %scfc) 100))

(defun mapcar-fun (fun a b)
  "applies fun to a and b
   a and b can be atoms or lists"
  (cond ((and (listp a) (listp b)) (mapcar fun a b))
        ((and (listp a) (atom  b)) (mapcar fun a (cirlist b)))
        ((and (atom a) (listp  b)) (mapcar fun (cirlist a) b))
        (t (funcall fun a b))))

(defun cirlist (elem)
  "makes a circular list out of elem"
  (setq elem (list elem))
  (rplacd elem elem))

(defun cumul-sum (lst)
  "calculates a cumulative sum out of lst starting from 0"
  (let ((res)(sum 0))
     (while   lst (push (setq sum (+ (pop lst) sum)) res))
     (nreverse res)))

(defun interpol (count minv maxv &optional float-fl)
"Returns a list integers or floats interpolated between minv maxv.
The length of the list is determined by count."
  (if (< count 2)
    (list minv)
    (let ((decrsfc (/ (-  maxv minv) (1- count)))
          (tempsum minv)(res))
      (repeat count
        (push (if float-fl tempsum (round tempsum)) res)
        (setq tempsum (+ tempsum decrsfc)))
     (nreverse res))))

(defun break-point-fun (tot-time times valuelst &optional float-fl)
  "makes a breakpointfunction as list of values with length tot-time"
  (let ((res)(timelst)(last-value)(value-now))
    (setq times (mapcar '/  (mapcar '* times (cirlist 100 ))(cirlist (car (last times)))))
    (setq times (mapcar '/  (mapcar '-  (cdr times) times) (cirlist 100)))
    (setq timelst (mapcar 'round
         (mapcar '* (cirlist tot-time)
           (cons (+ (car times) (- 1 (apply '+ times))) (cdr times)))))
    (setq last-value (pop valuelst))
    (while valuelst
       (setq value-now (pop valuelst ))
       (push
         (interpol (pop timelst) last-value (setq last-value value-now) float-fl) res))
     (apply 'append (nreverse res))))

;;(break-point-fun 30 '(0 3 5) '(50 40 50))
;;(break-point-fun 30 '(0 3 5) '(50 40 50) t)

(defun nth-remove (n lst)
  "removes nth (n) member of lst"
  (append (firstn n lst) (nthcdr (1+ n) lst)))

(defun firstn (count lst)
  "Returns first count elements of lst "
  (let ((len (length lst)))
     (if (> count len)
       (subseq lst 0 len)
       (subseq lst 0 count))))

;;(firstn 2 '(0 10 3 2 4 3 5 4 5 ))

(defun scale-low-high (lst low high
                           &optional fix-fl)
"Scales all values in lst beween low high."
  (let* ((max-val (apply #'max lst))
         (min-val (apply #'min lst))
         scfc res)
    (if (= min-val max-val)
       (make-list (length lst) :initial-element low) ; !!!
       (progn
         (setq scfc (/ (- high low) (- max-val min-val)))
         (while lst
            (push (+ low (* scfc (- (pop lst) min-val))) res))
         (if fix-fl
           (mapcar #'round (nreverse res))
           (nreverse res))))))
;;(pw-addmenu *pw-menu-patch* '(scale-low-high))
;;===============================

;;;; THE END ;;;;
