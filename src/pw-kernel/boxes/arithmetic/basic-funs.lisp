;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               basic-funs.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;
;;;;    PW Arithmetic modules
;;;;
;;;;AUTHORS
;;;;    Mikael Laurson, Jacques Duthen, Camilo Rueda.
;;;;    several modules by Tristan Murail.
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-07 <PJB> Changed license to GPL3; Added this header.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;
;;;;    Copyright IRCAM 1986 - 2014
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
(in-package "EPW")

(defun carlist! (thing)
  "Returns an atom if thing is an atom or a one-element list,
   otherwise returns the list unchanged "
  (if (and (consp thing) (= (length thing) 1)) (car! thing) thing))

(defun car-mapcar (fun list?  &rest args)
  "Mapcars <fun> if list? is a list or applies <fun> if it is an atom or
a one-element list"
  (cond  ((atom list?) (apply fun list? args))
         ((= (length list?) 1) (apply fun (car list?) args))
         (t (mapcar (lambda (x) (apply fun x  args ))  list? ))))

;; (defun deep-mapcar (fun fun1 list? &rest args)
;;   "Mapcars <fun> or applies <fun1> to <list?> <args> whether <list?> is a list or not."
;;   (cond
;;     ((null list?) ())
;;     ((not (consp list?)) (apply fun1 list? args))
;;     (t (cons (apply #'deep-mapcar fun fun1 (car list?) args)
;;              (apply #'deep-mapcar fun fun1 (cdr list?) args)))))
;;
;; (defun double-mapcar (fun1 list1? list2? &rest args)
;;   "Mapcars <fun> or applies <fun1> to <list1?> <list2?> <args>
;; whether each of <list1?> <list2?> is a list or not."
;;   (cond
;;     ((consp list1?)
;;      (if (consp list2?)
;;                                         ;(error "cannot double-mapcar 2 lists: ~S and ~S~%." list1? list2?) ;
;;          (mapcar (lambda (x1 x2) (apply fun1 x1 x2 args))
;;                  list1? list2?)
;;          (mapcar (lambda (x) (apply fun1 x list2? args))
;;                  list1?)))
;;     ((consp list2?)
;;      (mapcar (lambda (x) (apply fun1 list1? x args))
;;              list2?))
;;     (t (apply fun1 list1? list2? args))))
;;
;;
;; (defmethod arith-tree-mapcar ((fun cl:function) (arg1 number) (arg2 number))
;;   (funcall fun arg1 arg2))
;;
;; (defmethod arith-tree-mapcar ((fun cl:function) (arg1 cons) (arg2 number))
;;   (cons (arith-tree-mapcar fun (car arg1) arg2)
;;         (arith-tree-mapcar fun  (cdr arg1) arg2)))
;;
;; (defmethod arith-tree-mapcar ((fun cl:function) (arg1 null) arg2)
;;   (declare (ignore arg1 arg2)) nil)
;;
;; (defmethod arith-tree-mapcar ((fun cl:function) (arg1 number) (arg2 cons))
;;   (cons (arith-tree-mapcar fun arg1 (car arg2))
;;         (arith-tree-mapcar fun  arg1 (cdr arg2))))
;;
;; (defmethod arith-tree-mapcar ((fun cl:function) arg1 (arg2 null))
;;   (declare (ignore arg1 arg2)) nil)
;;
;; (defmethod arith-tree-mapcar ((fun cl:function) (arg1 cons) (arg2 cons))
;;   (cons (arith-tree-mapcar fun (car arg1) (car arg2))
;;         (arith-tree-mapcar fun  (cdr arg1) (cdr arg2))))


;;optimized (tail recursion) [Camilo 931004]
(defmethod arith-tree-mapcar ((fun cl:function) (arg1 number) (arg2 number) &optional accumulator)
  (if accumulator (reverse (cons (funcall fun arg1 arg2) accumulator)) (funcall fun arg1 arg2)))

(defmethod arith-tree-mapcar ((fun cl:function) (arg1 cons) (arg2 number) &optional accumulator)
  (arith-tree-mapcar fun (cdr arg1) arg2 (cons (arith-tree-mapcar fun (car arg1) arg2) accumulator)))

(defmethod arith-tree-mapcar ((fun cl:function) (arg1 null) arg2 &optional accumulator)
  (declare (ignore arg1 arg2)) (reverse accumulator))

(defmethod arith-tree-mapcar ((fun cl:function) (arg1 number) (arg2 cons) &optional accumulator)
  (arith-tree-mapcar fun arg1 (cdr arg2) (cons (arith-tree-mapcar fun arg1 (car arg2)) accumulator)))

(defmethod arith-tree-mapcar ((fun cl:function) arg1 (arg2 null) &optional accumulator)
  (declare (ignore arg1 arg2 )) (reverse accumulator))

(defmethod arith-tree-mapcar ((fun cl:function) (arg1 cons) (arg2 cons) &optional accumulator)
  (arith-tree-mapcar fun (cdr arg1) (cdr arg2)
                     (cons (arith-tree-mapcar fun (car arg1) (car arg2)) accumulator)))

(defun less-deep-mapcar (fun  list? &rest args)
  "Applies <fun> to <list?> <args> if <list?> is a one-level list .
   Mapcars <fun> to <list?> <args> if <list?> is a multi-level list. "
  (cond
    ((null list?) ())
    ((atom (car list?)) (apply fun list? args))
    ((atom (car (car list?)))
     (cons (apply fun (car list?)  args ) (apply #'less-deep-mapcar fun (cdr list?) args)))
    (t (cons (apply #'less-deep-mapcar fun  (car list?) args)
             (apply #'less-deep-mapcar fun  (cdr list?) args)))))

(defun one-elem (item)
  (or (atom item) (= (length item) 1)))

;; ==== extended arithmetic ====

(defunp L+ ((l1? numbers?) (l2? numbers?)) numbers?
    "Sum of two of numbers or lists."
  (double-mapcar '+ l1? l2?))

(defunp L- ((l1? numbers?) (l2? numbers?)) numbers?
    "Difference of two of numbers or lists."
  (double-mapcar '- l1? l2?))

(defunp L* ((l1? numbers?) (l2? numbers?)) numbers?
    "Product of two of numbers or lists."
  (double-mapcar '* l1? l2?))

(defunp L/ ((l1? numbers?) (l2? (numbers? (:value 1)))) numbers?
    "Truncation of two of numbers or lists."
  (double-mapcar '/ l1? l2?))

(defunp g+ ((l1? numbers?) (l2? numbers?)) numbers?
    "Sum of two of numbers or trees."
  (arith-tree-mapcar (function +) l1? l2?))

(defunp g- ((l1? numbers?) (l2? numbers?)) numbers?
    "Difference of two  numbers or trees."
  (arith-tree-mapcar (function -) l1? l2?))

(defunp g* ((l1? numbers?) (l2? numbers?)) numbers?
    "product of two  numbers or trees."
  (arith-tree-mapcar (function *) l1? l2?))

(defunp g/ ((l1? numbers?) (l2? numbers? (:value 1))) numbers?
    "quotient of two  numbers or trees."
  (arith-tree-mapcar (function /) l1? l2?))

(defunp g-power ((l1? numbers?) (power numbers? (:value 1))) numbers?
    "l1? to the power of l2?. "
  (arith-tree-mapcar (function expt) l1? power))

(defunp g-exp ((l? numbers?)) numbers?
    "Exponential of a number or a trees. "
  (deep-mapcar/1 'exp l?))

(defunp g-log ((l? numbers?)) numbers?
    "Calculates the  natural  logarithm of a number or a tree."
  (deep-mapcar/1 'log l?))

(defun approx-decimals (x y nbdec)
  (let ((ndec
          (if (> nbdec 0 ) (float (expt 10 nbdec)) (expt 10 nbdec))))
    (/ (round (/ x y) (/ ndec)) ndec)))

(defunp g-round ((l1? numbers?)
                 &optional (decimals fix)
                 (l2? numbers? (:value 1))) numbers?
    "Rounds a number or tree. This module allows many operations, since it is
extendible. (See the letter E on the module.) The input decimals sets the choice
of number of decimal places to round to. I2? specifies division of this rounded
number by a second before  rounding;."
  (arith-tree-mapcar (lambda (x y) (approx-decimals x y decimals)) l1? l2?))

(defunp g-div ((l1? numbers?) (l2? numbers? (:value 1))) numbers?
    "Integer division of two numbers or trees, Euclidean division. "
  (arith-tree-mapcar (function floor) l1? l2?))

(defunp g-ceiling ((l1? numbers?)) numbers?
    "quotient of two numbers or trees. Rounded to the smaller integer. "
  (deep-mapcar/1 'ceiling l1?))

(defunp g-floor ((l1? numbers?)) numbers?
    "Truncation of number or tree. Rounded to the larger integer. "
  (deep-mapcar/1 'floor l1?))

(defunp g-mod ((l1? numbers?) (mod numbers? (:value 1))) numbers?
    "Calculate the number that is congruent modulo l2? to l1?, or the remainder
of an integer division (Euclidean division between two numbers l1? and l2?).
"
  (arith-tree-mapcar (function mod) l1? mod))

(defunp g-abs ((l? numbers?)) numbers?
    "Absolute value of a number or a tree. "
  (deep-mapcar/1 'abs l?))

;; (defun deep-mapcar/1 (fun list? &rest args)
;;   "Mapcars <fun>  to <list?> <args> whether <list?> is a list or not."
;;   (cond
;;     ((null list?) ())
;;     ((not (consp list?)) (apply fun list? args))
;;     (t (cons (apply #'deep-mapcar/1 fun  (car list?) args)
;;              (apply #'deep-mapcar/1 fun  (cdr list?) args)))))

;;;optimized (tail recursion) [Camilo 931004]
(defun deep-mapcar/1 (fun list? &rest args)
  (labels ((map-structure (str accum)
             (cond ((null str) (reverse accum))
                   ((not (consp str))
                    (if accum (reverse (cons (apply fun str args) accum)) (apply fun str args)))
                   (t (map-structure (cdr str) (cons (map-structure (car str) ()) accum))))))
    (map-structure list? nil)))

(defunp LL/round ((l1? numbers?) (div (fix/float (:value 1)))) numbers?
    "Rounding of two of numbers or lists."
  (deep-mapcar/1 'round l1? div))

(defunp LL/floor ((l1? numbers?) (div (fix/float (:value 1)))) numbers?
    "Truncation of two of numbers or lists."
  (deep-mapcar/1 'floor l1? div))

(defunp LL/mod ((l1? numbers?) (div (fix/float (:value 1)))) numbers?
    "Modulo of two of numbers or lists."
  (deep-mapcar/1 'mod l1? div))

(defunp LL-log ((l? (numbers? (:value 1)))) numbers?
    "Logarithm of a number or a list."
  (deep-mapcar/1 'log l?))

(defunp L-exp ((l? numbers?)) numbers?
    "Exponential of a number or a list."
  (deep-mapcar/1 'exp l?))

(defunp L-power ((l1? numbers?) (l2? numbers?)) numbers?
    "Exponential of two of numbers or lists."
  (double-mapcar 'expt l1? l2?))

(defunp LL-abs ((l? numbers?)) numbers?
    "Absolute value of a number or a list."
  (deep-mapcar/1 'abs l?))

(defunp L-float ((l? numbers?)) numbers?
    "Converts to float a number or a list."
  (deep-mapcar 'L-float 'float l?))

(defunp l-scale% ((l1? numbers?) (l2? numbers?)) numbers?
    "Divides by 100 the product of <l1?> and <l2?>."
  (double-mapcar 'pw::scale% l1? l2?))

(defun mulalea (list percent)
  (* list (+ 1 (/ (pw::random2 (- percent) (float percent)) 100))))

(defune LLalea ((list numbers?) (percent float)) numbers?
    "Ajoute aléa à la liste (de profondeur quelconque) selon % indiqué"
  (deep-mapcar/1  'mulalea list percent))

(defunp g-alea ((list numbers?) (percent numbers?)) numbers?
    "Add a uniform random function to the list <list> of some depth according to a
percentage <percent> indicated. "
  (arith-tree-mapcar (function mulalea) list percent))

;; ==== Pitch modifications ====

(defunp L-abs-f ((l? freqs?)) freqs?
    "Absolute value of a freq or a list."
  (deep-mapcar 'L-abs-f 'abs-f1 l?))

(defunp x->dx ((xs list)) list
    "Returns the list of the intervals between the contiguous values of a list <xs>.
For example
\(pw::x->dx ‘(0 4 5 9 6 2 3 3))
 will return     ? PW->(4 1 4 -3 -4 1 0)  "
  (mapcar #'- (cdr xs) xs))

(defunp dx->x ((start fix/float) (dxs fixs)) list
    "Constructs a list of numbers from <start> with the consecutives intervals  of
<dxs>.
For example

\(pw::dx->x  0  ‘(0 4 5 9 6 2 3 3))
will return  ? PW->(0 0 4 9 18 24 26 29 32)

and
\(pw::dx->x  8  ‘(0 4 5 9 6 2 3 3))
will return  ? PW->(8 8 12 17 26 32 34 37 40)
"
  (let ((x start))
    (cons x (mapcar (lambda (dx) (incf x dx)) dxs))))

(defunp LLdecimals ((list numbers? (:value 1.6)) (nbdec fix)) numbers?
    "Arrondit liste de profondeur quelconque avec <nbdec> décimales"
  (let ((ndec
          (if (> nbdec 0 ) (float (expt 10 nbdec)) (expt 10 nbdec))))
    (deep-mapcar/1 '/
                   (deep-mapcar/1 'round list (/ 1 ndec)) ndec )))

(defunp random2 ((low fix/float) (high fix/float)) number
    "Returns a random value between low high inclusive"
  (if (zerop (- high low))
      (+ high low (- low))
      (let ((low-min (min low high)))
        (if (or (floatp  low) (floatp high))
            (+ (random (- (max low high) low-min)) low-min)
            (+ (random (- (1+ (max low high)) low-min)) low-min)))))

(defunp g-random ((low numbers?) (high numbers?)) numbers?
    "Returns a random value between low and high inclusive. Low and high
can be trees. "
  (arith-tree-mapcar (function random2) low high))

(defunp LL-oper ((list numbers?) (arg fix/float) (fun symbol (:value "+"))) numbers?
    "applique fun a arg et list (profondeur quelconque)."
  (deep-mapcar/1  fun list arg))

(in-package "EPW")
(defunp g-oper ((fun symbol (:value "+")) (obj1? list (:value 0 :type-list ()))
                &optional (obj2? list (:value '() :type-list ()))) nil
    "Applies fun to leaves of trees of obj1? and (optionally) obj2?.
fun may be a Lisp function (list, +, *, cons, etc.)
or a function object created by the  make-num-fun box.

For example:
\(pw::g-oper   '+  4   5)       will return  ? PW->9  ,
\(pw::g-oper 'list  4 5) will return    ? PW->(4 5)   ,

\(pw::g-oper '+  '(1 2) '( 3 4))  will return   ? PW->(4 6)   and

\(pw::g-oper (pw::make-num-fun '(f(x y)= x * 2 + y * 3))  '(1 2) '( 3 4))
will return     ?
 PW->(11 16)"
  (if obj2?
      (arith-tree-mapcar (if (functionp fun) fun (fdefinition fun)) obj1? obj2?)
      (deep-mapcar/1  fun obj1?)))

;; (defunp matrix-oper ((l1? numbers?) (l2? numbers?) (fun symbol (:value "+"))) numbers?
;;     "applies fun to elements of l1? and l2? considered as matrices"
;;   (mapcar (lambda (elem) (double-mapcar fun elem l2?)) (list! l1?)))

(defmethod tree-product ((fun cl:function) (tree1 number) (tree2 number))
  (funcall fun tree1 tree2))

(defmethod tree-product ((fun cl:function) (tree1 null) tree2) (declare (ignore tree1 tree2)) nil)

(defmethod tree-product ((fun cl:function) tree1 (tree2 null)) (declare (ignore tree1 tree2)) nil)

(defmethod tree-product ((fun cl:function) (tree1 cons) (tree2 number))
  (mapcar (lambda (x) (tree-product fun x tree2)) tree1))

(defmethod tree-product ((fun cl:function) (tree1 number) (tree2 cons))
  (mapcar (lambda (x) (tree-product fun tree1 x)) tree2))

(defmethod tree-product ((fun cl:function) (tree1 cons) (tree2 cons))
  (mapcar (lambda (x) (mapcar (lambda (y) (tree-product fun x y)) tree2)) tree1))

;;to be eliminated
(defunp matrix-oper ((l1? numbers?) (l2? numbers?) (fun symbol (:value "+"))) numbers?
    "applies fun to elements of l1? and l2? considered as matrices"
  (mapcar (lambda (x) (mapcar (lambda (y) (funcall fun x y)) (list! l2?))) (list! l1?)))

(defunp cartesian ((l1? numbers?) (l2? numbers?) (fun symbol (:value "+"))) numbers?
    "Applies the function fun to elements
of l1? and l2? considered as matrices.
Like  g-oper ;fun may be a Lisp function
\(list, +, *, cons, etc.)  or a function object
created by  the  make-num-fun ;box .
The result is a cartesian product of l1? by l2?.

\(epw::cartesian 5 5 '+)
will return ? PW->((10)) ,
 will return
? PW->((6 7 8 9) (7 8 9 10) (8 9 10 11) (9 10 11 12)) and
\(epw::cartesian '(1 2 3 4) '(5 6 7 8)  '+) will return
? PW->  (((1 5) (1 6) (1 7) (1 8)) ((2 5) (2 6) (2 7) (2 8)) ((3 5) (3 6) (3 7) (3 8))
 ((4 5) (4 6) (4 7) (4 8)))

"
  (mapcar (lambda (x) (mapcar (lambda (y) (funcall fun x y)) (list! l2?))) (list! l1?)))

(defunp pw::pgcd ((list list) (approx fix/float (:value 1))) float "fix/float gcd"
  (labels ((min-gcd? (x y) (<= (- y x) approx));;(<= (- 1 (/ x y)) approx))
           (enough-approx? (diff) (every (lambda (x) (min-gcd? (apply 'min diff) x)) diff))
           (loop-gcd (list)
             (let* ((min (apply 'min list))
                    (diff (g- (substitute (+ min min) min list :test #'min-gcd?) min)))
               (if (enough-approx? diff) (apply 'min diff) ;;(* (round (/ (apply 'min diff) approx)) approx)
                   (loop-gcd diff)))))
    (loop-gcd list)))


;;;; THE END ;;;;
