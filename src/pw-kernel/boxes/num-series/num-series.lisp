;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               num-series.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;  
;;;;    Numeric series functions
;;;;  
;;;;AUTHORS
;;;;    Mikael Laurson, Jacques Duthen, Camilo Rueda, Gérard Assayag.
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
(in-package "EPW")


(defunp arithm-ser ((begin fix/float) (step (fix/float (:value 1)))
                    (end (fix/float (:value 10)))) 
    list
    "Returns a list of numbers starting from begin to end with increment step. For 
example:   ? (epw::arithm-ser   0  1  12 ) 
returns:
PW->(0 1 2 3 4 5 6 7 8 9 10 11 12)"
  (let ((l ()))
    (for (i begin step end) (newl l i))
    (nreverse l)))

(defunp fibo-ser ((seed1 fix/float ) (seed2 fix/float (:value 1)) (limit fix>=0 (:value 10))
                  &optional (begin fix/float) (end (fix/float (:value most-positive-fixnum)))) list
    "Returns a list of numbers in the  Fibonacci series ;where the first element is 
seed and the additive factor is seed2. The limit parameter is the limit of this list. 
It is also possible to specify two parameters begin and end which delimit the 
calculation of the series. For example: 
 
\(pw::fibo-ser  0 1 337)
returns

? PW->(0 1 2 3 5 8 13 21 34 55 89 144 233),

 
\(pw::fibo-ser  0 4 337)
returns
? PW->(0 4 8 12 20 32 52 84 136 220)
and 

\(pw::fibo-ser  0 4 337 3  6)
returns
? PW->(12 20 32 52)"
  (labels ((compute-fibo (a b m n)
             (cond ((> m n) (values nil nil))
                   ((= m n) (values a b))
                   (t (compute-fibo b (+ a b) (1+ m) n)))))
    (let (res)
      (multiple-value-bind (ac1 ac2) (compute-fibo seed1 seed2 0 begin)
        (if (< begin 2)
            (if (and ac1 (> limit ac1)) (push ac1 res) )
            (if (> limit ac2) (push ac2 res)))
        (dotimes (k (- end begin) (nreverse res))
          (multiple-value-bind (a b) (compute-fibo ac1 ac2 (+ begin k) (+ begin k 1))
            (setq ac1 a ac2 b) (if (> limit b) (push b  res) (return (nreverse res)))))))))

(defunp geometric-ser ((seed fix/float (:value 1)) (factor (fix/float (:value 2))) (limit fix>=0 (:value 10))
                       &optional(begin fix/float)(end (fix/float (:value most-positive-fixnum))))
    list
    " The geometric-ser module returns a  geometric series ;of numbers in which 
the first element is seed  and the multiplicative coefficient is factor. The limit
parameter is the limit of this list.  It is also possible to specify two parameters 
begin and end which delimit the calculation of the series. For example: 

\(pw::geometric-ser  10  2   2000)
will return
? PW->(10 20 40 80 160 320 640 1280)
and if one sets begin to 2 and end to 5 

\(pw::geometric-ser  10  2   2000  2   5)
one obtains:
? PW->(40 80 160 320)"
  (let ((test (if (< factor 1) #'< #'>)) res)
    (do  ((accum seed (* accum factor)) (index 0 (1+ index)))
         ((funcall test accum limit) (nreverse res))
      (when (> index end) (return (nreverse res)))
      (unless (< index begin) (push accum res)))))

(defunp sinus ((phase fix/float) (nb-osc fix/float (:value 1))
               (nb-samples fix/float (:value 8)) (amp numbers? (:value 1))) list
    "parameters: phase = where we start on the sine curve (xmin)
nb-osc = number of oscillations needed (-> determines xmax)
nb-samples = how many steps on the fragment of curve thus defined
amplitude (ambitus normal -1 / 1)"
  (let* ((xmin (* phase (/ pi 180))) (xmax (+ xmin (* 2 pi nb-osc)))
         (step (/ (- xmax xmin) (1- nb-samples))))
    (g*  amp (sample-fun  'sin xmin step xmax))))

(defunp sample-fun ((fun symbol) (xmin fix/float (:value 1)) 
                    (step fix/float (:value 1)) (xmax fix/float (:value 10))) list
    "Returns the list of values of <fun> from <xmin> to <xmax> with <step>.
For example:
\(pw::sample-fun  'sin  0 1 6.3)
will return
? PW->(0.0 0.8414709848078965    0.9092974268256817     0.1411200080598672 
          -0.7568024953079282   -0.9589242746631385    -0.27941549819892586)
and
\(pw::sample-fun  (pw::make-num-fun '(f(x)= x + 1))  0 1 10)
will return
? PW->(1 2 3 4 5 6 7 8 9 10 11)
"
  (mapcar fun (arithm-ser xmin step xmax)))

(defunp average ((xs list) (weights? list (:value 1))) float
    "average value of <xs>, weighted by linear <weights> or 1."
  (let ((num 0) (den 0) ampl)
    (while xs
      (setq ampl (if (consp weights?) (nextl weights?) 1))
      (incf num (* ampl (nextl xs)))
      (incf den ampl) )
    (/ num den) ))

(defmethod less-tree-mapcar ((fun cl:function) (arg1 number) (arg2 number) &optional deep)
  (funcall fun (list arg1) 
           (if deep arg2 (list arg2))))

(defmethod less-tree-mapcar ((fun cl:function) (arg1 cons) (arg2 number) &optional deep)
  (if (consp (first arg1))
      (cons (less-tree-mapcar fun (car arg1) arg2 deep)
            (less-tree-mapcar fun  (cdr arg1) arg2 deep))
      (funcall fun arg1 (if deep arg2 (list arg2)))))

(defmethod less-tree-mapcar ((fun cl:function) (arg1 null) arg2 &optional deep)
  (declare (ignore arg1 arg2 deep)) nil)

(defmethod less-tree-mapcar ((fun cl:function) (arg1 number) (arg2 cons) &optional deep)
  (if (consp (first arg2))
      (cons (less-tree-mapcar fun arg1 (car arg2) deep)
            (less-tree-mapcar fun  arg1 (cdr arg2) deep))
      (funcall fun (list arg1) (car arg2))))

(defmethod less-tree-mapcar ((fun cl:function) arg1 (arg2 null) &optional deep)
  (declare (ignore arg1 arg2 deep)) nil)

(defmethod less-tree-mapcar ((fun cl:function) (arg1 cons) (arg2 cons) &optional deep)
  (if (or deep (consp (first arg1)) (consp (first arg2)))
      (cons (less-tree-mapcar fun (car arg1) (car arg2) deep)
            (less-tree-mapcar fun  (cdr arg1) (cdr arg2) deep))
      (funcall fun arg1 arg2)))

(defunp g-average ((xs list) (weights? list (:value 1))) midics?
    "average value of <xs>, weighted by linear <weights> or 1. <xs> and 
<weights> may be trees. Trees must be well-formed. That is, the children of a 
node must be either all leaves or all nonleaves. "
  (less-tree-mapcar (function average) xs weights?))

(defun list-min (l) (l-min l))
(defun list-max (l) (l-max l))

(defunp distor-ext1 ((val fix/float) (minin fix/float) (maxin fix/float)
                     (minout fix/float) (maxout fix/float)) number
    "Replaces <val> considered between <minin> and <maxin> by the value proportionaly
placed between <minout> and <maxout>."
  (if (= maxin minin) minout
      (+ minout (/ (* (- val minin) (- maxout minout)) (- maxin minin)))))

(defun distor-ext-less-1 (vals? minin maxin minout maxout)
  ;;; GA 160996 replaced apply by loop due to new PPC compiler behaviour (3.9p1b1)
  (if (= minin most-negative-fixnum) (setq minin (loop for elt in vals? minimize  elt)))
  (if (= maxin most-positive-fixnum) (setq maxin (loop for elt in vals? maximize  elt)))
  (mapcar (lambda (val) (distor-ext1 val minin maxin minout maxout)) vals?))

(defunp distor-ext ((vals? numbers?) (minin fix/float) (maxin fix/float)
                    (minout fix/float) (maxout fix/float)) numbers?
    "Replaces all the <vals?> considered between <minin> and <maxin> by the values proportionaly
placed between <minout> and <maxout>."
  (deep-mapcar 'distor-ext 'distor-ext1 vals? minin maxin minout maxout))

(defunp g-scaling ((vals? numbers?)
                   (minout fix/float) (maxout fix/float)
                   &optional (minin fix/float (:value most-negative-fixnum)) 
                   (maxin fix/float (:value most-positive-fixnum))) numbers?
    "Replaces all the <vals?> considered between the minimum value of the list 
and the maximum value of the list,  by the values proportionally placed between 
<minout> and <maxout>. If the list in question is a part of a larger list, or 
<vals?> is a variable that takes a value within a known interval, one can specify 
the minimum and maximum values by opening two optional windows by double-
clicking on ‘E’ at the right of the module. "
  (setq vals? (list! vals?))
  (less-deep-mapcar 'distor-ext-less-1 vals? minin maxin minout maxout))

(defunp distor ((vals? numbers?) (minout fix/float) (maxout fix/float)) numbers?
    "Replaces all the <vals?> considered between the minimum and maximum of <vals?>
by the values proportionaly placed between <minout> and <maxout>."
  (distor-ext vals? (list-min (list! vals?)) (list-max (list! vals?)) minout maxout))


(defun abc-interpolation (begin end samples curves)
  (mapc (lambda (curve) (when (<= curve 0) (error "non-positive curve:~S~%" curve)))
        curves )
  (if (<= samples 1)
      (list begin)
      (let ((len (1- (min (length begin) (length end))))
            (step (/ 1 (decf samples)))
            (res ())
            (temp ())
            (lcurves (length curves)))
        (for (j 0 1 samples)
          (setq temp ())
          (for (i 0 1 len)
            (newl temp
                  ;;(round
                  (-power-function
                   (nth i begin)
                   (nth i end)
                   (* j step)
                   (nth (mod i lcurves) curves) ) ;1
                  ))
          (newl res (nreverse temp)) )
        (nreverse res))))

;; ;;from Rhythms:Functions [magnus]
;; 
;; (defun -power-function (begin end time curve)
;;   (+ (* (- end begin) (expt time curve)) begin))


(defunp interpolation ((begin list) (end list) (samples fix>0 (:value 4))
                       (curves floats (:value 1)) 
                       &optional (format menu (:menu-box-list (("incl" . 1) ("excl". 2))) )) list
    "Interpolates two lists of the same length. (If the lists are not the same length, the 
operation produces only the number of terms equal to the shorter list.) begin and end, in 
samples steps (i.e., samples is the number of steps). curve is an optional value that 
selects the type of interpolation:

      1  =  straight line, 
    < 1  =  convex
    > 1  =  concave

If format    is  'incl' the two extremes are included in the output. If  format   is 
'excl' they are excluded."
                                        ;(*Esquisse* *Pitch*)
  (let ((int (abc-interpolation (list! begin) (list! end) samples
                                (cond 
                                  ((consp curves) curves)
                                  ((numberp curves) (list curves))
                                  (t (error "bad curves:~S~%" curves)) ))))
    (if (eql format 2) (butlast (rest int)) int)))

(defun dicho-iter (xmin xmax val fun &aux (x (* .5 (+ xmin xmax))) ymin ymax)
  (when (< (setq ymax (funcall fun xmax)) (setq ymin (funcall fun xmin)))
    (psetq xmin xmax xmax xmin ymin ymax ymax ymin))
  (unless (<= ymin val ymax)
    (error "~S is not between f(~S)=~S and f(~S)=~S." val xmin ymin xmax ymax))
  (until (or (= x xmax) (= x xmin))
    (if (< (funcall fun x) val)
        (setq xmin x)
        (setq xmax x))
    (setq x (* .5 (+ xmin xmax))))
  x)

;; ex : resolution de 2 ^ x + 3 ^ x = n
;; (setq fun2 (lambda (x) (+ (expt 2 x) (expt 3 x))))
;; (dicho-iter 0 100 100 fun2)
;; limite possible de la recherche : +-(log most-positive-fixnum) = 19.4

(defunp fun-bin-search ((xmin fix/float) (xmax fix/float (:value 100)) (value fix/float (:value 50))
                        (fun list (:value "(lambda(x)(* 2 x))"))) number
    "binary searches x in the interval [xmin,xmax] , such that fun(x)=value. fun must be either increasing
or decreasing in the interval"
  (dicho-iter xmin xmax value fun))

(defunp inverse ((xmin fix/float) (xmax fix/float (:value 100)) (value fix/float (:value 50))
                 (fun list (:value "(lambda(x)(* 2 x))"))) number
    "binary searches x in the interval [xmin,xmax] , such that fun(x)=value. fun must 
be either increasing
or decreasing in the interval"
  (dicho-iter xmin xmax value (make-fun-object fun)))

(defmethod make-fun-object ((self function)) self)

(defmethod make-fun-object ((self symbol)) (fdefinition self))

(defmethod make-fun-object ((self cons)) (eval `(function ,self)))





(defparameter *prime-numbers*
  #(1 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83
    89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173
    179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263
    269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359
    367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457
    461 463 467 479 487 491 499 503 509 521 523 541 547 557 563 569
    571 577 587 593 599 601 607 613 617 619 631 641 643 647 653 659
    661 673 677 683 691 701 709 719 727 733 739 743 751 757 761 769
    773 787 797 809 811 821 823 827 829 839 853 857 859 863 877 881
    883 887 907 911 919 929 937 941 947 953 967 971 977 983 991 997
    1009 1013 1019 1021 1031 1033 1039 1049 1051 1061 1063 1069 1087
    1091 1093 1097 1103 1109 1117 1123 1129 1151 1153 1163 1171 1181
    1187 1193 1201 1213 1217 1223 1229 1231 1237 1249 1259 1277 1279
    1283 1289 1291 1297 1301 1303 1307 1319 1321 1327 1361 1367 1373
    1381 1399 1409 1423 1427 1429 1433 1439 1447 1451 1453 1459 1471
    1481 1483 1487 1489 1493 1499 1511 1523 1531 1543 1549 1553 1559
    1567 1571 1579 1583 1597 1601 1607 1609 1613 1619 1621 1627 1637
    1657 1663 1667 1669 1693 1697 1699 1709 1721 1723 1733 1741 1747
    1753 1759 1777 1783 1787 1789 1801 1811 1823 1831 1847 1861 1867
    1871 1873 1877 1879 1889 1901 1907 1913 1931 1933 1949 1951 1973
    1979 1987 1993 1997 1999 2003 2011 2017 2027 2029 2039 2053 2063
    2069 2081 2083 2087 2089 2099 2111 2113 2129 2131 2137 2141 2143
    2153 2161 2179 2203 2207 2213 2221 2237 2239 2243 2251 2267 2269
    2273 2281 2287 2293 2297 2309 2311 2333 2339 2341 2347 2351 2357
    2371 2377 2381 2383 2389 2393 2399 2411 2417 2423 2437 2441 2447
    2459 2467 2473 2477 2503 2521 2531 2539 2543 2549 2551 2557 2579
    2591 2593 2609 2617 2621 2633 2647 2657 2659 2663 2671 2677 2683
    2687 2689 2693 2699 2707 2711 2713 2719 2729 2731 2741 2749 2753
    2767 2777 2789 2791 2797 2801 2803 2819 2833 2837 2843 2851 2857
    2861 2879 2887 2897 2903 2909 2917 2927 2939 2953 2957 2963 2969
    2971 2999 3001 3011 3019 3023 3037 3041 3049 3061 3067 3079 3083
    3089 3109 3119 3121 3137 3163 3167 3169 3181 3187 3191 3203 3209
    3217 3221 3229 3251 3253 3257 3259 3271 3299 3301 3307 3313 3319
    3323 3329 3331 3343 3347 3359 3361 3371 3373 3389 3391 3407 3413
    3433 3449 3457 3461 3463 3467 3469 3491 3499 3511 3517 3527 3529
    3533 3539 3541 3547 3557 3559 3571 3581 3583 3593 3607 3613 3617
    3623 3631 3637 3643 3659 3671 3673 3677 3691 3697 3701 3709 3719
    3727 3733 3739 3761 3767 3769 3779 3793 3797 3803 3821 3823 3833
    3847 3851 3853 3863 3877 3881 3889 3907 3911 3917 3919 3923 3929
    3931 3943 3947 3967 3989 4001 4003 4007 4013 4019 4021 4027 4049
    4051 4057 4073 4079 4091 4093 4099 4111 4127 4129 4133 4139 4153
    4157 4159 4177 4201 4211 4217 4219 4229 4231 4241 4243 4253 4259
    4261 4271 4273 4283 4289 4297 4327 4337 4339 4349 4357 4363 4373
    4391 4397 4409 4421 4423 4441 4447 4451 4457 4463 4481 4483 4493
    4507 4513 4517 4519 4523 4547 4549 4561 4567 4583 4591 4597 4603
    4621 4637 4639 4643 4649 4651 4657 4663 4673 4679 4691 4703 4721
    4723 4729 4733 4751 4759 4783 4787 4789 4793 4799 4801 4813 4817
    4831 4861 4871 4877 4889 4903 4909 4919 4931 4933 4937 4943 4951
    4957 4967 4969 4973 4987 4993 4999 5003 5009 5011 5021 5023 5039
    5051 5059 5077 5081 5087 5099 5101 5107 5113 5119 5147 5153 5167
    5171 5179 5189 5197 5209 5227 5231 5233 5237 5261 5273 5279 5281
    5297 5303 5309 5323 5333 5347 5351 5381 5387 5393 5399 5407 5413
    5417 5419 5431 5437 5441 5443 5449 5471 5477 5479 5483 5501 5503
    5507 5519 5521 5527 5531 5557 5563 5569 5573 5581 5591 5623 5639
    5641 5647 5651 5653 5657 5659 5669 5683 5689 5693 5701 5711 5717
    5737 5741 5743 5749 5779 5783 5791 5801 5807 5813 5821 5827 5839
    5843 5849 5851 5857 5861 5867 5869 5879 5881 5897 5903 5923 5927
    5939 5953 5981 5987 6007 6011 6029 6037 6043 6047 6053 6067 6073
    6079 6089 6091 6101 6113 6121 6131 6133 6143 6151 6163 6173 6197
    6199 6203 6211 6217 6221 6229 6247 6257 6263 6269 6271 6277 6287
    6299 6301 6311 6317 6323 6329 6337 6343 6353 6359 6361 6367 6373
    6379 6389 6397 6421 6427 6449 6451 6469 6473 6481 6491 6521 6529
    6547 6551 6553 6563 6569 6571 6577 6581 6599 6607 6619 6637 6653
    6659 6661 6673 6679 6689 6691 6701 6703 6709 6719 6733 6737 6761
    6763 6779 6781 6791 6793 6803 6823 6827 6829 6833 6841 6857 6863
    6869 6871 6883 6899 6907 6911 6917 6947 6949 6959 6961 6967 6971
    6977 6983 6991 6997 7001 7013 7019 7027 7039 7043 7057 7069 7079
    7103 7109 7121 7127 7129 7151 7159 7177 7187 7193 7207 7211 7213
    7219 7229 7237 7243 7247 7253 7283 7297 7307 7309 7321 7331 7333
    7349 7351 7369 7393 7411 7417 7433 7451 7457 7459 7477 7481 7487
    7489 7499 7507 7517 7523 7529 7537 7541 7547 7549 7559 7561 7573
    7577 7583 7589 7591 7603 7607 7621 7639 7643 7649 7669 7673 7681
    7687 7691 7699 7703 7717 7723 7727 7741 7753 7757 7759 7789 7793
    7817 7823 7829 7841 7853 7867 7873 7877 7879 7883 7901 7907 7919
    7927 7933 7937 7949 7951 7963 7993 8009 8011 8017 8039 8053 8059
    8069 8081 8087 8089 8093 8101 8111 8117 8123 8147 8161 8167 8171
    8179 8191 8209 8219 8221 8231 8233 8237 8243 8263 8269 8273 8287
    8291 8293 8297 8311 8317 8329 8353 8363 8369 8377 8387 8389 8419
    8423 8429 8431 8443 8447 8461 8467 8501 8513 8521 8527 8537 8539
    8543 8563 8573 8581 8597 8599 8609 8623 8627 8629 8641 8647 8663
    8669 8677 8681 8689 8693 8699 8707 8713 8719 8731 8737 8741 8747
    8753 8761 8779 8783 8803 8807 8819 8821 8831 8837 8839 8849 8861
    8863 8867 8887 8893 8923 8929 8933 8941 8951 8963 8969 8971 8999
    9001 9007 9011 9013 9029 9041 9043 9049 9059 9067 9091 9103 9109
    9127 9133 9137 9151 9157 9161 9173 9181 9187 9199 9203 9209 9221
    9227 9239 9241 9257 9277 9281 9283 9293 9311 9319 9323 9337 9341
    9343 9349 9371 9377 9391 9397 9403 9413 9419 9421 9431 9433 9437
    9439 9461 9463 9467 9473 9479 9491 9497 9511 9521 9533 9539 9547
    9551 9587 9601 9613 9619 9623 9629 9631 9643 9649 9661 9677 9679
    9689 9697 9719 9721 9733 9739 9743 9749 9767 9769 9781 9787 9791
    9803 9811 9817 9829 9833 9839 9851 9857 9859 9871 9883 9887 9901
    9907 9923 9929 9931 9941 9949 9967 9973))




(defun test-prime (n primes)
  (or (member n primes)
      (dolist (p primes)
        (when (> p (sqrt n)) (return-from test-prime t))
        (when (zerop (mod n p))  (return-from test-prime nil)))))


(defun gen-prime (pmax)
  (let ((prime-list (list 2 )) (last))
    (setf last (last prime-list))
    (do ((i (1+ (first last)) (1+ i)))
        ((> i pmax) prime-list)
      (when (test-prime i prime-list)
        (setf (cdr last) (cons i nil)
              last (cdr last))))
    (coerce (cons 1 prime-list) 'vector)))


(defun prime-facts (x)
  (let ((ip 1) (r) (n 0))
    (while (and (> x 1) 
                (<= (* (aref *prime-numbers* ip) 
                       (aref *prime-numbers* ip))
                    x))
      (when (= 0 (mod x (aref *prime-numbers* ip)))
        (setq n 1)
        (while (= 0 
                  (progn (setq x (/ x (aref *prime-numbers* ip))) 
                         (mod x (aref *prime-numbers* ip))))
          (incf n))
        (push  (list (aref *prime-numbers* ip) n) r))
      (incf ip))
    (when (/= x 1)   (push  (list x 1) r))
    (or (reverse r) (list (list 1 1)))))


(defunp prime-ser ((max fix (:value 1000))) 
    list
    "Returns the set of prime-numbers ranging from 0 upto max" 
  (coerce (gen-prime max) 'list))

(defunp prime-factors ((number fix (:value 1000))) 
    list
    "Returns the prime decomposition of  <number> in the form (... (prime exponent) ...)
Primes known to the system are the 1230 primes ranging from 1 to 9973. They are
in the global variable *prime-numbers*. You can change This variable by typing
\(setf *prime-numbers* (epw::gen-prime <max>))
where <max is an upper bound."
  (prime-facts number))



(defunp prime? ((n fix (:value 0))) bool
    "Tests if n is a prime number. n must be smaller than 99460729."
  (and (test-prime n (coerce  *prime-numbers* 'list)) t))


;;;; THE END ;;;;
