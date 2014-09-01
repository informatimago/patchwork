;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               num-fun-gen.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;    
;;;;    XXX
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;    Mikael Laurson
;;;;    Jacques Duthen
;;;;    Camilo Rueda
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

;;;
;;;Numeric function generation
;;;

(in-package "EPW")
(pw:enable-patchwork-reader-macros)

;; (eval-when (eval compile load)
;;   (import '(|CLPF-UTIL|:make-num-lambda |CLPF-UTIL|:prefix-expr
;;             |CLPF-UTIL|:prefix-help)))



(defunp make-num-fun ((fexpr list (:value "(f(x)= x + 1)"))) nil
    "Creates a lisp function object from the \"functional\" expr <fexpr> which is
basically an infixed expression (see prefix-expr and prefix-help).
When <fexpr> begins with something like (f(x)= ...), the formal arguments are 
taken from the given list, otherwise they are deduced from the body of <fexpr> 
and collected in the order they appear in it. Local variables are automatically 
handled. The resulting function is compiled when the value of *compile-num-
lambda* is T (default). <make-num-fun> has two different possible syntaxes.  
One being the standard Lisp syntax, i.e., (f(x) = (- (* x x) x))); the other being 
format like the C language, i.e., (f(x)= (x * x) - x). Note that the only difference 
between this notation and standard C notation is that spaces  must be put 
between operators. The variable name definition at the beginning of the 
function (f(x)= ...) is optional. If it is not included by the user, the program 
figures out which variables are involved."
  ;; fexpr == <expr> || (<fun> <args> = . <expr>)
  (multiple-value-bind (lambda name) (make-num-lambda fexpr)
    (cond
      (*compile-num-lambda*
       (compile name lambda))
      (name
       (eval `(defun ,name ,@(rest lambda))))
      (t
       (coerce lambda 'function)))))


(defunp lagrange ((l-x-y list)) ()
    "Returns a Lagrange polynomial defined by the points of list <l-x-y>."
  (let ((length (length l-x-y)) index cp)
    (unless (evenp length)
      (error "You must give as many ys as xs in ~S." l-x-y))
    (unless (every #'numberp l-x-y)
      (error "l-x-y must contain only numbers ~S." l-x-y))
    (let* ((length (/ length 2))
           (vx (make-array length))
           (vy (make-array length))
           (Aitken (make-array length)))
      (setq length (1- length) index -1)
      (while l-x-y
        (incf index)
        (setf (aref vx index) (nextl l-x-y))
        (setf (aref Aitken index) (car l-x-y))
        (setf (aref vy index) (nextl l-x-y)))
      (for (j 1 1 length)
        (setq cp (aref Aitken (1- j)))
        (for (i j 1 length)
          (psetf (aref Aitken i) (/ (- (aref Aitken i) cp) (- (aref vx i) (aref vx (- i j))))
                 cp (aref Aitken i))))
      ;;(compile ()
      (eval `(function
              (lambda (x)
               (let* ((length ,length) (vx ',vx) (Aitken ',Aitken) (z (aref Aitken length)))
                 (for (i (1- length) -1 0)
                   (setq z (+ (aref Aitken i) (* z (- x (aref vx i))))))
                 z)))))))

(defunp lagrange-fun ((l-x-y list)) ()
    "Retourne un polynome de Lagrange défini par les points de liste <l-x-y>."
  (lagrange l-x-y))

(defunp linear ((x0 float) (y0 float)
                (x1 float (:value 1)) (y1 float (:value 1))
                (sym list (:value "fline")) ) ()
    "calcule les paramètres de l'équation  y=ax+b en fct de deux points 
\(x0,y0) (x1,y1)."
  (let* ((a (/ (- y1 y0) (- x1 x0)))
         (b (- y1 (* x1 a))))
    (if (not (eql sym ())) (format t "y = ~S x + ~S ~%" (lldecimals a 6) (lldecimals b 6) ))
    (set (if (null sym) 'fline sym)
         ;;(compile ()
         (eval `(function
                 (lambda (x) (+ ,b (* x ,a)))) ))                      
    ))

(defvar *weird-symb* nil)

(defunp linear-fun ((x0 float) (y0 float)
                    (x1 float (:value 1)) (y1 float (:value 1))
                    &optional (print menu (:menu-box-list (("yes" . 1) ("no". 2))
                                           :type-list (no-connection)))
                    ) ()
    "Calculate the parameters of the equation y = a x + b as a function 
of the two points (x0,y0) (x1,y1). The optional parameter print  
lets one print the function. "
  (linear x0 y0 x1 y1 (if (= print 1) '*weird-symb* nil)))

(defunp power/2 ((x0 float (:value 1)) (y0 float (:value 1))
                 (x1 float (:value 2)) (y1 float (:value 4))
                 (sym list (:value "fpuiss2")) ) ()
    "calcule les paramètres de l'équation  y=ax^b en fct de deux points 
\(x0,y0) (x1,y1)."
  (if (zerop (* x1 x0 y0 y1))
      (progn (print "values of x and y must be different from zero") (pw::ed-beep))
      (let* ((b (/ (log (/ y1 y0)) (log (/ x1 x0))))
             (a (/ y1 (expt x1 b))))
        (if (not (eql sym ())) (format t "y = ~S x ** ~S ~%" (lldecimals a 10) (lldecimals b 10) ))
        (set  (if (null sym) 'fpuiss2 sym) 
              ;;(compile ()
              (eval `(function
                      (lambda (x) (* ,a (expt x ,b)))) ))                      
        )))

;;Taken from (c) Copyright Gerald Roylance 1982
(defun false-position-search (fcn s1 s2 eps)
  (do ((x1 s1)  (y1 (funcall fcn s1))
       (x2 s2)  (y2 (funcall fcn s2))
       (xn 0.0) (yn 0.0))
      (NIL)
    (declare (float x1 x2 y1 y2 xn yn))
    (if (= y1 y2) (return nil)) ;;(error "FALSE-POSITION-SEARCH Lost"))
    (setq xn (- x1 (* y1 (/ (- x2 x1) (- y2 y1)))))
    (setq yn (funcall fcn xn))
    (cond ((< (abs yn) eps) (return xn)))
    (cond ((> (abs y1) (abs y2))
           (setq x1 xn) (setq y1 yn))
          (t
           (setq x2 xn) (setq y2 yn)))))

(pw::defunp power/3 ((x0 float) (y0 float)
                     (x1 float (:value 2)) (y1 float (:value 4))
                     (x2 float (:value 3)) (y2 float (:value 9))
                     (bmin float)(bmax float (:value 19))  ;;to be eliminated!!!!
                     (sym list (:value "fpuiss3")) ) ()
    "calcule les paramètres de l'équation  y=ax^b+c en fct de trois points 
\(x0,y0) (x1,y1) (x2,y2) et crée la fonction correspondante .
La fct doit être continûment croissante ou décroissante.
bmin et bmax fixent les limites de la recherche du paramètre b par la 
méthode dichotomique. Prendre un intervalle négatif pour une fct décroissante, 
positif autrement. 19 ( ou -19) sont les valeurs maximales
sym est une variable qui contient le nom de la fonction créée.
Si sym = nil, la fct n'est pas affichée dans le listener"
  (declare (ignore bmin bmax))
  (setq x0 (float x0) x1 (float x1) x2 (float x2)
        y0 (float y0) y1 (float y1) y2 (float y2))
  (let* (a b c  (y/y #i((y2 - y1)/(y1 - y0))) res power
          (growing (or (and (> x2 x1) (> y2 y1)) (and (< x2 x1) (< y2 y1))))
          (bmin-min (if growing 1.0 -19))  ;0.1))
          (incr (if growing 1 1))  ;0.1))
          (bmax (if growing 19 -1))) ;;1.0)))
    (setq b
          (do ((bmin bmin-min (+ bmin incr))) 
              ((>= bmin bmax) (and res (apply 'max res)))
            (setq power
                  (false-position-search  
                   (lambda (b) #i(y/y * (x1 ** b - x0 ** b) - (x2 ** b - x1 ** b)))
                   bmin bmax 0.001))
            (if power (push power res))))
    (unless b 
      (pw::ed-beep) 
      (error "sorry... couldn't find an interpolation with this values")) 
    (setq a #i((y1 - y0)/(x1 ** b - x0 ** b)))
    (setq c #i(y0 - a x0 ** b))
    (if (not (eql sym ())) (format t "y = ~S x ** ~S + ~S  ~%" 
                                  (lldecimals a 10) (lldecimals b 10) (lldecimals c 10)))
    (set  (if (null sym) 'fpuiss3 sym)
          ;;(compile ()
          (eval `(function
                  (lambda (x) (+ ,c (* ,a (expt x ,b)))) ) ))                   
    ))


(defunp power-fun ((x0 float (:value 1)) (y0 float (:value 1))
                   (x1 float (:value 2)) (y1 float (:value 4))
                   &optional
                   (x2 list (:value '() :type-list (float fixnum)))
                   (y2 float (:value 9))
                   (print menu (:menu-box-list (("yes" . 1) ("no". 2))
                                :type-list (no-connection)))) ()
    "Calculate the parameters of the equation y = a xb + c  or y = a xb   
as a function of the points (x0,y0)  (x1,y1) and (optional) (x2,y2) 
and create the corresponding function, either y = axb 
\(for two pairs of points) or  y = a xb + c   (for three pairs of points).  "
  (if x2 
      (power/3 x0 y0 x1 y1 x2 y2 () () (if (= print 1) '*weird-symb* nil))   ;;;note bmin bmax arguments are to be eliminated
      (power/2 x0 y0 x1 y1 (if (= print 1) '*weird-symb* nil))))

;; (defunp power-fun ((x0 float (:value 1)) (y0 float (:value 1))
;;                    (x1 float (:value 2)) (y1 float (:value 4))
;;                    &optional
;;                    (x2 list (:value '() :type-list (float fixnum)))
;;                    (y2 float (:value 9))) ()
;;     "Calculate the parameters of the equation y = a xb + c  or y = a xb   
;; as a function of the points (x0,y0)  (x1,y1) and (optional) (x2,y2) 
;; and create the corresponding function, either y = axb 
;; (for two pairs of points) or  y = a xb + c   (for three pairs of points).  "
;;   (if x2 
;;       (power/3 x0 y0 x1 y1 x2 y2 () () '*weird-symb*)   ;;;note bmin bmax arguments are to be eliminated
;;       (power/2 x0 y0 x1 y1 '*weird-symb*)))

;;changed by aaa 28-08-95 from pw-modif
(defunp parabole/2 ((x0 float) (y0 float)
                    (x1 float (:value 2)) (y1 float (:value 12))
                    (sym list (:value "fparab2")) ) ()
    "calcule les paramètres de l'équation  y = ax^2 + b  en fct de deux points 
\(x0,y0) (x1,y1)"
  
  (let* ((a #i((y1 - y0) / (x1 * x1 - x0 * x0)))
         (b #i(y0 - a * x0 * x0)))
    (if (not (eql sym ())) (format t "y = ~S x 2 + ~S ~%"  (lldecimals a 6) (lldecimals b 6)))
    (set (if (null sym) 'fparab2 sym) 
         ;;(compile ()
         (eval `(function
                 (lambda (x) (+  (* ,a x x ) ,b )))))                      
    ))

;; (defunp parabole/2 ((x0 float) (y0 float)
;;                     (x1 float (:value 2)) (y1 float (:value 12))
;;                     (sym list (:value "fparab2")) ) ()
;;     "calcule les paramètres de l'équation  y = ax^2 + b  en fct de deux points 
;; (x0,y0) (x1,y1)"
;; 
;;   (let* ((a #i((y1 - y0) / (x1 * x1 - x0 * x0)))
;;          (b #i(y0 - a * x0 * x0)))
;;     (if (not (eql sym ())) (format t "y = ~S x 2 + ~S ~%"  (lldecimals a 6) (lldecimals b 6)))
;;     (set (if (null sym) 'fparab2 sym) 
;;          (compile ()
;;                   `(lambda (x) (+  (* ,a x x ) ,b ))))))

;;changed by aaa 28-08-95 from pw-modif
(defunp parabole/3 ((x0 float) (y0 float)
                    (x1 float (:value 2)) (y1 float (:value 15))
                    (x2 float (:value 1)) (y2 float (:value 7))
                    (sym list (:value "fparab3")) ) ()
    "calcule les paramètres de l'équation  y = ax^2 + bx + c en fct de trois points 
\(x0,y0) (x1,y1) (x2,y2) ."
  
  (let* ((a #i((y0 * (x1 - x2) + y1 * (x2 - x0) + y2 * (x0 - x1))
               /(x0 * x0 * (x1 - x2) + x1 * x1 * (x2 - x0) + x2 * x2 * (x0 - x1))))
         (b #i((y1 - y2 + a * (x2 * x2 - x1 * x1)) / (x1 - x2)))
         (c #i(y2 - a * x2 * x2 - b * x2)))
    (if (not (eql sym ())) (format t "y = ~S x 2 + ~S x + ~S  ~%" 
                                  (lldecimals a 6) (lldecimals b 6) (lldecimals c 6)))
    (set (if (null sym) 'fparab3 sym) 
         ;;(compile ()
         (eval `(function
                 (lambda (x) (+  (* ,a x x )  (* ,b x) ,c)))))))

;; (defunp parabole/3 ((x0 float) (y0 float)
;;                     (x1 float (:value 2)) (y1 float (:value 15))
;;                     (x2 float (:value 1)) (y2 float (:value 7))
;;                     (sym list (:value "fparab3")) ) ()
;;     "calcule les paramètres de l'équation  y = ax^2 + bx + c en fct de trois points 
;; (x0,y0) (x1,y1) (x2,y2) ."
;; 
;;   (let* ((a #i((y0 * (x1 - x2) + y1 * (x2 - x0) + y2 * (x0 - x1))
;;                /(x0 * x0 * (x1 - x2) + x1 * x1 * (x2 - x0) + x2 * x2 * (x0 - x1))))
;;          (b #i((y1 - y2 + a * (x2 * x2 - x1 * x1)) / (x1 - x2)))
;;          (c #i(y2 - a * x2 * x2 - b * x2)))
;;     (if (not (eql sym ())) (format t "y = ~S x 2 + ~S x + ~S  ~%" 
;;                                   (lldecimals a 6) (lldecimals b 6) (lldecimals c 6)))
;;     (set (if (null sym) 'fparab3 sym) 
;;          (compile ()
;;                   `(lambda (x) (+  (* ,a x x )  (* ,b x) ,c))))))

;;;; THE END ;;;;
