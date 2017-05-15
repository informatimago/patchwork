;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pw-list-functions.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;
;;;;    PW-list-functions
;;;;
;;;;AUTHORS
;;;;    Mikael Laurson, Jacques Duthen, Camilo Rueda.
;;;;    contributions by Tristan Murail
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


(defunp flat-once ((list list)) list
    "flattens the first level of a list of lists.Ex:
'( ((1 2 3)  (4 5 6))  ((7 8 9) (10 11 12)) ) becomes:
\((1 2 3) (4 5 6) (7 8 9) (10 11 12))"
  (if (consp (car list))
      (apply 'append list)  list))

(defunp make-list2 ((count (fix (:value 10))) (elem nilNum)) list
    "Returns a list of length count filled with elem "
  (make-list count :initial-element elem))

(defunp create-list ((count (fix (:value 10))) (elem nilNum)
                     &optional (list-fill list)) list
    "Returns a list of length <count> filled with repetitions of element <elem> "
  (if list-fill
      (list-fill list-fill count)
      (make-list count :initial-element elem)))

(defunp l-nth ((list list) (l-nth numbers?)) nil
    "<l-nth> may be a number or a list of numbers.  Returns a copy of <l-nth> where
each number is replaced by the corresponding element in the list <l>.
Eg: (l-nth  '(2 (0) 1) '(a b c d)) => (c (a) b)."
  (deep-mapcar 'l-nth 'nth l-nth list))

(defunp posn-match ((list list) (l-nth numbers?)) nil
    "<posn-match> can be a number or a list of numbers.  Returns a copy of <l-
nth> where each number is replaced by the corresponding element in the list <l>.
For example,  if  <list> is  (a b c d) and <l-nth> is (2 (0) 1)) the box returns  (c
\(a) b), where the list returned has the same structure as <list> "
  (deep-mapcar 'l-nth 'nth l-nth (pw::list! list)))

(defunp l-min ((list list)) fix
    "minimum value(s) of a list or list of numbers"
  (if (not (consp (first list)))
      (apply 'min list)
      (mapcar (lambda (x) (apply 'min x)) list)))

(defunp l-max ((list list)) fix
    "maximum value(s) of a list or list of numbers"
  (if (not (consp (first list)))
      (apply 'max list)
      (mapcar (lambda (x) (apply 'max x)) list)))

(defun the-min (x) (apply 'min x))
(defun the-max (x) (apply 'max x))

(defunp g-min ((list list)) midics?
    "returns a tree of the minimum value(s) of the leaves of each deepest level
subtree.
Trees must be well-formed: The children of a node must be either all leaves or
all nonleaves. "
  (less-deep-mapcar #'the-min (list! list)))

(defunp g-max ((list list)) midics?
    "returns a tree of the maximum value(s) of the leaves of each deepest level
subtree.
Trees must be well-formed: The children of a node must be either all leaves or
all nonleaves. "
  (less-deep-mapcar #'the-max (list! list)))

(defunp last-elem ((list list)) ()
    "returns the last element of <list>"
  (first (last (pw::list! list))))

(defunp l-last ((list list)) ()
    "returns the last element of list"
  (first (last list)))

(defunp flat ((lst list)) list
    "Takes off every parenthesis. There should be no dotted pair."
  (nreverse (rev-flat lst)))

(defunp rev-flat ((lst list)) list
    "Fast (nreverse (flat lst))."
  (let ((l ()))
    (while lst
      (if (not (consp (car lst)))
          (newl l (nextl lst))
          (setq l (nconc (rev-flat (nextl lst)) l))))
    l ))

;;les fonctions suivantes inspirées des inoubliables cours de LISP de M. Assayag
;; add by aaa 29-08-95 from pw-modifs

(in-package :pw)

(pw::defunp group-list ((list1 list)
                        (segm list)
                        (lecture menu (:menu-box-list (("lin" . 1)
                                                       ("circ". 2) )
                                       :type-list
                                       (no-connection)))) list
    "Ce module groupe les éléments de la liste <list1> en
sous-listes consécutives dont les longueurs sont données dans la liste <segm>
Exemple:
où
<list1>= '(0 1 2 3 4 5 6 7 8 9 10)
<segm>= '(2 4 3 2)
\(groupe-liste '(0 1 2 3 4 5 6 7 8 9 10)  '(2 4 3 2))
pw->((0 1) ( 2 3 4 5) ( 6 7 8) (9 10)).
Il est possible de choisir des lectures linéaires ou circulaires de <list1>
grâce au paramètre <lecture>"
  (remove nil
          (epw::list-filter 'eq  '()
                            (let ((aux) (aux1) (k 0))
                              (dolist (n segm (reverse aux1))
                                (dotimes (m n)
                                  (push
                                   (if (= lecture 1) (nth k list1) (nth (mod
                                                                         k (length list1)) list1))  aux)
                                  (incf k))
                                (push (reverse aux) aux1)
                                (setf aux nil))))))

(pw::defunp interlock ((lis1 list) (lis2 list) (plc1 list) ) list
    "insère les éléments de <lis2> dans <lis1> avant les éléments de <lis1> dont les rangs sont donnés par <plc1>"
  (let ((aux) (pointeur 0))
    (dotimes (n (length lis1) (reverse aux))
      (when (epw::included? (list n ) plc1)
        (progn ()
               (push (nth pointeur lis2) aux)
               (incf pointeur)))
      (push (nth n lis1) aux))))

(pw::defunp subs-posn ((lis1 list) (posn list) (val list)) list
    "substitution de tous les éléments de <lis1> qui sont dans les
positions <posn> par les éléments correspondant de la liste <val>.
Si la <posn> demandée n'existe pas l'opération de substitution n'est pas
effectuée."
  (let ((posn (pw::list! posn)) (val (pw::list! val)))
    (mapcar (lambda (p v) (when  (and (or (zerop p) (plusp p)) (< p
                                                                  (length lis1))) (setf (nth p lis1) v) ))  posn  val)
    lis1))


(pw::defunp last-n ((list list) (n fix>=0)) list
    "retourne les <n>derniers éléments de la liste <list>"
  (last list n))



(pw::defunp first-n ((list list) (n fix>=0)) list
    "retourne les <n> premiers éléments de la liste <liste>"
  (cond
    ((< (length list)  n) list )
    (t  (butlast  list (- (length list) n)))))


(in-package "EPW")


(defunp x-append ((l1? list (:type-list ())) (l2? list (:type-list ()))
                  &rest (lst? list (:type-list ()))) list
    "appends lists or atoms together to form a new list. This box can be
extended."

  (apply 'append (list! l1?) (list! l2?) (mapcar #'list! lst?)))

(defunp unique ((lst list)) list
    "returns a copy of <list>, dropping duplicate values (deepest level)"
  (less-deep-mapcar (lambda (x) (unique-1 x #'eq)) lst))

;; (defunp rem-dups ((lst list)
;;                   &optional (test symbol (:value 'eq :type-list ()))) list
;;     "<rem-dups>  removes repetitions of elements in <lst>,  according to <test> (if the
;; second input is open by clicking on 'E'). <test> must be commutative. For example, the list
;; \(this this is my list list) returns (this is my list). Note that the last occurrence of a
;; repeated element in a list is preserved;  thus, the list: (1 2 3 1 4) returns  (2 3 1 4).
;; Returns a copy of <lst>."
;;   (if test test #'eq)
;;   (cond
;;     ((not (consp lst)) lst)
;;     ((listp (car lst)) (mapcar (lambda (x) (rem-dups x test)) lst))
;;     (t (unique-1 lst test))))
;;
;; (defunp rem-dups ((lst list)
;;                   &optional (test symbol (:value 'eq :type-list ()))
;;                   (depth menu (:menu-box-list (("shallow" . 1) ("deep". 2))))) list
;;     "<rem-dups>  removes repetitions of elements in <lst>,  according to <test> (if
;; the second input is open by clicking on 'E'). <test> must be commutative. For
;; example, the list (this this is my list list) returns (this is my list). Note that the last
;; occurrence of a repeated element in a list is preserved;  thus, the list: (1 2 3 1
;; 4) returns  (2 3 1 4). Returns a copy of <lst>."
;;   (unless test (setq test #'eq))
;;   (cond
;;     ((not (consp lst)) lst)
;;     ((every 'listp lst)
;;      (if (eql depth 2)
;;          (mapcar (lambda (x) (rem-dups x test)) lst)
;;          (unique-1 lst test)))
;;     (t (unique-1 lst test))))

;; making test act on shallow or deep list level)

(defunp rem-dups ((lst list)
                  &optional (test symbol (:value 'eq :type-list ()))
                  (depth menu (:menu-box-list (("shallow" . 1) ("deep". 2))))) list
    "<rem-dups>  removes repetitions of elements in <lst>,  according to <test> (if
the second input is open by clicking on 'E'). <test> must be commutative. For
example, the list (this this is my list list) returns (this is my list). Note that the last
occurrence of a repeated element in a list is preserved;  thus, the list: (1 2 3 1
4) returns  (2 3 1 4). Returns a copy of <lst>."

  (unless test (setq test #'eq))
  (cond
    ((not (consp lst)) lst)
    ((every 'listp lst)
     (if (eql depth 2)
         (mapcar (lambda (x) (rem-dups x test)) lst)
         (unique-1 lst test)))
    (t (unique-1 lst test))))

(defunp unique-1 ((lst list) (test list)) list
    "returns a copy of the list, dropping duplicate values"
  (cond
    ((null lst) ())
    ((member (car lst) (cdr lst) :test test) (unique-1 (cdr lst) test))
    (t (cons (car lst) (unique-1 (cdr lst) test)))))

(defunp sort-list ((lst list)
                   &optional (test symbol (:value '< :type-list ()))
                   (key list (:value '() :type-list ()))) list
    "This module sorts  a list. By default, the order of the sort is ascending, but
since the module is extensible, you can open a second entry <test> to set the
choice of order. If <test> is '>' the order is ascending, '<' indicates descending,
and '=' keeps the order the same. One can also open a third input <key> for a
function. The function <key> evaluates each element of the list <lst> and the
result is then sorted according to the parameter <test>.
\(epw::sort-list ‘(3 13 15 17 9 10 16 3 6 7 1 12 6))will return
 ? PW->(1 3 3 6 6 7 9 10 12 13 15 16 17),

\(epw::sort-list ‘(3 13 15 17 9 10 16 3 6 7 1 12 6)  ‘>)
 will return
? PW->(17 16 15 13 12 10 9 7 6 6 3 3 1),

\(epw::sort-list ‘((13 13 4) ( 3 9 3) ( 16 16 1) (11 13 6) )   ‘<   ‘first)
will return
? PW->((3 9 3) (11 13 6) (13 13 4) (16 16 1)), and

\(epw::sort-list ‘((13 13 4) ( 3 9 3) ( 16 16 1) (11 13 6) )   ‘<   ‘second)
will return
? PW->((3 9 3) (13 13 4) (11 13 6) (16 16 1))
"
  (sort (copy-list lst) (or test #'<) :key key))

(defunp l-order ((list list)  (funct list (:value "<"))) list
    "gives the order of the elements of <list> according to
function <funct> ( < by default)"
  (mapcar (lambda (x) (position x list)) (epw::sort-list list funct)))

;;changed by aaa 28-08-95 from pw-modif
(defunp posn-order ((list list)  (funct list (:value "<"))) list
    "Ce module retourne une liste de rangs de la liste <list>
    ordonnée à partir de <funct>. Il est possible de changer <funct>
    et obtenir les rangs à partir d'autres ordres de rangement."
  (let* ((thelist (epw::sort-list list funct))  rep)
    (mapc (lambda (x)
            (let* ((repons (position x thelist)) (i 0))
              (while (position repons rep)
                (incf i)
                (setf repons (position  x thelist :start (+ (position x thelist) i))))
              (setf rep (cons repons rep)))) list)
    (reverse rep)))

;; (defunp posn-order ((list list)  (funct list (:value "<"))) list
;;     "Ce module retourne une liste de rangs de la liste <list>
;;     ordonnée à partir de <funct>. Il est possible de changer <funct>
;;     et obtenir les rangs à partir d'autres ordres de rangement."
;;   (mapcar (lambda (x) (position x list)) (pw::sort-list list funct)))

(defun noRep-union (lists oper test key)
  (let ((the-union (list! (car lists))))
    (dolist (one-in (cdr lists))
      (setq the-union (funcall oper the-union (list!  one-in)  :test test :key key)))
    the-union))

(defunp x-union ((l1? list (:type-list ())) (l2? list (:type-list ()))
                 &optional (test symbol (:value 'equal :type-list ()))
                 (key (symbol (:value 'identity)))
                 &rest (list list (:type-list ()))) list
    "This box merges 2  lists, <l1?> and <l2?>,
into a single list, with no repetitions.
If the optional <test> argument is added, it is used as a predicate to
detect equality between elements and and avoid repetition. Default value for <test> is the
function 'equal. If the optional <key> argument is added, it is used as
an accessor (e.g. first, second etc.) into the elements of the lists,
prior to executing the <test> function.
Additional lists  can be compared by extending the box."
  (noRep-union  (list* l1? l2? (and list (list! list))) 'union test key))

(defunp x-intersect ((l1? list (:type-list ())) (l2? list (:type-list ()))
                     &optional (test symbol (:value 'equal :type-list ()))
                     (key (symbol (:value 'identity)))
                     &rest (list list (:type-list ()))) list
    "This box returns a list of elements which are common
to both <l1?> and <l2?>.If the optional <test> argument is added, it is used as a predicate to
detect equality between elements. Default value for <test> is the
function 'equal. If the optional <key> argument is added, it is used as
an accessor (e.g. first, second etc.) into the elements of the lists,
prior to executing the <test> function.
Additional lists  can be compared by extending the box.
Beware that this operation is not commutative. For example:
\(epw::x-intersect  '(1 2 4 5 4) '( 2 4)) will return -> (2 4 4)
\(epw::x-intersect  '( 2 4)  (1 2 4 5 4)) will return -> (2 4 )
"
  (nreverse (noRep-union  (list* l1? l2? (and list (list! list))) 'intersection test key)))

(defunp x-Xor ((l1? list (:type-list ())) (l2? list (:type-list ()))
               &optional (test (symbol (:value 'equal)))
               (key (symbol (:value 'identity)))
               &rest (list list (:type-list ()))) list
    "This box compares lists <l1?> and <l2?> for elements that are present in
either one or the other list (but not in both), and then
returns them in a list. If the optional <test> argument is added, it is used as a predicate to
detect equality between elements. Default value for <test> is the
function 'equal. If the optional <key> argument is added, it is used as
an accessor (e.g. first, second etc.) into the elements of the lists,
prior to executing the <test> function.
Additional lists  can be compared by extending the box."
  (nreverse (noRep-union  (list* l1? l2? (and list (list! list))) 'set-exclusive-or test key)))


(defunp x-diff ((l1? list (:type-list ())) (l2? list (:type-list ()))
                &optional (test symbol (:value '= :type-list ()))
                (key (symbol (:value 'identity)))
                &rest (list list (:type-list ()))) list
    "This box compares l1? to l2? and then returns all elements
present in l1? but not in l2?, as a list.
If the optional <test> argument is added, it is used as a predicate to
detect equality between elements. Default value for <test> is the
function 'equal. If the optional <key> argument is added, it is used as
an accessor (e.g. first, second etc.) into the elements of the lists,
prior to executing the <test> function.
Additional lists  can be compared by extending the box. They have the same status as <l2?>"
  (nreverse (noRep-union  (list* l1? l2? (and list (list! list))) 'set-difference test key)))


(defunp included? ((lst1 list) (lst2 list) &optional (test (symbol (:value 'equal)))) bool
    "This box compares two lists, returning true if all the elements
in the first are also elements of the second. If the optional <test> argument
 is added, it is used as a predicate to detect equality between elements.
Default value for <test> is the function 'equal. "
  (subsetp lst1 lst2 :test test))

(defunp list-fill ((list list) (len fix>=0 (:value 10))) list
    "Duplicates the elements of <list> until its length equals <len>."
  (check-type len (integer 0 *) "a positive integer")
  (let* ((length (length (setq list (list! list))))
         ;; len = length*n + r
         (n (floor len length))
         (r (mod len length))
         ;; len = r*(n+1) + (length-r)*n
         (l ()))
    (repeat r
      (repeat (1+ n) (newl l (car list)))
      (nextl list))
    (repeat (- length r)
      (repeat n (newl l (car list)))
      (nextl list))
    (nreverse l)))

(defunp list-filter1 ((liste list)(fct (list (:value "="))) (val float) ) list
    "retire de la liste <list> toutes les valeurs répondant à la condition <fct val>"
  (let ((res))
    (dolist (num liste (nreverse res))
      (unless (funcall fct  num val) (push num res)))))

(defunp list-filter ( (test (list (:value "="))) (val float) (list list)) list
    "list-filter  removes elements from a <list> according to a predicate <test>. If the
predicate is 'eq', all instances of <val>  are removed from the list, regardless of
their level. If, for example, the predicate is >, all elements of list which are
greater than <val> are removed. Note that <val> can be a string, but only if the
predicate <test> can handle a string.
\(epw::list-filter  ‘=    5   ‘(5 7 3 5 11 5 16 3 1 7 15 5 8 10 0 7 5 4 5 10))
will return             ? PW->(7 3 11 16 3 1 7 15 8 10 0 7 4 10) ,
with    test.    "
  (less-deep-mapcar 'list-filter1 list test val ))

(defunp multi-filter ((test (list (:value "="))) (val float) (list list) (numcol fix)) list
    "retire de chacune des sous-listes de <list> les éléments dont le numéro d'ordre
correspond à chaque élément de la sous-liste de numéro <numcol> qui satisfait
à la condition <test val>"
  (if (atom (car list)) (list-filter test val list)
      (let ((longueur (length (nth numcol list)))
            (ncol (1- (length list)))  res)
        (dotimes (n longueur)
          (if (funcall test  (car (nth numcol list)) val)
              ()
              (for (i 0 1 ncol)
                (push (car (nth i list)) res)))
          (setq list (ll-suppress list 0)) )
        (list-part (reverse res) (1+ ncol))) ))

(defunp table-filter ((test (list (:value "="))) (val float) (list list) (numcol fix)) list
    "<list> is a list of sub-lists. <numcol> is an index into <list> that selects a sub-list.
<test> is a test function that is tested against each element of that sublist and <val>
\(e.g. (<test> <val> <element>). Matching elements are deleted from that sublist, as well
as elements at the same position in every other sub-list.
Example:
\(table-filter '= 100 '((1 2 3) (4 100 6) (7 8 9)) 1)
-> '((1 3) (4 6) (7 9))"
  (multi-filter test val list numcol))

(defun list-filter2 (liste val &optional not)
  "deletes from liste all elements contained in any one of tha ranges give in the
list of lists val."
  (let ((res))
    (dolist (num liste (nreverse res))
      (if not
          (if (in-the-range  num val) (push num res))
          (unless (in-the-range  num val) (push num res))))))

(defun in-the-range (value a-list)
  (dolist (range a-list nil)
    (if (and (>= value (apply 'min range)) (<= value (apply 'max range)))
        (return t))))

(defunp band-reject ((list list) (val list)) list
    "band-reject"
  (if (not (consp (car val))) (setq val (list val)))
  (less-deep-mapcar 'list-filter2 list val ))

(defunp band-pass ((list list) (val list)) list
    "band-pass"
  (if (not (consp (car val))) (setq val (list val)))
  (less-deep-mapcar 'list-filter2 list val t))

(defunp band-filter ((list list) (val list)
                     &optional (pass? fix>=0 (:value 1 :max-val 1))) list
    "<band-filter>  passes or rejects all elements from <list> that fall inside a band
of specified values of <val> . The range of values <val>  is given either as a list
of two numbers or as a list of lists of two numbers. Each pair of numbers define
an interval.  If <pass?> (the optional argument)  is one (the default) only the
element in list falling inside one of these intervals (of <val>) is selected. If
<delete>  is zero, elements in <list>  not falling
inside one of those intervals is selected. Intervals are defined by values inside
list.
For example, if <list> is  (2 4 6 8 10 12 14 16 18 20) and <val> is ((1 2 3) (7  9)),
<band-filter> returns (2 8), (the default is one). On the other hand (if the third
input is open), if <list> is  (2 4 6 8 10 12 14 16 18 20),  <val> is ((1 2 3) (7  9))
and <pass?> is 0 (zero), <band-filter> returns (4 6 10 12 14 16 18 20). The
argument list can be a list of lists. In this case the described behavior applies to
each sublist."
  (if (not (consp (car val))) (setq val (list val)))
  (less-deep-mapcar 'list-filter2 list val (= pass? 1)))

(defun within-range (x range-list test)
  (if test
      (in-the-range x range-list)
      (dolist (range range-list t)
        (if (and (>= x (apply #'min range)) (<= x (apply #'max range)))
            (return nil)))))

(defun band-select2 (list posns test)
  (let (result (ind 0))
    (dolist (elem list (nreverse result))
      (if (within-range ind posns test) (push elem result))
      (incf ind))))


(defunp band-select ((list list) (posn list)
                     &optional (delete integer (:value 0 :min-val 0 :max-val 1))) list
    "selects those elements of list that are within the range of positions given in posn.
if the optional argument is = 1, it rejects those elements"
  (let ((posns (if (consp (car posn)) posn (list posn)))
        (test (= delete 0)))
    (less-deep-mapcar 'band-select2 list posns test)))

(defunp range-filter ((list list) (posn list)
                      &optional (delete integer (:value 0 :min-val 0 :max-val 1))) list
    "<range-filter>  selects from a  list <list> all elements falling inside a range of
given positions <posn>.  The range of positions <posn>  is given either as a list
of two numbers or as a list of lists of two numbers. Each pair of numbers define
an interval.  If <delete> (the optional argument)  is zero (the default) any
element in list   falling inside one of these intervals is selected. If <delete>  is
one, elements in <list>  not falling inside one of those intervals is selected.
Intervals are defined by position inside list. For example, if <list> is  (4 7 2 3 1 8
5) and <posn> is ((4 5) (0 2)),  <range-filter> returns (4 7 2 1 8). On the other
hand (if the third input is open),
if <list> is  (4 7 2 3 1 8 5),  <posn> is ((4 5) (0 2))  and <delete> is 1, <range-
filter> returns (3 5). The argument list  can be a list of lists. In this case the
described behaviour applies to each sublist."
  (band-select list posn delete))


(defunp list-part ((list list) (ncol fix>0 (:value 2))) list
    "partitions <list> in <ncol> lists containing the elements modulo <ncol>"
  (let ((vector (make-array ncol :initial-element nil)) res)
    (while list
      (for (i 0 1 (1- ncol))
        (and list (vset vector i (push (pop list) (vref vector i))))))
    (for (i 0 1 (1- ncol))
      (push (remove nil (nreverse (vref vector i))) res))
    (nreverse res)))

(defunp list-modulo ((list list) (ncol fix>0 (:value 2))) list
    "<list-modulo>  groups elements of a list that occur at regular intervals,
 and returns these groups as lists. <ncol> defines the interval between group
members.
 For example, if we take the list (1 2 3 4 5 6 7 8 9) and give 2 for ncol, the result
is ((1 3 5 7 9) (2 4 6 8)).  In other words, every second element starting with the
first, and then every second element starting with the second. If the number of
<ncol> exceeds the number of elements in the list, the remaining lists are
returned as nil. In effect, list-modulo divides <list> into <ncol> sublists
containing elements modulo <ncol> according to their position in the list."
  (list-part list ncol))

;; (defunp list-explode ((list list) (nlists fix>0 (:value 2))) list
;;     "list-explode  divides a list into <nlist> sublists of consecutives elements.
;; For example, if list is (1 2 3 4 5 6 7 8 9), and ncol is 2, the result will be: ((1 2 3 4 5) (6 7 8 9)),
;; if list is (1 2 3 4 5 6 7 8 9), and ncol is 5, the result will be: ((1 2) (3 4) (5 6) (7 8) (9)).
;; If the number of divisions exceeds the number of elements in the list,
;; the remaining divisions will be returned as nil."
;;   (let* ((length (length list))  res
;;          (step (ceiling length nlists))
;;          (end (- length 1))  ;step))
;;          (ser (arithm-ser 0 1 (1- step))) )
;;     (for (i 0 step end)
;;       (push (remove () (l-nth  list(l+ i ser))) res))
;;     (nreverse res)))

;;high feverish and ugly... Minimizes number of sublists of different lengths [Camilo]
(defunp list-explode ((list list) (nlists fix>0 (:value 2))) list
    "list-explode divides a list into <nlist> sublists of consecutives elements.
For example, if list is (1 2 3 4 5 6 7 8 9), and ncol is 2, the result is ((1 2 3 4 5)
\(6 7 8 9)),
if list is (1 2 3 4 5 6 7 8 9), and ncol is 5, the result is: ((1 2) (3 4) (5 6) (7 8) (9)).
If the number of divisions exceeds the number of elements in the list, the
remaining divisions are returned as nil."
  (if (> nlists (length list))
      (setq list (append list (make-list (- nlists (length list)) :initial-element (first (last list))))))
  (if (<= nlists 1) list
      (let* ((length (length list))
             (low (floor length nlists))
             (high (ceiling length nlists))
             (step (if (< (abs (- length (* (1- nlists) high))) (abs (- length (* nlists low))))
                       high  low))
             (rest (mod length nlists))
             (end (- length 1 rest))
             (ser (arithm-ser 0 1 (1- step)))
             res)
        (for (i 0 step end)
          (push (remove () (l-nth  list(l+ i ser))) res))
        (setq low (length (flat-once res)))
        (if (< low length) (setq res (cons (append (first res) (nthcdr low list)) (rest res))))
        (cond ((> (length res) nlists)
               (nreverse (cons (nconc (second res) (first res)) (nthcdr 2 res))))
              ((< (length res) nlists)
               (when (= (length (first res)) 1)
                 (setq res (cons (nconc (second res) (first res)) (nthcdr 2 res))))
               (nreverse (nconc (nreverse (list-explode (first res) (1+ (- nlists (length res)))))
                                (rest res))))
              (t (nreverse res))))))

(defunp lo-flat ((list list)) list
    "Flattens lowest level sublists. Ex:
'( ((1 2 3)  (4 5 6))  ((7 8 9) (10 11 12)) ) becomes:
\((1 2 3 4 5 6) (7 8 9 10 11 12)) "
  (cond ((atom list) list)
        ((atom (car list)) (cons (car list) (lo-flat (cdr list))))
        ((atom (caar list)) (apply 'append list))
        (t (cons (lo-flat (car list)) (lo-flat (cdr list))))))

(defunp flat-low ((list list)) list
    "Flattens lowest level sublists. Ex:
'( ((1 2 3)  (4 5 6))  ((7 8 9) (10 11 12)) ) becomes:
\((1 2 3 4 5 6) (7 8 9 10 11 12)) "
  (lo-flat list))

(defunp ll-suppress ((lliste list) (elem numbers? )) list
    " retire les éléments de numéro <elem> de chaque sous-liste de la liste"
  (mapcar (lambda (x) (l-delete x elem)) lliste))

(defunp l-delete ((list list) (elem numbers? )) list
    "deletes the elemth (can be a list) element from list. If <elem> is a list of
numbers, these have to be ordered "
  (let ((numbers (get-useful-nums elem (length list))))
    (if numbers
        (let ((save-l (copy-list list)))
          (if (zerop (car numbers))
              (cdr (remove-all save-l (cdr numbers)))
              (remove-all save-l numbers)))
        list)))

(defun get-useful-nums (nums length)
  (let ((elems (unique (list! nums))) list)
    (dolist (a-num elems (nreverse list))
      (cond ((minusp a-num) )
            ((< a-num length) (push a-num list))
            (t (return (nreverse list)))))))

(defun remove-all (list nums)
  (let ((count 0))
    (dolist (item nums list)
      (setf (nthcdr (- item count) list) (nthcdr (+ item 1 (- count)) list))
      (incf count))))

(defunp l-scaler/max ((list list) (max float)) list
    "scales <list> so that its maximum becomes <max>"
  (let ((scaler (/ max (list-max list))))
    (l* list scaler)))

(pw::defunp epw::l-scaler/sum ((list list) (sum float (:value 1))) list
    "scales <list> so that its sum becomes <sum>"
  (epw::g-scaling/sum list sum))

(defunp g-scaling/sum ((list list) (sum midics? (:value 1))) list
    "scales <list> (may be tree) so that its sum becomes <sum>. Trees must be
well-formed. The children of a node must be either all leaves or all nonleaves. "
  (less-tree-mapcar (lambda (x y) (l* x (/ y (apply #'+ x)))) list sum t))

(defunp g-scaling/max ((list list) (max midics? (:value 1))) list
    "scales <list>  (may be tree) so that its max becomes <max>. Trees must be
well-formed: The children of a node must be either all leaves or all nonleaves. "
  (less-tree-mapcar (lambda (x y) (l* x (/ y (list-max x)))) list max t))

(defunp permut-circ ((list list) &optional (nth (fix (:value 1)))) list
    "Returns a circular permutation of a copy of <list> starting from its <nth>
element,
 (<nth> is the argument of the second optional input)  (which defaults to 1) ,
\(<nth> = 0 means the first element of <list>,  <nth> = 1 means the second
element of <list>, and so on)
For example, if <list> is  (1 2 3 4 5 6 7 8 9 10)   <permut-circ> returns (2 3 4 5 6
7 8 9 10 1), (the default is one). On the other hand (if the second input is open,
<nth> ),
if <list> is  (1 2 3 4 5 6 7 8 9 10),   and <nth> is 3 (zero) ,  <permut-circ> returns
\(4 5 6 7 8 9 10 1 2 3).."
  (permut-circn (copy-list list) nth))

(defunp permut-circn ((list list) &optional (nth (fix (:value 1)))) list
    "Returns a destructive circular permutation of <list> starting from its <nth> (which
defaults to 1) element, (n=0 means the \"car\", n=1 means the \"cadr\")."
  (when list
    (let ((length (length list)) n-1thcdr)
      (setq nth (mod nth length))
      (if (zerop nth) list
          (prog1
              (cdr (nconc (setq n-1thcdr (nthcdr (1- nth) list)) list))
            (rplacd n-1thcdr ()))))))

(defunp nth-random ((list list)) ()
    "nth-random returns a random element from its input <list>.
  For example, if the list (1 2 3 foo bar) might return the value 3 at the first
evaluation;
 at the next time a value was requested it might return the string foo, the next
time maybe 3 again, etc."
  (nth (random (length list)) list))

(defunp permut-random ((list list)) list
    " Returns a random permutation of list."
  (npermut-random (copy-list list)))

(defunp npermut-random ((list list)) list
    "Returns a destructive random permutation of <list>."
  (let ((result ()) (length (length list)) (list (copy-list list)))
    (nconc list list)
    (repeat length
      (setq list (nthcdr (random length) list)
            result (rplacd (prog1 (cdr list) (rplacd list (cddr list))) result)
            length (1- length)))
    result))



(defunp mat-trans ((matrix list (:value '((1 2))))) list
    "<mat-trans>  transposes a matrix. That is, it interchanges rows and columns.
 Thus for example, (mat-trans  '((1 2) (5 7))) returns the list ((1 5) (2 7)), or
if <matrix> is ((1 2) (3 4) (5 6) (7 8) (9 10)) <mat-trans> returns ((1 3 5 7 9) (2 4
6 8 10)).
 <mat-trans> behaves as if the sublists of matrix  where arranged vertically.
 Then a sublist is constructed for each column resulting from this arrangement.
 the result is the list of all these sublists. "
  ;;; GA 160996 replaced apply by loop due to new PPC compiler behaviour (3.9p1b1)
  (let ((maxl (1- (loop for elt in matrix maximize (length elt))))
        result)
    (for (i 0 1 maxl)
      (push (remove nil (mapcar (lambda (list) (nth i list)) matrix)) result))
    (nreverse result)))




(in-package :pw)

;;;Usual LISP functions
(defunt first ((list list)) nil)
(defunt rest ((list list)) list)
(defunt butlast ((list list) &optional (num fix>=0 (:value 1))) list)
(defunt reverse ((sequence list)) list)
(defunt length ((sequence list)) fix)
(defunt list (&rest (arguments nilNum)) list)
(defunt mapcar ((function symbol (:value "1+")) (list list) &rest (more-lists list)) list)
(defunt apply ((function symbol (:value "+")) (first-arg list) &rest (more-args list (:type-list ()))) nil)
(defunt cons ((x nilnum) (list-or-thing list (:type-list ()))) list)
(defunt funcall ((function symbol (:value "+")) &rest (arguments nilnum)) nil)
(defunt print ((object nilnum)  &optional (output-stream list (:value '() :type-list ()))) nil)


(defvar *valid-expand-chars* '(#\* #\_))

(defunp expand-lst ((list list (:value '(3*(2 4) 0_8)))) list
    "Expands a list by one (or both) of the following:

1. Repeating each item number times following a pattern
   of the form: number*

2. Creating a sequence of numbers going from n to m by steps of k,
indicated by the pattern n-m s k. A step of 1 can be omitted.

For example the list (3* (2 4) 0-8), returns

 (2 4 2 4 2 4 0 1 2 3 4 5 6 7 8),

 and the list (2* (a z 2*(4 12)  (1-5 )) 0-16s2) returns

 (a z 4 12 4 12 (1 2 3 4 5) a z 4 12 4 12 (1 2 3 4 5) 0 2 4 6 8 10 12 14 16)."
  (and list
       (let ((lists (pw::list! list))  result)
         (while lists
           (let ((next-elem (pop lists)))
             (cond
               ((symbolp next-elem)
                (let* ((form (coerce (format () "~A" next-elem) 'list))
                       (from-char (is-in form *valid-expand-chars*))
                       (char-symb (car from-char))
                       (third (cdr from-char))
                       (int (butlast form (length from-char)))
                       up-to)
                  (cond
                    ((and (not third) char-symb (char= #\* char-symb) int
                          (numberp (setq int (read-from-string (coerce int 'string)))))
                     (push (apply #'append
                                  (make-list int
                                             :initial-element
                                             (expand-lst (pop lists))))
                           result))
                    ((and char-symb (char= #\_ char-symb) third
                          (numberp (setq int (read-from-string (coerce int 'string)))))
                     (if (setq from-char (member #\s third :test #'char=))
                         (progn (setq up-to (butlast third (length from-char))
                                      char-symb (car from-char) third (cdr from-char))
                                (if (and char-symb
                                         (char= #\s char-symb)
                                         (or (null third)
                                             (numberp
                                              (setq third (read-from-string (coerce third 'string)))))
                                         (numberp
                                          (setq up-to (read-from-string (coerce up-to 'string)))))
                                    (push (epw::arithm-ser int (or third 1) up-to) result)
                                    (push (list next-elem) result)))
                         (progn
                           (setq up-to (read-from-string (coerce third 'string)))
                           (push (epw::arithm-ser int 1 up-to) result))
                         ))
                    (t (push (list next-elem) result)))))
               ((consp next-elem)
                (push (list (expand-lst next-elem)) result))
               (t (push (list next-elem) result)))))
         (apply #'append (nreverse result)))))

(defun is-in (list chars)
  (let (res)
    (dolist (ch chars res)
      (if (setq res (member ch list :test #'char=)) (return res)))))
