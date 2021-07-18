;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               clpf-utils.lisp
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
;;;;    1991-08-21 [jack] CLPF-UTILs.Lisp
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
(in-package "CLPF-UTIL")


(defun synonym (new-fun old-fun)
  "make symbol <new-fun> a synonym function of symbol <old-fun>"
  (setf (symbol-function new-fun) (symbol-function old-fun))
  (setf (documentation new-fun 'function) (documentation old-fun 'function))
  new-fun )

(defun vector-to-list (vector)
  "Takes the elements of a linear vector and collects them into a list."
  (do ((list () (cons (aref vector index) list))
       (index (1- (length vector)) (1- index)))
      ((< index 0) list)))

;; (defun vector-to-list1 (vector &aux (length (length vector)) (list ()))
;;   "Takes the elements of a simple vector and collects them into a list."
;;   (dotimes (index length)
;;     (push (svref vector (decf length)) list))
;;   list )
;;
;; (time (dotimes (i 10000) (vector-to-list #(a b c d))))
;; (dotimes (i 10000) (vector-to-list #(a b c d))) took 205 ticks (3.417 seconds) to run.
;;
;; (time (dotimes (i 10000) (vector-to-list1 #(a b c d))))
;; (dotimes (i 10000) (vector-to-list1 #(a b c d))) took 218 ticks (3.633 seconds) to run.

;; =============================================================================-======

(defun compile-file? (file)
  "Compiles <file> only if needed."
  (let* ((infile (merge-pathnames file "*.Lisp"))
         (outfile (compile-file-pathname infile)))
    (unless (and (probe-file outfile)
                 (> (file-write-date outfile) (file-write-date infile)))
      (compile-file infile :output-file outfile :verbose t) )))

;; =============================================================================-======

(defun file-compare… ()
  (let* ((file1 (ui:choose-file-dialog :button-string "Read 1st"))
         (file2 (ui:choose-file-dialog :button-string "Read 2nd"))
         (diffname (format () "~A/~A" (pathname-name file1) (pathname-name file2)))
         (outfile
          (ui:choose-new-file-dialog
           :directory
           (merge-pathnames (make-pathname  :name diffname :type "diff") file2)
           :prompt "Save difference as:"
           :button-string "Save diff" )))
    (file-compare file1 file2 outfile)))

(defun file-compare (file1 file2 outfile)
  (with-open-file (output-stream outfile :direction :output :if-exists :supersede)
    (let ((*package* *package*) l1 p1 l2 p2)
      (multiple-value-setq (l1 p1) (read-lists-from file1))
      (multiple-value-setq (l2 p2) (read-lists-from file2))
      (cond
       ((and (consp p1) (not (consp (cdr p1)))
             (consp p2) (not (consp (cdr p2)))
             (eql (first p1) (first p2)))
        (setf *package* (first p1))
        (print `(in-package ,(package-name (first p1))) output-stream))
       (t (ui:uiwarn "~S has packages ~S, while~%~S has packages ~S." file1 p1 file2 p2)))
    (list-compare l1 l2 output-stream))))

(defun read-lists-from (infile)
  (with-open-file (input-stream infile :direction :input)
    (let ((*package* *package*) (lists ()) (packages ()) read)
      (until (eql :eof (setq read (read input-stream nil :eof)))
        (when (and (listp read) (eql (first read) 'in-package))
          (newl packages (eval read)))
        (newl lists read))
      (values (nreverse lists) (nreverse packages)))))

(defun similar-exprs? (l1 l2)
  (and (listp l1) (listp l2) (eql (first l1) (first l2))
       (listp (cdr l1)) (listp (cdr l2)) (eql (second l1) (second l2))))

(defun list-compare (l1 l2 &optional (out *standard-output*))
  (let (pl1 expr1 pl2)
    (setq l1 (delete nil l1)
          l2 (delete nil l2)
          pl1 l1)
    (format out "~%~%;; Equal expressions")
    (while pl1
      (if (or (null (setq expr1 (first pl1)))
              (not (setq pl2 (member expr1 l2 :test #'equal))))
        (pop pl1)
        (progn
          (print expr1 out)
          (setf (car pl1) (cadr pl1)
                (cdr pl1) (cddr pl1)
                (car pl2) (cadr pl2)
                (cdr pl2) (cddr pl2)))))
    (format out "~%~%;; Similar expressions with differences")
    (setq l1 (delete nil l1)
          l2 (delete nil l2)
          pl1 l1)
    (while pl1
      (if (or (null (setq expr1 (first pl1)))
              (not (setq pl2 (member expr1 l2 :test #'similar-exprs?))))
        (pop pl1)
        (progn
          (format out "~%~%;; - ~S" (second expr1))
          (print expr1 out)
          (print (first pl2) out)
          (setf (car pl1) (cadr pl1)
                (cdr pl1) (cddr pl1)
                (car pl2) (cadr pl2)
                (cdr pl2) (cddr pl2)))))
    (setq l1 (delete nil l1)
          l2 (delete nil l2))
    (format out "~%~%;; Expressions of 1st list")
    (mapc (lambda (e) (print e out)) l1)
    (format out "~%~%;; Expressions of 2nd list")
    (mapc (lambda (e) (print e out)) l2)
    out))

;; =============================================================================-======
;; Expression prefixer
;; See tests at the end

;; ==== operation level ====

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct
      (level
       (:print-function
        (lambda (me stream depth)
          (declare (ignore depth))
          (format
           stream
           "#<ops=~S ~:[not~;   ~] associative ~:[~; ;  default=~:*~S~]~
       ~:[~;    inverse=~:*~S~]~:[~;   translations=~:*~S~]>"
           (level-ops me)
           (level-associative? me)
           (level-dop me)
           (level-iop me)
           (level-l-op.tr me)))))
    dop                                 ;default operators
    iop                                 ;inverse operators
    tr-default                          ;default operator translation
    tr-inverse                          ;inverse operator translation
    ops                                 ;all operators
    l-op.tr                             ;translations
    associative?)

  (defun level-has? (me op) (memq op (level-ops me)))
  (defun level-default? (me op) (memq op (level-dop me)))
  (defun level-inverse? (me op) (memq op (level-iop me)))
  (defun level-default (me) (first (level-dop me)))
  (defun level-translate (me op) (or (cassq op (level-l-op.tr me)) op))

  (defun create-level (dop iop l-op.tr associative? &aux me ops)
    "Create a level object from the specification of the default and inverse operators
and the associativity."
    (declare (ignorable ops))
    (unless (listp dop) (setq dop (list dop)))
    (unless (listp iop) (setq iop (list iop)))
    (setq me
          (make-level
           :dop dop
           :iop iop
           :ops (setq ops (remove-duplicates (append dop iop (mapcar #'car l-op.tr))))
           :l-op.tr l-op.tr
           :associative? associative?))
    (setf (level-tr-default me) (level-translate me (car (level-dop me)))
          (level-tr-inverse me) (level-translate me (car (level-iop me))))
    ;; (mapc
    ;;  (lambda (op)
    ;;      (check-type op symbol)
    ;;      (unless (eql (symbol-package op) (load-time-value (find-package "KEYWORD")))
    ;;        (import op "COMMON-LISP")
    ;;        (export op "COMMON-LISP")))
    ;;  ops)
    me)
  );;eval-when


;; ==== globals and macros ====

(defvar *levels*  (list
                   (create-level '\; () '((\; . progn)) t)
                   (create-level ':= () '((:= . setq)) nil)
                   (create-level '(==) () '((== . =)) t)
                   (create-level '(!=) () '((!= . /=)) t)
                   (create-level '(>) () () t)
                   (create-level '(<) () () t)
                   (create-level '(>=) () () t)
                   (create-level '(<=) () () t)
                   (create-level '+ '- () t)
                   (create-level '* '/ () t)
                   (create-level '(** ) () '((** . expt) ) nil)))

(defvar *all-ops* (apply #'append (mapcar #'level-ops *levels*)))

(defvar *default-operation* '*)

(defmacro operation? (op) `(memq ,op *all-ops*))

;; ==== help, user function and sharp macro character ====

(defun prefix-help ()
  "Describes the current levels of operations."
  (format t "~&The current levels of operations are:")
  (mapc 'print *levels*)
  (values))

(defun prefix-expr (expr)
  "Converts an (usually infixed) expression into a lisp (prefixed) expression.
Help on available operations can be obtained with (prefix-help)."
  (cond
   ((not (consp expr)) expr)
   ((and (symbolp (first expr)) (fboundp (first expr)))
    `(,(first expr) ,@(mapcar #'prefix-expr (rest expr))))
   (t (prefix-iexpr (test-syntax expr) *levels*))))


;; ==== internals ====

(defun prefix-iexpr (iexpr levels)
  ;; iexpr == (expr op expr op ... expr)
  (if (endp (rest iexpr))
    (prefix-expr (first iexpr))
    (let* ((exprs ())
           (ops ())
           (level (first levels))
           sub-iexpr)
      (while iexpr
        (setq sub-iexpr (list (nextl iexpr)))
        (while (and iexpr (not (level-has? level (first iexpr))))
          (newl sub-iexpr (nextl iexpr))
          (newl sub-iexpr (nextl iexpr)))
        (newl exprs (prefix-iexpr (nreverse sub-iexpr) (cdr levels)))
        (when iexpr (newl ops (nextl iexpr))))
      (simplify (nreverse exprs) (nreverse ops) level))))

(defun simplify (exprs ops level)
  ;; (length exprs) = (length ops) + 1
  (cond
   ;; only 1 elt
   ((null ops) (first exprs))
   ;; not associative (** ^): left to right
   ((not (level-associative? level))
    (let ((result (nextl exprs)))
      (mapc
       (lambda (op expr)
           (setq result (list (level-translate level op) result expr)))
       ops exprs)
      result))
   ;; associative and only inverse (- /)
   ((every (lambda (op) (level-inverse? level op)) ops)
    `(,(level-tr-inverse level) ,@exprs))
   ;; associative operator (+ - * /): skip default operators (+ *)
   ;; could use commutativity too...
   (t `(,(level-tr-default level)
        ,(nextl exprs)
        ,@(mapcar
           (lambda (op expr)
               (if (level-default? level op) expr
                   (list (level-translate level op) expr)))
           ops exprs)))))

(defun test-syntax (expr)
  "1st level syntax test and completion with * when omitted."
  (let ((orig-expr expr)
        (result ())
        (operation? nil)
        elt)
    (while expr
      (setq elt (first expr))
      (newl
       result
       (if operation?
         (if (operation? elt) (nextl expr) *default-operation*)
         (if (operation? elt)
           (if (eql orig-expr expr)
             (error
              "Syntax: the infixed expression should not begin with an operation:~%~S"
              orig-expr)
             (error
              "Syntax: The operations ~S and ~S should not be consecutive ~
               in the infixed expression:~%~S"
              (first result) elt orig-expr))
           (nextl expr))))
      (setq operation? (not operation?)))
    (unless operation?
      (error "Syntax: the infixed expression should not end with an operation:~%~S"
              orig-expr))
    (nreverse result)))

;; ==== global and user function ====

(defvar *compile-num-lambda* t)

;; (make-num-fun '(y := z - x \; y * z + y) nil)
;; => '(lambda (x z) (let (y) (progn (setq y (- z x)) (+ (* y z) y))))
;; but
;; (make-num-fun '(f(z x)= y := z - x \; y * z + y) nil)
;; => '(lambda (z x) (let (y) (progn (setq y (- z x)) (+ (* y z) y))))

(defun make-num-fun (fexpr)
  "Creates a lisp function object from the \"functional\" expr <fexpr> which is
basically an infixed expression (see prefix-expr and prefix-help).
When <fexpr> begins with something like (f(x)= ...), the formal arguments are taken
from the given list, otherwise they are deduced from the body of <fexpr> and collected
in the order they appear in it.
Local variables are automatically handled.
The resulting function is compiled when the value of *compile-num-lambda* is T (default)."
  ;; fexpr == <expr> || (<fun> <args> = . <expr>)
  (multiple-value-bind (lambda name) (make-num-lambda fexpr)
    (cond
      (*compile-num-lambda*
       (compile name lambda))
      (name
       (eval `(defun ,name ,@(rest lambda))))
      (t
       (coerce lambda 'function)))))

(defun make-num-lambda (fexpr)
  "Creates a lisp function object from the \"functional\" expr <fexpr> which is
basically an infixed expression (see prefix-expr and prefix-help).
When <fexpr> begins with something like (f(x)= ...), the formal arguments are taken
from the given list, otherwise they are deduced from the body of <fexpr> and collected
in the order they appear in it.
Local variables are automatically handled.
The resulting function is a lambda list not compiled."
  ;; fexpr == <expr> || (<fun> <args> = . <expr>)
  (let ((=? (and (consp fexpr) (consp (cdr fexpr)) (consp (cddr fexpr))
                 (symbolp (first fexpr))
                 (listp (second fexpr))
                 (eql '= (third fexpr))))
        (name ()) args expr rvars wvars)
    (when =?
      (setq name (nextl fexpr) args (nextl fexpr))
      (nextl fexpr))
    (setq expr (prefix-expr fexpr))
    (multiple-value-setq (rvars wvars)
      (if =? (rw-vars expr args) (rw-vars expr)))
    (values
     `(lambda ,rvars ,(if wvars `(let ,wvars ,expr) expr))
     name)))

;; ==== internals ====

(defun rw-vars (expr &optional (args () args-p))
  (let ((*rvars* (reverse args)) (*wvars* ()))
    (declare (special *rvars* *wvars*))
    (rw-vars-expr expr)
    (setq *rvars* (nreverse *rvars*))
    (when (and args-p (not (equal args *rvars*)))
      (ui:uiwarn "Found other free variables ~S not in ~S~%in expression ~S."
            (set-difference *rvars* args) args expr)
      (setq *rvars* args))
    (values *rvars* *wvars*)))

(defun rw-vars-expr (expr)
  (declare (special *rvars* *wvars*))
  (cond
   ((null expr))
   ((and (symbolp expr) (not (constantp expr)))
    (unless (or (memq expr *rvars*) (memq expr *wvars*))
      (newl *rvars* expr)))
   ((not (consp expr)))
   ((eql 'setq (car expr))
    (mapc #'rw-vars-expr (cddr expr))
    (unless (or (memq (second expr) *rvars*) (memq (second expr) *wvars*))
      (newl *wvars* (second expr))))
   (t (mapc #'rw-vars-expr (cdr expr)))))

;; =============================================================================-======
;; (defun tests ()
;;
;;   '#i(^ a b)
;;   '#i(a + * b)
;;   '#i(a + b *)
;;   (equal '#i(+ a b) '(+ a b))
;;   (equal '#i(a + () + b)     '(+ a () b))
;;   (equal '#i(a + b * c)      '(+ a (* b c)))
;;   (equal '#i(a + b * c + d)  '(+ a (* b c) d))
;;   (equal '#i(a + b c + d)    '(+ a (* b c) d))
;;   (equal '#i(a (cos (w t + f)))      '(* a (cos (+ (* w t) f))))
;;   (equal '#i(a ^ b ** c + d) '(+ (expt (expt a b) c) d))
;;   (equal '#i(a ** b ** c + d)        '(+ (expt (expt a b) c) d))
;;   (equal '#i(a + b + c + d)  '(+ a b c d))
;;   (equal '#i(a + b - c + d)  '(+ a b (- c) d))
;;   (equal '#i(a - b - c - d)  '(- a b c d))
;;   (equal '#i(a ^ b ^ c ^ d)  '(expt (expt (expt a b) c) d))
;;   (equal '#i(y := x + 2 \; y * y + y)
;;          '(progn (setq y (+ x 2)) (+ (* y y) y)))
;;   (equal (make-num-lambda '(y := x + 2 \; y * y + y))
;;          '(lambda (x) (let (y) (progn (setq y (+ x 2)) (+ (* y y) y)))))
;;   (equal (make-num-lambda '(y := x + z \; y * z + y))
;;          '(lambda (x z) (let (y) (progn (setq y (+ x z)) (+ (* y z) y)))))
;;   (equal (make-num-lambda '(f(z x)= y := x + z \; y * z + y))
;;          '(lambda (z x) (let (y) (progn (setq y (+ x z)) (+ (* y z) y)))))
;;   (equal (make-num-lambda '(f(x)= y := x + 2 \; y * y + y))
;;          '(lambda (x) (let (y) (progn (setq y (+ x 2)) (+ (* y y) y)))))
;;   (equal
;;    (make-num-lambda '(f(z)= y := (if (z > -1) (z + 1) 0) + 2 \; y * y + y))
;;    '(lambda (z) (let (y) (progn (setq y (+ (if (> z -1) (+ z 1) 0) 2)) (+ (* y y) y)))))
;;   (equal
;;    (make-num-lambda '(f(y)= y := (if (y > -1) (y + 1) 0) + 2 \; y * y + y))
;;    '(lambda (y) (progn (setq y (+ (if (> y -1) (+ y 1) 0) 2)) (+ (* y y) y))))
;;   (make-num-lambda '(f(z)= y := x + 2 \; y * y + y)))
;; =============================================================================-======

;;;; THE END ;;;;
