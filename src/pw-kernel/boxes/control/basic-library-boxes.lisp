;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               basic-library-boxes.lisp
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

;;====================================================================================================
;; circular buffer
#|(defclass  C-pw-circ (C-pw-resize-x)  
  ((data :initform nil :accessor data)
   (lock :initform nil :accessor lock)
   (value :initform nil :accessor value)))|#

(defclass  C-pw-circ (C-pw-resize-x)  
  ((data :initform nil :accessor data)
   (value :initform nil :accessor value)
   (last-clock :initform 32000 :accessor last-clock)))

#|(defmethod initialize-instance :after ((self C-pw-circ) &key ctrls)
  (declare (ignore ctrls))
  (set-view-size self (w self) (+ (h self) 7))
  (set-view-position (out-put self) 
                     (make-point (x (out-put self)) (+ (y (out-put self)) 7)))
  (setf (lock self)
        (make-instance 'C-radio-button
                       :view-position (make-point (- (truncate (w self) 2) 5)
                                                  (- (h self) 22))
                       :view-size (make-point 8 8)
                       :dialog-item-text (get-initial-button-text self)
                       :view-font '("Monaco" 8)
                       :view-container self
                       :dialog-item-action (get-lock-button-fun self))))|#

;;(defmethod initialize-instance :after ((self C-pw-circ) &key ctrls))

;;(defmethod decompile ((self C-pw-circ)) (call-next-method))
  ;;;(append (call-next-method) (list (make-point (w self) (- (h self) 7)))))

#|(defmethod get-lock-button-fun ((self C-pw-circ))
  (lambda (item)
    (if (value (view-container item))
      (progn (set-dialog-item-text item "o")
             (setf (data self) nil))
      (set-dialog-item-text item "x"))
    (setf (value (view-container item))
          (not (value (view-container item))))))|#

#|(defmethod get-initial-button-text ((self C-pw-circ)) "o")|#

#|(defmethod init-patch ((self C-pw-circ))
  (call-next-method)
  (setf (data self) ()))|#


(defmethod init-patch ((self C-pw-circ))
  (call-next-method)
  (setf (data self) () (last-clock self) -1))

#|(defmethod patch-value ((self C-pw-circ) obj)
  (if (value self)
    (if (data self)
      (pop (data self))
      (progn
        (setf (data self) (patch-value (car (input-objects self)) obj))
        (pop (data self))))
    (progn (setf (data self) (patch-value (car (input-objects self)) obj))
           (car (data self)))))|#

(defmethod patch-value ((self C-pw-circ) obj)
  (if (= (last-clock self) (clock obj))
    (if (data self)
      (pop (data self))
      (progn
        (setf (data self) (value self))
        (pop (data self))))
    (progn (setf (last-clock self) (clock obj))
           (setf (value self) (patch-value (car (input-objects self)) obj))
           (setf (data self) (value self))
           (pop (data self)))))
  
;;(defmethod draw-patch-extra :after ((self C-pw-circ)) )
  ;;(draw-char (+ -10 (x self)(w self)) (- (+ (y self)(h self)) 4) #\B)) 

(defunp circ ((list list)) nil
"Circular buffer that accepts lists as input. The module is reinitialized at each 
mouse click on its output; this returns the first element of the list.  Indirect 
evaluation of circ  (select the module and type 'v') causes the list of circulate
 around the buffer. "
  (declare (ignore list)))

;;===================================

(defclass  C-pw-circ-end (C-pw-circ)     
  ((stop-flag  :initform t :accessor stop-flag)))
 
(defmethod init-patch ((self C-pw-circ-end))
  (call-next-method)
  (setf (stop-flag self) ()))

(defmethod patch-value ((self C-pw-circ-end) obj)
   (cond  
      ((cdr (data self)) (pop (data self)))
      ((not (stop-flag self))
         (setf (stop-flag self) t)
         (setf (data self) (patch-value (car (input-objects self)) obj))
         (pop (data self)))
      (t (stop-clock (clock-obj obj))
         (pop (data self)))))

(defunp cirend ((list list)) nil
"Behaves like circ-box but sends a stop message
to the box who is requesting its value instead of 
evaluating its input when the list has been consumed.
This box can be resized in x-direction by
clicking inside the small rectangle bottom-right
and dragging the mouse."
  (declare (ignore list)))

;;===================================

(defclass C-pw-narg (C-pw-extend)())  

(defmethod patch-value ((self C-pw-narg) obj)
  (let ((args (ask-all (cdr (input-objects self)) 'patch-value obj)))
    (apply (patch-value (car (input-objects self)) obj) args)))

(defvar arg1 () "Applies function (fn) with argument (arg1) 
This box can be extended by option-clicking bottom-right 
\(E = extend).") 
(defvar arg2 () "Applies function (fn) with arguments (arg1 - arg2)  
This box can be extended by option-clicking bottom-right 
\(E = extend).") 
(defvar arg3 () "Applies function (fn) with arguments (arg1 - arg3)  
This box can be extended by option-clicking bottom-right 
\(E = extend).") 
(defvar arg4 () "Applies function (fn) with arguments (arg1 - arg4)  
This box can be extended by option-clicking bottom-right 
\(E = extend).") 
(defvar arg5 () "Look at the documentation of arg2.")
(defvar arg6 () "Look at the documentation of arg2.")
(defvar arg7 () "Look at the documentation of arg2.")
(defvar arg8 () "Look at the documentation of arg2.")
(defvar arg9 () "Look at the documentation of arg2.")
(defvar arg10 () "Look at the documentation of arg2.")

;;===================================
(defclass C-pw-test (C-pw-extend)())  

(defmethod give-new-extended-title ((self C-pw-test)) 'test)

(defmethod generate-extended-inputs ((self C-pw-test)) 
  (make-pw-ntest-arg-list (1+ (truncate (- (length (pw-controls self)) 3) 2))))

(defmethod correct-extension-box ((self C-pw-test) new-box values)
  (let ((ctrls (cdr (pw-controls new-box)))
        (values  (cdr (append (butlast values) (list* 0 0 (last values))))))
    (mapc (lambda (ctrl val) 
              (setf (value ctrl) val)
              (set-dialog-item-text ctrl (format () "~D" val))) ctrls values)
    (when (not (eql (car (last (input-objects self))) (car (last (pw-controls self)))))
      (setf (nth (1- (length values)) (input-objects new-box))
            (nth (1- (length values)) (pw-controls new-box)))
      (setf (open-state (nth (1- (length values)) (pw-controls new-box))) t)
      (setf (car (last (input-objects new-box))) (car (last  (input-objects self))))
      (setf (open-state (car (last  (pw-controls new-box)))) nil))))
  

(defmethod patch-value ((self  C-pw-test) obj)
  (let ((fun (patch-value (car (input-objects self)) obj))
        (input (list! (patch-value (nth 1 (input-objects self))  obj)))
        (test t)
        (count 1)
        (res) testarg1 accum)
    (while (setq testarg1 (pop input))
      (setq test t count 1 res nil)
      (while test
        (if (= count (- (length (input-objects self)) 2))
          (progn  
            (setq test ())
            (setq res 
                  (patch-value (nth (1- (length (input-objects self)))
                                    (input-objects self)) obj)))
          (progn  
            (when (funcall fun testarg1
                           (patch-value (nth (incf count) (input-objects self)) obj))
              (setq test ())
              (setq res (patch-value (nth (incf count)(input-objects self)) obj)) )) )
        (incf count))
      (push res accum))
    (if (cdr accum) (nreverse accum) (car accum))))

(defunp test ((pred (symbol (:value '=))) (input nilNum) (test-1 nilNum) (val1 nilNum)
               (else nilNum) &rest (testn nilNum)) nil 
"test applies a test function <pred> (or <testf> if other inputs are open)  using 
input and test1 as arguments.  If the test succeeds val1 is evaluated, otherwise 
else is evaluated.  This module can be extended to include multiple cases. In 
this case input is  compared with test1, then test2, test3, etc.; as soon as the 
test function succeeds, the corresponding val patch will 
be evaluated. For example if test1 and test2 return nil, but test3 returns a true, 
val3 is evaluated.  
In this case test4 and test5 will never be considered.  If all tests fail, then the 
else patch is evaluated. If input is a list, a list is returned with the results of 
applying the module's result to each element of the input list."
  (declare (ignore pred input test-1 val1 else testn)))

(defvar test2 () "Applies a testfunction (testfn) using as 
arguments input and one of the test-boxes on the left 
side of the PW-box.
The test is repeated for each test-box and 
if one of the tests succeeds,the corresponding val-box is
evaluated.If none of the tests succeed,else-box is 
evaluated.  
This box can be extended by option-clicking bottom-right 
\(E = extend)."
)

;;===================================
(defclass C-pw-gclock (C-patch)())  

(defmethod patch-value ((self  C-pw-gclock) obj)
  (declare (ignore obj))
  (clock *global-clock*))

(defunp pw-clock () nil
"Returns the value of the global clock in ticks.")
;;===================================

(defclass C-clock-constant (C-patch)  
   ((last-clock :initform 32000 :accessor last-clock)
    (last-value :initform 0 :accessor last-value)))
 
(defmethod patch-value ((self  C-clock-constant) obj)
 (if (= (last-clock self) (clock obj))
    (last-value self)
    (progn
      (setf (last-value self)  (patch-value (car (input-objects self)) obj)) 
      (setf (last-clock self) (clock obj))
      (last-value self))))

(defmethod init-patch ((self C-clock-constant))
  (call-next-method)
  (setf (last-clock self) -1))

(defunp cconst ((val nilNum)) nil
"(cconst = Clock constant)
Evaluates its input only if the clock of the requesting
box is different from the clock of the previous request.
Otherwise the value of the last evaluation is returned.
This is useful when you want to prevent evaluation."
  (declare (ignore val)))

(defunp ev-once ((val nilNum)) nil
        "This module bufferizes automatically its input and assures that all the modules 
that are connected to its output receive the same value. The internal buffer is reset each 
time an evaluation of a patch is triggered by a mouse-click, so use the mouse instead of 
the keyboard ('v') to evaluate when there is an ev-once in your patch."
  (declare (ignore val)))

;;===================================
(defclass C-pw-loop (C-patch)())  

(defmethod patch-value ((self  C-pw-loop) obj)
  (let ((res)(count (patch-value (car (input-objects self)) obj)))
    (repeat count
      (push (patch-value (nth 1 (input-objects self)) obj) res))
    (nreverse res)))

(defunp pwrepeat ((count (fix>0 (:value 10))) (patch nilNum)) list
"PWrepeat box allows you to collect data as a list.
PWrepeat box allows you to evaluated the
input <patch> <count> times.The first input <count> tells how many times the 
second
input <patch> is evaluated."
  (declare (ignore count patch)))
  
;;===================================
;; 
(defpackage "USER-SUPPLIED-IN-OUTS")

(defclass C-pw-out (C-patch)())  

(defmethod initialize-instance :after ((self C-pw-out) &key controls)
  (declare (ignore controls))
  (set (intern (dialog-item-text (car (pw-controls self))) "USER-SUPPLIED-IN-OUTS")  self))

;;(defmethod set-pw-out-symbol ((self C-pw-out) ctrl)
;;  (set (value ctrl) self))
 
(defmethod patch-value ((self C-pw-out) obj)
  (patch-value (car (input-objects self)) obj))

(defunp out ((patch user-out)) nil
"The out module allows one to pass 
the evaluation of a patch to a remote destination 
\(i.e., to another patch or to another window). 
out modules are assigned a name. in modules with the 
same name receive the results of the patch evaluation.
For example:
It is possible to have to windows that communicate with each other. 
In the first window , a patch can generate data that will be 
transmitted through the out module and received in a second window 
\(PW1) by module in. Notice that the two modules (in and out) have identical names.

Warning : it is advised to always load new modules 
to assign pairs of variables; also, avoid duplicating 
or changing the names of the already used  in ;and out modules."
  (declare (ignore patch)))

;;===================================

(defclass C-pw-in (C-patch)())  

(defmethod init-patch ((self C-pw-in))
  (init-patch (symbol-value (patch-value (car (input-objects self)) ()))))

(defmethod patch-value ((self  C-pw-in) obj)
  (patch-value (symbol-value (patch-value (car (input-objects self)) obj)) obj))

(defunp in ((name user-in (:type-list (no-connection)))) nil
"The in module receives remote messages from  out ;
modules that share the same name. 
See an example in the reference of out module."
  (declare (ignore name)))

;;;; THE END ;;;;

