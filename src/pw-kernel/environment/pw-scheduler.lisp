;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pw-scheduler.lisp
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

;;============================================================
;; non real time scheduler test

(defclass C-clock ()
  ((clock :initform 0 :accessor clock)  
   (stop-time :initform 0 :accessor stop-time)  
   (processes :initform () :accessor processes)  
   (wait-list :initform () :accessor wait-list)))

(defgeneric start-clock (self stop-time processes))
(defmethod  start-clock ((self C-clock) stop-time processes)
  (setf (processes self) processes)
  (setf (clock self) 0)
  (setf (stop-time self) stop-time)
  (setf (wait-list self) ())
  (tell processes 'begin-process)
  (continue-clock self))

(defgeneric insert-in-wait-list (self form))
(defmethod  insert-in-wait-list ((self C-clock) form)
  (let ((wait-list (wait-list self))
        (time1 (car form))
        (new-wait-list))
    (while (and wait-list (> time1 (caar wait-list)))(push (pop wait-list) new-wait-list))
    (setf (wait-list self)
          (append  (nreverse new-wait-list) (list form) wait-list))))

(defgeneric stop-clock (self))
(defmethod  stop-clock ((self C-clock))
  (setf (stop-time self) (clock self)))

(defgeneric continue-clock (self))
(defmethod  continue-clock ((self C-clock))
  ;;  (print (list 'clock (clock self)))
  (if (>= (clock self) (stop-time self))
      (tell (processes self) 'stop-process)
      (let ((ready-list)
            (wait-list (wait-list self)))
        (while wait-list
          (if (= (clock self) (caar (wait-list self)))
              (progn 
                (push (pop (wait-list self)) ready-list)
                (pop wait-list))
              (setq wait-list ())))
        (setq ready-list (nreverse ready-list))
        (while ready-list
          (eval (cadr (pop ready-list))))
        (incf (clock self))
        (continue-clock self)))) 

;;______________

(defclass C-process ()
  ((clock-obj :initform nil :allocation :class :accessor clock-obj)
   (process   :initform nil :initarg :process :accessor process)))

(defmethod  initialize-instance :after ((self C-process) &key process)
  (declare (ignore process))
  (unless (clock-obj self)
    (setf (clock-obj self)(make-instance 'C-clock))))

(defgeneric stop-process (self))
(defmethod  stop-process  ((self C-process))
  (print 'stopped))

(defgeneric begin-process (self))
(defmethod  begin-process  ((self C-process))
  (funcall (process self) self))

(defgeneric dfuncall-process (self delay))
(defmethod  dfuncall-process  ((self C-process) delay)
  (insert-in-wait-list (clock-obj self) 
                       (*dfuncall* (+ delay (clock (clock-obj self))) (process self) self)))

;;______________


(defun *dfuncall*  (delay fun process)
  `(,delay (,fun ,process)))

#|

    ;; examples of different processes
    ;; a process has no arguments

    (defmethod  proc-print-bar  ((self C-process))
      (print (list 'bar (clock (clock-obj self))))
      (dfuncall-process self  (random2 2 6)))

    (defmethod  proc-print-foo  ((self C-process))
      (print (list 'foo (clock (clock-obj self))))
      (dfuncall-process self  (random2 10 12))) 

    (defmethod  proc-print-blah  ((self C-process))
      (print (list 'blah (clock (clock-obj self))))
      (dfuncall-process self 10))

    (defmethod  proc-print-blah2  ((self C-process))
    ;;  (write-midi-note 10 1 60 100) 
      (print (list 'blah2 (clock (clock-obj self))))
      (dfuncall-process self 20))

    (setq proc1 (make-instance 'C-process :process 'proc-print-bar))
    (setq proc2 (make-instance 'C-process :process 'proc-print-foo))
    (setq proc3 (make-instance 'C-process :process 'proc-print-blah))
    (setq proc4 (make-instance 'C-process :process 'proc-print-blah2))

    (start-clock (clock-obj proc1) 300 (list proc1 proc2 proc3 proc4))

|#

;; a process with an individual begin-time and duration-time

(defclass C-process-begin+end (C-process)
  ((begin-time :initform 0  :initarg  :begin-time :accessor begin-time)
   (duration-time  :initform 0 :initarg :duration-time :accessor duration-time)))

(defgeneric set-begin-time (self time))
(defmethod  set-begin-time  ((self C-process-begin+end) time)
  (setf (begin-time self) time))
(defgeneric set-duration-time (self time))
(defmethod  set-duration-time  ((self C-process-begin+end) time)
  (setf (duration-time self) time))

#|

    (defmethod  proc-print-beg1  ((self C-process-begin+end))
     (when (< (clock (clock-obj self)) (+ (begin-time self) (duration-time self)))
       (if  (>= (clock (clock-obj self)) (begin-time self))
          (progn 
            (print (list 'beg1 (clock (clock-obj self))))
            (dfuncall-process self 30))
          (dfuncall-process self (begin-time self)))))

    (defmethod  proc-print-beg2  ((self C-process-begin+end))
     (when (< (clock (clock-obj self)) (+ (begin-time self) (duration-time self)))
       (if  (>= (clock (clock-obj self)) (begin-time self))
         (progn 
           (print (list 'beg2 (clock (clock-obj self))))
           (dfuncall-process self 20))
         (dfuncall-process self (begin-time self)))))

    (defmethod  proc-print-beg3  ((self C-process-begin+end))
     (when (< (clock (clock-obj self)) (+ (begin-time self) (duration-time self)))
       (if  (>= (clock (clock-obj self)) (begin-time self))
         (progn 
           (print (list 'beg3 (clock (clock-obj self))))
           (dfuncall-process self 15))
         (dfuncall-process self (begin-time self)))))

    (defmethod  proc-print-beg31  ((self C-process-begin+end))
     (when (< (clock (clock-obj self)) (+ (begin-time self) (duration-time self)))
       (if  (>= (clock (clock-obj self)) (begin-time self))
         (progn 
           (print (list 'beg31 (clock (clock-obj self))))
           (dfuncall-process self 2))
         (dfuncall-process self (begin-time self)))))

    (defmethod  proc-print-beg11  ((self C-process-begin+end))
     (when (< (clock (clock-obj self)) (+ (begin-time self) (duration-time self)))
       (if  (>= (clock (clock-obj self)) (begin-time self))
         (progn 
           (print (list 'beg11 (clock (clock-obj self))))
           (dfuncall-process self 5))
         (dfuncall-process self (begin-time self)))))


    (setq procb1 (make-instance 'C-process-begin+end :process 'proc-print-beg1 
         :begin-time 0 :duration-time 300))
    (setq procb11 (make-instance 'C-process-begin+end :process 'proc-print-beg11 
        :begin-time 0 :duration-time 50))
    (setq procb2 (make-instance 'C-process-begin+end :process 'proc-print-beg2 
        :begin-time 100 :duration-time 150))
    (setq procb31 (make-instance 'C-process-begin+end :process 'proc-print-beg31 
        :begin-time 150 :duration-time 25))
    (setq procb3 (make-instance 'C-process-begin+end :process 'proc-print-beg3 
        :begin-time 200 :duration-time 150))

    (start-clock (clock-obj procb1) 1000 (list procb1 procb2 procb3 procb11 procb31))

|#
