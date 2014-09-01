;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               scheduler.lisp
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
;;;;    Lee Boynton
;;;;MODIFICATIONS
;;;;    2012-05-07 <PJB> Changed license to GPL3; Added this header.
;;;;    1991-07-15 [jack]      Scheduler.Lisp
;;;;    1991                   Scheduler -- CLPF V1.0 (after Lee Boynton) © 1991 IRCAM
;;;;    1988       Lee Boynton preFORM 3.0 Scheduler 
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright IRCAM 1986 - 2012
;;;;    Copyright 1988 Lee Boynton, MIT Media Lab. For non-profit use only 
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
(in-package "PATCHWORK.SCHEDULER")


;; =============================================================================-======

(defparameter *eventhook* nil)

;; User Constants

(defconstant *highest-priority* 3
  "This constant is the highest priority available.
It cannot be changed, except in the source code.")

;; =============================================================================-======
;;;; 4-state Scheduler

;; The scheduler is stopped when an internal error occurs.
;; This variable contains nil or the last condition which occured.
(defvar *internal-error?*)

;; The current state of the scheduler
(defvar *scheduler-state*)

;; The scheduler's state set by init-scheduler
(defvar *scheduler-initial-state* :RT)

;; The state which will be restored when *nb-waiting-tasks* is 0 (in :OOT1 mode)
(defvar *scheduler-old-state*)

;; This flag indicates whether the scheduler runs in real time
(defvar *scheduler-RT?* t)

;; The number of steps the scheduler will execute in :STEP mode
(defvar *scheduler-steps* 0)

;; The difference between the scheduler time and the clock time
(defvar *schedulertime--clocktime* 0)

;; Returns the current real-time.
;; Any task with an exectime smaller than the current real-time is ready to execute.

(defmacro real-time () '(+ (patchwork.midi:clock-time) *schedulertime--clocktime*))

;; The current scheduler time.
(defvar *scheduler-time*)

;; =============================================================================-======
;;; Task objects (structure)

(defstruct
    (task
     (:constructor
         make-task (exectime logtime advance priority function arguments))
     (:print-function
      (lambda (task stream depth)
        (declare (ignore depth))
        (format stream "#<task @ time ~a>" (task-exectime task)))))
  link
  exectime
  logtime
  advance
  priority
  function
  arguments)

(defvar *free-tasks*)
(defvar *default-task*)
(defvar *current-task* nil)
(defvar *current-priority* -1)

;; =============================================================================-======
;;; User task context (read/write macros) and schedule macros

(defmacro advance ()
  "Returns the advance of the current scheduler task."
  `(task-advance *current-task*))
(defmacro priority ()
  "Returns the priority of the current scheduler task."
  `(task-priority *current-task*))
(defmacro logtime ()
  "Returns the logical time of the current scheduler task."
  `(task-logtime *current-task*))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (fmakunbound 'patchwork.midi:midi-write))
(defun patchwork.midi:midi-write (event)
  "New version"
  (patchwork.midi:midi-write-time event
                                  (if (null *current-task*)
                                      (patchwork.midi:midi-get-time)
                                      (* 10 (- (logtime) *schedulertime--clocktime*)))))

(defvar *error-when-extra-start?* t
  "Calling \"start\" from within a scheduler task signals an error when this variable
is T (the default).")

(defmacro start (&body body)
  "Initiates a scheduler pseudo-task which can create new tasks and returns the last
value of its body."
  (let ((start?-var (gensym "START?-")))
    `(let ((,start?-var (null *current-task*))
           (*current-task* *default-task*)
           (*current-priority* *highest-priority*))
       (if ,start?-var
           (setf (logtime) (if *scheduler-RT?* (real-time) *scheduler-time*))
           (when *error-when-extra-start?*
             (error "~S should not be called from within a scheduler task, with body~%~S"
                    'start '(start ,@body))))
       ,@body)))

(defmacro apdfuncall (advance priority delay function . arguments)
  "Evaluates immediately all its arguments (producing garbage with the <arguments>
list) and creates a scheduler task with the given <advance> and <priority>, which
will apply <function> to <arguments> after the given <delay>."
  `(when (check-start '(apdfuncall ,advance ,priority ,delay ,function ,@arguments))
     (create-task
      (logtime) ,advance ,priority ,delay ,function (list ,@arguments))))  ;GARBAGE

(defmacro dfuncall (delay function . arguments)
  "Evaluates immediately all its arguments (producing garbage with the <arguments>
list) and creates a scheduler task with the current advance and priority, which
will apply <function> to <arguments> after the given <delay>."
  `(apdfuncall (advance) (priority) ,delay ,function ,@arguments))

;; This routine does not allocate any new argument list.

(defmacro re-dfuncall (delay . arguments)
  "Reuses efficiently the current scheduler task with the same function, advance and
priority.  It must not be called more than once from the same task."
  `(let* ((task *current-task*)
          (arg-list (task-arguments task)))
     ;; HACK! I need a place to store a flag and a value
     ;; HACK! the delay is stored in (task-exectime task) (see execute-task)
     (setf (task-link task) :re-dfuncall (task-exectime task) ,delay)
     ,@(mapcan
        (lambda (arg) (list `(rplaca arg-list ,arg) '(pop arg-list)))
        arguments)
     task))

(defmacro with-more-priority (&rest body)
  "Continues the current task with the same priority, but the \"dfuncall\"s which
occur inside <body> will create new tasks with the next higher priority level which
is (1+ (priority)) unless *highest-priority* is achieved."
  `(with (((priority) (min *highest-priority* (1+ (priority))))) ,@body))

(defmacro with-less-priority (&rest body)
  "Continues the current task with the same priority, but the \"dfuncall\"s which
occur inside <body> will create new tasks with the next lower priority level which
is (1- (priority)) unless 0 is achieved."
  `(with (((priority) (max 0 (1- (priority))))) ,@body))

;; =============================================================================-======
;;; The scheduler state and user functions

(defun scheduler-state () "Returns the state of the scheduler." *scheduler-state*)

(defun set-scheduler-state (state)
  "Changes the state of the scheduler to <state> which must be one of the
keywords: :RT :STEP :OOT :OOT1."
  (if (eql state :RT)
      (unless *scheduler-RT?*
        (setf *schedulertime--clocktime* (- *scheduler-time* (patchwork.midi:clock-time))
              *scheduler-RT?* t))
      (progn
        (setf *scheduler-RT?* nil)
        (ecase state
          (:OOT1 (unless (eql *scheduler-state* :OOT1)
                   (setf *scheduler-old-state* *scheduler-state*)))
          (:STEP
              (format *error-output*
                      ";Warning: The scheduler is stopped. To restart it, try:~%~S"
                      '(set-scheduler-state :rt))
              (setf *scheduler-steps* 0))
          (:OOT))))
  (setf *internal-error?* nil *scheduler-state* state))

(defmacro with-scheduler-OOT1 (&body body)
  "Starts and evaluates the <body> with the scheduler in the mode \"Out-Of-Time-Once\",
and reverts to the previous mode after evaluation.  This is indispensable when wishing
to use the scheduler mode :OOT1 and starting with an empty scheduler queue."
  `(start (set-scheduler-state :OOT1) ,@body))

(defun scheduler-step (&optional (nb-step 1))
  "Signals an error if the scheduler was not in the :STEP mode.  Otherwise, the clock
was currently stopped and when the next interrupt comes (probably just after the
evaluation of the current form), the scheduler will execute the <nb-step> (1, by default)
next tasks.  It will first execute the ready ones if any (those which exectime is smaller
than the current scheduler time) or the clock will jump to the next waiting task."
  (unless (eql *scheduler-state* :STEP)
    (error "The scheduler must be in the :STEP mode. Use the function ~S to stop it."
           'set-scheduler-state))
  (setf *scheduler-steps* nb-step))

;; =============================================================================-======

;; Signals an error if the function is not embedded in a start

(defun check-start (form)
  (or *current-task*
      (error "The form ~S must be called from within a call to the macro ~S."
             form 'start)))

(defmacro tcdr (task) `(task-link ,task))

;; Remakes a task from the free pool.

(defun remake-task (exectime logtime advance priority function arguments
                    &aux (task *free-tasks*))
  (setf *free-tasks* (tcdr task)
        (task-exectime task) exectime
        (task-logtime task) logtime
        (task-advance task) advance
        (task-priority task) priority
        (task-function task) function
        (task-arguments task) arguments)
  task)

;; Creates a new task.
;; Tries to get one from the free pool, otherwise physically creates a new one.

(defun create-task (logtime adv pri delay fun args)
  (assert
   (or (typep fun 'function) (and (typep fun 'symbol) (fboundp fun)))
   (fun) "Cannot delay the undefined function ~S." fun)
  (without-interrupts
    (incf logtime delay)
    (wait-task
     (if *free-tasks*
         (remake-task (- logtime adv) logtime adv pri fun args)
         (make-task (- logtime adv) logtime adv pri fun args)))))

;; Aborts a scheduled task by replacing its function with a null one.

(defun no-op (&rest args) args)

(defun abort-task (task)
  "If the <task> has not begun yet, it makes it inoperant when its exectime comes
by replacing its function with a null one."
  (setf (task-function task) #'no-op)
  task)

;; =============================================================================-======
;;;; Queues
;;; The wait and ready queues are vectors containing chained tasks (see tcdr)
;;; For efficient insertion, there are pointers to the head and queue of each chain.
;;; The wait queue is indexed by the exectime of tasks (modulo its size).
;;; The ready queue is indexed by the priority of the tasks.

(defconstant *wait-queue-size* 2000)
(defvar *wait-queue-heads* (make-array (list *wait-queue-size*)))
(defvar *wait-queue-tails* (make-array (list *wait-queue-size*)))

(defconstant *ready-queue-size* (+ *highest-priority* 1))
(defvar *ready-queue-heads* (make-array (list *ready-queue-size*)))
(defvar *ready-queue-tails* (make-array (list *ready-queue-size*)))

(defvar *nb-waiting-tasks* 0)

(defun print-scheduler-queue (&optional (stream t))
  "Prints the current state of the scheduler queues (useful for debug when the scheduler
is stopped in the :STEP mode)."
  (let (task (tmax (+ *scheduler-time* *wait-queue-size*)))
    (format stream "; At current exec-time ~D (clock=~D) [~S mode]~%"
            *scheduler-time* (patchwork.midi:clock-time) (scheduler-state))
    (format stream "; Ready   |  lat |  exec  |   log  | adv | pri| form~%")
    (do ((index (1- *ready-queue-size*) (1- index)))
        ((<= index 0))
      (when (setq task (svref *ready-queue-heads* index))
        (pretty-print-task task)))
    (unless (zerop *nb-waiting-tasks*)
      (format stream "; Waiting |  lat |  exec  |   log  | adv | pri| form~%")
      (do ((the-time *scheduler-time* (1+ the-time)))
          ((>= the-time tmax))
        (when (setq task (svref *wait-queue-heads* (mod the-time *wait-queue-size*)))
          (pretty-print-task task))))))

(defun pretty-print-task (task &optional (stream t))
  (let ((next-task (task-link task)))
    (format stream "; #<task  |~5D |~7D |~7D |~4D |~3D |~%~S>~%"
            (- *scheduler-time* (task-exectime task))
            (task-exectime task)
            (task-logtime task)
            (task-advance task)
            (task-priority task)
            `(,(task-function task)
              ,@(mapcar (lambda (arg) `',arg) (task-arguments task))))
    (when next-task (pretty-print-task next-task stream))))

;; At current exec-time 219954 (clock=310151) [:step mode]
;; Ready   |  lat |  exec  |   log  | adv | pri| form
;; Waiting |  lat |  exec  |   log  | adv | pri| form
;; #<task  | -200 | 220154 | 220159 |   5 |  1 | (t-sched::dlooprint '8 '200)>
;; #<task  | -350 | 220304 | 220309 |   5 |  1 | (set-scheduler-state :step)>
;; #<task  | -850 | 220804 | 220809 |   5 |  1 | (set-scheduler-state :rt)>

;; Inserts a task in the wait-queue.

(defun wait-task (task &aux (exectime (task-exectime task)))
  (if (< exectime *scheduler-time*)
      (ready-task task)
      (let* ((index (mod exectime *wait-queue-size*))
             (head (svref *wait-queue-heads* index))
             tail)
        (incf *nb-waiting-tasks*)
        (cond
          ((null head)
           (setf (svref *wait-queue-heads* index) task
                 (svref *wait-queue-tails* index) task
                 (tcdr task) nil))
          ((<= (task-exectime (setq tail (svref *wait-queue-tails* index))) exectime)
           (setf (tcdr task) nil
                 (tcdr tail) task
                 (svref *wait-queue-tails* index) task))
          ((> (task-exectime head) exectime)
           (setf (tcdr task) head
                 (svref *wait-queue-heads* index) task))
          (t (insert-by-time head exectime task)))))
  task)

;; [in :RT mode] Finds the tasks which exectimes are between
;; the current *scheduler-time* and the new *scheduler-time*.
#|
(defun ready-tasks ()
(without-interrupts
(let* ((real-time (real-time))
(lateness (- real-time *scheduler-time*))
index task)
(when (> lateness 0)
(if (and (> lateness *wait-queue-size*)
(> lateness (max (* 1.5 *wait-queue-size*) *highest-latency*)))
(progn
(reset-scheduler) ; This is not the right place - should be at execute-task
(format t "Scheduler late.~%"))
(if (<= *nb-waiting-tasks* 0)
(setf *scheduler-time* real-time)
(do ((the-time *scheduler-time* (1+ the-time)))
((> the-time real-time)
(setq *scheduler-time* real-time))
(setq index (mod the-time *wait-queue-size*))
(when (and (setq task (svref *wait-queue-heads* index))
(<= (task-exectime task) the-time))
(ready-tasks-time the-time index task)))))))))
|#

;; [in :RT mode] Finds the tasks which exectimes are between
;; the current *scheduler-time* and the new *scheduler-time*.

(defun ready-tasks (&aux (real-time (real-time)))
  (without-interrupts
    (when (> *nb-waiting-tasks* 0)
      (let ((lateness (- real-time *scheduler-time*)) index task tmax)
        (unless (>= lateness 0)
          (error "The real time ~S is smaller than the scheduler time ~S."
                 real-time *scheduler-time*))
        (setq tmax (if (<= lateness *wait-queue-size*) real-time
                       (+ *scheduler-time* *wait-queue-size*)))
        (do ((the-time *scheduler-time* (1+ the-time)))
            ((> the-time tmax))
          (setq index (mod the-time *wait-queue-size*))
          (when (and (setq task (svref *wait-queue-heads* index))
                     (<= (task-exectime task) real-time))
            (ready-tasks-time real-time index task)))))
    (setq *scheduler-time* real-time)))

;; [in :STEP mode] Increments the scheduler-time until the next ready tasks if any
;; and returns a non-nil value.

(defun ready-tasks? ()
  (without-interrupts
    (when (> *nb-waiting-tasks* 0)
      (let ((tmin nil)
            (tmax (+ *scheduler-time* *wait-queue-size*))
            index task exectime)
        (or
         (do ((the-time *scheduler-time* (1+ the-time)))
             ((>= the-time tmax) nil)
           (setq index (mod the-time *wait-queue-size*))
           (when (setq task (svref *wait-queue-heads* index))
             (if (= (setq exectime (task-exectime task)) the-time)
                 (return (ready-tasks-time the-time index task))
                 (when (or (null tmin) (< exectime tmin))
                   (setq tmin exectime)))))
         (when tmin
           (ready-tasks-time
            tmin (setq index (mod tmin *wait-queue-size*))
            (svref *wait-queue-heads* index))))))))

;; Extracts the chain of waiting tasks and puts them into the ready queue.

(defun ready-tasks-time (the-time index task &aux next-task)
  (loop
    (setq next-task (tcdr task))
    (decf *nb-waiting-tasks*)
    (ready-task task)
    (if (not (and next-task (<= (task-exectime next-task) the-time)))
        (return)
        (setq task next-task)))
  (unless (setf (svref *wait-queue-heads* index) next-task)
    (setf (svref *wait-queue-tails* index) nil))
  (setq *scheduler-time* the-time))

;; Puts the given task into the ready queue.

(defun ready-task (task)
  (let* ((exectime (task-exectime task))
         (index (task-priority task))
         (head (svref *ready-queue-heads* index))
         (tail (svref *ready-queue-tails* index)))
    (cond
      ((null head)
       (setf (svref *ready-queue-heads* index) task
             (svref *ready-queue-tails* index) task
             (tcdr task) nil))
      ((<= (task-exectime tail) exectime)
       (setf (tcdr task) nil
             (tcdr tail) task
             (svref *ready-queue-tails* index) task))
      ((> (task-exectime head) exectime)
       (setf (tcdr task) head
             (svref *ready-queue-heads* index) task))
      (t (insert-by-time head exectime task))))
  task)

;; Extracts the next task that is ready with respect to the given priority level.

(defun next-ready-task (priority &aux task)
  (do ((i (1- *ready-queue-size*) (1- i)))
      ((<= i priority) nil)
    (when (setq task (svref *ready-queue-heads* i))
      (unless (setf (svref *ready-queue-heads* i) (tcdr task))
        (setf (svref *ready-queue-tails* i) nil))
      (return task))))

;;; Debugging the scheduler tasks

(defvar *highest-latency* 300
  "When the difference between the real-time and a task's exectime is greater than the
value of this variable, the task is considered to be very late.")

(defvar *print-on-late?* t
  "If T (the default), the task which is late is printed out.")
(defvar *step-on-late?* nil
  "If T (not the default), the task which is late stops the scheduler at the current
time with the :STEP mode.")
(defvar *reset-on-late?* nil
  "If T (not the default), the task which is late resets the scheduler.")
(defvar *eval-on-late?* t
  "If T (the default), the task which is late is executed anyway.")
(defvar *late-task* nil
  "The last task which was late.")

(defvar *print-on-error?* t
  "If T (the default), the task which produced an error is printed out.")
(defvar *step-on-error?* t
  "If T (the default), the task which produced an error stops the scheduler at the current time.")
(defvar *reset-on-error?* nil
  "If T (not the default), the task which produced an error resets the scheduler.")
(defvar *error-task* nil
  "The last task which produced an error.")
(defvar *condition* nil
  "The condition produced by the last task which produced an error.")

;; Executes the task.

(defun execute-task (task)
  (if (< (- (real-time) (task-exectime task)) *highest-latency*)
      (eval-task task)
      (progn
        (setf *late-task* task)
        ;;(when *print-on-late?* (format *error-output* "Late ~S" task))
        ;;Camilo [090593] late tasks printed only once
        (when *print-on-late?* (format *error-output* "Late ~S" task) (setf *print-on-late?* nil))
        (when *step-on-late?* (set-scheduler-state :STEP))
        (when *reset-on-late?* (reset-scheduler))
        (when *eval-on-late?* (eval-task task))))
  nil)

;; Evaluates a task in its dynamic context,
;; disposes of its argument list and puts it into the free pool.

;; [jack] 910905
;; This version works fine.
;; When an error occurs, the user entries the debugger and can inspect whatever,
;; but, when he continues, the scheduler is late.
;; It would be much better to handle the user's errors in a way which stops the
;; scheduler (:STEP mode) and invokes the debugger, with the options to reset or
;; restart the scheduler after debug.

(defun eval-task (task)
  (let ((*current-task* task)
        (*current-priority* (task-priority task))
        logtime)
    (apply (task-function task) (task-arguments task))
    (cond
      ((eql (task-link task) :re-dfuncall)
       (setf (task-logtime task)
             ;; hack! the delay is stored in (task-exectime task) (see re-dfuncall)
             (setq logtime (+ (task-logtime task) (task-exectime task)))
             (task-exectime task) (- logtime (task-advance task)))
       (wait-task task))
      (t
      ;;; (freelist% (task-arguments task)) ;GARBAGE
       (setf (tcdr task) *free-tasks*
             *free-tasks* task
             (task-arguments task) nil)))))

#|
(defun eval-task (task)
(let ((*current-task* task)
(*current-priority* (task-priority task))
logtime (error nil))
(apply (task-function task) (task-arguments task))
#|
(handler-case (apply (task-function task) (task-arguments task))
(serious-condition (condition)
(setf *error-task* task *condition* condition error t)
(when *print-on-error?*
(format *error-output*
";Warning: the serious-condition ~A occured while executing:~%"
condition)
(pretty-print-task task *error-output*))
(when *step-on-error?* (set-scheduler-state :STEP))
(when *reset-on-error?* (reset-scheduler)))
      ;(:no-error ...); BUG MCL2.0 ?    ;
      ;=> Error: Can't spread atoms on stack. While executing: handler-case) ;
)|#
(cond
(error)
((eql (task-link task) :re-dfuncall)
(setf (task-logtime task)
;; hack! the delay is stored in (task-exectime task) (see re-dfuncall)
(setq logtime (+ (task-logtime task) (task-exectime task)))
(task-exectime task) (- logtime (task-advance task)))
(wait-task task))
(t
      ;;; (freelist% (task-arguments task)) ;GARBAGE
(setf (tcdr task) *free-tasks*
*free-tasks* task
(task-arguments task) nil)))))
|#
;; Inserts a task in a chain according to its exectime.

(defun insert-by-time (queue exectime task &aux (next-task (tcdr queue)))
  (cond
    ((null next-task)
     (setf (tcdr queue) task
           (tcdr task) nil))
    ((> (task-exectime next-task) exectime)
     (setf (tcdr task) next-task
           (tcdr queue) task))
    (t (insert-by-time next-task exectime task))))

;; =============================================================================-======

;; This routine needs to be called periodically to make the scheduler run.

(defvar *nbmax-ticks-per-event* 0)
(defvar *nbmax-tasks-per-event* 100)

(defun set-tasks-duration (ms)
  "Sets the maximum duration of time spent in evaluating tasks."
  (setf *nbmax-ticks-per-event*
        (max 0 (ceiling (* internal-time-units-per-second ms) 1000))))



#|
;; Safe version (could be replaced by the fast one, if it's safe enough!)

(defun check-scheduler (&aux task tmax nb-tasks)
(unless *internal-error?*
(handler-case
(cond
(*scheduler-RT?*
(setq tmax (+ (get-internal-real-time) *nbmax-ticks-per-event*)
nb-tasks *nbmax-tasks-per-event*)
(loop
(ready-tasks)
(when (setq task (next-ready-task *current-priority*))
(execute-task task))
(when (or (null task)
(<= (decf nb-tasks) 0)
(>= (get-internal-real-time) tmax))
(return))))
((eql *scheduler-state* :STEP)
(when (and (> *scheduler-steps* 0)
(setq task (next-ready-task *current-priority*)))
(decf *scheduler-steps*)
(execute-task task)))
(t
(setq tmax (+ (get-internal-real-time) *nbmax-ticks-per-event*)
nb-tasks *nbmax-tasks-per-event*)
(loop
(when (setq task (next-ready-task *current-priority*))
(execute-task task))
(unless (ready-tasks?)
(return
(when (eql *scheduler-state* :OOT1)
(set-scheduler-state *scheduler-old-state*))))
(when (or (<= (decf nb-tasks) 0)
(>= (get-internal-real-time) tmax))
(return)))))
(condition (condition)
(set-scheduler-state :STEP)
(setf *internal-error?* condition)
(format *error-output* "Internal condition ~S~%" condition))))
nil)
|#

;; Fast version (should replace the safe one, if it's safe enough!)

(defun check-scheduler (&aux task tmax nb-tasks)
  (cond
    (*scheduler-RT?*
     (setq tmax (+ (get-internal-real-time) *nbmax-ticks-per-event*)
           nb-tasks *nbmax-tasks-per-event*)
     (loop
       (ready-tasks)
       (when (setq task (next-ready-task *current-priority*))
         (execute-task task))
       (when (or (null task)
                 (<= (decf nb-tasks) 0)
                 (>= (get-internal-real-time) tmax))
         (return))))
    ((eql *scheduler-state* :STEP)
     (when (and (> *scheduler-steps* 0)
                (setq task (next-ready-task *current-priority*)))
       (decf *scheduler-steps*)
       (execute-task task)))
    (t
     (setq tmax (+ (get-internal-real-time) *nbmax-ticks-per-event*)
           nb-tasks *nbmax-tasks-per-event*)
     (loop
       (when (setq task (next-ready-task *current-priority*))
         (execute-task task))
       (unless (ready-tasks?)
         (return
           (when (eql *scheduler-state* :OOT1)
             (set-scheduler-state *scheduler-old-state*))))
       (when (or (<= (decf nb-tasks) 0)
                 (>= (get-internal-real-time) tmax))
         (return)))))
  nil)

;; (setq *eventhook* #'check-scheduler)
;; (setq *eventhook* nil)

;; Aborts activity past due.

(defun reset-scheduler ()
  "Erases the scheduler queues.  All the ready and waiting tasks are forgotten."
  (without-interrupts
    (setf *nb-waiting-tasks* 0)
    (dotimes (i *ready-queue-size*)
      (setf (svref *ready-queue-heads* i) nil
            (svref *ready-queue-tails* i) nil))
    (dotimes (i *wait-queue-size*)
      (setf (svref *wait-queue-heads* i) nil
            (svref *wait-queue-tails* i) nil))
    (setf *free-tasks* (make-task 0 0 0 0 #'no-op nil)
          *schedulertime--clocktime* 0
          *scheduler-time* (patchwork.midi:clock-time))
    nil))

;; Initializes the scheduler (called only once).
(defun init-scheduler ()
  (set-tasks-duration 100)
  (setf *default-task* (make-task 0 0 5 1 #'no-op nil))
  (reset-scheduler)
  (set-scheduler-state *scheduler-initial-state*)
  (setf *eventhook* #'check-scheduler))


(on-quit quit/scheduler (setq *eventhook* nil))
(on-load-and-now init/scheduler (init-scheduler))


#||
(defun bar (n) (repeat n (print 'bar)))
(start
(setq s2 (dfuncall 100 'bar 100))
(with-more-priority
(setq s1 (dfuncall 60 'princ 'foo))))
||#

;;;; THE END ;;;;
