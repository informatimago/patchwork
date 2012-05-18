;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               eval.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    MCL Read Eval Print Loop.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-18 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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

(in-package "MCLGUI")


(defun eval-enqueue (form)
  "
The EVAL-ENQUEUE function queues up form for evaluation in the
read-eval-print loop.  The eval-enqueue function returns immediately.
This means that form is not executed at the event-handling level but
instead is executed as if it had been entered into the Listener.  (It
is executed only when other forms entered into the Listener or queued
up have returned.)

This function is useful for initiating programs from within event
handlers.  The form is executed as part of the normal read-eval-print
loop rather than as part of an event handler.  This means that other
events can be processed during the execution of form.

Note that EVAL-ENQUEUE is a function, and so its argument is
evaluated.  The result of this evaluation is put into the
read-eval-print loop.
"
  (niy eval-enqueue form))


(defun get-next-queued-form ()
  "
The GET-NEXT-QUEUED-FORM function returns the next form from the
pending queue or returns NIL if there are no forms pending. A second
value returned is T if there was a pending form and NIL if there was
no pending form.

During programming sessions, queued-up forms include text entered in
the Listener and evaluated from buffers as well as forms passed to
EVAL=ENQUEUE.
"
  (niy get-next-queued-form))



(defun initialize-eval ()
  (niy initialize-eval))

;;;; THE END ;;;;

