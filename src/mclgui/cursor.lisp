;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               cursor.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Cursors.
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


(defun set-cursor (cursor)
  "
DO:             Sets the cursor to cursor.

CURSOR:         A cursor record or a 'CURS' resource ID.

NOTE:           If set-cursor is called from anywhere except within a
                WINDOW-UPDATE-CURSOR function, a function that is the
                value of *CURSORHOOK*, or a WITHOUT-INTERRUPTS special
                form, the event system’s background cursor handling
                immediately resets the cursor to some other shape.  If
                cursor is not of an acceptable type, then no action is
                taken.  To prevent the system from hanging at cursor
                update time, no error is signaled.
"
  (niy set-cursor cursor))


(defun update-cursor (&optional (hook *cursorhook*))
  "
The UPDATE-CURSOR function does the actual work of cursor handling.
If hook is a function or symbol, it is called with no arguments;
otherwise, SET-CURSOR is called with hook.

The UPDATE-CURSOR function is called periodically by the global
eventhandling system. It is not usually necessary to call this
function directly, but it may be called to make sure that the cursor
is correct at a particular time.

HOOK:           A function, symbol, or cursor. The default value is
                *CURSORHOOK*.
"
  (niy update-cursor hook))


(defmacro with-cursor (cursor &body body)
  "
The WITH-CURSOR macro executes zero or more forms with
*CURSORHOOK* bound to cursor.

CURSOR:         A cursor structure.
"
  (niy with-cursor cursor)
  `(progn ,@body)
  #-(and)
  (let ((cursor-var (gensym "cursor")))
    `(let ((,cursor-var ,cursor))       
       (unwind-protect       
            (let ((*cursorhook* ,cursor-var))
              (update-cursor)
              ,@body)
         (update-cursor)))))


(defun initialize/cursor ()
  (niy initialize/cursor))

;;;; THE END ;;;;
