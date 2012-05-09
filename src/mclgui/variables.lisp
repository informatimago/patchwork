;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               variables.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines the global variables of the MCLGUI.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-09 <PJB> Created.
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


(defvar *font-list* '()
  "Contains a list of all the fonts installed in the
current Macintosh Operating System, sorted alphabetically.")



(defparameter *pen-modes*
  '(:srcCopy    :srcOr    :srcXor    :srcBic
    :notSrcCopy :notSrcOr :notSrcXor :notsrcbic
    :patCopy    :patOr    :patXor    :patBic
    :notPatCopy :notPatOr :notPatXor :notPatBic)
  "Pen modes as integers.  These integers match the zero-based numeric
position of the keyword in the *PEN-MODES* list.  So, for example, the
number of :SRCOR pen mode could be coded as (POSITION :SRCOR *PEN-MODES*).
The inverse operation (turning a pen-mode integer into
a keyword) can be performed with the Common Lisp function ELT.")


(defparameter *text-modes*
  '(:blend :addPin :addOver :subPin :transparent
    :addMax :subOver :addMin :grayishTextOr
    :hilite :ditherCopy))



(defparameter *transfer-modes*
  ;; NOTE: the encoding for text-mode may not have been that, there
  ;;       was #$subOver etc constants.
  (let ((modes  (append *pen-modes* *text-modes*)))
    (make-array (length modes) :initial-contents modes)))


(define-condition unknown-transfer-mode (error)
  ((mode :initarg :mode :accessor unknown-transfer-mode))
  (:report (lambda (err stream)
             (format stream "Unknown transfer mode ~S"
                     (unknown-transfer-mode err)))))

(defun xfer-mode-arg (name &optional error-p)
  (or (position name *transfer-modes*)
      (when error-p (error 'unknown-transfer-mode :mode name))
      0)) ; :srcCopy by default.


(define-condition unknown-transfer-value (error)
  ((value :initarg :value :accessor unknown-transfer-value))
  (:report (lambda (err stream)
             (format stream "Unknown transfer mode value ~S"
                     (unknown-transfer-value err)))))

(defun xfer-mode-to-name (mode)
  (if (array-in-bounds-p *transfer-modes* mode)
      (aref *transfer-modes* mode)
      (error 'unknown-transfer-value :value mode)))



(defparameter *style-alist* 
  '((:plain     . 0)
    (:bold      . 1)
    (:italic    . 2)
    (:underline . 4)
    (:outline   . 8)
    (:shadow    . 16)
    (:condense  . 32)
    (:extend    . 64))
  "An association list of font-style keywords and numbers that the
Macintosh computer uses to encode these styles.  The Macintosh
Operating System encodes styles as a byte, with each style represented
by a bit (this encoding allows multiple styles).  You can derive a byte
to pass to the Macintosh computer by adding the numbers corresponding
to the styles listed here.")


(defvar *black-pattern*  nil)
(defvar *dkgray-pattern* nil)
(defvar *gray-pattern*   nil)
(defvar *ltgray-pattern* nil)
(defvar *white-pattern*  nil)



(defparameter *screen-width*  512
  "Width in pixels of the main screen.")
(defparameter *screen-height* 342
  "Height in pixels of the main screen.")

(defparameter *pixels-per-inch-x* 72
  "Number of pixels per inch in the horizontal direction on the main screen.")
(defparameter *pixels-per-inch-y* 72
  "Number of pixels per inch in the vertical direction on the main screen.")



;;;; THE END ;;;;
