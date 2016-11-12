;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pw-types.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;  
;;;;    This is an old typing scheme, superseeded in 911020. It is kept for compatibility
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
(enable-patchwork-reader-macros)

;; x , y , doc-string has to be given from outside 

(defclass C-pw-type ()
  ((control-form :initform nil :initarg :control-form :reader control-form)))

;;________________

(defvar *pw-object-type-list* ())

(defun push-to-object-types (pw-object-name)
  (push pw-object-name *pw-object-type-list*))

;;________________
;; numboxes

(defvar *fix-pw-type*
  (make-instance 'C-pw-type
  :control-form
   `(make-instance 'C-numbox  :view-size #@(36 14) :value 0 :min-val -9999 :max-val 999999
      :type-list '(fixnum))))

(defvar *fix-float-pw-type*
  (make-instance 'C-pw-type
  :control-form
   `(make-instance 'C-numbox  :view-size #@(36 14) :value 0 :min-val -9999 :max-val 999999
      :type-list '(fixnum float))))

(defvar *nil-numbox-pw-type*
  (make-instance 'C-pw-type
  :control-form
   `(make-instance 'C-numbox  :view-size #@(36 14) :value 0 :min-val -9999 :max-val 999999
      :type-list ())))

(defvar *positive-fix-pw-type*
  (make-instance 'C-pw-type
  :control-form
   `(make-instance 'C-numbox  :view-size #@(36 14) :value 100 :min-val 0 :max-val 999999
     :type-list '(fixnum))))

(defvar *positive-fix-list-pw-type*
  (make-instance 'C-pw-type
  :control-form
   `(make-instance 'C-numbox  :view-size #@(36 14) :value 100 :min-val 0 :max-val 999999
     :type-list '(fixnum list))))

(defvar *16-bit-fix-pw-type*
  (make-instance 'C-pw-type
  :control-form
   `(make-instance 'C-numbox  :view-size #@(36 14) :value 0 :min-val -32000 :max-val 32000
     :type-list '(fixnum))))

;; tty boxes
;; inputs are lists
(defvar *symbol-pw-type*
  (make-instance 'C-pw-type
  :control-form
   `(make-instance 'C-ttybox  :view-size #@(36 14) :dialog-item-text "list" :type-list '(list))))

(defvar *string-pw-type*
  (make-instance 'C-pw-type
  :control-form
   `(make-instance 'C-ttybox-str  :view-size #@(36 14) :dialog-item-text "name")))

;;==================================
;;for absout absin
(defvar *string-absout-pw-type*
  (make-instance 'C-pw-type
  :control-form
   `(make-instance 'C-ttybox-absout :view-size #@(36 14) :dialog-item-text "name")))

(defvar *string-absin-pw-type*
  (make-instance 'C-pw-type
  :control-form
   `(make-instance 'C-ttybox-absin :view-size #@(36 14) :dialog-item-text "name" :type-list '(no-connection))))

;;==================================
;; default global list
(defvar cl-user::lst '(0 1 2 3 4))
;; inputs are lists
(defvar *symbol-eval-pw-type*
  (make-instance 'C-pw-type
  :control-form
   `(make-instance 'C-ttybox-eval  :view-size #@(36 14) :dialog-item-text "lst" :type-list '(list))))

;; argboxes

(defvar *symbol-argfn-type*
  (make-instance 'C-pw-type
  :control-form
   `(make-instance 'C-ttybox  :view-size #@(36 14) :dialog-item-text "+" :type-list '(symbol))))

;; testboxes

(defvar *symbol-testfn-type*
  (make-instance 'C-pw-type
  :control-form
   `(make-instance 'C-ttybox  :view-size #@(36 14) :dialog-item-text "=" :type-list '(symbol))))

(defvar *symbol-test-type*
  (make-instance 'C-pw-type
  :control-form
   `(make-instance 'C-ttybox  :view-size #@(36 14) :dialog-item-text "0")))


;; MN + midibox
;;(setq *MN-midi-type*
;;  (make-instance 'C-pw-type
;;  :control-form
;;   `(make-instance 'C-ttybox  :view-size #@(36 14) :dialog-item-text "midi" :type-list '(midi))))

;; MN + collector-box
(defvar *MN-collector-type*
  (make-instance 'C-pw-type
  :control-form
   `(make-instance 'C-ttybox  :view-size #@(36 14) :dialog-item-text "coll" :type-list '(collector))))

(defvar *MN-midi-ins-type*
  (make-instance 'C-pw-type
  :control-form
   `(make-instance 'C-ttybox  :view-size #@(36 14) :dialog-item-text "m-ins" :type-list '(midi-ins))))


;; midi

(defvar *midi-pw-type*
  (make-instance 'C-pw-type
  :control-form
   `(make-instance 'C-numbox  :view-size #@(36 14) :value 60 :min-val 0 :max-val 127 :type-list '(fixnum))))

(defvar *midi-time-pw-type*
  (make-instance 'C-pw-type
  :control-form
   `(make-instance 'C-numbox  :view-size #@(36 14) :value 100 :min-val 1 :max-val 99999 :type-list '(fixnum))))

(defvar *midicent-pw-type*
  (make-instance 'C-pw-type
  :control-form
   `(make-instance 'C-numbox  :view-size #@(36 14) :value 6000 :min-val 0 :max-val 12700 
       :type-list '(fixnum list))))

(defvar *midi-pw-type-chan16*
  (make-instance 'C-pw-type
                 :control-form
                 `(make-instance 'C-numbox  :view-size #@(36 14) :value 1 :min-val 1 :max-val 16
                    :type-list '(fixnum))))

(defvar *midi-pw-type-chan32*
  (make-instance 'C-pw-type
                 :control-form
                 `(make-instance 'C-numbox  :view-size #@(36 14) :value 1 :min-val 1 :max-val 32
                    :type-list '(fixnum))))

(defvar *midi-ins-srte-pw-type*
  (make-instance 'C-pw-type  :control-form
    `(make-instance 'C-numbox
       :view-size (make-point 36 14) :value 1 :min-val 1 :max-val 100
       :type-list '(fixnum))))

;;___________________
;; set-stop-time box

(defvar *midi-pw-type-stop-time*
  (make-instance 'C-pw-type
          :control-form
           `(make-instance 'C-numbox  :view-size #@(36 14) :value 1000 :min-val 0 :max-val 99999
              :type-list '(fixnum))))

;;___________________
;; pw out + in  box

(defvar *pw-out-tty-type*
  (make-instance 'C-pw-type
          :control-form
           `(make-instance 'C-ttybox-out :view-size #@(40 14) :dialog-item-text "sym")))

(defvar *pw-in-tty-type*
  (make-instance 'C-pw-type
          :control-form
           `(make-instance 'C-ttybox-in-box  :view-size #@(40 14) :dialog-item-text "sym"
               :type-list '(no-connection))))

;;================================
(defvar *boolean-pw-type*
  (make-instance 'C-pw-type :control-form `(make-instance 'C-menubox-val  :view-size #@(36 14)
    :menu-box-list '(("T" . t) ("NIL". nil)) 
    :type-list '(boolean))))


(defvar *fix-float-list-pw-type*
  (make-instance 'C-pw-type
  :control-form
   `(make-instance 'C-numbox  :view-size #@(36 14) :value 0 :min-val -9999 :max-val 999999
      :type-list '(fixnum float list))))

;;=================================
;;???? who uses
;; pw-type for fixnum-float-list
;;(setq *fix-float-list-pw-type*
;;  (make-instance 'C-pw-type
;;  :control-form
;;   `(make-instance 'C-ttybox  :view-size #@(36 14) :dialog-item-text "0" 
;;      :type-list '(fixnum float list))))

;;;; THE END ;;;;
