;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               radio-button-dialog-item.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Radio Button Dialog Item.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-19 <PJB> Created.
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


(defclass radio-button-dialog-item (control-dialog-item)
  ((width-correction      :allocation :class
                          :initform 20)
   (procid                :allocation :class
                          :initform 0 ; #.(+ #$RadioButProc #$kControlUsesOwningWindowsFontVariant)
                          )
   (radio-button-cluster  :initarg :radio-button-cluster
                          :initform 0
                          :accessor radio-button-cluster
                          :documentation "
The cluster to which the radio button belongs. Only one
button from a given cluster can be pushed at a time.
Whenever the user clicks a button, the function radiobutton-
unpush is applied to all other buttons having
the same value for radio-button-cluster. To check
to see whether two buttons are in the same cluster, use
eq. The default cluster is 0.
")
   (radio-button-pushed-p :initarg  :radio-button-pushed-p
                          :initform nil
                          :accessor radio-button-pushed-p
                          :documentation "
The radio-button-pushed-p generic function returns T if the radio
button is pushed and NIL if it is not.  The default value
is NIL.
"))
  (:documentation "
Radio buttons are small circles that contain a black dot when they are
selected (“pushed”). Radio buttons occur in clusters, and only one
button in a cluster may be pushed at a time. Clicking a radio button
unpushes the previously pushed one. The following class and functions
govern the behavior of radio buttons.
"))


(defmethod view-default-font ((view radio-button-dialog-item))
  (sys-font-spec))


(defmethod view-default-size ((item radio-button-dialog-item))
  (let ((size (call-next-method)))
    (make-point (point-h size) (max 16 (point-v size)))))


(defmethod install-view-in-window ((item radio-button-dialog-item) dialog
                                   &aux (first t))
  (declare (ignore dialog))
  (without-interrupts
      (let ((cluster (radio-button-cluster item))
            (container (view-container item)))
        (do-dialog-items (other-item container 'radio-button-dialog-item)
          (when (and (not (eq item other-item))
                     (eq cluster (radio-button-cluster other-item)))
            (return (setq first nil)))))
    (call-next-method) ;this is failing to do it upon return
    (when (or first (radio-button-pushed-p item))
      (radio-button-push item))))


(defmethod dialog-item-action ((item radio-button-dialog-item))
  (radio-button-push item)
  (call-next-method))                   ; dispatch to user's dialog-item-action code.



(defgeneric pushed-radio-button (view &optional cluster)
  (:documentation "
The pushed-radio-button generic function returns the pushed radio
button from the specified cluster.  The value NIL is returned if there
is no such cluster or if all the radio buttons in a cluster are
disabled.

WINDOW:         A window.

CLUSTER:        The cluster of radio buttons to search. Radio button
                clusters are numbered, starting with 0. The default is
                0.
")
  (:method ((view view) &optional (cluster 0))
    (dovector (item (view-subviews view))
              (when (and (typep item 'radio-button-dialog-item)
                         (= cluster (slot-value item 'radio-button-cluster))
                         (radio-button-pushed-p item))
                (return-from pushed-radio-button item)))))


(defgeneric radio-button-push (item)
  (:documentation "
The radio-button-push generic function pushes a radio button and
unpushes the previously pushed one.  The function merely toggles the
states of the two radio buttons; it does not run any action. The function
returns NIL.

ITEM:           A radio-button dialog item.
")
  (:method ((item radio-button-dialog-item))
    (let ((cluster      (radio-button-cluster item))
          (container    (view-container item))
          ;; (handle (dialog-item-handle item))
          )
      (when container
        (do-dialog-items (other-item container 'radio-button-dialog-item)
          (when (and (not (eq other-item item))
                     (eq (radio-button-cluster other-item) cluster))
            (radio-button-unpush other-item)))
        (niy radio-button-push item)
        ;; (when (and handle (installed-item-p item))
        ;;   (with-focused-dialog-item (item container)
        ;;     (#_SetControlValue (dialog-item-handle item) 1)))
        )
      (setf (radio-button-pushed-p item) t))))


(defgeneric radio-button-unpush (item)
  (:documentation "
The radio-button-unpush generic function unpushes the radio button
and returns NIL.

ITEM:           A radio-button dialog item.
")
  (:method ((item radio-button-dialog-item))
    (niy radio-button-unpush item)
    ;; (let ((handle (dialog-item-handle item)))
    ;;   (when (and handle (installed-item-p item))
    ;;     (with-focused-dialog-item (item)
    ;;       (#_SetControlValue (dialog-item-handle item) 0))))
    (setf (radio-button-pushed-p item) nil)))


;;;; THE END ;;;;
