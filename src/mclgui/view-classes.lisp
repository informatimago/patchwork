;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               view-classes.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    XXX
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-15 <PJB> 
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
(enable-sharp-at-reader-macro)

;;;---------------------------------------------------------------------

(defclass simple-view (colored wrapper fundamental-character-output-stream)
  ((help-spec            :initform nil         :initarg  :help-spec            :accessor help-spec)
   (view-container       :initform nil                                         :reader view-container
                         :documentation "The view that contains this view.")
   (view-position        :initform #@(0 0)     :initarg  :view-position        :reader view-position
                         :documentation "
The position of the view in its container, in the coordinate system of
the container.
")
   (view-size            :initform #@(100 100) :initarg  :view-size            :reader view-size
                         :documentation "The size of the view.")
   (view-scroll-position :initform #@(0 0)
                         :initarg :view-scroll-position
                         :accessor view-scroll-position
                         :documentation "
The current scroll position of the view, which is the coordinate of
the origin of the view, in the coordinate system of the container.
")
   (view-origin          :initform #@(0 0)
                         :accessor view-origin-slot
                         :documentation "
This is the vector to the origin of the view in the view coordinate
system, relative to the top-left corner of the view.  (The coordinate
of the top-left corner of the view, in the view coordinate system, is
the opposite of view-origin.)
")
   (view-nick-name       :initform nil         :initarg  :view-nick-name       :reader view-nick-name
                         :documentation "The nickname of the view.")
   (view-alist           :initform nil                                         :accessor view-alist)))


(defmethod print-object ((view simple-view) stream)
  (print-parseable-object
   (view stream :type t :identity t)
   (:view-position        (point-to-list (view-position view)))
   (:position/window      (point-to-list (convert-coordinates (view-position view)
                                                              view (view-window view))))
   (:view-size            (point-to-list (view-size view)))
   (:view-scroll-position (point-to-list (view-scroll-position view)))))


(defgeneric view-subviews (view)
  (:documentation "
RETURN:         A vector containing all of the view’s subviews.  This
                vector should never be changed directly.  It is
                updated automatically by calls to
                SET-VIEW-CONTAINER.
")
  (:method ((view simple-view))
    #()))


(defgeneric view-get (view key &optional default)
  (:documentation "
RETURN:         The property KEY of the VIEW.  If it is not present then return DEFAULT.
")
  (:method ((view simple-view) key &optional default)
    (let ((cell (assoc key (view-alist view))))
      (if cell
          (cdr cell)
          default))))


(defgeneric view-put (view key value)
  (:documentation "
DO:             Set the property KEY of the VIEW to VALUE.
RETURN:         VALUE.
")
  (:method ((view simple-view) key value)
    (let ((cell (assoc key (view-alist view))))
      (if cell
          (setf (cdr cell) value)
          (push (cons key value) (view-alist view)))
      value)))


(defgeneric (setf view-get) (value view key &optional default)
  (:documentation "
DO:             Set the property KEY of the VIEW to VALUE.
RETURN:         VALUE.
")
  (:method (value (view simple-view) key &optional default)
    (declare (ignore default))
    (view-put view key value)))


(defgeneric view-remprop (view key)
  (:documentation "
DO:             Remove the property KEY from the VIEW.
")
  (:method ((view simple-view) key)
    (setf (view-alist view)
          (delete key (view-alist view) :key (function car)))
    view))


;;;---------------------------------------------------------------------

(defclass view (simple-view)
  ((view-subviews        :initform nil
                         :initarg :view-subviews
                         :initarg :subviews
                         :reader   view-subviews
                         ;; Note: the vector is initialized in initialize-instance.
                         :documentation "A vector of subviews.")
   (view-valid           :initform nil
                         :accessor view-valid) ; for lazy clip-region updating.
   (view-clip-region     :initform nil
                         :accessor view-clip-region-slot)))



;;;---------------------------------------------------------------------

(defvar *next-window-ptr* 0)

(defclass window (view)
  ((window-cursor                    :reader    window-cursor
                                     :initform  *arrow-cursor*)
   (view-pen                         :reader    view-pen
                                     :initform  (make-instance 'pen-state))
   (window-grow-rect                 :reader    window-grow-rect
                                     :initform  nil)
   (window-drag-rect                 :reader    window-drag-rect
                                     :initform  nil)
   (color-list                       :reader    window-color-list
                                     :initform  nil)
   (back-color                       :initform  nil)
   (my-item                          :initform  nil)
   (window-do-first-click            :accessor  window-do-first-click
                                     :initarg   :window-do-first-click
                                     :initform  nil)
   (window-other-attributes          :accessor  window-other-attributes
                                     :initarg   :window-other-attributes
                                     :initform  0) 
   (window-active-p                  :accessor  window-active-p
                                     :initform  nil)
   ;; The window-active-p generic function returns t if window is the active
   ;; window, nil otherwise.
   ;; Except when Macintosh Common Lisp is not the active application, it returns
   ;; t for all floating windows and for the frontmost non-floating visible window.
   
   (window-erase-region              :accessor  window-erase-region
                                     :initform  (new-rgn))
   (window-invalid-region            :initform  nil                          
                                     :accessor  window-invalid-region)
   (process                          :accessor  window-process
                                     :initarg  :process
                                     :initform  nil)
   (queue                            :reader    window-process-queue
                                     :initform  nil) ; (make-process-queue "Window")
   (auto-position                    :initarg   :auto-position               
                                     :initform  :noAutoCenter
                                     :type      (member nil
                                                        :noAutoCenter
                                                        :alertPositionParentWindow
                                                        :centerMainScreen
                                                        :staggerParentWindow
                                                        :alertPositionMainScreen
                                                        :centerParentWindowScreen
                                                        :staggerMainScreen
                                                        :alertPositionParentWindowScreen
                                                        :centerParentWindow
                                                        :staggerParentWindowScreen))
   (window-title                     :reader     window-title
                                     :initarg    :window-title
                                     :initform   "Untitled")
   (visiblep                         :reader     window-visiblep
                                     :initarg    :visiblep
                                     :reader     window-shown-p
                                     :initarg    :window-show
                                     :initform   t)
   (colorp                           :reader     window-colorp
                                     :initarg    :colorp
                                     :initarg    :color-p
                                     :initform   nil)
   (close-box-p                      :reader     window-close-box-p
                                     :initarg   :close-box-p
                                     :initform   t)
   (grow-icon-p                      :reader     window-grow-icon-p
                                     :initarg   :grow-icon-p
                                     :initform   nil)
   (window-layer                     :initarg   :window-layer
                                     :type       integer
                                     :initform   0)
   (theme-background                 :accessor   window-theme-background 
                                     :accessor   theme-background
                                     :initarg    :theme-background
                                     :initform   nil)
   (window-prior-theme-drawing-state :accessor   window-prior-theme-drawing-state
                                     :initform   nil)
   (window-type                      :accessor   window-type
                                     :initform   :document-with-zoom
                                     :type       (member :document
                                                         :document-with-grow
                                                         :document-with-zoom
                                                         :double-edge-box
                                                         :single-edge-box
                                                         :shadow-edge-box
                                                         :tool))
   (erase-anonymous-invalidations    :initform   t
                                     :initarg   :erase-anonymous-invalidations)
   ;; WINDOW-PTR is a simulated handle (a mere integer identifying the window).
   ;; It's reset to NIL when the real window handle is released.
   (window-ptr                       :reader window-ptr
                                     :initform (incf *next-window-ptr*))))


(defgeneric view-allocate-clip-region (window))


(defmethod print-object ((window window) stream)
  (print-parseable-object (window stream :type t :identity t)
                          (:title (ignore-errors (window-title window)))
                          (:view-position (point-to-list (view-position window)))
                          (:view-size (point-to-list (view-size window)))))


;;;---------------------------------------------------------------------


(defclass windoid (window)
  ((show-on-resume-p :initform nil))
  (:default-initargs :window-title "" :window-do-first-click t :window-type :windoid))

(defgeneric windoid-p (window)
  (:method ((self t))       nil)
  (:method ((self windoid)) t))


;;;---------------------------------------------------------------------


(define-condition view-error (simple-error)
  ((view :initarg :view :reader view-error-view))
  (:report (lambda (err stream)
             (format stream "~?"
                     (simple-condition-format-control err)
                     (simple-condition-format-arguments err)))))


(define-condition view-size-error (view-error)
  ((size :initarg :size :reader view-error-size)))


;;;; THE END ;;;;
