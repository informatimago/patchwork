;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               ui.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements user interface primitives that were available in MCL.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-04-07 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    Proprietary
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
;;;;    
;;;;    All Rights Reserved.
;;;;    
;;;;    This program and its documentation constitute intellectual property 
;;;;    of Pascal J. Bourguignon and is protected by the copyright laws of 
;;;;    the European Union and other countries.
;;;;**************************************************************************

(in-package "PW-MACOSX")

(defvar *lisp-cleanup-functions* '())

;; (defstruct (point
;;              (:constructor make-point (x y)))
;;   x y)
;; 
;; (defmethod make-load-form ((p point) &optional env)
;;   `(make-point ,(point-x p) ,(point-y p)))

(defun make-point (x &optional y)
  (if y
      (dpb (ldb (byte 16 0) x) (byte 16 16) (ldb (byte 16 0) y))
      x))
(defun point-x (p) (ldb (byte 16  0) p))
(defun point-y (p) (ldb (byte 16 16) p))
(defun point-h (p) (point-x p))
(defun point-v (p) (point-y p))

(defun add-points (a b)
  (make-point (+ (point-x a) (point-x b))
              (+ (point-y a) (point-y b))))

(defun subtract-points (a b)
  (make-point (- (point-x a) (point-x b))
              (- (point-y a) (point-y b))))

(declaim (inline point-x point-y point-h point-v add-points subtract-points))
(defun point-to-list (p) (list (point-x p) (point-y p)))


(defun message-dialog (message &rest args)
  (format *query-io* "~&~72,,,'-<~>~%Message: ~?~%~72,,,'-<~>~%" message args)
  (force-output  *query-io*))


(defun choose-new-file-dialog (&key
                               (directory *default-pathname-defaults*)
                               (prompt "Save a new file")
                               (button-string "Save file"))
  (format *query-io*
          "~&~72,,,'-<~>~A~2%Directory: ~A~%Please enter a new file name (~A): "
          prompt
          (make-pathname :name nil :type nil :version nil :defaults directory)
          button-string)
  (force-output *query-io*)
  (let ((file-name (read-line *query-io*)))
    (merge-pathnames file-name directory nil)))


(defun columnify (stream list &optional (width 80))
  (let* ((maxwidth (1+ (reduce (function max) list :key (function length))))
         (ncolumns (floor width maxwidth))
         (nrows    (ceiling (length list) ncolumns))
         (columns  (loop
                     :for i :from 0 :by nrows :below (length list)
                     :collect (subseq list i (min (length list) (+ i nrows))))))
    (loop
      :while (and columns (some (function identity) columns))
      :initially (terpri stream)
      :do (loop
            :for cells :in columns
            :do (format stream "~V<~@[~A~]~;~;~>" maxwidth (car cells))
            :finally (format stream "~%"))
      :finally (terpri stream)
      :do (setf columns (mapcar (function cdr) columns)))))


(defun choose-file-dialog (&key
                           (directory *default-pathname-defaults*)
                           (prompt "Open a file")
                           (button-string "Open file"))
  (format *query-io*
          "~&~72,,,'-<~>~%~A~2%Directory: ~A~%"
          prompt
          (make-pathname :name nil :type nil :version nil :defaults directory))
  (columnify *query-io* (mapcar 'file-namestring
                                (directory (make-pathname :name :wild :type :wild :version nil
                                                          :defaults directory))))
  (format *query-io* "Please select a file name (~A): " button-string)
  (force-output *query-io*)
  (let ((file-name (read-line *query-io*)))
    (merge-pathnames file-name directory nil)))


;;;---------------------------------------------------------------------
;;; Cursors
;;;---------------------------------------------------------------------

(defvar *watch-cursor*  nil)
(defvar *i-beam-cursor* nil)
(defvar *arrow-cursor*  nil)

(defvar *current-cursor* nil)

(defmacro with-cursor (cursor &body body)
  `(let ((*current-cursor* ,cursor))
     ,@body))

;;;---------------------------------------------------------------------
;;; Patterns
;;;---------------------------------------------------------------------

(defvar *black-pattern*  nil)
(defvar *dkgray-pattern* nil)
(defvar *gray-pattern*   nil)
(defvar *ltgray-pattern* nil)
(defvar *white-pattern*  nil)

;;;---------------------------------------------------------------------
;;; Views
;;;---------------------------------------------------------------------


(defclass simple-view ()
  ((view-window    :initarg :view-window    :accessor view-window    :initform nil)
   (view-container :initarg :view-container :accessor view-container :initform nil)
   (view-position  :initarg :view-position  :accessor view-position  :initform (make-point 0 0))
   (view-size      :initarg :view-size      :accessor view-size      :initform (make-point 100 100))
   (view-font      :initarg :view-font      :accessor view-font      :initform '("Monaco" 9 :srcor :plain))))


(defmethod set-view-position (view h &optional v)
  (if v
      (setf (view-position view) (make-point h v))
      (setf (view-position view) h))
  view)

(defmethod set-view-size (view h &optional v)
  (if v
      (setf (view-size view) (make-point h v))
      (setf (view-size view) h))
  view)

(defmethod set-view-font (view font)
  (setf (view-font view) font)
  view)



(defclass view (simple-view)
  ((subviews       :initarg :view-subviews  :accessor view-subviews  :initform '()
                   :initarg :subviews       :accessor subviews)
   (bottom-boarder :initarg :bottom-boarder :accessor bottom-boarder :initform nil)
   (v-scrollp      :initarg :v-scrollp      :accessor v-scrollp      :initform nil)
   (h-scrollp      :initarg :h-scrollp      :accessor h-scrollp      :initform nil)
   (track-thumb-p  :initarg :track-thumb-p  :accessor track-thumb-p  :initform nil)))


(defvar *view-indent* 0)

(defmethod print-object ((self view) stream)
  (format stream "~V<~>~S ~S~%"
          *view-indent*
          (class-name (class-of self))
          (list (point-to-list (view-position self))
                (point-to-list (view-size self))))
  (when (view-subviews self)
    (format stream "~V<~>  subviews:~%" *view-indent*)
    (let ((*view-indent* (+ 4 *view-indent*)))
      (dolist (subview (view-subviews self))
        (print subview stream))))
  self)


(defmethod remove-subview ((self view) (subview simple-view))
  (alexandria:deletef (view-subviews self) subview)
  (when (eq self (view-container subview))
    (setf (view-container subview) nil
          (view-window    subview) nil))
  subview)

(defmethod add-subview ((self view) (subview simple-view))
  (when (view-container subview)
    (remove-subview (view-container subview) subview))
  (setf (view-container subview) self
        (view-window    subview) (view-window self))
  (push subview (view-subviews self))
  subview)


(defmethod remove-subviews (self &rest subviews)
  (dolist (subview subviews self)
    (remove-subview self subview)))

(defmethod add-subviews (self &rest subviews)
  (dolist (subview subviews self)
    (add-subview self subview)))



(defclass control (view)
  ())

(defclass scroller (control)
  ((outline :initarg :scroller-outline :accessor scroller-outline :initform nil)))

(defclass scroller-mixin ()
  ((v-scroller             :initarg :v-scroller             :accessor v-scroller            :initform nil)
   (h-scroller             :initarg :h-scroller             :accessor h-scroller            :initform nil)
   (scroll-bar-correction  :initarg :scroll-bar-correction  :accessor scroll-bar-correction :initform nil)))

(defvar *current-view* nil)
(defvar *current-font* nil)

(defmacro with-focused-view (view &body body)
  `(let ((*current-view* ,view))
     ,@body))

(defmacro with-font-focused-view (view font &body body)
  `(let ((*current-view* ,view)
         (*current-font* ,font))
     ,@body))





;;;---------------------------------------------------------------------
;;; Windows
;;;---------------------------------------------------------------------

(defvar *window-list* '())

(defclass window ()
  ((window-kind   :initarg :window-kind         :accessor window-kind         :initform 0)
   (visiblep      :initarg :visiblep            :accessor visiblep            :initform t)
   (hilitedp      :initarg :hilitedp            :accessor hilitedp            :initform nil)
   (close-box-p   :initarg :close-box-p         :accessor close-box-p         :initform t)
   (window-title  :initarg :window-title        :accessor window-title        :initform "Untitled")
   (view-position :initarg :view-position       :accessor view-position       :initform (make-point 0 0))
   (view-size     :initarg :view-size           :accessor view-size           :initform (make-point 100 100))
   (content-view  :initarg :window-content-view :accessor window-content-view :initform nil)))


(defmethod initialize-instance :after ((self window) &key &allow-other-keys)
  (push self *window-list*)
  self)


(defmethod print-object ((self window) stream)
  (let ((title-bar (make-string 72 :initial-element #\-))
        (winlen    (min 56 (length (window-title self)))))
    (when (close-box-p self) (replace title-bar "[]" :start1 3))
    (replace title-bar (window-title self)
             :start1 (truncate (- (length title-bar) winlen) 2)
             :end2 winlen)
    (format stream "~A~%" title-bar))
  (format stream "~V<~>~S ~S ~S~%"
          *view-indent*
          (class-name (class-of self))
          (window-title self)
          (list (point-to-list (view-position self))
                (point-to-list (view-size self))))
  (when (window-content-view self)
    (format stream "~V<~>  contents:~%" *view-indent*)
    (let ((*view-indent* (+ 4 *view-indent*)))
      (print (window-content-view self) stream)))
  (format stream "~72,,,'-<~>~%")
  self)


;; (defclass C-ttybox (static-text-dialog-item) 
;;   ((open-state :initform t  :initarg :open-state :accessor open-state)
;;    (doc-string :initform "" :initarg :doc-string  :accessor doc-string)
;;    (value :initform nil :initarg :value)
;;    (type-list :initform ()  :initarg :type-list :accessor type-list)))



(defclass fred-window (window)
  ())


(defmethod set-window-title (window font)
  (setf (window-title window) font)
  window)


(defmethod create-content-view ((self window))
  (setf (window-content-view self) (make-instance 'view
                                       :view-window self
                                       :view-size (view-size self)
                                       :view-position (view-position self))))

(defmethod subviews ((self window))
  (unless (window-content-view self) (create-content-view self))
  (subviews (window-content-view self)))

(defmethod remove-subview ((self window) (subview simple-view))
  (unless (window-content-view self) (create-content-view self))
  (remove-subview (window-content-view self) subview))

(defmethod add-subview ((self window) (subview simple-view))
  (unless (window-content-view self) (create-content-view self))
  (add-subview (window-content-view self) subview))


(defmethod bring-to-front ((self window))
  (alexandria:deletef *window-list* self)
  (push self *window-list*)
  self)

(defmethod window-select ((self window))
  (dolist (window *window-list*) (setf (hilitedp window) nil))
  (setf (hilitedp self) t)
  (bring-to-front self)
  (progn
   (terpri *trace-output*)
   (print self *trace-output*)
   (terpri *trace-output*)
   (finish-output *trace-output*))
  (values))

(defun front-window ()
  (first *window-list*))


(defun find-visible-window ()
  (find-if (function visiblep) *window-list*))


(defmethod window-show ((self window))
  (unless (visiblep self)
   (setf (visiblep self) t))
  self)


(defmethod window-hide ((self window))
  (when (visiblep self)
    (setf (visiblep self) t)
    (when (eq (front-window) self)
      (let ((visible  (find-visible-window)))
        (when visible
          (bring-to-front visible)))))
  self)


(defmethod window-close ((self window))
  (window-hide self)
  self)


(defmethod view-activate-event-handler ((self window))
  self)

(defmethod view-deactivate-event-handler ((self window))
  self)


;;;---------------------------------------------------------------------
;;; Dialogs
;;;---------------------------------------------------------------------

(defclass dialog-item (view)
  ((dialog-item-action :initarg :dialog-item-action :accessor dialog-item-action :initform nil)))

(defmethod set-dialog-item-text ((di dialog-item) text)
  (setf (dialog-item-text di) text)
  di)

(defclass static-text-dialog-item (dialog-item)
  ((dialog-item-text :initarg :dialog-item-text :accessor dialog-item-text :initform "")))

(defclass editable-text-dialog-item (static-text-dialog-item)
  ())

(defclass table-dialog-item (dialog-item)
  ((table-dimensions :initarg :table-dimensions :accessor table-dimensions :initform (make-point 100 100))
   (cell-size        :initarg :cell-size        :accessor cell-size        :initform (make-point 10 10))))

(defclass button-dialog-item (static-text-dialog-item)
  ((default-button :initarg :default-button :initform nil)))

(defclass radio-button-dialog-item (button-dialog-item)
  ())

(defclass check-box-dialog-item (button-dialog-item)
  ())


(defclass dialog (window)
  ())

(defun y-or-n-dialog (query)
  (format *query-io* "~72,,,'-<~>~%")
  (loop
    (format *query-io* "~A (Y/N): ")
    (finish-output *query-io*)
    (let ((answer (string-upcase (string-trim " " (read-line *query-io*)))))
      (case answer
        ((#\Y) (return t))
        ((#\N) (return nil))
        (otherwise (format *query-io* "Please type Y or N.~%"))))))


;;;---------------------------------------------------------------------
;;; Menus
;;;---------------------------------------------------------------------

(defclass menu-element ()
  ((title       :initarg :menu-title   :accessor menu-title     :initform "Menu")
   (enabled     :initarg :menu-enabled :accessor menu-enabled-p :initform t)))

(defgeneric menu-element-p (object)
  (:method ((object t))            nil)
  (:method ((object menu-element)) t))

(defclass menu-item (menu-element)
  ((title       :initarg :menu-item-title        :accessor menu-item-title      :initform "Item")
   (enabled     :initarg :menu-item-enabled      :accessor menu-enabled-p       :initform t)
   (action      :initarg :menu-item-action       :accessor menu-item-action     :initform nil
                :accessor menu-item-action-function)
   (command-key :initarg :menu-item-command-key  :accessor command-key          :initform nil)
   (check-mark  :initarg :menu-item-check-mark   :accessor menu-item-check-mark :initform nil)))

(defgeneric menu-item-p (object)
  (:method ((object t))         nil)
  (:method ((object menu-item)) t))


(defmethod menu-item-enable ((mi menu-item))
  (setf (menu-enabled-p mi) t)
  mi)

(defmethod menu-item-disable ((mi menu-item))
  (setf (menu-enabled-p mi) nil)
  mi)


(defmethod set-command-key ((mi menu-item) key)
  (setf (command-key mi) key)
  mi)

(defmethod set-menu-item-check-mark ((mi menu-item) mark)
  (setf (menu-item-check-mark mi) mark)
  mi)

(defmethod set-menu-item-action-function ((mi menu-item) action)
  (setf (menu-item-action mi) action)
  mi)





(defclass menu (menu-element)
  ((items       :initarg :menu-items   :accessor menu-items     :initform '())))

(defgeneric menup (object)
  (:method ((object t))    nil)
  (:method ((object menu)) t))


(defmethod add-menu-items ((menu menu) &rest items)
  (assert (every (function menu-element-p) items))
  (alexandria:appendf (menu-items menu) items))


(defun find-menu (title)
  (find title *menubar*
        :key (function menu-title)
        :test (function string=)))

(defun find-menu-item (menu title)
  (find title (menu-items menu)
        :key (function menu-item-title)
        :test (function string=)))


(defparameter *menubar*
  (list (make-instance 'menu
            :menu-title "Lisp"
            :menu-items (list  (make-instance 'menu-item
                                  :menu-item-title "Abort"
                                  :menu-item-action (lambda () (invoke-restart 'abort)))
                                (make-instance 'menu-item
                                  :menu-item-title "Break"
                                  :menu-item-action (lambda () (invoke-restart 'break)))
                                 (make-instance 'menu-item
                                  :menu-item-title "Continue"
                                  :menu-item-action (lambda () (invoke-restart 'continue)))
                                 (make-instance 'menu-item
                                  :menu-item-title "Force Quit"
                                  :menu-item-action (lambda () (ccl:quit)))))))


(defun menubar () (copy-list *menubar*))

(defun set-menubar (ui:menubar) 
  (check-type menubar list)
  (assert (every (function menup) menubar))
  (setf *menubar* (copy-list menubar))
  menubar)


;;;---------------------------------------------------------------------
;;; Miscellaneaous
;;;---------------------------------------------------------------------

(defun make-popupbox (title view menu
                            &rest args
                            &key (view-position (make-point 10 10))
                            view-container
                            view-font
                            &allow-other-keys)
  (warn "~S is not implemented, called with ~S" 'make-popupbox args)
  (make-instance 'view))



(defun pw::mkso (a b c patch-path)
  (warn "~S called while scripting is disabled." 'pw::mkso)
  ;; see pw-lib/pwscript/pw-scripting.lisp
  '(:|cpat|
    nil
    :|name|
    "/home/pjb/works/patchwork/addition-et-multiplication.pwpatch")
  nil) 

(defun pw::record--ae (&rest args)
  (warn "~S called while scripting is disabled." 'pw::record--ae)
  '(:|aevt|
    :|odoc|
    ((:----
      nil)))
  nil)


(defun double-click-p () nil)
(defun mouse-down-p   () nil)
(defun command-key-p  () nil)
(defun control-key-p  () nil)
(defun option-key-p   () nil)
(defun shift-key-p    () nil)

(defun key-equal (key name)
  (etypecase name
    (character (equal key name))
    (symbol    nil)))

(defun wptr (x) x)

(defmacro def-load-pointers (name lambda-list &body body)
  (declare (ignore name lambda-list body))
  (warn "~S not implemented" 'def-load-pointers))


;;;; THE END ;;;;
