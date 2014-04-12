;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               abstraction-m.lisp
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
;;;;    2014-04-10 <PJB> Removed useless declare special.
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

;; CLASS: C-abstract-M,  for patch abstraction, with pop-up-menu-attached
;; METHODS:
;; User-Menu-Include,    creates a menu item for instanciating objects of the abstraction type.

(defpackage "USER-ABSTRACTION")

(defvar *active-config-object* ())  ;the patch holding the current configuration

(defvar *abstract-popUpMenu*
  (new-menu " "
            (new-leafmenu "Open" (lambda () (open-patch-win *target-action-object*)))
            (new-leafmenu "Tomenu" (lambda () (user-menu-include *target-action-object*)))
            (new-leafmenu "No Menu" (lambda () (user-menu-remove *target-action-object*)))
            (new-leafmenu "Redraw" (lambda () (redraw-patch *target-action-object*)))
            (new-leafmenu "Save" (lambda () (save *target-action-object*)))
            (new-leafmenu "Compile" (lambda () (compile-me *target-action-object*
                                                           *target-action-object*)))))

(defclass C-abstract-M (C-abstract)
  ((popUpBox :accessor popUpBox))) 
;;;;

(defmethod initialize-instance :after ((self C-abstract-M ) &key controls)
  (declare (ignore controls))
  (setf (popUpBox self) 
        (make-popUpbox "" self
                       *abstract-popUpMenu*
                       :view-position (make-point (- (w self) 10)
                                                  (- (h self) 14))
                       :view-container self
                       :view-font '("monaco"  9  :srcor))))

(defvar *abs-code* ())
(defgeneric user-menu-include (self)
  (:method ((self C-abstract-M))
    (let* ((box-title (window-title (patch-win self)))
           (menu (find-menu-item *pw-menu-patch* box-title)))
      (unless menu
        (add-to-configuration  box-title)
        (setf *abs-code* (decompile self))
        (ui:add-menu-items  *pw-menu-patch*
                            (new-leafmenu box-title
                                          (eval 
                                           `(function (lambda ()
                                              (let ((box ,*abs-code*))
                                                (set-view-position box 15 15)
                                                (setf (pw-function-string box) ,box-title)
                                                (add-patch-box *active-patch-window* box))))))  )))))

(defgeneric user-menu-remove (self)
  (:method ((self C-abstract-M))
    (let* ((box-title (window-title (patch-win self)))
           (menu (find-menu-item *pw-menu-patch* box-title)))
      (when menu
        (remove-from-configuration  box-title)
        (remove-menu-items *pw-menu-patch* menu) ) ) ))

(defvar *current-user-configuration* ())

(defun add-to-configuration(title)
  (setf *current-user-configuration* (cons title *current-user-configuration*)) )

(defun remove-from-configuration(title)
  (setf *current-user-configuration* (remove title *current-user-configuration* :test #'string=)) )

(defgeneric rename-yourself (self str)
  (:method ((self C-abstract-M) str)
    (setf (pw-function self) str) ))


(defgeneric redraw-patch (self))
(defmethod redraw-patch ((self C-abstract-M))
  (let* ((win (patch-win self))
         (patches (controls win))
         (out-box (car (find-abstract-out-box win patches)))
         (in-boxes (find-abstract-in-boxes win patches))
         (title (window-title win))
         (in-docs-temp)
         (in-put-docs 
           (when in-boxes 
             (setq in-docs-temp (get-absin-boxes-types-n patches in-boxes))
             (cond 
               ((member nil in-docs-temp)
                (ui:message-dialog 
                 "WARNING! absin box connected to irreducible types. ALL-type used")
                (mapcar (lambda (type-spec) (declare (ignore type-spec)) '(nilNum))
                        in-docs-temp))
               (t in-docs-temp))))
         (abstract-box 
           (make-std-patch-box (type-of self)  
                               (read-from-string title) in-put-docs win in-boxes)))
    (dmove-patch abstract-box (x self) (y self))
    ;;   connections
    (make-abstract-box-connections *active-patch-window* 
                                   abstract-box out-box in-boxes win)
    (tell (controls *active-patch-window*) 'draw-connections t)
    (when (member self *current-user-configuration*)
      (user-menu-remove self) (user-menu-include abstract-box))
    (tell (controls *active-patch-window*) 'deactivate-control)
    (setf (active-mode self) t)
    (setf (patch-win self) nil)       ;unconnect old patch from window
    (cut *active-patch-window*)
    (setf (pw-function-string abstract-box) title)
    (set-window-title (patch-win abstract-box) title)
    (add-patch-box *active-patch-window* abstract-box)
    (ask-all (controls *active-patch-window*) 'connect-new-patch? self abstract-box)
    (tell (controls *active-patch-window*) 'draw-connections) ) )

(defgeneric make-abstraction-M (self &optional abstract-class)
  (:method ((self C-pw-window) 
            &optional (abstract-class 'C-abstract-M))
    (let ((*si-record* nil))
      (if (not (active-patches self))
          (ui:message-dialog "No active patches!")
          (let ((active-rect (find-active-rectangle self))
                (new-win)(patches)(abstract-box)(in-boxes) 
                (out-box (find-abstract-out-box self (active-patches self))))
            (cond 
              ((not out-box) 
               (ui:message-dialog "One abstract-out-box should be selected !"))
              ((> (length out-box) 1)
               (ui:message-dialog "Only one abstract-out-box should be selected !"))
              (t 
               (cut self)
               (setq patches (eval (patch-scrap self)))
               (tell patches 'dmove-patch (- (car active-rect)) (- (second active-rect)))
               (setq out-box (car (find-abstract-out-box self patches)))
               (setq new-win 
                     (make-instance (type-of self) :close-box-p nil
                                                   :window-title (dialog-item-text (car (pw-controls out-box)))
                                                   :view-position 
                                                   (make-point (+ (x self) (first active-rect))
                                                               (+ (y self) (second active-rect))) 
                                                   :view-size 
                                                   (make-point (third active-rect) (fourth active-rect)) ))
               (apply #'add-subviews new-win patches)
               (deactivate-control out-box)
               ;;inboxes        
               (setq in-boxes (find-abstract-in-boxes self patches))
               (setq abstract-box 
                     (make-std-abstract-box self patches new-win in-boxes abstract-class ))
               (unless abstract-box 
                 (window-close new-win)
                 (apply #'add-subviews self patches))
               (when abstract-box
                 (add-subviews self abstract-box)
                 ;;connections
                 (make-abstract-box-connections self abstract-box out-box in-boxes new-win)
                 (tell (controls new-win) 'update-win-pointers new-win))
               (let ((*si-record* t))
                 (record-event :|core| :|crel| `((,:|kocl| ,:|obab|))))
               abstract-box)))))))


(defgeneric make-std-abstract-box (self patches new-win in-boxes abstract-class)
  (:method ((self C-pw-window) patches new-win in-boxes  abstract-class)
    (let (in-put-docs)
      (when in-boxes        
        (setq in-put-docs (get-absin-boxes-types-n patches in-boxes))
        (when (member nil in-put-docs)
          (message-dialog  
           "WARNING! absin box connected to irreducible types. ALL-type used.")
          (setq in-put-docs
                (mapcar (lambda (type-spec) (declare (ignore type-spec)) '(nilNum))
                        in-put-docs))))
      (make-std-patch-box abstract-class  
                          (read-from-string (window-title new-win)) in-put-docs new-win in-boxes))))

(defun all-absins-different (absins)
  (= (length absins) (length (remove-duplicates absins))))

(defun make-std-patch-box (class fun-string type-specs new-win in-boxes)
  (multiple-value-bind (fun-name exists?)
      (intern (string fun-string) "USER-ABSTRACTION")
    (when exists?
      (ui:uiwarn "the name ~D already exists for an abstraction.
A unique name will be chosen (if not a 'redraw')" fun-name)
      (ui:ed-beep)
      (setq fun-name (intern (string (gensym (string fun-name))) "USER-ABSTRACTION"))
      (set-window-title new-win (string fun-name)))
    (let ((arg-names (mapcar (lambda (absin) (read-from-string (doc-string absin)))
                             in-boxes))
          ;; (thename (read-from-string (concatenate 'string "USER-ABSTRACTION::" (string fun-name))))
          )
      (if (all-absins-different arg-names)
          (progn
            (set-PW-symbolic-type-data fun-name
                                       (list (cons '&required 
                                                   (mapcar (lambda (name type) (cons name type)) arg-names
                                                           (setq type-specs (quote-value type-specs))))
                                             '(&optional) '(&rest)) 'nil)  ;output type is nil, for the moment...
            (setf (fdefinition fun-name)
                  (eval `(function (lambda ,arg-names (declare (ignore ,@arg-names)) nil))))
            ;;(eval `(defun ,thename ,arg-names (declare (ignore ,@arg-names)) nil))
            (make-PW-standard-box class fun-name (make-point 15 15)
                                  (get-current-inbox-vals type-specs)))
          (progn
            (ui:message-dialog
             "Absin names and Absin numbers should be unique. Please correct this") nil)))))


(defun quote-value (type-specs)
  (mapcar (lambda (type) (list (car type) (replace-value-keyword (cadr type))))
          type-specs))

(defun replace-value-keyword (key-list)
  (let* ((value (member :value key-list))
         (where (length value))
         (size (length key-list)))
    (if (or (zerop where) (numberp (second value)))
        key-list
        (append (subseq key-list 0 (- size where -1)) (cons `',(cadr value) (cddr value))))))

(defun get-current-inbox-vals (type-specs)
  (mapcar (lambda (type) 
            (if (member :value (cadr type))
                (eval (cadr (member :value (cadr type))))
                (or (get-type-default (car type))
                    (cadr (member :dialog-item-text (cadr type))))))
          type-specs))

(defgeneric make-abstract-box-connections (self abstract-box out-box in-boxes new-win)
  (:method ((self C-pw-window) 
            abstract-box out-box in-boxes new-win)
    (setf (abstract-obj out-box) abstract-box)
    (for (i 0 1 (1- (length in-boxes)))
      (connect-to-nth-input (nth i in-boxes) i abstract-box))
    (setf (patch-win abstract-box) new-win)
    (setf (out-obj abstract-box) out-box)
    (setf (abstract-box new-win) abstract-box) ))

;;
;; collects a list of the control objects of all input boxes the absins are connected to
(defun get-absin-boxes-types-n(patches absins)
  (let ((useful-patches (set-difference patches absins))
        (absin-types)
        (all-types))
    (dolist (in-box absins)
      (dolist (patch useful-patches)
        (push (the-type-of-n patch in-box) absin-types))
      (push  (coerce-PW-types-in-list (remove nil absin-types))  all-types)
      (setq absin-types nil))
    (nreverse all-types)))

(defun the-type-of-n (patch in-box)
  (let* ((the-in-objects (input-objects patch))
         (where-found)
         (type-list))
    (while (setq where-found (nth? in-box the-in-objects))
      (setq the-in-objects (substitute nil in-box the-in-objects :test 'eq :count 1))
      (push (patch-work-type-of patch where-found ) type-list))
    (and type-list (coerce-PW-types-in-list type-list))))

;;gives a control object whose type coerces to the intersection of all types
;;of the control objects the absin is connected to
(defmethod patch-work-type-of ((self C-patch) ctrl-index)
  (let* ((types (if (defunp-function? (pw-function self))
                    (get-intypes (pw-function self))
                    (list 
                     (cons '&required
                           (mapcar (lambda (ctrl) 
                                     (list (read-from-string (doc-string ctrl)) 'nilNum))
                                   (pw-controls self)))
                     '(&optional) '(&rest))))
         (type-specs-list 
           (append (mapcar #'cdr (cdr (assoc '&required types)))
                   (mapcar #'cdr (cdr (assoc '&optional types)))
                   (mapcar #'cdr (cdr (assoc '&rest types))))))
    (if (>= ctrl-index (length type-specs-list))
        (set-current-value (car (last type-specs-list))
                           (patch-value (car (last (pw-controls self))) self))
        (set-current-value (nth ctrl-index type-specs-list)
                           (patch-value (nth ctrl-index (pw-controls self)) self)))))

(defgeneric patch-work-ctrl-box (self ctrl-index)
  (:method ((self C-patch) ctrl-index) ;{Camilo} should be put in PW-wind+patch 
    (nth ctrl-index (pw-controls self))))

;;coerces types to their intersection. e.g '(float fixnum) and '(fixnum) coerces to
;;'(fixnum). The result is an object with this type (if any).
(defun coerce-PW-types-in-list (list-of-types)
  (and list-of-types
       (let ((current-intersect 
               (get-the-type-list (car list-of-types))))
         (dolist (type-spec (cdr list-of-types))
           (setq current-intersect 
                 (type-intersection current-intersect (get-the-type-list type-spec))))
         (and current-intersect 
              (set-the-type-list 
               (car list-of-types)
               (if (eq current-intersect 'all-types) nil current-intersect))))))

(defun type-intersection (list1 list2)
  (cond ((eq list1 'all-types) list2)
        ((eq list2 'all-types) list1)
        (t (intersection list1 list2))))

;;Camilo [922010]
#|(defun set-current-value (type-spec value)
(set-type-default (car type-spec)
value))|#

(defun set-current-value (type-spec value) 
  (set-type-default 
   (copy-tree  (if (consp (car type-spec)) (car type-spec) type-spec))
   ;;(car type-spec)
   value))

(defun get-the-type-list (type-spec)
  (let ((the-type-list 
          (member :type-list
                  (if (cdr type-spec)
                      (do-merge-specs (get-PW-type-specs (car type-spec))
                        (cadr type-spec))
                      (get-PW-type-specs (car type-spec))))))
    (and the-type-list
         (if (cadr the-type-list) (cadr the-type-list) 'all-types))))

(defun set-the-type-list (type-spec type-list)
  (list (car type-spec)
        (do-merge-specs (get-PW-type-specs (car type-spec))
          (list* :type-list type-list (cadr type-spec)))))

;;;=======================================================================
;;; The save user-patch-configuration function
;;;
;;; (Camilo 7/2/91... All this should go to a separate file
;;; PW-user-patches should be a logical path variable (where?))
;;;=======================================================================

(defclass C-patch-configurer(C-patch)
  ((file :initform () :initarg :file-path :accessor file-path)
   (action-box :initform () :initarg :action-box :accessor action-box)
   (MenuBox :initform () :initarg :MenuBox :accessor menubox))
  (:documentation "the class of patch-configuration patches"))

(defmethod decompile ((self C-patch-configurer))
  (when (eq self *active-config-object*) 
    (setf *active-config-object*())
    (reset-fill-control self))
  `(sbox ',(type-of self) ',(pw-function self) ,(pw-function-string self)
         ,(active-mode self) ,(view-position self) '("()")
         ,(make-point (w self) (- (h self) 15))
         ',(file-path self)))

(defmethod complete-box ((self C-patch-configurer) files)
  (setf (file-path self) files) self)

(defunp config ((files list (:value "()"))) list
    "configures PW menus according to user defined library loadings"
  (declare (ignore files)))

(defvar *Config-popUpMenu*
  (new-menu " "
            (new-leafmenu "New" (lambda () (change-config *target-action-object*)))
            (new-leafmenu "Add" (lambda () (add-to-config *target-action-object*)))
            (new-leafmenu "Remove" (lambda () (remove-from-config *target-action-object*)))
            (new-leafmenu "Save" (lambda () (save *target-action-object*)))))

(defmethod initialize-instance :after((self C-patch-configurer) &key file-path)
  (declare (ignore file-path))
  (set-view-size self (w self) (+ (h self) 15))
  (set-view-position (out-put self) 
                     (make-point (x (out-put self)) (+ (y (out-put self)) 15)))
                                        ;(move-control (out-put self) (x (out-put self)) (+ (y (out-put self)) 15))
  (let ((popUpBox (make-popUpbox "M" self
                                 *Config-popUpMenu*
                                 :view-position (make-point (- (w self) 10)
                                                            (- (h self) 14))
                                 :view-container self
                                 :view-font '("monaco"  9  :srcor))))
    (setf (action-box self) (make-actionControl-box self))
    (setf (menubox self) popUpBox)))

(defgeneric make-actionControl-box (self)
  (:method ((self C-patch-configurer))
    (let ((w (point-h (view-size self)))
          (h (point-v (view-size self))))
      (make-instance 'static-text-dialog-item
                     :view-position (make-point (- (truncate w 2) 5) (- h 27))
                     :view-size (make-point 8 10)
                     :dialog-item-text "D"
                     :view-container self
                     :view-font '("monaco"  9  :srcor)
                     :dialog-item-action (lambda (item) 
                                           (configure (view-container item)))))))

(defgeneric set-fill-control (self)
  (:method ((self C-patch-configurer))
    (set-dialog-item-text (action-box self) "C")))

(defgeneric reset-fill-control (self)
  (:method ((self C-patch-configurer))
    (set-dialog-item-text (action-box self) "D")))


(defun make-user-patch-configurer(&key ((:file file) ()))
  (let ((patch
          (make-patch-box 'C-patch-configurer "config" 
                          '(*symbol-eval-pw-files-type* "FileList") '(list))))
    (setf (file-path patch) file)
    patch))

(defmethod patch-value ((self C-patch-configurer) obj)
  (let ((arg (ask  (input-objects self) 'patch-value obj)))
    (if (file-path self)
        (or (and (not arg) (file-path self))
            (config-union arg (file-path self) ))
        arg)) )


(defvar *added-box-object* ())
(defun no-add-patch-box (win box)
  (declare (ignore win))
  (setf *added-box-object* box))

(defgeneric Configure (self)
  (:method ((self C-patch-configurer))
    (let ((Lib&patch-files (patch-value self self)))
      (apply #'remove-menu-items *pw-menu-patch* (menu-items *pw-menu-patch*)) ; erase all sub-menus
      (with-cursor *watch-cursor*
        (dolist (file-name Lib&patch-files)
          (if (library-p (car file-name))
              (handle-library-load (car file-name) (cdr file-name) )
              (let ((saved-fun '*very-odd-fun-name*))
                (setf (fdefinition saved-fun) (fdefinition 'add-patch-box))
                (setf (fdefinition 'add-patch-box) (fdefinition 'no-add-patch-box))
                (load (car file-name))
                (setf (fdefinition 'add-patch-box) (fdefinition saved-fun))
                (form-patch-subMenu (car file-name) 
                                    `(add-patch-box *active-patch-window*
                                                    ,(decompile *added-box-object*)))))))
      (set-user-patch-config Lib&patch-files)
      (unless (eq self *active-config-object*)
        (set-fill-control self)
        (and *active-config-object* 
             (reset-fill-control *active-config-object*))
        (setf *active-config-object* self)))))

(defgeneric change-config (self)
  (:method ((self C-patch-configurer))
    (SAVE-PATCH-CONFIG)
    (setf (file-path self) (get-user-patch-config))))

(defgeneric add-to-config (self)
  (:method ((self C-patch-configurer))
    (SAVE-PATCH-CONFIG)
    (setf (file-path self) (config-union (file-path self) (get-user-patch-config)))))

(defgeneric remove-from-config (self)
  (:method ((self C-patch-configurer))
    (let* ((lib&patches (mapcar #'car (file-path self)))
           (items 
             (ui:catch-cancel (ui:select-item-from-list
                               (mapcar (lambda (path) (file-namestring path)) lib&patches)
                               :window-title "Please Select Abstract and Library Files"
                               :selection-type :disjoint))))
      (when (and items (not (eq items :cancel)))
        (ADD-CELLS items lib&patches)
        (setf (file-path self)
              (config-difference (file-path self) (get-user-patch-config)))))))

;;;==========================================================================
;;; the config functions
;;;==========================================================================    
(defun SAVE-PATCH-CONFIG()                              
  (SHOW-PATCH-LIB nil nil))

(defun SHOW-PATCH-LIB(conf-list obj)   ;useless parameters. Present for "connection-style".
  (declare (ignore conf-list obj))
  (let* ((patches (with-cursor *watch-cursor*
                    (append (directory *PW-user-library-pathName*)
                            (directory *PW-user-abstract-pathName*))))  ; all Libs!!
         (items (catch-cancel (select-item-from-list
                               (mapcar (lambda (path) (file-namestring path)) patches)
                               :window-title "Please Select Abstract and Library Files"
                               :selection-type :disjoint))))
    (when (and items (not (eq items :cancel)))
      (ADD-CELLS items patches))))


(defun ADD-CELLS(selection all-list) 
  (set-user-patch-config  (get-path-names selection all-list)))

(defun get-path-names(files path-list)
  (mapcar (lambda (f) (form-lib-selection (find-file f path-list))) files))

(defun find-file(file path-list)
  (do ((paths path-list))
      ((or (not paths) (string= file (file-namestring (car paths))))
       (namestring (car paths)))
    (pop paths)))

(defun form-lib-selection (file-name)
  (let*((lib-a-list)
        (sel
          (when (library-p file-name)
            (setq lib-a-list (with-cursor *watch-cursor*
                               (WITH-OPEN-FILE (lib file-name :direction :input )
                                 (read lib))))
            (if (and (consp lib-a-list) (consp (car lib-a-list))
                     (atom (caar lib-a-list)))
                (ui:catch-cancel
                  (ui:select-item-from-list 
                   (eval lib-a-list) 
                   :window-title (format nil "please select ~A functions" (file-namestring file-name))
                   :table-print-function (lambda (it &optional strm)(princ (cdr it) strm))
                   :selection-type :disjoint))))))
    (cons file-name (and (not (eq sel :cancel)) sel))))

;;;
;;;takes an abstract-box  and  the decompiled code (abstract-box)
;;;and builds the user Lib Menu.

(defun form-patch-subMenu (patch-file-name code)
  (let ((sub-dir-list 
          (cdr (member "PW-user-patches" (parse-file-name patch-file-name) :test #'string=)))
        (current-sub-menu *pw-menu-patch*)
        (menu))
    (dotimes (x (1- (length sub-dir-list)))
      (unless 
          (setq menu (find-menu-item current-sub-menu (car sub-dir-list)))
        (ui:add-menu-items current-sub-menu
                           (setq menu (new-menu (car sub-dir-list)))))                            
      (setq current-sub-menu menu)
      (pop sub-dir-list))
    (or (find-menu-item current-sub-menu (car sub-dir-list))
        (ui:add-menu-items current-sub-menu (new-leafmenu (car sub-dir-list) 
                                                          (eval `(function (lambda () ,code))))))))

(defun parse-file-name (name)
  (let ((dir (directory-namestring name))
        (name-list (list (file-namestring name))))
    (do ((subdir dir (directory-namestring subdir))) ((string= "" subdir) )
      (setq subdir (string-right-trim ":" subdir))
      (push (file-namestring subdir) name-list))
    name-list))

(defun handle-library-load (lib-file-name key-a-list)
  (setq *library-selection* 
        (mapcar #'cdr key-a-list))
  (load lib-file-name))

(defun config-union (cf1 cf2)
  (let ((result cf2))
    (dolist (file-pair1 cf1)
      (if (library-p (car file-pair1))
          (setq result (merge-lib-select file-pair1 result))
          (setq result (adjoin file-pair1 result :test #'string=  
                                                 :key (lambda (item) (car item))))))
    result))

(defun config-difference(from-list take-list)
  (let ((result from-list))
    (dolist (file-pair1 take-list)
      (if (library-p (car file-pair1))
          (setq result (delete-lib-select file-pair1 result))
          (setq result (remove (car file-pair1) result :test #'string=  
                                                       :key (lambda (item) (car item))))))
    result))

(defun library-p (file-name)
  (string-equal *PW-user-library-extension* (pathname-type  file-name)))

(defun merge-lib-select (file-pair file-list)
  (let ((found (member (car file-pair) file-list :test #'string= :key (lambda (it)(car it)))))
    (if found
        (substitute (merge-lib-keys file-pair (car found))
                    (config-lib-name found)
                    file-list
                    :test #'string=
                    :key (lambda (it)(car it))
                    :count 1)
        (cons file-pair file-list))))

(defun config-lib-name (pair) (caar pair))

(defun delete-lib-select(pair file-list)
  (let ((found (member (car pair) file-list :test #'string= :key (lambda (it)(car it)))))
    (if found
        (let ((item (delete-lib-keys pair (car found))))
          (if (not item)
              (remove (car pair) file-list :test #'string= :key (lambda (it) (car it)))
              (substitute item
                          (config-lib-name found)
                          file-list
                          :test #'string=
                          :key (lambda (it)(car it))
                          :count 1)))
        file-list)))

(defun merge-lib-keys(f-p1 f-p2) 
  (cons (car f-p1) (union (cdr f-p1)(cdr f-p2) :test #'equal :key #'car)))

(defun delete-lib-keys(f-p1 f-p2)
  (let ((selections (set-difference (cdr f-p2) (cdr f-p1) :test #'equal :key #'car)))
    (and selections (cons (car f-p1) selections))))

;;;; THE END ;;;;
