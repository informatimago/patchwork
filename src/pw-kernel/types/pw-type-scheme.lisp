;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pw-type-scheme.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;    
;;;;    A typing scheme for PW.
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

;;=====================================================
;;Class C-pw-type-set
;;=====================================================

(defgeneric play                (patch) (:method (patch) (declare (ignorable patch))))
(defgeneric stop-play           (patch) (:method (patch) (declare (ignorable patch))))
(defgeneric open-patch-win      (patch) (:method (patch) (declare (ignorable patch))))
(defgeneric record              (patch) (:method (patch) (declare (ignorable patch))))
(defgeneric browse              (patch) (:method (patch) (declare (ignorable patch)))) 

(defclass C-PW-type-set ()
  ((type-list :initform () :initarg :type-list :accessor type-list))
  (:documentation 
   "the class of internal types. Type-list is an a-list of (type-name . type-obj) specs"))

(defun make-type-set (list)
  (make-instance 'C-PW-type-set 
                 :type-list (mapcar (lambda (type) (cons (type-name type) type)) list)))

(defgeneric get-type (self type-name)
  (:method ((self C-PW-type-set) type-name)
    (cdr (assoc type-name (type-list self) :test #'string=))))

(defgeneric get-type-specs (self type-name)
  (:method ((self C-PW-type-set) type-name)
    (let ((type (get-type self type-name)))
      (and type (type-specs type)))))

(defgeneric delete-type (self type-name)
  (:method ((self C-PW-type-set) type-name)
    (setf (type-list self) 
          (delete (list type-name) (type-list self) :key #'car :test #'string=))))

(defgeneric add-type (self type)
  (:method ((self C-PW-type-set) type)
    (unless (get-type self (type-name type))
      (setf (type-list self) (acons (type-name type) type (type-list self))))))

(defclass C-pw-type-obj ()
  ((type-name :initform 'type :initarg :type-name :accessor type-name)
   (type-class :initform 'C-numbox :initarg :type-class :accessor type-class)
   (type-specs :initform '(:value 0 :min-value -100 :max-value 100 :doc-string "fix")
               :initarg :type-specs :accessor type-specs)))

(defun make-type-object (name class specs)
  (make-instance 'C-pw-type-obj 
                 :type-name name :type-class class :type-specs specs))

(defgeneric merge-specs (self new-specs)
  (:method ((self C-pw-type-obj) new-specs)
    (do-merge-specs (type-specs self) new-specs)))

(defun do-merge-specs (old-specs new-specs)
  "replaces old-spec values by new-specs values in all equal keys. Adds the spec if
   new key"
  (let ((specs (copy-list old-specs)) (n-sp new-specs) res)
    (mapc (lambda (key-val) (update-spec key-val specs))
          (progn (while n-sp (push (cons (pop n-sp) (pop n-sp)) res))
                 (nreverse res)))
    specs))

(defun update-spec (key-value specs)
  (let ((sublist (member (first key-value) specs)))
    (if sublist 
      (setf (second sublist) (cdr key-value))
      (error "invalid key-name ~S for type: ~S" (car key-value) (cdr key-value)))
    specs))

;;utilities for collecting function arguments
(defvar *valid-defune-keywords* '((&optional) (&rest)))
(defvar *valid-defp-keywords* '(&optional &rest))

(defun collect-keywords (arglist)
  (let ((collection (cons (list '&required) *valid-defune-keywords*))
        (current-key '&required)
        element)
    (dolist (arg arglist)
      (setq element (cdr (assoc current-key collection)))
      (if (not (consp arg))
        (if (valid-keyword-p arg)
          (setq current-key arg)
          (error "invalid keyword: ~S" arg))
        (setq collection 
              (substitute (cons current-key (append element (list arg)))
                          (cons current-key element)
                          collection :test (lambda (x y) (string= (car x) (car y)))))))
    collection))
  
(defun valid-keyword-p (arg)
  (and (symbolp arg)
       (member arg *valid-defune-keywords* :test (lambda (x y) (string= x (car y))))))

(defun get-arg-specs (arg-name args-types &optional position)
  (let (specs var-spec)
    (if position
      (progn
        (dolist (key (cons '&required *valid-defp-keywords*) nil)
          (setq specs (append specs (cdr (assoc key args-types)))))
        (setq var-spec (cdr (nth position specs)))
        (if (not (consp (car var-spec))) var-spec (car var-spec)))
      (dolist (key (cons '&required *valid-defp-keywords*) nil)
        (setq specs (cdr (assoc key args-types)))
        (when specs 
          (setq var-spec (cdr (assoc arg-name specs :test #'string=)))
          (if var-spec 
            (return (if (not (consp (car var-spec))) var-spec (car var-spec))))))
      )))


;;=========================
;;PW-types 
;;=========================

(defclass C-menubox-val-mod (C-menubox) ())

(defmethod menubox-value ((self C-menubox-val-mod)) 
  (car (call-next-method))) 

(defmethod patch-value ((self C-menubox-val-mod) obj) 
  (declare (ignore obj))
  (cdr (nth (mod (value self) (length (menu-box-list self))) (menu-box-list self)))) 


(defvar *box-open* nil)

(defmethod view-double-click-event-handler ((self C-menubox-val-mod) where)
  (declare (ignore where))
  (setf *box-open* t)
  (call-next-method))

(defmethod set-numbox-item-text  ((self C-menubox-val-mod) value)
  (when *box-open*
    (setf *box-open* nil)
    (setf (cdr (nth (mod (value self) (length (menu-box-list self)))
                    (menu-box-list self))) value))
  (set-dialog-item-text self (menubox-value self)))

;;=============================================

(defvar *pw-integer-type*
   (make-type-object 'integer 'C-numbox
                     (list :view-size #@(36 14) :value 0 :min-val -9999 :max-val 999999 
                           :doc-string "fix" :type-list '(fixnum))))
(defvar *pw-fix/float-type*
  (make-type-object 'fix/float 'C-numbox
                    (list :view-size #@(36 14) :value 0 :min-val -9999 :max-val 999999 
                          :doc-string "fix/fl" :type-list '(fixnum float))))
(defvar *pw-midic-type*
  (make-type-object 'midic 'C-numbox
                    (list :view-size #@(36 14) :value 6000 :min-val 0 :max-val 12700 
                          :doc-string "midic" :type-list '(fixnum list))))
(defvar *pw-fx/fl/l-type*
  (make-type-object 'fix/fl/list 'C-numbox
                    (list :view-size #@(36 14) :value 0 :min-val -9999 :max-val 999999
                          :doc-string "fx/fl/l" :type-list '(fixnum float list))))
(defvar *pw-approx-type*
  (make-type-object 'approx 'C-numbox
                    (list :view-size #@(36 14) :value 1 :min-val 1 :max-val 16 
                          :doc-string "approx" :type-list '(fixnum))))

(defvar *pw-nilNum-type*
  (make-type-object 'nilNum 'C-numbox
                    (list :view-size #@(36 14) :value 0 :min-val -9999 :max-val 999999 
                          :doc-string "num" :type-list ())))

(defvar *pw-positiveFix-type*
  (make-type-object 'Pfix 'C-numbox
                    (list :view-size #@(36 14) :value 100 :min-val 0 :max-val 999999 
                          :doc-string "num" :type-list '(fixnum))))

(defvar *pw-symbol-type*
  (make-type-object 'fixs 'C-ttybox
                    (list :view-size #@(36 14) :doc-string "lst" :value nil
                          :dialog-item-text "(1 2)" :type-list '(list))))

(defvar *pw-string-type*
  (make-type-object 'string 'C-ttybox-str
                    (list :view-size #@(36 14) :dialog-item-text "name" 
                          :doc-string "str" :type-list '(list))))

(defvar *pw-symEval-type*
  (make-type-object 'list-eval 'C-ttybox-eval
                    (list :view-size #@(36 14) :dialog-item-text "lst" 
                          :doc-string "list" :type-list '(list))))

(defvar *pw-menu-type*
  (make-type-object 'menu 'C-menubox-val
                    (list :view-size #@(36 14) :type-list '(no-connection)
                    :menu-box-list '(("a" . 1) ("b". 2)) :value 0)))

(defvar *pw-absout-type*
  (make-type-object 'absout 'C-ttybox-absout
                    (list :view-size #@(36 14) :dialog-item-text "name")))

(defvar *pw-absin-type*
  (make-type-object 'absin 'C-ttybox-absin
                    (list :view-size #@(36 14) :dialog-item-text "name"
                          :type-list '(no-connection))))

(defvar *pw-user-out-type*
  (make-type-object 'user-out 'C-ttybox-out
                    (list :view-size #@(36 14) :doc-string "sym" :value nil
                          :dialog-item-text "foo" :type-list '())))
(defvar *pw-user-in-type*
  (make-type-object 'user-in 'C-ttybox-in-box
                    (list :view-size #@(36 14) :doc-string "sym" :value nil
                          :dialog-item-text "foo" :type-list '())))

(defvar *pw-object-type*
  (make-type-object 'object 'C-ttybox
                    (list :view-size #@(36 14) :doc-string "obj" :value nil
                          :dialog-item-text "()" :type-list '())))

(defvar *chord-box-M-pw-type*
  (make-type-object 'ch-box 'C-chord-box
                 (list  :view-size (make-point 38 120)
                        :type-list '(list midic chord))))
    
(defvar *PW-all-basic-types*
  (make-type-set 
   (list *pw-integer-type* *pw-fix/float-type* *pw-midic-type* *pw-fx/fl/l-type*
         *chord-box-M-pw-type* *pw-approx-type* *pw-nilNum-type* *pw-positiveFix-type* 
         *pw-symbol-type* *pw-string-type* *pw-absin-type* *pw-symEval-type* *pw-menu-type* 
         *pw-absout-type* *pw-absin-type* *pw-object-type* *pw-user-out-type* *pw-user-in-type*)))

(defvar *pw-all-out-types*
  (list
   (cons 'nil      '())
   (cons 'all-types '())
   (cons 'list     '(list))
   (cons 'fix      '(fixnum))
   (cons 'float    '(float))
   (cons 'bool     '(bool))               ;; What else ?
   (cons 'freq     '(float fixnum))
   (cons 'midic    '(fixnum list))        ;; Hélas oui !!
   (cons 'freqs?   '(float fixnum list))
   (cons 'midics?  '(fixnum list))        ;; Hélas oui !!
   (cons 'string   '(string))
   (cons 'strings? '(string list))
   (cons 'chord    '(list))
   (cons 'chords   '(list))
   (cons 'freqs    '(list))
   (cons 'formants '(list))
   (cons 'number   '(float fixnum))
   (cons 'numbers? '(float fixnum list))
   (cons 'beat '(beat))
   (cons 'measure '(measure))
   (cons 'note-ob '(note-obj))
   (cons 'ch-ob '(chord))
   (cons 'ch-line '(chord-line))
   (cons 'collector '(collector))
   (cons 'no-connection '(no-connection))))

(defun add-output-type (name type-list)
  (if (assoc name *pw-all-out-types*)
    (progn
      (ui:uiwarn "output type ~A already exists!" name))
    (push (cons name type-list) *pw-all-out-types*))
  nil)

(defun add-pw-input-type (name class type-form-list)
  (add-type *PW-all-basic-types*
            (make-type-object name class type-form-list)))

;;
;;A list of type alias for PW basic types. Car is the alias name. Cdr is the corresponding
;;PW type with (optional) new specs
(defun foo () nil)   ; the symbol by default!!!
(defvar foo nil)

(defvar *pw-all-type-alias*
  `((list . fixs) (symbol . (fixs .  (:dialog-item-text "foo")))
    (freq . (fix/float . (:value 440 :doc-string "freq")))
    (cents . midic) (freqs? . (fix/fl/list . (:value 440))) 
    (freqs . (fix/fl/list . (:value 440)))
    (midics? . midic) (chord . midic)
    (fix . fix/float) (fix>=0 . (Pfix . (:value 0)))
    (fix>0 . (Pfix . (:min-val 1 :value 1)))
    (fix>0s? . (fix/fl/list . (:min-val 0 :value 1)))
    (float . fix/float) (floats . fix/fl/list)
    (all-types . (list . (:value "nil" :type-list nil)))
    (delta-freq . fix/fl/list) (numbers? . fix/fl/list)) )

(defun add-alias-to-pw (name pw-type &optional specs)
  (setf *pw-all-type-alias*
        (acons name (if specs (cons pw-type specs) pw-type) *pw-all-type-alias*)))

(defun Get-Pw-Type-Specs (type-name)
  "given a type-name (or alias) gets the type specs"
  (let ((alias (cdr (assoc type-name *pw-all-type-alias* :test #'string=)))
        type)
    (if alias
      (values 
       (merge-specs (setq type (get-type *PW-all-basic-types* (get-alias-name alias)))
                   (get-alias-specs alias))
       (type-class type))
      (values 
       (get-type-specs *PW-all-basic-types* type-name)
       (type-class (get-type *PW-all-basic-types* type-name))))))

(defun get-alias-name (alias)
  (if (not (consp alias)) alias (car alias)))

(defun get-alias-specs (alias)
  (and (consp alias) (cdr alias)))

(defun get-type-default (type-form)
  (if (consp type-form)
    (or (cadr (member :value (cadr type-form)))
        (cadr (member :value (get-pw-type-specs (car type-form)))))
    (cadr (member :value (get-pw-type-specs type-form)))))

(defun set-type-default (type-form value)
  (if (consp type-form)
    (let (sub-type)
      (cond ((setq sub-type (member :value (cadr type-form)))
             (setf (cadr sub-type) value)
             type-form)
            ((member :value (get-pw-type-specs (car type-form)))
             (list (car type-form) (list* :value value (cadr type-form))))
            (t (list (car type-form) 
                     (list* :dialog-item-text (format nil "~D" value) (cadr type-form))))))
    (if (member :value (get-pw-type-specs type-form))
      (list  type-form (list :value value))
      (list type-form (list :dialog-item-text (format nil "~D" value))))))

;;;============================================================
;;; user PW function definition (modified from J. Duthen's version)
;;;===========================================================
(defvar *error-functiondoc-missing* "doc missing for :~S")

(defmacro defunp (name args outtype documentation &body body)
  "creates a function and stores info about input and output types"
  (let* ((parsed-list (collect-keywords args))
         (lambda-list (get-lambda-list parsed-list)))
    (unless (stringp documentation)
      (error *error-functiondoc-missing* name) )
     `(progn
        (defun ,name ,lambda-list ,documentation ,@body)
        (set-PW-symbolic-type-data ',name  ',parsed-list ',outtype))))

(defun get-lambda-list (collected-a-list)
"returns a standard lambda list from the a-list of type-specs"
   (let* ((keys (cons  '&required *valid-defp-keywords*))
          (lambda-list (mapcan (lambda (key) (get-arg-names key collected-a-list)) keys)))
     (if (eq (car lambda-list) '&required)
       (cdr lambda-list)
       lambda-list)))

(defun collect-optionals (arg-type-pair)
  (list (first arg-type-pair)
        (if (third arg-type-pair)
          (get-type-default (cdr arg-type-pair))
          (get-type-default (second arg-type-pair)))))

(defun get-arg-names (key a-list)
  "gets argument names from the type a-list"
  (let ((type-list 
         (mapcar (if (string= key '&optional) #'collect-optionals #'car)
                 (cdr (assoc key a-list)))))
    (if type-list
      (cons key type-list))))

(defmacro defunt (name args outtype)
    `(set-PW-symbolic-type-data ',name  ',(collect-keywords args) ',outtype))

;; (nconc (cassq 'function *define-type-alist*) (list "unp" "unt"))
  
(defgeneric set-PW-symbolic-type-data (me intypes outtype)
  (:documentation "the symbol stores information about its function")
  (:method ((me symbol) intypes outtype)
    (mapc (lambda (type) (check-input-types type  me)) intypes)
    (check-io-type outtype :output me)
    (setf (get me '*type-intypes*) intypes)
    (setf (get me '*type-outtype*) (find-out-type outtype))
    ;;(import me)
    ;;(export me)
    me))

(defun copy-PW-symbolic-type-data (pw-function new-name)
  "copies type-data from pw-function to new-name"
  (setf (get new-name '*type-intypes*) (get pw-function '*type-intypes*))
  (setf (get new-name '*type-outtype*) (get pw-function '*type-outtype*)))

(defun defunp-function? (function)
  (or (get function '*type-outtype*) (get function '*type-intypes*)))

(defun get-out-type-list (function)
  (get function '*type-outtype*))

(defun get-intypes (function)
  (get function '*type-intypes*))

(defun set-function-arg-names (fun name-list)
  (let (result)
    (setf (get fun '*type-intypes*)
          (mapcar (lambda (args) 
                      (setq result nil)
                      (cons (car args) 
                            (and name-list 
                                 (nreverse (dolist (var-form (cdr args) result)
                                             (push (cons (pop name-list) (cdr var-form))
                                                   result))))))
                  (get-intypes fun)))))

(defun check-input-types (type-form fun)
  (mapc (lambda (form) (check-io-type (if (not (consp (cadr form)))
                                          (cadr form) (caadr form)) :input fun))
        (cdr type-form)))
  
(defconstant *error-unknown-symbolic-pw-type* "unknown symbolic ~A type ~A for fun ~A")
(defvar *keywords-for-extension* '(&optional &rest))

(defun check-io-type (type in-out sym)
  (or (ecase in-out (:input (or (assoc type *pw-all-type-alias* :test #'string=)
                            (get-type *PW-all-basic-types* type)))
             (:output (assoc type *pw-all-out-types* :test #'string=)))
      (error *error-unknown-symbolic-pw-type* in-out type sym)))

(defun find-out-type (name) 
  (cdr (assoc name *pw-all-out-types* :test #'string=)))

(defun get-arglist (function)
 (ccl:arglist function))


(defun make-defunp-function-arg-list (function &optional (nb-arg 0))
  (let ((arg-list (get-arglist function))
        (res ())
        (keyword nil)
        extensible? name)
    (when (null (car arg-list)) ; no argument == (())
      (setq arg-list ()))
    (while (and arg-list (not (memq (car arg-list) *valid-defp-keywords*)))
      (setq res 
            (list* (string-downcase (string (setq name (pop arg-list))))
                   (multiple-value-bind (specs class)
                            (merge-pw-type-specs function name) (cons class specs)) res))
      (decf nb-arg))
    (setq extensible? 
          (not (null (intersection arg-list *keywords-for-extension*))))
    (while (> nb-arg 0)
      (while (and arg-list
                  (not (eq keyword '&rest))
                  (memq (car arg-list) lambda-list-keywords))
        (setq keyword (pop arg-list)))
      (case keyword
        (&rest
         (repeat nb-arg 
           (setq res (list* "arg" 
                            (multiple-value-bind (specs class)
                              (merge-pw-type-specs function (car arg-list))
                              (cons class specs))
                            res)))
         (setq nb-arg 0))
        (t (if (endp arg-list) 
             (progn (ui:ed-beep) (setq nb-arg 0))
             ;(error "Function ~S cannot provide ~D extra argument~:P.~%"
                   ; function nb-arg))
           (ccase keyword
                  (&optional
                   (setq res 
                         (list* (string-downcase (string (setq name (pop arg-list))))
                                (multiple-value-bind (specs class)
                                     (merge-pw-type-specs function name) (cons class specs))
                                res))
                   (decf nb-arg)))))))
    (values (nreverse res) extensible?)))

(defun merge-pw-type-specs (function name)
"merges the type-specs supplied in defunp with those of the corresponding basic type"
  (let* ((args-types (get function '*type-intypes*))
         (type-a-list (get-arg-specs name args-types)))
    (unless type-a-list
      (setq type-a-list 
            (get-arg-specs name args-types (find-position name (get-arglist function)))))
    (multiple-value-bind (specs class)
                         (Get-Pw-Type-Specs (car type-a-list))
      (values (do-merge-specs specs (cadr type-a-list)) class))))

(defun find-position (var args)
  (labels ((search-arg (args num)
           (if (member (car args) *keywords-for-extension* :test #'string=)
             (search-arg (cdr args) num)
             (if (eq (car args) var) num (search-arg (cdr args) (1+ num))))))
    (search-arg args 0)))
  
;;=================================
;; the PW-box construction function
;;=================================

(defun make-PW-standard-box (class-name pw-function 
                             &optional (position (make-point 15 15)) value-list size)
  (let ((input-boxes-types (make-defunp-function-arg-list pw-function (length value-list)))
        (y-now 5)
        (index 0)
        (input-boxes '())
        (module))
    (while input-boxes-types
      (push (apply 'make-instance (pop input-boxes-types)) input-boxes)
      (setf (doc-string (car input-boxes)) (pop input-boxes-types)))
    (setq input-boxes (nreverse input-boxes))
    (if value-list (set-new-values input-boxes value-list))
    ;;(if size (adjust-new-size input-boxes size))
    (dolist (box input-boxes)
      (if (zerop index)
          (progn (set-view-position box (make-point 5 y-now))
                 ;; (ui::format-trace "make-pw-standard-box" (point-to-list (make-point 5 y-now)) box)
                 )
          (progn (set-view-position box (make-point (+ 7 (w box)) y-now))
                 ;; (ui::format-trace "make-pw-standard-box" (point-to-list (make-point (+ 7 (w box)) y-now)) box)
                 (incf y-now (+ 2 (h box)))))
      (setq index (mod (incf index) 2)))
    (if (not (zerop index)) (incf y-now (+ 2 (h (car (last input-boxes))))))
    (setq module (make-instance class-name
                                :view-position position 
                                :view-size  (make-point 
                                             (+ 5  (if input-boxes
                                                       (apply #'max (ask-all input-boxes 'x+w))
                                                       42))
                                             (+ 13 y-now))
                                :pw-function pw-function
                                :type-list (get-out-type-list pw-function)
                                :VIEW-SUBVIEWS input-boxes))
    (if size (resize-patch-box module size 0))
    module))

(defun box (class-name pw-function pw-function-string
            &optional (position (make-point 15 15)) value-list size sp)
  "same as make-pw-standard-box with a shorter name and specific method calling, for
 even more compact PW boxes decompilation "
  (let ((box (make-PW-standard-box class-name pw-function position value-list size)))
    (setf (pw-function-string box) pw-function-string)
    (complete-box box sp)
    box))

(defun sbox (class-name pw-function pw-function-string active?
             &optional (position (make-point 15 15)) value-list size sp)
  "same as box with activation flag "
  (let ((box (box class-name pw-function pw-function-string position value-list size sp)))
    (setf (active-mode box) active?)
    box))

(defun set-new-values (boxes values)
  (mapc (lambda (box val) 
            (unless (eq val :default)
              (setf (value box) val)
              (set-dialog-item-text box 
                   (if (or (numberp val) (listp val))
                     (format () "~D" val)
                     val  ; (string-downcase val)
                     ))
              (set-special-text box val)))
        boxes values))

(defun adjust-new-size (boxes size)
  (if (= (length boxes) 1)   ;not clear what to do in other cases....
    (let* ((box (car boxes))
           (w-scale (/ (point-h size) (+ (w box) 10)))
           (h-scale (/ (point-v size) (+ (h box) 18))))
      (set-view-size box (make-point (truncate (* w-scale (w box)))
                                     (truncate (* h-scale (h box))))))))

(defun make-functional-pw-boxes (function win) 
  (if (not (fboundp function))
      (format t "~15A~25A" function "no such function !" )
      (multiple-value-bind (args extensible?) (make-defunp-function-arg-list function)
        (add-patch-box win 
                       (make-PW-standard-box
                        (cond
                          (extensible?           'C-pw-functional)
                          ((= (length args) 2)   'C-pw-resize-x)
                          (t                     'C-patch))
                        function)))))


;; (progn
;;   (setq foo (make-type-object 'bar 'C-numbox 
;;                               '(:value 4 :min-val -100 :max-val 400 :doc-string "bar")))
;; 
;;   (setq foo2 (make-type-object 'lago 'C-tty 
;;                                '(:value 'fru  :doc-string "logo")))
;;   (setq set (make-type-set (list foo2 foo)))
;;   (merge-specs (get-type set 'lago) '(min-val 887))
;;   (get-pw-type-specs 'fix>0s?)
;;   (defunp my-fun3 ((x symbol) (y (fix (:value 1))) &optional (z fix)) list "my-fun"
;;     (mapcar (lambda (item) (+ (if z z 0) (* item y))) x))
;;   (get 'my-fun3 '*type-intypes*)
;;   (Get-Pw-Type-Specs 'fix>0)
;;   (set-function-arg-names 'my-fun3 '(w b d)))

;;A macro for PW user classes

(defun match-inputs-to-args (args obj)
  (let (res)
    (do ((next-args args (cdr next-args)) (index 0 (1+ index)))
        ((null next-args) (nreverse res))
      (push 
       (cond
        ((string= (first next-args) '&optional)
         (prog1 (list (caadr next-args)
                      `(if (nth ,index (input-objects self)) (patch-value (nth ,index (input-objects self)) ,obj) ,(cadadr next-args)))
           (setq next-args (cddr next-args))))
        ((string= (first next-args) '&rest)
         (prog1 (list (caadr next-args) `(ask-all (nthcdr ,index (input-objects self)) 'patch-value ,obj))
           (setq next-args (cddr next-args))))
        (t  (list (car next-args) `(patch-value (nth ,index (input-objects self)) ,obj))))
       res))))

(defmacro defmethodp (name class-argument args outtype documentation &body body)
  "creates a method and stores info about input and output types"
  (let* ((parsed-list (collect-keywords args))
         (lambda-list (get-lambda-list parsed-list))
         (obj-var (gensym)) )
    (unless (stringp documentation)
      (error *error-functiondoc-missing* name) )
    `(progn
       (defmethod pw::patch-value ((self ,class-argument) ,obj-var)
         (let ,(match-inputs-to-args lambda-list obj-var) ,@body))
       (defun ,name ,lambda-list
         (declare (ignore ,@lambda-list))
         ,documentation
         nil)
       (set-PW-symbolic-type-data ',name  ',parsed-list ',outtype))))

;;(defclass C-fru (C-PW-functional) ((boa :initform 5 :accessor boa)))
;;(macroexpand (defmethodp fifi C-fru ((x fix) (y fix)) fix "fu" (+ x y  (boa self))))

;;;; THE END ;;;;
