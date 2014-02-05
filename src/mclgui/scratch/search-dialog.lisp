

;;;Search Dialog

(defvar *search-default* "")
(defvar *replace-default* "")
(defclass search-dialog (string-dialog) ())
;(defvar *search-dialog* nil)
(defvar *search-dialog-pos* '(:bottom 130))

(defun search-window-dialog (&aux (search-dialog (front-window :class 'search-dialog)))
  #-bccl (require 'fred-misc)
  (unless search-dialog
    (setq search-dialog
          (make-instance 'search-dialog
                         :window-title "String Search"
                         :view-position *search-dialog-pos*
                         :back-color *tool-back-color*
                         :theme-background t
                         :allow-empty-strings '(replace-text-item)
                         :view-size #@(374 108)
                         :help-spec 12081
                         :window-show nil))
    (add-subviews
     search-dialog
     (make-dialog-item 'default-button-dialog-item
                       #@(7 59) #@(114 18) "Search"                       
                       (lambda (item) (do-search (view-container item) :forward))
                       :help-spec 12084)
     (make-dialog-item 'button-dialog-item
                       #@(131 59)  #@(114 18) "Reverse"
                       (lambda (item) (do-search (view-container item) :reverse))
                       :view-nick-name 'reverse-search
                       :help-spec 12085)
     (make-dialog-item 'button-dialog-item
                       #@(255 59) #@(114 18) "From Top"
                       (lambda (item) (do-search (view-container item) :from-top))
                       :help-spec 12086)
     (make-dialog-item 'button-dialog-item
                       #@(7 84)  #@(114 18) "Replace"
                       (lambda (item) (do-replace (view-container item)))
                       :help-spec 12087
                       :view-nick-name 'replace-button)
     (make-dialog-item 'button-dialog-item
                       #@(131 84) #@(114 18) "Replace/Search"
                       (lambda (item)
                           (let ((my-dialog (view-container item)))
                             (do-replace my-dialog)
                             (do-search my-dialog :forward)))
                       :help-spec 12088
                       :view-nick-name 'replace/find-button)
     (make-dialog-item 'button-dialog-item
                       #@(255 84) #@(114 18) "Replace All"
                       (lambda (item)
                           (do-replace-all (view-container item)))
                       :view-nick-name 'replace-all
                       :help-spec 12089)
     (make-dialog-item 'static-text-dialog-item
                       #@(10 8) #@(94 16) "Search For:" nil :help-spec 12081)
     (make-dialog-item 'editable-text-dialog-item
                       #@(107 8) #@(251 16) *search-default* nil
                       :view-nick-name 'search-text-item
                       :help-spec 12082)
     (make-dialog-item 'static-text-dialog-item
                       #@(10 34) #@(94 16)  "Replace With:" nil
                       :help-spec 12081)
     (make-dialog-item 'editable-text-dialog-item
                       #@(107 34) #@(251 16) *replace-default* nil
                       :view-nick-name 'replace-text-item
                       :help-spec 12083)))
  (window-select search-dialog))

(defmethod update-default-button ((d search-dialog))
  (call-next-method)
  (set-dialog-item-enabled-p (view-named 'reverse-search d)
                             (dialog-item-enabled-p (default-button d)))
  (set-dialog-item-enabled-p (view-named 'replace-all d)
                             (dialog-item-enabled-p (default-button d))))

(defun do-search (w search-type)
  (let ((text (dialog-item-text (view-named 'search-text-item w)))
        (sw (target)))
    (when (eq search-type :from-top)
      (window-top sw))
    (window-search sw text (eq search-type :reverse)))
  (enable-replace w))

(defun do-replace (w)
  (let ((text (dialog-item-text (view-named 'replace-text-item w)))
        (sw (target)))
    (window-replace sw text))
  (enable-replace w))

(defun do-replace-all (w &aux
                         (search-text  (dialog-item-text (view-named 'search-text-item  w)))
                         (replace-text (dialog-item-text (view-named 'replace-text-item w)))
                         (search-win (target)))
  (when (and search-win 
             (> (length search-text) 0))
    (window-replace-all search-win search-text replace-text 0)
    (enable-replace w)))

(defmethod window-close :before ((w search-dialog))
  (setq *search-default*    (dialog-item-text (view-named 'search-text-item w))
        *replace-default*   (dialog-item-text (view-named 'replace-text-item w))
        *search-dialog-pos* (view-position w)
        ;*search-dialog* nil)
        ))

(defun enable-replace (w)
  (let ((sw (target))
        (b 0)
        (e 0))
    (when (and sw (method-exists-p 'selection-range sw))
      (multiple-value-setq (b e) (selection-range sw)))
    (let ((enable-p (/= b e)))
      (dolist (name '(replace-button replace-all replace/find-button))
        (set-dialog-item-enabled-p (view-named name w) enable-p)))))

(defmethod view-activate-event-handler :after ((w search-dialog))
  (let ((text (view-named 'search-text-item w)))
    (set-current-key-handler w text)
    (set-selection-range text 0 32000)
    (update-default-button w)
    (let ((enable-p (> (dialog-item-text-length text) 0)))
      (dolist (name '(replace-button  replace/find-button))
        (dialog-item-disable (view-named name w)))
      (set-dialog-item-enabled-p (view-named 'replace-all w) enable-p))))

#|
; Default's for search stuff, pass through to key-handler
(defun funcall-key-handler (window gf beep-p &rest args)
  (declare (dynamic-extent args))
  (apply #'funcall-if-method-exists 
         gf (and beep-p #'ed-beep) (current-key-handler window) args))
|#


; This should really be merged with window-do-operation
(defun maybe-apply-to-key-handler (window gf &rest args)
  (declare (dynamic-extent args))
  (let ((thing (current-key-handler window)))
    (if (or (not thing)
            (or (not gf) (not (apply 'method-exists-p gf thing args))))
      (progn (ed-beep) (cancel))
      (apply gf thing args))))

(defmethod window-top ((w window))
  (maybe-apply-to-key-handler w 'window-top))

(defmethod window-search ((w window) text &optional reverse-p silent-p)
  (maybe-apply-to-key-handler w 'window-search text reverse-p silent-p))

(defmethod selection-range ((w window))
  (selection-range (current-key-handler w)))

(defmethod window-replace ((w window) text)
  (maybe-apply-to-key-handler w 'window-replace text))

(defmethod window-replace-all ((w window) search-text replace-text &optional start)
  (maybe-apply-to-key-handler w 'window-replace-all search-text replace-text start))
