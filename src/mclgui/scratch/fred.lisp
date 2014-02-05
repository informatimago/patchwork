;;;---------------------------------------------------------------------
;;;

(defclass tsm-document-mixin ()
  ())

(defclass fred-mixin ()
  ())

(defclass fred-dialog-item (focus-rect-mixin tsm-document-mixin fred-mixin basic-editable-text-dialog-item)
  ()
  (:default-initargs :buffer-chunk-size 128
                     :history-length 2
                     :text-edit-sel-p t
                     :save-buffer-p t))


(defmethod install-view-in-window ((view fred-dialog-item) window)
  (call-next-method)
  (when (null (getf (slot-value view 'color-list) :body nil))
    (set-part-color view :body *white-color*)))


(defmethod initialize-instance :around ((item fred-dialog-item)  &rest args &key view-container )
  ;; fix for (make-instance 'fred-dialog-item :view-container w) - need frec etal before set-view-container
  (declare (dynamic-extent args))
  (if (not view-container)
      (call-next-method)
      (progn
        (apply #'call-next-method item :view-container nil args)
        (set-view-container item view-container))))


(defmethod initialize-instance :after  ((item fred-dialog-item) &key 
                                buffer filename dialog-item-text (margin 1)
                                (view-font (view-default-font item)))
  (when view-font (set-view-font item view-font))
  (let ((color (part-color item :text)))
    (when color 
      (let ((ff-color (color->ff-index color)))
        (multiple-value-bind (ff ms)(view-font-codes item)
          (set-view-font-codes item (logior ff ff-color) ms)))))        
  (let ((frec (frec item)))
    (setf (fr.margin frec) margin            ; Just room for the selection box
          (fr.hpos frec) margin))
  (when (or (and (null buffer)(null filename)) dialog-item-text)
    (let* ((text (slot-value item 'dialog-item-text))
           (buf (fred-buffer item)))
      ;(buffer-delete buf (buffer-size buf) 0)
      (when text
        (buffer-insert-substring buf text 0 (length text)))))
  (setf (slot-value item 'dialog-item-text) nil))


(defmethod set-default-size-and-position ((item fred-dialog-item) &optional container)
  (declare (ignore container))
  (let ((size (view-size item)))    
    (call-next-method)
    (unless (eql size (setq size (view-size item)))
      (with-focused-view item
        (frec-set-size (frec item) size)))))


(defmethod view-convert-coordinates-and-click ((item fred-dialog-item) where container)
  (view-click-event-handler item (convert-coordinates where container item)))


(defmethod view-click-event-handler ((item fred-dialog-item) where)
  (declare (ignore where))
  (when (dialog-item-enabled-p item)
    (call-next-method)))
    
(defmethod view-activate-event-handler ((item fred-dialog-item))
  (when (eq item (current-key-handler (view-window item)))
    (call-next-method)))

;; view-deactivate-event-handler for fred-dialog-item uses the one for fred-mixin.


(defmethod dialog-item-disable :before ((item fred-dialog-item))
  (when (dialog-item-enabled-p item)
    (set-selection-range item 0 0)
    (view-deactivate-event-handler item)))



;; Is an around method to avoid the after method for dialog item
;; which invalidates the border as well as the contents.

(defmethod set-dialog-item-text :around ((item fred-dialog-item) text)
  (if (slot-value item 'dialog-item-text)
    (setf (slot-value item 'dialog-item-text) text)
    (let ((buf (fred-buffer item)))
      (buffer-delete buf (buffer-size buf) 0)
      (buffer-insert-substring buf text)))
  (let ((container (view-container item)))
    (when (and container (wptr item))
      (let ((pos (view-position item)))
        (invalidate-corners container pos (add-points pos (view-size item))))))
  text)
    

(defmethod dialog-item-text ((item fred-dialog-item))
  (or (slot-value item 'dialog-item-text)
      (let ((buf (fred-buffer item)))
        (buffer-substring buf (buffer-size buf) 0))))


(defmethod view-draw-contents ((item fred-dialog-item))
  (unless (view-quieted-p item)
    (let* ((enabled-p (dialog-item-enabled-p item))
           (colorp (color-or-gray-p item)))
      (with-focused-view item
        (with-fore-color (if (and colorp (not enabled-p))
                           *gray-color*
                           (part-color item :text))
          (with-back-color (part-color item :body)              
            (frec-draw-contents (frec item))
            ))))))


(defmethod part-color ((item fred-dialog-item) key)
  (or (getf (slot-value item 'color-list) key nil)
      (case key (:body *white-color*))))


; This is called by all the editor commands.
(defmethod fred-update ((item fred-dialog-item))
  (unless (or (view-quieted-p item) (not (wptr item)))
    (call-next-method))
  (when (wptr item)
    (let ((modcnt (buffer-modcnt (fred-buffer item))))
      (unless (eq modcnt (view-get item 'action-modcnt)) ; what is this
        ; formerly the action was invoked within with-focused-view - now it isn't - that may be bad
        (setf (view-get item 'action-modcnt) modcnt)
        (dialog-item-action item)))))


(defmacro with-text-colors (item &body body)
  (let ((item-var (gensym)))
    `(let ((,item-var ,item))
       (with-fore-and-back-color (part-color ,item-var :text)
                                 (or (part-color ,item-var :body) *white-color*)
         ,@body))))


(defmethod set-selection-range :after ((item fred-dialog-item) &optional start end)
  (declare (ignore start end))
  (with-focused-view item
    (with-text-colors item
      (frec-update (frec item)))))



