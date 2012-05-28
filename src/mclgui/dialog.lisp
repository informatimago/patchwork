;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               dialog.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Dialogs.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-17 <PJB> Created.
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



(defmacro %get-default-button (window)
  `(view-get ,window '%default-button))

(defmacro %get-cancel-button (window)
  `(view-get ,window '%cancel-button))



(defgeneric default-button (window)
  (:documentation "
The DEFAULT-BUTTON generic function returns the current default
button, or NIL if the window has no default button.  The default button
is the button whose action is run when the user presses Return or
Enter. It is outlined with a heavy black border.

If carriage returns are allowed in the current editable-text item,
they are sent to that item rather than to the default button.
")
  (:method ((window window))
    (%get-default-button window)))


(defgeneric set-default-button (window new-button)
  (:documentation "
The set-default-button generic function changes the default button
according to the value of new-button and returns new-button.  If
carriage returns are allowed in the current editable-text item, they
are sent to that item rather than to the default button.

WINDOW:         A window.

NEW-BUTTON:     The button that should be made the default button, or
                NIL, indicating that there should be no default button.
")
  (:method ((dialog window) new-button)
    (let ((default-button (%get-default-button dialog)))
      (unless (eq default-button new-button)
        (without-interrupts
            (when default-button
              (invalidate-view-border default-button t)
              (niy set-default-button window new-button)
              #-(and)
              (#_setwindowdefaultbutton (wptr dialog) (%null-ptr)))
          (setf (%get-default-button dialog) new-button)
          (when new-button
            (when (dialog-item-handle new-button)
              (if (dont-throb new-button) ;(and (osx-p)(neq (view-container new-button)(view-window new-button)))
                  nil  ;; so we act like a default button but dont look like one
                  (niy set-default-button window new-button)
                  #-(and)
                  (#_setwindowdefaultbutton (wptr dialog) (dialog-item-handle new-button))))
            (invalidate-view-border new-button)))))
    new-button))


(defgeneric default-button-p (item)
  (:documentation "
The DEFAULT-BUTTON-P generic function returns true if item is the
default button in the view-window of ITEM.  Otherwise it returns NIL.
")
  (:method ((item default-button-mixin))
    (let ((window (view-window item)))
      (and window (eq item (default-button window))))))




(defgeneric cancel-button (window)
  (:documentation "
")
  (:method ((window window))
    (%get-cancel-button window)))


(defgeneric set-cancel-button (window new-button)
  (:documentation "")
  (:method ((window window) new-button)
    (setf (%get-cancel-button window) new-button)))






key-handler-mixin



(defgeneric look-for-a-button-named-cancel (window))
(defgeneric press-button (dialog-item))









(defun message-dialog (message &key ok-text (size #@(330 110)) position)
  "
The MESSAGE-DIALOG function displays a dialog box containing the
string message and a single button.  The function returns T when the
user clicks this button or presses Return or Enter.

MESSAGE:        A string to be displayed as the message in the dialog box.

OK-TEXT:        The text to be displayed in the button. The default
                button text is OK. If the text is too long, this
                string is clipped (that is, the button is not enlarged
                to accommodate the longer string). You can set the
                size with the :size keyword.

SIZE:           The size of the dialog box. The default size is #@(335
                100).  A larger size provides more room for text.

POSITION:       The position of the dialog box. The default position
                is the top center of the screen.
"
  (niy message-dialog message ok-text size position)
  (format *query-io* "~&~72,,,'-<~>~%Message: ~A~%~72,,,'-<~>~%" message)
  (force-output  *query-io*))











(eval-when (eval compile load)

(defmacro with-item-rect ((var the-item) &body body)
  (let ((pos (gensym))
        (size (gensym))
        (item (gensym)))
    `(let* ((,item ,the-item)
            (,pos (view-position ,item))
            (,size (view-size ,item)))
       (rlet ((,var :rect :topleft ,pos :bottomright (add-points ,pos ,size)))
         ,@body))))
)



(defclass dialog (window) 
  ()
  (:default-initargs 
   :window-title "Untitled Dialog"
   :window-type :document))

(defmethod view-default-font ((view dialog))
  (sys-font-spec)
  ;'("chicago" 12 :plain)
  )

(defmethod view-default-size ((dialog dialog)) #@(300 200))
(defmethod view-default-position ((dialog dialog)) '(:top 100))

(defmacro %get-current-key-handler (window)
  `(view-get ,window '%current-key-handler))

(defmethod current-key-handler ((w window))
  (%get-current-key-handler w))

(defmethod current-key-handler ((w (eql nil)))  nil)

(defmacro %get-key-handler-list (window)
  `(view-get ,window '%key-handler-list))

(defmethod key-handler-list ((view simple-view))
  (let ((w (view-window view)))
    (and w (%get-key-handler-list w))))


(defmethod editing-dialogs-p ((window t))
  nil)

(defclass color-dialog (dialog)
  ()
  (:default-initargs :color-p t))




(defvar *first-menustate* nil)

#-(and)
(defmethod modal-dialog ((dialog window) &optional (close-on-exit t) eventhook 
                           &aux ret)
  (#_FlushEvents #xfff7 0)
  (let ((eventhook
         #'(lambda (&aux (event *current-event*)
                         (what (rref event eventrecord.what)))
             (when (and *modal-dialog-on-top* (eq dialog (caar *modal-dialog-on-top*)))
               (unless (wptr dialog)    ; this does nothing if *modal-dialog-on-top* is nil
                 (return-from-modal-dialog :cancel))
               (when (wptr dialog)  ; it may be gone
                 (when (not *in-foreign-window*)  ;; added 05/24/05
                   (unless (eq (window-layer dialog) 0)
                     (set-window-layer dialog 0)))
                 (if (and eventhook
                          (if (listp eventhook)
                            (dolist (f eventhook)
                              (when (funcall f) (return t)))
                            (funcall eventhook)))
                   t                   
                   (if (eq #$mouseDown what)
                     (%stack-block ((wp 4))
                       (let* ((code (#_FindWindow (rref event eventrecord.where) wp)))
                         (cond 
                          ((eq code #$inMenubar) nil)
                          ((%ptr-eql (wptr dialog) (%get-ptr wp))
                           nil)                          
                          (t  (#_Sysbeep 5) t))))
                     (if nil ; (or (eq what #$keyDown) (eq what #$autoKey))
                       (when (menukey-modifiers-p (rref event eventrecord.modifiers))
                         (ed-beep)
                         t))))))))
        (*interrupt-level* 0)
        (old-modal-dialog *modal-dialog-on-top*)
        #+REMOVE (old-window-process (window-process dialog))
        )
    (declare (dynamic-extent eventhook))
    (progn ;let-globally ()        
      ;(declare (special *processing-events*))
      
      (setq *processing-events* nil)
      (let ()
        (unwind-protect
          (with-focused-view nil
            (with-cursor 'cursorhook 
              (setq ret (multiple-value-list
                         (restart-case
                           (catch '%modal-dialog
                             (progn ; (let-globally (*modal-dialog-process* *current-process*))
                               #+carbon-compat
                               (let ((wptr (wptr dialog)))  ;; make it modal now
                                 (when wptr (#_changewindowattributes wptr 0 #$kWindowCollapseBoxAttribute)) ;; lose collapse box
                                 (when (and wptr) ; (wptr-dialog-p wptr)) ;(FIND-CLASS 'DRAG-RECEIVER-DIALOG NIL)(typep dialog 'drag-receiver-dialog)) ;; ??
                                   ;(PUSH (LIST 'ONE (GETWINDOWCLASS WPTR)(WINDOW-LAYER DIALOG) DIALOG) barf)
                                   (#_setwindowclass wptr #$kMovableModalWindowClass) ; CHANGES WINDOW-LAYER
                                   (setwindowmodality wptr #$kWindowModalityAppModal)
                                   ;(PUSH (LIST 'TWO (GETWINDOWCLASS WPTR) (WINDOW-LAYER DIALOG) DIALOG) barf)
                                   ))
                               (set-window-layer dialog 0)
                               ;(#_hidefloatingwindows)
                               
                               #+REMOVE (setf (window-process dialog) *current-process*)  ; do this first
                               (setq *modal-dialog-on-top* (cons (list dialog *current-process* eventhook) *modal-dialog-on-top*)
                                     ;*eventhook* eventhook
                                     )
                               (when (not old-modal-dialog)
                                 (setq *first-menustate* (update-menus :disable))
                                 )
                               
                               (window-show dialog)
                               ;(setq ms (update-menus :disable))
                               (loop
                                 (when t ;(eq *current-process* *event-processor*)  ;; 05/24/05
                                   (process-wait "Event-poll" #'event-available-p))
                                 (event-dispatch))))
                           (abort () :cancel)
                           (abort-break () :cancel))))
              (if (eq (car ret) :cancel)
                (throw-cancel :cancel)
                (apply #'values ret))))
          (without-interrupts  ; << maybe this helps - not really
           (without-event-processing ; delay events until the window-close is over
             #+REMOVE (setf (window-process dialog) old-window-process)
             ; if this one is still on top reset to nil, else leave alone
             (setq *modal-dialog-on-top* (nremove (assq dialog *modal-dialog-on-top*) *modal-dialog-on-top*))
             (let ((mdot *modal-dialog-on-top*))
               (when mdot
                 (when (not (wptr (caar mdot)))
                   (setq *modal-dialog-on-top* (cdr *modal-dialog-on-top*)))))
             ;(setq *eventhook* nil)  ; kill the same bug 2 ways.             
             ; moved update-menus back to after window-close - fixes do-about-dialog when carbon (weird)
             (if close-on-exit
               (window-close dialog)
               (progn (window-hide dialog)
                      (set-window-layer dialog 9999)))
             (when (not *modal-dialog-on-top*)(update-menus :enable *first-menustate*)
                   ;(#_showfloatingwindows)
                   ))))))))


(defmacro return-from-modal-dialog (form)
  `(multiple-value-call '%return-from-modal-dialog ,form))

#|
;; in case clim patch to modal dialog is loaded
(defun modal-dialog-process (thing)
  (when thing
    (let ((p (cdr thing)))
      (if (consp p) (car p) p))))
|#

(defun %return-from-modal-dialog (&rest values)
  (declare (dynamic-extent values))
  ;(when  (not *modal-dialog-on-top*) (dbg 2))
  (when *modal-dialog-on-top* ; << maybe its gone or not set yet
    (let ((process (modal-dialog-process (car *modal-dialog-on-top*))))
      (when process
        (apply #'process-interrupt
               process
               #'(lambda (&rest values)
                   (declare (dynamic-extent values))
                   (throw '%modal-dialog (apply #'values values)))
               values)))))

(defmethod window-close :before ((dialog window))
  (when (wptr dialog)
    (when (assq dialog *modal-dialog-on-top*)
      (return-from-modal-dialog :closed))))

(defmethod window-close :after ((w window))
  (setf (slot-value w 'my-item) nil)
  (let ((wm *windows-menu*))
    (when (and (typep wm 'menu) (menu-enabled-p wm))
      (update-windows-menu wm))
    (let ((em (edit-menu)))
      (when em (menu-update em)))))







(defmethod installed-item-p (item)
  (let ((dialog (view-container item)))
    (and dialog (wptr dialog))))



(defmethod window-null-event-handler :before ((dialog window))
  (let ((item (current-key-handler dialog)))
    (when item
      (key-handler-idle item dialog))))

(defmethod key-handler-idle ((item simple-view) &optional dialog)
  (declare (ignore dialog))
  )


; Overwrites method in l1-windows
(defmethod view-key-event-handler ((window window) char &aux 
                                   (key-hdlr (current-key-handler window))
                                   ;(d-button (default-button window))
                                   )  
  (when (and (eql char #\esc) (not (any-modifier-keys-p))) ; (not key-hdlr))
    (let ((cancel-button (cancel-button window)))
      (if cancel-button
        (when (dialog-item-enabled-p cancel-button)
          (press-button cancel-button)
          (return-from view-key-event-handler char))
        (when (not key-hdlr) ;; ??  -        
          (let ((x (look-for-a-button-named-cancel window)))
            (when (and x (dialog-item-enabled-p x))
              (press-button x)
              (return-from view-key-event-handler char)))))))      
  (unless (or (any-modifier-keys-p)
              (and key-hdlr 
                   (eq (fred-shadowing-comtab key-hdlr) ;; nil if not fred-mixin
                       *control-q-comtab*)))
    (case char
      (#\tab
       (unless (and key-hdlr (allow-tabs-p key-hdlr))
         (change-key-handler window)
         (setq key-hdlr nil)))
      ((#\return :enter)
       (let ((d-button (default-button window)))
         (cond
          ((and (eql char #\return)
                key-hdlr
                (or (allow-returns-p key-hdlr) (setq key-hdlr nil))))
          ((and d-button (dialog-item-enabled-p d-button))
           (press-button d-button)
           (setq key-hdlr nil)))))))
  (if key-hdlr
    (view-key-event-handler key-hdlr char)
    (if *top-listener*
      (view-key-event-handler *top-listener* char))))



(defmacro do-subviews ((subview-var view &optional subview-type)
                       &body body)
  (let* ((type-var-p (not (or (symbolp subview-type) (constantp subview-type))))
         (type-var (if type-var-p (gensym)))
         (subviews-var (gensym))
         (len-var (gensym))
         (i (gensym))
         (subviews-copy-var (gensym)))
    `(with-managed-allocation
       (let* (,@(if type-var-p `((,type-var ,subview-type)))
              (,subviews-var (view-subviews ,view))
              (,len-var (length ,subviews-var))
              (,subviews-copy-var (%make-temp-uvector ,len-var))
              ,subview-var)
         (declare (fixnum ,len-var))
         (dotimes (,i ,len-var)
           (setf (%svref ,subviews-copy-var ,i)
                 (aref ,subviews-var ,i)))
         (dotimes (,i ,len-var)
           (setq ,subview-var (%svref ,subviews-copy-var ,i))
           (when ,(if subview-type
                    `(typep ,subview-var ,(if type-var-p type-var subview-type))
                    t)
             ,@body))))))

(defmethod map-subviews ((view view) function &optional subview-type)
  (if subview-type
    (do-subviews (subview view subview-type)
      (funcall function subview))
    (do-subviews (subview view)
      (funcall function subview))))

(defmethod subviews ((view simple-view) &optional subview-type)
  (declare (ignore subview-type))
  nil)

(defmethod subviews ((view view) &optional subview-type)
  (let ((res nil))
    (let* ((add-em #'(lambda (subview)  (push subview res))))
      (declare (dynamic-extent add-em))
      (map-subviews view add-em subview-type))
    (nreverse res)))

(defmethod find-subview-of-type ((view view) subview-type)
  (let ((subs (view-subviews view)))
    (when subs 
      (dotimes (i (length subs))
        (let ((it (aref subs i)))
          (when (typep  it subview-type)
            (return it)))))))

(defmethod find-subview-of-type ((view simple-view) subview-type)
  nil)
        





(defmethod set-current-key-handler ((dialog window) item &optional (select-all t)
                                      &aux old)
  (unless (or (null item)
              (and (memq item (%get-key-handler-list dialog))
                   (key-handler-p item)))
    (error "~s is either disabled or is not a key-handler item of ~s" item dialog))
  (without-interrupts
   (if (and (neq item (setq old (%get-current-key-handler dialog)))
            (if old 
              (when (exit-key-handler old item)
                (multiple-value-bind (s e) (selection-range old)
                  (declare (ignore s))
                  ; do this first else display may be wrong.
                  (set-selection-range old e e))
                (setf (%get-current-key-handler dialog) nil) ;; << !! so frame.. knows
                (view-deactivate-event-handler old)
                t)
              t))
     (progn
       (setf (%get-current-key-handler dialog) item)
       (when item
         (when select-all
           (set-selection-range item 0 most-positive-fixnum))
         (if (window-active-p dialog)
           (view-activate-event-handler item))
         (enter-key-handler item old)))
     (when (and item (eq item old) select-all)
       (set-selection-range item 0 most-positive-fixnum))))
  item)

;Check for a view-key-event-handler method?
(defmethod key-handler-p ((item dialog-item))
  nil)

(defmethod key-handler-p ((item key-handler-mixin))
  (or (not (method-exists-p #'dialog-item-enabled-p item))
       (dialog-item-enabled-p item)))

(defmethod change-key-handler ((view view))
  (let* ((dialog (view-window view))
         (items (%get-key-handler-list dialog))
         (old-handler (current-key-handler dialog))
         (rest (memq old-handler items)))
    (set-current-key-handler 
     dialog
     (or 
      (dolist (x (cdr rest))
        (if (key-handler-p x)(return x)))
      (dolist(x items)
        (if (key-handler-p x)(return x)))))))

#|
(defmethod cut ((dialog window))
  (window-delegate-op dialog 'cut))

(defmethod copy ((dialog window))
  (window-delegate-op dialog 'copy))

(defmethod paste ((dialog window))
  (window-delegate-op dialog 'paste))

(defmethod clear ((dialog window))
  (window-delegate-op dialog 'clear))

(defmethod undo ((w window))
  (window-delegate-op w 'undo))

(defmethod undo-next ((w window))
  (window-delegate-op w 'undo-next))
|#








;ffing stupid - doc'd but unused
(defmethod find-dialog-item ((view view) text)
  (dovector (item (view-subviews view))
    (if (and (typep item 'dialog-item) (equalp text (dialog-item-text item)))
        (return item))))






(defmethod view-convert-coordinates-and-click ((item dialog-item) where container)
  (when (dialog-item-enabled-p item)
    (with-focused-dialog-item (item container)
      (view-click-event-handler item where))))




;;;Button dialog items
;;;default-button dialog-items



(defmethod clip-inside-view ((item simple-view) &optional (h #@(1 1)) v)
  (let* ((p (make-point h v))
         (ul (view-position item))
         (size (view-size item))
         lr)
    (when (and ul size)
      (psetq ul (add-points ul p)
             lr (subtract-points (add-points ul size) p))
      (niy clip-inside-view item h v)
      #-(and)
      (rlet ((rect :rect :topleft ul :bottomright lr))
        (#_ClipRect rect)))))







;;;Radio Button dialog items



;;;;;;;;;; 
;; draw-theme-text-box
 ;; moved from pop-up-menu.lisp

(defun current-pixel-depth ()
  (niy current-pixel-depth)
  32
  #-(and)
  (with-port-macptr port
    (with-macptrs ((portpixmap (#_getportpixmap port)))
      (href portpixmap :pixmap.pixelsize))))

(defun current-port-color-p ()
  (niy current-port-color-p)
  (make-color 65535 65535 65535)
  #-(and) (with-port-macptr port
            (#_isportcolor port)))


(defun draw-theme-text-box (text rect &optional (text-justification :center) truncwhere (active-p t))
  ;; could add a truncate option and use TruncateThemeText
  (let ((start 0) 
        (end (length text)))
    (when (not (simple-string-p text))
      (multiple-value-setq (text start end) (string-start-end text start end)))
    (niy draw-theme-text-box text rect text-justificiation truncwhere active-p)
    #-(and)
    (when (not (fixnump text-justification))
      (setq text-justification
            (case text-justification
              (:center #$tejustcenter)
              (:left #$tejustleft)
              (:right #$tejustright)
              (t #$tejustcenter))))
    #-(and)
    (with-theme-state-preserved
        (when (not active-p)  ;; let it have it's own color if active
          (#_SetThemeTextColor #$kThemeTextColorDialogInactive
                               (current-pixel-depth) (current-port-color-p)))
      (let ((len (- end start)))
        (%stack-block ((the-chars (%i+ len len)))
                      (copy-string-to-ptr text start len the-chars)          
                      (if (not truncwhere)
                          (with-macptrs ((cfstr (#_CFStringCreatewithcharacters (%null-ptr) the-chars len))) 
                            (#_Drawthemetextbox cfstr #$kThemeCurrentPortFont #$Kthemestateactive t rect text-justification (%null-ptr))
                            (#_CFRelease cfstr))
                          (progn
                            (setq truncwhere
                                  (case truncwhere
                                    (:end #$truncend)
                                    (:middle #$truncmiddle)
                                    (t #$truncend)))              
                            (with-macptrs ((cfstr (#_CFStringCreateMutable (%null-ptr) 0)))
                              (#_CFStringAppendCharacters cfstr the-chars len)                
                              (unwind-protect
                                   (progn
                                     (rlet ((foo :boolean))
                                           (errchk (#_TruncateThemeText cfstr #$kThemeCurrentPortFont #$Kthemestateactive
                                                                        (- (pref rect :rect.right)(pref rect :rect.left))
                                                                        truncwhere
                                                                        foo)))
                                     (#_DrawThemetextbox cfstr #$kThemeCurrentPortFont #$Kthemestateactive t rect text-justification (%null-ptr)))
                                (#_cfrelease cfstr))))))))))




(defun grafport-font-codes-with-color ()
  (niy grafport-font-codes-with-color)
  #-(and)
  (multiple-value-bind (ff ms)(grafport-font-codes)
    (let* ((foo (grafport-fore-color))) ;; 0 is black is 0    
      (if (neq foo 0)(setq ff (logior (logand ff (lognot #xff)) (fred-palette-closest-entry foo))))
      (values ff ms))))

(defun color->ff-index (color)
  (niy color->ff-index color)
  #-(and)
   (if (and color (neq color *black-color*))
     (fred-palette-closest-entry color)
     0))




;; don't errchk - may get -8808 


;; (defconstant #$kATSULineBreakInWord -8808)
;; ;; This is not an error code but is returned by ATSUBreakLine to

;;     indicate that the returned offset is within a word since there was
;;     only less than one word that could fit the requested width.

(defun atsu-line-break-given-layout (layout start width)
  ;;  *    oLineBreak:
  ;;  *      On return, the value specifies the soft line break as
  ;;  *      determined by ATSUBreakLine. If the value returned is the same
  ;;  *      value as specified in iLineStart , you have made an input
  ;;  *      parameter error. In this case, check to make sure that the line
  ;;  *      width specified in iLineWidth is big enough for ATSUBreakLine
  ;;  *      to perform line breaking. ATSUBreakLine does not return an
  ;;  *      error in this case.
  (niy atsu-line-break-given-layout layout start width)
  #-(and)
  (rlet ((outoff :ptr))
        (#_atsubreakline layout start (#_long2fix width) t outoff)
        (let ((res (%get-unsigned-long outoff)))
          (if (eql res start)(error "phooey"))
          res)))

(defun draw-string-in-rect (string rect &key
                                   truncation justification compress-p
                                   (start 0)(end (length string))
                                   ff ms color)
  (when (not (and ff ms))
    (multiple-value-setq (ff ms) (grafport-font-codes-with-color)))
  (when color
    (setq ff (logior (logand ff (lognot #xff)) (color->ff-index color))))
  (when (not (simple-string-p string))
    (multiple-value-setq (string start end) (string-start-end string start end)))
  (multiple-value-bind (line-ascent descent width leading)(font-codes-info ff ms)
    (declare (ignore width))
    (niy draw-string-in-rect string rect truncation justification compress-p start end ff ms color)
    #-(and)
    (with-clip-rect-intersect rect ;; can we assume callers have done this? - nah let callers assume done here
      (let* ((numchars (- end start))
             (hpos (pref rect :rect.left))
             (vpos (pref rect :rect.top))
             (max-width (- (pref rect :rect.right) hpos)))
        (unless (eq numchars 0)
          (%stack-block ((ubuff (%i+ numchars numchars)))
            (copy-string-to-ptr string start numchars ubuff)
            (with-atsu-layout (layout ubuff numchars ff ms)
              (when (and truncation (neq truncation :none))
                (set-layout-line-truncation-given-layout layout truncation (null compress-p))) ;; aha need no-squash-p                
              (set-layout-line-width-given-layout layout max-width)
              (when justification  ;; doesnt work - fixed now
                (set-layout-line-justification-given-layout layout justification))
              (cond
               ((and truncation (neq truncation :none))
                (errchk (#_atsudrawtext layout 0 numchars
                         (#_long2fix hpos)
                         (#_long2fix (%i+ vpos line-ascent)))))
               (t
                (let* ((line-height (%i+ line-ascent descent leading))
                       (rect-height (- (pref rect :rect.bottom) vpos))
                       (now-height 0)
                       (my-start 0))                      
                  (loop
                    (let ((next (atsu-line-break-given-layout layout my-start max-width)))
                      ;(cerror "g" "h ~a ~a ~A ~a" my-start numchars next (- next my-start))
                      (errchk (#_atsudrawtext layout my-start (- next my-start)
                               (#_long2fix hpos)
                               (#_long2fix (%i+ vpos line-ascent))))
                      (setq my-start next)
                      (when (%i>= my-start numchars)(return))
                      (setq now-height (%i+ now-height line-height))
                      (when (%i>= now-height rect-height)(return))
                      (setq vpos (%i+ vpos line-height))))))))))))
    nil))



;;;; THE END ;;;;
