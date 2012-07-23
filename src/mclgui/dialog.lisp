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

(defclass dialog (window) 
  ()
  (:default-initargs 
   :window-title "Untitled Dialog"
   :window-type :document))

(defmethod view-default-font ((view dialog))
  (sys-font-spec))

(defmethod view-default-size ((dialog dialog)) #@(300 200))
(defmethod view-default-position ((dialog dialog)) '(:top 100))






(defmacro %get-cancel-button (window)
  `(view-get ,window '%cancel-button))


;; The specifications of catch-cancel and throw-cancel in 3.0/4.0 are
;; contradictory.  We just use the implementation in mcl 5.1

(defmacro catch-cancel (&body body)
  "
The catch-cancel macro sets up a cancel catch and evaluates form. It
returns the value of the last form if there was no cancel throw. Otherwise,
it returns the symbol :cancel.
"
  `(catch :cancel ,@body))


(defmacro throw-cancel (&optional value)
  "
The throw-cancel macro throws the value of value-form to the most
recent outstanding catch-cancel.
"
  `(throw :cancel ,value))



(defgeneric cancel-button (window)
  (:documentation "
")
  (:method ((window window))
    (%get-cancel-button window)))


(defgeneric set-cancel-button (window new-button)
  (:documentation "")
  (:method ((window window) new-button)
    (setf (%get-cancel-button window) new-button)))


(defgeneric look-for-a-button-named-cancel (window))




(defmacro %get-current-key-handler (window)
  `(view-get ,window '%current-key-handler))

(defgeneric current-key-handler (window)

  (:method ((window window))
    (%get-current-key-handler window))

  (:method ((window null))
    nil))


(defmacro %get-key-handler-list (window)
  `(view-get ,window '%key-handler-list))


(defmethod key-handler-list ((view simple-view))
  (let ((window (view-window view)))
    (and window (%get-key-handler-list window))))





(defun modal-dialog-eventhook (thing)
  (and thing
       (consp (cdr thing))
       (third thing)))


(defun modal-dialog-process (thing)
  (when thing
    (let ((p (cdr thing)))
      (if (consp p)
          (car p)
          p))))


(defun process-event (event)
  (niy process-event)
  #-(and)
  (let ((e-code (pref event :eventrecord.what)))
    (when (eq e-code $MButDwnEvt) 
      (process-multi-clicks event) 
      ;; attempt to workaround OSX bug re leaking double-clicks - doesn't help.
      (if (and #|(osx-p)|# (%i> *multi-click-count* 1)) (#_FlushEvents #$mUpMask 0)))
    (let* ((*current-event* event))
      (declare (special *current-event* *processing-events*))
      (block foo
        (with-restart *event-abort-restart*
          (let ((eventhook (or (and *modal-dialog-on-top*
                                    (wptr (caar *modal-dialog-on-top*)) ;; blech
                                    (modal-dialog-eventhook (car *modal-dialog-on-top*)))
                             *eventhook*)))
            (unless (and eventhook
                         (flet ((process-eventhook (hook)
                                  (unless (member hook *eventhooks-in-progress*)
                                    (let ((*eventhooks-in-progress*
                                           (cons hook *eventhooks-in-progress*)))
                                      (declare (dynamic-extent *eventhooks-in-progress*))
                                      (funcall hook)))))
                           (declare (inline process-eventhook))
                           (if (listp eventhook)
                             (dolist (item eventhook)
                               (when (process-eventhook item) (return t)))
                             (process-eventhook eventhook))))
              (return-from foo (catch-cancel (do-event)))))))
      e-code)))



(defvar *first-menustate* nil)


(defmethod window-close :before ((dialog window))
  (when (assoc dialog *modal-dialog-on-top*)
    (return-from-modal-dialog :closed)))



(defmethod modal-dialog ((dialog window) &optional (close-on-exit t) eventhook )
  "

The MODAL-DIALOG generic function displays dialog modally.  That is,
it makes dialog the active window, displays it, and then intercepts
subsequent user events until a RETURN-FROM-MODAL-DIALOG is executed.
The function returns the value(s) supplied by
RETURN-FROM-MODAL-DIALOG. 

If CLOSE-ON-EXIT is true (the default), the window is closed on exit;
otherwise, it is hidden. 

Closing the dialog box automatically prevents the accumulation of
numerous hidden windows during development.  Modal dialog boxes may be
nested. 

NOTE:           The body of MODAL-DIALOG is unwind protected, and so
                any throw past MODAL-DIALOG will close or hide the
                window, as appropriate. 


WINDOW:         A window. 

CLOSE-ON-EXIT:  An argument determining whether the window should be
                closed or simply hidden when the call to modaldialog
                returns.  If this argument is true, the window is
                closed.  If it is false, the window is hidden but not
                closed.  The default is T. 

EVENTHOOK:       A hook.  The function modal-dialog binds *EVENTHOOK*
                in order to intercept all event processing; this hook
                is provided so that you can perform any special event
                processing while the modal dialog is on the screen.
                The value of eventhook should be a function of no
                arguments, or a list of functions of no arguments.
                Whenever modal-dialog looks for events, it calls the
                functions in eventhook until one of them returns a
                non-NIL result.  If all of them return NIL,
                modal-dialog processes events as it normally would.
                Otherwise, it assumes that the hook function handled
                the event.  The variable *current-event* is bound to
                an event record for the current event when each hook
                function is called.  The default value of eventhook is
                NIL. 

"
  (niy modal-dialog dialog close-on-exit eventhook)
  #-(and)
  (#_FlushEvents #xfff7 0)
  #-(and)
  (let ((ret)
        (eventhook
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
      
      (setf *processing-events* nil)
      (let ()
        (unwind-protect
             (with-focused-view nil
               (with-cursor 'cursorhook 
                 (setf ret (multiple-value-list
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
                                    (setf *modal-dialog-on-top* (cons (list dialog *current-process* eventhook) *modal-dialog-on-top*)
                                        ;*eventhook* eventhook
                                          )
                                    (when (not old-modal-dialog)
                                      (setf *first-menustate* (update-menus :disable))
                                      )
                                    
                                    (window-show dialog)
                                        ;(setf ms (update-menus :disable))
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
                  (setf *modal-dialog-on-top* (nremove (assq dialog *modal-dialog-on-top*) *modal-dialog-on-top*))
                  (let ((mdot *modal-dialog-on-top*))
                    (when mdot
                      (when (not (wptr (caar mdot)))
                        (setf *modal-dialog-on-top* (cdr *modal-dialog-on-top*)))))
                                        ;(setf *eventhook* nil)  ; kill the same bug 2 ways.             
                                        ; moved update-menus back to after window-close - fixes do-about-dialog when carbon (weird)
                  (if close-on-exit
                      (window-close dialog)
                      (progn (window-hide dialog)
                             (set-window-layer dialog 9999)))
                  (when (not *modal-dialog-on-top*)(update-menus :enable *first-menustate*)
                                        ;(#_showfloatingwindows)
                        ))))))))


(defun %return-from-modal-dialog (&rest values)
  (when *modal-dialog-on-top* ; << maybe its gone or not set yet
    (let ((process (modal-dialog-process (car *modal-dialog-on-top*))))
      (when process
        (apply (function process-interrupt)
               process
               (lambda (&rest values)
                 (throw '%modal-dialog (apply #'values values)))
               values)))))


(defmacro return-from-modal-dialog (form)
  "

The macro RETURN-FROM-MODAL-DIALOG causes one or more values to be
returned from the most recent call to MODAL-DIALOG. 

The dialog is hidden or closed according to the value of CLOSE-ON-EXIT
that was passed to the call to MODAL-DIALOG.  (Any throw past the
modaldialog call also causes the dialog box to be hidden or closed).
If the dialog box is only hidden, its contents remain intact and it
continues to take up memory until the window-close function is
explicitly called. 

VALUES:         Any values.  The following two values have special
                meanings:

                :CLOSED:        If a dialog that is used modally has
                                a close box and the window is closed,
                                RETURN-FROM-MODAL-DIALOG is called
                                with the value :CLOSED. 

                :CANCEL:        If the user selects the cancel
                                button, RETURN-FROM-MODAL-DIALOG is
                                called returning :CANCEL.  The
                                function MODAL-DIALOG then performs a
                                THROW-CANCEL. 

"
  `(multiple-value-call '%return-from-modal-dialog ,form))



(defun return-cancel (i)
  (declare (ignore i))
  (return-from-modal-dialog :cancel))





(defmethod find-dialog-item ((view view) text)
  "

The FIND-DIALOG-ITEM generic function returns the first item in the
view whose DIALOG-ITEM-TEXT is the same as string (using equalp for
the comparison).  The items are searched in the order in which they
were added to the view.

This function may yield unexpected results in views with editable-text
items.  If the user types text identical to the text of another item,
the editable-text item may be returned instead of the desired
item.  For this reason, FIND-DIALOG-ITEM is best used during programming
and debugging sessions.

To identify items in a dialog, you should use nicknames and the
functions VIEW-NAMED and FIND-NAMED-SIBLING.

DIALOG:         A view or window containing dialog items.

STRING:         A string against which to compare the text of the
                dialog items.

"
  (dovector (item (view-subviews view))
            (when (and (typep item 'dialog-item) (equalp text (dialog-item-text item)))
              (return item))))














(defmethod editing-dialogs-p ((window t))
  nil)

(defclass color-dialog (dialog)
  ()
  (:default-initargs :color-p t))











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
              (and (member item (%get-key-handler-list dialog) :test (function eq))
                   (key-handler-p item)))
    (error "~s is either disabled or is not a key-handler item of ~s" item dialog))
  (without-interrupts
   (if (and (not (eq item (setf old (%get-current-key-handler dialog))))
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
         (rest (member old-handler items)))
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
      (psetf ul (add-points ul p)
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


(defun string-start-end (string &optional start end)
  (let* ((string (string string))
         (len    (length string))
         (start  (or start 0))
         (end    (or end len)))
    (flet ((are (a i) (error "Array index ~S out of bounds for ~S." i a)))    
      (unless (<= 0 end len)   (are string end))
      (unless (<= 0 start len) (are string start))
      (unless (<= start end)
        (error "Start ~S exceeds end ~S for a string operation." start end))
      (multiple-value-bind (str off) (array-displacement string)
        (values (or str string) (+ off start) (+ off end))))))


(defun draw-theme-text-box (text rect &optional (text-justification :center) truncwhere (active-p t))
  ;; could add a truncate option and use TruncateThemeText
  (let ((start 0) 
        (end (length text)))
    (when (not (simple-string-p text))
      (multiple-value-setq (text start end) (string-start-end text start end)))
    (niy draw-theme-text-box text rect text-justificiation truncwhere active-p)
    #-(and)
    (when (not (fixnump text-justification))
      (setf text-justification
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
                            (setf truncwhere
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
      (if (not (eq foo 0)) (setf ff (logior (logand ff (lognot #xff)) (fred-palette-closest-entry foo))))
      (values ff ms))))

(defun color->ff-index (color)
  (niy color->ff-index color)
  #-(and)
   (if (and color (not (eq color *black-color*)))
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
    (setf ff (logior (logand ff (lognot #xff)) (color->ff-index color))))
  (when (not (simple-string-p string))
    (multiple-value-setq (string start end) (string-start-end string start end)))
  (multiple-value-bind (line-ascent descent width leading) (font-codes-info ff ms)
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
              (when (and truncation (not (eq truncation :none)))
                (set-layout-line-truncation-given-layout layout truncation (null compress-p))) ;; aha need no-squash-p                
              (set-layout-line-width-given-layout layout max-width)
              (when justification  ;; doesnt work - fixed now
                (set-layout-line-justification-given-layout layout justification))
              (cond
               ((and truncation (not (eq truncation :none)))
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
                      (setf my-start next)
                      (when (%i>= my-start numchars)(return))
                      (setf now-height (%i+ now-height line-height))
                      (when (%i>= now-height rect-height)(return))
                      (setf vpos (%i+ vpos line-height))))))))))))
    nil))



;;;; THE END ;;;;
