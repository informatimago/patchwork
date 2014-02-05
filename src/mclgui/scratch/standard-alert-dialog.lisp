
(defparameter *alert-types* '((:stop . #.#$kalertStopAlert)
                              (:note . #.#$kalertNoteAlert)
                              (:caution . #.#$kalertCautionAlert)
                              (:plain . #.#$kalertPlainAlert)))
(defun get-alert-type (type)
  (cond ((cdr (assq type *alert-types*)))
        ((and (fixnump type)(rassoc type *alert-types*))
         type)        
        (t (error  "Unknown alert type ~S" type))))


(defvar *cancel-char*)
(defvar *yes-char*)
(defvar *no-char*)

(add-pascal-upp-alist-macho 'modal-key-handler-proc "NewModalFilterUPP")


(defpascal modal-key-handler-proc (:ptr targetref :ptr eventrecord :ptr index :word)
  (declare (ignorable targetref index))
  (let ((res #$false))
    (when (eq (pref eventrecord :eventrecord.what) #$keydown)
      (when (eq 0 (logand (pref eventrecord :eventrecord.modifiers)
                          (logior #$cmdKey #$controlKey #$rightControlKey)))
        (let* ((char-code (logand (pref eventrecord :eventrecord.message) #$charcodemask))
               (char (code-char char-code)))
          (declare (fixnum char-code))
          (when (> char-code #x7f)  ;; not perfect but better than nothing for e.g. Finnish keyboard or option-something
            (setq char (convert-char-to-unicode char (get-key-script))))
          (cond ((and *no-char* (char-equal char *no-char*))
                 (%put-word index #$kAlertStdAlertOtherButton) 
                 (setq res -1))  ;; aka #$true
                ((and *yes-char* (or (eq char #\return)(eq char :enter)(char-equal char *yes-char*)))
                 (%put-word index #$kAlertStdAlertOKButton)
                 (setq res -1))
                ((and *cancel-char* (or (eq char #\escape)(char-equal char *cancel-char*))) 
                 (%put-word index #$kAlertStdAlertCancelButton)
                 (setq res -1))))))
    res))

;; other options #$kWindowstaggerparentwindow #$kWindowStaggerMainScreen


(defun standard-alert-dialog (message &key (position :front-window)
                                      (yes-text "Yes")
                                      (no-text "No")
                                      (cancel-text "Cancel")
                                      (alert-type #$kalertcautionalert alert-type-p)
                                      explanation-text
                                      action-function
                                      help)
  (when  (not (fixnump position))
    (setq position
          (case position
            (:front-window #.#$kWindowAlertPositionParentWindow)
            (:center-front-window #.#$kWindowCenterParentWindow)
            (:main-screen #.#$kWindowAlertPositionMainScreen)
            (:center-main-screen #.#$kWindowCenterMainScreen)  ;; lower
            (t #.#$kWindowDefaultPosition))))  
  (when (not (or (stringp message)(encoded-stringp message)))
    (setq message (coerce message 'string)))
  (when (and explanation-text (not (or (stringp explanation-text)(encoded-stringp explanation-text))))
    (setq explanation-text (coerce explanation-text 'string)))
  (let ()
    (if alert-type-p
      (setq alert-type (get-alert-type alert-type)))
    (rlet ((params :AlertStdCFStringAlertParamRec)
           (the-alert-ptr :ptr)
           (alert-res :sint16))
      (#_GetStandardAlertDefaultParams params #$kStdCFStringAlertVersionOne)
      (with-cfstrs-hairy ((message-str message)(yes-str yes-text)(no-str no-text)(cancel-str cancel-text) (explanation-str explanation-text))
        (when cancel-text
          (setf (pref params :AlertStdCFStringAlertParamRec.cancelText) cancel-str)
          (setf (pref params :AlertStdCFStringAlertParamRec.cancelbutton) #$kAlertStdAlertCancelButton)) ;; make esc key work!
        (when no-text
          (setf (pref params :AlertStdCFStringAlertParamRec.otherText) no-str))
        (when yes-text
          (setf (pref params :AlertStdCFStringAlertParamRec.defaultText) yes-str))
        (if help
          (setf (pref params :AlertStdCFStringAlertParamRec.helpButton) T))
        ;; #+ignore  ;; dont know how to set the actual on screen position - now we know some options        
        (setf (pref params :AlertStdCFStringAlertParamRec.position) position)
        (flet ((first-char (text)(if (characterp text) text (char text 0))))
          (let ((*yes-char* (and yes-text (char-downcase (first-char yes-text))))
                (*no-char* (and no-text (char-downcase (first-char no-text))))
                (*cancel-char* (and cancel-text (char-downcase (first-char cancel-text)))))
            (let ((result (#_CreateStandardAlert
                           alert-type
                           message-str
                           (if explanation-text explanation-str *null-ptr*)  ;; some other string
                           params                         
                           the-alert-ptr)))
              (when (/= result #$noerr)
                (throw-cancel :cancel))
              ;; doesn't draw fully when called from Listener if more than 17 windows or so and timer is enabled
              (let () ;(*event-loop-initial-fire-time* 0.4d0))  ;; this helps but why?? - just change initial-fire-time
                (with-foreign-window 
                  (setq result (#_RunStandardAlert (%get-ptr the-alert-ptr) modal-key-handler-proc alert-res))))
              (WHEN (/= result #$noerr) (throw-cancel :cancel))
              (let ((action (%get-signed-word alert-res)))
                (if action-function
                  (funcall action-function action)
                  (case action 
                    (#.#$kAlertStdAlertOKButton t)
                    (#.#$kAlertStdAlertOtherButton nil)
                    (t (throw-cancel :cancel))))))))))))
