;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pw-scripting.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;    
;;;;    XXX
;;;;    
;;;;AUTHORS
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


(defvar *list-pw-script-objects* nil)
(defvar *contador-pw-script-object* 0)
(defvar *si-record* t)
(defvar *position-new-box* nil)
(defvar *charge?* nil)

(defun put-appleevent-par (aevent param-key data)
  (ui:uiwarn "~S is not implemented" 'put-appleevent-par)
  ;; (if (macptrp data) 
  ;;   (ae-error (#_AEPutParamDesc aevent param-key data))
  ;;   (ae-error (#_AEPutParamDesc aevent param-key (cl-user::getDescRecPtr (cl-user::asAEDesc  data))))
  ;;   )
  )

;;(cl-user::asAEDesc  100.0)

(defun mkSO (a b c d)
  (format *trace-output*  "Not Implemented Yet ~S" 'mkso)
  ;; (make-instance 
  ;;   'cl-user::ObjectSpecifier
  ;;   :class     a
  ;;   :container b
  ;;   :form      c
  ;;   :data      d)
  )


(defmethod salve-as ((self C-pw-window) as?)
  (let ((new-name as?)
         (*print-pretty* ()))
     (setf (patch-win-pathname self) new-name)
     (set-window-title self (pathname-name new-name))
     (when (wins-menu-item self)
       (set-menu-item-title (wins-menu-item self) (window-title self)))
     (delete-file new-name)   ;ML
     (ui:with-cursor *watch-cursor* 
       (WITH-OPEN-FILE  (out new-name :direction :output 
                                      :if-does-not-exist :create :if-exists :supersede) 
         (prin1 '(in-package :pw) out)
          (let ((*package* :pw))
            (prin1 (decompile self) out))))))


(defun find-pw-menu-item (item)
  (if (igual item "window comment")
    (find-menu-item pw::*pwoper-menu* "Window Comment")
  (let* ((all-menus (concatenate 'list (menu-items pw::*pw-kernel-menu*)
                                 (menu-items pw::*pw-menu-Music*)
                                 (menu-items (pw::the-user-menu))))
         rep (seguir t))
    (while seguir
      (setf rep (find-menu-item (car all-menus) item))
      (if (null rep)
        (if (null (cdr all-menus))
          (setf seguir nil)
          (setf all-menus (cdr all-menus)))
        (setf seguir nil)))
    rep)))


(defun get-pw-script-obj (string)
  (nth (- (read-from-string (subseq string 2)) 1) *list-pw-script-objects*))

(defun AEresolve (theData)
  (ui:uiwarn "~S is not implemented" 'AEresolve)
  ;; (when TheData
  ;;   (let* ((clase  (cl-user::getclass TheData)))
  ;;     (cond
  ;;      ((equal clase :|cte |) (cl-user::getdata TheData))
  ;;      ((equal clase :|null|)  nil)
  ;;      ((equal clase :|type|) (cl-user::getdata TheData))
  ;;      ((equal clase :|file|) (cl-user::getdata TheData))
  ;;      ((equal clase :|cnot|) (if (equal (cl-user::getForm theData) :|name|)
  ;;                               (get-pw-script-obj (cl-user::getdata TheData))
  ;;                               (nth (- (cl-user::getdata theData) 1) (notes (AEresolve (cl-user::getcontainer TheData))))))
  ;;      ((equal clase :|ccho|) (if (equal (cl-user::getForm theData) :|name|)
  ;;                               (get-pw-script-obj (cl-user::getdata TheData))
  ;;                               (nth (- (cl-user::getdata theData) 1) 
  ;;                                    (let* ((obj (AEresolve (cl-user::getcontainer TheData))))
  ;;                                      (if (equal (class-name (class-of obj)) 'c-beat)
  ;;                                        (get-chords-from-beat obj)
  ;;                                        (chords obj))))))
  ;;      ((equal clase :|cmea|) (if (equal (cl-user::getForm theData) :|name|)
  ;;                               (get-pw-script-obj (cl-user::getdata TheData))
  ;;                               (nth (- (cl-user::getdata theData) 1) (measures (AEresolve (cl-user::getcontainer TheData))))))
  ;;      ((equal clase :|cbea|) (if (equal (cl-user::getForm theData) :|name|)
  ;;                               (get-pw-script-obj (cl-user::getdata TheData))
  ;;                               (nth (- (cl-user::getdata theData) 1) (beat-objects (AEresolve (cl-user::getcontainer TheData))))))
  ;;      ((equal clase :|cvoi|) (get-pw-script-obj (cl-user::getdata TheData)))
  ;;      ((equal clase :|cmli|) (get-pw-script-obj (cl-user::getdata TheData)))
  ;;      ((equal clase :|cpat|) (cl-user::getdata TheData))
  ;;      ((equal clase :|cmen|) (let* ((resp (AEresolve (cl-user::getcontainer TheData))))
  ;;                               (if (patch-type-p resp)
  ;;                                 (list (find-menu-item (menu (popUpbox resp)) (cl-user::getdata theData)) resp)
  ;;                                 (list (find-menu-item (first resp) (cl-user::getdata theData)) (second resp)))))
  ;;      ((equal clase :|cbox|) (getbox-name (cl-user::getdata theData) (list-of-box (AEresolve (cl-user::getcontainer TheData)))))
  ;;      ((equal clase :|obab|) (cl-user::getdata TheData))
  ;;      ((equal clase :|obli|) (cl-user::getdata TheData))
  ;;      ((equal clase :|cinp|) (list (- (cl-user::getdata theData) 1) (AEresolve (cl-user::getcontainer TheData))))
  ;;      (t nil))))
  )

(defun getbox-name (name lis)
  (flet ((match (item ) (igual name (pw-function-string item))))
    (let* ((thebox (find-if #'match lis)))
      thebox)))

(defun getpatch-name (patch)
  (if (null patch)
    *active-patch-window*
    (flet ((match (item ) (igual patch (win-title item))))
        (let* ((thepatch (find-if #'match *pw-window-list*)))
          (if (null thepatch)
            *active-patch-window*
            thepatch)))))

(defun list-of-box (patch)
  (let* ((thepatch (getpatch-name patch)))
      (when thepatch)
        (controls thepatch)))

(defun win-title (win)
 (delete #\† (window-title win)))


(defun igual (str1 str2)
  (equal (string-downcase str1) (string-downcase str2)))

(defmethod get-chords-from-beat ((self c-beat))
  (let* ((beat-chord (beat-chord self)) rep)
    (if (null beat-chord)
      (let* ((rtm-list (rtm-list self)))
        (mapcar (lambda (mes)
                (setf rep (concatenate 'list rep (get-chords-from-beat mes) ))) rtm-list)
        rep)
      (concatenate 'list rep  (list beat-chord)))))
;;***********HANDLERS***************

;;OPEN

(defmethod even-open ((a application) theAppleEvent reply handlerRefcon)
  (declare (ignore handlerRefcon reply))
    (ui:uiwarn "~S is not implemented" 'even-open)
  ;; (let* ((AE-clos-Ob (make-instance 'cl-user::AppleEvent :descRecPtr theAppleEvent ))
  ;;        (thePatch (cl-user::getparam AE-clos-Ob :|----| ))
  ;;        (Pname (AEresolve thePatch)))
  ;;  (if (patch-type-p Pname)
  ;;     (open-patch-win Pname)
  ;;  (let ((name Pname)
  ;;       (*compile-definitions* ()))  
  ;;   (with-cursor *watch-cursor* 
  ;;     (load name :verbose t)
  ;;     (pw::PW-update-wins-menu name)))))
  )

;;CLOSE

(defmethod even-close ((a application) theAppleEvent reply handlerRefcon)
  (declare (ignore handlerRefcon reply))
  (ui:uiwarn "~S is not implemented" 'even-close)
  ;; (let* ((AE-clos-Ob (make-instance 'cl-user::AppleEvent :descRecPtr theAppleEvent ))
  ;;        (thePara (cl-user::getparam AE-clos-Ob :|----| :|type|)))
  ;;   (if (equal ThePara :|cpat|)
  ;;     (kill-patch-window *active-patch-window*)))
  )




;;MAKE NEW
(defmethod even-new ((a application) theAppleEvent reply handlerRefcon)
  (declare (ignore handlerRefcon ))
  (ui:uiwarn "~S is not implemented" 'even-new)
  ;; (let* ((AE-clos-Ob (make-instance 'cl-user::AppleEvent :descRecPtr theAppleEvent ))
  ;;        (eltipo (cl-user::getparam AE-clos-Ob :|kocl| :|type|)))
  ;;   (cond
  ;;    ((equal eltipo :|obab|) (let ((theabs (make-abstraction-M *active-patch-window*))
  ;;                                  (bposi (cl-user::getparam AE-clos-Ob :|mbpb|)))
  ;;                              (if (equal t theabs) (put-appleevent-par reply #$keyDirectObject  nil)
  ;;                                  (progn
  ;;                                    (when bposi 
  ;;                                      (set-view-position theabs (make-point (first bposi) (second bposi) )))
  ;;                                                     
  ;;                                    (put-appleevent-par reply #$keyDirectObject 
  ;;                                                            (mkSO :|cbox| nil :|name| (pw-function-string theabs)))))))
  ;; 
  ;;    ((equal eltipo :|cpat|) (progn (pw::make-new-pw-window t)
  ;;                                   (put-appleevent-par reply #$keyDirectObject 
  ;;                                        (mkSO :|cpat| nil :|name| (win-title *active-patch-window*)))))
  ;;    ((equal eltipo :|cbox|) (let* ((bclase (cl-user::getparam AE-clos-Ob :|mbcn|))
  ;;                                   (bname (cl-user::getparam AE-clos-Ob :|mbnb|))
  ;;                                   (bposi (cl-user::getparam AE-clos-Ob :|mbpb|))
  ;;                                   (themenu (if (igual bclase "funlisp") t (find-pw-menu-item bclase)))
  ;;                                   thebox)
  ;;                              (when themenu
  ;;                                (setf *position-new-box* (make-point 15 15 ))
  ;;                                (when bposi 
  ;;                                  (setf *position-new-box* (make-point (first bposi) (second bposi) )))
  ;;                                (if (igual bclase "funlisp")
  ;;                                  (setf thebox (make-lisp-pw-boxes (read-from-string bname) *active-patch-window*))
  ;;                                  (setf thebox (menu-item-action themenu)))
  ;;                                (setf *position-new-box* nil)
  ;;                                (when  (and bname (not (igual bclase "funlisp")))
  ;;                                  (unless *pw-controls-dialog-text-item* (make-pw-controls-dialog))
  ;;                                  (setf *pw-controls-current-pw-control* thebox)
  ;;                                  (set-dialog-item-text *pw-controls-dialog-text-item* bname)
  ;;                                  (set-view-position *pw-controls-dialog-text-item* 
  ;;                                                     (make-point 1 (- (pw::h thebox) 14)))
  ;;                                  (resize-text-item thebox (make-point (pw::w thebox) 13))
  ;;                                  (add-subviews thebox *pw-controls-dialog-text-item*)
  ;;                                  (setf *current-small-inBox* (car (pw::pw-controls thebox)))
  ;;                                  (change-menu-actions)
  ;;                                  (set-dialog-item-text-from-dialog pw::*pw-controls-current-pw-control* 
  ;;                                                                    (dialog-item-text pw::*pw-controls-dialog-text-item*))
  ;;                                  (kill-text-item)
  ;;                                  )
  ;;                                (put-appleevent-par reply #$keyDirectObject 
  ;;                                                          (mkSO :|cbox| nil :|name| (pw-function-string thebox) )))))
  ;;    (t (print "error no tipo para new"))))
  )



;;LOAD 

(defmethod even-load ((a application) theAppleEvent reply handlerRefcon)
  (declare (ignore handlerRefcon reply))
  (ui:uiwarn "~S is not implemented" 'even-new)
  ;; (let* ((AE-clos-Ob (make-instance 'cl-user::AppleEvent :descRecPtr theAppleEvent ))
  ;;        (thePara (cl-user::getparam AE-clos-Ob :|----| )) 
  ;;        (thetype (cl-user::getclass ThePara))
  ;;        (Pname (AEresolve thePara)))
  ;;   (cond
  ;;    ((equal thetype :|obli|) (load-one-user-library (pathname Pname)))
  ;;    ((equal thetype :|obab|) (load&form-abstr-menu (pathname Pname)) )
  ;;    (t (print "load a library or an abstract"))))
  )
 


;;OPTIONS
(defmethod even-options ((a application) theAppleEvent reply handlerRefcon)
  (declare (ignore handlerRefcon reply))
    (ui:uiwarn "~S is not implemented" 'even-options)
  ;; (let* ((AE-clos-Ob (make-instance 'cl-user::AppleEvent :descRecPtr theAppleEvent ))
  ;;        (Scale (cl-user::getparam AE-clos-Ob :|opsc| )) 
  ;;        (Appro (cl-user::getparam AE-clos-Ob :|opap| ))
  ;;        (Playop (cl-user::getparam AE-clos-Ob :|oppl| )))
  ;;   (when Scale
  ;;     (cond
  ;;      ((igual Scale "Chromatic") (set-globally-scale *chromatic-scale*))
  ;;      ((igual Scale "C-major") (set-globally-scale *c-major-scale*))))
  ;;   (when Appro
  ;;     (cond
  ;;      ((igual Appro "SemiTone") (set-globally-scale *c-major-scale*))
  ;;      ((igual Appro "Quarter tone") (set-globally-scale *1/4-tone-chromatic-scale*))
  ;;      ((igual Appro "Eigth tone") (set-globally-scale *1/8-tone-chromatic-scale*))))
  ;;   (when Playop
  ;;     (cond
  ;;      ((igual Playop "Pitch Bend") (set-playing-option :pb))
  ;;      ((igual Playop "Multi Channel") (set-playing-option :mc))))
  ;;   )
  )



;;SET
(defmethod even-set ((a application) theAppleEvent reply handlerRefcon)
  (declare (ignore handlerRefcon reply))
      (ui:uiwarn "~S is not implemented" 'even-set)
  ;; (let* ((AE-clos-Ob (make-instance 'cl-user::AppleEvent :descRecPtr theAppleEvent ))
  ;;        (thePara (cl-user::getparam AE-clos-Ob :|----| )) 
  ;;        (theinput (AEresolve thePara))
  ;;        (thedata (cl-user::getparam AE-clos-Ob :|data| )) theinbox theentre)
  ;;   (cond
  ;;    ((listp theinput)  
  ;;     (setf theentre (nth (first theinput)  (pw-controls (second theinput))))
  ;;     (setf theinbox (nth (first theinput)  (input-objects (second theinput))))
  ;;     (if (or (equal (class-name (class-of theentre)) 'C-ttybox-absout)
  ;;             (equal (class-name (class-of theentre)) 'C-ttybox-out))
  ;;       (setf theinbox theentre))
  ;;     (when (not (patch-type-p theinbox))
  ;;       (cond
  ;;        ((equal (class-name (class-of theentre)) 'C-chord-box)
  ;;         (let ((thechord (AEresolve thedata)))
  ;;           (setf (t-time thechord) 0)
  ;;           (setf (chord-line theentre) (make-instance 'C-chord-line :chords (list thechord )))
  ;;           (update-control theentre)))
  ;;        ((equal (class-name (class-of (second theinput))) 'C-patch-polifMN-mod)
  ;;         (setf (nth (first theinput) (chord-line-list (second theinput))) (aeresolve thedata)))
  ;;        ((equal (class-name (class-of (second theinput))) 'C-patch-PolifRTM)
  ;;         (let* ((win (application-object (second theinput)))
  ;;                (editors (beat-editors (car (subviews win)))))
  ;;           (setf (nth (first theinput) (measure-line-list (second theinput))) (aeresolve thedata))
  ;;           (setf (measure-line (nth (first theinput) editors)) (nth (first theinput) (measure-line-list (second theinput))))
  ;;           (erase+view-draw-contents (nth (first theinput) editors))))
  ;;        (t (set-dialog-item-text-from-dialog theinbox (format nil "~5D" thedata))))))))
  )


;;GET
(defmethod even-get ((a application) theAppleEvent reply handlerRefcon)
  (declare (ignore handlerRefcon))
        (ui:uiwarn "~S is not implemented" 'even-get)
  ;; (let* ((AE-clos-Ob (make-instance 'cl-user::AppleEvent :descRecPtr theAppleEvent ))
  ;;        (thePara (cl-user::getparam AE-clos-Ob :|----| ))
  ;;        (theinput (if (equal (cl-user::getclass thepara) :|prop|) (AEresolve (cl-user::getcontainer thepara)) 
  ;;                      (AEresolve thePara))))
  ;;   (cond 
  ;;    ((equal (cl-user::getclass thepara) :|prop|)  
  ;;     (put-appleevent-par reply #$keyDirectObject (eval-ae (solucione theinput (cl-user::getdata thepara)))))
  ;;    ((or (equal (cl-user::getclass thepara) :|cpat|) (equal (cl-user::getclass thepara) :|cbox|))
  ;;     (put-appleevent-par reply #$keyDirectObject (cl-user::getdescrecptr (cl-user::asaedesc thepara))))
  ;;    ((equal (cl-user::getclass thepara) :|cinp|)
  ;;     (put-appleevent-par reply #$keyDirectObject 
  ;;                                    (eval-ae (patch-value (nth (first theinput)  (input-objects (second theinput)))
  ;;                                                          (nth (first theinput)  (input-objects (second theinput)))))))
  ;;    (t  (put-appleevent-par reply #$keyDirectObject (eval-ae theinput)))))
  )



(defun solucione (nota clase)
  (cond
   ((equal clase :|psig|)  (list (read-from-string (high nota)) (read-from-string (low nota))))
   ((equal clase :|ptem|)  (list (metronome nota) (metronome-unit nota)))
   ((equal clase :|cpit|)  (midic nota))
   ((equal clase :|cdur|) (dur nota))
   ((equal clase :|cvel|) (vel nota))
   ((equal clase :|ccha|) (chan nota))
   ((equal clase :|leng|) 
    (cond
     ((equal (class-name (class-of nota)) 'patch-work::c-chord-line)
            (length (chords nota)))
     ((equal (class-name (class-of nota)) 'patch-work::c-chord)
            (length (notes nota)))
     ((equal (class-name (class-of nota)) 'patch-work::c-measure-line)
            (length (measures nota)))
     ((equal (class-name (class-of nota)) 'patch-work::c-beat)
            (length (print (get-chords-from-beat nota))))))
   ((equal clase :|ctim|) (if (equal (class-name (class-of nota)) 'patch-work::c-note)
                            (offset-time nota)
                            (t-time nota)))))
       
       

 
;;CONNECT
(defmethod even-connect ((a application) theAppleEvent reply handlerRefcon)
  (declare (ignore handlerRefcon reply))
  (ui:uiwarn "~S is not implemented" 'even-connect)
 ;;     (let* ((AE-clos-Ob (make-instance 'cl-user::AppleEvent :descRecPtr theAppleEvent ))
  ;;      (thePara (cl-user::getparam AE-clos-Ob :|----| ))
  ;;      (thePara1 (cl-user::getparam AE-clos-Ob :|data| ))
  ;;      (theData (AEResolve thePara))
  ;;      (input (AEResolve thePara1)))
  ;; (pw::connect-nth-control (second input) (first input) theData)
  ;; (pw::draw-connections (second input)))
  )

 
;;UNCONNECT

(defmethod even-unco ((a application) theAppleEvent reply handlerRefcon)
  (declare (ignore handlerRefcon reply))
  (ui:uiwarn "~S is not implemented" 'even-unco)
  ;; (let* ((AE-clos-Ob (make-instance 'cl-user::AppleEvent :descRecPtr theAppleEvent ))
  ;;       (thePara (cl-user::getparam AE-clos-Ob :|----| ))
  ;;       (theinput (AEresolve thePara))
  ;;       (thectr (nth (first theinput)  (pw::pw-controls (second theinput)))))
  ;;   (pw::disconnect-ctrl (second theinput) thectr)
  ;;  )
  )



  
;;MOVE

(defmethod even-move ((a application) theAppleEvent reply handlerRefcon)
  (declare (ignore handlerRefcon reply))
  (ui:uiwarn "~S is not implemented" 'even-move)
  ;; (let* ((AE-clos-Ob (make-instance 'cl-user::AppleEvent :descRecPtr theAppleEvent ))
  ;;        (thePara (cl-user::getparam AE-clos-Ob :|----| ))
  ;;        (thePoint (cl-user::getparam AE-clos-Ob :|insh|  ))
  ;;        (theBox (AEresolve thePara)))
  ;;   (tell (pw::controls pw::*active-patch-window*) 'pw::draw-connections t)
  ;;   (set-view-position theBox (first thepoint) (second thepoint))
  ;;   (tell (pw::controls pw::*active-patch-window*) 'pw::draw-connections)
  ;;   )
  )




;;SAVE

;;(remember-config)

(defmethod even-save ((a application) theAppleEvent reply handlerRefcon)
  (declare (ignore handlerRefcon reply))
  (ui:uiwarn "~S is not implemented" 'even-save)
  ;; (let* ((AE-clos-Ob (make-instance 'cl-user::AppleEvent :descRecPtr theAppleEvent ))
  ;;        (thePara (cl-user::getparam AE-clos-Ob :|----| )) 
  ;;        (thePatch (cond ((equal thePara :|conf|) nil)
  ;;                        ((not (equal thePara :|cpat|)) (getpatch-name (AEresolve thePara)))
  ;;                        (t *active-patch-window*)))
  ;;        (as? (cl-user::getparam AE-clos-Ob :|asna| ))
  ;;        (withmn? (cl-user::getparam AE-clos-Ob :|mnpa| )))
  ;;   (if (equal thePara :|conf|)
  ;;     (remember-config)
  ;;     (when thePatch
  ;;       (if withmn?
  ;;         (progn
  ;;           (setq pw::*decompile-chords-mode* t)
  ;;           (if (null as?)
  ;;             (pw::PW-WINDOW-SAVE thePatch)
  ;;             (pw::salve-as thePatch  as?))
  ;;           (setq pw::*decompile-chords-mode* nil))
  ;;         (if (null as?)
  ;;           (pw::PW-WINDOW-SAVE thePatch )
  ;;           (pw::salve-as thePatch  as?))))))
  )


 
;;EVAL

(defmethod even-eval ((a application) theAppleEvent reply handlerRefcon)
  (declare (ignore handlerRefcon))
  (ui:uiwarn "~S is not implemented" 'even-eval)
  ;; (let* ((AE-clos-Ob (make-instance 'cl-user::AppleEvent :descRecPtr theAppleEvent ))
  ;;       (thePara (cl-user::getparam AE-clos-Ob :|----| ))
  ;;       (thebox (AEresolve thePara))
  ;;       (rep (eval-ae (patch-value thebox thebox)) ))
  ;;  (put-appleevent-par reply #$keyDirectObject rep))
  )

(defun meta-objeto (obj)
  (if (position obj *list-pw-script-objects*)
    (+ (position obj *list-pw-script-objects* ) 1)
    (progn
      (setf *list-pw-script-objects* (reverse (cons obj (reverse *list-pw-script-objects*))))
      (incf *contador-pw-script-object* ))))


(defun eval-ae (repons)
  (cond
     ((rationalp repons) (float repons))
     ((or (numberp repons)) repons)
     ((stringp repons) repons)
     ((symbolp repons) repons)
     ((equal (class-name (class-of repons)) 'patch-work::c-chord-line)
      (mkSO :|cvoi| nil :|name| (concatenate 'string "Cl" (format nil "~D" (meta-objeto repons)))))
     ((equal (class-name (class-of repons)) 'patch-work::c-measure-line)
      (mkSO :|cmli| nil :|name| (concatenate 'string "Ml" (format nil "~D" (meta-objeto repons)))))
     ((equal (class-name (class-of repons)) 'patch-work::c-measure)
      (mkSO :|cmea| nil :|name| (concatenate 'string "Me" (format nil "~D" (meta-objeto repons)))))
     ((equal (class-name (class-of repons)) 'patch-work::c-beat)
      (mkSO :|cbea| nil :|name| (concatenate 'string "Be" (format nil "~D" (meta-objeto repons)))))
     ((equal (class-name (class-of repons)) 'patch-work::c-note)
      (mkSO :|cnot| nil :|name| (concatenate 'string "Nt" (format nil "~D" (meta-objeto repons)))))
     ((equal (class-name (class-of repons)) 'patch-work::c-chord)
      (mkSO :|ccho| nil :|name| (concatenate 'string "Ch" (format nil "~D" (meta-objeto repons)))))
     ((listp repons)  (elimine-ptr repons))
    (t (format nil "~D" repons))))

(defun elimine-ptr (lis)
  (mapcar (lambda (repons)
              (cond
               ((or (numberp repons) (rationalp repons) (stringp repons)) repons)
               ((equal (class-name (class-of repons)) 'patch-work::c-chord-line)
                (mkSO :|cvoi| nil :|name| (concatenate 'string "Cl" (format nil "~D" (meta-objeto repons)))))
               ((equal (class-name (class-of repons)) 'patch-work::c-measure-line)
                (mkSO :|cmli| nil :|name| (concatenate 'string "Ml" (format nil "~D" (meta-objeto repons)))))
               ((equal (class-name (class-of repons)) 'patch-work::c-measure)
                (mkSO :|cmea| nil :|name| (concatenate 'string "Me" (format nil "~D" (meta-objeto repons)))))
               ((equal (class-name (class-of repons)) 'patch-work::c-beat)
                (mkSO :|cbea| nil :|name| (concatenate 'string "Be" (format nil "~D" (meta-objeto repons)))))
               ((equal (class-name (class-of repons)) 'patch-work::c-note)
                (mkSO :|cnot| nil :|name| (concatenate 'string "Nt" (format nil "~D" (meta-objeto repons)))))
               ((equal (class-name (class-of repons)) 'patch-work::c-chord)
                (mkSO :|ccho| nil :|name| (concatenate 'string "Ch" (format nil "~D" (meta-objeto repons)))))
               ((listp repons) (elimine-ptr repons))
               (t (format nil "~D" repons)))) lis))




;;SELECT

(defmethod even-sele ((a application) theAppleEvent reply handlerRefcon)
  (declare (ignore handlerRefcon reply))
  (ui:uiwarn "~S is not implemented" 'even-sele)
  ;; (let* ((AE-clos-Ob (make-instance 'cl-user::AppleEvent :descRecPtr theAppleEvent ))
  ;;        (thePara (cl-user::getparam AE-clos-Ob :|----| ))
  ;;        (shift? (cl-user::getparam AE-clos-Ob :|chif| ))
  ;;        (thesele (if (not (listp thePara)) (AEresolve thePara))))
  ;;   (if thesele
  ;;     (if (equal (cl-user::getclass thePara) :|cpat|)
  ;;       (progn (view-activate-event-handler (getpatch-name thesele))
  ;;              (window-select (getpatch-name thesele)))
  ;;       (if (equal (cl-user::getclass thePara) :|cbox|)
  ;;         (if (null shift?)
  ;;           (toggle-patch-active-mode thesele)
  ;;           (with-focused-view thesele
  ;;             (setf (active-mode thesele) (not (active-mode thesele)))
  ;;             (with-pen-state (:mode :patxor :pattern *black-pattern*)
  ;;               (fill-rect* 1 1 (- (w thesele) 2) 2))))))
  ;;     (dolist (item thePara)
  ;;       (let ((thebox (AEresolve item)))
  ;;         (activate-control thebox)))))
  )
 
;;RENAME

(defmethod even-rena ((a application) theAppleEvent reply handlerRefcon)
  (declare (ignore handlerRefcon))
    (ui:uiwarn "~S is not implemented" 'even-rena)
  ;; (let* ((AE-clos-Ob (make-instance 'cl-user::AppleEvent :descRecPtr theAppleEvent ))
  ;;        (thePara (cl-user::getparam AE-clos-Ob :|----| ))
  ;;        (thename (cl-user::getparam AE-clos-Ob :|newn| ))
  ;;        (thesele (AEresolve thePara)))
  ;;   (unless pw::*pw-controls-dialog-text-item* (pw::make-pw-controls-dialog))
  ;;   (setf pw::*pw-controls-current-pw-control* thesele)
  ;;   (set-dialog-item-text pw::*pw-controls-dialog-text-item* thename)
  ;;   (set-view-position pw::*pw-controls-dialog-text-item* 
  ;;                      (make-point 1 (- (pw::h thesele) 14)))
  ;;   (pw::resize-text-item thesele (make-point (pw::w thesele) 13))
  ;;   (add-subviews thesele pw::*pw-controls-dialog-text-item*)
  ;;   (setf pw::*current-small-inBox* (car (pw::pw-controls thesele)))
  ;;   (pw::change-menu-actions)
  ;;   (pw::set-dialog-item-text-from-dialog pw::*pw-controls-current-pw-control* 
  ;;                                         (dialog-item-text pw::*pw-controls-dialog-text-item*))
  ;;   (pw::kill-text-item)
  ;;   (put-appleevent-par reply #$keyDirectObject 
  ;;                                 (mkSO :|cbox| nil :|name| thename)))
  )


     
;;LOCK
(defmethod even-lock ((a application) theAppleEvent reply handlerRefcon)
  (declare (ignore handlerRefcon reply))
      (ui:uiwarn "~S is not implemented" 'even-lock)
  ;; (let* ((AE-clos-Ob (make-instance 'cl-user::AppleEvent :descRecPtr theAppleEvent ))
  ;;        (thePara (cl-user::getparam AE-clos-Ob :|----| ))
  ;;        (thebox (AEresolve thePara)))
  ;;   (if (not (value thebox))
  ;;     (progn
  ;;       (set-dialog-item-text (lock thebox) "x")
  ;;       (if (equal (class-name (class-of thebox)) 'C-PATCH-BUFFER::C-patch-buffer)
  ;;         (setf (C-PATCH-BUFFER::value thebox) (not (C-PATCH-BUFFER::value thebox)))
  ;;         (setf (value thebox) (not (value thebox)))))))
  )
 

;;UNLOCK
(defmethod even-unlo ((a application) theAppleEvent reply handlerRefcon)
  (declare (ignore handlerRefcon reply))
        (ui:uiwarn "~S is not implemented" 'even-unlo)
  ;; (let* ((AE-clos-Ob (make-instance 'cl-user::AppleEvent :descRecPtr theAppleEvent ))
  ;;        (thePara (cl-user::getparam AE-clos-Ob :|----| ))
  ;;        (thebox (AEresolve thePara)))
  ;;   (if (value thebox)
  ;;     (progn
  ;;       (set-dialog-item-text (lock thebox) "o")
  ;;       (if (equal (class-name (class-of thebox)) 'C-PATCH-BUFFER::C-patch-buffer)
  ;;         (setf (C-PATCH-BUFFER::value thebox) (not (C-PATCH-BUFFER::value thebox)))
  ;;         (setf (value thebox) (not (value thebox)))))))
  )



;;ACTIVATE

(defmethod even-actv ((a application) theAppleEvent reply handlerRefcon)
  (declare (ignore handlerRefcon theAppleEvent reply))
          (ui:uiwarn "~S is not implemented" 'even-actv)
  ;; ;(if (null (position "PW-modifs" module::*loaded-modules* :test 'equal))
  ;; ;  (without-interrupts
  ;; ;  (tell *eval-queue* 'eval)
  ;; ;  (setf *eval-queue* nil)))
  ;; (let* ((psn (make-record :ProcessSerialNumber)))
  ;;   (setf *list-pw-script-objects* nil)
  ;;   (setf *contador-pw-script-object* 0)
  ;;   (if (not *active-patch-window*) 
  ;;     (make-new-pw-window t) 
  ;;     (if (not (wptr *active-patch-window*)) 
  ;;       (search-for-next-pw-window))) 
  ;;   (enable-all-apps-menu-items)
  ;;   (menu-item-disable *apps-PW-menu-item*)
  ;;   (window-select  *active-patch-window*)
  ;;   (#_getcurrentprocess psn)
  ;;   (#_setfrontprocess psn)
  ;;   )
  )
  

  
;;DUPLICATE

(defmethod even-dupli ((a application) theAppleEvent reply handlerRefcon)
  (declare (ignore handlerRefcon reply theAppleEvent))
  (duplicate *active-patch-window*))
  
;;DELETE

(defmethod even-delete ((a application) theAppleEvent reply handlerRefcon)
  (declare (ignore handlerRefcon reply theAppleEvent))
  (cut-delete *active-patch-window*))


;;CUT
(defmethod even-cut ((a application) theAppleEvent reply handlerRefcon)
  (declare (ignore handlerRefcon reply theAppleEvent))
  (cut *active-patch-window*))

;;COPY
(defmethod even-copy ((a application) theAppleEvent reply handlerRefcon)
  (declare (ignore handlerRefcon reply theAppleEvent))
  (copy *active-patch-window*))

;;PASTE
(defmethod even-paste ((a application) theAppleEvent reply handlerRefcon)
  (declare (ignore handlerRefcon reply theAppleEvent))
  (paste *active-patch-window*))

;;PRINT
(defmethod even-print ((a application) theAppleEvent reply handlerRefcon)
  (declare (ignore theAppleEvent reply handlerRefcon))
  (ui::window-hardcopy (front-window)))

;;COMMAND

(defmethod pw::popUpbox ((self c-patch-file-buffer::c-patch-ascii-buffer))
  (c-patch-file-buffer::popUpBox self))

(defmethod pw::popUpbox ((self C-PATCH-LIST-EDITOR::C-patch-list-editor))
  (C-PATCH-LIST-EDITOR::popUpBox self))


(defmethod pw::popUpbox ((self C-PW-MIDI-IN::C-pw-midi-in))
  (C-PW-MIDI-IN::popUpBox self))


(defmethod even-command ((a application) theAppleEvent reply handlerRefcon)
  (declare (ignore handlerRefcon reply))
  (ui:uiwarn "~S is not implemented" 'even-command)

  ;; (let* ((AE-clos-Ob (make-instance 'cl-user::AppleEvent :descRecPtr theAppleEvent ))
  ;;        (thePara (cl-user::getparam AE-clos-Ob :|----| ))
  ;;        (thebox (AEresolve thePara))
  ;;        (parametro (cl-user::getparam AE-clos-Ob :|fina| )))
  ;;   (if (listp thebox)
  ;;     (let* ((iteme (first thebox))
  ;;            (boite (second thebox))
  ;;            (clase (class-name (class-of boite))))
  ;;       (setf *target-action-object* boite)
  ;;       (cond
  ;;        ((or (igual (menu-item-title iteme) "Save Chord") (igual (menu-item-title iteme) "Save"))
  ;;         (let* ((new-name parametro)
  ;;                (*print-pretty* nil)
  ;;                (*decompile-chords-mode* t))
  ;;           (setf (pw-function-string *target-action-object*) (string-downcase  new-name))
  ;;           (delete-file new-name)   ;ML
  ;;           (with-cursor *watch-cursor*
  ;;             (WITH-OPEN-FILE (out new-name :direction :output :if-exists :supersede :if-does-not-exist :create)
  ;;               (prin1 '(in-package :pw) out)
  ;;               (let ((*package* :pw))
  ;;                 (prin1 `(add-patch-box *active-patch-window* ,(decompile *target-action-object*)) out))))))
  ;;        ((and (igual (menu-item-title iteme) "Open") (equal 'C-patch-file-buffer clase))
  ;;         (cond ((and (c-patch-file-buffer::fred-win boite) (wptr (c-patch-file-buffer::fred-win boite)))
  ;;                (window-select (c-patch-file-buffer::fred-win boite)))
  ;;               ((c-patch-file-buffer::fred-win boite) (script-get-selected-file boite parametro))
  ;;               ((c-patch-file-buffer::file-name boite)
  ;;                (let* ((f-name (c-patch-file-buffer::file-name boite))
  ;;                       (w-name (c-patch-file-buffer::file-namestring f-name))
  ;;                       (win (c-patch-file-buffer::find-window w-name)))
  ;;                  (if (and win (wptr win))
  ;;                    (window-select win)
  ;;                    (script-get-selected-file boite))))
  ;;               (t (script-get-selected-file boite parametro))))
  ;;        ((and (igual (menu-item-title iteme) "Open File") 
  ;;              (or (equal 'c-patch-file-buffer::C-patch-file-buffer clase) 
  ;;                  (equal 'c-patch-file-buffer::c-patch-ascii-buffer clase)))
  ;;         (script-get-selected-file boite parametro))
  ;;        (t (menu-item-action iteme))))))
  )


(in-package "C-PATCH-FILE-BUFFER")

(defmethod pw::script-get-selected-file ((self C-patch-file-buffer) para)
  (niy 'pw::script-get-selected-file self para)
  ;; (let ((name (full-pathname para)))
  ;;   (ui:with-cursor *watch-cursor*
  ;;     (setf (fred-win self) 
  ;;           (make-instance 'fred-window :window-show nil))
  ;;     (setf (file-name self) name)
  ;;     (set-window-filename (fred-win self) name)
  ;;     (buffer-insert-file (fred-buffer (fred-win self)) name)
  ;;     (update-box-name self name)
  ;;     (window-select (fred-win self))))
  )

(in-package :pw)


;;EXTEND
(defmethod even-extend ((a application) theAppleEvent reply handlerRefcon)
  (declare (ignore handlerRefcon ))
  (ui:uiwarn "~S is not implemented" 'even-extend)
  ;; (let* ((AE-clos-Ob (make-instance 'cl-user::AppleEvent :descRecPtr theAppleEvent ))
  ;;        (thePara (cl-user::getparam AE-clos-Ob :|----| ))
  ;;        (thebox (AEresolve thePara))
  ;;        (new-box (mouse-pressed-no-active-extra thebox 0 0)))
  ;;   (put-appleevent-par reply #$keyDirectObject (mkSO :|cbox| nil :|name| (pw-function-string new-box))))
  )



;;PLAY 
(defmethod even-play ((a application) theAppleEvent reply handlerRefcon)
  (declare (ignore handlerRefcon))
    (ui:uiwarn "~S is not implemented" 'even-play)
  ;; (let* ((AE-clos-Ob (make-instance 'cl-user::AppleEvent :descRecPtr theAppleEvent ))
  ;;        (thePara (cl-user::getparam AE-clos-Ob :|----| ))
  ;;        (theobj (AEresolve thePara)))
  ;;   (if (equal (cl-user::getclass thepara) :|cinp|)
  ;;     (setf theobj (patch-value (nth (first theobj)  (input-objects (second theobj)))
  ;;                               (nth (first theobj)  (input-objects (second theobj))))))
  ;;   (if (patch-type-p theobj) (play theobj) (play-object theobj 1)))
    ;; (put-appleevent-par reply #$keyDirectObject t)
    )


(defmethod even-stop ((a application) theAppleEvent reply handlerRefcon)
  (declare (ignore handlerRefcon))
    (ui:uiwarn "~S is not implemented" 'even-stop)
  ;; (let* ((AE-clos-Ob (make-instance 'cl-user::AppleEvent :descRecPtr theAppleEvent ))
  ;;        (thePara (cl-user::getparam AE-clos-Ob :|----| ))
  ;;        (theobj (AEresolve thePara)))
  ;;   (if (equal (cl-user::getclass thepara) :|cinp|)
  ;;     (setf theobj (patch-value (nth (first theobj)  (input-objects (second theobj)))
  ;;                               (nth (first theobj)  (input-objects (second theobj))))))
  ;;   (stop-play theobj))
    ;; (put-appleevent-par reply #$keyDirectObject t)
    )



(defmethod even-record ((a application) theAppleEvent reply handlerRefcon)
  (declare (ignore handlerRefcon))
  t)

(defmethod even-unrecord ((a application) theAppleEvent reply handlerRefcon)
  (declare (ignore handlerRefcon))
  t)

;; (install-appleevent-handler :|core| :|clos| #'even-close)
;; (install-appleevent-handler :|aevt| :|odoc| #'even-open)
;; (install-appleevent-handler :|core| :|crel| #'even-new)
;; (install-appleevent-handler :|core| :|setd| #'even-set)
;; (install-appleevent-handler :|core| :|getd| #'even-get)
;; (install-appleevent-handler :|core| :|move| #'even-move)
;; (install-appleevent-handler :|core| :|save| #'even-save)
;; (install-appleevent-handler :|core| :|delo| #'even-delete)
;; (install-appleevent-handler :|PWst| :|conn| #'even-connect)
;; (install-appleevent-handler :|PWst| :|eval| #'even-eval)
;; (install-appleevent-handler :|core| :|clon| #'even-dupli)
;; (install-appleevent-handler :|PWst| :|unco| #'even-unco)
;; (install-appleevent-handler :|PWst| :|sele| #'even-sele)
;; (install-appleevent-handler :|PWst| :|rena| #'even-rena)
;; (install-appleevent-handler :|PWst| :|cand| #'even-lock)
;; (install-appleevent-handler :|PWst| :|cann| #'even-unlo)
;; (install-appleevent-handler :|misc| :|actv| #'even-actv)
;; (install-appleevent-handler :|misc| :|copy| #'even-copy)
;; (install-appleevent-handler :|misc| :|past| #'even-paste)
;; (install-appleevent-handler :|aevt| :|pdoc| #'even-print)
;; (install-appleevent-handler :|core| :|pwpe| #'even-paste)
;; (install-appleevent-handler :|core| :|cuto| #'even-cut)
;; (install-appleevent-handler :|PWst| :|pweb| #'even-extend)
;; (install-appleevent-handler :|PWst| :|popo| #'even-play)
;; (install-appleevent-handler :|PWst| :|ploa| #'even-load)
;; (install-appleevent-handler :|PWst| :|stop| #'even-stop)
;; (install-appleevent-handler :|PWst| :|gloo| #'even-options)
;; (install-appleevent-handler :|PWst| :|come| #'even-command)
;; 
;; (install-appleevent-handler :|aevt| :|rec1| #'even-record)
;; (install-appleevent-handler :|aevt| :|rec0| #'even-unrecord)




