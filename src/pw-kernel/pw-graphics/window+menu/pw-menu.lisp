;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pw-menu.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;    
;;;;    XXX
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;    Mikael Laurson
;;;;    Jacques Duthen
;;;;    Camilo Rueda.
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

(in-package :patch-work)

;;===========================================
;; menubar for lisp
(defvar *PW-box-instance-list* '())
(defvar *pw-window-list*       '())
(defvar *active-patch-window*  nil)


(defun new-menu (title &rest menus)
  "Creates a new menu with the given <title> and the list of <menus>."
  (let ((menu (make-instance 'menu :menu-title title)))
    (apply #'add-menu-items menu menus)
    menu))

(defun menu-list (menu) (menu-items menu)) 

(defun new-leafmenu (title action)
  "Creates a new leaf menu with the given <title> and <action>."
  (make-instance 'menu-item :menu-item-title title :menu-item-action action))

(defvar *pw-menu-apps*        nil)

(defvar *apps-lisp-menu-item* nil)
(defvar *apps-PW-menu-item*   nil)

;;_________________

(defvar *original-CCL-menubar* (copy-list ui:*default-menubar*))
(defvar *default-CCL-menubar*  '())


;;___________

(defun enable-all-apps-menu-items ()
  (let ((menus (menu-list *pw-menu-apps*)))
    (while menus (menu-item-enable (pop menus)))))

(defun enable-Lisp-apps-menu-item? ()
  (unless (member nil (mapcar #'menu-enabled-p (menu-list *pw-menu-apps*)))
    (ui:set-menubar *default-CCL-menubar*)
    ;;added 920818 [Camilo]
    (mapc #'menu-enable (cdr *default-CCL-menubar*))
    (menu-item-disable *apps-lisp-menu-item*)))

(defun add-apps-item-to-apps-menu (title action)
  (let ((menu (new-leafmenu title action)))
    (ui:add-menu-items  *pw-menu-apps* menu)
    menu))

;;___________

(defun search-for-next-pw-window ()
  (setf *active-patch-window* (ask *pw-window-list* 'top-level-patch-win?))
  (if *active-patch-window*
      (window-select *active-patch-window*)
      (make-new-pw-window t)))

;;=========================================
;; file

(defvar *pw-menu-file*                   nil)
(defvar *pw-menu-file-close-item*        nil)
(defvar *pw-menu-file-only-Save-item*    nil)
(defvar *pw-menu-file-only-SaveMN-item*  nil)
(defvar *pw-menu-file-Save-item*         nil)
(defvar *pw-menu-file-SaveMN-item*       nil)

;;=========================================
;; edit

(defvar *pw-menu-edit* nil)


;;;=====================================
;;the PW Menu-bar menus
;;======================================

(defvar *PWoper-menu* nil)

(defvar *pw-kernel-menu* nil)
(defvar *pw-data-menu* nil)
(defvar *pw-Arith-menu* nil)
(defvar *pw-Num-series-menu* nil)
(defvar *pw-Num-Fun-Gen-menu* nil)
(defvar *pw-control-menu* nil)
(defvar *pw-usual-lisp-menu* nil)
(defvar *pw-List-menu* nil)
(defvar *pw-set-menu* nil)
;;(defvar *pw-list-gen-menu* nil)
;;(defvar *pw-list-trans-menu*  nil)
(defvar *pw-list-combin-menu* nil)
(defvar *pw-Abs-menu* nil)
(defvar *pw-BPF-menu* nil)
(defvar *pw-Vision-menu* nil)
(defvar *pw-Extern-menu* nil)
(defvar *pw-Multidim-menu* nil)


(defvar *pw-menu-patch* (new-menu "UserLib"))

(defun the-user-menu () *pw-menu-patch*)

(defvar *pw-windows-menu* (new-menu "Wins"))

;;============================================
;; PW

(defvar *pw-debug-menu*       nil)
(defvar *g-option-c-major*    nil)
(defvar *play-Pbend-menu*     nil)
(defvar *play-Multichan-menu* nil)
(defvar *g-option-chromatic*  nil)
(defvar *semitone-menu*       nil)
(defvar *quartertone-menu*    nil)
(defvar *eighthtone-menu*     nil)
(defvar *click-menu*          nil)
(defvar *option-click-menu*   nil)


(defun save-special-pw-image ()
  (let ((file (choose-new-file-dialog
               :prompt "Save new image as:"
               :button-string  "Save"
               :directory (merge-pathnames "Desktop/" (user-homedir-pathname))
               #-(and)"CL:images;Pw.image"))
        (sizes (list (* 8500 1024)
                     (* 6500  1024))))
    (when file
      (clear-patchwork)
      #+ccl (setf ccl:*lisp-cleanup-functions* (list (car ccl:*lisp-cleanup-functions*)))
      (eval-enqueue        
       (lambda ()
         #+ccl (ccl::build-application
                :name (file-namestring file)
                :directory (make-pathname :name nil :type nil :version nil :defaults file)
                :copy-ide-resources t
                ;; :init-file "HOME:patchwork-init.lisp"
                ;; '(pathname "~/application-init.lisp")
                ;;  (lambda ()
                ;;              (make-pathname :name  "patchwork-init" :type "lisp"
                ;;                             :defaults (user-homedir-pathname)))
                )
         #-ccl (error "Not implemented yet on ~A" (lisp-implementation-type))
         #-(and)
         (save-application
          file
          :application-class (find-class 'ui::lisp-development-system)
          :size sizes
          ;;aaa 140397 new protection
          :resources (ui::get-app-resources "CL:PW-inits;Drivers+Resources;CLPF.rsrc" :|CCL2|)
          ;;:init-file "PW:PW-lib;PWscript;image-init" GA 230996
          :init-file "image-init" 
          :clear-clos-caches t
          :excise-compiler   nil))))))


(defun dis/enable-menu-item (menu-item fl)
  (if fl
      (menu-item-enable menu-item)
      (menu-item-disable menu-item)))

(defun update-PW-file-menu-items (fl change-fl)
  ;;(dis/enable-menu-item *pw-rename-menu-item* (not fl))
  (dis/enable-menu-item *pw-menu-file-close-item* fl)
  (dis/enable-menu-item *pw-menu-file-Save-item* fl)
  (dis/enable-menu-item *pw-menu-file-SaveMN-item* fl)
  (dis/enable-menu-item *pw-menu-file-only-Save-item* (and fl change-fl))
  (dis/enable-menu-item *pw-menu-file-only-SaveMN-item* (and fl change-fl)))

;;============================================
;;============================================
;; menubar for PW

(defvar *patch-work-menu-root* nil)



;;==================================================

(defun cleanup-PW-wins ()
  (setf *pw-window-list* (remove nil (remove nil *pw-window-list* :key #'wptr)
                                 :key #'abstract-box :test-not #'eq ))
  (tell  *pw-window-list* 'kill-patch-window))



(defmacro handling-errors (&body body)
  "
DO:       Execute the BODY with a handler for CONDITION and
          SIMPLE-CONDITION reporting the conditions.
"
  `(handler-case (progn ,@body)
     (simple-error  (err) 
       (format *trace-output* "~&~A: ~%" (class-name (class-of err)))
       (apply (function format) *trace-output*
              (simple-condition-format-control   err)
              (simple-condition-format-arguments err))
       (format *trace-output* "~&")
       (finish-output))
     (error (err) 
       (format *trace-output* "~&~A: ~%  ~S~%" (class-name (class-of err)) err)
       (finish-output))))

(defun lisp-menu-action ()
  (niy lisp-menu-action)
  #-(and)
  (let ((listener (find-if (lambda (w)
                             (subtypep (type-of w) 'listener))
                           (windows))))
    (when listener
      (window-select listener)))
  (enable-all-apps-menu-items)
  (menu-item-disable *apps-lisp-menu-item*))


(defun pw-menu-action ()
  (com.informatimago.common-lisp.cesarum.utility:tracing
   (format *trace-output* "~&~S --> ~S~%" '*active-patch-window* *active-patch-window*)
   (if *active-patch-window* 
       (if (window-visiblep *active-patch-window*) 
           (window-select *active-patch-window*)
           (search-for-next-pw-window)) 
       (make-new-pw-window t))
   (enable-all-apps-menu-items)
   (menu-item-disable *apps-PW-menu-item*)))


(defun initialize-menus ()
  ;;------------------------------
  (setf *pw-menu-apps*         (new-menu "Patchwork"))
  (setf *default-CCL-menubar*  (list* (first *original-CCL-menubar*)
                                      *pw-menu-apps*
                                      (rest *original-CCL-menubar*)))
  ;;------------------------------
  (setf *apps-lisp-menu-item*  (add-apps-item-to-apps-menu "Lisp" 'lisp-menu-action))
  ;;------------------------------
  (setf *apps-PW-menu-item*    (add-apps-item-to-apps-menu "PW"  'pw-menu-action))
  ;;------------------------------
  (setf *pw-menu-file* (new-menu "File"))
  (let ((menu-now))
    (add-menu-items  *pw-menu-file* 
                     (setf menu-now (new-leafmenu "New"        (lambda() (make-new-pw-window t)))))
    (set-command-key menu-now #\N)
    (add-menu-items  *pw-menu-file*
                     (setf menu-now
                           (new-leafmenu "Open patch..."       (lambda () (PW-LOAD-PATCH)))))
    (set-command-key menu-now #\O)
    (add-menu-items  *pw-menu-file* 
                     (setf *pw-menu-file-close-item*
                           (new-leafmenu "Close"               (lambda () (kill-patch-window *active-patch-window*)))))
    (set-command-key *pw-menu-file-close-item* #\W)
    (add-menu-items  *pw-menu-file* 
                     (setf *pw-menu-file-only-Save-item*
                           (new-leafmenu "Save"                (lambda () (PW-WINDOW-SAVE *active-patch-window*)))))
    (set-command-key *pw-menu-file-only-Save-item* #\S)
    (add-menu-items  *pw-menu-file* 
                     (setf *pw-menu-file-only-SaveMN-item*
                           (new-leafmenu "Save with MN"        (lambda () (PW-WINDOW-SAVE-MN *active-patch-window*)))))
    (add-menu-items  *pw-menu-file* 
                     (setf *pw-menu-file-Save-item*
                           (new-leafmenu "Save as..."          (lambda () (PW-WINDOW-SAVE-as *active-patch-window*)))))
    (add-menu-items  *pw-menu-file* 
                     (setf *pw-menu-file-SaveMN-item*
                           (new-leafmenu "Save with MN as..."  (lambda () (PW-WINDOW-SAVE-MN-as *active-patch-window*))))))
  ;;------------------------------
  (setf *pw-menu-edit* (new-menu "Edit"))
  (let ((menu-now))
    (add-menu-items  *pw-menu-edit* (setf menu-now (new-leafmenu "Cut"         (lambda () (cut *active-patch-window*)))))
    (set-command-key menu-now #\X)
    (add-menu-items  *pw-menu-edit* (setf menu-now (new-leafmenu "Copy"        (lambda () (copy *active-patch-window*)))))
    (set-command-key menu-now #\C)
    (add-menu-items  *pw-menu-edit* (setf menu-now (new-leafmenu "Paste"       (lambda () (paste *active-patch-window*)))))
    (set-command-key menu-now #\V)
    (add-menu-items  *pw-menu-edit* (setf menu-now (new-leafmenu "Duplicate"   (lambda () (duplicate *active-patch-window*)))))
    (set-command-key menu-now #\D)
    (add-menu-items  *pw-menu-edit* (setf menu-now (new-leafmenu "Select All"  (lambda () (activate-all *active-patch-window*) ))))
    (set-command-key menu-now #\A))
  ;;------------------------------
  (setf *PWoper-menu*          (new-menu "PWoper"))
  (setf *pw-kernel-menu*       (new-menu "Kernel"))
  (setf *pw-data-menu*         (new-menu "Data"))
  (setf *pw-Arith-menu*        (new-menu "Arithmetic"))
  (setf *pw-Num-series-menu*   (new-menu "Num-series"))
  (setf *pw-Num-Fun-Gen-menu*  (new-menu "Function"))
  (setf *pw-control-menu*      (new-menu "control"))
  (setf *pw-usual-lisp-menu*   (new-menu "LISP"))
  (setf *pw-List-menu*         (new-menu "List"))
  (setf *pw-set-menu*          (new-menu "Set Operations"))
  ;;(setf *pw-list-gen-menu*     (new-menu "Generation"))
  ;;(setf *pw-list-trans-menu*   (new-menu "Transformation"))
  (setf *pw-list-combin-menu*  (new-menu "Combinatorial"))
  (setf *pw-Abs-menu*          (new-menu "Abs"))
  (setf *pw-BPF-menu*          (new-menu "BPF"))
  (setf *pw-Vision-menu*       (new-menu "Vision"))
  (setf *pw-Extern-menu*       (new-menu "Extern"))
  (setf *pw-Multidim-menu*     (new-menu "Multidim"))
  (add-menu-items *pw-kernel-menu* 
                  *pw-data-menu* *pw-Arith-menu* *pw-Num-series-menu* *pw-Num-Fun-Gen-menu*
                  *pw-control-menu* *pw-usual-lisp-menu*
                  *pw-List-menu* *pw-set-menu* *pw-list-combin-menu* *pw-Abs-menu* *pw-BPF-menu*
                  ;;*pw-Vision-menu* 
                  *pw-Extern-menu*
                  *pw-Multidim-menu*)
  ;;------------------------------
  (setf *pw-menu-patch*   (new-menu "UserLib"))
  (setf *pw-windows-menu* (new-menu "Wins"))
  (add-menu-items  *PWoper-menu* 
                   (new-leafmenu "Documentation"  (lambda () (tell (ask-all (active-patches *active-patch-window*) 'pw-function) 'show-documentation)))
                   (new-leafmenu "Definition"     (lambda () (tell (ask-all (active-patches *active-patch-window*) 'pw-function) 'edit-definition)))
                   (new-leafmenu "Window Comment" (lambda () (get-window-text-box)))
                   (new-leafmenu "-" nil)
                   (new-leafmenu "Abstract"       (lambda () (make-abstraction-M *active-patch-window*))))
  (add-menu-items *PWoper-menu*
                  (new-leafmenu "Lisp function…"
                                (lambda () 
                                  (let ((string
                                         (get-string-from-user  "Lisp function"
                                                                :size (make-point 200 85)
                                                                :position :centered
                                                                :initial-string "list")))
                                    (when string
                                      (setf *si-record* nil)
                                      (let* ((patch (make-lisp-pw-boxes (read-from-string string) 
                                                                        *active-patch-window*)))
                                        (setf *si-record* t)
                                        (when patch
                                          (record-patch "funlisp"
                                                        (list (point-h (view-position patch))
                                                              (point-h (view-position patch)))
                                                        string)))))))
                  (new-leafmenu "-" nil)
                  ;; (new-leafmenu "Abort" (lambda () (toplevel))))
                  )
  (let ((menu (find-menu "Lisp")))
    (when menu
      (dolist (title '("Abort" "Break" "Continue"))
        (let ((item (find-menu-item menu title)))
          (when item
            (add-menu-items *PWoper-menu* item))))))
  ;;------------------------------
  (setf *g-option-c-major*     (new-leafmenu "C-major"        (lambda () (set-globally-scale *c-major-scale*))))
  (setf *play-Pbend-menu*      (new-leafmenu "Pitch Bend"     (lambda () (set-playing-option :pb))))
  (setf *play-Multichan-menu*  (new-leafmenu "Multi Channel"  (lambda () (set-playing-option :mc))))
  (setf *g-option-chromatic*   (new-leafmenu "Chromatic"      (lambda () (set-globally-scale *chromatic-scale*))))
  (setf *semitone-menu*        (new-leafmenu "SemiTone"       (lambda () (set-globally-scale *c-major-scale*))))
  (setf *quartertone-menu*     (new-leafmenu "Quarter tone"   (lambda () (set-globally-scale *1/4-tone-chromatic-scale*))))
  (setf *eighthtone-menu*      (new-leafmenu "Eigth tone"     (lambda () (set-globally-scale *1/8-tone-chromatic-scale*))))
  (setf *click-menu*           (new-leafmenu "Click"          (lambda () (set-eval-click nil))))
  (setf *option-click-menu*    (new-leafmenu "Option-Click"   (lambda () (set-eval-click t))))
  ;;------------------------------
  (add-menu-items *PWoper-menu* 
                  (new-leafmenu "-" nil)
                  (setf *pw-debug-menu* (new-leafmenu "PW-debug-ON" (lambda () (flip-pw-debug))))
                  (new-leafmenu "show error box" 'activate-current-patch-value-patch)
                  (new-leafmenu "-" nil)
                  (new-menu "Global options"
                            (new-menu "Scale" *g-option-c-major* *g-option-chromatic*)
                            (new-menu "Approximation"
                                      *semitone-menu*
                                      (prog1 *quartertone-menu* (set-menu-item-check-mark *quartertone-menu* t))
                                      *eighthtone-menu*)
                            (new-menu "Play Option"
                                      *play-Pbend-menu*
                                      *play-Multichan-menu*)
                            (new-menu "Evaluation"
                                      *option-click-menu*
                                      *click-menu*))
                  (new-leafmenu "-" nil)
                  (new-leafmenu "Load Library…"       (lambda () (load-library-config)))
                  (new-leafmenu "Load Abstracts…"     (lambda () (load-abstr-config)))
                  (new-leafmenu "Save Current Config" (lambda () (remember-config)))
                  (new-leafmenu "Delete Config"       (lambda () (forget-all-config)))
                  (new-leafmenu "-" nil)
                  (new-leafmenu "Save Image"          (lambda () (save-special-pw-image))))
  ;;------------------------------
  (setf *patch-work-menu-root* (list
                                *pw-menu-apps*
                                *pw-menu-file* *pw-menu-edit* 
                                *PWoper-menu* *pw-kernel-menu* *pw-menu-patch*
                                *pw-windows-menu*))
  ;;------------------------------
  (set-menubar       *default-CCL-menubar*)
  (menu-item-disable *apps-lisp-menu-item*)
  (set-command-key   *apps-lisp-menu-item* #\L)
  (set-command-key   *apps-PW-menu-item*   #\1)
  ;;------------------------------
  #+ccl (pushnew (function cleanup-PW-wins) ccl:*lisp-cleanup-functions*)
  #-ccl (niy *lisp-cleanup-functions*))


(initialize-menus)
;;;; THE END ;;;;
