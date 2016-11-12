;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               make-image.lisp
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

#|
(defun do-save-application (d applic-p)
  (let ((menubar-popup (view-named 'menubar-popup d))
        (error-popup   (view-named 'error-popup d)))
    (labels ((item-text (name)
               (dialog-item-text (view-named name d)))
             (read-item-text (name)
               (ignore-errors
                (read-from-string (item-text name)))))
      (let ((toplevel  (read-item-text 'toplevel-function))  ; package??
            (sig       (item-text 'creator))
            (min-size  (read-item-text 'min-size))
            (pref-size (read-item-text 'pref-size))
            (res-file  (item-text 'res-file))
            (item-num  (pop-up-menu-default-item menubar-popup))
            (error-handler (menu-item-action (nth (1- (pop-up-menu-default-item error-popup))
                                                  (menu-items error-popup)))))
        (cond ((not (numberp min-size))
               (message-dialog "Minimum size must be a number"))
              ((not (numberp pref-size))
               (message-dialog "Preferred size must be a number"))
              ((not (>= pref-size min-size))
               (message-dialog "Preferred size must be greater or equal minimum size"))            
              ((not (or (null toplevel)
                        (functionp toplevel)
                        (and (symbolp toplevel)
                             (fboundp toplevel))))
               (message-dialog "Toplevel function must be a defined function or NIL"))
              ((not (and (stringp sig)
                         (eql (length sig) 4)))
               (message-dialog "Application signature must be 4 characters long"))                         
              (t
               (setq sig (intern sig (find-package :keyword)))               
               (catch-cancel
                 (let ((file (choose-new-file-dialog
                              :prompt (if applic-p
                                        "Build application as:"
                                        "Save new image as:")
                              :button-string (if applic-p "Build" "Save")
                              :directory *app-name*))
                       (sizes (list (* pref-size 1024)
                                    (* min-size  1024))))
                   (cond (applic-p
                          (build-application file res-file sig sizes))
                         (t
                          (menu-item-action (nth (1- item-num) (menu-items menubar-popup)))
                          (eval-enqueue         ; see comment below
                           (lambda ()
                               (save-application
                                file
                                :application-class (find-class *app-class-name*)
                                :error-handler error-handler
                                :toplevel-function
                                (and (neq toplevel 'toplevel-function)
                                     toplevel)
                                :size sizes
                                ; :init-file "PW:PW-lib;PWscript;image-init" GA
                                :init-file "image-init" 
                                :resources (get-app-resources res-file sig)
                                :clear-clos-caches (check-box-checked-p (view-named 'caches d))
                                :excise-compiler   (check-box-checked-p (view-named 'excise d))                                            
                                :creator sig)))))))))))))

|#



#|
(defun remove-protec-resource (resource-file)
  (declare (ignore resource-file))
  (let ((refnum (#_CurResfile)))
    (ui::with-pstrs ((name "Forum Owner"))
      (let ((reshandle (#_getNamedResource :|fipr| name)))
        (unless (ui::%null-ptr-p reshandle)
          (#_rmveresource reshandle))))
    (#_updateresfile refnum)))
|#


(defun clear-patchwork ()
  (patchwork.midi:midi-close))

(defun save-dump-image (image-name &optional heap-size no-compiler)
  "Save an image"
  (clear-patchwork)
  (let ((file (choose-new-file-dialog
               :prompt "Save new image as:"

               :button-string  "Save"
               :directory "CL:images;PW.image"))
        (sizes (list (* 8500 1024)
                     (* 6500  1024))))
    (eval-enqueue      
     (lambda ()
         (save-application
          file
          :application-class (find-class 'ui::lisp-development-system)
          :size sizes
          :resources (ui::get-app-resources "CL:PW-inits;Drivers+Resources;CLPF.rsrc" :|CCL2|)
          :init-file "image-init" 
          :clear-clos-caches t
          :creator :|CCL2|
          :excise-compiler   no-compiler )))))

(defun remove-protec-resource (resource-file)
  (declare (ignore resource-file)))


(defun load-protection ()
  (unless (and (ui:shift-key-p) (ui:command-key-p) (ui:control-key-p))
    (ui::without-interrupts (load "CL:pw-inits;drivers+resources;magic" :verbose nil :print nil)
                             (ui::check-protection))))


