;;;; -*- mode:lisp; coding:utf-8 -*-
(in-package :ccl)
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
                         (eq (length sig) 4)))
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
                           #'(lambda ()
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
    (ccl::with-pstrs ((name "Forum Owner"))
      (let ((reshandle (#_getNamedResource :|fipr| name)))
        (unless (ccl::%null-ptr-p reshandle)
          (#_rmveresource reshandle))))
    (#_updateresfile refnum)))
|#



(in-package :pw)

;(defun clear-patchwork ()
;  (midi::midi-close))

(defun save-dump-image (image-name &optional heap-size no-compiler)
  "Save an image"
  ;(clear-patchwork)
  (let ((file (choose-new-file-dialog
               :prompt "Save new image as:"
               :button-string  "Save"
               :directory "CL:images;PW.image"))
        (sizes (list (* 8500 1024)
                     (* 6500  1024))))
    (eval-enqueue        
     #'(lambda ()
         (save-application
          file
          :application-class (find-class 'ccl::lisp-development-system)
          :size sizes
          :resources (ccl::get-app-resources "CL:PW-inits;Drivers+Resources;CLPF.rsrc" :|CCL2|)
          :init-file "image-init" 
          :clear-clos-caches t
          :creator :|CCL2|
          :excise-compiler   no-compiler )))))

(defun remove-protec-resource (resource-file)
  (declare (ignore resource-file)))


(defun load-protection ()
  (unless (and (ccl:shift-key-p) (ccl:command-key-p) (ccl:control-key-p))
    (ccl::without-interrupts (load "CL:pw-inits;drivers+resources;magic" :verbose nil :print nil)
                             (ccl::check-protection))))

