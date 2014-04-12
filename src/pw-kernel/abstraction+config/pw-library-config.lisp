;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pw-library-config.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;    
;;;;    User Library configuration
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

(defvar *config-default-libr-path*  "PW-USER:library-autoload;")
(defvar *config-default-abst-path*  "PW-USER:abstract-autoload;")
(defvar *config-init-file*          "PW-USER:PW-inits;config.init")

(defvar *compiled-abstr-extension* ".comp")

(defparameter *user-library-folder-path*  (merge-pathnames "**;*.lib" *config-default-libr-path*))

(defvar *user-abstracts-config* '())
(defvar *user-libs-config*      '())


(defvar *names-of-loaded-files* (make-hash-table :test (function equal)))

(defun is-loaded-p (name)
  (gethash name *names-of-loaded-files*))

(defun load-once (path &key verbose (if-does-not-exist :error))
  (let ((name  (pathname-name path)))
    (unless (is-loaded-p name)
      (handler-case
          (load path :verbose verbose :if-does-not-exist :error)
        (:no-error ()
          (setf (gethash name *names-of-loaded-files*) t))
        (error (err)
          (when if-does-not-exist
            (error err)))))))

(defun load-again (path &key verbose (if-does-not-exist :error))
  (let ((name  (pathname-name path)))
   (setf (gethash name *names-of-loaded-files*) nil))
  (load-once path :verbose verbose :if-does-not-exist if-does-not-exist))



(defun load-abstr-config ()
  (let* ((thestring (choose-directory-dialog :directory "cl:PW-user;")))
    (load&form-abstr-menu thestring)
    (record-event :|PWst| :|ploa| `((,:|----| ,(mkSO :|obab| nil :|name| (namestring (truename thestring))))))))


(defun load&form-abstr-menu (path &optional index)
  (if path
    (with-cursor *watch-cursor* 
      (let ((patches (directory (format () "~A**:*.*"path))))
        (when patches
          (setf *user-abstracts-config* (nconc *user-abstracts-config* (list path)))
          (dolist (file-name patches)
            (if (not-compiled-abstr file-name patches)
              (let ((saved-fun '*very-odd-fun-name*))
                (setf (fdefinition saved-fun) (fdefinition 'add-patch-box))
                (setf (fdefinition 'add-patch-box) (fdefinition 'no-add-patch-box))
                (load file-name)
                (setf (fdefinition 'add-patch-box) (fdefinition saved-fun))
                (form-abstract-subMenu 
                 (namestring file-name) (namestring path) 
                 `(add-patch-box *active-patch-window*
                                 ,(decompile *added-box-object*))
                 index)))))))))

(defun form-abstract-subMenu (patch-file-name from-folder code &optional index)
  (let ((sub-dir-list 
         (nthcdr (- (length (parse-file-name from-folder)) (or index 2))
                 (parse-file-name patch-file-name)))
        (current-sub-menu *pw-menu-patch*)
        (menu))
    (dotimes (x (1- (length sub-dir-list)))
      (unless 
          (setq menu (find-menu-item current-sub-menu (car sub-dir-list)))
        (ui:add-menu-items current-sub-menu
                           (setq menu (new-menu (car sub-dir-list)))))                            
      (setq current-sub-menu menu)
      (pop sub-dir-list))
    (or (find-menu-item current-sub-menu (car sub-dir-list))
        (ui:add-menu-items current-sub-menu (new-leafmenu (trim-extension (car sub-dir-list)) 
                                                          (eval `(function (lambda () ,code))))))))


(defun not-compiled-abstr (name files)
  (let ((name (namestring name))
        (files (mapcar #'namestring files)))
    (or (search *compiled-abstr-extension* name :test #'string=)
        (let* ((point (position  #\. name :from-end t :test #'char=)))
          (if point
            (not (member (concatenate 'string (subseq name 0 point) *compiled-abstr-extension*)
                         files  :test #'string=))
            (not (member (concatenate 'string name *compiled-abstr-extension*)
                         files :test #'string=)))))))

(defun trim-extension (name)
  (let* ((point (position  #\. name :from-end t :test #'char=)))
    (if point (subseq name 0 point) name)))


;;(load-abstr-config)

(defun load-library-config ()
  (let* ((thestring (choose-file-dialog :directory "cl:User-library;" :button-string "load lib")))
    (load-one-user-library thestring)
    (record-event :|PWst| :|ploa| `((,:|----| ,(mkSO :|obli| nil :|name| (namestring (truename thestring))))))))



#||
GA 17/5/94

(defun load-one-user-library (path)
  (if path
    (with-cursor *watch-cursor*
      (setf *user-libs-config* (nconc *user-libs-config* (list path)))
      (load path :verbose t :if-does-not-exist nil))))

(defun remember-config ()
  (let ((file-config-list
         (with-open-file (file *config-init-file* :direction :input :if-does-not-exist nil)
           (and file (read file)))))
    (with-open-file (file *config-init-file* :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
      (prin1 `(,(append (first file-config-list) *user-libs-config*)
                ,(append (second file-config-list) *user-abstracts-config*)
                ,(get-global-options-marks)
                ,(get-evaluation-option)) file))))

(defun load-remembered-config ()
  (apply #'remove-menu-items *pw-menu-patch* (menu-items *pw-menu-patch*))
  (let ((in-user-lib (directory *user-library-folder-path*))
        (file-config-list
         (with-open-file (file *config-init-file* :direction :input :if-does-not-exist nil)
           (and file (read file)))))
    (dolist (lib in-user-lib)
      (load-again lib))
    (dolist (lib (first file-config-list))
      (load-one-user-library lib))
    (dolist (abstr-path (second file-config-list))
      (load&form-abstr-menu abstr-path))
    (load&form-abstr-menu (mac-namestring *config-default-abst-path*) 1)
    (and (third file-config-list)
         (restore-global-options (third file-config-list) 
                                 (fourth file-config-list)))))

(defun forget-all-config ()
  (apply #'remove-menu-items *pw-menu-patch* (menu-items *pw-menu-patch*))
  (setf *user-libs-config* ())
  (setf *user-abstracts-config* ())
  (delete-file *config-init-file* :if-does-not-exist nil)
  (with-open-file (file *config-init-file* :direction :output :if-exists :supersede
                        :if-does-not-exist :create)
    (prin1 `(nil nil (nil t t nil nil :mc) t) file))
  (format t "configuration erased"))

||#



(defun find&load-lib (logical-dir-str file-to-load directory-path)
  (let ((path-to-load (merge-pathnames file-to-load logical-dir-str)))
    (cond ((directory (make-pathname :type :wild :defaults path-to-load))
           (load-again path-to-load))
          ((directory (format nil "PW-USER:library-autoload;~A;~A.*" directory-path file-to-load))
           (let ((old-path (logical-pathname-translations logical-dir-str)))
             (setf (logical-pathname-translations logical-dir-str)
                   `(("**;" ,(format nil "PW-USER:library-autoload;~A;**;" directory-path))))
             (unwind-protect
                  (load-again path-to-load)
               (setf (logical-pathname-translations logical-dir-str) old-path))))
          (t (format t "can't find library in path: ~S" 
                     (full-pathname (format nil "~A:" logical-dir-str)))
             (ui:ed-beep)))))

(defun define-logical-path-dir (dir path)
  (setf (logical-pathname-translations dir)
        `(("**;" ,(format nil "~A;**;" path)))))

;;(load-abstr-config)
;;(load-library-config)
;;(remember-config)
;;(load-remembered-config)
;;(forget-all-config)





;;; GA 17/3/94
;;; do not erase user menu before loading image. cf chant.

(defun load-remembered-config ()
  ;;(apply #'remove-menu-items *pw-menu-patch* (menu-items *pw-menu-patch*))
  (let ((in-user-lib (directory *user-library-folder-path*))
        (file-config-list
         (with-open-file (file *config-init-file* :direction :input :if-does-not-exist nil)
           (and file (read file)))))
    (dolist (lib in-user-lib)
      (load-again lib))
    (dolist (lib (first file-config-list))
      (load-one-user-library lib))
    (dolist (abstr-path (second file-config-list))
      (load&form-abstr-menu abstr-path))
    (load&form-abstr-menu (namestring *config-default-abst-path*) 1)
    (and (third file-config-list)
         (restore-global-options (third file-config-list) 
                                 (fourth file-config-list)))))

(defun load-one-user-library (path)
  (if path
    (with-cursor *watch-cursor*
      (unless (is-loaded-p (pathname-name path))
        (setf *user-libs-config* (nconc *user-libs-config* (list path))))
      (load-once path :verbose t :if-does-not-exist nil))))

(defun remember-config ()
  (with-open-file (file *config-init-file* :direction :output :if-exists :supersede
                                           :if-does-not-exist :create)
    (prin1 `(,*user-libs-config*
             ,*user-abstracts-config*
             ,(get-global-options-marks)
             ,(get-evaluation-option)) file)))


(defun forget-all-config ()
  ;;(apply #'remove-menu-items *pw-menu-patch* (menu-items *pw-menu-patch*))
  ;;(dolist (lib *user-libs-config*)
  ;;  (setf module::*loaded-modules*
  ;;        (remove  (print (pathname-name lib)) module::*loaded-modules*  :test #'string-equal)))
  (setf *user-libs-config* ())
  (setf *user-abstracts-config* ())
  (ignore-errors (delete-file *config-init-file*))
  (with-open-file (file *config-init-file* :direction :output :if-exists :supersede
                                           :if-does-not-exist :create)
    (prin1 `(nil nil (nil t t nil nil :mc) t) file))
  (format t "~&configuration erased~%"))


;;;; THE END ;;;;



