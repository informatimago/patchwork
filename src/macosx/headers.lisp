;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               headers.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;  
;;;;    Defines logical pathname translations for the additionnal
;;;;    framework header and interface directories.
;;;;  
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-04-14 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;  
;;;;    Copyright Pascal J. Bourguignon 2014 - 2014
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
(in-package :cl-user)


;;; MacOSX Framework directories:

(defun directory-exists-p (path)
  (directory path))

(defun make-framework-directory (framework-directory framework-name)
  (format nil "~A/~A.framework/" framework-directory framework-name))

(defun make-framework-headers-directory (framework-directory framework-name)
  (format nil "~AHeaders/"
          (make-framework-directory framework-directory framework-name)))

(defun make-framework-library-file (framework-directory framework-name)
  (format nil "~A~A"
          (make-framework-directory framework-directory framework-name)
          framework-name))


(defun find-framework-path (item framework-name)
  (check-type item (member :framework :headers :library))
  (dolist (framework-directory '("/System/Library/Frameworks" "/Library/Frameworks") nil)
    (let ((path (funcall (ecase item
                           (:framework (function make-framework-directory))
                           (:headers   (function make-framework-headers-directory))
                           (:library   (function make-framework-library-file)))
                         framework-directory framework-name)))
      (when (directory-exists-p path)
        (return path)))))

(defun find-framework-headers-directory (framework-name)
  (find-framework-path :headers framework-name))


;;; CCL Header directories:

(defparameter *additionnal-headers-directory*
  (make-pathname :name nil :type nil :version nil
                 :defaults (truename #.(or *compile-file-pathname* *load-pathname* #P"./"))))


(defun headers-wild-pathname (name bits)
  (merge-pathnames (make-pathname
                    :case :local
                    :directory (list :relative
                                     (format nil "headers~A" bits)
                                     name
                                     :wild-inferiors)
                    :name :wild :type :wild
                    :defaults *additionnal-headers-directory*)
                   *additionnal-headers-directory*
                   nil))


(defun add-headers-logical-pathname-translations (name)
  (setf (logical-pathname-translations "CCL")
        (list* (list (make-pathname :host "CCL"
                                    :case :local
                                    :name :wild
                                    :type :wild
                                    :directory (list :absolute
                                                     "darwin-x86-headers"
                                                     name
                                                     :wild-inferiors))
                     (headers-wild-pathname name 32))
               (list (make-pathname :host "CCL"
                                    :case :local
                                    :name :wild
                                    :type :wild
                                    :directory (list :absolute
                                                     "darwin-x86-headers64"
                                                     name
                                                     :wild-inferiors))
                     (headers-wild-pathname name 64))
               (logical-pathname-translations "CCL"))))

(defun populate-path (name bits)
  (pathname (format nil "CCL:~A;~(~A~);C;populate.sh"
                    (ecase bits
                      (32 "darwin-x86-headers")
                      (64 "darwin-x86-headers64"))
                    name)))



;;; CCL interface generation from headers:

;; /Developer/SDKs/MacOSX10.6.sdk/System/Library/Frameworks/
;; /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.8.sdk/System/Library/Frameworks/
;; /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.9.sdk/System/Library/Frameworks/


(defun generate-populate.sh (name &optional dependencies defines)
  (loop :for options :in '("-m32 -msse2" "-m64")
        :for bits :in '(32 64)
        :do (let ((path (populate-path name bits)))
              (ensure-directories-exist path)
              (with-open-file (stream path :direction :output :if-does-not-exist :create :if-exists :supersede)
                (let ((*print-circle* nil) (*print-escape* nil) (*print-case* :upcase))
                  (format stream "#!/bin/sh
if [ \"$(basename \"$(pwd)\")\" = C -a \"$(basename \"$(dirname \"$(pwd)\")\")\" = ~(~A~) ] ; then
    rm -rf System Developer usr
    CFLAGS=\"~A -fobjc-abi-version=2 -isysroot / -mmacosx-version-min=10.6 ~:{-D~A=~A~^ ~}\"
    export CFLAGS
    h-to-ffi.sh  ~{-I~A~^ ~}  ~A~0@*~A.h
else
    echo \"Please   cd ~(~:*~A~)/C   before running   sh ./populate.sh\"
fi
"
                          name
                          options
                          defines
                          (mapcar (function find-framework-headers-directory) dependencies)
                          (find-framework-headers-directory name)))))))




(defun populate (name)
  (loop :for  bits :in '(32 64)
        :do (asdf:run-shell-command (format nil "cd ~S ; chmod a+x ./populate.sh ; ./populate.sh"
                                                   (namestring (truename (make-pathname :name nil :type nil :version nil
                                                                                        :defaults (populate-path name bits))))))))


;;;; THE END ;;;;
